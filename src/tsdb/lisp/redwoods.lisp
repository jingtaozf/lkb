(in-package :tsdb)

;;;
;;; ToDo
;;;
;;; - protect against saving with an empty `decision' set;
;;; - fill in `preference' relation;
;;; - confidence menu;
;;; - `Reset' button: re-instantiate original, preset state;
;;; - reorder trees: active at top;
;;; - pairwise comparison of trees;
;;; - highlighting of discriminats on tree select;
;;; - highlighting of trees on discriminant select;
;;; - utilize status vector to, e.g. fast-forward to first unannotated;
;;; - record all :select decisions, valid at `Save' time;
;;; - add print button: include edge id in display and print out;
;;;


(defun browse-trees (&optional (data *tsdb-data*)
                     &key (condition *statistics-select-condition*)
                          (cache *tsdb-cache-database-writes-p*)
                          (verbose t)
                          (stream *tsdb-io*)
                          purge meter)

  (declare (optimize (speed 3) (safety 0) (space 0)))

  (when purge
    (unless (do-import-database (find-tsdb-directory data) purge 
                                :meter (when meter (make-meter 0 1))
                                :except '("tree" "decision"))
      (return-from browse-trees nil)))

  (let* ((condition (if condition
                      (concatenate 'string "(readings >= 1) && " condition)
                      "readings >= 1"))
         (items
          (if (stringp data) 
            (analyze data 
                     :condition condition :meter meter :message t)
            data))
         (message (format 
                   nil 
                   "~a `~a' trees ..." 
                   data (if purge "purging" "browsing")))
         (items (sort (copy-seq items) 
                      #'< :key #'(lambda (foo) (get-field :i-id foo))))
         (schema (read-database-schema data))
         (cache (when cache
                  (create-cache (or purge data)
                                :schema schema :verbose verbose 
                                :protocol cache)))
         %client%)
    (declare (special %client%))

    (when (functionp *statistics-result-filter*)
      (setf items
        (loop
            for item in items
            for result = (funcall *statistics-result-filter* item)
            when result collect result)))
    
    (when meter
      (status :text message)
      (meter :value 0))

    (when (or purge #+:expand t)
      (install-gc-strategy nil :tenure nil :burst t :verbose t))
    
    (loop
        with increment = (and meter (/ 1 (if items (length items) 1)))
        with frame = (unless #-:expand purge #+:expand nil
                       (clim:make-application-frame 'lkb::compare-frame))
        with title = (format 
                      nil 
                      "[incr tsdb()] Tree Selection~@[ @ `~a'~]" condition)
        with nitems = (length items)
        with annotated = (make-array nitems :initial-element 0)
        with position = 0
        initially
          #+:debug
          (setf *frame* frame)
          (unless #-:expand purge #+:expand nil
            (setf (lkb::compare-frame-current-chart frame) nil)
            (setf (clim:frame-pretty-name frame) title)
            (setf (lkb::compare-frame-controller frame) *current-process*))
        for item = (when position (nth position items))
        for i-id = (get-field :i-id item)
        for status = (when (integerp i-id) 
                       (browse-tree 
                        data i-id frame 
                        :cache cache :title title :purge purge
                        :verbose verbose :stream stream))
        for action = (get-field :status status)
        for offset = (or (get-field :offset status) 1)
        while (and status (not (eq action :close)) (numberp position))
        do 
          (when (and (eq action :save) increment 
                     (zerop (aref annotated position)))
            (meter-advance increment))
          (case action
            (:first (setf position 0))
            (:previous (setf position (max (- position offset) 0)))
            ((:skip :null)
             (setf position 
               (when (< position (- nitems 1)) (+ position offset))))
            ((:next :save) 
             (when (eq action :save) (incf (aref annotated position)))
             (setf position 
               (if (= position (- nitems 1))
                 (if #+:expand t #-:expand purge nil (- nitems 1))
                 (+ position offset))))
            (:last (setf position (- nitems 1))))
        finally
          (when frame (clim:frame-exit frame))
          (when (mp:process-p %client%)
            (mp:process-kill %client%)))
  
    (when cache (flush-cache cache :verbose verbose))
    (purge-profile-cache data)
    
    (when meter
      (status :text (format nil "~a done" message) :duration 10)
      (meter :value 1))))

(defun browse-tree (data i-id frame &key title cache purge verbose stream)
  
  (declare (special %client%))

  (when (or (null %client%)
            (and (mp:process-p %client%) (mp:process-active-p %client%)))
    (let* ((condition (format nil "i-id = ~a" i-id))
           (items (analyze data :thorough '(:derivation) :condition condition))
           (item (and (null (rest items)) (first items)))
           (input (or (get-field :o-input item) (get-field :i-input item)))
           (i-id (get-field :i-id item))
           (parse-id (get-field :parse-id item))
           (trees (when parse-id
                    (select '("parse-id" "t-version" "t-active" "t-confidence" 
                              "t-author" "t-start" "t-end" "t-comment")
                            '(:integer :integer :integer :integer 
                              :string :date :date :string)
                            "tree" 
                            (format nil "parse-id == ~a" parse-id) 
                            data
                            :sort :parse-id)))
           (version (loop
                        for tree in trees
                        maximize (get-field :t-version tree)))
           (trees (loop
                      for tree in trees
                      when (eq version (get-field :t-version tree))
                      collect tree))
           (user (get-field :t-author (first trees)))
           (date (get-field :t-end (first trees)))
           (confidence (let* ((foo (get-field :t-confidence (first trees))))
                         (if (and (integerp foo) (>= foo 0) (<= foo 3))
                           foo
                           3)))
           (history (let* ((foo (get-field :t-confidence (first trees)))
                           (confidence 
                            (if (and (integerp foo) (>= foo 0) (<= foo 3))
                              (aref #("zero" "low" "fair" "high") foo)
                              "unknown")))
                      (if (and (> version 0) user date)
                        (format
                         nil
                         "~a (~a) confidence; version ~d on ~a by `~a'"
                         foo confidence version date user)
                        "")))
           (results (get-field :results item))
           (edges (unless (or #-:expand purge #+:expand (null trees))
                    (loop
                        with edges
                        for result in results
                        for id = (get-field :result-id result)
                        for derivation = (get-field :derivation result)
                        for edge = (and derivation (reconstruct derivation))
                        when edge do 
                          (setf (lkb::edge-score edge) id)
                          (setf (lkb::edge-parents edge) derivation)
                          (push edge edges)
                        finally (return (nreverse edges)))))
           (foo (first edges))
           (start (and foo (lkb::edge-from foo)))
           (end (and foo (lkb::edge-to foo)))
           (decisions (when (and parse-id version)
                        (select '("parse-id" "version"
                                  "d-state" "d-type" "d-key" "d-value" 
                                  "d-start" "d-end" "d-date")
                                '(:integer :integer
                                  :integer :integer :string :string 
                                  :integer :integer :date)
                                "decision" 
                                (format 
                                 nil 
                                 "parse-id == ~a && version == ~a" 
                                 parse-id version) 
                                data)))
           (discriminants (unless #-:expand purge #+:expand nil
                            (reconstruct-discriminants decisions)))
           (lkb::*parse-record* edges))
      (declare (ignore active))

      (when purge
        (when trees
          (write-tree purge (first trees) :cache cache))
        (loop
            for decision in decisions
            do (write-decision purge decision :cache cache))
        #-:expand
        (return-from browse-tree (acons :status :save nil)))

      (when (null edges)
        (when verbose
          (format
           stream
           "browse-tree(): failed to reconstruct item # ~d (parse # ~d).~%"
           i-id parse-id))
        (return-from browse-tree (acons :status :null nil)))
      
      (setf (lkb::compare-frame-input frame) input)
      (setf (lkb::compare-frame-item frame) i-id)
      (setf (lkb::compare-frame-start frame) start)
      (setf (lkb::compare-frame-end frame) end)
      (setf (lkb::compare-frame-version frame) history)
      (setf (lkb::compare-frame-confidence frame) confidence)
      (setf (lkb::compare-frame-preset frame) discriminants)
      (when (null %client%)
        (setf %client%
          (mp:run-function 
           (or title "[incr tsdb()] Tree Selection")
           #'clim:run-frame-top-level frame))
        #+:debug
        (when (find :compiler *features*)
          (excl:advise mp:process-kill :after nil nil
                       `(when (and ,%client% 
                                   (eq (first excl:arglist) ,%client%))
                          (ignore-errors 
                           (process-revoke-arrest-reason 
                            *current-process* :wait))))))
      
      (let ((status (lkb::set-up-compare-frame lkb::*parse-record* frame))) 
        #+:expand
        (lkb::record-decision (lkb::make-decision :type :save) frame)
        #-:expand
        (unless (eq status :skip)
          (clim:redisplay-frame-panes frame :force-p t)
          (process-add-arrest-reason *current-process* :wait)))

      (let* ((decisions (lkb::compare-frame-decisions frame))
             (status (lkb::decision-type (first decisions)))
             (recent (second decisions)))
        (when (eq status :save)
          (let* ((version (if version 
                            #-:expand (incf version) #+:expand version
                            1))
                 (trees (lkb::compare-frame-in-parses frame))
                 (active (length trees))
                 (foo (lkb::compare-frame-confidence frame))
                 (confidence (if (and (integerp foo)
                                      (>= foo 0) (<= foo 3))
                               foo
                               -1))
                 (t-author (current-user))
                 (t-start (let* ((start (first (last decisions)))
                                 (start (when (lkb::decision-p start)
                                          (lkb::decision-time start))))
                            (if start 
                              (decode-time start :long t)
                              (current-time :long t))))
                 (t-end (let* ((end (first decisions))
                               (end (when (lkb::decision-p end)
                                      (lkb::decision-time end))))
                          (if end 
                            (decode-time end :long t)
                            (current-time :long t)))))
            #-:expand
            (write-tree data (pairlis '(:parse-id 
                                        :t-version :t-active :t-confidence
                                        :t-author :t-start :t-end :t-comment)
                                      (list parse-id 
                                            version active confidence
                                            t-author t-start t-end ""))
                        :cache cache)

            (loop
                for tree in trees
                for edge = (get tree 'lkb::edge-record)
                for id = (when (lkb::edge-p edge) (lkb::edge-score edge))
                do
                  (write-preference (or purge data)
                                    (pairlis '(:parse-id 
                                               :t-version :result-id)
                                             (list parse-id
                                                   version id))
                                    :cache cache)))

          (when (and (lkb::decision-p recent)
                     (member (lkb::decision-type recent) '(:reject :select)))
            (let* ((version (or version 1))
                   (state (encode-discriminant-state recent))
                   (type (encode-discriminant-type recent))
                   (start (lkb::compare-frame-start frame))
                   (end (lkb::compare-frame-end frame))
                   (time (let ((time (lkb::decision-time recent)))
                           (if time
                             (decode-time time :long t)
                             (current-time)))))
              #-:expand
              (write-decision data 
                              (pairlis '(:parse-id :version 
                                         :d-state :d-type :d-key :d-value 
                                         :d-start :d-end :d-date)
                                       (list parse-id version 
                                             state type nil nil 
                                             start end time))
                              :cache cache)))
          
          (loop
              with version = (or version 1)
              for discriminant in (lkb::compare-frame-discrs frame)
              for state = (encode-discriminant-state discriminant)
              for type = (encode-discriminant-type discriminant)
              for key = (lkb::discr-key discriminant)
              for value = (lkb::discr-value discriminant)
              for start = (lkb::discr-start discriminant)
              for end = (lkb::discr-end discriminant)
              for time = (let ((time (lkb::discr-time discriminant)))
                           (if time
                             (decode-time time :long t)
                             (current-time)))
              unless (= state 5)
              do
                #+:expand nil
                #-:expand
                (write-decision data 
                                (pairlis '(:parse-id :version 
                                           :d-state :d-type :d-key :d-value 
                                           :d-start :d-end :d-date)
                                         (list parse-id version 
                                               state type key value 
                                               start end time))
                                :cache cache)))
                 
        (pairlis '(:status) (list status))))))

(defun encode-discriminant-state (discriminant)
  (cond
   ((lkb::discr-p discriminant)
    (let ((toggle (lkb::discr-toggle discriminant))
          (state (lkb::discr-state discriminant)))
      (cond
       ((eq toggle t) 1)
       ((null toggle) 2)
       ((eq state t) 3)
       ((null state) 4)
       (t 5))))
   ((lkb::decision-p discriminant)
    -1)
   (t -1)))

(defun encode-discriminant-type (discriminant)
  (cond
   ((lkb::discr-p discriminant)
    (case (lkb::discr-type discriminant)
      (:rel 1)
      (:type 2)
      (:constituent 3)
      (t 0)))
   ((lkb::decision-p discriminant)
    (case (lkb::decision-type discriminant)
      (:select 4)
      (:reject 5)
      (t -1)))
   (t -1)))

(defun reconstruct-discriminants (decisions)
  (loop
      for decision in decisions
      for state = (get-field :d-state decision)
      for type = (get-field :d-type decision)
      for key = (get-field :d-key decision)
      for value = (get-field :d-value decision)
      for start = (get-field :d-start decision)
      for end = (get-field :d-end decision)
      for discriminant = (and state type key value start end
                              (not (minus-one-p type))
                              (reconstruct-discriminant 
                               state type key value start end))
      when discriminant collect discriminant))

(defun reconstruct-discriminant (istate type key value start end)
  (let* ((type (cond 
                ((eq type 1) :rel)
                ((eq type 2) :type)
                ((eq type 3) :constituent)
                ((eq type 4) :select)
                ((eq type 5) :reject)
                (t nil)))
         (toggle :unknown)
         (state :unknown))
    (cond
     ((eq istate 1) (setf toggle t) (setf state t))
     ((eq istate 2) (setf toggle nil) (setf state nil))
     ((eq istate 3) (setf state t))
     ((eq istate 4) (setf state nil)))
    (lkb::make-discr :type (intern type :keyword) 
                     :key key :value value 
                     :start start :end end
                     :toggle toggle :state state)))

(defun analyze-trees (&optional (data *tsdb-data*)
                      &key (condition *statistics-select-condition*)
                           file append (format :latex)
                           meter)
  (let* ((stream (create-output-stream file append))
         (items (if (stringp data) 
                  (analyze-aggregates data :condition condition :trees t
                                      :meter meter :format format) 
                  data))
         (averages 
          (summarize-competence-parameters items))
         (total (get-field :total averages)))
    
    (format
     stream
     "`~a'~%~%  ~
      ~d items; ~d results; ~
      ~,2f tokens; ~,2f words (~,2f); ~,2f readings;~%    ~
      rejected: ~d [~,2f ~,2f (~,2f) ~,2f]~%    ~
      unambiguous: ~d [~,2f ~,2f (~,2f) ~,2f]~%    ~
      ambiguous: ~d [~,2f ~,2f (~,2f) ~,2f]~%    ~
      unannotated: ~d [~,2f ~,2f (~,2f) ~,2f]~%"
     data
     (get-field :items total)
     (get-field :results total)
     (get-field :i-length total)
     (get-field :words total)
     (divide (get-field :words total) (get-field :i-length total))
     (get-field :analyses total)
     (get-field :rresults total)
     (get-field :rlength total)
     (get-field :rwords total)
     (divide (get-field :rwords total) (get-field :rlength total))
     (get-field :ranalyses total)
     (get-field :uresults total)
     (get-field :ulength total)
     (get-field :uwords total)
     (divide (get-field :uwords total) (get-field :ulength total))
     (get-field :uanalyses total)
     (get-field :aresults total)
     (get-field :alength total)
     (get-field :awords total)
     (divide (get-field :awords total) (get-field :alength total))
     (get-field :aanalyses total)
     (get-field :sresults total)
     (get-field :slength total)
     (get-field :swords total)
     (divide (get-field :swords total) (get-field :slength total))
     (get-field :sanalyses total))))

(defun marty (data &key condition)
  
  (loop
      with items = (analyze data :thorough '(:derivation) 
                            :condition condition)
      for item in items
      for i-id = (get-field :i-id item)
      for parse-id = (get-field :parse-id item)
      for trees = (select '("t-version") '(:integer) "tree" 
                          (format nil "parse-id == ~a" parse-id) 
                          data
                          :sort :parse-id)
      for version = (loop
                        for tree in trees
                        maximize (get-field :t-version tree))
      for active = (let ((foo (select '("result-id") '(:integer) "preference" 
                                      (format 
                                       nil 
                                       "parse-id == ~a && t-version == ~d" 
                                       parse-id version) 
                                      data)))
                     (loop for bar in foo collect (get-field :result-id bar)))
      for results = (get-field :results item)
      do
        (format 
         t 
         "marty(): ~d tree~p for item # ~d.~%" 
         (length active) (length active) i-id)
        
        (loop
            for i from 1
            for result in results
            for id = (get-field :result-id result)
            for derivation = (when (member id active :test #'eql)
                               (get-field :derivation result))
            for edge = (and derivation (reconstruct derivation))
            for foo = (when edge
                        (with-standard-io-syntax
                          (let ((*package* lkb::*lkb-package*))
                            (write-to-string
                             (lkb::compute-derivation-tree edge) 
                             :case :downcase))))
            for mrs = (and edge (mrs::get-mrs-string edge))
            when edge do
              (format
               t
               "~d (~d); derivation:~%~a~%~d (~d); MRS string:~%~a~%~%"
               id i foo id i mrs))))

(defun kristina (data &key condition path prefix)
  
  (loop
      with target = (or path 
                        (format nil "/home/tmp/~a" (substitute #\. #\/ data)))
      with *reconstruct-cache* = (make-hash-table :test #'eql)
      with items = (analyze data :thorough '(:derivation) 
                            :condition condition)
      for item in items
      for input = (or (get-field :o-input item) (get-field :i-input item))
      for i-id = (get-field :i-id item)
      for parse-id = (get-field :parse-id item)
      for results = (get-field :results item)
      for trees = (select '("t-version") '(:integer) "tree" 
                          (format nil "parse-id == ~a" parse-id) 
                          data
                          :sort :parse-id)
      for version = (when trees
                      (loop
                          for tree in trees
                          maximize (get-field :t-version tree)))
      for active = (when version
                     (let ((foo (select '("result-id") '(:integer) 
                                        "preference" 
                                        (format 
                                         nil 
                                         "parse-id == ~a && t-version == ~d" 
                                         parse-id version) 
                                        data)))
                       (loop 
                           for bar in foo 
                           collect (get-field :result-id bar))))
      initially 
        #+(and :null :allegro) (excl::delete-directory-and-files target)
        #+:allegro (mkdir target)
      do
        (format 
         t 
         "kristina(): ~a active tree~@[s~] (of ~d) for item # ~d.~%" 
         (if version (length active) "all")
         (or (null version) (> (length active) 1))
         (length results) i-id)

        (clrhash *reconstruct-cache*)
        (with-open-file (stream (format 
                                 nil 
                                 "~a/~@[~a.~]~d" 
                                 target prefix i-id)
                         :direction :output
                         :if-exists :supersede :if-does-not-exist :create)
          (format 
           stream
           "[~d: ~a of ~d] `~a'~%~a~%"
           i-id 
           (if version (length active) "all")
           (length results) input #\page)
          #-:null
          (loop
              with *package* = (find-package lkb::*lkb-package*)
	      with lkb::*deleted-daughter-features* = nil
              for i from 1
              for result in results
              for id = (get-field :result-id result)
              for derivation = (when (member id active :test #'eql)
                                 (get-field :derivation result))
              for edge = (and derivation (reconstruct derivation))
              for dag = (and edge (lkb::tdfs-indef (lkb::edge-dag edge)))
              when dag do
                (format stream "~s~%" derivation)
                (lkb::display-dag1 dag 'lkb::tdl stream)
                (format stream "~%")
                (format stream "~c~%" #\page))
          #-:null
          (loop
              with *package* = (find-package lkb::*lkb-package*)
	      with lkb::*deleted-daughter-features* = nil
              for result in results
              for id = (get-field :result-id result)
              for derivation = (unless (member id active :test #'eql)
                                 (get-field :derivation result))
              for edge = (and derivation (reconstruct derivation))
              for dag = (and edge (lkb::tdfs-indef (lkb::edge-dag edge)))
              when dag do
                (format stream "~s~%" derivation)
                (lkb::display-dag1 dag 'lkb::tdl stream)
                (format stream "~%")
                (format stream "~c~%" #\page)))))

(defun score-heuristics (&key (profiles '("trees/vm6/ezra/01-07-19"
                                          "trees/vm13/ezra/01-07-30"
                                          "trees/vm31/ezra/01-08-03"
                                          "trees/vm32/ezra/01-07-16"))
                              (condition *statistics-select-condition*))
  
  (loop
      with tsum = 0 with msum = 0
      for data in profiles
      for total = (length (select '("parse-id") nil
                                  '("parse")
                                  (format 
                                   nil 
                                   "(t-active == 1)~@[ && (~a)~]"
                                   condition)
                                  data))
      for match = (length (select '("parse-id") nil
                                  '("parse" "preference")
                                  (format
                                   nil
                                   "(t-active == 1 && result-id == 0)~
                                    ~@[ && (~a)~]"
                                   condition)
                                  data))
      do
        (incf tsum total) (incf msum match)
      finally
        (format
         t
         "total: ~d; correct: ~d: ~,2f~%"
         tsum msum (/ msum tsum))))
