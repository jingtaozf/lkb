(in-package :tsdb)

;;;
;;; ToDo
;;;
;;; - no need to save yield (`value') for discriminants; recompute from start
;;;   and end indices into full string;
;;; - protect against saving with an empty `decision' set;
;;; - fill in `preference' relation;
;;; - confidence menu;
;;; - `Reset' button: re-instantiate original, preset state;
;;; - reorder trees: active at top;
;;; - pairwise comparison of trees;
;;; - highlighting of discriminants on tree select;
;;; - highlighting of trees on discriminant select;
;;; - utilize status vector to, e.g. fast-forward to first unannotated;
;;; - record all :select decisions, valid at `Save' time;
;;; - add print button: include edge id in display and print out;
;;;


(defun browse-trees (&optional (data *tsdb-data*)
                     &key (condition *statistics-select-condition*)
                          gold
                          (cache *tsdb-cache-database-writes-p*)
                          (verbose t) interactive
                          (stream *tsdb-io*)
                          strip meter)

  (declare (optimize (speed 3) (safety 0) (space 0)))

  (initialize-tsdb)
  (when strip
    (unless (do-import-database (find-tsdb-directory data) strip 
                                :meter (when meter (make-meter 0 1))
                                :except '("tree" "decision"))
      (return-from browse-trees nil)))

  (let* ((condition (if (and condition (not (equal condition "")))
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
                   (if strip "normalizing" "browsing") data))
         (items (sort (copy-seq items) 
                      #'< :key #'(lambda (foo) (get-field :i-id foo))))
         (schema (read-database-schema data))
         (cache (when cache
                  (create-cache (or strip data)
                                :schema schema :verbose verbose 
                                :protocol cache)))
         (gc-strategy 
          (unless interactive
            (install-gc-strategy 
             nil :tenure *tsdb-tenure-p* :burst t :verbose t)))
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

    (loop
        with increment = (and meter (/ 1 (if items (length items) 1)))
        with frame = (unless strip
                       (clim:make-application-frame 'lkb::compare-frame))
        with title = (format 
                      nil 
                      "[incr tsdb()] Tree Selection~@[ @ `~a'~]" condition)
        with nitems = (length items)
        with annotated = (make-array nitems :initial-element 0)
        with position = 0
        initially
          #-:debug
          (setf *frame* frame)
          (unless strip
            (setf (lkb::compare-frame-current-chart frame) nil)
            (setf (clim:frame-pretty-name frame) title)
            (setf (lkb::compare-frame-controller frame) *current-process*))
        for item = (when position (nth position items))
        for i-id = (get-field :i-id item)
        for status = (when (integerp i-id) 
                       (if *tsdb-tenure-p*
                         (#+:allegro excl:tenuring #-:allegro progn
                          (browse-tree 
                           data i-id frame 
                           :gold gold
                           :cache cache :title title :strip strip
                           :verbose verbose :stream stream))
                         (browse-tree 
                          data i-id frame 
                          :gold gold
                          :cache cache :title title :strip strip
                          :verbose verbose :stream stream)))
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
                 (if strip nil (- nitems 1))
                 (+ position offset))))
            (:last (setf position (- nitems 1))))
        finally
          (when frame (clim:frame-exit frame))
          (when (mp:process-p %client%)
            (mp:process-kill %client%)))
  
    (when cache (flush-cache cache :verbose verbose))
    (when gc-strategy (restore-gc-strategy gc-strategy))
    (purge-profile-cache data)
    
    (when meter
      (status :text (format nil "~a done" message) :duration 10)
      (meter :value 1))))

(defun browse-tree (data i-id frame &key gold title cache strip verbose stream)
  
  (declare (special %client%))

  (when (or (null %client%)
            (and (mp:process-p %client%) (mp:process-active-p %client%)))
    #+:allegro
    (format
     excl:*initial-terminal-io*
     "~&[~a] browse-tree(): `~a' ~@[(~a) ~] --- item # ~a~%"
     (current-time :long :short) data gold i-id)

    (let* ((*reconstruct-cache* (make-hash-table :test #'eql))
           (condition (format nil "i-id = ~a" i-id))
           (items (analyze data :thorough '(:derivation) :condition condition))
           (item (and (null (rest items)) (first items)))
           (input (or (get-field :o-input item) (get-field :i-input item)))
           (i-id (get-field :i-id item))
           (parse-id (get-field :parse-id item))
           (results (get-field :results item))
           (trees (when parse-id
                    #+:allegro
                    (format
                     excl:*initial-terminal-io*
                     "~&[~a] browse-tree(): ~
                      retrieved item # ~a (~a parse~p).~%"
                     (current-time :long :short) i-id 
                     (length results)(length results))
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
                      (if (and (>= version 0) user date)
                        (format
                         nil
                         "~a (~a) confidence; version ~d on ~a by `~a'"
                         foo confidence version date user)
                        "")))
           (edges (unless strip
                    #+:allegro
                    (format
                     excl:*initial-terminal-io*
                     "~&[~a] browse-tree(): retrieved ~a tree record~p.~%"
                     (current-time :long :short) (length trees) (length trees))
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
                        finally
                          #+:allegro
                          (format
                           excl:*initial-terminal-io*
                           "~&[~a] browse-tree(): reconstructed ~a edge~p.~%"
                           (current-time :long :short) 
                           (length edges) (length edges))
                          (return (nreverse edges)))))
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
           (discriminants (unless strip
                            #+:allegro
                            (format
                             excl:*initial-terminal-io*
                             "~&[~a] browse-tree(): retrieved ~a decision~p.~%"
                             (current-time :long :short)
                             (length decisions) (length decisions))
                            (reconstruct-discriminants decisions)))
           (gtrees (when (and gold parse-id (null strip))
                     (select '("parse-id" "t-version" 
                               "t-active" "t-confidence" 
                              "t-author" "t-start" "t-end" "t-comment")
                             '(:integer :integer 
                               :integer :integer 
                               :string :date :date :string)
                             "tree" 
                             (format nil "parse-id == ~a" parse-id) 
                             gold
                             :sort :parse-id)))
           (gversion (loop
                         for tree in gtrees
                         maximize (get-field :t-version tree)))
           #+:null
           (gtrees (loop
                       for tree in gtrees
                       when (eq gversion (get-field :t-version tree))
                       collect tree))
           (gdecisions (when (and gold parse-id gversion)
                         #+:allegro
                         (format
                          excl:*initial-terminal-io*
                          "~&[~a] browse-tree(): retrieved ~a gold tree~p.~%"
                          (current-time :long :short) 
                          (length gtrees) (length gtrees))
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
                                  parse-id gversion) 
                                 gold)))
           (gdiscriminants (when gdecisions
                             #+:allegro
                             (format
                              excl:*initial-terminal-io*
                              "~&[~a] browse-tree(): ~
                               retrieved ~a gold decision~p.~%"
                              (current-time :long :short) 
                              (length gdecisions) (length gdecisions))
                             (reconstruct-discriminants gdecisions)))
           (lkb::*parse-record* edges))
      (declare (ignore active))

      (when strip
        (if trees
          (write-tree strip (first trees) :cache cache)
          (let* ((user (current-user))
                 (time (current-time :long :tsdb))
                 (tree (pairlis '(:parse-id 
                                  :t-version :t-active :t-confidence
                                  :t-author :t-start :t-end :t-comment)
                                (list parse-id 
                                      0 -1 -1
                                      user time time ""))))
            (write-tree strip tree :cache cache)))
        (loop
            for decision in decisions
            do (write-decision strip decision :cache cache))
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
      (setf (lkb::compare-frame-derivations frame) 
        (loop
            for result in results collect (get-field :derivation result)))
      (setf (lkb::compare-frame-version frame) history)
      (setf (lkb::compare-frame-confidence frame) confidence)
      (setf (lkb::compare-frame-preset frame) discriminants)
      (setf (lkb::compare-frame-gold frame) gdiscriminants)
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
        (unless (eq status :skip)
          (clim:redisplay-frame-panes frame :force-p t)
          (process-add-arrest-reason *current-process* :wait)))

      (let* ((decisions (lkb::compare-frame-decisions frame))
             (status (lkb::decision-type (first decisions)))
             (recent (second decisions)))
        (when (eq status :save)
          (let* ((version (if version (incf version) 1))
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
                              (decode-time start :long :tsdb)
                              (current-time :long :tsdb))))
                 (t-end (let* ((end (first decisions))
                               (end (when (lkb::decision-p end)
                                      (lkb::decision-time end))))
                          (if end 
                            (decode-time end :long :tsdb)
                            (current-time :long :tsdb)))))
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
                  (write-preference (or strip data)
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
                             (decode-time time :long :tsdb)
                             (current-time :long :tsdb)))))
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
                             (decode-time time :long :tsdb)
                             (current-time :long :tsdb)))
              unless (= state 5)
              do
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
         (averages (remove 0 averages 
                           :key #'(lambda (foo)
                                    (get-field :results (rest foo)))))
         (naggregates (- (length averages) 1))
         (alabel (if (eq *statistics-aggregate-dimension* :phenomena)
                   "Phenomenon"
                   "Aggregate"))
         #+:latex
         (ncolumns 17)
         #+:latex
         (caption (format 
                   nil "(generated by ~a at ~a)"
                   *tsdb-name* (current-time :long :pretty))))
    
    (when (or (null items) (null averages))
      (return-from analyze-trees 1))
    
    (case format
      (:tcl
       (when *statistics-tcl-formats* 
         (format stream *statistics-tcl-formats*))
       (format
        stream
        "layout col def -m1 5 -r 1 -m2 5 -c black -j right~%~
         layout row def -m1 5 -r 0 -m2 5 -c black -j center~%~
         layout col 0 -m1 5 -r 2 -m2 5 -c black -j right~%~
         layout col 1 -m1 5 -r 2 -m2 5 -c black -j left~%~
         layout col 4 -m1 5 -r 2 -m2 5 -c black -j right~%~
         layout col 7 -m1 5 -r 2 -m2 5 -c black -j right~%~
         layout col 10 -m1 5 -r 2 -m2 5 -c black -j right~%~
         layout col 13 -m1 5 -r 2 -m2 5 -c black -j right~%~
         layout col 16 -m1 5 -r 2 -m2 5 -c black -j right~%~
         layout row 0 -m1 5 -r 2 -m2 5 -c black -j center~%~
         layout row 2 -m1 5 -r 2 -m2 5 -c black -j center~%~
         layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%~
         layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%~%"
        (+ naggregates 2) (+ naggregates 3))
       (format
        stream
        "cell 1 1 -contents {~a} -format title~%~
         region 1 1 2 1 -contents {~a} -format title ~
           -hor_justify left -ver_justify center~%~
         region 1 2 1 4 -contents {all results} ~
           -format title -hor_justify center~%~
         region 1 5 1 7 -contents {t-active = 0} ~
           -format title -hor_justify center~%~
         region 1 8 1 10 -contents {t-active = 1} ~
           -format title -hor_justify center~%~
         region 1 11 1 13 -contents {t-active > 1} ~
           -format title -hor_justify center~%~
         region 1 14 1 16 -contents {unannotated} ~
           -format title -hor_justify center~%"
        alabel alabel)
       (loop
           for i from 2 to 14 by 3
           do
             (loop
                 for label in '("parses\\n#" "words\\n\\330" "trees\\n\\330")
                 for j from 0
                 for k = (+ i j)
                 do
                   (format
                    stream
                    "cell 2 ~d -contents \"~a\" -format title~%~
                     region 2 ~d 2 ~d -contents \"~a\" -format title ~
                       -hor_justify center~%"
                    k label k k label)))))
    (loop
        with i = 2
        for aggregate in (remove :all items :key #'first)
        for data = (rest (assoc (first aggregate) averages))
        for name = (if (eq format :latex)
                     (latexify-string (second aggregate))
                     (second aggregate))
        when data do
          (incf i)
          (case format
            (:tcl
             (format
              stream
              "cell ~d 1 -contents {~a} -format aggregate~%~
               cell ~d 2 -contents ~d -format data~%~
               cell ~d 3 -contents ~,2f -format data~%~
               cell ~d 4 -contents ~,1f -format data~%~
               cell ~d 5 -contents ~d -format data~%~
               cell ~d 6 -contents ~,2f -format data~%~
               cell ~d 7 -contents ~,1f -format data~%~
               cell ~d 8 -contents ~d -format data~%~
               cell ~d 9 -contents ~,2f -format data~%~
               cell ~d 10 -contents ~,1f -format data~%~
               cell ~d 11 -contents ~d -format data~%~
               cell ~d 12 -contents ~,2f -format data~%~
               cell ~d 13 -contents ~,1f -format data~%~
               cell ~d 14 -contents ~d -format data~%~
               cell ~d 15 -contents ~,2f -format data~%~
               cell ~d 16 -contents ~,1f -format data~%"
              i name
              i (get-field :results data) i (get-field :i-length data)
              i (get-field :analyses data)
              i (get-field :rresults data) i (get-field :rlength data)
              i (get-field :ranalyses data)
              i (get-field :uresults data) i (get-field :ulength data)
              i (get-field :uanalyses data)
              i (get-field :aresults data) i (get-field :alength data)
              i (get-field :aanalyses data)
              i (get-field :sresults data) i (get-field :slength data)
              i (get-field :sanalyses data)))))
    (let* ((total (rest (assoc :total averages)))
           (name "Total")
           (n (+ naggregates 3)))
      (case format
        (:tcl
         (format
          stream
          "cell ~d 1 -contents {~a} -format aggregate~%~
           cell ~d 2 -contents ~d -format data~%~
           cell ~d 3 -contents ~,2f -format data~%~
           cell ~d 4 -contents ~,1f -format data~%~
           cell ~d 5 -contents ~d -format data~%~
           cell ~d 6 -contents ~,2f -format data~%~
           cell ~d 7 -contents ~,1f -format data~%~
           cell ~d 8 -contents ~d -format data~%~
           cell ~d 9 -contents ~,2f -format data~%~
           cell ~d 10 -contents ~,1f -format data~%~
           cell ~d 11 -contents ~d -format data~%~
           cell ~d 12 -contents ~,2f -format data~%~
           cell ~d 13 -contents ~,1f -format data~%~
           cell ~d 14 -contents ~d -format data~%~
           cell ~d 15 -contents ~,2f -format data~%~
           cell ~d 16 -contents ~,1f -format data~%"
          n name
          n (get-field :results total) n (get-field :i-length total)
          n (get-field :analyses total)
          n (get-field :rresults total) n (get-field :rlength total)
          n (get-field :ranalyses total)
          n (get-field :uresults total) n (get-field :ulength total)
          n (get-field :uanalyses total)
          n (get-field :aresults total) n (get-field :alength total)
          n (get-field :aanalyses total)
          n (get-field :sresults total) n (get-field :slength total)
          n (get-field :sanalyses total))))
      
      #-:debug
      (format
       t
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
       (get-field :sanalyses total)))

    
    (when (or (stringp file) (stringp append)) (close stream))
    0))

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

(defun export-trees (data &key (condition *statistics-select-condition*)
                               path prefix)
  
  (loop
      with target = (format 
                     nil 
                     "~a/~a"
                     (or path "/lingo/oe/tmp") (substitute #\. #\/ data))
      with *reconstruct-cache* = (make-hash-table :test #'eql)
      with items = (analyze data :thorough '(:derivation) 
                            :condition condition)
      for item in items
      for input = (or (get-field :o-input item) (get-field :i-input item))
      for i-id = (get-field :i-id item)
      for parse-id = (get-field :parse-id item)
      for results = (get-field :results item)
      for trees = (select '("t-active" "t-version") '(:integer :integer) 
                          "tree" 
                          (format nil "parse-id == ~a" parse-id) 
                          data)
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
        #+:allegro (ignore-errors (mkdir target))
      do
        (format 
         t 
         "kristina(): ~a active tree~:[~;s~] (of ~d) for item # ~d.~%" 
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
          (loop
              with *package* = (find-package lkb::*lkb-package*)
	      with lkb::*deleted-daughter-features* = nil
              for i from 1
              for result in results
              for id = (get-field :result-id result)
              for derivation = (when (member id active :test #'eql)
                                 (get-field :derivation result))
              for edge = (and derivation (reconstruct derivation))
              for tree = (and edge (lkb::parse-tree-structure edge))
              for dag = (and edge (lkb::tdfs-indef (lkb::edge-dag edge)))
              for mrs = (and edge (mrs::extract-mrs edge))
              when dag do
                (setf lkb::*cached-category-abbs* nil)
                (format stream "~s~%~%" derivation)
                (if tree
                  (format stream "~a~%" tree)
                  (format stream "()~%"))
                (lkb::display-dag1 dag 'lkb::tdl stream)
                (format stream "~%~%")
                (mrs::mrs-output-psoa mrs :stream stream)
                (format stream "~c~%" #\page))
          (loop
              with *package* = (find-package lkb::*lkb-package*)
	      with lkb::*deleted-daughter-features* = nil
              for result in results
              for id = (get-field :result-id result)
              for derivation = (unless (member id active :test #'eql)
                                 (get-field :derivation result))
              for edge = (and derivation (reconstruct derivation))
              for tree = (and edge (lkb::parse-tree-structure edge))
              for dag = (and edge (lkb::tdfs-indef (lkb::edge-dag edge)))
              for mrs = (and edge (mrs::extract-mrs edge))
              when dag do
                (setf lkb::*cached-category-abbs* nil)
                (format stream "~s~%~%" derivation)
                (if tree
                  (format stream "~a~%" tree)
                  (format stream "()~%"))
                (lkb::display-dag1 dag 'lkb::tdl stream)
                (format stream "~%~%")
                (mrs::mrs-output-psoa mrs :stream stream)
                (format stream "~c~%" #\page)))))

(defun semantic-equivalence (data &key condition (file "/tmp/equivalences"))
  
  (loop
      with stream = (open file :direction :output :if-exists :supersede)
      with *reconstruct-cache* = (make-hash-table :test #'eql)
      with items = (analyze data :thorough '(:derivation) :condition condition)
      for item in items
      for i-id = (get-field :i-id item)
      for input = (or (get-field :o-input item) (get-field :i-input item))
      for results = (nreverse (copy-list (get-field :results item)))
      do
        (clrhash *reconstruct-cache*)
        (format t "~a: [~a] `~a'~%" i-id (length results) input)
        (format stream "~a: [~a] `~a'~%" i-id (length results) input)
        (loop
            with *package* = (find-package lkb::*lkb-package*)
            for result in results
            for derivation = (get-field :derivation result)
            for edge = (when derivation (reconstruct derivation))
            for id = (when edge (lkb::edge-id edge))
            for mrs = (when edge (mrs::extract-mrs edge))
            do (nconc result (pairlis '(:id :mrs) (list id mrs))))
        (loop
            for result = (pop results)
            for id1 = (get-field :id result)
            for mrs1 = (get-field :mrs result)
            while result do
              (format stream "~a:"id1)
              (loop
                  for foo in results
                  for id2 = (get-field :id foo)
                  for mrs2 = (get-field :mrs foo)
                  when (apply #'mrs::mrs-equalp mrs1 mrs2 '(t nil)) do
                    (format stream " ~a" id2))
              (format stream "~%"))
        (format stream "~a~%" #\page)
      finally (close stream)))

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

(defun qtree (tree &key (stream t))
  (let ((root (derivation-root tree))
        (daughters (derivation-daughters tree)))
    (if (null daughters)
      (format stream "\\leaf{~a}~%" root)
      (loop
          for daughter in daughters
          do
            (qtree daughter :stream stream)
          finally
             (format stream "\\branch{~d}{~a}~%" (length daughters) root)))))
