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

(defparameter *redwoods-semantix-hook* nil)

(defparameter *redwoods-trees-hook* nil)

(defparameter %redwoods-increment% 100)

(defparameter %model% nil)

(defun browse-trees (&optional (data *tsdb-data*)
                     &key (condition *statistics-select-condition*)
                          gold strip inspect 
                          (bestp *redwoods-thinning-normalize-p*)
                          (cache *tsdb-cache-database-writes-p*)
                          (verbose t) interactive
                          (stream *tsdb-io*)
                          (runp t) interrupt meter)

  (declare (optimize (speed 3) (safety 0) (space 0)))

  (initialize-tsdb)
  (when strip
    (unless (do-import-database (find-tsdb-directory data) strip 
                                :meter (when meter (make-meter 0 1))
                                :except (append
                                         '("tree" "decision" "preference")
                                         (and bestp '("result"))))
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
                   (cond
                    (strip "normalizing")
                    (gold "updating")
                    (t "browsing"))
                   data))
         (items (sort (copy-list items) 
                      #'< :key #'(lambda (foo) (get-field :i-id foo))))
         (schema (read-database-schema data))
         (cache (when cache
                  (create-cache (or strip data)
                                :schema schema :verbose verbose 
                                :protocol cache)))
         (gc-strategy 
          (unless (or interactive (null runp))
            (install-gc-strategy 
             nil :tenure *tsdb-tenure-p* :burst t :verbose t)))
         (frame (unless #-:expand strip #+:expand nil
                  (if runp
                    (clim:make-application-frame 'lkb::compare-frame)
                    (clim:make-application-frame 
                     'lkb::compare-frame :frame-manager nil))))
                    
         %client%)
    (declare (special %client%))

    #+:debug
    (setf lkb::%frame% frame)
    (setf (lkb::compare-frame-ids frame) 
      (loop for item in items collect (get-field :i-id item)))
    
    (when (functionp *statistics-result-filter*)
      (setf items
        (loop
            for item in items
            for result = (funcall *statistics-result-filter* item)
            when result collect result)))
    
    (when meter
      (status :text message)
      (meter :value 0))
    (when runp
      (loop
          with last = nil
          with increment = (and meter (/ 1 (if items (length items) 1)))
          with title = (format 
                        nil 
                        "[incr tsdb()] Tree ~a (`~a'~
                         ~:[~*~; from `~a'~])~@[ @ `~a'~]" 
                        (if gold "Update" "Annotation")
                        data gold gold condition)
          with nitems = (length items)
          with annotated = (make-array nitems :initial-element 0)
          with position = 0
          initially
            (unless #-:expand strip #+:expand nil
              (setf (lkb::compare-frame-chart frame) nil)
              (setf (clim:frame-pretty-name frame) title)
              (setf (lkb::compare-frame-controller frame) *current-process*))
          for item = (when position (nth position items))
          for i-id = (get-field :i-id item)
          for status = (when (integerp i-id) 
                         (or 
                          #+:allegro
                          (when *tsdb-tenure-p*
                            (excl:tenuring
                             (browse-tree 
                              data i-id frame 
                              :gold gold :strip strip :bestp bestp 
                              :inspect inspect
                              :cache cache :title title
                              :verbose verbose :stream stream)))
                          (browse-tree 
                           data i-id frame 
                           :gold gold :strip strip :bestp bestp 
                           :inspect inspect
                           :cache cache :title title
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
              (:previous 
               (decf position offset)
               (close-connections :data data))
              ((:skip :null :flag)
               (if (eq last :previous) (decf position) (incf position))
               (setf action last))
              ((:next :save) 
               (when (eq action :save) (incf (aref annotated position)))
               (incf position offset))
              (:last (setf position (- nitems 1))))
            (setf last action)
            (when (or (>= position nitems) (< position 0))
              (setf position nil))
            (purge-profile-cache data :expiryp nil)
            (when gold (purge-profile-cache gold  :expiryp nil))
          when (interrupt-p interrupt) do
            (format 
             stream
             "browse-trees(): external interrupt signal~%")
            (force-output stream)
            (return))

      (when frame 
        ;;
        ;; according to section 9.6 of the CLIM User Guide, frame-exit() cannot
        ;; have an effect unless called from the process running the top-level
        ;; of that frame.
        ;;
        (clim:frame-exit frame)
        #+:null
        (clim:destroy-frame frame))
      (when (mp:process-p %client%)
        (mp:process-kill %client%))

      (when meter
        (status :text (format nil "~a done" message) :duration 10)
        (meter :value 1)))
  
    (when cache (flush-cache cache :verbose verbose))
    (when gc-strategy (restore-gc-strategy gc-strategy))
    (purge-profile-cache data :expiryp nil)
    
    frame))

(defun browse-tree (data i-id frame &key gold strip bestp inspect 
                                         title cache verbose 
                                         (runp t) stream)
  
  (declare (special %client%))

  (when (or (null runp)
            (null %client%)
            (and (mp:process-p %client%) (mp:process-active-p %client%)))
    #+:allegro
    (format
     excl:*initial-terminal-io*
     "~&[~a] browse-tree(): `~a' ~@[(~a) ~]--- item # ~a~%"
     (current-time :long :short) data gold i-id)

    (let* ((*reconstruct-cache* (make-hash-table :test #'eql))
           (lkb::*tree-update-match-hook* #'update-match-p)
           (lkb::*tree-automatic-update-p* 
            (when gold lkb::*tree-automatic-update-p*))
           (condition (format nil "i-id = ~a" i-id))
           (items (analyze data :thorough '(:derivation) :condition condition))
           (item (and (null (rest items)) (first items)))
           (input (or (get-field :o-input item) (get-field :i-input item)))
           (i-id (get-field :i-id item))
           (readings (get-field :readings item))
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
                         "(~a) ~a on ~a: ~a (~a)"
                         version user date confidence foo)
                        "")))
           (edges (unless (or #-:expand strip #+:expand (null trees))
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
                        for edge = (when derivation
                                     (reconstruct derivation :word))
                        when edge do 
                          (setf (lkb::edge-foo edge) id)
                          (setf (lkb::edge-bar edge) derivation)
                          (push edge edges)
                        finally
                          #+:allegro
                          (format
                           excl:*initial-terminal-io*
                           "~&[~a] browse-tree(): reconstructed ~a edge~p.~%"
                           (current-time :long :short) 
                           (length edges) (length edges))
                          (return (nreverse edges)))))
           (edges (sort edges #'< :key #'lkb::edge-foo))
           (foo (first edges))
           (start (and foo (lkb::edge-from foo)))
           (end (and foo (lkb::edge-to foo)))
           (decisions (when (and parse-id version)
                        (select '("parse-id" "t-version"
                                  "d-state" "d-type" "d-key" "d-value" 
                                  "d-start" "d-end" "d-date")
                                '(:integer :integer
                                  :integer :integer :string :string 
                                  :integer :integer :date)
                                "decision" 
                                (format 
                                 nil 
                                 "parse-id == ~a && t-version == ~a" 
                                 parse-id version) 
                                data)))
           (discriminants (unless #-:expand strip #+:expand nil
                            #+:allegro
                            (format
                             excl:*initial-terminal-io*
                             "~&[~a] browse-tree(): retrieved ~a decision~p.~%"
                             (current-time :long :short)
                             (length decisions) (length decisions))
                            (reconstruct-discriminants decisions)))
           (greadings (when (and gold parse-id (null strip))
                        (let ((items (select 
                                      '("readings") '(:integer) "parse" 
                                      (format nil "i-id == ~a" i-id)
                                      gold)))
                          (when (= (length items) 1)
                            (get-field :readings (first items))))))
           (gtrees (when (and gold parse-id (null strip))
                     (select '("parse-id" "t-version" 
                               "t-active" "t-author" "t-end")
                             '(:integer :integer 
                               :integer :string :date)
                             "tree" 
                             (format nil "parse-id == ~a" parse-id) 
                             gold
                             :sort :parse-id)))
           (gversion (loop
                         for tree in gtrees
                         maximize (get-field :t-version tree)))
           (gtrees (loop
                       for tree in gtrees
                       when (eq gversion (get-field :t-version tree))
                       collect tree))
           (gactive (when (= (length gtrees) 1)
                      (let ((gactive (get-field :t-active (first gtrees))))
                        (unless (minus-one-p gactive) gactive))))
           (gitem (when (and gactive (= readings 1))
                    (first
                     (analyze 
                      gold :thorough '(:derivation) :condition condition))))
           (gpreferences (when (and gitem (= (length gtrees) 1))
                           (select '("parse-id" "t-version" "result-id")
                                   '(:integer :integer :integer)
                                   "preference" 
                                   (format 
                                    nil 
                                    "parse-id == ~a && t-version == ~a" 
                                    parse-id gversion) 
                                   gold)))
           (gderivation (when (= (length gpreferences) 1)
                          (loop
                              with gpreference = (first gpreferences)
                              with key = (get-field :result-id gpreference)
                              for result in (get-field :results gitem)
                              for id = (get-field :result-id result)
                              thereis (when (= id key)
                                        (get-field :derivation result)))))
           (ghistory (when (and (integerp greadings) (integerp gactive))
                       (let* ((guser (get-field :t-author (first gtrees)))
                              (gdate (get-field :t-end (first gtrees))))
                         (format 
                          nil 
                          "(~a) ~a on ~a; [~a : ~a] active"
                          gversion guser gdate 
                          gactive (- greadings gactive)))))
           (gdecisions (when (and gold gversion)
                         #+:allegro
                         (format
                          excl:*initial-terminal-io*
                          "~&[~a] browse-tree(): retrieved ~a gold tree~p.~%"
                          (current-time :long :short) 
                          (length gtrees) (length gtrees))
                         (select '("parse-id" "t-version"
                                   "d-state" "d-type" "d-key" "d-value" 
                                   "d-start" "d-end" "d-date")
                                 '(:integer :integer
                                   :integer :integer :string :string 
                                   :integer :integer :date)
                                 "decision" 
                                 (format 
                                  nil 
                                  "parse-id == ~a && t-version == ~a" 
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
           (version (max (if version version 0) (if gversion gversion 0)))
           (lkb::*parse-record* edges))
      (declare (ignore active))

      (when strip
        #-:expand
        (loop
            with preferences = (select '("parse-id" "t-version" "result-id")
                                       '(:integer :integer :integer)
                                       "preference" 
                                       (format 
                                        nil 
                                        "parse-id == ~a && t-version == ~a" 
                                        parse-id version) 
                                       data)
            for preference in preferences
            do 
              (write-preference strip preference :cache cache)
            finally
              (when (and trees bestp)
                (let* ((ids (loop
                                for preference in preferences
                                collect (get-field :result-id preference)))
                       (condition (format
                                   nil
                                   "parse-id == ~a~
                                    ~@[ && (~{result-id == ~a~^ ||~})~]"
                                   parse-id ids))
                       (schema (read-database-schema data))
                       (relation (loop
                                     for (relation . structure) in schema
                                     when (string= relation "result")
                                     return structure))
                       (fields (loop
                                   for field in relation 
                                   collect (first field)))
                       (types (loop
                                  for field in relation 
                                  collect (second field)))
                       (results (select fields types "result" condition data)))
                  (when (or *redwoods-semantix-hook* *redwoods-trees-hook*)
                    (loop
                        for result in results
                        for derivation = (get-field :derivation result)
                        for edge = (when derivation (reconstruct derivation))
                        for mrs = (when (and edge *redwoods-semantix-hook*)
                                    (call-hook *redwoods-semantix-hook* edge))
                        for tree = (when (and edge *redwoods-trees-hook*)
                                     (call-hook *redwoods-trees-hook* edge))
                        when mrs do (setf (get-field :mrs result) mrs)
                        when tree do (setf (get-field :tree result) tree)))
                  (write-results parse-id results strip :cache cache))))
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
        #-:expand
        (return-from browse-tree (acons :status :save nil)))

      (when (null edges)
        (when verbose
          (format
           stream
           "browse-tree(): failed to reconstruct item # ~d (parse # ~d).~%"
           i-id parse-id))
        (return-from browse-tree (acons :status :null nil)))
      
      (setf (lkb::compare-frame-edges frame) nil)
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
      (setf (lkb::compare-frame-gversion frame) ghistory)
      (setf (lkb::compare-frame-gactive frame) gactive)
      (setf (lkb::compare-frame-gderivation frame) gderivation)
      (setf (lkb::compare-frame-inspect frame) inspect)
      (setf (lkb::compare-frame-update frame)
        (when (and gactive greadings)
          (pairlis '(:parse-id :u-gin :u-gout)
                   (list parse-id gactive (- greadings gactive)))))
      
      (when (and (null %client%) runp)
        (setf %client%
          (mp:run-function 
           (or title "[incr tsdb()] Tree Selection")
           #'lkb::run-compare-frame frame)))

      (let ((status (lkb::set-up-compare-frame 
                     frame lkb::*parse-record* :runp runp)))
        #+:expand
        (lkb::record-decision (lkb::make-decision :type :save) frame)
        #-:expand
        (unless (or (eq status :skip) (null runp))
          (process-add-arrest-reason *current-process* :wait)))
      
      (when runp
        (let* ((decisions (lkb::compare-frame-decisions frame))
               (status (lkb::decision-type (first decisions)))
               (recent (second decisions)))

          (when (and (eq status :flag) (null trees))
            (let* ((user (current-user))
                   (time (current-time :long :tsdb))
                   (tree (pairlis '(:parse-id 
                                    :t-version :t-active :t-confidence
                                    :t-author :t-start :t-end :t-comment)
                                  (list parse-id 
                                        0 -1 -1
                                        user time time ""))))
              (write-tree strip tree :cache cache)))

          (when (eq status :save)
            (let* ((version (if version 
                              #-:expand (incf version) #+:expand version
                              1))
                   (edges (lkb::compare-frame-in frame))
                   (active (length edges))
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
              #-:expand
              (write-tree data (pairlis '(:parse-id 
                                          :t-version :t-active :t-confidence
                                          :t-author :t-start :t-end :t-comment)
                                        (list parse-id 
                                              version active confidence
                                              t-author t-start t-end ""))
                          :cache cache)

              (loop
                  for edge in edges
                  for id = (when (lkb::edge-p edge) (lkb::edge-foo edge))
                  do
                    (write-preference data
                                      (pairlis '(:parse-id 
                                                 :t-version :result-id)
                                               (list parse-id
                                                     version id))
                                      :cache cache)))

            #-:expand
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
                                (pairlis '(:parse-id :t-version 
                                           :d-state :d-type :d-key :d-value 
                                           :d-start :d-end :d-date)
                                         (list parse-id version 
                                               state type nil nil 
                                               start end time))
                                :cache cache)))
            #-:expand
            (loop
                with version = (or version 1)
                for discriminant in (lkb::compare-frame-discriminants frame)
                for state = (encode-discriminant-state discriminant)
                for type = (encode-discriminant-type discriminant)
                for key = (lkb::discriminant-key discriminant)
                for value = (lkb::discriminant-value discriminant)
                for start = (lkb::discriminant-start discriminant)
                for end = (lkb::discriminant-end discriminant)
                for time = (let ((time (lkb::discriminant-time discriminant)))
                             (if time
                               (decode-time time :long :tsdb)
                               (current-time :long :tsdb)))
                unless (= state 5)
                do
                  (write-decision data 
                                  (pairlis '(:parse-id :t-version 
                                             :d-state :d-type :d-key :d-value 
                                             :d-start :d-end :d-date)
                                           (list parse-id version 
                                                 state type key value 
                                                 start end time))
                                  :cache cache))
            #-:expand
            (let* ((update (lkb::compare-frame-update frame))
                   (discriminants (lkb::compare-frame-discriminants frame))
                   (decisions 
                    (loop
                        for foo in discriminants
                        for bar = (lkb::discriminant-toggle foo)
                        count (and (null (lkb::discriminant-gold foo))
                                   (not (eq bar :unknown)))))
                   (in (length (lkb::compare-frame-in frame)))
                   (out (length (lkb::compare-frame-out frame))))
              (when (and update (>= (profile-granularity data) 0210))
                (write-update 
                 data (append
                       (pairlis '(:t-version :u-new :u-in :u-out)
                                (list (or version 1) decisions in out))
                       update)
                 :cache cache))))

          (pairlis '(:status) (list status)))))))

(defun encode-discriminant-state (discriminant)
  (cond
   ((lkb::discriminant-p discriminant)
    (let ((toggle (lkb::discriminant-toggle discriminant))
          (state (lkb::discriminant-state discriminant)))
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
   ((lkb::discriminant-p discriminant)
    (case (lkb::discriminant-type discriminant)
      (:relation 1)
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
                ((eq type 1) :relation)
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
    (lkb::make-discriminant :type (intern type :keyword) 
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
         (ncolumns 17)
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
        "flags 2~%~
         layout col def -m1 5 -r 1 -m2 5 -c black -j right~%~
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
                    k label k k label))))
      (:html
       (format stream "<table  cellspacing=0>~%")
       (html-output "redwoods-annotations-header.html" :stream stream
                    :values (list alabel))))
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
              i (get-field :sanalyses data)))
            (:html
             (html-output
              (if (= i 3)
                "redwoods-annotations-body-first.html"
                "redwoods-annotations-body.html")
              :stream stream
              :values (list 
                       name
                       (get-field :results data) (get-field :i-length data)
                       (get-field :analyses data)
                       (get-field :rresults data) (get-field :rlength data)
                       (get-field :ranalyses data)
                       (get-field :uresults data) (get-field :ulength data)
                       (get-field :uanalyses data)
                       (get-field :aresults data) (get-field :alength data)
                       (get-field :aanalyses data)
                       (get-field :sresults data) (get-field :slength data)
                       (get-field :sanalyses data))))))
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
          n (get-field :sanalyses total)))
        (:html
         (html-output
          "redwoods-annotations-total.html" :stream stream
          :values (list 
                   name
                   (get-field :results total) (get-field :i-length total)
                   (get-field :analyses total)
                   (get-field :rresults total) (get-field :rlength total)
                   (get-field :ranalyses total)
                   (get-field :uresults total) (get-field :ulength total)
                   (get-field :uanalyses total)
                   (get-field :aresults total) (get-field :alength total)
                   (get-field :aanalyses total)
                   (get-field :sresults total) (get-field :slength total)
                   (get-field :sanalyses total)))
         (format
          stream
          "<tr>~%  ~
           <td class=\"itsdb@caption\" colspan=~a align=right>~%    ~
           ~a~%  </td>~%</tr>~%</table>~%"
          ncolumns caption)))

      #+:debug
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

(defun analyze-update (&optional (data *tsdb-data*)
                       &key (condition *statistics-select-condition*)
                            file append (format :latex)
                            meter)
  (declare (ignore meter))

  (let* ((stream (create-output-stream file append))
         (averages (summarize-update data :condition condition :format format))
         (naggregates (length averages))
         (alabel "Aggregate")
         (ncolumns 11)
         #+:latex
         (caption (format 
                   nil "(generated by ~a at ~a)"
                   *tsdb-name* (current-time :long :pretty))))
    
    (when (or (null averages) (= naggregates 1))
      (return-from analyze-update 1))
    
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
         layout col 2 -m1 5 -r 2 -m2 5 -c black -j right~%~
         layout col 4 -m1 5 -r 2 -m2 5 -c black -j right~%~
         layout col 6 -m1 5 -r 2 -m2 5 -c black -j right~%~
         layout col 8 -m1 5 -r 2 -m2 5 -c black -j right~%~
         layout col 9 -m1 5 -r 2 -m2 5 -c black -j right~%~
         layout col ~a -m1 5 -r 2 -m2 5 -c black -j right~%~
         layout row 0 -m1 5 -r 2 -m2 5 -c black -j center~%~
         layout row 2 -m1 5 -r 2 -m2 5 -c black -j center~%~
         layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%~
         layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%~%"
        ncolumns
        (+ naggregates 1) (+ naggregates 2))
       (format
        stream
        "cell 1 1 -contents {~a} -format title~%~
         region 1 1 2 1 -contents {~a} -format title ~
           -hor_justify left -ver_justify center~%~
         region 1 3 1 4 -contents {gold} ~
           -format title -hor_justify center~%~
         region 1 5 1 6 -contents {matches} ~
           -format title -hor_justify center~%~
         region 1 7 1 8 -contents {update} ~
           -format title -hor_justify center~%~
         region 1 9 1 9 -contents {} ~
           -format title -hor_justify center~%~
         region 1 10 1 11 -contents {final} ~
           -format title -hor_justify center~%"
        alabel alabel)
       (loop
           for i from 2 to 11
           for label in '("items\\n#" "in\\n\\330" "out\\n\\330"
                          "yes\\n\\330" "no\\n\\330"
                          "in\\n\\330" "out\\n\\330"
                          "new\\n\\330"
                          "in\\n\\330" "out\\n\\330")
           do
             (format
              stream
              "cell 2 ~d -contents \"~a\" -format title~%~
               region 2 ~d 2 ~d -contents \"~a\" -format title ~
                 -hor_justify center~%"
              i label i i label))))
    (loop
        with i = 2
        for aggregate in (remove :total averages :key #'first)
        for data = (rest (rest aggregate))
        for name = (second aggregate)
        do
          (incf i)
          (case format
            (:tcl
             (format
              stream
              "cell ~d 1 -contents {~a} -format aggregate~%~
               cell ~d 2 -contents ~d -format data~%~
               cell ~d 3 -contents ~,1f -format data~%~
               cell ~d 4 -contents ~,1f -format data~%~
               cell ~d 5 -contents ~,1f -format data~%~
               cell ~d 6 -contents ~,1f -format data~%~
               cell ~d 7 -contents ~,1f -format data~%~
               cell ~d 8 -contents ~,1f -format data~%~
               cell ~d 9 -contents ~,1f -format data~%~
               cell ~d 10 -contents ~,1f -format data~%~
               cell ~d 11 -contents ~,1f -format data~%"
              i name i (get-field :nitems data)
              i (get-field :gin data) i (get-field :gout data)
              i (get-field :matches data) i (get-field :mismatches data)
              i (get-field :pin data) i (get-field :pout data)
              i (get-field :new data)
              i (get-field :in data) i (get-field :out data)))))
    (let* ((data (rest (rest (assoc :total averages))))
           (name "Total")
           (i (+ naggregates 2)))
      (case format
        (:tcl
         (format
          stream
          "cell ~d 1 -contents {~a} -format aggregate~%~
           cell ~d 2 -contents ~d -format data~%~
           cell ~d 3 -contents ~,1f -format data~%~
           cell ~d 4 -contents ~,1f -format data~%~
           cell ~d 5 -contents ~,1f -format data~%~
           cell ~d 6 -contents ~,1f -format data~%~
           cell ~d 7 -contents ~,1f -format data~%~
           cell ~d 8 -contents ~,1f -format data~%~
           cell ~d 9 -contents ~,1f -format data~%~
           cell ~d 10 -contents ~,1f -format data~%~
           cell ~d 11 -contents ~,1f -format data~%"
          i name i (get-field :nitems data)
          i (get-field :gin data) i (get-field :gout data)
          i (get-field :matches data) i (get-field :mismatches data)
          i (get-field :pin data) i (get-field :pout data)
          i (get-field :new data)
          i (get-field :in data) i (get-field :out data)))))
    
    (when (or (stringp file) (stringp append)) (close stream))
    0))

(defun summarize-update (data &key condition format)
  (loop
      with result = nil
      with tnitems = 0
      with tmatches = 0
      with tmismatches = 0
      with tnew = 0
      with tgin = 0
      with tgout = 0
      with tpin = 0
      with tpout = 0
      with tin = 0
      with tout = 0
      with items = (select '("parse-id" "t-version" 
                             "u-matches" "u-mismatches" "u-new"
                             "u-gin" "u-gout" "u-pin" "u-pout"
                             "u-in" "u-out")
                           '(:integer :integer
                             :integer :integer :integer 
                             :integer :integer :integer :integer  
                             :integer :integer)
                           "update" condition data :sort :parse-id)
      with aggregates = (aggregate-by-classes 
                         items '(0 1 2) 
                         :dimension :u-new :format format)
      for aggregate in aggregates
      for data = (rest (rest aggregate))
      for anitems = (length data)
      for amatches = 0
      for amismatches = 0
      for anew = 0
      for agin = 0
      for agout = 0
      for apin = 0
      for apout = 0
      for ain = 0
      for aout = 0
      do
        (loop
            for item in data
            do
              (incf amatches (get-field :u-matches item))
              (incf amismatches (get-field :u-mismatches item))
              (incf anew (get-field :u-new item))
              (incf agin (get-field :u-gin item))
              (incf agout (get-field :u-gout item))
              (incf apin (get-field :u-pin item))
              (incf apout (get-field :u-pout item))
              (incf ain (get-field :u-in item))
              (incf aout (get-field :u-out item)))
        (push (append
               (list (first aggregate) (second aggregate))
               (pairlis '(:nitems :matches :mismatches :new 
                          :gin :gout :pin :pout :in :out)
                        (list anitems 
                              (divide amatches anitems)
                              (divide amismatches anitems)
                              (divide anew anitems)
                              (divide agin anitems)
                              (divide agout anitems)
                              (divide apin anitems)
                              (divide apout anitems)
                              (divide ain anitems)
                              (divide aout anitems))))
              result)
        (incf tnitems anitems)
        (incf tmatches amatches) (incf tmismatches amismatches)
        (incf tnew anew)
        (incf tgin agin) (incf tgout agout)
        (incf tpin apin) (incf tpout apout)
        (incf tin ain) (incf tout aout)
      finally (return 
                (nreverse
                 (cons
                  (append 
                   (list :total "Total")
                   (pairlis '(:nitems :matches :mismatches :new
                              :gin :gout :pin :pout :in :out)
                            (list tnitems 
                                  (divide tmatches tnitems)
                                  (divide tmismatches tnitems)
                                  (divide tnew tnitems)
                                  (divide tgin tnitems)
                                  (divide tgout tnitems)
                                  (divide tpin tnitems)
                                  (divide tpout tnitems)
                                  (divide tin tnitems)
                                  (divide tout tnitems))))
                  result)))))

(defun update-match-p (frame)
  ;;
  ;; during updates, a `save' match is indicated by the following conditions:
  ;;
  ;;   - the current item has not been tree annotated already;
  ;;   - the number of active trees in the current set equals the number of
  ;;     active trees in the gold set;
  ;;   - either the current item has more than one reading, or that single one
  ;;     reading has the exact same derivation as the preferred tree from the
  ;;     gold set.
  ;;
  (and (or (null (lkb::compare-frame-version frame))
           (equal(lkb::compare-frame-version frame) ""))
       (integerp (lkb::compare-frame-gactive frame))
       (= (length (lkb::compare-frame-in frame)) 
          (lkb::compare-frame-gactive frame))
       (or (not (= (length (lkb::compare-frame-edges frame)) 1))
           (derivation-equal 
            (lkb::compare-frame-gderivation frame)
            (loop
                with id = (lkb::edge-id (first (lkb::compare-frame-in frame)))
                for derivation in (lkb::compare-frame-derivations frame)
                thereis (and (= (derivation-id derivation) id) derivation))))))

(defun export-trees (data &key (condition *statistics-select-condition*)
                               path prefix interrupt meter 
                               (compressor "gzip -c -9") (suffix "gz")
                               (stream *tsdb-io*))
  
  (loop
      with offset = (cond
                     ((search "vm6" data) 60000)
                     ((search "vm13" data) 130000)
                     ((search "vm31" data) 310000)
                     ((search "vm32" data) 320000)
                     (t 0))
      with target = (format 
                     nil 
                     "~a/~a"
                     (or path "/lingo/oe/tmp") (directory2file data))
      with *reconstruct-cache* = (make-hash-table :test #'eql)
      with items = (analyze data :thorough '(:derivation) :condition condition)
      with increment = (when (and meter items)
                         (/ (- (get-field :end meter) (get-field :start meter))
                            (length items) 1))
      with gc-strategy = (install-gc-strategy 
                          nil :tenure *tsdb-tenure-p* :burst t :verbose t)

      initially
        #+:allegro (ignore-errors (mkdir target))
        (when meter (meter :value (get-field :start meter)))
      for item in items
      for i-wf = (get-field :i-wf item)
      for input = (or (get-field :o-input item) (get-field :i-input item))
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
      for file = (format 
                  nil 
                  "~a/~@[~a.~]~d~@[.~a~]" 
                  target prefix (+ parse-id offset) suffix)
      do
        (format 
         stream 
         "[~a] export-trees(): [~a] ~a active tree~:[~;s~] (of ~d).~%" 
         (current-time :long :short)
         (+ parse-id offset)
         (if version (length active) "all")
         (or (null version) (> (length active) 1))
         (length results))
        (clrhash *reconstruct-cache*)
        
        #+:allegro
        (multiple-value-bind (stream foo pid)
            (run-process
             compressor :wait nil :input :stream
             :output file :if-output-exists :supersede
             :error-output nil)
          (declare (ignore foo #-:allegro pid))

          (format
           stream
           ";;;~%;;; Redwoods export of `~a';~%;;; (~a@~a; ~a).~%;;;~%~%"
           data (current-user) (current-host) (current-time :long :pretty))
          (format 
           stream
           "[~d] (~a of ~d) {~d} `~a'~%~a~%"
           (+ parse-id offset)
           (if version (length active) "all") (length results) i-wf
           input #\page)
          
          (setf (get-field :results item) (nreverse results))
          (export-tree item active :offset offset :stream stream)
          (unless *redwoods-thinning-export-p*
            (export-tree item active 
                         :complementp t :offset offset :stream stream))

          (force-output stream)
          (close stream)
          (sys:os-wait nil pid))
        
        (when increment (meter-advance increment))
      when (interrupt-p interrupt) do
        (format 
         stream
         "[~a] export-trees(): external interrupt signal~%"
         (current-time :long :short))
        (force-output stream)
        (return)
      finally
        (when meter (meter :value (get-field :end meter)))
        (when gc-strategy (restore-gc-strategy gc-strategy))))

(defun export-tree (item active 
                    &key complementp (offset 0) (stream *tsdb-io*))

  #+:debug
  (setf %item% item %active% active)
  (loop
      with *package* = (find-package :lkb)
      with lkb::*deleted-daughter-features* = 
        (if (or (eq *redwoods-export-values* :all)
                (smember :avm *redwoods-export-values*))
          nil
          lkb::*deleted-daughter-features*)
      with parse-id = (get-field :parse-id item)
      with results = (get-field :results item)
      for i from 1
      for result in results
      for result-id = (get-field :result-id result)
      for derivation = (when (if complementp
                               (not (member result-id active :test #'eql))
                               (member result-id active :test #'eql))
                         (get-field :derivation result))
      for edge = (and derivation (reconstruct derivation))
      for tree = (and edge (lkb::parse-tree-structure edge))
      for dag = (and edge (lkb::tdfs-indef (lkb::edge-dag edge)))
      for mrs = (and edge (mrs::extract-mrs edge))
      when (zerop (mod i 100)) do (clrhash *reconstruct-cache*)
      when dag do
        (format 
         stream 
         "[~d:~d] ~:[(active)~;(inactive)~]~%~%" 
         (+ parse-id offset) result-id complementp)
        (setf lkb::*cached-category-abbs* nil)
        (when (or (eq *redwoods-export-values* :all)
                  (smember :derivation *redwoods-export-values*))
          (let ((*package* (find-package :tsdb)))
            (format stream "~s~%~%~%" derivation)))
        (when (or (eq *redwoods-export-values* :all)
                  (smember :tree *redwoods-export-values*))
          (if tree
            (format stream "~a~%~%" tree)
            (format stream "()~%~%")))
        (when (or (eq *redwoods-export-values* :all)
                  (smember :avm *redwoods-export-values*))
          (lkb::display-dag1 dag 'lkb::compact stream)
          (format stream "~%~%"))
        (when (or (eq *redwoods-export-values* :all)
                  (smember :mrs *redwoods-export-values*))
          (mrs::output-mrs1 mrs 'mrs::simple stream))
        (when (and (not (eq *redwoods-export-values* :all))
                   (smember :indexed *redwoods-export-values*))
          (mrs::output-mrs1 mrs 'mrs::indexed stream))
        (when (or (eq *redwoods-export-values* :all)
                  (smember :prolog *redwoods-export-values*))
          (mrs::output-mrs1 mrs 'mrs::prolog stream)
          (format stream "~%"))
        (when (or (eq *redwoods-export-values* :all)
                  (smember :dependencies *redwoods-export-values*))
          (mrs::ed-output-psoa mrs :stream stream))
        (format stream "~c~%" #\page)))

(defun semantic-equivalence (data &key condition (file "/tmp/equivalences"))
  
  (loop
      with stream = (open file :direction :output :if-exists :supersede)
      with *reconstruct-cache* = (make-hash-table :test #'eql)
      with items = (analyze data :thorough '(:derivation) 
                            :condition condition :readerp nil)
      for item in items
      for i-id = (get-field :i-id item)
      for input = (or (get-field :o-input item) (get-field :i-input item))
      for results = (nreverse (copy-list (get-field :results item)))
      do
        (clrhash *reconstruct-cache*)
        (format t "~a: [~a] `~a'~%" i-id (length results) input)
        (format stream "~a: [~a] `~a'~%" i-id (length results) input)
        (loop
            with *package* = (find-package :lkb)
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

(defun analyze-scores (data 
                       &optional (gold data)
                       &key (condition *statistics-select-condition*)
                            spartanp (scorep t) (n 1)  test loosep
                            file append (format :latex)
                            meter)

  (let* ((stream (create-output-stream file append))
         (aggregates (summarize-scores
                      data gold :condition condition 
                      :spartanp spartanp :scorep scorep :n n
                      :test test :loosep loosep
                      :format format :meter meter))
         (aggregates (nreverse aggregates))
         (alabel (if (eq *statistics-aggregate-dimension* :phenomena)
                   "Phenomenon"
                   "Aggregate"))
         (caption (format 
                   nil "(generated by ~a at ~a)"
                   *tsdb-name* (current-time :long :pretty)))
         (n (if (and n (> n 1)) (- n 1) 0))
         (ncolumns (+ n (if loosep 8 7)))
         (i 2))
                   
    (case format
      (:latex
        (format
         stream
         "\\begin{tabular}{@{}|l|c|c|c|c|c|c|c|@{}}~%  ~
          \\hline~%  ~
          \\multicolumn{~d}{|c|}~%    {\\bf `~a' ~a Profile}\\\\~%  ~
          \\hline\\hline~%  ~
          & {\\bf  total} & {\\bf total} & {\\bf word} ~
            & {\\bf parser}~%    ~
            & {\\bf exact} & {\\bf near}~:[~; & {\\bf loose}~]~
            & {\\bf overall}\\\\~%  ~
          {\\bf ~a} & {\\bf items} & {\\bf scores} & {\\bf string} ~
            & {\\bf analyses}~%    ~
            & {\\bf matches} & {\\bf matches}~:[~; & {\\bf matches}~]~%    ~
            & {\\bf accuracy}\\\\~%  ~
          & $\\sharp$ & $\\sharp$ & $\\phi$ & $\\phi$~%    ~
            & $\\sharp$ & $\\sharp$~:[~; & $\\sharp$~] & $\\%$\\\\~%  ~
          \\hline~%  ~
          \\hline~%"
         ncolumns
         (if (stringp data) data "Some") "Parse Selection"
         loosep 
         alabel loosep 
         loosep))
      (:tcl
       (format stream *statistics-tcl-formats*)
       (format
        stream
        "flags 2~%~
         layout col def -m1 5 -r 1 -m2 5 -c black -j right~%~
         layout row def -m1 5 -r 0 -m2 5 -c black -j center~%~
         layout col 0 -m1 5 -r 2 -m2 5 -c black -j left~%~
         layout col 1 -m1 5 -r 2 -m2 5 -c black -j left~%~
         layout col ~d -m1 5 -r 2 -m2 5 -c black -j right~%~
         layout row 0 -m1 5 -r 2 -m2 5 -c black -j center~%~
         layout row 1 -m1 5 -r 2 -m2 5 -c black -j center~%"
        ncolumns)
       (format
        stream
        "cell 1 1 -contents {~a} -format title~%~
         cell 1 2 -contents \"total\\nitems\\n#\" -format title~%~
         cell 1 3 -contents \"total\\nscores\\n#\" -format title~%~
         cell 1 4 -contents \"word\\nstring\\n\\330\" -format title~%~
         cell 1 5 -contents \"parser\\nanalyses\\n\\330\" -format title~%~
         cell 1 6 -contents \"exact\\nmatches\\n#\" -format title~%~
         ~:[~*~;cell 1 ~d -contents \"loose\\nmatches\\n#\" -format title~%~]~
         cell 1 ~d -contents \"overall\\naccuracy\\n%\" -format title~%"
        alabel loosep (+ 7 n) ncolumns)
       (unless (zerop n)
         (loop
             for j from 0 to (- n 2)
             do
               (format
                stream
                "layout col ~d -m1 5 -r 0 -m2 5 -c black -j right~%"
                (+ 7 j)))

         (format
          stream
          "~:[~;cell 1 7 -contents \"near\\nmatches\\n#\" -format title~%~]~
           region 1 7 1 ~d -contents \"near\\nmatches\\n#\" -format title ~
           -hor_justify center -ver_justify center~%"
          (= n 1) (+ 7 (- n 1))))
       (format stream "~%")))

    (loop
        for (id foo . data) in aggregates
        for name = (if (eq format :latex) (latexify-string foo) foo)
        for items = (get-field :items data)
        for scores = (get-field :scores data)
        for length = (get-field+ :i-length data 0)
        for analyses = (get-field :analyses data)
        for exact = (get-field :exact data)
        for near = (get-field :near data)
        for successes = (get-field :successes data)
        for loose = (and loosep (get-field :loose data))
        for accuracy = (if (zerop scores)
                         100
                         (* (divide (+ exact near) scores) 100))
        unless (or (smember id '(:all :total)) (zerop scores)) do
          (setf id id)
          (case format
            (:latex
             (format
              stream
              "  ~a & ~d & ~d & ~,2f & ~,2f ~
               & ~,1f & ~,1f~@[ & ~d~] & ~,2f\\\\~%"
              name items scores length analyses 
              exact near loose accuracy))
            (:tcl
             (format
              stream
              "cell ~d 1 -contents {~a} -format aggregate~%~
               cell ~d 2 -contents ~d -format data~%~
               cell ~d 3 -contents ~d -format data~%~
               cell ~d 4 -contents ~,2f -format data~%~
               cell ~d 5 -contents ~,2f -format data~%~
               cell ~d 6 -contents ~,1f -format data~%~
               ~:[~*~*~*~;cell ~d ~d -contents ~d -format data~%~]~
               cell ~d ~d -contents ~,2f -format data~%"
              i name
              i items
              i scores
              i length
              i analyses
              i exact
              loosep i (+ 7 n) loose
              i ncolumns accuracy)
             (unless (zerop n)
               (loop
                   for j from 0 to (- n 1)
                   for k = (aref successes (+ j 1))
                   do
                     (format
                      stream
                      "cell ~d ~d -contents ~,1f -format data~%"
                      i (+ 7 j) k)))
             (format stream "~%")))
          (incf i))
    
    (let* ((data (rest (rest (assoc :total aggregates))))
           (name "Total")
           (items (get-field :items data))
           (scores (get-field :scores data))
           (length (get-field+ :i-length data 0))
           (analyses (get-field :analyses data))
           (exact (get-field :exact data))
           (near (get-field :near data))
           (successes (get-field :successes data))
           (loose (and loosep (get-field :loose data)))
           (accuracy (if (zerop scores)
                       100
                       (* (divide (+ exact near) scores) 100))))
      (case format
        (:latex
         (format
          stream
          "~:[~;  \\hline~%  \\hline~%~]  ~
           {\\bf ~a} & {\\bf ~d} & {\\bf ~d} & {\\bf ~,2f} & {\\bf ~,2f}~%    ~
           & {\\bf ~,1f} & {\\bf ~,1f}~@[ & {\\bf ~d}~] & {\\bf ~,2f}\\\\~%  ~
           \\hline~%"
          (= i 1)
          name items scores length analyses exact near loose accuracy)
         (format
          stream
          "  \\multicolumn{~d}{r}{\\tiny ~%    ~a}~%~
           \\end{tabular}~%"
          ncolumns caption))
        (:tcl
         (format
          stream
          "layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%~
           layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%~%~
           cell ~d 1 -contents {~a} -format total~%~
           cell ~d 2 -contents ~d -format total~%~
           cell ~d 3 -contents ~d -format total~%~
           cell ~d 4 -contents ~,2f -format total~%~
           cell ~d 5 -contents ~,2f -format total~%~
           cell ~d 6 -contents ~,1f -format total~%~
           ~:[~*~*~*~;cell ~d ~d -contents ~d -format total~%~]~
           cell ~d ~d -contents ~,2f -format total~%"
          (- i 1) i
          i name
          i items
          i scores
          i length
          i analyses
          i exact
          loosep i (+ 7 n) loose
          i ncolumns accuracy)
         (unless (zerop n)
           (loop
               for j from 0 to (- n 1)
               for k = (aref successes (+ j 1))
               do
                 (format
                  stream
                  "cell ~d ~d -contents ~,1f -format total~%"
                  i (+ 7 j) k)))
         (format stream "~%"))))
    (when (or (stringp file) (stringp append)) (close stream))))

(defun summarize-scores (data &optional (gold data)
                         &key (condition *statistics-select-condition*)
                              spartanp (scorep t) (n 1) test loosep
                              (format :latex) meter)
  (declare (ignore meter))
  
  ;;
  ;; score results in .data. against ground truth in .gold.  operates in
  ;; several slightly distinct modes: (i) using the implicit parse ranking in
  ;; the order of `results' or (ii) using an explicit ranking from the `score'
  ;; relation an orthogonal dimension of variation is (a) scoring by result
  ;; identifier (e.g. within the same profile or against one that is comprised
  ;; of identical results) vs. (b) scoring by derivation equivalence (e.g.
  ;; when comparing best-first parser output against a gold standard).
  ;;
  (let* ((thorough (when (eq test :derivation) '(:derivation)))
         (items (if (stringp data)
                  (analyze (if spartanp gold data)
                           :thorough (or thorough spartanp)
                           :condition condition :score (if scorep data t)
                           :readerp (eq test :derivation))
                  data))
         (aggregates (aggregate items :format format))
         (gitems (if (stringp gold)
                   (analyze gold
                            :thorough thorough
                            :condition condition :gold gold 
                            :readerp (eq test :derivation))
                   gold))
         (gaggregates (aggregate-by-analogy gitems aggregates :loosep t))
         results)

    #+:debug
    (setf %aggregates% aggregates %gaggregates% gaggregates)

    (loop
        with tnitems = 0
        with tnscores = 0
        with tlength = 0
        with treadings = 0
        with texact = 0
        with tnear = 0
        with tloose = 0
        with tsuccesses = (and n (make-array n :initial-element 0))
        for (id name . data) in aggregates
        for gaggregate = (when (equal (first (first gaggregates)) id)
                           (pop gaggregates))
        for gdata = (rest (rest gaggregate))
        when gdata do
          (loop
              with anitems = 0
              with anscores = 0
              with alength = 0
              with areadings = 0
              with aexact = 0
              with anear = 0
              with aloose = 0
              with asuccesses = (and n (make-array n :initial-element 0))
              for item in data
              for i-id = (get-field :i-id item)
              for length = (get-field :i-length item)
              for readings = (get-field :readings item)
              for gitem = (loop
                              for gitem = (first gdata)
                              while (and gitem
                                         (< (get-field :i-id gitem) i-id))
                              do (pop gdata)
                              finally
                                (return 
                                  (let ((i (get-field :i-id (first gdata))))
                                    (when (and i (= i i-id))
                                      (pop gdata)))))
              when gitem do
                (multiple-value-bind (i score loosep)
                    (score-item item gitem :test test :n n :loosep loosep)
                  (incf anitems)
                  (cond
                   ((null i))
                   ((zerop i)
                    (incf anscores) 
                    (incf alength length) (incf areadings readings))
                   (t
                    (incf anscores) 
                    (incf alength length) (incf areadings readings)
                    (when (<= i n)
                      (if (= i 1) (incf aexact score) (incf anear score)))
                    (when loosep (incf aloose))
                    (when asuccesses 
                      (incf (aref asuccesses (- i 1)) score)))))
              finally
                (incf tnitems anitems) (incf tnscores anscores)
                (incf tlength alength) (incf treadings areadings) 
                (incf texact aexact) (incf tnear anear) (incf tloose aloose)
                (loop
                    for i from 0
                    for j across asuccesses
                    do
                      (incf (aref tsuccesses i) j))
                (push (nconc (list id name)
                             (pairlis '(:items :scores 
                                        :i-length 
                                        :analyses
                                        :exact :near :loose 
                                        :successes)
                                      (list anitems anscores 
                                            (divide alength anscores)
                                            (divide areadings anscores)
                                            aexact anear aloose 
                                            asuccesses)))
                      results))
        finally
          (push (nconc (list :total "Total")
                       (pairlis '(:items :scores 
                                  :i-length
                                  :analyses
                                  :exact :near :loose 
                                  :successes)
                                (list tnitems tnscores 
                                      (divide tlength tnscores)
                                      (divide treadings tnscores)
                                      texact tnear tloose 
                                      tsuccesses)))
                results))
    (when (eq test :derivation)
      (purge-profile-cache data :expiryp nil)
      (unless (equal data gold) (purge-profile-cache gold :expiryp nil)))

    results))

(defun score-item (item gold &key test (n 1) (loosep t) errorp)
  
  #+:debug
  (setf %item% item %gold% gold)
  
  (let ((ranks (get-field :ranks item))
        (granks (get-field :ranks gold))
        (test (cond
               ((functionp test) test)
               ((or (null test) (eq test :id))
                #'(lambda (old new)
                    (let ((foo (get-field :result-id old))
                          (bar (get-field :result-id new)))
                      (and foo bar (= foo bar)))))
               ((eq test :derivation)
                #'(lambda (old new)
                    (let ((foo (get-field :derivation old))
                          (bar (get-field :derivation new)))
                      (and foo bar (derivation-equal foo bar))))))))
                
    (cond
     ((or (null ranks) (null granks)) nil)
     ((loop
          for grank in granks
          for rank = (get-field :rank grank)
          thereis (or (not (integerp rank)) (not (= rank 1))))
      nil)
     ((and (rest granks) (null loosep)) nil)
     (t
      (loop
          with result = nil with best = nil
          for grank in granks
          for match = (loop
                          for rank in ranks
                          for i = (get-field :rank rank)
                          while (or (null n) errorp (<= i n))
                          thereis (and (funcall test rank grank) rank))
          for i = (get-field :rank match)
          for matches = (when i
                          (loop
                              for rank in ranks
                              for j = (get-field :rank rank)
                              while (<= j i)
                              when (= i j) collect rank))
          for errors = (when errorp
                         (if i
                           (loop
                               for rank in ranks
                               for j = (get-field :rank rank)
                               while (< j i) collect rank)
                           ranks))
          when (and (numberp i)
                    (or (null result)
                        (< i (get-field :rank best))
                        (and (= i (get-field :rank best)) 
                             (< (length matches) (length result)))))
          do
            (setf best match)
            (setf result matches)
          finally 
            (return (values (get-field+ :rank best 0)
                            (divide 1 (length result)) 
                            (rest granks) 
                            errors match (delete match result))))))))

(defun analyze-errors (data 
                       &optional (gold data)
                       &key (condition *statistics-select-condition*)
                            spartanp (scorep t) (n 1)  test loosep
                            file append (format :tcl)
                            meter)
  (declare (ignore meter))

  (let* ((errors (summarize-errors data gold :condition condition
                                   :spartanp spartanp :scorep scorep :n n
                                   :test test :loosep loosep))
         (stream (create-output-stream file append)))
    (when (listp errors)
      (case format
        (:tcl
         (when *statistics-tcl-formats* 
           (format stream *statistics-tcl-formats*))
         (format
          stream
          "flags 0~%~
           layout col def -m1 5 -r 1 -m2 5 -c black -j right~%~
           layout row def -m1 5 -r 0 -m2 5 -c black -j center~%~
           layout col 0 -m1 5 -r 2 -m2 5 -c black -j right~%~
           layout col 1 -m1 5 -r 2 -m2 5 -c black -j right~%~
           layout col 2 -m1 5 -r 2 -m2 5 -c black -j left~%~
           layout col 4 -m1 5 -r 2 -m2 5 -c black -j right~%~
           layout col 7 -m1 5 -r 2 -m2 5 -c black -j right~%~
           layout row 0 -m1 5 -r 2 -m2 5 -c black -j center~%~
           layout row 2 -m1 5 -r 2 -m2 5 -c black -j center~%")
         (format
          stream
          "cell 1 1 -contents {i-id} -format title~%~
           region 1 1 2 1 -contents {i-id} ~
             -format title -hor_justify center~%~
           cell 1 2 -contents {i-input} -format title~%~
           region 1 2 2 2 -contents {i-input} ~
             -format title -hor_justify center~%~
           cell 1 3 -contents {xxxx} -format title~%~
           cell 1 4 -contents {xxxx} -format title~%~
           region 1 3 2 4 -contents {readings} ~
             -format title -hor_justify center~%")
         (format
          stream
          "region 1 5 1 7 -contents {scores} ~
             -format title -hor_justify center~%~
           cell 2 5 -contents {<} -format title~%~
           cell 2 6 -contents {H(p)} -format title~%~
           region 2 6 2 6 -contents {H(p)} ~
             -format title -hor_justify center~%~
           cell 2 7 -contents {=} -format title~%")))
      (loop
          for (item gitem rank errors match others entropy) in errors
          for i-id = (get-field :i-id item)
          for i-input = (or (get-field :o-input item)
                            (get-field :i-input item))
          for greadings = (get-field :readings gitem)
          for readings = (get-field :readings item)
          for tag = (intern (gensym "") :keyword)
          with i = 3
          when (zerop (mod (- i 2) 10))
          do 
            (case format
              (:tcl
               (format
                stream
                "layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%"
                i)))
          do
            ;;
            ;; _fix_me_ this creates a potential memory leak: as soon as the
            ;; window for this table is destroyed, there will be no further
            ;; reference to the (tag) symbols used to store data on the lisp
            ;; side.  yet, the values associated with the symbol properties
            ;; will never become unbound.                         (16-feb-03)
            ;;
            (setf rank rank)
            (setf (get :source tag) gold)
            (setf (get :i-id tag) i-id)
            (setf (get :i-input tag) i-input)
            (setf (get :match tag) match)
            (setf (get :errors tag) errors)
            (setf (get :others tag) others)
            (format
             stream
             "cell ~d 1 -contents {~a} -format data~%~
              cell ~d 2 -contents {~a} -format data -key ~d -source {~a}~%~
              cell ~d 3 -contents {~a} -format data~%~
              cell ~d 4 -contents {~a} -format data~%~
              cell ~d 5 -contents {~a} -format data~%~
              cell ~d 6 -contents {~,4f} -format data ~
                -action inspect -tag ~a~%~
              cell ~d 7 -contents {~a} -format data~%"
             i i-id 
             i i-input i-id data
             i greadings
             i readings
             i (length errors)
             i entropy tag
             i (length others))
            (incf i)
          finally
            (when (> i 3)
              (case format
                (:tcl
                 (format
                  stream
                  "layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%~
                   cell ~d 1 -contents {~a} -format total~%~
                   layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%"
                  (- i 1) i (- i 3) i))))))
    
    (when (or (stringp file) (stringp append)) (close stream))
    (if (listp errors) 0 -1)))

(defun summarize-errors (data 
                         &optional (gold data)
                         &key (condition *statistics-select-condition*)
                              spartanp (scorep t) (n 1) test loosep
                              meter)
  (declare (ignore meter))
  
  (let* ((thorough (when (eq test :derivation) '(:derivation)))
         (items (if (stringp data)
                   (analyze (if spartanp gold data)
                            :thorough (or thorough spartanp)
                            :condition condition :score (if scorep data t)
                            :readerp (eq test :derivation) :scorep t)
                  data))
         (items (sort (copy-list items) #'< 
                      :key #'(lambda (foo) (get-field :i-id foo))))
         (gitems (if (stringp gold)
                   (analyze gold
                            :thorough thorough
                            :condition condition :gold gold 
                            :readerp (eq test :derivation) :scorep t)
                   gold))
         (gitems (sort (copy-list gitems) #'< 
                       :key #'(lambda (foo) (get-field :i-id foo))))
         result)

    (loop
        for item in items
        for gitem in gitems
        for i-id = (get-field :i-id item)
        for gi-id = (get-field :i-id gitem)
        when (or (not (numberp i-id)) (not (numberp gi-id))
                 (not (= i-id gi-id))) 
        do
          (setf result :error)
          (return)
        else do
          (multiple-value-bind (rank foo bar errors match others)
              (score-item item gitem :test test :n n :loosep loosep :errorp t)
            (declare (ignore foo bar))
            (when (and rank (or (zerop rank) (> rank n) others))
              (let* ((ranks (when (or errors others)
                              (get-field :ranks item)))
                     (scores (loop 
                                 for rank in ranks
                                 collect (get-field :score rank)))
                     (probabilities (scores-to-probabilities scores))
                     (entropy (entropy probabilities)))
                (push (list item gitem rank errors match others entropy)
                      result)))))
    (if (listp result) (nreverse result) result)))

(defun train (sources file &key (condition *statistics-select-condition*)
                                (type :mem) model (estimatep t) (recursep t)
                                (verbose t) (stream t)
                                interrupt meter)
  (declare (ignore interrupt))

  (cond
   ((consp sources)
    (loop
        with model = (or model 
                         (case type
                           (:mem (let ((model (make-mem)))
                                   (initialize-mem model)
                                   model))))
        with gc = (install-gc-strategy 
                   nil :tenure *tsdb-tenure-p* :burst t :verbose verbose)
        with condition = (if (and condition (not (equal condition "")))
                           (format nil "t-active >= 1 && (~a)" condition)
                           "t-active >= 1")
        with meter = (when meter (madjust / meter (length sources)))
        with duration = (when meter (mduration meter))
        for i from 0
        for sources on sources
        for rmeter = (when duration (madjust + meter (* duration i)))
        do
          (train (first sources) nil :condition condition :type type
                 :model model :estimatep (null (rest sources))
                 :verbose verbose :stream stream :meter rmeter)
        finally
          (when verbose
            (format stream "train(): exporting ~a~%" model))
          (print-mem model :file file :format :export)
          (restore-gc-strategy gc)
          (setf %model% model)
          (return model)))
   (recursep
    (format 
     t 
     "[~a] train(): reading `~a'~%" 
     (current-time :long :short) sources)
    (loop
        with delta = %redwoods-increment%
        with n = (ceiling (tcount sources "item") delta)
        with increment = (when meter (/ (mduration meter) n))
        for i from 1 to n
        for foo = (format 
                   nil 
                   "i-id >= ~d && i-id < ~d~@[ && (~a)~]"
                   (* (- i 1) delta) (* i delta) condition)
        initially (when meter (meter :value (get-field :start meter)))
        do
          (when meter (meter-advance increment))
          (train sources nil :condition foo :type type
                 :model model :estimatep (and estimatep (= i n))
                 :recursep nil :verbose verbose :stream stream)
        finally (when meter (meter :value (get-field :end meter)))))
   (t
    (let ((items (analyze sources :gold sources :condition condition
                          :thorough '(:derivation) :readerp nil)))
      (purge-profile-cache sources :expiryp nil)
      (case type
        (:mem
         (estimate-mem 
          items :model model :estimatep estimatep :stream stream)))))))

(defun rank-profile (source 
                     &optional (target source)
                     &key (condition *statistics-select-condition*)
                          (nfold 10) (type :mem) model
                          (stream *tsdb-io*) (cache :raw) (verbose t)
                          interrupt meter)
  
  (declare (ignore meter))
  
  (format
   stream
   "~&[~a] rank-profile:() `~a' -->~%                           `~a'~%"
   (current-time :long :short) source target)

  (purge-test-run target :action :score)
  (loop
      with gc = (install-gc-strategy 
                 nil :tenure *tsdb-tenure-p* :burst t :verbose t)
      with cache = (create-cache target :verbose verbose :protocol cache)
      with condition = (if (and condition (not (equal condition "")))
                         (format nil "t-active >= 1 && (~a)" condition)
                         "t-active >= 1")
      with data = (analyze source 
                           :thorough '(:derivation)
                           :condition condition :gold source :readerp nil)
      with nfold = (if model 1 (min (length data) nfold))
      #+:debug initially #+:debug (setf %data% data)
      for i from 1 to (if (>= nfold 1) nfold 1)
      when (interrupt-p interrupt) do
        (format 
         stream
         "[~a] rank-profile(): external interrupt signal~%"
         (current-time :long :short))
        (flush-cache cache :verbose verbose)
        (restore-gc-strategy gc)
        (return)
      do
        (multiple-value-bind (test train) 
            (ith-nth data i (if (zerop nfold) 10 nfold))
          (when (null train) (setf train test))
          (when (and test train)
            (format
             stream
             "~&[~a] rank-profile:() iteration # ~d (~d against ~d)~%"
             (current-time :long :short) i (length test) (length train))
            (loop
                with items = (train-and-rank 
                              train test :type type :model model)
                for item in items
                for parse-id = (get-field :parse-id item)
                for ranks = (get-field :ranks item)
                do
                  (loop
                      for foo in ranks
                      for result-id = (get-field :result-id foo)
                      for score = (let ((score (get-field :score foo)))
                                    (if score (format nil "~,16f" score) ""))
                      for rank = (get-field :rank foo)
                      do
                        #+:debug
                        (format
                         stream
                         "  parse: ~a; result: ~d; rank: ~d; score: ~a~%"
                         parse-id result-id rank score)
                        (write-score target (pairlis '(:parse-id :result-id
                                                       :rank :score)
                                                     (list parse-id result-id
                                                           rank score))
                                     :cache cache)))))
      finally 
        (flush-cache cache :verbose verbose)
        (restore-gc-strategy gc)
        (purge-profile-cache source :expiryp nil)
        (purge-profile-cache target)))

(defun train-and-rank (train test 
                       &key (type :mem) model (stream *tsdb-io*))

  (loop
      with model = (or model
                       (case type
                         (:pcfg (estimate-cfg train))
                         (:mem (estimate-mem train))
                         (:chance "chance")))
      for item in test
      for iid = (get-field :i-id item)
      for readings = (get-field :readings item)
      for results = (get-field :results item)
      for ranks = nil
      initially 
        (format
         stream
         "~&[~a] train-and-rank(): using ~a;~%"
         (current-time :long :short)  model)
        #+:debug (setf %model% model)
      when (and (integerp readings) (> readings 1)) do
        (format 
         stream
         "~&[~a] train-and-rank(): item # ~a (~a reading~p);~%"
         (current-time :long :short) iid readings readings)
        (loop
            with *reconstruct-cache* = (make-hash-table :test #'eql)
            for result in results
            for id = (get-field :result-id result)
            for derivation = (get-field :derivation result)
            for edge = (unless (eq type :chance) (reconstruct derivation nil))
            for score = (and (or edge (eq type :chance))
                             (case type
                               (:pcfg (pcfg-score-edge edge model))
                               (:mem (mem-score-edge edge model))
                               (:chance 0.0)))
            when (and (null edge) (not (eq type :chance))) do 
              (format 
               stream
               "~&[~a] train-and-rank(): ignoring this item (no edge);~%"
               (current-time :long :short))
              (setf ranks nil) (return)
            else do 
              (push (nconc (pairlis '(:result-id :score)
                                    (list id score))
                           result)
                    ranks))
      and collect 
        (let* ((ranks (sort ranks #'> 
                            :key #'(lambda (foo) (get-field :score foo))))
               (ranks (loop
                          with last = (get-field :score (first ranks))
                          with i = 1
                          for rank in ranks
                          for j from 1
                          for score = (get-field :score rank)
                          unless (= score last) do
                            (setf i j) (setf last score)
                          collect (acons :rank i rank))))
          (nconc (acons :ranks ranks nil) item))))
