(in-package :tsdb)

;;;
;;; ToDo
;;;
;;; - confidence menu;
;;; - `Reset' button: re-instantiate original, preset state;
;;; - reorder trees: active at top;
;;; - pairwise comparison of trees;
;;; - highlighting of discriminats on tree select;
;;; - highlighting of trees on discriminant select;
;;; - utilize status vector to, e.g. fast-forward to first unannotated;
;;; - record all :select decisions, valid at `Save' time;
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

    (loop
        with increment = (and meter (/ 1 (if items (length items) 1)))
        with frame = (unless purge
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
          (unless purge
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
                 (if purge nil (- nitems 1))
                 (+ position offset))))
            (:last (setf position (- nitems 1))))
        finally
          (when frame (clim:frame-exit frame))
          (when (mp:process-p %client%)
            (mp:process-kill %client%)))
  
    (when cache (flush-cache cache :verbose verbose))

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
           (edges (unless purge
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
           (discriminants (unless purge (reconstruct-discriminants decisions)))
           (lkb::*parse-record* edges))
      (declare (ignore active))

      (when purge
        (when trees
          (write-tree purge (first trees) :cache cache))
        (loop
            for decision in decisions
            do (write-decision purge decision :cache cache))
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
        (unless (eq status :skip)
          (clim:redisplay-frame-panes frame :force-p t)
          (process-add-arrest-reason *current-process* :wait)))

      (let* ((decisions (lkb::compare-frame-decisions frame))
             (status (lkb::decision-type (first decisions)))
             (recent (second decisions)))
        (when (eq status :save)
          (let* ((version (if version (incf version) 1))
                 (active (length (lkb::compare-frame-in-parses frame)))
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
            (write-tree data (pairlis '(:parse-id 
                                        :t-version :t-active :t-confidence
                                        :t-author :t-start :t-end :t-comment)
                                      (list parse-id 
                                            version active confidence
                                            t-author t-start t-end ""))
                        :cache cache))

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





