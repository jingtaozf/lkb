(in-package :tsdb)

;;;
;;; ToDo
;;;
;;; - confidence menu
;;; - `Reset' buttom: re-instantiate original, preset state
;;; - reorder trees: active at top
;;; - pairwise comparison of trees
;;; - highlighting of discriminats on tree select
;;; - highlighting of trees on discriminant select
;;;


(defun browse-trees (&optional (data *tsdb-data*)
                     &key (condition *statistics-select-condition*)
                          (cache *tsdb-cache-database-writes-p*)
                          (verbose t)
                          meter)

  (declare (optimize (speed 3) (safety 0) (space 0)))

  (let* ((condition (if condition
                      (concatenate 'string "(readings >= 1) && " condition)
                      "readings >= 1"))
         (items
          (if (stringp data) 
            (analyze data 
                     :condition condition :meter meter :message t)
            data))
         (message (format nil "browsing `~a' trees ..." data))
         (items (sort (copy-seq items) 
                      #'< :key #'(lambda (foo) (get-field :i-id foo))))
         (schema (read-database-schema data))
         (cache (when cache
                  (create-cache data 
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
        with frame = (clim:make-application-frame 'lkb::compare-frame)
        with nitems = (length items)
        with annotated = (make-array nitems :initial-element 0)
        with position = 0
        initially
          (setf *frame* frame)
          (setf (lkb::compare-frame-current-chart frame) nil)
          (setf (clim:frame-pretty-name frame) "[incr tsdb()] Tree Selection")
          (setf (lkb::compare-frame-controller frame) *current-process*)
        for item = (nth position items)
        for i-id = (get-field :i-id item)
        for status = (and (integerp i-id) (browse-tree 
                                           data i-id frame
                                           :cache cache))
        for action = (get-field :status status)
        while (and status (not (eq action :close)))
        do 
          (when (eq action :save)
            (when (and increment (zerop (aref annotated position)))
              (meter-advance increment)))
          (let ((action (get-field :status status)))
            (case action
              (:first (setf position 0))
              (:previous (setf position (max (- position 1) 0)))
              ((:next :save) 
               (when (eq action :save) (incf (aref annotated position)))
               (setf position (min (+ position 1) (- nitems 1))))
              (:last (setf position (- nitems 1)))))
        finally
          (clim:frame-exit frame)
          (when (mp:process-p %client%)
            (mp:process-kill %client%)))
  
    (when cache (flush-cache cache :verbose verbose))

    (when meter
      (status :text (format nil "~a done" message) :duration 10)
      (meter :value 1))))

(defun browse-tree (data i-id frame &key cache)
  
  (declare (special %client%))

  (when (or (null %client%)
            (and (mp:process-p %client%) (mp:process-active-p %client%)))
    (let* ((condition (format nil "i-id = ~a" i-id))
           (items (analyze data :thorough '(:derivation) :condition condition))
           (item (and (null (rest items)) (first items)))
           (parse-id (get-field :parse-id item))
           (trees (select '("parse-id" "t-version" "t-active"
                            "t-confidence" "t-author" "t-end")
                          '(:integer :integer :integer
                            :integer :string :date)
                          "tree" 
                          (format nil "parse-id == ~a" parse-id) 
                          data
                          :sort :parse-id))
           (version (loop
                        for tree in trees
                        maximize (get-field :t-version tree)))
           (trees (loop
                      for tree in trees
                      when (eq version (get-field :t-version tree))
                      collect tree))
           (confidence (let* ((foo (get-field :t-confidence (first trees))))
                         (if (and (integerp foo) (>= foo 0) (<= foo 3))
                            foo
                            3)))
           (history (let* ((foo (get-field :t-confidence (first trees)))
                           (confidence 
                            (if (and (integerp foo) (>= foo 0) (<= foo 3))
                              (aref #("zero" "low" "fair" "high") foo)
                              "unknown"))
                           (user (get-field :t-author (first trees)))
                           (date (get-field :t-end (first trees))))
                      (if (and (> version 0) user date)
                        (format
                         nil
                         "[~a confidence; version ~d on ~a by `~a']"
                         confidence version date user)
                        "")))
           (results (get-field :results item))
           (edges (loop
                      with edges
                      for result in results
                      for derivation = (get-field :derivation result)
                      do
                        (let ((edge (and derivation (reconstruct derivation))))
                          (push edge edges))
                      finally (return (nreverse edges))))
           (discriminants (reconstruct-discrimininants data parse-id version))
           (lkb::*parse-record* edges))
      (declare (ignore active))
      (setf (lkb::compare-frame-item frame) i-id)
      (setf (lkb::compare-frame-version frame) history)
      (setf (lkb::compare-frame-confidence frame) confidence)
      (setf (lkb::compare-frame-preset frame) discriminants)
      (lkb::set-up-compare-frame lkb::*parse-record* frame)
      (if (null %client%)
        (setf %client%
          (mp:run-function 
           "[incr tsdb()] Tree Selection"
           #'clim:run-frame-top-level frame))
        (clim:redisplay-frame-panes frame :force-p t))
      (process-add-arrest-reason *current-process* :wait)
      (let* ((decisions (lkb::compare-frame-decisions frame))
             (status (lkb::decision-type (first decisions))))
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
            (write-tree data parse-id version active confidence
                        t-author t-start t-end "" 
                        :cache cache))
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
                (write-decision data parse-id version 
                                state type key value start end time
                                :cache cache)))
                 
        (pairlis '(:status) (list status))))))

(defun encode-discriminant-state (discriminant)
  (let ((toggle (lkb::discr-toggle discriminant))
        (state (lkb::discr-state discriminant)))
    (cond
     ((eq toggle t) 1)
     ((null toggle) 2)
     ((eq state t) 3)
     ((null state) 4)
     (t 5))))

(defun encode-discriminant-type (discriminant)
  (case (lkb::discr-type discriminant)
    (:rel 1)
    (:type 2)
    (:constituent 3)
    (t 0)))

(defun reconstruct-discrimininants (data parse-id version)
  (loop
      with decisions = (when (and parse-id version)
                         (select '("d-state" "d-type" "d-key" "d-value" 
                                   "d-start" "d-end")
                                 '(:integer :integer :string :string 
                                   :integer :integer)
                                 "decision" 
                                 (format 
                                  nil 
                                  "parse-id == ~a && version == ~a" 
                                  parse-id version) 
                                 data))
      for decision in decisions
      for state = (get-field :d-state decision)
      for type = (get-field :d-type decision)
      for key = (get-field :d-key decision)
      for value = (get-field :d-value decision)
      for start = (get-field :d-start decision)
      for end = (get-field :d-end decision)
      for discriminant = (and state type key value start end
                              (or (= state 1) (= state 2))
                              (reconstruct-discriminant 
                               state type key value start end))
      when discriminant collect discriminant))

(defun reconstruct-discriminant (istate type key value start end)
  (let ((type (cond 
               ((eq type 1) :rel)
               ((eq type 2) :type)
               ((eq type 3) :constituent)
               (t nil)))
        toggle state)
    (cond
     ((eq istate 1) (setf toggle t) (setf state :unknown))
     ((eq istate 2) (setf toggle nil) (setf state :unknown))
     ((eq istate 3) (setf toggle :unknown) (setf state t))
     ((eq istate 4) (setf toggle :unknown)(setf state nil))
     ((eq istate 5) (setf toggle :unknown) (setf state :unknown))
     (t (return-from reconstruct-discriminant nil)))
    (lkb::make-discr :type (intern type :keyword) 
                     :key key :value value 
                     :start start :end end
                     :toggle toggle :state state)))





