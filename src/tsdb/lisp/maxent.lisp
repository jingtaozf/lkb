;;; -*- Mode: COMMON-LISP; Syntax: Common-Lisp; Package: TSDB -*-

(in-package :tsdb)

(defparameter *maxent-collapse-irules-p* nil)

(defparameter *maxent-use-preterminal-types-p* t)

(defparameter *maxent-ngram-size* 3)

(defparameter *maxent-ngram-values* '(:type))

(defparameter *maxent-ngram-back-off-p* nil)

(defparameter *maxent-debug-p* t)

(defstruct (feature) 
  code 
  (count 1))

(defstruct (event) 
  id 
  frequency
  features 
  (size 0))

(defun record-feature (feature event)
  (let ((code (feature-code feature)))
    (cond
     ((or (zerop (event-size event)) 
          (< code (feature-code (first (event-features event)))))
      (push feature (event-features event))
      (incf (event-size event)))
     (t
      (loop
          for features on (event-features event)
          for this = (first features)
          for next = (first (rest features))
          when (= (feature-code this) code) do
            (incf (feature-count this) (feature-count feature))
            (return)
          else when (or (null next) (< code (feature-code next))) do
            (setf (rest features) (cons feature (rest features)))
            (incf (event-size event) (feature-count feature))
            (return))))))

(defun print-event (event &key (stream t) (format :rpm) model)
  (declare (ignore model))
  (case format
    (:rpm
     (write (event-frequency event) :stream stream)
     (write-char #\Space stream)
     (write (event-size event) :stream stream)
     (loop
         for feature in (event-features event)
         do 
           (write-char #\Space stream)
           (write (feature-code feature) :stream stream)
           (write-char #\Space stream)
           (write (feature-count feature) :stream stream))
     (terpri stream))))

(defstruct (context) 
  id 
  (size 0)
  events)

(defun record-event (event context)
  (push event (context-events context))
  (incf (context-size context)))

(defun print-context (context &key (stream t) (format :rpm) model)
  (case format
    (:rpm
     (format stream "~d~%" (context-size context))
     (loop
         for event in (context-events context)
         do 
           (print-event event :stream stream :model model :format format)))))

(defstruct (mem)
  (table (make-symbol-table :test #'equal))
  contexts
  (weights (make-array 512 :initial-element 0.0))
  (count 0)
  (size 512))

(defmethod print-object ((object mem) stream)
  (format 
   stream 
   "#[MEM (~d context~p; ~d weight~p)]"
   (length (mem-contexts object)) (length (mem-contexts object))
   (mem-count object) (mem-count object)))

(defun record-context (context model)
  (push context (mem-contexts model)))

(defun score-feature (code model)
  (if (< code (mem-count model)) (aref (mem-weights model) code) 0.0))

(defun read-weights (model file)
  (with-open-file (stream file :direction :input :if-does-not-exist nil)
    (when stream
      (loop
          for i from 0
          for weight = (read stream nil nil)
          while weight do
            (when (>= i (mem-size model))
              (let ((n (setf (mem-size model) (* (mem-size model) 2))))
                (setf (mem-weights model)
                  (adjust-array (mem-weights model) n :initial-element 0.0))))
            (setf (aref (mem-weights model) i) weight)
            (incf (mem-count model))))))

(defun print-mem (model &key file stream (format :rpm))
  (with-open-file (foo file :direction :output :if-exists :supersede)
    (loop
        with stream = (or stream foo)
        for context in (mem-contexts model)
        do 
          (print-context context :stream stream :model model :format format))))

(defun estimate-mem (items &key (stream *tsdb-io*))
  #+:debug
  (setf %items% items)
  (loop
      with model = (make-mem)
      for item in items
      for iid = (get-field :i-id item)
      for readings = (get-field :readings item)
      when (and (integerp readings) (> readings 0)) do
        (format 
         stream
         "~&[~a] estimate-mem(): item # ~a (~a reading~p);~%"
         (current-time :long :short) iid readings readings)
        (loop
            with context = (make-context :id iid)
            with *reconstruct-cache* = (make-hash-table :test #'eql)
            for result in (get-field :results item)
            for rid = (get-field :result-id result)
            for frequency = (loop
                                for rank in (get-field :ranks item)
                                for i = (get-field :rank rank)
                                for id = (get-field :result-id rank)
                                when (and (= i 1) (= id rid)) do (return 1)
                                finally (return 0))
            for derivation = (get-field :derivation result)
            for edge = (or (get-field :edge result)
                           (reconstruct derivation nil))
            for event = (when edge (edge-to-event edge model))
            when (null edge) do
              (format
               stream
               "[~a] estimate-mem(): ignoring item # ~d (no edge for ~d)~%"
               (current-time :long :short) iid rid)
              (return)
            when event do 
              (setf (event-id event) rid)
              (setf (event-frequency event) frequency)
              (record-event event context)
            finally
              (record-context context model))
      finally 
        (let* ((in (format nil "/tmp/.mem.~a.mee" (current-user)))
               (out (format nil "/tmp/.mem.~a.mew" (current-user)))
               (command (format 
                         nil 
                         "estimate -events_in ~a -params_out ~a"
                         in out))
               (output (if *maxent-debug-p* nil "/dev/null")))
          (print-mem model :file in)
          (when (probe-file out) (ignore-errors (delete-file out)))
          (when (and (zerop (run-process 
                             command :wait t 
                             :output output :if-output-exists :append))
                     (probe-file out))
            (read-weights model out)))
        (return model)))

(defun edge-to-event (edge model &key (event (make-event) eventp))

  (let* ((code (edge-to-code edge model))
         (ngrams (unless eventp (edge-to-ngrams edge model))))
    (record-feature (make-feature :code code) event)
    (loop
        for code in ngrams 
        do (record-feature (make-feature :code code) event)))
  (loop
      for edge in (lkb::edge-children edge)
      do
        (edge-to-event edge model :event event))
  event)

(defun edge-to-code (edge model)
  (labels ((edge-root (edge)
             (typecase (lkb::edge-rule edge)
               (lkb::rule (lkb::rule-id (lkb::edge-rule edge)))
               (string (let ((instance (first (lkb::edge-lex-ids edge))))
                         (if *maxent-use-preterminal-types-p*
                           (type-of-lexical-entry instance)
                           instance)))
               (t (error 
                   "edge-to-code(): unknown rule type in edge ~a~%" 
                   edge)))))
             
    (let* ((table (mem-table model))
           (root (edge-root edge))
           (daughters (lkb::edge-children edge))
           (irulep (lkb::inflectional-rule-p root)))
      (cond
       ((and (eq (lkb::edge-foo edge) model) (integerp (lkb::edge-bar edge)))
        (lkb::edge-bar edge))
       ((null daughters)
        (symbol-to-code  table)
        (let* ((feature (list root (first (lkb::edge-leaves edge))))
               (code (symbol-to-code feature table)))
          (setf (lkb::edge-foo edge) model)
          (setf (lkb::edge-bar edge) code)))
       ((and *maxent-collapse-irules-p* irulep)
        (let* ((extra (loop
                          for daughter = (first daughters)
                          then (first (lkb::edge-children daughter))
                          while daughter
                          collect (edge-root daughter)))
               (feature (nconc extra (list (first (lkb::edge-leaves edge)))))
               (code (symbol-to-code feature table)))
          (setf (lkb::edge-foo edge) model)
          (setf (lkb::edge-bar edge) code)))
       (t
        (let* ((daughters (loop
                              for edge in daughters
                              collect  (edge-root edge)))
               (feature (cons root daughters))
               (code (symbol-to-code feature table)))
          (setf (lkb::edge-foo edge) model)
          (setf (lkb::edge-bar edge) code)))))))

(defun edge-to-ngrams (edge model)
  
  (loop
      with result = nil
      with table = (mem-table model)
      with forms = (lkb::edge-leaves edge)
      with ids = (lkb::edge-lex-ids edge)
      with types = (when (smember :type *maxent-ngram-values*)
                     (loop
                         for id in ids
                         collect (type-of-lexical-entry id)))
      initially
        (if (and (eq (lkb::edge-foo edge) model)
                 (consp (lkb::edge-baz edge)))
          (return (lkb::edge-baz edge))
          (when (or (null *maxent-ngram-values*)
                  (not (= (length forms) (length ids)))
                  (and types (not (= (length forms) (length types)))))
            (return)))
      for (form1 form2 form3) on (append (cons '^ forms) '($))
      for (id1 id2 id3) on (append (cons '^ ids) '($))
      for (type1 type2 type3) on (append (cons '^ types) '($))
      while form3 do
        (let* ((feature (nconc (when (smember :form *maxent-ngram-values*)
                                 (list form1 form2 form3))
                               (when (smember :id *maxent-ngram-values*)
                                 (list id1 id2 id3))
                               (when (smember :type *maxent-ngram-values*)
                                 (list type1 type2 type3)))))
          (push (symbol-to-code feature table) result))
      finally 
        (setf (lkb::edge-foo edge) model)
        (setf (lkb::edge-baz edge) result)
        (return result)))
                
(defun mem-score-edge (edge model &key recursivep)
  (let* ((code (edge-to-code edge model))
         (weight (score-feature code model))
         (ngrams (unless recursivep (edge-to-ngrams edge model))))
    (+ weight
       (loop
           for code in ngrams
           sum (score-feature code model))
       (loop
           for edge in (lkb::edge-children edge)
           sum (mem-score-edge edge model :recursivep t)))))
