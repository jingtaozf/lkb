;;; -*- Mode: COMMON-LISP; Syntax: Common-Lisp; Package: TSDB -*-

(in-package :tsdb)

(defparameter *maxent-collapse-irules-p* nil)

(defparameter *maxent-use-preterminal-types-p* t)

(defparameter *maxent-lexicalization-p* nil)

(defparameter *maxent-active-edges-p* t)

(defparameter *maxent-ngram-size* 0)

(defparameter *maxent-ngram-tag* :type)

(defparameter *maxent-ngram-back-off-p* t)

(defparameter *maxent-frequency-threshold* 10)

(defparameter *maxent-options*
  '(*maxent-collapse-irules-p*
    *maxent-use-preterminal-types-p*
    *maxent-lexicalization-p*
    *maxent-active-edges-p*
    *maxent-ngram-size*
    *maxent-ngram-tag*
    *maxent-ngram-back-off-p*
    *maxent-frequency-threshold*))
    
(defparameter *maxent-debug-p* t)

(defstruct (feature) 
  code 
  (count 1))

(defstruct (event) 
  id 
  frequency
  features 
  (size 0))

(defstruct (context) 
  id 
  (size 0)
  events)

(defstruct (mem)
  (table (make-symbol-table :test #'equal))
  contexts
  (frequencies (make-array 512 :initial-element 0))  
  (weights (make-array 512 :initial-element 0.0))
  (count 0)
  (size 512))

(defmethod print-object ((object mem) stream)
  (format 
   stream 
   "#[MEM (~d context~p; ~d weight~p)]"
   (length (mem-contexts object)) (length (mem-contexts object))
   (mem-count object) (mem-count object)))

(defun record-feature (feature event model)
  (let ((code (feature-code feature)))
    (when (>= code (mem-size model))
      (let ((n (setf (mem-size model) (* (mem-size model) 2))))
        (setf (mem-frequencies model)
          (adjust-array (mem-frequencies model) n :initial-element 0))
        (setf (mem-weights model)
          (adjust-array (mem-weights model) n :initial-element 0.0))))
    (incf (aref (mem-frequencies model) code) (feature-count feature))
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
            (incf (event-size event))
            (return))))))

(defun print-event (event &key (stream t) (format :rpm) model)

  (case format
    (:rpm
     (write (event-frequency event) :stream stream)
     (write-char #\Space stream)
     (write (event-size event) :stream stream)
     (loop
         for feature in (event-features event)
         for code = (feature-code feature)
         for frequency = (aref (mem-frequencies model) code)
         when (>= frequency *maxent-frequency-threshold*) do
           (write-char #\Space stream)
           (write code :stream stream)
           (write-char #\Space stream)
           (write (feature-count feature) :stream stream))
     (terpri stream))))

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
                (setf (mem-frequencies model)
                  (adjust-array (mem-frequencies model) n :initial-element 0))
                (setf (mem-weights model)
                  (adjust-array (mem-weights model) n :initial-element 0.0))))
            (setf (aref (mem-weights model) i) weight)
            (incf (mem-count model))))))

(defun print-mem (model &key (file "/dev/null") stream (format :rpm))
  (case format
    (:rpm
     (with-open-file (foo file :direction :output :if-exists :supersede)
       (loop
           with stream = (or stream foo)
           for context in (mem-contexts model)
           do 
             (print-context 
              context :stream stream :model model :format format))))
    (:export
     (with-open-file (foo file :direction :output :if-exists :supersede)
       (let  ((stream (or stream foo)))
         (format 
          stream 
          ";;;~%;;; ~a~%;;; (~a@~a; ~a)~%;;;~%"
          model (current-user) (current-host) (current-time :long :pretty))
         (format stream "~%:begin :mem ~d.~%~%" (length (mem-contexts model)))
         (loop
             with *print-case* = :downcase
             for key in *maxent-options*
             when (boundp key) do
               (format stream "~a := ~s.~%~%" key (symbol-value key)))
         (format stream ":begin :features ~d.~%~%" (mem-count model))
         (loop
             with *print-case* = :downcase
             with *package* = (find-package lkb::*lkb-package*)
             with table = (mem-table model)
             for i from 0
             for feature = (code-to-symbol i table)
             for weight across (mem-weights model)
             while feature do
               (format stream "[~{~s~^ ~}] ~,10f~%" feature weight))
         (format stream "~%:end :features.~%~%:end :mem.~%"))))))

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

  (loop
      for edge in (lkb::edge-children edge)
      do
        (edge-to-event edge model :event event))

  (let* ((codes (edge-to-codes edge model))
         (ngrams (unless eventp (edge-to-ngrams edge model))))
    (loop
        for code in codes
        do (record-feature (make-feature :code code) event model))
    (loop
        for code in ngrams 
        do (record-feature (make-feature :code code) event model)))

  event)

(defun edge-to-codes (edge model)
  (let* ((table (mem-table model))
         (root (edge-root edge))
         (daughters (lkb::edge-children edge))
         (irulep (lkb::inflectional-rule-p root)))
    (cond
     ((and (eq (lkb::edge-foo edge) model) (consp (lkb::edge-bar edge)))
      (lkb::edge-bar edge))
     ((null daughters)
      (let* ((feature (list 1 root (first (lkb::edge-leaves edge))))
             (code (symbol-to-code feature table)))
        (setf (lkb::edge-head edge) root)
        (setf (lkb::edge-foo edge) model)
        (setf (lkb::edge-bar edge) (list code))))
     ((and *maxent-collapse-irules-p* irulep)
      (let* ((extra (loop
                        for daughter = (first daughters)
                        then (first (lkb::edge-children daughter))
                        while daughter
                        collect (edge-root daughter)))
             (feature (nconc (list 1 root)
                             extra 
                             (list (first (lkb::edge-leaves edge)))))
             (code (symbol-to-code feature table)))
        (pprint feature)
        (setf (lkb::edge-head edge) (first (last extra)))
        (setf (lkb::edge-foo edge) model)
        (setf (lkb::edge-bar edge) (list code))))
     (t
      (when *maxent-lexicalization-p*
        ;;
        ;; decorate local edge with head lexicalization information: find the
        ;; head (or key) daughter in the local rule and project its head up
        ;; to the current edge.
        ;;
        (let* ((rule (lkb::edge-rule edge))
               (key (if (eq *maxent-lexicalization-p* :head)
                      (lkb::rule-head rule)
                      (first (lkb::rule-rhs rule)))))
          (setf (lkb::edge-head edge) (lkb::edge-head (nth key daughters)))))
      (let* ((roots (loop
                        for edge in daughters
                        for root = (edge-root edge)
                        for head = (lkb::edge-head edge)
                        nconc (cons root (when *maxent-lexicalization-p*
                                           (list head)))))
             (head (lkb::edge-head edge))
             (feature (cons root roots))
             (codes (if *maxent-lexicalization-p*
                      (list (symbol-to-code (cons 1 (cons head feature)) table)
                            (symbol-to-code (cons 1 feature) table))
                      (list (symbol-to-code (cons 1 feature) table)))))
        ;;
        ;; include (back-off, in a sense) features for partially instantiated
        ;; constituents (corresponding to active edges in the parser): for
        ;; the rule instantiation order .rhs., for each prefix, extract the
        ;; (sub-)sets of corresponding daughters, perform head lexicalization
        ;; if necessary, and add the resulting features to .codes.
        ;;
        (when *maxent-active-edges-p*
          (loop
              with rhs = (lkb::rule-rhs (lkb::edge-rule edge))
              for i from 1 to (- (length rhs) 1)
              for foo = (ith-n rhs 1 i)
              for roots = (loop
                              for j in foo
                              for root = (edge-root (nth j daughters))
                              for head = (lkb::edge-head (nth j daughters))
                              nconc (cons root (when *maxent-lexicalization-p*
                                                 (list head))))
              for feature = (cons root roots)
              when *maxent-lexicalization-p* do
                (push
                 (symbol-to-code (cons 2 (cons head feature)) table)
                 codes)
              do
                (push (symbol-to-code (cons 2 feature) table) codes)))
        (setf (lkb::edge-foo edge) model)
        (setf (lkb::edge-bar edge) codes))))))

(defun edge-to-ngrams (edge model)
  
  (loop
      with result = nil
      with table = (mem-table model)
      with forms = (lkb::edge-leaves edge)
      with ids = (lkb::edge-lex-ids edge)
      with tags = (if (eq *maxent-ngram-tag* :type)
                    (loop
                        for id in ids
                        collect (type-of-lexical-entry id))
                    ids)
      initially
        (if (and (eq (lkb::edge-foo edge) model)
                 (consp (lkb::edge-baz edge)))
          (return (lkb::edge-baz edge))
          (when (or (zerop *maxent-ngram-size*)
                    (not (= (length forms) (length tags))))
            (return)))
      for forms on (append (cons '^ forms) '($))
      for tags on (append (cons '^ tags) '($))
      while (first forms) do
        (loop
            for i from (if *maxent-ngram-back-off-p* 1 *maxent-ngram-size*)
            to *maxent-ngram-size*
            for form = (nth (- i 1) forms)
            for tags = (ith-n tags 1 i)
            when form do
              (push (symbol-to-code (cons form tags) table) result))
      finally 
        (setf (lkb::edge-foo edge) model)
        (setf (lkb::edge-baz edge) result)
        (return result)))

(defun edge-root (edge)
  (typecase (lkb::edge-rule edge)
    (lkb::rule (lkb::rule-id (lkb::edge-rule edge)))
    (string (let ((instance (first (lkb::edge-lex-ids edge))))
              (if *maxent-use-preterminal-types-p*
                (type-of-lexical-entry instance)
                instance)))
    (t (error "edge-root(): unknown rule in edge ~a~%" edge))))
                
(defun mem-score-edge (edge model &key recursivep)
  (if (and (not recursivep) 
           (eq (lkb::edge-foo edge) model) (numberp (lkb::edge-score edge)))
    (lkb::edge-score edge)
    (let* ((codes (edge-to-codes edge model))
           (ngrams (unless recursivep (edge-to-ngrams edge model))))
      (setf (lkb::edge-score edge)
        (+ (loop
               for code in codes
               sum (score-feature code model))
           (loop
               for code in ngrams
               sum (score-feature code model))
           (loop
               for edge in (lkb::edge-children edge)
               sum (mem-score-edge edge model :recursivep t)))))))

(defun mem-score-task (task model)
  (cond
   ((lkb::chart-configuration-p task)
    (mem-score-edge (lkb::chart-configuration-edge task) model))
   ((lkb::edge-p task)
    (mem-score-edge task model))
   ((lkb::rule-p (first task))
    (let* ((rule (first task))
           (root (lkb::rule-id rule))
           (passive (lkb::chart-configuration-edge (rest task)))
           (daughter (edge-root passive))
           (feature (list root daughter))
           (code (symbol-to-code feature (mem-table model))))
      #+:debug
      (format t "r&p: `~a'" feature)
      (+ (score-feature code model)
         (mem-score-edge passive model))))
   ((lkb::chart-configuration-p (first task))
    (let* ((active (lkb::chart-configuration-edge (first task)))
           (rule (lkb::edge-rule active))
           (root (lkb::rule-id rule))
           (daughters (loop
                          for daughter in (lkb::edge-children active)
                          collect (edge-root daughter)))
           (passive (lkb::chart-configuration-edge (rest task)))
           (daughter (edge-root passive))
           (feature (cons root (nconc daughters (list daughter))))
           (code (symbol-to-code feature (mem-table model))))
      #+:debug
      (format t "a&p: `~a'" feature)
      (+ (score-feature code model)
         (loop
             for edge in (lkb::edge-children active)
             sum (mem-score-edge edge model))
         (mem-score-edge passive model))))
   (t -10)))




