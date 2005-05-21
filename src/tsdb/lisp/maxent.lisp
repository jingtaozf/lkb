;;; -*- Mode: COMMON-LISP; Syntax: Common-Lisp; Package: TSDB -*-

;;;
;;; [incr tsdb()] --- Competence and Performance Profiling Environment
;;; Copyright (c) 1996 -- 2005 Stephan Oepen (oe@csli.stanford.edu)
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as published by
;;; the Free Software Foundation; either version 2.1 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
;;; License for more details.
;;; 

;;;
;;; some notes on the encoding of features: to make sure we operate in separate
;;; namespaces for the various feature templates, each feature is prefixed with
;;; an integer identifying the feature type (aka template), viz.
;;;
;;;   1: local derivational configuration; a subtree of depth one taken from 
;;;      the derivation; e.g. [1 0 hspec det_poss_my_le noptcomp]; the second
;;;      integer indicates the amount of grandparenting used, where parent 
;;;      labels will precede the root of the local configuration.
;;;   2: `active' local derivational configuration; similar to type 1, but for
;;;      sub-sets of the daughters (corresponding to active edges), determined
;;;      by rule instantiation order (the .rhs. slot in LKB rule structures),
;;;      e.g. [2 0 hspec noptcomp].
;;;   3: n-gram features; e.g. [3 3 "saw" ^ n_proper_le v_np_trans_le]
;;;   4: preterminal-only n-gram features; like type 3, but without the surface
;;;      form; e.g. [4 "saw" ^ n_proper_le v_np_trans_le].
;;;  42: language model score; the second integer is the number of bins used 
;;;      (if any), and the third the divisor used in scaling (*maxent-lm-p*), 
;;;      e.g. [42 0 100]
;;;
(in-package :tsdb)

(defparameter *maxent-grandparenting* 2)

(defparameter *maxent-use-preterminal-types-p* t)

(defparameter *maxent-lexicalization-p* nil)

(defparameter *maxent-active-edges-p* t)

(defparameter *maxent-ngram-size* 0)

(defparameter *maxent-ngram-tag* :type)

(defparameter *maxent-ngram-back-off-p* t)

(defparameter *maxent-lm-p* 100)

(defparameter *maxent-frequency-threshold* 0)

(defparameter *maxent-random-sample-size* 1000)

(defparameter *maxent-method* :tao_lmvm)

(defparameter *maxent-iterations* nil)

(defparameter *maxent-relative-tolerance* 1e-7)

(defparameter *maxent-variance* 1e2)

(defparameter *maxent-options*
  '(*maxent-grandparenting*
    *maxent-use-preterminal-types-p*
    *maxent-lexicalization-p*
    *maxent-active-edges-p*
    *maxent-ngram-size*
    *maxent-ngram-tag*
    *maxent-ngram-back-off-p*
    *maxent-lm-p*
    *maxent-frequency-threshold*
    *maxent-random-sample-size*
    *maxent-method*
    *maxent-iterations*
    *maxent-relative-tolerance*
    *maxent-variance*))
    
(defparameter *maxent-debug-p* t)

(defun mem-environment ()
  (format 
   nil
   "GP[~a] ~:[-~;+~]PT ~:[-~;+~]AE ~
    NS[~a] NS[~a] NT[~(~a~)] ~:[-~;+~]NB LM~:[0~*~;~a~] ~
    FT[~a] RS[~a] MM[~(~a~)] MI[~a] RT[~a] VA[~a]"
   *maxent-grandparenting* *maxent-use-preterminal-types-p*
   *maxent-lexicalization-p* *maxent-active-edges-p*
   *maxent-ngram-size* *maxent-ngram-tag*
   *maxent-ngram-back-off-p* *maxent-lm-p* *maxent-lm-p*
   *maxent-frequency-threshold* *maxent-random-sample-size*
   *maxent-method* (or *maxent-iterations* 0) 
   *maxent-relative-tolerance* *maxent-variance*))

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
  contexts (ncontexts 0)
  (frequencies (make-array 512 :initial-element 0))  
  (weights (make-array 512 :initial-element 0.0))
  (count 0)
  (size 512)
  (file (format nil "/tmp/.mem.~a.~a.mee" (current-user) (current-pid)))
  stream)

(defmethod print-object ((object mem) stream)
  (format 
   stream 
   "#[MEM (~d context~p; ~d weight~p)]"
   (mem-ncontexts object) (mem-ncontexts object)
   (mem-count object) (mem-count object)))

(defun initialize-mem (model)
  (when (mem-file model)
    (setf (mem-stream model)
      (open (mem-file model) :direction :output 
            :if-does-not-exist :create :if-exists :supersede))))

(defun read-mem (file &key (verbose t))
  (labels ((|[|-reader (stream char)
               (declare (ignore char))
               (read-delimited-list #\] stream nil)))
    (let* ((*readtable* (copy-readtable nil))
           (*package* (find-package :lkb))
           (model (make-mem))
           (table (mem-table model))
           (name (format 
                  nil "~a~@[.~a~]"
                  (pathname-name file) (pathname-type file))))
      (set-syntax-from-char #\. #\space *readtable*)
      (if (probe-file file)
        (with-open-file (stream file :direction :input)
          (unless (and (eq (read stream nil nil) :begin)
                       (eq (read stream nil nil) :mem)
                       (integerp (read stream nil nil)))
            (format t "read-mem(): invalid header in `~a'.~%" name)
            (return-from read-mem))
          (when verbose
            (format t "~&read-mem(): reading file `~a'.~%" name))
          (loop
              with bodyp = nil
              for form = (read stream nil :eof)
              while (not (eq form :eof))
              when (and (eq form :begin) (eq (read stream nil nil) :features))
              do
                (let ((n (read stream nil nil)))
                  (unless (and (integerp n) (>= n 0))
                    (format 
                     t 
                     "read-mem(): invalid `:begin :feature' block in `~a'.~%"
                     name)
                    (return-from read-mem))
                  (setf (mem-size model) n)
                  (setf (mem-weights model) 
                    (make-array n :initial-element 0.0)))
                (setf *readtable* (copy-readtable nil))
                (set-syntax-from-char #\[ #\( *readtable*)
                (set-syntax-from-char #\] #\) *readtable*)
                (set-macro-character #\[ #'|[|-reader nil *readtable*)
                (setf bodyp t)
              else when (eq form :end) do
                (set-syntax-from-char #\. #\space *readtable*)
                (unless (and (eq (read stream nil nil) :features)
                             (eq (read stream nil nil) :end) 
                             (eq (read stream nil nil) :mem))
                  (format t "read-mem(): invalid MEM prologue.~%")
                  (return-from read-mem))
                (return (setf %model% model))
              else when bodyp do
                (unless (consp form)
                  (format t "read-mem(): invalid feature `~a'.~%" form)
                  (return-from read-mem))
                (let ((code (symbol-to-code form table))
                      (weight (read stream nil nil)))
                  (unless (numberp weight)
                    (format 
                     t 
                     "read-mem(): invalid weight on `~a'.~%"
                     form)
                    (return-from read-mem))
                  (when (>= code (mem-size model))
                    (format 
                     t 
                     "read-mem(): mysterious feature overflow (~a vs. ~a).~%"
                     code (mem-size model))
                    (return-from read-mem))
                  (setf (aref (mem-weights model) code) weight))
                (incf (mem-count model))))
        (format t "read-mem(): unable to open `~a'.~%" name)))))

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
  (incf (mem-ncontexts model))
  (if (open-stream-p (mem-stream model))
    (print-context 
     context :stream (mem-stream model) :model model :format :rpm)
    (push context (mem-contexts model))))

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
         (format stream "~%:begin :mem ~d.~%~%" (mem-ncontexts model))
         (loop
             with *print-case* = :downcase
             for key in *maxent-options*
             when (boundp key) do
               (format stream "~a := ~s.~%~%" key (symbol-value key)))
         (format stream ":begin :features ~d.~%~%" (mem-count model))
         (loop
             with *print-case* = :downcase
             with *package* = (find-package :lkb)
             with table = (mem-table model)
             for i from 0
             for feature = (code-to-symbol i table)
             for weight across (mem-weights model)
             while feature do
               (unless (= weight 0.0)
                 (format stream "[~{~s~^ ~}] ~,20f~%" feature weight)))
         (format stream "~%:end :features.~%~%:end :mem.~%"))))))

(defun estimate-mem (items &key (stream *tsdb-io*) model (estimatep t))

  (loop
      with model = (or model 
		       (let ((model (make-mem)))
			 (initialize-mem model)
			 model))
      with table = (mem-table model)
      with code = (symbol-to-code (list 42 0 *maxent-lm-p*) table)
      for item in items
      for iid = (get-field :i-id item)
      for readings = (get-field :readings item)
      when (and (integerp readings) (> readings 0)) do
        #+:null
	(format 
	 stream
	 "~&[~a] estimate-mem(): item # ~a (~a reading~p);~%"
	 (current-time :long :short) iid readings readings)
	(loop
	    with results = (get-field :results item)
	    with n = (length results)
	    with i = 0
            with low = (loop
                           for result in (get-field :results item)
                           minimize (get-field :result-id result))
	    with active = (loop
                              for rank in (get-field :ranks item)
                              for i = (get-field :rank rank)
                              for id = (get-field :result-id rank)
                              when (= i 1) collect id)
	    with sample = (when (and *maxent-random-sample-size*
                                     (< *maxent-random-sample-size* 
                                        (- n (length active))))
                            (random-sample 
                             low (if (zerop low) (- n 1) n)
                             (min 
                              (- n (length active)) 
                              *maxent-random-sample-size*)
                             active))
            with context = (make-context :id iid)
	    with *reconstruct-cache* = (make-hash-table :test #'eql)
	    for result in results
	    for rid = (get-field :result-id result)
	    for frequency = (if (member rid active :test #'=) 1 0)
            for lm = (get-field :lm result)
	    for derivation = (when  (or (not sample)
                                        (or (not (zerop frequency))
                                            (member rid sample :test #'=)))
                               (get-field :derivation result))
	    for edge = (when derivation
			 (or (get-field :edge result)
                             (let* ((edge (reconstruct derivation nil))
                                    (string (when (lkb::edge-p edge)
                                              (lkb::edge-string edge))))
                               (nconc result (acons :string string nil))
                               edge)))
	    for event = (when edge (edge-to-event edge model))
	    while (or (not sample) 
                      (< i (+ *maxent-random-sample-size* (length active))))
	    when (and derivation (null edge)) do
	      (format
	       stream
	       "~&[~a] estimate-mem(): ignoring item # ~d (no edge for ~d)~%"
	       (current-time :long :short) iid rid)
	      (return)
	    when event do 
              (when lm
                (record-feature 
                 (make-feature :code code :count lm) event model))
	      (setf (event-id event) rid)
	      (setf (event-frequency event) frequency)
	      (record-event event context)
              (incf i)
	    finally
              (record-context context model)
              (format 
               stream
               "~&[~a] estimate-mem(): item # ~d: ~d ~@[[of ~a] ~]~
                event~p (~d active);~%" 
               (current-time :long :short) iid i (and sample n) 
               i (length active))) 
      finally
	(when estimatep
	  (let* ((out (format 
                       nil
                       "/tmp/.mem.~a.~a.mew" (current-user) (current-pid)))
                 (variances (when  (numberp *maxent-variance*)
                              (let ((name (format 
                                           nil
                                           "/tmp/.mem.~a.~a.mev"
                                           (current-user)
                                           (current-pid))))
                                (with-open-file (stream name
                                                 :direction :output
                                                 :if-exists :supersede)
                                  (format stream "~,4f" *maxent-variance*))
                                name)))
		 (command (format 
			   nil 
			   "estimate -events_in ~a -params_out ~a~
                              ~@[ -method ~(~a~)~]~
                              ~@[ -max_it ~a~]~@[ -frtol ~a~]~
                              ~@[ -variances ~a~]"
			   (mem-file model) out
                           *maxent-method*
                           *maxent-iterations* *maxent-relative-tolerance*
                           variances))
		 (output (if *maxent-debug-p* nil "/dev/null")))
	    (force-output (mem-stream model))
	    (close (mem-stream model))
	    (setf (mem-stream model) nil)
	    (when (probe-file out) (ignore-errors (delete-file out)))
	    (when (and (zerop (run-process 
			       command :wait t 
			       :output output :if-output-exists :append))
		       (probe-file out))
	      (read-weights model out))))
	(return model)))

(defun edge-to-event (edge model
                      &key (event (make-event) eventp)
                           (parents '(^)))

  (loop
      with parents 
      = (when (> *maxent-grandparenting* 0)
          (append (last parents (- *maxent-grandparenting* 1)) (list edge)))
      for edge in (lkb::edge-children edge)
      do
        (edge-to-event edge model :event event :parents parents))
  
  (let* ((codes (edge-to-codes edge parents model))
         (ngrams (unless eventp (edge-to-ngrams edge model))))
    (loop
        for code in codes
        do (record-feature (make-feature :code code) event model))
    (loop
        for code in ngrams 
        do (record-feature (make-feature :code code) event model)))

  event)

(defun edge-to-codes (edge parents model)
  (let* ((table (mem-table model))
         (root (edge-root edge))
         (parents (loop
                      for parent in parents
                      collect (if (lkb::edge-p parent)
                                (edge-root parent)
                                parent)))
         (daughters (or (lkb::edge-children edge)
                        (let ((foo (lkb::edge-morph-history edge)))
                          (and foo (list foo))))))
    (cond
     ;;
     ;; see whether we have extracted features from this edge before; if so,
     ;; re-use cache of earlier results (safe-guarded by .model. identity).
     ;;
     ((and (eq (lkb::edge-foo edge) model) (consp (lkb::edge-bar edge)))
      (lkb::edge-bar edge))
     ;;
     ;; at the terminal yield of a derivation, things are relatively simple:
     ;; create one feature for the local configuration at the yield, plus as
     ;; many as are possible to derive from grandparenting, i.e. prefixing the
     ;; tuple corresponding to the local feature with all suffixes of .parents.
     ;;
     ((null daughters)
      (let* ((feature (cons root (lkb::edge-leaves edge)))
             (codes (list (symbol-to-code (list* 1 0 feature) table))))
        (loop
            for i from 1 to (min (length parents) *maxent-grandparenting*)
            for iparents = (last parents i)
            for code = (symbol-to-code
                        (append (list 1 i) iparents feature)
                        table)
            do (push code codes))
        (when *maxent-lexicalization-p* (setf (lkb::edge-head edge) root))
        (setf (lkb::edge-foo edge) model)
        (setf (lkb::edge-bar edge) codes)))
     (t
      ;;
      ;; _fix_me_
      ;; lexicalized features, presumably, should use a separate identifier;
      ;; the complete lexicalization set-up needs testing.      (3-apr-05; oe)
      ;;
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
                        collect (edge-root edge)))
             (head (lkb::edge-head edge))
             (feature (cons root roots))
             (codes (nconc
                     (when *maxent-lexicalization-p*
                       (list (symbol-to-code (list* 1 0 head feature) table)))
                     (list (symbol-to-code (list* 1 0 feature) table)))))
        
        (loop
            for i from 1 to (min (length parents) *maxent-grandparenting*)
            for iparents = (last parents i)
            for code = (symbol-to-code
                        (append (list 1 i) iparents feature)
                        table)
            do (push code codes))

        ;;
        ;; include (back-off, in a sense) features for partially instantiated
        ;; constituents (corresponding to active edges in the parser): for
        ;; the rule instantiation order .rhs., for each prefix, extract the
        ;; (sub-)sets of corresponding daughters, perform head lexicalization
        ;; if necessary, and add the resulting features to .codes.
        ;;
        ;; _fix_me_
        ;; not quite sure what to do about grandparenting; would it make sense
        ;; on these `active edges' too?                          (3-apr-05; oe)
        ;;
        (when *maxent-active-edges-p*
          (loop
              with rhs = (lkb::rule-rhs (lkb::edge-rule edge))
              for i from 1 to (- (length rhs) 1)
              for foo = (ith-n rhs 1 i)
              for roots = (loop
                              for j in foo
                              collect (edge-root (nth j daughters)))
              for feature = (cons root roots)
              when *maxent-lexicalization-p* do
                (push
                 (symbol-to-code (list* 2 0 head feature) table)
                 codes)
              do
                (push (symbol-to-code (list* 2 0 feature) table) codes)))
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
            for itags = (ith-n tags 1 i)
            when (and form (not (and (= i 1) (smember form '(^ $))))) do
              (push (symbol-to-code (list* 3 i form itags) table) result)
              (push (symbol-to-code (list* 4 i itags) table) result))
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
                
(defun mem-score-edge (edge 
                       &optional (model %model%) 
                       &key (parents '(^)) recursivep lm)

  (if (and (not recursivep) 
           (eq (lkb::edge-foo edge) model) (numberp (lkb::edge-score edge)))
    (lkb::edge-score edge)
    (let* ((codes (edge-to-codes edge parents model))
           (ngrams (unless recursivep (edge-to-ngrams edge model)))
           (score (if (numberp lm)
                    (let* ((table (mem-table model))
                           (code
                            (symbol-to-code (list 42 0 *maxent-lm-p*) table)))
                      (* (score-feature code model) lm))
                    0)))
      (setf (lkb::edge-score edge)
        (+ (loop
               for code in codes
               sum (score-feature code model))
           (loop
               for code in ngrams
               sum (score-feature code model))
           score
           (loop
               with parents 
               = (when (> *maxent-grandparenting* 0)
                   (append
                    (last parents (- *maxent-grandparenting* 1)) (list edge)))
               for edge in (or (lkb::edge-children edge)
                               (let ((foo (lkb::edge-morph-history edge)))
                                 (and foo (list foo))))
               sum (mem-score-edge
                    edge model :recursivep t :parents parents)))))))

(defun mem-item-enhancer (item)
  (when (and (numberp *maxent-lm-p*) (not (= *maxent-lm-p* 0)))
    (loop
        with foo
        with results = (get-field :results item)
        with strings = (loop
                           for result in results
                           for string = (get-field :tree result)
                           when string 
                           collect string 
                           and do (push result foo))
        with scores = (mt::lm-score-strings strings)
        for result in (nreverse foo)
        for score = (/ (rest (pop scores)) *maxent-lm-p*)
        do (nconc result (acons :lm score nil))))
  item)

(defun mem-score-task (task model)
  ;;
  ;; _fix_me_
  ;; the following seem to not have an integer first() in their features.
  ;;                                                           (27-oct-04; oe)
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

(defun mem-score-configuration (edge daughters &optional (model %model%))
  ;;
  ;; _fix_me_
  ;; there is too much duplication in various bits of feature extraction code;
  ;; rework from scratch, one day.                             (30-dec-04; oe)
  ;;
  (let* ((table (mem-table model))
         (roots (if daughters
                  (loop
                      for edge in daughters
                      collect (edge-root edge))
                  (lkb::edge-leaves edge)))
         (feature (cons (edge-root edge) roots))
         (code (symbol-to-code (list* 1 0 feature) table)))
    (score-feature code model)))

(defconstant e (exp 1d0))

(defun scores-to-probabilities (scores)
  (loop
      with sum = 0d0
      for score in scores
      for foo = (if (stringp score) (read-from-string score) score)
      for p = (expt e (coerce foo 'long-float))
      collect p into probabilities
      do (incf sum p)
      finally
        (return (loop 
                    for p in probabilities
                    collect (/ p sum)))))

(defun entropy (probabilities)
  (loop
      with h = 0d0
      for p in probabilities
      do (incf h (* p (log p 2d0)))
      finally (return (- h))))

(defun random-sample (low high size &optional sample)
  ;;
  ;; returns a set of .size. unique random integers from [.low. -- .high.], not
  ;; including elements of the initial .sample., when supplied.
  ;;
  (loop
      with n = (+ (- high low) 1)
      for i = (+ low (random n))
      unless (smember i sample) do
        (push i sample)
        (decf size)
      while (> size 0)
      finally (return sample)))
