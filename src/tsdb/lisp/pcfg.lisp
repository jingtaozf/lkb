;;; -*- Mode: COMMON-LISP; Syntax: Common-Lisp; Package: TSDB -*-

(in-package :tsdb)

(defstruct (symbol-table)
  (forward (make-hash-table :test #'eq))
  (backward (make-array 512))
  (size 512)
  (count 0))

(defmethod print-object ((object symbol-table) stream)
  (let ((n (hash-table-count (symbol-table-forward object))))
    (format 
     stream 
     "#[Symbol Table (~d forward~p; ~d backward~p of ~s)]"
     n n (symbol-table-count object) (symbol-table-count object)
     (symbol-table-size object))))

(defparameter *pcfg-symbol-table* (make-symbol-table))

(defun symbol-to-code (symbol &optional (table *pcfg-symbol-table*))
  (or
   (gethash symbol (symbol-table-forward table))
   (let* ((i (symbol-table-count table)))
     (setf (gethash symbol (symbol-table-forward table)) i)
     (when (>= i (symbol-table-size table))
       (setf (symbol-table-size table) (* 2 (symbol-table-size table)))
       (setf (symbol-table-backward table)
         (adjust-array 
          (symbol-table-backward table) (symbol-table-size table))))
     (setf (aref (symbol-table-backward table) i) symbol)
     (incf (symbol-table-count table))
     i)))

(defun code-to-symbol (code &optional (table *pcfg-symbol-table*))
  (when (< code (symbol-table-count table))
    (aref (symbol-table-backward table) code)))

(defstruct (cfr)
  type
  lhs
  rhs
  (count 1)
  probability
  grammar)

(defun cfr-equal (foo bar)
  (and (= (cfr-lhs foo) (cfr-lhs bar))
       (eq (cfr-type foo) (cfr-type bar))
       (equal (cfr-rhs foo) (cfr-rhs bar))))

(defun print-cfr (rule grammar &key (stream t) (prefix ""))
  (let* ((table (cfg-table grammar))
         (count (cfr-count rule))
         (probability (cfr-probability rule))
         (type (case (cfr-type rule) 
                 (:nary "N") (:unary "U") (:irule "I") (:word "W")))
         (lhs (cfr-lhs rule))
         (rhs (cfr-rhs rule)))
    (format 
     stream 
     "~a{~a}~@[ <~,4f>~] ~a: ~a[~a] -->" 
     prefix count probability type (code-to-symbol lhs table) lhs)
    (if (stringp rhs)
      (format stream " ~s" rhs)
      (loop
          for code in rhs do
            (format stream " ~a[~a]" (code-to-symbol code table) code)))
    (format stream "~%")))

(defstruct (cfg)
  (table (make-symbol-table))
  (rules (make-array 4096))
  (counts (make-array 4096 :initial-element 0))
  (epsilon 1.0)
  (samples 0)
  (size 4096)
  (count 0))

(defmethod print-object ((object cfg) stream)
  (format 
   stream 
   "#[CFG {~d sample~p; ~d rule~p} <~,4f>]"
   (cfg-samples object) (cfg-samples object)
   (cfg-count object) (cfg-count object) (cfg-epsilon object)))

(defun record-cfr (rule grammar)
  #+:debug
  (print-cfr rule grammar)
  (let ((i (cfr-lhs rule)))
    (if (>= i (cfg-size grammar))
      (let ((n (setf (cfg-size grammar) (* 2 (cfg-size grammar)))))
        (setf (cfg-rules grammar) (adjust-array (cfg-rules grammar) n))
        (setf (cfg-counts grammar) 
          (adjust-array (cfg-counts grammar) n :initial-element 0))
        (push rule (aref (cfg-rules grammar) i)))
      (loop
          for foo in (aref (cfg-rules grammar) i)
          when (cfr-equal foo rule) 
          do (incf (cfr-count foo)) (return)
          finally (push rule (aref (cfg-rules grammar) i))))
    (incf (aref (cfg-counts grammar) i)))
  (incf (cfg-count grammar)))

(defun find-cfr (rule grammar)
  (let ((i (cfr-lhs rule)))
    (when (and (integerp i) (< i (cfg-size grammar)))
      (loop
          for foo in (aref (cfg-rules grammar) i)
          thereis (when (cfr-equal foo rule) foo)))))

(defun estimate-probabilities (grammar)
  (loop
      for i from 0 to (- (cfg-size grammar) 1)
      for bucket = (aref (cfg-rules grammar) i)
      for count = (aref (cfg-counts grammar) i)
      when bucket do
        (loop
            for rule in bucket
            for probability = (/ (cfr-count rule) count)
            do 
              (setf (cfr-probability rule) probability)
              (setf (cfg-epsilon grammar) 
                (min (cfg-epsilon grammar) probability)))))

(defun print-cfg (grammar &key (stream t))
  (loop
      with table = (cfg-table grammar)
      for i from 0 to (- (cfg-size grammar) 1)
      for bucket = (aref (cfg-rules grammar) i)
      for count = (aref (cfg-counts grammar) i)
      when bucket do
        (let ((code (cfr-lhs (first bucket))))
          (format 
           stream 
           "~%~a[~a] {~a}~%" 
           (code-to-symbol code table) code count))
        (loop
            for rule in bucket 
            do (print-cfr rule grammar :stream stream :prefix "  "))))

(defun estimate-cfg (edges)
  (loop
      with grammar = (make-cfg)
      for edge in edges
      do
        (edge-to-cfrs edge grammar)
      finally 
        (estimate-probabilities grammar)
        (setf (cfg-samples grammar) (length edges))
        (return grammar)))

(defun edge-to-cfrs (edge grammar)
  (let ((rule (edge-to-cfr edge grammar)))
    (record-cfr rule grammar)
    (unless (smember (cfr-type rule) '(:irule :word))
      (loop
          for daughter in (lkb::edge-children edge)
          do (edge-to-cfrs daughter grammar)))))

(defun score-edge (edge grammar)
  (let* ((rule (edge-to-cfr edge grammar))
         (rule (find-cfr rule grammar))
         (probability (if rule (cfr-probability rule) (cfg-epsilon grammar))))
    (if (null (lkb::edge-children edge))
      probability 
      (* probability
         (loop
             with result = 1
             for daughter in (lkb::edge-children edge)
             do (setf result (* result (score-edge daughter grammar)))
             finally (return result))))))

(defun edge-to-cfr (edge grammar)
  (labels ((edge-root (edge)
             (typecase (lkb::edge-rule edge)
               (lkb::rule (lkb::rule-id (lkb::edge-rule edge)))
               (string (first (lkb::edge-lex-ids edge)))
               (t (error 
                   "edge-to-cfr(): unknown rule type in edge ~a~%" 
                   edge)))))
    (let* ((rule (lkb::edge-foo edge))
           (table (cfg-table grammar))
           (root (edge-root edge))
           (daughters (lkb::edge-children edge))
           (irulep (lkb::inflectional-rule-p root)))
      (cond
       ((and (cfr-p rule) (eq (cfr-grammar rule) grammar)) rule)
       ((null daughters)
        (let* ((lhs (symbol-to-code root table))
               (rhs (first (lkb::edge-leaves edge))))
          (setf (lkb::edge-foo edge)
            (make-cfr :type :word :lhs lhs :rhs rhs))))
       (irulep
        (let* ((rhs (first (lkb::edge-leaves edge)))
               (extra (loop
                          for daughter = (first daughters)
                          then (first (lkb::edge-children daughter))
                          while daughter
                          collect (edge-root daughter)))
               (extra (format nil "~:@(~a~)#~{~:@(~a~)~^#~}" irulep extra))
               (root (intern extra lkb::*lkb-package*))
               (lhs (symbol-to-code root table)))
          (setf (lkb::edge-foo edge)
            (make-cfr :type :irule :lhs lhs :rhs rhs))))
       (t
        (let* ((type (if (rest daughters) :nary :unary))
               (lhs (symbol-to-code root table))
               (rhs (loop
                        for edge in daughters
                        collect (symbol-to-code (edge-root edge) table))))
          (setf (lkb::edge-foo edge)
            (make-cfr :type type :lhs lhs :rhs rhs))))))))

(defun train-and-rank (train test &key (stream *tsdb-io*))

  #+:debug
  (setf %train% train %test% test)
  (loop
      with edges = (loop
                       for item in train
                       for ranks = (get-field :ranks item)
                       for best = (loop
                                      for rank in ranks
                                      for i = (get-field :rank rank)
                                      when (and i (= i 1)) 
                                      collect (get-field :edge rank))
                       nconc best)
      with grammar = (estimate-cfg edges)
      for item in test
      for readings = (get-field :readings item)
      for results = (get-field :results item)
      for ranks = nil
      initially 
        (format
         stream
         "~&[~a] train-and-rank(): using ~a;~%"
         (current-time :long :short) grammar)
      when (and (integerp readings) (> readings 1)) do
        (loop
            for result in results
            for id = (get-field :result-id result)
            for edge = (get-field :edge result)
            for score = (score-edge edge grammar)
            do 
              (push (nconc (pairlis '(:result-id :score)
                                    (list id score))
                           result)
                    ranks))
      and collect 
        (let* ((ranks (sort ranks #'> 
                            :key #'(lambda (foo) (get-field :score foo))))
               (ranks (loop
                          for i from 1
                          for rank in ranks
                          collect (acons :rank i rank))))
          (nconc (acons :ranks ranks nil) item))))
