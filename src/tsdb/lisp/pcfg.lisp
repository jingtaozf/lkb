;;; -*- Mode: COMMON-LISP; Syntax: Common-Lisp; Package: TSDB -*-

(in-package :tsdb)

(defparameter *pcfg-collapse-irules-p* nil)

(defparameter *pcfg-use-preterminal-types-p* t)

(defparameter *pcfg-laplace-smoothing-p* t)

(defparameter *pcfg-geometric-mean-p* nil)

(defstruct (symbol-table 
            (:constructor make-symbol-table 
                          (&key (test #'eq)
                                (forward (make-hash-table :test test))
                                (backward (make-array 512))
                                (size 512)
                                (count 0))))
  forward backward size count)

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

(defun print-cfr (rule grammar &key (stream t) (prefix "") (suffix ""))
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
    (when suffix (format stream suffix))))

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
  (let ((i (cfr-lhs rule)))
    (if (>= i (cfg-size grammar))
      (let ((n (setf (cfg-size grammar) (* 2 (cfg-size grammar)))))
        (setf (cfg-rules grammar) (adjust-array (cfg-rules grammar) n))
        (setf (cfg-counts grammar) 
          (adjust-array (cfg-counts grammar) n :initial-element 0))
        (push rule (aref (cfg-rules grammar) i)))
      (if (cfr-grammar rule)
        (incf (cfr-count rule))
        (loop
          for foo in (aref (cfg-rules grammar) i)
          when (cfr-equal foo rule) 
          do (incf (cfr-count foo)) (return)
          finally (push rule (aref (cfg-rules grammar) i)))))
    (incf (aref (cfg-counts grammar) i)))
  (setf (cfr-grammar rule) grammar)
  (incf (cfg-count grammar)))

(defun match-cfr (rule grammar)
  (loop
      with i = (cfr-lhs rule)
      with bucket = (and (< i (cfg-size grammar)) (aref (cfg-rules grammar) i))
      for foo in bucket
      thereis (when (cfr-equal foo rule) foo)))

(defun estimate-probabilities (grammar)
  (loop
      for i from 0 to (- (cfg-size grammar) 1)
      for bucket = (aref (cfg-rules grammar) i)
      for count = (aref (cfg-counts grammar) i)
      when bucket do
        (when *pcfg-laplace-smoothing-p*
          (incf (aref (cfg-counts grammar) i) (+ (length bucket) 1))
          (setf count (aref (cfg-counts grammar) i)))
        (loop
            for rule in bucket
            when *pcfg-laplace-smoothing-p* do (incf (cfr-count rule))
            do 
              (let ((probability (/ (cfr-count rule) count)))
                (setf (cfr-probability rule) probability)
                (setf (cfg-epsilon grammar) 
                  (min (cfg-epsilon grammar) probability))))))

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
           "~a[~a] {~a}~%" 
           (code-to-symbol code table) code count))
        (loop
            for rule in bucket 
            do 
              (print-cfr rule grammar :stream stream 
                         :prefix "  " :suffix "~%"))))

(defun estimate-cfg (items &key (stream *tsdb-io*))

  (loop
      with grammar = (make-cfg)
      for item in items
      for id = (get-field :i-id item)
      for ranks = (get-field :ranks item)
      for edges = (loop
                      with *reconstruct-cache* = (make-hash-table :test #'eql)
                      for rank in ranks
                      for i = (get-field :rank rank)
                      for derivation = (get-field :derivation rank)
                      when (and i (= i 1))
                      collect (reconstruct derivation nil))
      when edges do
        (loop
            for edge in edges do
              (edge-to-cfrs edge grammar))
        (incf (cfg-samples grammar))
      else do
        (format 
         stream
         "~&[~a] estimate-cfg(): ignoring item # ~d (no edge);~%"
         (current-time :long :short) id)
      finally 
        (estimate-probabilities grammar)
        (return grammar)))

(defun edge-to-cfrs (edge grammar)
  (let ((rule (edge-to-cfr edge grammar)))
    (record-cfr rule grammar)
    (unless (smember (cfr-type rule) '(:irule :word))
      (loop
          for daughter in (lkb::edge-children edge)
          do (edge-to-cfrs daughter grammar)))))

(defun edge-to-cfr (edge grammar)
  (labels ((edge-root (edge)
             (typecase (lkb::edge-rule edge)
               (lkb::rule (lkb::rule-id (lkb::edge-rule edge)))
               (string (let ((instance (first (lkb::edge-lex-ids edge))))
                         (if *pcfg-use-preterminal-types-p*
                           (type-of-lexical-entry instance)
                           instance)))
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
       ((and *pcfg-collapse-irules-p* irulep)
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

(defun pcfg-score-edge (edge grammar &optional recursionp)
  
  (if (and (numberp (lkb::edge-score edge)) (eq (lkb::edge-foo edge) grammar))
    (values (lkb::edge-score edge) (lkb::edge-bar edge))
    (let* ((rule (edge-to-cfr edge grammar))
           (i (cfr-lhs rule))
           (match (if (eq (cfr-grammar rule) grammar)
                    rule
                    (match-cfr rule grammar)))
           (probability (if match
                          (cfr-probability match)
                          (if *pcfg-laplace-smoothing-p*
                            (let ((count  (when (< i (cfg-size grammar)) 
                                            (aref (cfg-counts grammar) i))))
                              (if (zerop count)
                                (cfg-epsilon grammar)
                                (/ 1 count)))
                            (cfg-epsilon grammar)))))
      
      (setf (lkb::edge-foo edge) grammar)
      (multiple-value-bind (score count)
          (if (smember (cfr-type rule) '(:irule :word))
            (values probability 1)
            (loop
                with result = 1
                with count = 1
                for daughter in (lkb::edge-children edge)
                do (multiple-value-bind (p n)
                       (pcfg-score-edge daughter grammar t)
                     (setf result (* result p))
                     (incf count n))
                finally 
                  (let* ((probability (* probability result))
                         (score (if (and *pcfg-geometric-mean-p*
                                         (null recursionp))
                                    (expt 10 (/ (log probability 10) count))
                                  probability)))
                    (return (values score count)))))
        (setf (lkb::edge-score edge) score)
        (setf (lkb::edge-bar edge) count)
        (values score count)))))
