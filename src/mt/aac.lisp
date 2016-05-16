(in-package :mt)

;;; added by AAC - called from rmrs-convert.lisp
;;; takes a string and tries to look it up in the SEMI
;;; could no doubt be improved by better understanding of the code

(defun find-semi-entries (pred)
;;; code adapted from test-semi-compliance
  (unless *semis*     (error "Semis not initialised"))
  (let* ((semi (first *semis*))
	 (pred-symbol (mrs::vsym (string-upcase pred))))
    (if
	(or
;;;	 (eql pred-symbol 'lkb::def_or_a_or_udef_q_rel)
;;; we get errors if we let through a symbol which isn't in the SEM-I
	 (member pred-symbol *semi-fragment-relations* :test #'eq)
	 (member
	  pred-symbol
	  *semi-punctuation-relations* :test #'eq)
	 (member pred-symbol *semi-token-relations* :test #'eq)
	 (lookup-predicate pred-symbol semi))
	pred-symbol
      (if (lookup-predicate pred semi)
	  pred
	nil))))
