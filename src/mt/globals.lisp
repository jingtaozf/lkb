(in-package :mt)

(defparameter *transfer-rule-sets* nil)

(defparameter *transfer-lexicon* nil)

(defparameter *transfer-result-filter* '(:complete))

(defparameter *mtr-filter-path* (list (mrs::vsym "FILTER")))

(defparameter *mtr-context-path* (list (mrs::vsym "CONTEXT")))

(defparameter *mtr-input-path* (list (mrs::vsym "INPUT")))

(defparameter *mtr-output-path* (list (mrs::vsym "OUTPUT")))

(defparameter *mtr-flags-path* (list (mrs::vsym "FLAGS")))

(defconstant *mtr-optional-path* (list (mrs::vsym "OPTIONAL")))

(defconstant *mtr-equal-path* (list (mrs::vsym "EQUAL")))

(defconstant *mtr-subsume-path* (list (mrs::vsym "SUBSUME")))

(defconstant *mtr-skolem-property* (mrs::vsym "SKOLEM"))

(defconstant *mtr-scratch-property* (mrs::vsym "SCRATCH"))

(defconstant *mtr-mark-property* (mrs::vsym "MARK"))

(defconstant *mtr-ditch-property* (mrs::vsym "DITCH"))

(defconstant *mtr-upcase-operator* (mrs::vsym "+upcase+"))

(defconstant *mtr-downcase-operator* (mrs::vsym "+downcase+"))

(defconstant *mtr-copy-operator* (mrs::vsym "+copy+"))

(defconstant *mtr-true-type* (mrs::vsym "+"))

(defconstant *mtr-false-type* (mrs::vsym "-"))

(defconstant *semi-u-type* "u")

(defconstant *semi-h-type* "h")

(defconstant *semi-i-type* "i")

(defconstant *semi-e-type* "e")

(defconstant *semi-x-type* "x")

(defparameter *semi-fragment-left* (mrs::vsym "L-HNDL"))

(defparameter *semi-fragment-right* (mrs::vsym "R-HNDL"))

(defparameter *semi-punctuation-relations*
  (list "comma_rel" "period_rel" "exclamation_point_rel" "question_mark_rel"
        (mrs::vsym "comma_rel") (mrs::vsym "period_rel")
        (mrs::vsym "exclamation_point_rel") (mrs::vsym "question_mark_rel")))

(defparameter *semi-token-relations*
  (list "token_rel" (mrs::vsym "token_rel")))
