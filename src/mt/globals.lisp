(in-package :mt)

(defparameter *transfer-rule-sets* nil)

(defparameter *transfer-result-filter* '(:complete))

(defparameter *mtr-filter-path* (list (mrs::vsym "FILTER")))

(defparameter *mtr-context-path* (list (mrs::vsym "CONTEXT")))

(defparameter *mtr-input-path* (list (mrs::vsym "INPUT")))

(defparameter *mtr-output-path* (list (mrs::vsym "OUTPUT")))

(defparameter *mtr-flags-path* (list (mrs::vsym "FLAGS")))

(defconstant *mtr-optional-path* (list (mrs::vsym "OPTIONAL")))

(defconstant *mtr-unique-path* (list (mrs::vsym "UNIQUE")))

(defconstant *mtr-skolem-property* (mrs::vsym "SKOLEM"))

(defconstant *mtr-scratch-property* (mrs::vsym "SCRATCH"))

(defconstant *mtr-mark-property* (mrs::vsym "MARK"))

(defconstant *mtr-ditch-property* (mrs::vsym "DITCH"))

(defconstant *mtr-upcase-operator* (mrs::vsym "+upcase+"))

(defconstant *mtr-downcase-operator* (mrs::vsym "+downcase+"))

(defconstant *mtr-copy-operator* (mrs::vsym "+copy+"))

(defconstant *mtr-equal-operator* (mrs::vsym "+equal+"))

(defconstant *mtr-subsume-operator* (mrs::vsym "+subsume+"))

(defconstant *mtr-true-type* (mrs::vsym "+"))

(defconstant *mtr-false-type* (mrs::vsym "-"))

(defconstant *semi-u-type* "u")

(defconstant *semi-h-type* "h")

(defconstant *semi-i-type* "i")

(defconstant *semi-e-type* "e")

(defconstant *semi-x-type* "x")
