(in-package :mt)

(defparameter *transfer-rule-sets* nil)

(defparameter *transfer-result-filter* '(:complete))

(defparameter *mtr-filter-path* (list (mrs::vsym "FILTER")))

(defparameter *mtr-context-path* (list (mrs::vsym "CONTEXT")))

(defparameter *mtr-input-path* (list (mrs::vsym "INPUT")))

(defparameter *mtr-output-path* (list (mrs::vsym "OUTPUT")))

(defparameter *mtr-flags-path* (list (mrs::vsym "FLAGS")))

(defconstant *mtr-optional-path* (list (mrs::vsym "OPTIONAL")))

(defconstant *mtr-upcase-operator* (mrs::vsym "+upcase+"))

(defconstant *mtr-downcase-operator* (mrs::vsym "+downcase+"))
