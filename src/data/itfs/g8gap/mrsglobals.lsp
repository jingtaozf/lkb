(in-package "MRS")

(defparameter *initial-semantics-path* `(,(vsym "SEM") ))

(defparameter *main-semantics-path* `(,(vsym "SEM") ,(vsym "RELS") ,(vsym "LIST")))

(defparameter *top-semantics-type* (vsym "RELATION"))

(defparameter *rel-name-path* `(,(vsym "PRED") ))

(defparameter *rel-handel-path* nil)

(defparameter *psoa-top-h-path* nil)

(defparameter *psoa-liszt-path* `(,(vsym "RELS") ,(vsym "LIST")))

(defparameter *psoa-rh-cons-path* nil)

(defparameter *ref-ind-type* (vsym "object"))

