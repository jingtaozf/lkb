(in-package "MRS")

(defparameter *initial-semantics-path* `(,(vsym "SEM") ))

(defparameter *main-semantics-path* `(,(vsym "SEM") ,(vsym "RELS") ,(vsym "LIST")))

(defparameter *construction-semantics-path* nil)

(defparameter *top-semantics-type* (vsym "RELATION"))

(defparameter *rel-name-path* `(,(vsym "PRED") ))

(defparameter *dummy-relations* `(,(vsym "no_rel")))

(defparameter *value-feats* `(,(vsym "NAME")))

(defparameter *psoa-index-path* 
  `(,(vsym "HOOK") ,(vsym "INDEX"))
  "path to get an index from a psoa")

(defparameter *psoa-liszt-path* `(,(vsym "RELS") ,(vsym "LIST")))
(defparameter *psoa-rh-cons-path* `(,(vsym "HCONS") ,(vsym "LIST")))

(defparameter *ref-ind-type* (vsym "object"))

(defparameter *rel-handel-path*
    `(,(vsym "HNDL"))
  "path to get the handel from a relation")

(defparameter *sc-arg-feature* (vsym "HARG")
  "the feature in a qeq that leads to the first argument")

(defparameter *outscpd-feature* (vsym "LARG")
  "the feature in a qeq that leads to the second argument")

(defparameter *feat-priority-list*  
    `( ,(vsym "HNDL") ,(vsym "INDEX"))
  "A not-necessarily-complete list of features that determines printing
order in an MRS")

(defparameter *top-level-rel-types* 
    '("it_rel" "she_rel"))

(defparameter *quant-rel-types* nil)

(defparameter *bv-feature* (vsym "BV"))

(defparameter *scope-feat* (vsym "BODY"))