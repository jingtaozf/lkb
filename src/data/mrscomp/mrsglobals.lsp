(in-package "MRS")

(defparameter *initial-semantics-path* `(,(vsym "SEM") ))

(defparameter *main-semantics-path* `(,(vsym "SEM") ,(vsym "RELS") ,(vsym "LIST")))

(defparameter *construction-semantics-path* nil)

(defparameter *top-semantics-type* (vsym "RELATION"))

(defparameter *rel-name-path* `(,(vsym "PRED") ))

(defparameter *dummy-relations* `("no_rel"))

(defparameter *value-feats* `(,(vsym "NAME")))

(defparameter *psoa-index-path* 
  `(,(vsym "HOOK") ,(vsym "INDEX"))
  "path to get an index from a psoa")

(defparameter *psoa-top-h-path* nil)

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
    `( ,(vsym "LTOP") ,(vsym "INDEX") ,(vsym "LBL") ,(vsym "BV")
       ,(vsym "ARG0") ,(vsym "ARG1") ,(vsym "ARG2") ,(vsym "ARG3") 
       ,(vsym "RESTR") ,(vsym "BODY"))
  "A not-necessarily-complete list of features that determines printing
order in the MRS output routines and also determines the order
of variables in the indexed representation")

(defparameter *top-level-rel-types* 
    '("it_rel" "she_rel" "he_rel" "they_rel" "her_rel" "kim_rel" "sandy_rel"))

(defparameter *quant-rel-types* nil)

(defparameter *bv-feature* (vsym "BV"))

(defparameter *scope-feat* (vsym "BODY"))

;;; parameters for slot detection

(defparameter *algebra-ignore-feats* '(lkb::arg-s lkb::kcmp))

(defparameter *algebra-ignore-paths*
    '((lkb::args) (lkb::c-cont)
      (lkb::sem)
      (lkb::synsem lkb::local lkb::cont)))

;;; following is for naming of slots
(defparameter *non-slot-features* '(lkb::SEM lkb::cont lkb::HOOK lkb::SYNSEM 
				    lkb::LOCAL lkb::cat lkb::val lkb::head))

(defparameter *fix-spelling-fn* 'lkb::fix-spelling)



