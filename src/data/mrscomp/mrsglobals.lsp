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

(defparameter *rule-algebra-table*
    '((head-complement-rule-0  1 nil)
      (head-complement-rule-1  1 (COMPS1))
      (head-complement-rule-2  1 (COMPS1 COMPS2) (2 3))
      (head-specifier-rule  2 (SPR1))
      (determiner-head-rule  1 (SPEC))
      (head-modifier-rule 2 (MOD))
      (modifier-head-rule 1 (MOD))
      (noun-modifier-rule 1 nil)
      (bare-pl-noun-rule 0 (DTR1))
      (passive-rule 1 nil) ; modifies slots
      (inversion-rule 1 nil)		; ditto
      (coord-rule 1 nil)		; wrong ...
      (head-gap-rule-1 1 nil) ; modifies slots
      (head-gap-rule-2 1 (COMP2))		; modifies slots
      (head-gap-rule-3 1 (COMP1))	; modifies slots
      (head-filler-rule 2 (GAP))))
      
;;;  rule-name   semhead slot

;;; parameters for slot detection

(defparameter *algebra-ignore-feats* '(lkb::arg-s lkb::kcmp))

(defparameter *algebra-ignore-paths*
    '((lkb::args) (lkb::c-cont)
      (lkb::sem)
      (lkb::synsem lkb::local lkb::cont)))

;;; following is for naming of slots
(defparameter *non-slot-features* '(lkb::SEM lkb::cont lkb::HOOK lkb::SYNSEM 
				    lkb::LOCAL lkb::cat lkb::val lkb::head))
