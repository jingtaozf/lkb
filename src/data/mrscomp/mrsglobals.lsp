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
      (head-modifier-rule 2 (MOD1))
      (modifier-head-rule 1 (MOD1))
      (noun-modifier-rule 1 nil)
      (bare-pl-noun-rule 0 (DTR1))
      (passive-rule 1 nil) ; modifies slots
      (inversion-rule 1 nil)		; ditto
      (coord-rule 1 nil)		; wrong ...
      (head-gap-rule-1 1 nil) ; modifies slots
      (head-gap-rule-2 1 (COMP2))		; modifies slots
      (head-gap-rule-3 1 (COMP1))	; modifies slots
      (head-filler-rule 2 (GAP1))
      (dative-shift-lrule 1 nil)	; modifies slots
      (past-v_irule 1 nil)
      (non3sg-v_irule 1 nil)
      (non3sg-v-regular_irule 1 nil)
      (3sg-v_irule 1 nil)
      (pres-part-v_irule 1 nil)
      (past-part-v_irule 1 nil)
      (infl-v_irule 1 nil)
      (pl-noun_irule 1 nil) 
      (sg-noun_irule 1 nil)
      (const-pump 1 nil)))
      
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

(defparameter *fix-spelling-fn* 'lkb::fix-spelling)



