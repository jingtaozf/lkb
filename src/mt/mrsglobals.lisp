(in-package :mrs)

;;;
;;; these are general MRS variables, irrespective of the AVM encoding used in
;;; a particular grammar.
;;; 

(setf *rel-handel-path* (list (vsym "LBL")))

(setf *sc-arg-feature* (vsym "HARG"))

(setf *outscpd-feature* (vsym "LARG"))

(setf *bv-feature* (vsym "ARG0"))

(setf *scope-feat* (vsym "BODY"))

(setf *ignored-sem-features* nil)

(setf *top-semantics-type* (vsym "RELATION"))

(setf *value-feats* 
  (list
   (vsym "CARG")))

(setf *sem-relation-suffix* "_rel")

(setf *ignored-extra-features* 
  (list 
   (vsym "SORT")))

;;;
;;; types for variable naming in output (copy from `.../src/mrs/mrsglobals.lsp'
;;; but here to remind us to adapt them, as appropriate).
;;;

(setf *event-type* (vsym "e"))
(setf *event_or_index-type* (vsym "i"))
(setf *handle-type* (vsym "h"))
(setf *ref-ind-type* (vsym "x"))

(setf %mrs-extras-filter%
  (list
   (cons (mrs::vsym "NUM") (mrs::vsym "NUMBER"))
   (cons (mrs::vsym "PERS") (mrs::vsym "PERSON"))
   (cons (mrs::vsym "NATGEND") (mrs::vsym "GENDER"))
   (cons (mrs::vsym "TENSE") (mrs::vsym "TENSE"))
   (cons (mrs::vsym "PROG") (mrs::vsym "LUK"))
   (cons (mrs::vsym "PERF") (mrs::vsym "LUK"))))
