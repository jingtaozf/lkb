(in-package :mrs)

;;;
;;; LOGON-specific MRS globals; on by default for LOGON, not loaded in the core
;;; LKB system otherwise.
;;;

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
  (append
   (list (vsym "SORT") (vsym "INSTLOC"))
   *ignored-extra-features*))

(setf *mrs-equalp-ignored-roles*
  (list (vsym "LNK") (vsym "PSV")))

;;;
;;; types for variable naming in output (copy from `.../src/mrs/mrsglobals.lsp'
;;; but here to remind us to adapt them, as appropriate).
;;;

(setf *event-type* (vsym "e"))
(setf *event_or_index-type* (vsym "i"))
(setf *handle-type* (vsym "h"))
(setf *ref-ind-type* (vsym "x"))

(setf *semi-fragment-relations*
  (list "fragment_rel" (vsym "unspec_conj_rel")))
