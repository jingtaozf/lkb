;;; Copyright (c) 1998-2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package "MRS")

;;; Globals - freed of VM specific stuff

(defparameter *giving-demo-p* nil)
;; when set, this avoids error and warning messages

(defparameter *mrs-results-check* nil)
;; mrscorpus and mrsfns - causes results to be checked against
;; previous stored results.  Currently non-operational.

(defparameter *mrs-scoping* nil)
;; interface control - causes scoping code to be run when set

(defparameter *mrs-output-p* nil)
;; interface control - causes MRS code to be run

;;; Following are needed to construct an MRS feature
;;; structure, or to create an MRS Lisp structure from a 
;;; feature structure

(defparameter *initial-semantics-path* 
  `(,(vsym "SYNSEM") ,(vsym "LOCAL") ,(vsym "CONT"))
  "Following this path into a sign gets you to the MRS structure")

(defparameter *rel-name-path*
    `(,(vsym "PRED"))
  "path within a rel to get the predicate name 
   (type of rel may be used instead)")
;;; i.e., this is a default location for a predicate name within
;;; a relation - if this path doesn't have a value, the type of the
;;; relation is used instead.  This is designed for the situation
;;; where some relations have PREDs and others don't.

(defparameter *rel-handel-path*
    `(,(vsym "HANDEL"))
  "path to get the handel from a relation")

(defparameter *rel-cto-feature*
    (vsym "CTO")
  "feature with the CTO")

(defparameter *rel-cfrom-feature*
    (vsym "CFROM")
  "feature with the CFROM")

(defparameter *psoa-top-h-path* 
  `(,(vsym "TOP-H"))
  "path to get the top handle from a psoa")

(defparameter *psoa-index-path* 
  `(,(vsym "INDEX"))
  "path to get an index from a psoa")

(defparameter *psoa-liszt-path* 
    `(,(vsym "LISZT") ,(vsym "LIST"))
  "path to get a liszt from a psoa")

(defparameter *psoa-rh-cons-path*
    `(,(vsym "H-CONS") ,(vsym "LIST"))
  "path to get a list of handle constraints from a root psoa")

(defparameter *sc-arg-feature* (vsym "SC-ARG")
  "the feature in a qeq that leads to the first argument")

(defparameter *outscpd-feature* (vsym "OUTSCPD")
  "the feature in a qeq that leads to the second argument")

(defparameter *qeq-type* (vsym "qeq")
  "the type associated with a qeq relation")

;;; generic paths

(defparameter *first-path*  `(,(vsym "FIRST"))
  "path for first element in a list and a liszt")
;;; note - assumption is made in mrsoutput that this is a singleton

(defparameter *rest-path*  `(,(vsym "REST")))
;;; note - assumption is made in mrsoutput that this is a singleton

;;; other globals for FS <-> MRS structure conversion

(defparameter *value-feats* nil
   "A list of features within an MRS that take constant values
   instead of variables")

(defparameter *feat-priority-list*  
    `( ,(vsym "TOP-H") ,(vsym "HANDEL") ,(vsym "INDEX"))
  "A not-necessarily-complete list of features that determines printing
order in an MRS")

(defparameter *ignored-sem-features* `( ,(vsym "IDIOMP")
					,(vsym "CFROM")
					,(vsym "CTO"))
  "A list of features which are ignored completely")

(defparameter *ignored-extra-features* `( ,(vsym "INSTLOC"))
  "A list of features in the variable substructure 
   which are ignored completely")

;;; types for variable naming in mrsoutput

(defparameter *event-type* (vsym "event"))
(defparameter *event_or_index-type* (vsym "event_or_index"))
(defparameter *non_expl-ind-type* (vsym "non_expl-ind"))
(defparameter *handle-type* (vsym "handle"))
(defparameter *ref-ind-type* (vsym "ref-ind"))
(defparameter *deg-ind-type* (vsym "deg-ind"))


;;; used in mrsresolve

(defparameter *bv-feature* (vsym "BV"))

(defparameter *quant-rel-types* nil)

(defparameter *scope-feat* (vsym "SCOPE"))


;;;
;;; MRS output control

(defparameter *sem-relation-suffix* nil
  "a suffix string marking semantic relations - 
   used in the compact MRS representation")

;;; for generation - real values in mrsglobals-eng

(defparameter *null-semantics-hack-p* nil
  "for debugging - if set, this cheats on null semantic items
   WARNING - do not set when processing in batch mode")

(defparameter *dummy-relations* nil)
;;; this allows the LISZT to contain relations that
;;; have no effect on the MRS.  The type of the relation should be
;;; specified

(defparameter *main-semantics-path* nil
  "the path into a lexical entry which gives the list of
   relations - typically (append *initial-semantics-path* '(LISZT LIST))")

(defparameter *construction-semantics-path* nil
  "the path into a rule/construction which gives the
   semantics specific to that construction")

(defparameter *top-semantics-type* nil
  "the highest type in the hierarchy under which all
   rels are found")

(defparameter *top-semantics-entry* nil
  "set in lexutils - not user-settable")

;;; INSTLOC is a feature that should appear on all feature structures
;;; representing variables (handles etc)  It is needed by the generator
;;; as a location for a constant value that distinguishes different
;;; variables.  

(defparameter *instloc-type* (vsym "INSTLOC"))

(defparameter *instloc-path*   `(,(vsym "INSTLOC")))

;;; for munging - not user-settable

(defparameter *ordered-mrs-rule-list* nil)

(defparameter *inverted-mrs-rule-list* nil)

;;; for generator heuristics - list of ids dealt with - not user-settable

(defparameter *gen-rule-ids* nil)

;;; for scoping

;;; not user-settable
(defvar *canonical-bindings* nil
"global variable which is set to the current set of bindings for the
printing routines -  convenient to make this global to keep printing generic")


;;; set in mrsglobals-eng

(defvar *top-level-rel-types* nil
  "the types of any relations which introduce variables
which need not be scoped - pronouns etc")

(defvar *top-level-variables* nil
  "the variables which correspond to pronouns -
set in the code")

;;; to control scoping or cheap scope

(defvar *fragment-p* nil
  "if t for a parse, cheapscope is not called - not user settable")


;;; the following are needed only for the detection of fragments
;;; indicated in the LinGO gramar by the value of ROOT


(defvar *root-path* nil)

(defvar *false-type* nil)
(defvar *true-type* nil)

(defparameter *alex-mode* nil
  "if t, allows scope to have specified relations")

;;; for some reason, the function determine-variable-type 
;;; was moved here, where it surely doesn't belong (given that it's
;;; a function ...)
;;; Moved (back?) to mrsoutput.lisp, since it only relates to
;;; grammar-to-MRS construction.  Note that the types available
;;; for variables are now something that's part of the definition of
;;; MRS/RMRS, so not user-controllable any more.

