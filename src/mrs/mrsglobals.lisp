(in-package "MRS")

;;; Globals - freed of VM specific stuff

(defparameter *mrs-results-check* nil)
;; mrscorpus and mrsfns - causes results to be checked against
;; previous stored results

(defparameter *mrs-scoping* nil)
;; interface control - causes scoping code to be run

(defparameter *mrs-output-p* nil)
;; interface control - causes MRS code to be run

;;; Following are needed to construct an MRS feature
;;; structure, or to create an MRS Lisp structure from a 
;;; feature structure

(defparameter *initial-semantics-path* 
  `(,(vsym "SYNSEM") ,(vsym "LOCAL") ,(vsym "CONT"))
  "Following this path gets you to the MRS structure")

(defparameter *rel-name-path*
    `(,(vsym "PRED"))
  "path within a rel to get the predicate name 
   (type of rel may be used instead)")

(defparameter *rel-handel-path*
    `(,(vsym "HANDEL"))
  "path to get the handel from a relation")

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

(defparameter *sc-arg-feature* (vsym "SC-ARG"))

(defparameter *outscpd-feature* (vsym "OUTSCPD"))

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

(defparameter *ignored-sem-features* nil
  "A list of features which are ignored completely")

(defparameter *ignored-extra-features* `( ,(vsym "INSTLOC"))
  "A list of features in the variable substructure 
   which are ignored completely")

;;; types for variable naming in mrsoutput

(defparameter *event-type* (vsym "event"))
(defparameter *event_or_index-type* (vsym "event_or_index"))
(defparameter *non_expl-ind-type* (vsym "non_expl-ind"))
(defparameter *eventtime-type* (vsym "eventtime"))
(defparameter *handle-type* (vsym "handle"))
(defparameter *group_lab-type* (vsym "group_lab"))
(defparameter *hole-type* (vsym "hole"))
(defparameter *label-type* (vsym "label"))
(defparameter *ref-ind-type* (vsym "ref-ind"))
(defparameter *full_ref-ind-type* (vsym "full_ref-ind"))
(defparameter *deg-ind-type* (vsym "deg-ind"))
(defparameter *individual-type* (vsym "individual"))
(defparameter *difference-list-type* (vsym "*diff-list*"))
(defparameter *conj-ind-type* (vsym "conj-ind"))


;;; used in mrsresolve

(defparameter *bv-feature* (vsym "BV"))

(defparameter *scope-feat* (vsym "SCOPE"))


;;;
;;; MRS output control

(defparameter *sem-relation-suffix* nil
  "a suffix string marking semantic relations - 
   used in the compact MRS representation")

;;; for generation - real values in mrsglobals-eng

(defparameter *null-semantics-hack-p* nil
  "for debugging - if set, this cheats on null semnatic items")

(defparameter *dummy-relations* nil)

(defparameter *main-semantics-path* nil)

(defparameter *construction-semantics-path* nil)

(defparameter *top-semantics-type* nil)

(defparameter *top-semantics-entry* nil
  "set in lexutils")

(defparameter *instloc-type* (vsym "INSTLOC"))

(defparameter *instloc-path*   `(,(vsym "INSTLOC")))

(defparameter *rel-name-path-only* nil
  "if set, the indexing code only looks for the rel on the
   rel-name-path")


;;; for munging

(defparameter *ordered-mrs-rule-list* nil)

(defparameter *inverted-mrs-rule-list* nil)

;;; for scoping

(defvar *canonical-bindings* nil
"global variable which is set to the current set of bindings for the
printing routines -  convenient to make this global to keep printing generic")

;;; set in mrsglobals-eng

(defvar *top-level-rel-types* nil)

(defvar *top-level-variables* nil
  "the variables which correspond to pronouns or proper names")

;;; to control scoping or cheap scope

(defvar *fragment-p* nil
  "if t for a parse, cheapscope is not called")

(defvar *root-path* nil)

(defvar *false-type* nil)
(defvar *true-type* nil)

(defparameter *alex-mode* nil
  "if t, allows scope to have specified relations")

