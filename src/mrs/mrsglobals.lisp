;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   $RCSfile$
;;  $Revision$
;;      $Date$
;;     Author: Walter Kasper (DFKI)
;;    Purpose: 
;;   Language: Allegro Common Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; $Log$
;; Revision 1.12  1999/04/09 23:20:56  danf
;; Merged WK's changes
;;
;; Revision 1.11  1999/03/06 03:45:59  aac
;; got rid of special message stuff in generator, added code so compatible lexical entries can be retrieved, added menu item in CLIM for lexical ids on trees
;;
;; Revision 1.10  1999/03/04 06:03:52  aac
;; construction semantics correct for lexical rules, redid equality checking
;;
;; Revision 1.9  1999/02/25 06:27:48  aac
;; not very well tested changes to generation lexical lookup, also fixing references to user rather than cl-user package
;;
;; Revision 1.8  1999/01/16 05:12:15  aac
;; minor fixes because of PC version, generator changes
;;
;; Revision 1.7  1998/10/09 00:48:46  aac
;; merging in Walter's code again ...
;;
;; Revision 1.6  1998/10/07 20:54:19  danf
;; Added support for VM word latices
;;
;; Revision 1.5  1998/09/04 00:43:31  aac
;; merging WK's changes
;;
;; Revision 1.4  1998/08/24 21:59:14  oe
;; committing minor changes contributed by the manager; make MRS work for PAGE ...
;;
;; Revision 1.3  1998/08/12 01:38:30  malouf
;; Change to DFKI directory structure and add tsdb.
;;
;; Revision 1.2  1998/07/23 01:24:05  aac
;; mrs equality and removing remnants of page packages
;;
;; Revision 1.1  1998/06/24 17:15:11  aac
;; adding mrs code to source control
;;
;; Revision 1.1.1.1  1997/12/12 20:18:29  malouf
;; DFKI preliminary version of 11-Dec-1997.
;; 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MRS")

(defparameter *mrs-results-check* nil)

(defparameter *mrs-for-language* 'german)

(defparameter *mrs-to-vit* t)

(defparameter *mrs-scoping* nil)

;;; to be done: use (tdl::show-current-domain) rather than hardwire
;;; domain-package 
;;; the defaults are for German

(defparameter *initial-semantics-path* 
  `(,(vsym "SYNSEM") ,(vsym "LOC") ,(vsym "CONT"))
  "Following this path gets you to the MRS structure")

(defparameter *rel-name-path*
    `(,(vsym "PRED"))
  "path to get the predicate name if it exists")

(defparameter *rel-handel-path*
    `(,(vsym "HANDEL"))
  "path to get the handel from a relation")

(defparameter *psoa-top-h-path* 
  `(,(vsym "TOP-H"))
  "path to get the top handle from a psoa")

(defparameter *psoa-handel-path* 
  `(,(vsym "HANDEL"))
  "path to get a handle from a psoa")

(defparameter *psoa-label-path* 
  `(,(vsym "LABEL"))
  "path to get a handle from a psoa")

(defparameter *psoa-event-path* 
  `(,(vsym "INDEX"))
  "path to get an event from a psoa")

(defparameter *psoa-liszt-path* 
    `(,(vsym "LISZT") ,(vsym "LIST"))
  "path to get a liszt from a psoa")

(defparameter *psoa-rh-cons-path*
    `(,(vsym "H-CONS") ,(vsym "LIST"))
  "path to get a list of handle constraints from a root psoa")

(defparameter *psoa-h-cons-path*
    `(,(vsym "H-CONS") ,(vsym "LIST"))
  "path to get a list of handle constraints from a psoa")

(defparameter *psoa-constr-path* 
  `(,(vsym "SC-ARG"))
  "path to get a handle from a psoa")

(defparameter *psoa-message-path* 
  `(,(vsym "MESSAGE"))
  "path to get a handle from a psoa")

(defparameter *liszt-first-path* 
  `(,(vsym "FIRST"))
  "path for first element in a liszt")

(defparameter *liszt-rest-path* 
  `(,(vsym "REST"))
  "path for rest of a liszt")

(defparameter *psoa-wgliszt-path*
    `(,(vsym "WGLISZT") ,(vsym "LIST"))
  "path to get the word lattice identifiers from a psoa")

(defparameter *value-feats* `(,(vsym "VREF") ,(vsym "VTYPE")
                              ,(vsym "MONTH") ,(vsym "DAY") ,(vsym "HOUR") 
			       ,(vsym "MINUTE") ,(vsym "ORD") 
                              ,(vsym "NAMED") ,(vsym "PRED") 
                              ,(vsym "DEMONTYPE"))
   "A list of features within an MRS that takes constant values
   instead of variables")

(defparameter *feat-priority-list*  `( ,(vsym "HANDEL") ,(vsym "INDEX")
				       ,(vsym "EVENT") ,(vsym "INST")
                                       ,(vsym "ACT") ,(vsym "BV") 
                                       ,(vsym "RESTR") ,(vsym "SCOPE") 
				       ,(vsym "QUANT"))
  "A not-necessarily-complete list of features that determines printing
order in an MRS")

(defparameter *do-not-convert-sort-list* nil
  "relations which will be ignored in the conversion process")

(defparameter *relation-extra-feats* nil
  "A list of features in rel-structures containing additional information")

(defparameter *ignored-sem-features* nil
  "A list of features which are ignored completely")

(defparameter *sem-sort-suffix* nil
  "a suffix string marking semantic sorts")

(defparameter *sem-relation-suffix* nil
  "a suffix string in relation names (removed in VITS)")

(defparameter *sem-relation-prefix* nil
  "a prefix string in relation names (removed in VITs)")

(defvar *vit-hole-prefix* "HH"
  "a prefix string designating a hole in VITs")

(defvar *vit-instance-prefix* "IH"
  "a prefix string designating an instance in VITs")

(defvar *vit-label-prefix* "LH"
  "a prefix string designating an instance in VITs")

;;; at present only simple feature-value-pairs are treated (no complex values)

(defparameter *index-feature-transform-table* nil
  "an assoc list of entries (Feature VIT-accessor (Value . VIT-Values)
VIT-Values is a list of pairs (VIT-special-form  VIT-value)
a default special form can be specified by (t VIT-special-form)
Value is then used as it is; otherwise missing values are ignored
a default value can be specified by '(others (VIT-special-form defaultvalue))'
TDL-type-checking is invoked on entries '(type Supertype VIT-Values)'
Search order is left to right
The distinction between simple Value and type checking is used to filter out
  certain Values which either have a different representation in VITs or none
 at all e.g. underspecified values")

(defparameter *relation-extra-transform-table* nil
  "an assoc list similar to that of *index-feature-transform-table*
for extra features for using the extra-features of a relation")

(defparameter *vit-sort-feature* (vsym "SORT")
  "name of features for fs-sort slot")

(defparameter *vm-top-sort-symbol* (vsym "ANYTHING")
  "top sort in the VM ontology")

(defparameter *vm-ignored-sort-list* (list *vm-top-sort-symbol*)
  "list of sorts which should not enter vits")

(defparameter *vm-ignored-sentence-mood* `(,(vsym "MESSAGE"))
  "list of sentence moods (message-types) which should be ignored in VIT")

(defparameter *mrs-arg-features* nil
  "assoc-list of arg-features with default-VIT-roles")

;;; spezielle Typen
(defparameter *special-type-info* nil
  "assoc-list for type-value info")

(defparameter *special-type-treatment* nil
  "assoc-list (type . function) ; function takes rel, vit, groups, labels and
  should return the vit")

(defparameter *relation-type-check* nil
  "assoc list '(supertype accessor VIT-Values)'")

;;; *vm-special-label-hack-list*
;;; assoc list of (relation . arg); rg is a number (the nth element in flist); 
;;; at present this seems to be more robust than the use of feature
;;; names; arg refers to a label which according to spme unaccountable
;;; principles of transfer must be base label

(defparameter *vm-special-label-hack-list* nil)

;    `((,(vsym "support_rel") . 1)
;      (,(vsym "nominalize-rel") . 1)
;       (,(vsym "nominalize_rel") . 1)
;       (,(vsym "support-rel") . 1)))

(defparameter *complex-extra-feats* `(,(vsym "VIT")))

;;; The following globals replace stuff hardwired into mrsoutput.lisp
;;; features

(defparameter *list-feature* (vsym "LIST"))

(defparameter *sc-arg-feature* (vsym "SC-ARG"))

(defparameter *cands-feature* (vsym "CANDS"))

(defparameter *outscpd-feature* (vsym "OUTSCPD"))

(defparameter *prec-feature* (vsym "PREC"))

(defparameter *word-feature* (vsym "WORD"))

(defparameter *id-feature* (vsym "ID"))

(defparameter *handels-feature* (vsym "HANDELS"))

;;; paths

(defparameter *time-path* `(,(vsym "TIME")))

(defparameter *reference-path* `(,(vsym "REFERENCE")))

(defparameter *spch-path* `(,(vsym "SPCH")))

(defparameter *png-path* `(,(vsym "PNG")))

(defparameter *pn-path*  `(,(vsym "PN")))

(defparameter *gen-path*  `(,(vsym "GEN")))

(defparameter *first-path*  `(,(vsym "FIRST")))

(defparameter *rest-path*  `(,(vsym "REST")))

(defparameter *list-path*  `(,(vsym "LIST")))

(defparameter *last-path*  `(,(vsym "LAST")))

;;; used in mrs-to-vit

(defparameter *throle-feature* (vsym "THROLE"))

(defvar *prosodic-accent-feature* (vsym "ACCENT"))

(defvar *prosodic-mood-feature* (vsym "PMOOD"))

;; an assoc list listing for each prosodic mood compatible syntactic moods
(defvar *prosodic-syntactic-mood-table* nil)

;;; used in mrsresolve

(defparameter *bv-feature* (vsym "BV"))

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


(defparameter *no-inst-arg-roles* nil)


;;;
;;; MRS output control

(defparameter *mrs-output-p* nil)
(defparameter *raw-mrs-output-p* nil)

;;; display of extra features in an MRS

(defparameter *mrs-extra-display* nil)

;;; take VM arg role control out of package MAIN

(defvar *VM-arg-roles-only-p* nil)
(defvar *VM-arg-roles* nil)
(defvar *suppressed-VM-arg-roles* nil)

;;; for generation - real values in mrsglobals-eng

(defparameter *dummy-relations* nil)

(defparameter *main-semantics-path* nil)

(defparameter *construction-semantics-path* nil)

(defparameter *external-semantics-path* nil)

(defparameter *message-semantics-path* nil)

(defparameter *top-semantics-type* nil)

(defparameter *top-semantics-entry* nil
  "set in lexutils")

;;; for passing non-mrs-information (SYN, CTXT) to Vits:

(defvar *psoa-extras-paths* nil)
(defvar *syntax-paths* nil)

;;; for using the prolog vitADT site specific stuff:
;;; values must contain program/library-path if necessary
(defvar *prolog-parameters* '((prolog-cmd . "prolog")
                              (vitADT . "vitADT")))


(defparameter *key-handel-path* nil)

;;; for munging

(defparameter *ordered-mrs-rule-list* nil)

;;; for scoping

(defvar *canonical-bindings* nil
"global variable which is set to the current set of bindings for the
printing routines -  convenient to make this global to keep printing generic")
