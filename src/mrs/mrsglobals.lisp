;;; Copyright (c) 1998-2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `LICENSE' for conditions.

(in-package "MRS")

;;; Globals for MRS
;;; This version is aligned with the 2004 version of the MRS paper
;;; and therefore quite extensively changed from previous versions.
;;; This may necessitate some changes to mrsglobals for grammars
;;; which use earlier style MRS and were relying on the defaults here.
;;; Changing the actual variable names would require
;;; extensive hacking of the code and cause far more serious disruption
;;; with older grammars - comments here briefly explain old nomenclature

;;; Note the use of vsym below - this allows for the code to be
;;; run independently of the LKB and without worrying about
;;; having to specify packages.  

;;; The following are needed to construct an MRS feature
;;; structure, or to create an MRS Lisp structure from a 
;;; feature structure

(defparameter *initial-semantics-path* 
  `(,(vsym "SYNSEM") ,(vsym "LOCAL") ,(vsym "CONT"))
  "Following this path into a sign fs gets you to the MRS fs")

(defparameter *rel-name-path*
    `(,(vsym "PRED"))
  "path within a relation fs to get the predicate name 
   (this may be set no nil, in which case the type of relation 
    is used instead to extract the predicate)")

(defparameter *rel-handel-path*
    `(,(vsym "LBL"))
  "path to get the label from a relation fs (old MRS called this HANDEL)")

(defparameter *rel-cto-feature*
    (vsym "CTO")
  "cto feature for recording character positions on relations")

(defparameter *rel-cfrom-feature*
    (vsym "CFROM")
  "cfrom feature for recording character positions on relations")


(defparameter *lnkp* :characters
  "activate recording of surface links (whatever form they take) in EPs")

(defparameter *rel-lnk-path* (list (vsym "LNK"))
  "list-valued attribute on EPs to record surface links (in whatever form)")

(defparameter *show-lnk-p* t
  "boolean flag to toggle output of surface links (e.g. character positions)")

(defparameter *hook-type* 
  (vsym "HOOK")
  "type of the hook in an mrs fs")

(defparameter *hook-path* 
  `(,(vsym "HOOK"))
  "path to get the hook from an mrs fs")

(defparameter *ltop-path* 
  `(,(vsym "LTOP"))
  "path to get the ltop from an mrs hook fs")

(defparameter *index-path* 
  `(,(vsym "INDEX"))
  "path to get the index from an mrs hook fs")

(defparameter *xarg-path* 
  `(,(vsym "XARG"))
  "path to get the xarg from an mrs hook fs")

(defparameter *top-hcons* nil
  "optional handle constraint (e.g. `qeq') to be inserted above the LTOP, when
   creating an MRS from a TFS, to avoid leaving the TOP handle just dangling")

;;; following three are retained for backward compatability only

(defparameter *psoa-top-h-path* 
  `(,(vsym "HOOK") ,(vsym "LTOP"))
  "path to get the hook's ltop from an mrs fs (psoa - parameterised state of affairs -
   old terminology) - this should be NIL if messages are not used")

(defparameter *psoa-index-path* 
  `(,(vsym "HOOK") ,(vsym "INDEX"))
  "path to get the hook's index from an mrs")

(defparameter *psoa-xarg-path* 
  `(,(vsym "HOOK") ,(vsym "XARG"))
  "path to get the hook's external index from an mrs")

;;; end obsolete

(defparameter *psoa-liszt-path* 
    `(,(vsym "RELS") ,(vsym "LIST"))
  "path to get the list of relations from an mrs")
;;; note LIST here - assumption is that we're using a difference list
;;; encoding

(defparameter *psoa-rh-cons-path*
    `(,(vsym "HCONS") ,(vsym "LIST"))
  "path to get a list of handle constraints from an mrs fs")

(defparameter *sc-arg-feature* (vsym "HARG")
  "the feature in a qeq that leads to the first argument")

(defparameter *outscpd-feature* (vsym "LARG")
  "the feature in a qeq that leads to the second argument")

(defparameter *qeq-type* (vsym "qeq")
  "the fs type associated with a qeq relation")

;;; additions for ICONS, parallel HCONS

(defparameter *psoa-icons-path*
    `(,(vsym "ICONS") ,(vsym "LIST"))
  "path to get a list of icons from an mrs fs")

(defparameter *iarg1-feature* (vsym "IARG1")
  "the feature in an icons that leads to the first argument")

(defparameter *iarg2-feature* (vsym "IARG2")
  "the feature in an icons that leads to the second argument")

;;; no constraint on relation

(defparameter *psoa-a-cons-path*
    `(,(vsym "ACONS") ,(vsym "LIST"))
  "path to get a list of attachment constraints from an mrs fs")

(defparameter *mod-spec-type* (vsym "ISECT-MOD")
  "the fs type associated with a modifier attachment relation")

(defparameter *mod-anc* (vsym "MOD-ANC")
  "the feature in the mod spec that leads to the modifier anchor")

(defparameter *target-ancs-path* 
    `(,(vsym "TARGET-ANCS") ,(vsym "LIST"))
  "the path in the mod spec that leads to the target list")


;;; generic paths

(defparameter *first-path*  `(,(vsym "FIRST"))
  "path for first element in a list and a rels list")
;;; note - assumption is made in mrsoutput that this is a singleton

(defparameter *rest-path*  `(,(vsym "REST")))
;;; note - assumption is made in mrsoutput that this is a singleton

;;; other globals for FS <-> MRS structure conversion

(defparameter *value-feats* `(,(vsym "CARG"))
   "A list of features within an MRS that take constant values
   instead of variables")

(defparameter *feat-priority-list*  
    `( ,(vsym "LTOP") ,(vsym "INDEX") ,(vsym "LBL")
       ,(vsym "ARG0") ,(vsym "ARG1") ,(vsym "ARG2") ,(vsym "ARG3") 
       ,(vsym "RSTR") ,(vsym "BODY")
       ,(vsym "MARG") ,(vsym "CARG"))
  "A not-necessarily-complete list of features that determines printing
order in the MRS output routines and also determines the order
of variables in the indexed representation")

(defparameter *property-order*
  (list
   (vsym "SF") (vsym "TENSE") (vsym "MOOD") (vsym "PERF") (vsym "PROG")
   (vsym "PERS") (vsym "NUM") (vsym "GEND") 
   (vsym "IND") (vsym "DIV") (vsym "PT")))

(defparameter *ignored-sem-features* `(,(vsym "IDIOMP")
                                       ,(vsym "LNK")
                                       ,(vsym "CFROM")
                                       ,(vsym "CTO"))
  "A list of features which are ignored completely when constructing
an MRS from a FS representation of an MRS")

(defparameter *ignored-extra-features* `( ,(vsym "INSTLOC"))
  "A list of features in the variable substructure 
   which are ignored completely when constructing
   an MRS from a FS representation of an MRS")

(defparameter *dummy-relations* nil
   "this allows the rels list in the FS to contain relations that
    have no effect on the extracted MRS.  The values set here should
    correspond to the value of the pred - so they could be the
    type of the ep fs or the value of *rel-name-path*")

;;;
;;; finally, we are eliminating the symbol vs. string contrast on predicates;
;;; whereas a grammar might use either one (or both, even for the abstractly
;;; same predicate, e.g. _afterwards_p and "_afterwards_p" in the 1214 ERG),
;;; these shall be treated as equivalent in the MRS universe.  also, we will
;;; now always strip the (optional) *sem-relation-suffix* (‘_rel’ in the ERG)
;;; from predicate names, as this is a mechanism on the TFS side only (to give
;;; the grammar something like a separate namespace for its predicates).  in
;;; the past, some MRS serializations suppressed the symbol vs. type contrast,
;;; some exposed it; likewise, some stripped the suffix, and others kept it.
;;; see the discussion on the ‘developers’ list from january 2016 for details.
;;;
;;; to make the transition into a better future less painful, we will preserve
;;; the traditional behavior (still on by default) for at least a transition
;;; period.  however, ERG 1214 will ship with predicate normalization enabled.
;;;
(defparameter *normalize-predicates-p* nil)

;;; types for variable naming in mrsoutput

(defparameter *event-type* (vsym "event"))
(defparameter *event_or_index-type* (vsym "event_or_index"))
(defparameter *non_expl-ind-type* (vsym "non_expl-ind"))
(defparameter *handle-type* (vsym "handle"))
(defparameter *ref-ind-type* (vsym "ref-ind"))
(defparameter *deg-ind-type* (vsym "deg-ind"))
(defparameter *non_event-type* (vsym "non_event"))
;;;
;;; _fix_me_
;;; the above should soon be deprecated, due to the VPM extension. 
;;;                                                            (29-jul-08; oe)
;;;
;;; determine which VPM to use in mapping MRS variable types; none by default,
;;; for backwards compatibility (in an interim transition perioid, i hope).
;;;
(defparameter *variable-type-mapping* nil)

;;; used in mrsresolve (code for scoping MRSs)

(defparameter *bv-feature* (vsym "ARG0")
  "the feature used to indicate a quantifier's bound variable")

(defparameter *scope-feat* (vsym "BODY")
  "the feature used to indicate a quantifier's scope - required
   by the scoping code and also, by default, used in the test for
   quantifierness of EPs")

(defparameter *quant-rel-types* nil
  "the scoping code needs to know which relations correspond
   to quantifiers - the default way of doing this is to assume
   that quantifiers uniquely have a body feature but alternatively
   this can be set to an exhaustive list of quantifiers - useful
   for teaching purposes")

(defparameter *scoping-ignored-roles* (list (vsym "TPC") (vsym "PSV"))
  "Variables which are the values of these features are ignored
   with respect to the scoping code.  This is curently used for the
   simple information structure encoding.")

(defvar *top-level-rel-types* nil
  "the types of any relations which introduce variables
which need not be scoped - pronouns etc.  Not used in current ERG")

;;;
;;; MRS output control

(defparameter *sem-relation-suffix* "_rel"
  "a suffix string marking semantic relations - 
   used in the compact MRS representation")

;;; for generation

(defparameter *null-semantics-hack-p* nil
  "for debugging - if set, this cheats on null semantic items
   WARNING - do not set when processing in batch mode")

(defparameter *main-semantics-path* 
    (append *initial-semantics-path*  *psoa-liszt-path*)
  "the path into a lexical entry which gives the list of
   relations - typically (append *initial-semantics-path* 
   *psoa-liszt-path*)")

(defparameter *c-cont-path* 
    `(,(vsym "C-CONT")))

(defparameter *construction-semantics-path* 
  (append *c-cont-path* *psoa-liszt-path*)
  "the path into a rule/construction which gives the
   semantics specific to that construction")

(defparameter *top-semantics-type* (vsym "predsort")
  "the highest type in the hierarchy under which all
   rels are found")

(defparameter *top-semantics-entry* nil
  "set in lexutils - not user-settable")

;;; INSTLOC is a feature that should appear on all feature structures
;;; representing variables (handles etc)  It is needed by the generator
;;; as a location for a constant value that distinguishes different
;;; variables.  It is now assumed to be string-valued.

(defparameter *instloc-path* `(,(vsym "INSTLOC")))

(defparameter *slash-semantics-path* 
    `(,(vsym "SYNSEM") ,(vsym "NON-LOCAL") 
      ,(vsym "SLASH") ,(vsym "LIST") ,(vsym "FIRST") 
      ,(vsym "CONT"))
  "This path is used for filtering the edges in the generator to improve efficiency")

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

(defvar *top-level-variables* nil
  "the variables which correspond to pronouns -
set in the code")

;;; to control scoping or cheap scope

(defvar *fragment-p* nil
  "if t for a parse, cheapscope is not called - not user settable")

;;; the following are needed only for the detection of fragments
;;; indicated in the LinGO grammar by the value of ROOT

(defvar *root-path* `(,(vsym "ROOT")))

(defvar *false-type* (vsym "-"))
(defvar *true-type* (vsym "+"))

(defparameter *alex-mode* nil
  "if t, allows scope to have specified relations")


;;; Control of functionality

(defparameter *giving-demo-p* nil
  "when set, this squashes various error and warning messages in the MRS code.
   This is not a good thing to do in general!")

(defparameter *mrs-results-check* nil)
;; mrscorpus and mrsfns - causes results to be checked against
;; previous stored results.  Currently non-operational.

;;; for some reason, the function determine-variable-type 
;;; was moved here, where it surely doesn't belong (given that it's
;;; a function ...)
;;; Moved (back?) to mrsoutput.lisp, since it only relates to
;;; grammar-to-MRS construction.  Note that the types available
;;; for variables are now something that's part of the definition of
;;; MRS/RMRS, so not user-controllable any more.

;;; MRS <-> RMRS conversion - was in convert.lisp but should be per-grammar

(defparameter *var-extra-conversion-table* 
    '(
      ((png.gen fem) . (gender f))
      ((png.gen masc) . (gender m))
      ((png.gen andro) . (gender m-or-f))
      ((png.gen neut) . (gender n))
      
      ((png.pn 1sg) . (AND (pers 1) (num sg)))
      ((png.pn 2sg) . (AND (pers 1) (num sg)))
      ((png.pn 3sg) . (AND (pers 3) (num sg)))
      ((png.pn non1sg) . (AND (pers 2-or-3) (num sg)))
      
      ((png.pn 1pl) . (AND (pers 1) (num pl)))
      ((png.pn 2pl) . (AND (pers 1) (num pl)))
      ((png.pn 3pl) . (AND (pers 3) (num pl)))
      
      ((png.pn 1per) .  (pers 1))
      ((png.pn 2per) .  (pers 2))
      ((png.pn 3per) .  (pers 3))
      
      ((e.tense basic_tense) . (tense u))
      ((e.tense no_tense) . (tense u))
      ((e.tense nontense) . (tense u))
      ((e.tense future) . (tense future))
      ((e.tense present) . (tense present))
      ((e.tense past) . (tense past))
 ;;;  ((e.tense nonpresent) . (tense non-present))
      ((e.tense nonpresent) . (tense u))
   ;;; my version of the DTD doesn't have `non-present'
   ;;; replace this with line above if using a DTD that does
      ((e.tense nonpast) . (tense non-past))
      
  ;;; note the interpretation is intended to be that the 
  ;;; first match is taken.  For RMRS->MRS conversion, there's
  ;;; a sl problem in that nontense and no_tense are 
  ;;; both possible values corresponding to (tense u)
  ;;; and that this also corresponds to the `don't know'
  ;;; case.  We therefore need to translate the RMRS `u'
  ;;; into `basic_tense'
      
      )
  "used to define the conversion of extra values between MRS and RMRS
   - specified by the grammar")


;;; for algebra

(defparameter *algebra-ignore-feats* `(,(vsym "ARG-S")))

(defparameter *algebra-ignore-paths* 
    `(
      (,(vsym "C-CONT"))
      (,(vsym "SYNSEM") ,(vsym "LOCAL") ,(vsym "CONT"))
      (,(vsym "NH-DTR"))
      (,(vsym "HD-DTR"))
      (,(vsym "DTR"))))


(defparameter *non-slot-features* 
    `(,(vsym "CONT") ,(vsym "HOOK") ,(vsym "SYNSEM")
      ,(vsym "LOCAL") ,(vsym "CAT") ,(vsym "VAL") ,(vsym "HEAD")))

(defvar *allow-sloppy-message-matching-p* nil
  "temporary expedient (I hope) to allow for the current ERG
obtaining message predicates in complicated ways - should be nil for 
grammars that don't do this")


;;; generator spelling fixing - nil by default
;;; but can be set to 'lkb::fix-spelling for a/an - see spell.lisp
;;; this is no longer used in the ERG

(defparameter *fix-spelling-fn* nil)
