;;; Copyright (c) 1991-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions
;;;
;;; This is the base globals file because parameters have to be
;;; defined somewhere, but in most cases a particular set of
;;; grammar files will have their own associated parameters
;;;
;;; All functions have now been moved to user-fns.lsp

(in-package :lkb)

;;; Keep track of all parameters that get defined, so we can have a way to
;;; edit them interactively

(defvar *lkb-params* nil)

;;; distinguish between user parameters - which can be edited interactively
;;; - and other parameters, which normally shouldn't be

(defvar *lkb-user-params* nil)

(defvar *user-params-file* nil
  "file where values of user params are saved")

(defmacro def-lkb-parameter (var val &optional doc type)
  `(progn
     (if (eql ,type :user) 
         (pushnew (quote ,var) *lkb-user-params*)
         (pushnew (quote ,var) *lkb-params*))
     (defparameter ,var ,val ,doc)))

;;; Strings

(def-lkb-parameter *toptype* 'top)

(def-lkb-parameter *string-type* 'string
   "a special type name - any lisp strings are subtypes of it")

;;; Lexical files

(def-lkb-parameter *orth-path* '(orth lst))

(def-lkb-parameter *alt-orth-path* nil
  "an alternative location for the orthography --- needed for Aline's grammar")

(def-lkb-parameter *list-tail* '(tl))

(def-lkb-parameter *list-head* '(hd))

(def-lkb-parameter *empty-list-type* '*null*)

(def-lkb-parameter *list-type* '*list*)

(def-lkb-parameter *diff-list-type* '*diff-list*)

(def-lkb-parameter *diff-list-list* 'list)

(def-lkb-parameter *diff-list-last* 'last)

(def-lkb-parameter *key-daughter-path* '(key-arg))

(def-lkb-parameter *key-daughter-type* '+)

(def-lkb-parameter *lex-rule-suffix* nil
  "creates the inflectional rule name from the information
   in irregs.tab - for PAGE compatability")

(def-lkb-parameter *sense-unif-fn* nil)

(def-lkb-parameter *gc-before-reload* nil)

;;; Parsing

(defconstant *maximum-number-of-tasks* 50000
  "limits the number of pending tasks on the agenda")

(def-lkb-parameter *maximum-number-of-edges* 500 
  "limits the size of the parse chart" :user)

(declaim (type fixnum *maximum-number-of-edges*))

(def-lkb-parameter *chart-limit* 100)

(def-lkb-parameter *bracketing-p* nil
  "If set, the input may contain a partial bracketing which
   will be respected (currently not with the active parser)"
  :user)

(def-lkb-parameter *mother-feature* 0
   "The feature giving the mother in a grammar rule")

(def-lkb-parameter *start-symbol* 'sign
  "a type which specifies the type of any valid parse" 
  :user)

(def-lkb-parameter *feature-ordering* nil
  "partial order of features for fixing or resolving default ordering")

(def-lkb-parameter *maximal-lex-rule-applications* 7
   "The number of lexical rule applications which may be made
   before it is assumed that some rules are applying circularly")

(def-lkb-parameter *deleted-daughter-features* nil
  "These features will not be passed from daughter to mother
   when parsing")

(def-lkb-parameter *check-paths* nil
   "an alist in which the keys are feature paths that often fail -
    these are checked first before attempting unification")

(def-lkb-parameter *check-path-count* 30
  "the number of check paths actually used")

(def-lkb-parameter *substantive-roots-p* nil
  "if this is set, root edges are regarded as real edges
   for the purposes of chart display")

(def-lkb-parameter *irregular-forms-only-p* nil
  "if this is set, the parser will not parse regular spellings
   if there is an irregular spelling (eated, dreamed)")

(def-lkb-parameter *unknown-word-types* nil
  "if this is set, the user-fn make-unknown-word-sense-unifications
   should also be defined.  Together they result in entries
   of the specified type or types being created for any unknown
   words which may occur in the input")

#+:packing
(def-lkb-parameter *packing-restrictor* nil
  "restrictor used when parsing with ambiguity packing")

(def-lkb-parameter *rule-keys* nil
  "assoc() list mapping rule identifier to position of key daughter")
    
;;; Display 

(def-lkb-parameter *display-type-hierarchy-on-load* t
  "controls whether the type hierarchy appears automatically"
  :user)

(def-lkb-parameter *show-morphology* t
  "if set, the morphological structures are shown in parse trees"
  :user)

(def-lkb-parameter *show-lex-rules* t
  "if set, applications of lexical rules are shown in parse trees"
  :user)

(def-lkb-parameter *parse-tree-font-size* 12
  "size of font in parse trees"
  :user)

(def-lkb-parameter *fs-type-font-size* 12
  "size of font in AVMs"
  :user)

(def-lkb-parameter *fs-title-font-size* 12
  "size of font of AVM window titles"
  :user)

(def-lkb-parameter *type-tree-font-size* 12
  "size of font in type hierarchy display"
  :user)

(def-lkb-parameter *dialog-font-size* 12
  "size of font in dialogs"
  :user)

(def-lkb-parameter *maximum-list-pane-items* 50
  "maximum number of items in a list pane")

;;; YADU

(def-lkb-parameter *description-persistence* 'l
   "Atom marking tails which should be made non-default when a description is exapanded")

;;; Parse tree node labels

(def-lkb-parameter *simple-tree-display* nil
  "turn off PAGE style labels")

;;; these are actually only used when in PAGE compatability mode

;;; the path where the name string is stored
(def-lkb-parameter *label-path* '(LABEL-NAME))

;;; the path for the meta prefix symbol
(def-lkb-parameter *prefix-path* '(META-PREFIX))

;;; the path for the meta suffix symbol
(def-lkb-parameter *suffix-path* '(META-SUFFIX))

;;; the path where the args are stored
(def-lkb-parameter *args-path* '(ARGS))

;;; the path for the recursive category
(def-lkb-parameter *recursive-path* '(NON-LOCAL SLASH LIST FIRST))

;;; the path inside the node to be unified with the recursive node
(def-lkb-parameter *local-path* '(LOCAL))

;;; the path inside the node to be unified with the label node
(def-lkb-parameter *label-fs-path* '(SYNSEM))

(def-lkb-parameter *label-template-type* 'label)

;;;
;;; size of static pool used to cache safe dag instances (see `dag.lsp').
;;;
;;; if each dag instance has approximately 50 bytes, then we should be able to
;;; affort some (static) 2.5 mbytes for pool storage.  once make-dag() runs out
;;; of pool instances, new structures will be allocated dynamically (and become
;;; garbage eventually).  hence, the initial pool size can be a crucial choice.
;;;
;;; though it seems tempting to allocate new pools as required, this would mean
;;; that one huge sentence could grow the image size permanently.  for the time
;;; being we prefer to assume that the user choice of pool size is appropriate
;;; for what is required on average.
;;; 
(def-lkb-parameter *dag-pool-size* 50000)

;;; controls TDL or path syntax switch (not really needed)

(def-lkb-parameter *lkb-system-version* :page)

;;; Agenda system

(def-lkb-parameter *first-only-p* nil
  "if set, only the first parse is produced"
  :user)

(def-lkb-parameter *gen-first-only-p* nil
  "if set, only the first realization is produced"
  :user)

;;; for the compare function 

(def-lkb-parameter *discriminant-path* '(synsem local cont key))

(defparameter *current-grammar-load-file* nil
  "not user settable - has to be here because it's
   used in lisp specific stuff as well as elsewhere")

(defvar *lexicon* nil)
(defvar *leaf-types* nil)

(defvar *current-language* nil)

;;; MRS interaction control

(defvar *mrs-loaded* nil)

;;; generator globals (should be moved eventually)

(defparameter *semantics-index-path* '(synsem local cont index)
  "path used by generator to index chart")

(defparameter *alt-semantics-index-path* nil
  "alternative path used by generator to index chart --- needed
   for Aline's grammar")

(defparameter *intersective-rule-names* '(adjh_i nadj_i hadj_i_uns)
  "names of rules that introduce intersective modifiers")

(defparameter *duplicate-lex-ids* '(AN)
  "temporary expedient to avoid generating dual forms")

