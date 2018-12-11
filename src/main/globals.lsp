;;; Copyright (c) 1991--2018
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Benjamin Waldron;
;;;   see LICENSE for conditions.
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

(def-lkb-parameter *list-type* '*list*)

(def-lkb-parameter *empty-list-type* '*null*)

(def-lkb-parameter *non-empty-list-type* '*cons*)

(def-lkb-parameter *diff-list-type* '*diff-list*)

(def-lkb-parameter *diff-list-list* 'list)

(def-lkb-parameter *diff-list-last* 'last)

(def-lkb-parameter *head-daughter-path* '(head-dtr))

(def-lkb-parameter *head-path* '(synsem local cat head))

(def-lkb-parameter *key-daughter-path* '(key-arg))

(def-lkb-parameter *key-daughter-type* '+)

;;;
;;; as we move into the chart mapping universe, lexical entries behave similar
;;; to rules: the list of input tokens that license a lexical entry are unified
;;; into *lexicon-tokens-path* (when set).  furthermore, to give the grammarian
;;; easier access to the token in the right periphery, the last element of the 
;;; tokens list is made re-entrant with *lexicon-last-token-path*.
;;;
(def-lkb-parameter *lexicon-tokens-path* nil)
(def-lkb-parameter *lexicon-last-token-path* nil)

;;;
;;; token feature structures are grammar-specific bundles of properties that
;;; were input to parsing; to look up individual components, assume a number
;;; of (customizable) paths.
;;;
(def-lkb-parameter *token-id-path* nil)

(def-lkb-parameter *lex-rule-suffix* nil
  "creates the inflectional rule name from the information
   in irregs.tab - for PAGE compatability")

(def-lkb-parameter *sense-unif-fn* nil)

(def-lkb-parameter *gc-before-reload* nil)

;;; Morphology

(def-lkb-parameter *morph-option* :default
    "possible values are :default, :distinct-mphon, :external-rule-by-rule 
   :external-partial-tree, :with-tokeniser-partial-tree,  
   :with-tokeniser-retokenise")

(def-lkb-parameter *foreign-morph-fn* nil
  "alternative to built in LKB morphology")

;;; Revised morphology

;;; affix type

(def-lkb-parameter *affix-type* 'affix)

(def-lkb-parameter *rule-affix-feature* 'affixation)

(def-lkb-parameter *affix-pattern-feature* 'pattern)

(def-lkb-parameter *affix-class-feature* 'affix-class)

(def-lkb-parameter *letter-set-feature* 'letters)

(def-lkb-parameter *letter-set-char-feature* 'char)

(def-lkb-parameter *letter-set-match-feature* 'match)

(defparameter *known-cyclic-rules* nil
  "to allow disabling of warning messages about possible cycles -
 advanced users only")

;;; Parsing

(def-lkb-parameter *maximum-number-of-tasks* 200000
  "limits the number of pending tasks on the agenda")

(def-lkb-parameter *maximum-number-of-edges* 500 
  "limits the size of the parse chart" :user)

(declaim (type fixnum *maximum-number-of-edges*))

(def-lkb-parameter *chart-limit* 2000)

(def-lkb-parameter *bracketing-p* nil
  "If set, the input may contain a partial bracketing which
   will be respected (currently not with the active parser)"
  :user)

(def-lkb-parameter *mother-feature* 0
   "The feature giving the mother in a grammar rule")

(def-lkb-parameter *start-symbol* 'sign
  "a type which specifies the type of any valid parse" 
  :user)

(def-lkb-parameter *gen-start-symbol* 'sign
  "a type which specifies the type of any valid generator derivation" 
  :user)

(def-lkb-parameter *fragment-start-symbols* nil
  "a list of types (or instances) which specify the type of a valid derivation
   in fragment parsing or generation mode" 
  :user)

(def-lkb-parameter *non-idiom-root* nil
  "an fs id which specifies a structure which prevents idioms")

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
   if there is an irregular spelling (eated, dreamed)
   Consider replacing with *most-specific-only-p*")

(def-lkb-parameter *most-specific-only-p* nil
  "if this is set, the parser will not parse regular spellings
   if there is an irregular spelling (eated, dreamed) and will only
   accept the most specific pattern in a subrule.")


(def-lkb-parameter *unknown-word-types* nil
  "if this is set, the user-fn make-unknown-word-sense-unifications
   should also be defined.  Together they result in entries
   of the specified type or types being created for any unknown
   words which may occur in the input")

(def-lkb-parameter *packing-restrictor* nil
  "restrictor used when parsing with ambiguity packing")

(def-lkb-parameter *rule-keys* nil
  "assoc() list mapping rule identifier to position of key daughter")
    
;;; Display 

(def-lkb-parameter *comparison-dependencies-font-size* 12
  "size of font in tree comparison (elementary dependencies in tree pane)"
  :user)

(def-lkb-parameter *comparison-discriminant-font-size* 8
  "size of font in tree comparison (discriminant pane)"
  :user)

(def-lkb-parameter *comparison-tree-font-size* 8
  "size of font in tree comparison (tree pane)"
  :user)

(def-lkb-parameter *dialog-font-size* 12
  "size of font in dialogs"
  :user)

(def-lkb-parameter *display-type-hierarchy-on-load* t
  "controls whether the type hierarchy appears automatically"
  :user)

(def-lkb-parameter *fs-type-font-size* 12
  "size of font in AVMs"
  :user)

(def-lkb-parameter *fs-title-font-size* 12
  "no longer used: AVM window titles now use *fs-type-font-size*")

(def-lkb-parameter *list-item-font-size* 12
  "size of font in list windows, such as generator output"
  :user)

(def-lkb-parameter *parse-tree-font-size* 12
  "size of font in parse trees"
  :user)

(def-lkb-parameter *show-lex-rules* t
  "if set, applications of lexical rules are shown in parse trees"
  :user)

(def-lkb-parameter *show-morphology* t
  "if set, the morphological structures are shown in parse trees"
  :user)

(def-lkb-parameter *summary-tree-font-size* 8
  "size of font in parse tree summary")

(def-lkb-parameter *type-tree-font-size* 12
  "size of font in type hierarchy display"
  :user)

(def-lkb-parameter *maximum-list-pane-items* 150
  "maximum number of items in a list pane")

(def-lkb-parameter *lex-rule-show-one-step* t
  "if set, lexical rule application will be shown one step at a time")

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

(def-lkb-parameter *rich-tree-labels-p* nil)

;;;
;;; size of static pool used to cache safe dag instances (see `dag.lsp').
;;;
;;; if each dag instance has approximately 80 bytes, then we should be able to
;;; affort some (static) 4 MB for pool storage (assuming a pool of size
;;; 50,000).  once make-dag() runs out of pool instances, new structures will
;;; be allocated dynamically (and become garbage eventually).  hence, the
;;; initial pool size can be a crucial choice.
;;;
;;; though it seems tempting to allocate new pools as required, this would mean
;;; that one huge sentence could grow the image size permanently.  for the time
;;; being we prefer to assume that the user choice of pool size is appropriate
;;; for what is required on average.
;;; 
;;; Grammars such as the ERG could increase this parameter and then recreate
;;; the pool -- JAC 28-May-18

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

(defparameter *intersective-rule-names* nil
  "names of rules that introduce intersective modifiers")

(defparameter *duplicate-lex-ids* nil
  "temporary expedient to avoid generating dual forms")

;;;
;;; temporary expedient: allow the ERG to not use select rules (currently, ones
;;; putting on punctuation recursively) in generation.    (20-may-05; dpf & oe)
;;; temporarily, provide a similar mechanism for parsing.        (2-nov-12; oe)
;;
(defparameter *gen-ignore-rules* nil)
(defparameter *parse-ignore-rules* nil)

;;; RMRS interactive globals

(def-lkb-parameter *pos-sensitive-rmrs-p* nil
  "Controls whether the interactive RMRS comparison is sensitive to character position")

(def-lkb-parameter *lexdb-params* nil
  "parameter settings for lexical database"
  :user)

(defparameter *recording-word* nil
  "if used this is set to a feature which should occur in the MRS relation with value *toptype*")

(def-lkb-parameter *show-spelling-rules* t
  "if set, spelling rule (if it exists) is shown when displaying a lexical rule"
  :user)

;;; 'ORIGSTR

(def-lkb-parameter *characterize-p* t
  "if set CFROM/CTO character pointers are enabled")
