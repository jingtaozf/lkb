;;; Copyright Ann Copestake 1991-1997 All Rights Reserved.
;;; No use or redistribution without permission.
;;;
;;; This is the base globals file because parameters have to be
;;; defined somewhere, but in most cases a particular set of
;;; grammar files will have their own associated parameters
;;;
;;; All functions have now been moved to user-fns.lsp

(in-package :cl-user)

;;; Keep track of all parameters that get defined, so we can have a way to
;;; edit them interactively

(defvar *lkb-params* nil)

(defmacro def-lkb-parameter (var val &optional doc)
  `(progn
     (pushnew (quote ,var) *lkb-params*)
     (defparameter ,var ,val ,doc)))


(def-lkb-parameter *templates* nil 
   "types which are treated as templates to avoid excessive glbs")

;;; Strings

(def-lkb-parameter *toptype* 'top)

(def-lkb-parameter *string-type* 'string
   "a special type name - any lisp strings are subtypes of it")

;;; Lexical files

(def-lkb-parameter *orth-path* '(orth lst))

(def-lkb-parameter *list-tail* '(tl))

(def-lkb-parameter *list-head* '(hd))

(def-lkb-parameter *empty-list-type* '*null*)

(def-lkb-parameter *diff-list-type* '*diff-list*)

(def-lkb-parameter *diff-list-list* 'list)

(def-lkb-parameter *diff-list-last* 'last)

(def-lkb-parameter *lex-rule-suffix* nil
  "creates the inflectional rule name from the information
   in irregs.tab - for PAGE compatability")

(def-lkb-parameter *sense-unif-fn* nil)
  
;;; Parsing

(def-lkb-parameter *maximum-number-of-edges* 3000)

(def-lkb-parameter *chart-limit* 100)

(def-lkb-parameter *mother-feature* 0
   "The feature giving the mother in a grammar rule")

(def-lkb-parameter *head-marking-path* nil
   "a feature path - a head daughter in a rule may be identified by
    having the same value for this path as the mother")

(def-lkb-parameter *start-symbol* 'sign
  "a type which specifies the type of any valid parse")

(def-lkb-parameter *feature-ordering* nil
  "partial order of features for fixing or resolving default ordering")

(def-lkb-parameter *maximal-lex-rule-applications* 7
   "The number of lexical rule applications which may be made
   before it is assumed that some rules are applying circularly")

(def-lkb-parameter *deleted-daughter-features* nil
  "These features will not be passed from daughter to mother
   when parsing")

(def-lkb-parameter *bc96lrules* nil)


(def-lkb-parameter *check-paths* nil
   "an alist in which the keys are feature paths that often fail -
    these are checked first before attempting unification")

(def-lkb-parameter *substantive-roots-p* nil
  "if this is set, root edges are regarded as real edges
   for the purposes of chart display")


;;; Display 

(def-lkb-parameter *display-type-hierarchy-on-load* t)

(def-lkb-parameter *display-glb-messages* nil
   "if set, informs user of glbtypes as they are created")

(def-lkb-parameter *settings-options* nil
  "controls whether user is asked for type display options file")

(def-lkb-parameter *feature-abbreviations* 
   '(("-first" . "H")
     ("-last" . "T"))
   "a list of pairs of strings - if the end of a feature name 
   matches the first string it is displayed as the 
   second string in windows.  Used to make lists more readable") 
   
(def-lkb-parameter *dont-show-morphology* nil
  "if set, the morphological structures are not shown in parse trees")

(def-lkb-parameter *dont-show-lex-rules* nil
  "if set, applications of lexical rules are not shown in parse trees")

(def-lkb-parameter *parse-tree-font-size* 12)

(def-lkb-parameter *fs-type-font-size* 12)

(def-lkb-parameter *fs-title-font-size* 12)

(def-lkb-parameter *type-tree-font-size* 12)

(def-lkb-parameter *dialog-font-size* 12)

   
;;; Indexing


(def-lkb-parameter *batch-mode* nil
   "set when indexing, could also be set by the user 
   to prevent errors in expanding a lexical entry being
   signalled as continuable errors, rather than written
   to a file.")


;;; Languages
;;; (not very useful in core LKB, maybe remove from here?)

(def-lkb-parameter *current-language* 'English
   "Specifies the default language for the
   various interactions where a language has to
   be selected")

(def-lkb-parameter *possible-languages* nil
   "Specifies the possible languages for interactions
   where a language has to be selected or specified")
   

;;; Warnings etc

(def-lkb-parameter *warn-of-unary-branches* nil
   "If set warns of unary branches when type system is loaded")

;;; YADU

(def-lkb-parameter *lexical-persistence* 'lex
   "Atom marking lexical persistence of tails")

(def-lkb-parameter *rule-persistence* nil
   "Atom marking persistence of tails in rules")


;;; Parse tree node labels

;;; these are actually only used when in PAGE compatability mode

;;; the path where the name string is stored
(def-lkb-parameter *label-path* '(LABEL-NAME))

;;; the path for the meta prefix symbol
(def-lkb-parameter *prefix-path* '(META-PREFIX))

;;; the path for the meta suffix symbol
(def-lkb-parameter *suffix-path* '(META-SUFFIX))

;;; the path for the recursive category
(def-lkb-parameter *recursive-path* '(NON-LOCAL SLASH LIST FIRST))

;;; the path inside the node to be unified with the recursive node
(def-lkb-parameter *local-path* '(LOCAL))

;;; the path inside the node to be unified with the label node
(def-lkb-parameter *label-fs-path* '(SYNSEM))

(def-lkb-parameter *label-template-type* 'label)

