;;; Copyright Ann Copestake 1991-1997 All Rights Reserved.
;;; No use or redistribution without permission.
;;;
;;; This is the base globals file because parameters have to be
;;; defined somewhere, but in most cases a particular set of
;;; grammar files will have their own associated parameters

(defparameter *display-glb-messages* nil)

;;; Avoiding too many glbs - treat some types as
;;; templates

(defparameter *templates* nil)

;;; Strings

(defparameter *toptype* 'top)

(defparameter *string-type* 'string
   "a special type name - any lisp strings are subtypes of it")

;;; Lexical files


(defparameter *orth-path* '(orth lst))

(defparameter *list-tail* '(tl))

(defparameter *list-head* '(hd))

(defparameter *lex-rule-suffix* nil)

(defparameter *sense-unif-fn* nil)
  
;;; Parsing

(defparameter *preprocess-sentence-fn* nil)

(defparameter *chart-limit* 100)

(defparameter *sign-type* 'sign
   "a special type wrt parsing - rule indexing is checked for its
   descendants")

(defparameter *mother-feature* 0
   "The feature giving the mother in a grammar rule")

(defparameter *head-marking-path* nil
   "a feature path - a head daughter in a rule may be identified by
    having the same value for this path as the mother")

;;; value is 'node for YADU

  
(defparameter *start-symbol* 'sign
   "a type which specifies the type of any valid parse")

(defparameter *morph-rule-type* 'morph-rule
   "lexical rules of this type will not be applied
   by the parser because it currently has no morphological
   component")

(defparameter *maximal-lex-rule-applications* 7
   "The number of lexical rule applications which may be made
   before it is assumed that some rules are applying circularly")

(defparameter *deleted-daughter-features* nil
  "These features will not be passed from daughter to mother
   when parsing")

(defparameter *bc96lrules* nil)

(defparameter *check-paths* nil
   "an alist in which the keys are feature paths that often fail -
    these are checked first before attempting unification")


;;; Display 

(defparameter *settings-options* nil
  "controls whether user is asked for type display options file")

(defparameter *feature-abbreviations* 
   '(("-first" . "H")
     ("-last" . "T"))
   "a list of pairs of strings - if the end of a feature name 
   matches the first string it is displayed as the 
   second string in windows.  Used to make lists more readable") 
   
(defparameter *dont-show-morphology* nil
  "if set, the morphological structures are not shown in parse trees")

(defparameter *parse-tree-font-size* 9)

(defparameter *fs-type-font-size* 9)

(defparameter *fs-title-font-size* 9)

(defparameter *type-tree-font-size* 9)

(defparameter *dialog-font-size* 12)

   
;;; Indexing


(defparameter *batch-mode* nil
   "set when indexing, could also be set by the user 
   to prevent errors in expanding a lexical entry being
   signalled as continuable errors, rather than written
   to a file.")


;;; Languages
;;; (not very useful in core LKB, maybe remove from here?)

(defparameter *current-language* 'English
   "Specifies the default language for the
   various interactions where a language has to
   be selected")

(defparameter *possible-languages* '(Spanish Italian Dutch English French)
   "Specifies the possible languages for interactions
   where a language has to be selected or specified")
   

;;; Warnings etc

(defparameter *warn-of-unary-branches* nil
   "If set warns of unary branches when type system is loaded")

;;; YADU

(defparameter *lexical-persistence* 'lex
   "Atom marking lexical persistence of tails")

(defparameter *rule-persistence* nil
   "Atom marking persistence of tails in rules")


;;; Parse tree node labels

;;; the path where the name string is stored
(defparameter *label-path* '(LABEL-NAME))

;;; the path for the meta prefix symbol
(defparameter *prefix-path* '(META-PREFIX))

;;; the path for the meta suffix symbol
(defparameter *suffix-path* '(META-SUFFIX))

;;; the path for the recursive category
(defparameter *recursive-path* '(NON-LOCAL SLASH LIST FIRST))

;;; the path inside the node to be unified with the recursive node
(defparameter *local-path* '(LOCAL))

;;; the path inside the node to be unified with the label node
(defparameter *label-fs-path* '(SYNSEM))

(defparameter *label-template-type* 'label)

