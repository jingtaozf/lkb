(in-package :lkb)

;;; globals file for Matt's grammar
;;; settings are generaly similar to main LKB globals file
;;; but are repeated to allow for the case where this is read in after 
;;; another grammar

(def-lkb-parameter *deleted-daughter-features* '(|1| |2|)
  "These features will not be passed from daughter to mother
   when parsing")

(defparameter *templates* nil 
   "types which are treated as templates to avoid excessive glbs")

;;; Strings

(defparameter *toptype* 'top)

(defparameter *string-type* 'string
   "a special type name - any lisp strings are subtypes of it")

;;; Lexical files

(defparameter *orth-path* '(orth lst))

(defparameter *list-tail* '(tl))

(defparameter *list-head* '(hd))

(defparameter *empty-list-type* 'e-list)

(defparameter *lex-rule-suffix* ""
;;; create the lexical rule name from the info in irregs.tab
;;; for TDL - value should be an upcase string - NIL otherwise
)

(defparameter *sense-unif-fn* nil)
    ;;; #'make-sense-unifications)

;;; Parsing

(defparameter *chart-limit* 100)

(defparameter *mother-feature* nil
   "The feature giving the mother in a grammar rule")

;;; eu tirei para teste
;;;(defparameter *start-symbol* '(root)
;;;   "a type which specifies the type of any valid parse")

(defparameter *start-symbol* '(root)
  "a type which specifies the type of any valid parse")


;;; Parse tree node labels

;;; the path where the name string is stored
(defparameter *label-path* '(LABEL-NAME))

;;; the path for the meta prefix symbol
(defparameter *prefix-path* '(META-PREFIX))

;;; the path for the meta suffix symbol
(defparameter *suffix-path* '(META-SUFFIX))

;;; the path for the recursive category
(defparameter *recursive-path* '(NODE CAT ))

;;; the path inside the node to be unified with the recursive node
(defparameter *local-path* nil)

;;; the path inside the node to be unified with the label node
(defparameter *label-fs-path* nil)

(defparameter *label-template-type* 'label)

(defparameter *maximal-lex-rule-applications* 7
   "The number of lexical rule applications which may be made
   before it is assumed that some rules are applying circularly")

(defparameter *possible-languages* nil
"Specifies the possible languages for interactions
   where a language has to be selected or specified -
 nil if irrelevant")

(defparameter *settings-options* nil
  "controls whether user is asked for type display options file")

(defparameter *feature-ordering*
 '(arg-str orth syn sem))

(defparameter *gc-before-reload* t)

(defparameter *display-type-hierarchy-on-load* nil)

(def-lkb-parameter *maximum-number-of-edges* 2000 
  "limits the size of the parse chart" :user)



