;;; globals file for Matt's grammar
;;; settings are generaly similar to main LKB globals file
;;; but are repeated to allow for the case where this is read in after 
;;; another grammar

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

(defparameter *lex-rule-suffix* nil)
;;; create the lexical rule name from the info in irregs.tab
;;; for TDL - value should be an upcase string - NIL otherwise

(defparameter *sense-unif-fn* nil)

;;; Parsing

(defparameter *chart-limit* 100)

(defparameter *sign-type* 'category
   "a special type wrt parsing - rule indexing is checked for its
   descendants")
 
(defparameter *mother-feature* 0
   "The feature giving the mother in a grammar rule")

(defparameter *start-symbol* 'root-cat
   "a type which specifies the type of any valid parse")

(defparameter *morph-rule-type* 'lrule
   "rules of this type may have associated orthographic effects
    (but don't have to)")

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