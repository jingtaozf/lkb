;;; Copyright Ann Copestake 1991-1998 All Rights Reserved.
;;; No use or redistribution without permission.
;;;

;;; Strings

(defparameter *toptype* '*top*)

(defparameter *string-type* 'string
   "a special type name - any lisp strings are subtypes of it")

;;; Lexical files

(defparameter *orth-path* '(orth list))

(defparameter *list-tail* '(rest))

(defparameter *list-head* '(first))

(defparameter *lex-rule-suffix* "_INFL_RULE"
 "creates the inflectional rule name from the information
   in irregs.tab - for PAGE compatability")

;;; Parsing

(defparameter *chart-limit* 100)

(defparameter *sign-type* 'synsem-struc
   "a special type wrt parsing - rule indexing is checked for its
   descendants")

(defparameter *mother-feature* NIL
   "The feature giving the mother in a grammar rule")


;;; ?
(defparameter *start-symbol* '(root frag frag-msg fin_frag)
   "specifing valid parses")

;;; ?
(defparameter *morph-rule-type* 'lrule-super
    "morphology system checks for rules which are
     of this type or a subtype of it")

(defparameter *maximal-lex-rule-applications* 7
   "The number of lexical rule applications which may be made
   before it is assumed that some rules are applying circularly")

#|

(defparameter *deleted-daughter-features* '(ARGS HEAD-DTR NON-HEAD-DTR LCONJ-DTR RCONJ-DTR)
   "features pointing to daughters deleted on building a constituent")

(defparameter *head-marking-path* '(SYNSEM LOCAL CONT KEY)
   "coreferenced between mother and head daughter")

|#

;;; Parse tree node labels

;;; the path where the name string is stored
(defparameter *label-path* '(LABEL-NAME))

;;; the path for the meta prefix symbol
(defparameter *prefix-path* '(META-PREFIX))

;;; the path for the meta suffix symbol
(defparameter *suffix-path* '(META-SUFFIX))

;;; the path for the recursive category
(defparameter *recursive-path* '(SYN GAP LIST FIRST))

;;; the path inside the node to be unified with the recursive node
(defparameter *local-path* nil)

;;; the path inside the node to be unified with the label node
(defparameter *label-fs-path* nil)

(defparameter *label-template-type* 'label)

