;;; Copyright Ann Copestake 1991-1997 All Rights Reserved.
;;; No use or redistribution without permission.
;;;
;;; course grammar specific globals file
;;; parameters only - grammar specific functions 
;;; should go in user-fns.lsp

;;; Types

(defparameter *toptype* '*top*)

(defparameter *string-type* 'string
   "a special type name - any lisp strings are subtypes of it")

;;; Lexical files

(defparameter *orth-path* '(orth list))

(defparameter *list-tail* '(rest))

(defparameter *list-head* '(first))

(def-lkb-parameter *empty-list-type* '*null*)

(def-lkb-parameter *list-type* '*list*)

(def-lkb-parameter *diff-list-type* '*diff-list*)

(def-lkb-parameter *diff-list-list* 'list)

(def-lkb-parameter *diff-list-last* 'last)

(defparameter *lex-rule-suffix* ""
 "creates the inflectional rule name from the information
   in irregs.tab - for PAGE compatability")

;;;

(defparameter *display-type-hierarchy-on-load* t)

;;; Parsing

(defparameter *maximum-number-of-edges* 500)

(defparameter *chart-limit* 100)

(defparameter *mother-feature* NIL
   "The feature giving the mother in a grammar rule")

(defparameter *start-symbol* '(root)
   "specifing valid parses")

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






