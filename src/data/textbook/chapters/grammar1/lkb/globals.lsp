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

(defparameter *orth-path* '(orth))

(defparameter *list-tail* '(rest))

(defparameter *list-head* '(first))

;;;

(defparameter *display-type-hierarchy-on-load* t)

(defparameter *simple-tree-display* t)

;;; Parsing

(defparameter *last-parses* '("The dog chased the cat"))

(defparameter *mother-feature* NIL
   "The feature giving the mother in a grammar rule")

(defparameter *start-symbol* '(root)
   "specifing valid parses")

(defparameter *morph-rule-type* 'word
    "morphology system checks for rules which are
     of this type or a subtype of it")


;;; Generation

(defparameter  *semantics-index-path* '(SEM INDEX))





