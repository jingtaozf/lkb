;;; Copyright Ann Copestake 1991-1997 All Rights Reserved.
;;; No use or redistribution without permission.
;;;
;;; course grammar specific globals file
;;; parameters only - grammar specific functions 
;;; should go in user-fns.lsp

(defparameter *morph-option* :distinct-mphon)

(defparameter *active-parsing-p* nil)

;;; Types

(defparameter *toptype* '*top*)

(defparameter *string-type* 'string
   "a special type name - any lisp strings are subtypes of it")

;;; Lexical files

(defparameter *orth-path* '(orth list first))

(defparameter *list-tail* '(rest))

(defparameter *list-head* '(first))

(def-lkb-parameter *diff-list-type* '*dlist*)

(def-lkb-parameter *diff-list-list* 'list)

(def-lkb-parameter *diff-list-last* 'last)

;;;

(defparameter *display-type-hierarchy-on-load* nil)

(defparameter *simple-tree-display* t)

;;; Parsing

(defparameter *last-parses* 
    (if 
     (boundp '*last-parses*)
    (if 
      (equal *last-parses* '("Kim sleeps"))
            '("The dog chased the cat")
      *last-parses*)
    nil))

(defparameter *mother-feature* NIL
   "The feature giving the mother in a grammar rule")

(defparameter *start-symbol* '(start-symbol)
   "specifing valid parses")

;;; Morphology

(defparameter *morph-rule-type* 'word
    "morphology system checks for rules which are
     of this type or a subtype of it")

;;; Generation

(defparameter  *semantics-index-path* '(SEM HOOK INDEX))

;;; Parse tree node labels

(defparameter *simple-tree-display* nil)

;;; the path where the name string is stored
(defparameter *label-path* '(LABEL-NAME))

;;; the path inside the node to be unified with the label node
(defparameter *label-fs-path* nil)

(defparameter *label-template-type* 'label)
