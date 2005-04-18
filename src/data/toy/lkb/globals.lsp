;;; Hey, emacs, this file is -*- Mode: common-lisp; -*- ... got that?

;;;
;;; a set of global declations --- various aspects of feature geometry et al.
;;;

(in-package :lkb)

(defparameter *toptype* '*top*)

(defparameter *string-type* '*string*)

(defparameter *orth-path* '(ORTH LIST FIRST))

(defparameter *list-tail* '(REST))

(defparameter *list-head* '(FIRST))

(defparameter *list-type* '*list*)

(defparameter *empty-list-type* '*null*)

(defparameter *non-empty-list-type* '*ne-list*)

(defparameter *display-type-hierarchy-on-load* t)

(defparameter *simple-tree-display* t)


(defparameter *last-parses*
  (let ((symbol (find-symbol "*LAST-PARSES*" :lkb)))
    (if (and (boundp symbol) (rest (symbol-value symbol)))
      (symbol-value symbol)
      '("the dog barks"))))

(defparameter *mother-feature* nil)

(defparameter *start-symbol* '(root))

(defparameter *morph-rule-type* 'word)

(defparameter  *semantics-index-path* '(SEM INDEX))
