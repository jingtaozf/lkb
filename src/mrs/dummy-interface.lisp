;;; Copyright (c) 2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

;;;
;;; set of dummy back-end interface functions for MRS generation; mostly used
;;; as a skeleton for interfacing to another (T)FS system.  vsym(), however, is
;;; needed even when we are loading neither on top of the LKB or PET (the two
;;; systems for which back-end interface functions are defined).
;;;

(in-package :mrs)

(defun vsym (string) 
  (intern (string-upcase (string string)) :mrs))

(defun get-parse-fs (result)
  ;;
  ;; given a parse result (i.e. whatever structure the parser returns _after_
  ;; unpacking), extract the feature structure (DAG) that includes the MRS.
  ;;
  (declare (ignore result)))
      
(defun deref (fs)
  ;;
  ;; given a feature structure, dereference it (i.e. follow pointer, if need
  ;; be).
  ;; _fix_me_
  ;; probably, this should not be exposed through the interface but called by
  ;; all fs-manipulating routines in the interface instead.   (24-aug-03; oe)
  ;;
  (declare (ignore fs)))

(defun cyclic-p (fs)
  ;;
  ;; given a feature structure, test for cycles (which are not allowed by the
  ;; FS logic, so probably the MRS construction code should be able to assert
  ;; that it will never be called on an invalid structure).
  ;;
  (declare (ignore fs)))

(defun path-value (fs path)
  ;;
  ;; given a feature structure and a list of symbols naming features, extract
  ;; the feature structure under the specified path.
  ;;
  (declare (ignore fs path)))

(defun is-valid-fs (fs)
  ;;
  ;; given a feature structure, test its validity.
  ;;
  (declare (ignore fs)))

(defun fs-arcs (fs)
  ;;
  ;; given a feature structure, return an association list containing feature
  ;; -- value (aka feature structure) pairs, e.g.
  ;;
  ;;   ((LBL . #D[handle ...]) (WLINK . #D[*cons* ...]) (PRED . #D[*top* ...])
  ;;    (ARG0 . #D[event ...]) (ARG1 . #D[ref-ind ...]))
  ;;
  ;; where features are symbols and values whatever representation is used for
  ;; feature structures in the interface (i.e. integers for PET).
  ;;
  (declare (ignore fs)))

(defun fs-type (fs)
  ;;
  ;; given a feature structure, extract its type.
  ;;
  (declare (ignore fs)))

(defun is-valid-type (type)
  ;;
  ;; given a type, test its validity.
  ;;
  (declare (ignore type)))

(defun is-top-type (type)
  ;;
  ;; given a type, return true if it is the top (i.e. most general) type.
  ;;
  (declare (ignore type)))

(defun equal-or-subtype (type1 type2)
  ;;
  ;; given two types, return true if .type1. is equal to .type2. or one of its
  ;; descendants.
  ;;
  (declare (ignore type1 type2)))

(defun compatible-types (type1 type2)
  ;;
  ;; given two types, return true if .type1. and .type2. are either identical
  ;; or have a greatest lower bound (common descendant).
  ;;
  (declare (ignore type1 type2)))
