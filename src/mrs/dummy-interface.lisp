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
      
(defun deref (fs)
  (declare (ignore fs)))

(defun cyclic-p (fs)
  (declare (ignore fs)))

(defun path-value (fs path)
  (declare (ignore fs path)))

(defun is-disjunctive-fs (fs)
  (declare (ignore fs)))

(defun is-valid-fs (fs)
  (declare (ignore fs)))

(defun fs-arcs (fs)
  (declare (ignore fs)))

(defun fs-type (fs)
  (declare (ignore fs)))

(defun is-top-type (type)
  (declare (ignore type)))

(defun is-valid-type (type)
  (declare (ignore type)))

(defun equal-or-subtype (type1 type2)
  (declare (ignore type1 type2)))

(defun compatible-types (type1 type2)
  (declare (ignore type1 type2)))
