;;; Copyright (c) 2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :mrs)

(defconstant %pet-top% 0)

(defconstant %pet-bottom% -1)

(defparameter %pet-types% nil)

(defparameter %pet-features% nil)

(defun vsym (string) 
  (intern (string-upcase string) :mrs))
      
(defun deref (fs)
  (when (fixnump fs) (pet_fs_deref fs)))

(defun cyclic-p (fs)
  (when (fixnump fs) (pet_fs_cyclic_p fs)))

(defun path-value (fs path)
  (when (fixnump fs)
    (loop
        with vector = (make-array 
                       (length path) :element-type 'fixnum
                       :adjustable nil :fill-pointer 0)
        for i from 0
        for feature in path
        for code = (pet-feature-to-code feature)
        unless code do (return-from path-value)
        else do (setf (svref vector i) code)
        finally (return (pet_fs_path_value fs vector)))))

(defun is-disjunctive-fs (fs)
  (declare (ignore fs))
  nil)

(defun is-valid-fs (fs)
  (when (fixnump fs) (pet_fs_valid_p fs)))

(defun fs-arcs (fs)
  (when (fixnump fs)
    (let ((arcs (pet_fs_arcs fs)))
      (loop
          for (feature . value) in arcs
          collect 
            (cons (pet-code-to-feature feature) (pet-code-to-type value))))))

(defun fs-type (fs)
  (when (fixnump fs) (pet_fs_type fs)))

(defun is-top-type (type)
  (let ((code (pet-type-to-code type)))
    (when (fixnump code) (= code %pet-top%))))

(defun is-valid-type (type)
  (let ((code (pet-type-to-code type)))
    (when (fixnump code) (pet_type_valid_p code))))

(defun equal-or-subtype (type1 type2)
  ;;
  ;; true if .type1. is identical to .type2. or one of its supertypes.
  ;;
  (let ((code1 (pet-type-to-code type1))
        (code2 (pet-type-to-code type2)))
    (when (and (fixnump code1) (fixnump code2))
      (or (= code1 code2)
          (not (zerop (pet_subtype_p code1 code2)))))))

(defun compatible-types (type1 type2)
  (or (and (null type1) (null type2))
      (let* ((code1 (pet-type-to-code type1))
             (code2 (pet-type-to-code type2)))
        (when (and (fixnump code1) (fixnump code2))
          (let ((glb (pet_glb code1 code2)))
            (unless (= glb %pet-bottom%)))))))

(defun fs-to-mrs (fs)
  (when (fixnump fs)
    (let ((mrs (extract-mrs-from-fs fs)))
      (if (mrs-p mrs)
        (output-mrs1 mrs 'simple t)
        (format t "fs-to-mrs(): unable to extract MRS from fs # ~a.~%" fs)))))
