;;; Copyright (c) 2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :mrs)

(defconstant %pet-top% 0)

(defconstant %pet-bottom% -1)

(defparameter %pet-types% nil)

(defparameter %pet-features% nil)

(defun pet-clear-symbol-tables ()
  (setf %pet-types% nil)
  (setf %pet-features% nil))

(defun vsym (string) 
  #+:debug
  (format t "vsym(): `~a'.~%" string)
  (intern (string-upcase string) :mrs))
      
(defun deref (fs)
  #+:debug
  (format t "deref(): ~a.~%" fs)
  (when (fixnump fs) (pet_fs_deref fs)))

(defun cyclic-p (fs)
  #+:debug
  (format t "cyclic-p(): ~a.~%" fs)
  (when (fixnump fs) (pet_fs_cyclic_p fs)))

(defun path-value (fs path)
  #+:debug
  (format t "path-value(): ~a ~a.~%" fs path)
  (when (fixnump fs)
    (loop
        with vector = (make-array (length path))
        for i from 0
        for feature in path
        for code = (pet-feature-to-code feature)
        unless code do (return-from path-value)
        else do (setf (aref vector i) code)
        finally (return (pet_fs_path_value fs vector)))))

(defun is-disjunctive-fs (fs)
  (declare (ignore fs))
  nil)

(defun is-valid-fs (fs)
  #+:debug
  (format t "is-valid-fs(): ~a.~%" fs)
  (when (fixnump fs) (not (zerop (pet_fs_valid_p fs)))))

(defun fs-arcs (fs)
  #+:debug
  (format t "fs-arcs(): ~a.~%" fs)
  (when (fixnump fs)
    (let ((arcs (pet_fs_arcs fs)))
      (loop
          for (feature . value) in arcs
          collect 
            (cons (pet-code-to-feature feature) value)))))

(defun fs-type (fs)
  #+:debug
  (format t "fs-type(): ~a.~%" fs)
  (when (fixnump fs)
    (let ((code (pet_fs_type fs)))
      (unless (= code -1) (pet-code-to-type code)))))

(defun is-top-type (type)
  #+:debug
  (format t "is-top-type(): ~a.~%" type)
  (let ((code (pet-type-to-code type)))
    (when (fixnump code) (= code %pet-top%))))

(defun is-valid-type (type)
  #+:debug
  (format t "is-valid-type(): ~a.~%" type)
  (let ((code (pet-type-to-code type)))
    (when (fixnump code) (not (zerop (pet_type_valid_p code))))))

(defun equal-or-subtype (type1 type2)
  #+:debug
  (format t "equal-or-subtype(): ~a ~a.~%" type1 type2)
  ;;
  ;; true if .type1. is identical to .type2. or one of its supertypes.
  ;;
  (let ((code1 (pet-type-to-code type1))
        (code2 (pet-type-to-code type2)))
    (when (and (fixnump code1) (fixnump code2))
      (or (= code1 code2)
          (not (zerop (pet_subtype_p code1 code2)))))))

(defun compatible-types (type1 type2)
  #+:debug
  (format t "compatible-types(): ~a ~a.~%" type1 type2)
  (or (and (null type1) (null type2))
      (let* ((code1 (pet-type-to-code type1))
             (code2 (pet-type-to-code type2)))
        (when (and (fixnump code1) (fixnump code2))
          (let ((glb (pet_glb code1 code2)))
            (unless (= glb %pet-bottom%) (pet-code-to-type glb)))))))

(defun fs-to-mrs (fs)
  (when (fixnump fs)
    (let* ((%pet-types% nil)
           (%pet-features% nil)
           (mrs (extract-mrs-from-fs fs)))
      (if (psoa-p mrs)
        (output-mrs1 mrs 'simple t)
        (format t "fs-to-mrs(): unable to extract MRS from fs # ~a.~%" fs)))))
