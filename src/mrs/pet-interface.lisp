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

(defconstant *mrs-package* :mrs)

(defun vsym (string) 
  #+:debug
  (format t "vsym(): `~a'.~%" string)
  ;;
  ;; mediate between PET-internal conventions for atomic types (i.e. |'foo| 
  ;; and |"foo"|) and MRS conventions; a little clumsy ... :-{.
  ;;
  (let ((n (length string)))
    (cond
     ((char= (char string 0) #\')
      (string-upcase (subseq string 1)))
     ((and (char= (char string 0) #\") (char= (char string (- n 1)) #\"))
      (subseq string 1 (- (length string) 1)))
     (t
      (intern (string-upcase string) *mrs-package*)))))
      
(defun deref (fs)
  ;;
  ;; given a feature structure, dereference it (i.e. follow pointer, if need
  ;; be).
  ;; _fix_me_
  ;; probably, this should not be exposed through the interface but called by
  ;; all fs-manipulating routines in the interface instead.   (24-aug-03; oe)
  ;;
  #+:debug
  (format t "deref(): ~a.~%" fs)
  (when (fixnump fs) (pet_fs_deref fs)))

#|
(defun cyclic-p (fs)
  ;;
  ;; given a feature structure, test for cycles (which are not allowed by the
  ;; FS logic, so probably the MRS construction code should be able to assert
  ;; that it will never be called on an invalid structure).
  ;;
  #+:debug
  (format t "cyclic-p(): ~a.~%" fs)
  (when (fixnump fs) (pet_fs_cyclic_p fs)))
|#

(defun path-value (fs path)
  ;;
  ;; given a feature structure and a list of symbols naming features, extract
  ;; the feature structure under the specified path.
  ;;
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

(defun is-valid-fs (fs)
  ;;
  ;; given a feature structure, test its validity.
  ;;
  #+:debug
  (format t "is-valid-fs(): ~a.~%" fs)
  (when (fixnump fs) (not (zerop (pet_fs_valid_p fs)))))

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
  #+:debug
  (format t "fs-arcs(): ~a.~%" fs)
  (when (fixnump fs)
    (let ((arcs (pet_fs_arcs fs)))
      (loop
          for (feature . value) in arcs
          collect 
            (cons (pet-code-to-feature feature) value)))))

(defun fs-type (fs)
  ;;
  ;; given a feature structure, extract its type.
  ;;
  #+:debug
  (format t "fs-type(): ~a.~%" fs)
  (when (fixnump fs)
    (let ((code (pet_fs_type fs)))
      (unless (= code -1) (pet-code-to-type code)))))

(defun is-valid-type (type)
  ;;
  ;; given a type, test its validity.
  ;;
  #+:debug
  (format t "is-valid-type(): ~a.~%" type)
  (let ((code (pet-type-to-code type)))
    (when (fixnump code) (not (zerop (pet_type_valid_p code))))))

(defun is-top-type (type)
  ;;
  ;; given a type, return true if it is the top (i.e. most general) type.
  ;;
  #+:debug
  (format t "is-top-type(): ~a.~%" type)
  (let ((code (pet-type-to-code type)))
    (when (fixnump code) (= code %pet-top%))))

(defun equal-or-subtype (type1 type2)
  ;;
  ;; given two types, return true if .type1. is equal to .type2. or one of its
  ;; descendants.
  ;;
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
  ;;
  ;; given two types, return true if .type1. and .type2. are either identical
  ;; or have a greatest lower bound (common descendant).
  ;;
  #+:debug
  (format t "compatible-types(): ~a ~a.~%" type1 type2)
  (or (and (null type1) (null type2))
      (let* ((code1 (pet-type-to-code type1))
             (code2 (pet-type-to-code type2)))
        (when (and (fixnump code1) (fixnump code2))
          (let ((glb (pet_glb code1 code2)))
            (unless (= glb %pet-bottom%) (pet-code-to-type glb)))))))

(defun fs-to-mrs (fs &optional (mode 'simple))
  ;;
  ;; top-level entry point for PET: given a full FS (typically obtained from a
  ;; parsing result), return a string representing the MRS in the requested
  ;; format.
  ;;
  (when (fixnump fs)
    (ignore-errors
     ;;
     ;; _fix_me_
     ;; not sure the shadowing of type and feature tables is necessary: it will
     ;; restrict caching to calls within a structure, but given the overall
     ;; architecture of PET, the mapping of integers to types and features will
     ;; never change over time.  resolve, once efficiency of MRS generation
     ;; becomes an issue.                                      (9-sep-03; oe)
     ;;
     (let* ((%pet-types% nil)
            (%pet-features% nil)
            (psoa (extract-mrs-from-fs fs)))
       (if (psoa-p psoa)
         (let* ((mode 
                 (typecase mode
                   (symbol (intern (string-upcase (symbol-name mode)) 
                                   *mrs-package*))
                   (string (intern (string-upcase mode) *mrs-package*))))
                (result  
                 (with-output-to-string (stream)
                   (case mode
                     ((simple indexed prolog html)
                      (output-mrs1 psoa mode stream))
                     (scoped
                      (let ((scopes (make-scoped-mrs psoa)))
                        (loop
                            for scope in scopes
                            do
                              (setf *canonical-bindings* 
                                (canonical-bindings scope))
                              (mrs::output-scoped-mrs 
                               psoa :stream stream)
                            finally (setf *canonical-bindings* nil))))
                     ((eds dependencies)
                      (ed-output-psoa psoa :stream stream))
                     ((rmrs xml)
                      (let ((rmrs (mrs-to-rmrs psoa)))
                        (when (rmrs-p rmrs)
                          (output-rmrs1 
                           rmrs 
                           (if (eq mode 'rmrs) 'compact 'xml) 
                           stream))))))))
           (when (and result (not (string= result ""))) result))
         (format 
          t 
          "fs-to-mrs(): unable to extract MRS from fs # ~a.~%" 
          fs))))))
