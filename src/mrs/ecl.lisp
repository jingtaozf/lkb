;;; Copyright (c) 2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.


;;;
;;; to avoid (bogus, i think) redefinition warning for print-object() method
;;; _extensions_.                                              (21-feb-05; oe)
;;;
#+:ecl
(eval-when #+:ansi-eval-when (:load-toplevel :compile-toplevel :execute)
	   #-:ansi-eval-when (load eval compile)
  (si::package-lock "CL" nil))

(in-package :mrs)

(ffi:defentry pet_type_to_code (:object) (:int "pet_type_to_code"))
(ffi:defentry pet_code_to_type (:int) (:object "pet_code_to_type"))
(ffi:defentry pet_feature_to_code (:object) (:int "pet_feature_to_code"))
(ffi:defentry pet_code_to_feature (:int) (:object "pet_code_to_feature"))

(ffi:defentry pet_fs_deref (:int) (:int "pet_fs_deref"))
(ffi:defentry pet_fs_cyclic_p (:int) (:int "pet_fs_cyclic_p"))
(ffi:defentry pet_fs_valid_p (:int) (:int "pet_fs_valid_p"))
(ffi:defentry pet_fs_type (:int) (:int "pet_fs_type"))

(ffi:defentry pet_fs_path_value (:int :object) (:int "pet_fs_path_value"))
(ffi:defentry pet_fs_arcs (:int) (:object "pet_fs_arcs"))

(ffi:defentry pet_type_valid_p (:int) (:int "pet_type_valid_p"))
(ffi:defentry pet_subtype_p (:int :int) (:int "pet_subtype_p"))
(ffi:defentry pet_glb (:int :int) (:int "pet_glb"))

(defun pet-type-to-code (key)
  (or (loop
          for (code . type) in %pet-types%
          thereis (when (equal key type) code))
      (let* ((name (typecase key
                     (symbol (symbol-name key))
                     (string key)))
             (code (and name (pet_type_to_code (string-downcase name)))))
        (when code
          (push (cons code key) %pet-types%)
          code))))

(defun pet-code-to-type (key)
  (when (fixnump key)
    (or (loop
            for (code . type) in %pet-types%
            thereis (when (equal key code) type))
        (let* ((type (pet_code_to_type key))
               (symbol (when type (vsym type))))
          (when symbol
            (push (cons key symbol) %pet-types%)
            symbol)))))

(defun pet-feature-to-code (key)
  (or (loop
          for (code . feature) in %pet-features%
          thereis (when (equal key feature) code))
      (let* ((name (typecase key
                     (symbol (symbol-name key))
                     (string key)))
             (code (and name (pet_feature_to_code (string-upcase name)))))
        (when code
          (push (cons code key) %pet-features%)
          code))))

(defun pet-code-to-feature (key)
  (when (fixnump key)
    (or (loop
            for (code . feature) in %pet-features%
            thereis (when (equal key code) feature))
        (let* ((feature (pet_code_to_feature key))
               (symbol (when feature (vsym feature))))
          (when feature
            (push (cons key symbol) %pet-features%)
            symbol)))))

(defun fixnump (n)
  (and (integerp n)
       (<= n most-positive-fixnum)
       (>= n most-negative-fixnum)))
