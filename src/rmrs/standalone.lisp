;;; Copyright (c) 2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.


;;; WARNING: this file must be kept in sync with mrs/basemrs.lisp

(in-package :common-lisp-user)

(defpackage :mrs 
  (:use :common-lisp)
  (:export ))


(in-package :mrs)

;;; defines structures also defined in the main MRS code, so the RMRS
;;; can be run standalone

;;; variable generation (from mrs/basemrs.lisp)

(defvar *restart-variable-generator* t)

(defun create-variable-generator (&optional start)
  (let ((number (or start 0)))
    #'(lambda nil
        (incf number)
        number)))

;;; structures (from mrs/basemrs.lisp)

(defstruct (basemrs)
  top-h
  liszt
  h-cons)

(defstruct (rel)
  handel                               
  pred					; relation name
  flist
  parameter-strings                     ; copy of the relations with constant
					; values, used in the generator
                                        ; candidate for removal!
  extra)                                ; extra is a junk slot
                                        ; needed for the munging rules 

(defstruct (char-rel (:include rel))
  cfrom
  cto)

(defstruct (var)
  type
  extra ; useful for e.g. agreement values
  id)

(defstruct (extrapair)
  feature
  value)

(defstruct (handle-var (:include var)))

(defstruct (grammar-var (:include var)))
;;; a sort of placeholder variable

(defstruct (hcons)
  relation
  scarg
  outscpd)

;;; macros

(defmacro is-handel-var (var)
    ;;; test is whether the type is "h"
  `(and (var-p ,var)
       (equal (var-type ,var) "h")))
