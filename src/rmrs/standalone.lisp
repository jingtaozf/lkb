;;; Copyright (c) 2003--2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.


;;; WARNING: this file must be kept in sync with mrs/basemrs.lisp
;;; and mrs/mrscorpus.lisp

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

;;; test for variable equality

(defun eql-var-id (var1 var2)
  ;;; can't be macroized cos used where fn is required
  ;;; has to be `equal' since
  ;;; used for grammar vars etc in RMRS composition code
  ;;; where the id is a string
  (equal (var-id var1) (var-id var2)))

;;; from mrs/mrscorpus.lisp

(defun combine-similar-relations (liszt result-so-far test-fn)
  (if (null liszt)
      result-so-far
    (let ((test-rel (car liszt))
          (similar nil)
          (non-similar nil))
      (loop for rel in (cdr liszt)
           do
           (if (apply test-fn (list test-rel rel))
               (push rel similar)
             (push rel non-similar)))
      (combine-similar-relations non-similar
                                 (push (cons test-rel similar)
				       result-so-far)
				 test-fn))))
