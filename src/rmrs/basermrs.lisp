;;; Copyright (c) 2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :mrs)

;;; RMRS structure

(defstruct (rmrs (:include basemrs))
  rmrs-args 
  in-groups
  bindings
  cfrom
  cto
  origin)

;;; origin values are currently :erg or :rasp - we need this
;;; because the handling of variables is different

(defstruct rmrs-arg
  arg-type
  label
  val)

(defstruct in-group
  labels)

;;; the variables are replaced by canonical forms when 
;;; the structure is printed

(defstruct realpred
  lemma
  pos
  sense)

;;; An RMRS rel has no more than one variable.  In fact, its flist should have
;;; only one element, which is that variable.  This is assumed
;;; rather than checked in the following.

(defun retrieve-rmrs-ep-var (ep)
  (unless (rel-p ep) (error "Incorrect type passed to ep-var"))
  (car (rel-flist ep)))

;;; added for comparison code

(defstruct rmrs-position
  position
  ep)

(defun record-rmrs-position (position ep)
  (make-rmrs-position
   :position position
   :ep ep))

