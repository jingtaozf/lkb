;;; Copyright (c) 2003--2004
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
  label-a label-b)

;;; Jan 2004 - AAC 
;;; changed structure to reflect the XML and to support the 
;;; notion of a canonical approach to in-groups

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

(defstruct rmrs-position-record 
  top eps args ings hcons)

(defstruct rmrs-object-position
  position
  object)

(defun record-rmrs-position (position object)
  (make-rmrs-object-position
   :position position
   :object object))

