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
  origin
  surface
  ident)

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

;;; Comparison structures (mainly for compare.lisp, but also annlt.lisp
;;; and lkb-acl-rmrs.lisp)

(defstruct rmrs-comparison-record
  matched-top matched-rels matched-args matched-ings matched-hcons bindings
  qeq-pairs)

(defstruct match-rel-record
  rel1
  rel2
  pred-comp-status
  var-comp-status
  arg-comp-status
  label-pair ;; rel labels
  anchor-pair ;; rel anchors (for version without ING) 
  cvar-pair ;; characteristic vars (non-handle)
  hvar-pair ;; non arg vars (handle)
  )

;;; comp-status records the class of compatibility
;;; between the relation
;;; :equal
;;; :sub1 - rel1 is more specific tham rel2
;;; :sub2 - rel2 is more specific tham rel1
;;; :comp - rel1 and rel2 are compatible but not equal
;;;         and not in a subsumes relationship

(defstruct match-top-record
  label1
  label2)

(defstruct match-arg-record
  arg1
  arg2
  comp-status
  arg-type) ; want to record if this is a carg

(defstruct match-ing-record
  ing1
  ing2)

(defstruct match-hcons-record
  hcons1
  hcons2)

;;; output as string - used in output but also in convert

(defun convert-realpred-to-string (lemma pos sense)
  (if sense
	(format nil "_~(~a_~a_~@[~a~]~)" 
		lemma (or pos "U") sense)
      (format nil "_~(~a_~a~)" 
	      lemma (or pos "U"))))

