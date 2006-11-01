;; Copyright (c) 2006
;;;   Ben Waldron;
;;; see `licence.txt' for conditions.

;;
;; code to convert SAF XML into SAF object
;;

;;
;; TODO: complete move to :saf namespace
;;

(in-package :saf)

(defvar *char-map-add-offset*)

(defvar *lmap* nil)
(defvar *dir* nil)

(defstruct saf
  meta
  lattice)

(defstruct saf-lattice
  start-node
  end-node
  nodes
  edges)

(defstruct saf-edge
  id
  type
  source
  target
  from
  to
  deps
  content
  l-content
  )

(defstruct saf-fv
  feature
  value)

(defstruct saf-meta
  document
  addressing
  olac
  text)

(defun saf-fs-feature-value (fs feature)
  (let ((x (find feature fs 
		 :key #'saf-fv-feature
		 :test #'string=)))
    (if x
	(saf-fv-value x))))

(defun saf-fs-feature-value2 (fs feature)
  (let ((x (find feature fs 
		 :key #'saf-fv-feature)))
    (if x
	(saf-fv-value x))))

