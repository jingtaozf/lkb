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

;; paths of length betw 1 and len
(defun annot-paths (annot lattice &key len)
  (cond
   ((zerop len))
   ((= 1 len)
    (list (list annot)))
   (t
    (loop
	with next-node = (saf-edge-target annot)
	for next-annot in (get-edges-source next-node :lattice lattice)
	append
	  (loop 
	      for path in (annot-paths next-annot lattice :len (1- len))
	      collect (push annot path))
	into paths
	finally
	  (return (push (list annot) paths))))))

;; return outgoing edges from source node
(defun get-edges-source (source &key lattice)
  (unless lattice
    (error "missing LATTICE argument"))
  ;; FIXME: inefficient
  (loop 
      for edge in (saf-lattice-edges lattice)
      for source1 = (saf-edge-source edge)
      when (equalp source1 source)
      collect edge))

