;;; Extension to the LKB to unfill feature structures
;;; Copyright (C) 2002-2004  Frederik Fouvry
;;; 
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;;
;;; Author contact: Frederik.Fouvry@coli.uni-saarland.de
;;;                 Saarland University, FR 4.7 Allgemeine Linguistik,
;;;                 PO Box 15 11 50, D-66041 Saarbruecken, Germany

;;; see licence.txt for conditions

(in-package :lkb)

;;; May be necessary
;; (setf *chart-packing-p* nil)


(defmacro redundant-dag-type-p (type reference-type)
  `(not (subtype-p ,type ,reference-type)))


(defparameter *reentrancy-dag* nil)

;; Keys are dags, and the values are arrays.  They can be accessed
;; with the macros below.
(defparameter *redundancy-hash* (make-hash-table :test #'eq))
(defparameter *redundancy-hash-initial-content*
    (make-array (list 7)
		:initial-contents (list :undefined nil nil nil nil nil :undefined)))

(defmacro redundant-node (array) `(aref ,array 0)) ; recursively checked
(defmacro redundant-type (array) `(aref ,array 1))
(defmacro features-to-remove (array) `(aref ,array 2))
(defmacro first-path (array) `(aref ,array 3))
(defmacro non-redundant-reentrant-paths (array) `(aref ,array 4))
(defmacro redundant-reentrant-paths (array) `(aref ,array 5))
(defmacro junk-slot (array) `(aref ,array 6))

(defun updated-reference-dag (reference-dag dag-type redundant-type)
  (if (or redundant-type (stringp dag-type))
      reference-dag
    ;; Reentrancy references should not become out-of-date
    (let ((*visit-generation* *visit-generation*))
      ;; Unifications are necessary in order to keep the constraints
      ;; that come from higher up in the feature structure
      (unify-dags (constraint-of dag-type) reference-dag))))
    


;; Check the type redundancy of all nodes (nb: reentrant ones should
;; be checked from any point of entrance, and then recursively -
;; unless it is known that there are only redundant type values below
;; - not done here), and build up a feature structure for comparison
;; of reentrancies (re-dag).
;; 
;; NOTE: sometimes (eg with *chart-packing-p*), the reference feature
;; structures contain MORE information than dag.  Can this be solved easily?
(defun check-type-values (dag reference-dag fpath)
  (unless reference-dag
    (error "Second argument of check-type-values() cannot be nil"))
  (let* ((dag-type (type-of-fs dag))
	 (redundant-type
	  (redundant-dag-type-p dag-type (type-of-fs reference-dag)))
	 (all-arcs-redundant t))
    (unless (eq (dag-visit dag) t)
      (setf (dag-visit dag) t
	    (gethash dag *redundancy-hash*)
	    (copy-seq *redundancy-hash-initial-content*)))
    (if redundant-type
	(setf (redundant-type (gethash dag *redundancy-hash*)) t)
      (unless (stringp dag-type)
	;; update *reentrancy-dag*
	(push (make-unification :lhs (create-path-from-feature-list (reverse fpath))
				:rhs (make-u-value :type dag-type))
	      *reentrancy-dag*)))
    (unless (eq (junk-slot (gethash dag *redundancy-hash*)) t)
      (setf (junk-slot (gethash dag *redundancy-hash*))
	(and
	 (let ((updated-reference-dag
		(when (dag-arcs dag) ; don't call when result will not be used
		  (updated-reference-dag reference-dag dag-type redundant-type))))
	   (dolist (arc (dag-arcs dag) all-arcs-redundant)
	     (let* ((feature (dag-arc-attribute arc)))
	       (unless (check-type-values (dag-arc-value arc)
					  (get-dag-value updated-reference-dag
							 feature)
					  (cons feature fpath))
		 (setf all-arcs-redundant nil))))
	   redundant-type))))))


;; Check a dag with *reentrancy-dag*, that is built up in
;; check-type-values().  A reentrancy is redundant if it also occurs
;; in the reference dag re-dag.  Unvisited nodes and non-redundant
;; reentrancy dags are processed recursively.
(defun check-reentrancies (dag re-dag fpath)
  ;; dag type may be more general than re-dag type (packing)
  (let* ((visited (dag-visit dag)))
    (if (and visited (eq visited re-dag)) ; redundant
	(let ((dag-info (gethash dag *redundancy-hash*)))
	  ;; With redundant reentrancies: take the shortest of the available paths
	  (if (< (length fpath) (length (first-path dag-info)))
	      (progn
		(setf (redundant-reentrant-paths (gethash dag *redundancy-hash*))
		  (delete fpath (cons (first-path dag-info)
				      (redundant-reentrant-paths dag-info))
			  :test #'equal))
		(setf (first-path (gethash dag *redundancy-hash*)) fpath))
	    (pushnew fpath
		     (redundant-reentrant-paths (gethash dag *redundancy-hash*))
		     :test #'equal)))
      (let (new-re-dag)
	(if visited			; non-redundant
	    (progn
	      (pushnew fpath
		       (non-redundant-reentrant-paths (gethash dag *redundancy-hash*))
		       :test #'equal)
	      ;; continue with a new re-dag
	      (setf new-re-dag visited))
	  ;; unvisited
	  (setf (dag-visit dag) re-dag
		(first-path (gethash dag *redundancy-hash*)) fpath
		new-re-dag re-dag))
	(dolist (arc (dag-arcs dag))
	  (check-reentrancies (dag-arc-value arc)
			      (get-dag-value new-re-dag (dag-arc-attribute arc))
			      (cons (dag-arc-attribute arc) fpath)))))))


;; Walk through fs and check feature values: if they are all (a)
;; type-redundant, (b) have no non-redundant reentrancies pointing to
;; them, (c) and all their arcs are redundant, then their status can
;; be summarized in array[0].  Redundant reentrancies have to be
;; removed regardless of the status of the nodes they point to: that
;; is tested separately.
(defun check-nodes (dag fpath)
  (let ((dag-info (gethash dag *redundancy-hash*)))
    (if (eq (redundant-node dag-info) :undefined)
	(or (member fpath (redundant-reentrant-paths dag-info) :test #'equal)
	    (let* ((all-arcs-redundant t)
		   (arc-redundant
		    (dolist (arc (dag-arcs dag) all-arcs-redundant)
		      (if (check-nodes (dag-arc-value arc)
				       (cons (dag-arc-attribute arc) fpath))
			  (pushnew (dag-arc-attribute arc)
				   (features-to-remove
				    (gethash dag *redundancy-hash*))
				   :test #'eq)
			(setf all-arcs-redundant nil)))))
	      (setf (redundant-node (gethash dag *redundancy-hash*))
		(and (redundant-type dag-info)
		     (null (non-redundant-reentrant-paths dag-info))
		     arc-redundant))))
      (or (redundant-node dag-info)
	  (member fpath (redundant-reentrant-paths dag-info)
		  :test #'equal)))))


;; Main access function
(defun unfilled-dag (dag)
  ;; Changes the dag destructively.
  ;; If necessary, dag should be copied before it is passed to this function.
  (let* ((dag-type (type-of-fs dag))
	 (reference-dag (copy-dag (constraint-of dag-type))))
    (setf *redundancy-hash* (make-hash-table :test #'eq)
	  *reentrancy-dag* nil)
    (invalidate-visit-marks)
    (check-type-values dag reference-dag nil)
    (setf *reentrancy-dag*
      (create-wffs
       (process-unifications
	(cons (make-unification :lhs (create-path-from-feature-list nil)
				:rhs (make-u-value :type dag-type))
	      *reentrancy-dag*))))
    (invalidate-visit-marks)
    (check-reentrancies dag *reentrancy-dag* nil)
    (invalidate-visit-marks)
    (check-nodes dag nil)
    (maphash #'(lambda (d redundancy)
		 ;; Remove types - may not be desirable for running
		 #-null
		 (declare (ignore redundancy))
		 #+null
		 (when (redundant-type redundancy)
		   (setf (type-of-fs d) *toptype*))
		 (setf (dag-arcs d)
		   (delete-if #'(lambda (arc)
				  (member (dag-arc-attribute arc)
					  (features-to-remove
					   (gethash d *redundancy-hash*))))
			      (dag-arcs d)))) *redundancy-hash*)
    (setf (type-of-fs dag) dag-type)
    (setf *redundancy-hash* nil
	  *reentrancy-dag* nil)
    dag))


(defun unfilled-tdfs (tdfs)
  ;; Destructively changes tdfs.
  ;; If necessary, tdfs should be copied before it is passed to this function.
  (setf (tdfs-indef tdfs) (unfilled-dag (tdfs-indef tdfs)))
  tdfs)
