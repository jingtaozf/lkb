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

(defmethod print-object ((object saf) stream)
  (format stream "[SAF]"))

(defmethod print-object ((object saf-lattice) stream)
  (format stream "[SAF-LATTICE]"))

(defun saf-fs-feature-value2 (fs feature)
  (let ((x (find feature fs 
		 :key #'saf-fv-feature)))
    (if x
	(saf-fv-value x))))

;; len can be nil, meaning no limit
;; paths of length betw 1 and len
(defun annot-paths (annot lattice &key len)
  (cond
   ((and (numberp len)
	 (zerop len))
    nil)
   ((and (numberp len)
	 (= 1 len))
    (list (list annot)))
   (t
    (loop
	with next-node = (saf-edge-target annot)
	for next-annot in (get-edges-source next-node :lattice lattice)
	append
	  (loop 
	      for path in (annot-paths next-annot lattice 
				       :len (if (numberp len)
						(1- len)))
	      collect (push annot path))
	into paths
	finally
	  (return (push (list annot) paths))))))

;;; paths of length betw 1 and len
;(defun annot-paths (annot lattice &key len)
;  (cond
;   ((zerop len))
;   ((= 1 len)
;    (list (list annot)))
;   (t
;    (loop
;	with next-node = (saf-edge-target annot)
;	for next-annot in (get-edges-source next-node :lattice lattice)
;	append
;	  (loop 
;	      for path in (annot-paths next-annot lattice :len (1- len))
;	      collect (push annot path))
;	into paths
;	finally
;	  (return (push (list annot) paths))))))

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

(defun point-to-char-point (point addressing)
  (if (null point)
      (return-from point-to-char-point))
  (unless addressing
    (error "ADDRESSING cannot be null"))
  (cond
   ((eq addressing :|char|) 
    ;(ignore-errors 
     (parse-integer point)
    ; )
    )
    ((eq addressing :|xpoint|) -1)
    (t (error "unknown addressing scheme '~a'" addressing))))


(defun rename-nodes (saf node-map)
  (with-slots (lattice) saf
    (with-slots
	(nodes start-node end-node edges) lattice
      (setf nodes (rename-nodes-nodes nodes node-map))
      (setf start-node (mapped-node start-node node-map))
      (setf end-node (mapped-node end-node node-map))
      (setf edges (rename-nodes-edges edges node-map)))))


(defun rename-nodes-nodes (nodes node-map)
  (loop
      for node in nodes
      for i from 0
      for mapped-node = (mapped-node node node-map)
      do
	(setf (nth i nodes) mapped-node))
  nodes)


(defun rename-nodes-edges (edges node-map)
  (loop
      for annot in edges
      do
	(with-slots (source target) annot
	  (setf source (mapped-node source node-map))
	  (setf target (mapped-node target node-map))))
  edges)


(defun mapped-node (node node-map)
  (cdr (assoc node node-map :test #'equalp)))

(defun push-node-point (node point node-points)
  (pushnew point (gethash node node-points) :test #'equalp))

(defun get-node-map (saf)
  (let* ((lattice (saf-lattice saf))
	 (meta (saf-meta saf))
	 (addressing (saf-meta-addressing meta))
	 (annots (saf-lattice-edges lattice))
	 (start-node (saf-lattice-start-node lattice))
	 (end-node (saf-lattice-end-node lattice))
	 (node-points (make-hash-table :test #'equalp)))
    (loop
	for annot in annots
	do
	  ;(print annot) (terpri)
	  (with-slots (id source target from to) annot
	    ;(format t "~&~a ~a ~a ~a ~a" id source target from to)
	    (if (and from source) (pushnew from (gethash source node-points) :test #'equalp))
	    (if (and to target) (pushnew to (gethash target node-points) :test #'equalp))
	    ))
    
    (remhash start-node node-points)
    (remhash end-node node-points)
    
    ;(terpri)
    (loop
	with node-map = 
	  (append (list (cons start-node nil))
		  (sort
		   (loop
		       for node being each hash-key in node-points
		       for points = (gethash node node-points)
		       for point = (loop for p in points
				       minimize (point-to-char-point p addressing))
				   
		       collect (cons node point))
		   #'<
		   :key #'cdr)
		  (list (cons end-node nil)))
	for x in node-map
	for i from 0
	for new-node = (format nil "~a" i)
	do
	  (setf (cdr x) new-node)
	finally
	  (return node-map)
	  )))

#+:null
(defun get-node-map (saf)
  (let* ((lattice (saf-lattice saf))
	 (meta (saf-meta saf))
	 (addressing (saf-meta-addressing meta))
	 (annots (saf-lattice-edges lattice))
	 (node-points (make-hash-table :test #'equalp)))
    (loop
	for annot in annots
	do
	  ;(print annot) (terpri)
	  (with-slots (id source target from to) annot
	    ;(format t "~&~a ~a ~a ~a ~a" id source target from to)
	    (if (and from source) (pushnew from (gethash source node-points) :test #'equalp))
	    (if (and to target) (pushnew to (gethash target node-points) :test #'equalp))
	    ))
    ;(terpri)
    (loop
      with node-map = 
	  (sort
	   (loop
	       for node being each hash-key in node-points
	       for points = (gethash node node-points)
	       for point = (loop for p in points
			       maximize (point-to-char-point p addressing))
			       ;minimize (point-to-char-point p addressing))
			   
	       collect (cons node point))
	   #'<
	   :key #'cdr)
	for x in node-map
	for i from 0
	for new-node = (format nil "V~a" i)
	do
	  (setf (cdr x) new-node)
	finally
	  (return node-map)
	  )))


(defun rename-nodes-by-point-order (saf)
  (let ((node-map (get-node-map saf)))
    (rename-nodes saf node-map)
    saf))

(defun get-all-annot-paths (lattice max-tokens)
  (loop
      with edges = (saf:saf-lattice-edges lattice)
      for annot in edges
      append
	 (saf:annot-paths annot lattice :len max-tokens)
	))

