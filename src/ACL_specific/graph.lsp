;;; Copyright (c) 1991-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions


;;; Define :parse-tree, a new graph type for use with format-graph-from-root,
;;; that draws things that look more like traditional parse trees than what's
;;; produced by the :tree graph type.

(in-package :clim-internals)

(defclass parse-tree-graph-output-record (tree-graph-output-record) ())

(define-graph-type :parse-tree parse-tree-graph-output-record)

(defmethod layout-graph-nodes ((graph parse-tree-graph-output-record) 
			       stream arc-drawer options)
  (declare (ignore stream arc-drawer options))
  (with-slots (root-nodes properties) graph
    (let ((dx (getf properties :within-generation-separation))
	  (dy (getf properties :generation-separation)))
      (dolist (node root-nodes)
	(compute-widths node dx)
	(layout-node node (floor dx 2) (floor dy 2) dx dy))))
  (tree-recompute-extent graph))


;; Calculate the width of the subtree rooted at node, and store in the node's
;; GENERATION slot.

(defun compute-widths (node dx)
  (with-slots (node-children) node
    (if node-children
	;; Branching node
	(let ((width 0))
	  (dolist (child node-children)
	    (compute-widths child dx)
	    (incf width (graph-node-generation child)))
	  (setf (graph-node-generation node) 
	    (max (bounding-rectangle-width node)
		 (+ width (* (1- (length node-children)) dx)))))
      ;; Leaf node
      (setf (graph-node-generation node) 
	(+ dx (bounding-rectangle-width node))))))

(defun layout-node (node x y dx dy)
  (setf (graph-node-y node) y)
  (with-slots (node-children) node
    (if node-children
	;; Branching node
	(progn
	  (let ((x1 x)
		(y1 (+ y (bounding-rectangle-height node) dy)))
	    ;; Hack for single-branching nodes
	    (unless (cdr node-children)
	      (setf (graph-node-generation (car node-children))
		(max (graph-node-generation (car node-children))
		     (graph-node-generation node))))
	    (dolist (child node-children)
	      (layout-node child x1 y1 dx dy)
	      (incf x1 (+ (graph-node-generation child) dx))))
	  (setf (graph-node-x node) 
	    (- (floor (+ (point-x (bounding-rectangle-center 
				   (car node-children)))
			 (point-x (bounding-rectangle-center 
				   (car (last node-children)))))
		      2)
	       (floor (bounding-rectangle-width node) 2))))
      ;; Leaf node
      (setf (graph-node-x node) 
	(+ x (- (floor (graph-node-generation node) 2)
		(floor (bounding-rectangle-width node) 2)))))))

#|  
(defclass parse-chart-graph-output-record (directed-graph-output-record) ())

(define-graph-type :parse-chart parse-chart-graph-output-record)

(defmethod layout-graph-nodes :around ((graph parse-chart-graph-output-record) 
				       stream arc-drawer options)
  (call-next-method))

(defmethod layout-graph-nodes ((graph parse-chart-graph-output-record) 
			       stream arc-drawer options)
  (declare (ignore stream arc-drawer options))
  (setq user::x graph)
  (with-slots (root-nodes properties) graph
    (let* ((dx (getf properties :generation-separation))
	   (dy (getf properties :within-generation-separation))
	   (top-layer (loop for node in (graph-node-children (car root-nodes))
			  for i upfrom 1
			  do (setf (slot-value node 'generation-tick) i)
			  collecting node))
	   (widest-layer (sort-layers top-layer)))
      (let ((x dx))
	(loop for layer = top-layer then (next-layer layer)
	    while layer
	    do (let ((y 0)
		     (widest 0)
		     (new-layer (stable-sort 
				 (copy-list layer) #'< 
				 :key #'(lambda (node)
					  (slot-value node 
						      'generation-tick)))))
		 (loop for node in new-layer
		     do (setf (graph-node-x node) x)
			(setf (graph-node-y node) 
			  (incf y (+ 30 dy))) ;;(bounding-rectangle-height node) dy)))
			(setf widest (max widest 
					  (bounding-rectangle-width node))))
		 (incf x (+ widest dx)))))))
  (tree-recompute-extent graph))
	    
(defun sort-layers (layer)
  (when layer
    (let* ((new-layer 
	    (sort-layers
	     (loop for node in (next-layer layer)
		 do (setf (slot-value node 'generation-tick)
		      (* (/ (length (graph-node-parents node)))
			 (loop for parent in (graph-node-parents node)
			     sum (slot-value parent 'generation-tick))))
		 collecting node))))
      (if (> (length layer) (length new-layer))
	  layer
	new-layer))))

(defun next-layer (layer)
  (loop for node in layer
      appending (loop for child in (graph-node-children node)
		    when (= (graph-node-generation child) 
			    (1+ (graph-node-generation node)))
		    collect child)))
|#