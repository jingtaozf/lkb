;;; Define :parse-tree, a new graph type for use with format-graph-from-root,
;;; that draws things that look more like traditional parse trees that what's
;;; produced by the :tree graph type.

(in-package :clim-internals)

(defclass parse-tree-graph-output-record (tree-graph-output-record) ())

(define-graph-type :parse-tree parse-tree-graph-output-record)

(defmethod layout-graph-nodes ((graph parse-tree-graph-output-record) 
			       stream arc-drawer options)
  (declare (ignore stream arc-drawer options))
  (setq x graph)
  (with-slots (root-nodes properties) graph
    (let ((dx (getf properties :within-generation-separation))
	  (dy (getf properties :generation-separation)))
      (dolist (node root-nodes)
	(compute-widths node dx)
	(layout-node node (floor dx 2) (floor dy 2) dx dy))))
  (tree-recompute-extent graph))

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

  
  
