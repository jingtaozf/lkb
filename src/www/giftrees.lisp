;;; Draw fancy parse trees
;;;   Rob Malouf, 8-May-1996

(in-package :http-user)

(defparameter *tree-font* gd:font-giant)
(defparameter *tree-font-height* (gd:font-h *tree-font*))
(defparameter *tree-font-width* (gd:font-w *tree-font*))

(defparameter *tree-vskip* 10)
(defparameter *tree-hskip* 20)

(defparameter *tree-vmargin* 15)
(defparameter *tree-hmargin* 10)

(defparameter *font-width* 9)
(defparameter *font-height* 14)

(defun label-width (node)
  (* (length (get node 'label)) *font-width*))


(defun layout-tree (node)
  (compute-widths node *tree-hskip*)
  (layout-node node (floor *tree-hskip* 2) (floor *tree-vskip* 2) 
	       *tree-hskip* *tree-vskip*))

;; Calculate the width of the subtree rooted at node.

(defun compute-widths (node dx)
  (setf (get node 'label) (user::get-string-for-edge node))
  (let ((node-children (user::find-children node)))
    (if node-children
	;; Branching node
	(let ((width 0)
	      (height 0))
	  (dolist (child node-children)
	    (compute-widths child dx)
	    (incf width (get child 'width))
	    (setq height (max height (get child 'height))))
	  (setf (get node 'width) 
	    (max (label-width node)
		 (+ width (* (1- (length node-children)) dx))))
	  (setf (get node 'height)
	    (+ height *font-height* *tree-vskip*)))
      ;; Leaf node
      (progn
	(setf (get node 'width) (+ dx (label-width node)))
	(setf (get node 'height) *font-height*)))))

(defun layout-node (node x y dx dy)
  (setf (get node 'y) y)
  (let ((node-children (user::find-children node)))
    (if node-children
	;; Branching node
	(progn
	  (let ((x1 x)
		(y1 (+ y *font-height* dy)))
	    ;; Hack for single-branching nodes
	    (unless (cdr node-children)
	      (setf (get (car node-children) 'width)
		(max (get (car node-children) 'width)
		     (get node 'width))))
	    (dolist (child node-children)
	      (layout-node child x1 y1 dx dy)
	      (incf x1 (+ (get child 'width) dx))))
	  (setf (get node 'x) 
	    (+ (get (car node-children) 'x)
	       (floor (- (get (car (last node-children)) 'x)
			 (get (car node-children) 'x))
		      2))))
      ;; Leaf node
      (setf (get node 'x) (+ x (floor (label-width node) 2))))))

(defmethod draw-tree (tree image color)
  (let ((cx (- (get tree 'x)
	       (floor (label-width tree) 2))))
    (gd:image-string image 
		     *tree-font* 
		     cx (get tree 'y)
		     (get tree 'label)
		     color)
    (dolist (dtr (user::find-children tree))
      (gd:image-line image (get tree 'x) 
		     (+ *tree-font-height*
			(get tree 'y))
		     (get dtr 'x) (get dtr 'y)
		     color)
      (draw-tree dtr image color))))

;;; Draw a nested list of nodes in a GIF file, and return the width
;;; and height of the image

(defun draw-parse-www (tree stream)
  (let ((user::*dont-show-morphology* t))
    (layout-tree tree)
    (let* ((width (+ (get tree 'width) (* *tree-hmargin* 2)))
	   (height (+ (get tree 'height) (* *tree-hmargin* 2)))
	   (image (gd:image-create width height))
	   (white (gd:image-color-allocate image 255 255 255))
	   (black (gd:image-color-allocate image 0 0 0)))
      (gd:image-color-transparent image white)
      (draw-tree tree image black)
      (gd::image-gif image stream)
      (gd:image-destroy image))))

  
  
