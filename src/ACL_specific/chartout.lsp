;;; Copyright John Carroll 1998 All Rights Reserved.
;;; No use or redistribution without permission.
;;; 

(in-package :user)

;;; (show-chart)

(defun show-chart nil 
  (unwind-protect
      (let ((root (make-symbol "")))
	(setf (get root 'chart-edge-descendents)
	  (make-array *chart-limit* :initial-element nil))
	(let*
            ((end (create-chart-pointers root))
             (word-alt-sets
	      ;; each element is a set to allow for multi-word lexical
	      ;; entries at each position in input
	      (coerce (subseq (get root 'chart-edge-descendents) 0 end) 
		      'list)))
	  (setf (get root 'chart-edge-descendents) 
	    (apply #'append word-alt-sets))
	  (draw-chart-lattice root
			      (format nil "Chart for \"~A...\"" 
				      (car (get root 'chart-edge-descendents)))
			      t)
	  root))))


(defun create-chart-pointers (root)
  (dotimes (vertex (1- *chart-limit*))
    (if (aref *chart* (1+ vertex)) 
	(dolist (span (chart-entry-configurations (aref *chart* (1+ vertex))))
	  (create-chart-pointers1 span (1+ vertex) root))
      (return vertex))))

(defun create-chart-pointers1 (span right-vertex root)
  (let*
      ((edge (chart-configuration-edge span))
       (edge-id (make-edge-symbol (edge-id edge))))
    (setf (get edge-id 'chart-edge-span)
      (format nil "~A-~A" (chart-configuration-begin span) right-vertex))
    (if (edge-children edge)
	(dolist (child (edge-children edge))
	  (push edge-id
		(get (make-edge-symbol (edge-id child)) 
		     'chart-edge-descendents)))
      (progn
	(dolist (lex (edge-leaves edge))
	  (push edge-id (get (intern lex) 'chart-edge-descendents)))
	(pushnew (intern (car (last (edge-leaves edge))))
		 (aref (get root 'chart-edge-descendents) (1- right-vertex)))))
    (setf (get edge-id 'chart-edge-contents) edge)))


(defun clear-chart-pointers ()
  (dotimes (vertex (1- *chart-limit*))
    (if (aref *chart* (1+ vertex)) 
	(dolist (span (chart-entry-configurations (aref *chart* (1+ vertex))))
	  (let ((edge (chart-configuration-edge span)))
	    (if (edge-children edge)
		(dolist (child (edge-children edge))
		  (setf (get (make-edge-symbol (edge-id child)) 
			     'chart-edge-descendents)
		    nil))
	      (dolist (lex (edge-leaves edge))
		(setf (get (intern lex) 'chart-edge-descendents) nil)))
	    (setf (get (make-edge-symbol (edge-id edge)) 'chart-edge-contents)
	      nil)))
      (return nil))))

;;; dialect specific from this point

(defvar *chart-frame* nil)
(defvar *chart-selected* nil)

(clim:define-application-frame parse-chart ()
  ((root :initform nil
	 :accessor chart-window-root))
  (:panes
   (display  
    (clim:outlining (:thickness 1)
      (clim:spacing (:thickness 1)  
	(clim:scrolling (:scroll-bars :both)
	  (clim:make-pane 'clim:application-pane
			  :display-function 'draw-chart-window
			  :text-cursor nil
			  :width :compute
			  :height :compute
			  :text-style *ptree-text-style*
			  :end-of-line-action :allow
			  :end-of-page-action :allow
			  :borders nil
			  :background clim:+white+
			  :foreground clim:+black+
			  :display-time nil))))))
  (:layouts
    (:default display)))

(define-parse-chart-command (com-exit-chart-window :menu "Close")
    ()
  (clear-chart-pointers)
  (setq *chart-frame* nil)
  (unhighlight-edge)
  (clim:frame-exit clim:*application-frame*))

;;; use *parse-tree-font-size* from globals.lsp

(defparameter *chart-font* (list "Helvetica" (or *parse-tree-font-size* 9)))
(defparameter *chart-bold-font* (list "Helvetica" 
				      (or *parse-tree-font-size* 9) :bold))


(defun draw-chart-lattice (node title horizontalp)
  (declare (ignore horizontalp))
  (cond (*chart-frame*
	 (clim:enable-frame *chart-frame*)
	 (clim:raise-frame *chart-frame*))
	(t (let ((chart-window 
		  (clim:make-application-frame 'parse-chart)))
	     (setf (chart-window-root chart-window) node)
	     (setf (clim:frame-pretty-name chart-window) title)
	     (setf *chart-frame* chart-window)
	     (setf *chart-selected* nil)
	     (mp:process-run-function "CHART" 
				      #'clim:run-frame-top-level
				      chart-window)))))


(defun draw-chart-window (window stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (clim:format-graph-from-root
   (chart-window-root window)
   #'(lambda (node stream)
       (multiple-value-bind (s bold-p)
	   (chart-node-text-string node)
	 (clim:with-text-face (stream (if bold-p :bold :roman))
	   (let ((cont (get node 'chart-edge-contents)))
	     (if cont
		 (clim:with-output-as-presentation 
		     (stream cont 'edge)
		   (write-string s stream))
	       (write-string s stream))))))
   #'(lambda (node) 
       (get node 'chart-edge-descendents))
   :stream stream 
   :graph-type :dag
   :merge-duplicates t
   :orientation :horizontal
   :generation-separation *tree-level-sep*
   :within-generation-separation *tree-node-sep*
   :center-nodes nil))

(defun chart-node-text-string (x)
   (let ((edge-record (get x 'chart-edge-contents)))
      (if edge-record
         (values
            (format nil "~A [~A] ~A"
               (get x 'chart-edge-span)
               (edge-id edge-record)
               (tree-node-text-string (edge-rule-number edge-record)))
            nil)
         (values (tree-node-text-string x) t))))

(define-parse-chart-command (com-edge-menu)
    ((edge-rec 'edge :gesture :select)) 
  (when (edge-p edge-rec)
    (unhighlight-edge)
    (let ((command (clim:menu-choose
		    (append '(("Feature structure" :value fs))
			    `((,(format nil "Edge ~A" (edge-id edge-rec))
			       :value edge))
			    (unless (stringp (edge-rule-number edge-rec))
			      `((,(format nil "Rule ~A" 
					  (or (edge-rule-number edge-rec) ""))
				 :value rule)))))))
      (when command
	(handler-case
            (ecase command
	      (fs (display-fs (edge-dag edge-rec)
			      (format nil "Edge ~A" (edge-id edge-rec))))
	      (edge (display-parse-tree edge-rec nil))
	      (rule (let* ((rule-name (edge-rule-number edge-rec))
			   (rule (or (get-grammar-rule-entry rule-name)
				     (get-lex-rule-entry rule-name))))
		      (when rule
			(display-fs (rule-full-fs rule)
				    (format nil "~A" rule-name))))))
	  (error (condition)
	    (format clim-user:*lkb-top-stream*  
		    "~%Error: ~A~%" condition)))))))

;;; called from display-parse-tree - when it is called to display an edge find
;;; topmost chart window on screen, and ask for type hierarchy window to be
;;; scrolled so given edge is visible in center, and the edge highlighted

(defun display-edge-in-chart (edge)
  (when *chart-frame*
    (clim:enable-frame *chart-frame*)
    (clim:raise-frame *chart-frame*)
    (let* ((stream (clim:frame-standard-output *chart-frame*))
	   (record (find-edge stream (edge-id edge))))
      (when record
	(scroll-to record stream)
	(highlight-edge record stream)))))

;;; Center the viewport on object

(defun scroll-to (record stream)
  (let* ((vp-width (clim:bounding-rectangle-width 
		    (clim:pane-viewport-region stream)))
         (vp-height (clim:bounding-rectangle-height
		     (clim:pane-viewport-region stream)))
	 (x-pos (clim:point-x (clim:bounding-rectangle-center 
			       (clim:output-record-parent record))))
	 (y-pos (clim:point-y (clim:bounding-rectangle-center 
			       (clim:output-record-parent record))))
	 (x-max (clim:bounding-rectangle-max-x stream))
	 (y-max (clim:bounding-rectangle-max-y stream)))
    (clim:scroll-extent stream
			(max 0 (min (- x-pos (floor vp-width 2))
				    (- x-max vp-width)))
			(max 0 (min (- y-pos (floor vp-height 2))
				    (- y-max vp-height))))))

;;; Search the display list for an edge with the right id

(defun find-edge (stream id)
  (catch 'edge
    (find-edge-1 (slot-value stream 'clim:output-record) stream id)))

(defun find-edge-1 (rec stream id)
  (clim:map-over-output-records 
   #'(lambda (rec)
       (when (clim:presentationp rec) 
	 (if (eql id (edge-id (clim:presentation-object rec)))
	     (throw 'edge rec)))
       (dolist (q (clim:output-record-children rec)) 
	 (find-edge-1 q stream id)))
   rec))

;;; Show a highlighted edge - this is still not very reliable.  The
;;; highlighting often gets cleared by some internal CLIM action without us
;;; knowing about it, so then the when we try to unhighlight the selection we
;;; end up re-highlighting it.  It might be better to not use the CLIM
;;; highlighting mechanism at all.

(defun highlight-edge (edge stream)
  (let ((*standard-output* stream))
    (unhighlight-edge))
  (setq *chart-selected* edge)
  (clim:highlight-output-record edge stream :highlight))

(defun unhighlight-edge ()
  (when *chart-selected*
    (clim:highlight-output-record *chart-selected* *standard-output* 
				  :unhighlight)    
    (setf *chart-selected* nil)))