
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
  (setq *chart-frame* nil)
  (unhighlight-edges *standard-output*)
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
   ;; This trickery is to avoid drawing the connections from the dummy
   ;; root node to the lexical edges
   :arc-drawer #'(lambda (stream from to x1 y1 x2 y2 &rest args)
		   (when (or (not (symbolp to))
			     (get to 'chart-edge-contents))
		     (apply #'clim-internals::draw-linear-arc
			    (append (list stream from to x1 y1 x2 y2)
				    args))))   
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
    (unhighlight-edges *standard-output*)
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
			      (format nil "Edge ~A ~A - FS" 
				      (edge-id edge-rec)
				      (if (gen-chart-edge-p edge-rec) 
					  "G" 
					"P"))))
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
	   (records (collect-records edge stream)))
      (when records
	(scroll-to (car records) stream))
      (highlight-edges records stream))))

(defun collect-records (edge stream)
  (when edge
    (let ((record (find-edge stream (edge-id edge))))
      (append (when record 
		(list record))
	      (mapcan #'(lambda (x) (collect-records x stream))
		      (edge-children edge))))))

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

;;; Show a list of highlighted edges

(defun highlight-edges (edges stream)
  (unhighlight-edges stream)
  (setq *chart-selected* 
    (clim:with-new-output-record (stream)
      (clim:with-output-recording-options (stream :record t)
	(dolist (edge edges)
	  (when edge
	    (multiple-value-bind (x1 y1 x2 y2)
		(clim:bounding-rectangle* 
		 (clim:output-record-parent edge))
	      (clim:draw-rectangle* stream x1 y1 x2 y2 :
				    ink clim:+flipping-ink+ :filled t))))))))

(defun unhighlight-edges (stream)
  (when *chart-selected*
    (clim:erase-output-record *chart-selected* stream)))

;;; -----------------------------------------------------------------
;;; Draw chart as a shared forest

(clim:define-application-frame parse-forest ()
  ((root :initform nil
	 :accessor chart-window-root))
  (:panes
   (display  
    (clim:outlining (:thickness 1)
      (clim:spacing (:thickness 1)  
	(clim:scrolling (:scroll-bars :both)
	  (clim:make-pane 'clim:application-pane
			  :display-function 'draw-forest-window
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

(define-parse-forest-command (com-exit-forest-window :menu "Close")
    ()
  (clim:frame-exit clim:*application-frame*))

(defstruct node label daughters)

(defvar *nodes* nil)

(defun show-forest nil 
  (let* ((*nodes* nil)
	 (tree (mapcar #'make-forest *parse-record*)))
    (draw-chart-forest tree "Parse forest" t)))

(defun make-forest (parse)
  (if (assoc parse *nodes* :test #'eq)
      (cdr (assoc parse *nodes* :test #'eq))
    (let ((node (make-node)))
      (setf (node-label node) 
	(find-category-abb (edge-dag parse)))
      (setf (node-daughters node) 
	(mapcar #'make-forest (edge-children parse)))
      (push (cons parse node) *nodes*)
      node)))


(defun draw-chart-forest (node title horizontalp)
  (declare (ignore horizontalp))
  (let ((chart-window 
	 (clim:make-application-frame 'parse-forest)))
    (setf (chart-window-root chart-window) node)
    (setf (clim:frame-pretty-name chart-window) title)
    (mp:process-run-function "CHART" 
			     #'clim:run-frame-top-level
			     chart-window)))

(defun draw-forest-window (window stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (clim:format-graph-from-roots
   (chart-window-root window)
   #'(lambda (node stream)
       (write-string (node-label node) stream))
   #'node-daughters
   :stream stream 
   :graph-type :dag
   :merge-duplicates t
   :orientation :vertical
   :generation-separation (* 2 *ptree-node-sep*)
   :within-generation-separation (* 2 *ptree-level-sep*)
   :center-nodes nil))
