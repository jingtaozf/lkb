
;;; dialect specific from this point

(in-package :user)

(define-lkb-frame chart-window 
  ((root :initform nil
	 :accessor chart-window-root)
   (edges :initform nil
	  :accessor chart-window-edges))
  :display-function 'draw-chart-window
  :width :compute 
  :height :compute
  :text-style *ptree-text-style*)

;;; use *parse-tree-font-size* from globals.lsp

(defparameter *chart-font* (list "Helvetica" (or *parse-tree-font-size* 9)))
(defparameter *chart-bold-font* (list "Helvetica" 
				      (or *parse-tree-font-size* 9) :bold))


(defun draw-chart-lattice (node title horizontalp)
  (declare (ignore horizontalp))
  (let ((chart-window (clim:make-application-frame 'chart-window)))
    (setf (chart-window-root chart-window) node)
    (setf (clim:frame-pretty-name chart-window) title)
    (mp:process-run-function "CHART" 
			     #'clim:run-frame-top-level
			     chart-window)))

(defun draw-chart-window (window stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let ((*chart-edges* nil))
    (declare (special *chart-edges*))
    (clim:format-graph-from-root
     (chart-window-root window) 
     #'(lambda (node stream)
	 (multiple-value-bind (s bold-p)
	     (chart-node-text-string node)
	   (clim:with-text-face (stream (if bold-p :bold :roman))
	     (let ((cont (get node 'chart-edge-contents)))
	       (if cont
		   (progn
		     (push cont *chart-edges*)
		     (clim:with-output-as-presentation 
			 (stream cont 'edge)
		       (write-string s stream)))
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
     :center-nodes nil)
    (setf (chart-window-edges window) *chart-edges*)))

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

(define-chart-window-command (com-edge-menu)
    ((edge-rec 'edge :gesture :select)) 
  (unhighlight-objects clim:*application-frame*)
  (when (edge-p edge-rec)
    (pop-up-menu
     (append '(("Feature structure" :value fs))
	     `((,(format nil "Edge ~A" (edge-id edge-rec))
		:value edge))
	     (unless (stringp (edge-rule-number edge-rec))
	       `((,(format nil "Rule ~A" 
			   (or (edge-rule-number edge-rec) ""))
		  :value rule)))
	     '(("Highlight nodes" :value highlight))
	     '(("New chart" :value new))) 
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
			   (format nil "~A" rule-name)))))
     (highlight (display-edge-in-chart edge-rec))
     (new (display-edge-in-new-window clim:*application-frame* edge-rec)))))

;; called from display-parse-tree - when it is called to display an edge find
;; topmost chart window on screen, and ask for type hierarchy window to be
;; scrolled so given edge is visible in center, and the edge highlighted

(defun display-edge-in-chart (edge)
  (let ((frame (reuse-frame 'chart-window)))
    (when frame
      (let* ((stream (clim:frame-standard-output frame))
	     (records (append (collect-subs edge stream)
			      (collect-supers edge frame stream))))
	(when records
	  (scroll-to (car records) stream))
	(highlight-objects-mark records frame)))))

(defun collect-subs (edge stream)
  (when edge
    (let ((record (find-object stream #'(lambda (e) (eql (edge-id e)
							 (edge-id edge))))))
      (append (when record 
		(list record))
	      (mapcan #'(lambda (x) (collect-subs x stream))
		      (edge-children edge))))))

(defun collect-supers (edge frame stream)
  (labels
      ((highlight-chart-edge-path-p (e)
	 ;; path from e recursively through children to edge?
	 (or (eq e edge)
	     (some #'highlight-chart-edge-path-p (edge-children e)))))
    (loop for record in (chart-window-edges frame)
	appending
	  (when (and (not (eq edge record))
		     (highlight-chart-edge-path-p record))
	    (list
	     (find-object stream #'(lambda (e) (eql (edge-id e)
						    (edge-id record)))))))))

;;; create a new chart window and display just the descendents and ancestors
;;; of the edge in it

(defun display-edge-in-new-window (frame edge)
  (if edge
      (draw-chart-lattice
       (filtered-chart-lattice (chart-window-root frame) edge nil)
       (string (gentemp (format nil "~A-" (clim:frame-pretty-name frame))))
       t)
    (lkb-beep)))

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

(defstruct fnode label daughters)

(defvar *nodes* nil)

(defun show-forest nil 
  (let* ((*nodes* nil)
	 (tree (mapcar #'make-forest *parse-record*)))
    (draw-chart-forest tree "Parse forest" t)))

(defun make-forest (parse)
  (if (assoc parse *nodes* :test #'eq)
      (cdr (assoc parse *nodes* :test #'eq))
    (let ((node (make-fnode)))
      (setf (fnode-label node) 
	(find-category-abb (edge-dag parse)))
      (setf (fnode-daughters node) 
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
       (write-string (string (fnode-label node)) stream))
   #'fnode-daughters
   :stream stream 
   :graph-type :dag
   :merge-duplicates t
   :orientation :vertical
   :generation-separation (* 2 *ptree-node-sep*)
   :within-generation-separation (* 2 *ptree-level-sep*)
   :center-nodes nil))
