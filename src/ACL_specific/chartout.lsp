;;; Copyright (c) 1997-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions

(in-package :lkb)

;;; want to make sure that a chart window is not left open after
;;; a new sentence is parsed, since the interactions sometimes
;;; get confused if there's a window which corresponds to an 
;;; old parse
;;; uses globals *main-chart-frame* and *sub-chart-window-frames*
;;; which are defined in parseout


(defun close-existing-chart-windows nil
  (invalidate-chart-commands)
  (when *main-chart-frame*
    (clim:execute-frame-command *main-chart-frame* '(clim-user::com-close-to-replace)))
  (loop for frame in *sub-chart-window-frames*
       do
       (clim:execute-frame-command frame '(clim-user::com-close-to-replace)))
  (setf *main-chart-frame* nil)
  (setf *sub-chart-window-frames* nil))

(define-lkb-frame chart-window 
  ((root :initform nil
	 :accessor chart-window-root)
   (edges :initform nil
	  :accessor chart-window-edges)
   (selected-words :initform nil
		   :accessor chart-window-words)
   (selected-edge :initform nil
		  :accessor chart-window-selected-edge))	       
  :info-bar t
  :display-function 'draw-chart-window
  :width :compute 
  :height :compute
  :text-style (lkb-parse-tree-font))

(clim:define-presentation-type word ())

(defun draw-chart-lattice (node title &key (horizontalp t) (subframe-p nil))
  (declare (ignore horizontalp))
  (when (and *main-chart-frame* (not subframe-p))
    (clim:execute-frame-command *main-chart-frame* 
				'(clim-user::com-close-to-replace)))
  ;; Initialize fonts
  (setq *parse-tree-font* (clim:parse-text-style (lkb-parse-tree-font)))
  (mp:run-function "CHART" #'draw-chart-lattice-really
                   node title subframe-p))
                           
(defun draw-chart-lattice-really (node title subframe-p)                    
  (let* ((chart-window (clim:make-application-frame 'chart-window))
	 (yield-pane 
	  (find :path (clim:frame-current-panes chart-window)
		:test #'eq :key #'clim:pane-name)))
    ;; Set up yield display
    (setf (lkb-window-doc-pane chart-window) yield-pane)
    #+:allegro
    (clim:change-space-requirements 
     yield-pane
     :resize-frame t
     :height (clim:text-style-height *parse-tree-font* yield-pane)
     :max-height (clim:text-style-height *parse-tree-font* yield-pane))
    ;;
    (if subframe-p
        (push chart-window
              *sub-chart-window-frames*)
      (setf *main-chart-frame* chart-window))
    (setf (chart-window-root chart-window) node)
    (setf (clim:frame-pretty-name chart-window) title)
    (clim:run-frame-top-level chart-window)))


(defun draw-chart-window (window stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let ((*chart-edges* nil))
    (declare (special *chart-edges*))
    ;; Don't bother if there's no chart
    (unless (null (get (chart-window-root window) 'chart-edge-descendents))
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
		   (clim:with-output-as-presentation 
		       (stream (symbol-name node) 'word)
		     (write-string s stream)))))))
       #'(lambda (node) 
           (get node 'chart-edge-descendents))
       ;; This trickery is to avoid drawing the connections from the dummy
       ;; root node to the lexical edges
       :arc-drawer #'(lambda (stream from to x1 y1 x2 y2 &rest args)
                       (when (or (not (symbolp to))
                                 (not (get from 'root)))
			 (apply #'clim-internals::draw-linear-arc
			        (append (list stream from to x1 y1 x2 y2)
			                args))))   
       :stream stream 
       :graph-type :dag
       :merge-duplicates t
       :orientation :horizontal
       :maximize-generations t
       :generation-separation *tree-level-sep*
       :within-generation-separation *tree-node-sep*
       :center-nodes nil)
      (setf (chart-window-edges window) *chart-edges*))))

(defun chart-node-text-string (node)
  (let ((edge (get node 'chart-edge-contents)))
    (if edge
	(let ((rule (edge-rule edge)))
	  (values
	   (format nil "~@[~A ~][~A] ~A"
		   (get node 'chart-edge-span)
		   (edge-id edge)
		   (tree-node-text-string (cond ((rule-p rule) (rule-id rule))
						((g-edge-p edge) rule)
						(t (edge-category edge)))))
	   nil))
      (values (tree-node-text-string node) t))))

;; Update the yield window when we are over an edge

(define-info-bar edge (edge-record stream)
  (let ((yield (when edge-record (edge-leaves edge-record))))
    (when yield
      (clim:with-text-style (stream *parse-tree-font*)
	(dolist (word yield)
	  (write-string (string-downcase word) stream)
	  (write-char #\space stream))))))

;; Click on background to clear selection

(define-chart-window-command (com-background-menu)
    ((obj 'clim:blank-area :gesture :select)) 
  (declare (ignore obj))
  (clim:with-application-frame (frame)
    (setf (chart-window-words frame) nil)
    (setf (chart-window-selected-edge frame) nil)
    (unhighlight-objects frame)))

;; Click on word to add to highlighted words

(define-chart-window-command (com-word-menu)
    ((node 'word :gesture :select)) 
  (clim:with-application-frame (frame)
    (with-slots (selected-words) frame
      (setf (chart-window-selected-edge frame) nil)
      (unhighlight-objects frame)
      (if (member node selected-words :test #'equal)
	  (setf selected-words (delete node selected-words :test #'equal))
	(push node selected-words))
      (highlight-words frame))))

;; Highlight selected words and all edges which cover selected words

(defun highlight-words (frame)
  (let* ((stream (clim:frame-standard-output frame))
	 (words (chart-window-words frame))
	 (objects (nconc
		   (loop for edge in (chart-window-edges frame)
		       when (and (subsetp words (edge-leaves edge) 
					  :test #'equal)
				 (subsetp (edge-leaves edge) words 
					  :test #'equal))
		       collect (find-object stream 
					    #'(lambda (x) (eq x edge))))
		   (loop for word in words
		       collect (find-object stream 
					    #'(lambda (x) (equal x word)))))))
    (highlight-objects objects frame)))
		      
;; Pop-up menu for edges

(define-chart-window-command (com-edge-menu)
    ((edge-rec 'edge :gesture :select)) 
  (clim:with-application-frame (frame)
    (when (edge-p edge-rec)
      (pop-up-menu
       (append '(("Highlight nodes" :value highlight))
	       '(("Feature structure" :value fs))
	       (when (rule-p (edge-rule edge-rec))
		 `((,(format nil "Rule ~A" 
			     (rule-id (edge-rule edge-rec)))
		    :value rule)))
	       '(("New chart" :value new))
	       `((,(format nil "Tree ~A" (edge-id edge-rec))
		  :value edge))
	       `(("Compare" :value compare
			    :active ,(chart-window-selected-edge frame)))
	       `(("Unify" :value unify
			  :active ,*fs1*)))
       (fs (display-fs (edge-dag edge-rec)
		       (format nil "Edge ~A ~A - FS" 
			       (edge-id edge-rec)
			       (if (g-edge-p edge-rec) 
				   "G" 
				 "P"))))
       (edge (display-parse-tree edge-rec nil))
       (rule (let* ((item (edge-rule edge-rec))
		    (rule (and (rule-p item) item)))
	       (when rule
		 (display-fs (rule-full-fs rule)
			     (format nil "~A" (rule-id rule))))))
       (highlight (setf (chart-window-selected-edge frame) edge-rec)
		  (highlight-edge edge-rec frame))
       (new (display-edge-in-new-window frame edge-rec))
       (compare (compare (list (chart-window-selected-edge frame) edge-rec)))
       (unify (try-unify-fs-in-chart (edge-dag edge-rec)))))))

(defun try-unify-fs-in-chart (fs)
  ;;; very similar to the function in activefs
  (let* ((fs1 *fs1*)
         (path1 *path1*)
         (result nil))
    (when (and fs1 (listp path1))
      (with-output-to-top ()
        (setf result
          (unify-paths-with-fail-messages 
           (create-path-from-feature-list path1)
           fs1
           (create-path-from-feature-list nil)
           (tdfs-indef fs)
           :selected1 path1 :selected2 nil))
        (terpri))
      (when result
        (display-fs result "Unification result")))
    (setq *fs1* nil)))            
             
;; called from display-parse-tree - when it is called to display an edge find
;; topmost chart window on screen, and ask for chart window to be
;; scrolled so given edge is visible in center, and the edge highlighted

(defun display-edge-in-chart (edge)
  (let ((frame (reuse-frame 'chart-window)))
    (when frame
      (highlight-edge edge frame :scroll-to t))))

(defun highlight-edge (edge frame &key (scroll-to nil))
  (let* ((stream (clim:frame-standard-output frame))
	 (record (find-object stream #'(lambda (e)
                                         (and (edge-p e)
                                              (eql (edge-id e)
                                                   (edge-id edge))))))
	 (on-path (append (cdr (collect-subs edge stream))
			  (collect-supers edge frame stream))))
    (setf (chart-window-words frame) nil)
    (cond (record
	   (when scroll-to 
	     (scroll-to record stream))
	   (highlight-objects-mark (cons record on-path) frame))
	  (t (highlight-objects on-path frame)))))

(defun collect-subs (edge stream)
  (when edge
    (let ((record (find-object stream #'(lambda (e) 
                                          (and (edge-p e)
                                               (eql (edge-id e)
                                                    (edge-id edge)))))))
      (append (when record 
		(list record))
	      (if (edge-morph-history edge)
		  (collect-subs (edge-morph-history edge) stream)
		(mapcan #'(lambda (x) (collect-subs x stream))
			(edge-children edge)))))))

(defun collect-supers (edge frame stream)
  (when (edge-p edge)
  (labels
      ((highlight-chart-edge-path-p (e)
         ;; path from e recursively through children to edge?
         (and (edge-p e)
              (or (eq e edge)
                  (eq (edge-morph-history e) edge)
                  (some #'highlight-chart-edge-path-p (edge-children e))))))
    (loop for record in (chart-window-edges frame)
	appending
	  (when (and (not (eq edge record))
		     (highlight-chart-edge-path-p record))
	    (list
	     (find-object stream #'(lambda (e) 
                                     (and (edge-p e)
                                          (eql (edge-id e)
                                               (edge-id record)))))))))))

;;; create a new chart window and display just the descendents and ancestors
;;; of the edge in it

(defun display-edge-in-new-window (parent-frame edge)
  (if edge
      (progn 
	(draw-chart-lattice
	 (filtered-chart-lattice (chart-window-root parent-frame) edge nil)
	 (string (gentemp (format nil "~A-" 
				  (clim:frame-pretty-name parent-frame))))
	 :subframe-p t))
    (lkb-beep)))
