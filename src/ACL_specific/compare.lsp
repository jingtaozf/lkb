;;;
;;; Tools to help build a tree bank
;;;

(in-package :user)

;;; **********************************************************************
;;; Main entry point

(defun compare-parses nil
  (when *parse-record*
    (compare *parse-record*)))

(defun compare (parses)
  (let ((compare-frame
	 (clim:make-application-frame 'compare-frame)))
    (setf (compare-frame-parses compare-frame) parses)
    (setf (compare-frame-discrs compare-frame) (find-discriminants parses))
    (dolist (p parses)
      (push (make-ptree :top (make-new-parse-tree p 1) :parse p)
	    (compare-frame-trees compare-frame)))
    (recompute-in-and-out compare-frame)
    (setf (clim:frame-pretty-name compare-frame)
      (format nil "~a" (edge-leaves (car parses))))
    (mp:process-run-function "Compare" 
			     #'clim:run-frame-top-level
			     compare-frame)))

;;; **********************************************************************
;;; Collect differences among a set of parses

(defstruct discr key value in out toggle type)

(defvar *discrs*)

(defun find-discriminants (parses)
  (let ((*discrs* nil))
    ;; Collect all constituents
    (dolist (parse parses)
      (find-discriminants-in-parse parse parse))
    ;; Filter out discriminants that are implied by other longer discriminants
    (setq *discrs*
      (delete-if #'(lambda (x) 
		     (some #'(lambda (y)
			       (and (equal (discr-in x)
					   (discr-in y))
				    (eq (discr-type x) :constituent)
				    (eq (discr-type y) :constituent)
				    (> (length (discr-value y))
				       (length (discr-value x)))))
			   *discrs*))
		 *discrs*))
    ;; Filter out discriminants that don't rule out any parses
    (setq *discrs*
      (delete-if #'(lambda (x) (= (length (discr-in x ))
				  (length parses)))
		 *discrs*))
    ;; Compute out from in
    (dolist (x *discrs*)
      (setf (discr-out x) (set-difference parses (discr-in x))))
    ;; Sort in order of yield, with semantic discriminants at end
    (sort *discrs* #'> :key #'(lambda (n) 
				(if (stringp (discr-value n))
				    (length (discr-value n))
				  0)))))

(defun find-discriminants-in-parse (parse top)
  (let ((label (find-category-abb (edge-dag parse)))
	(yield (apply #'concatenate 
		      (append '(string) 
			      (mapcan #'(lambda (x) 
					  (list x " ")) 
				      (edge-leaves parse))))))
    (add-discriminant label yield :constituent top))
  (unless (edge-children parse)
    (let ((word (car (edge-leaves parse)))
	  (type (type-of-fs (tdfs-indef (edge-dag parse))))
	  (rel (type-of-fs (existing-dag-at-end-of 
			    (tdfs-indef (edge-dag parse)) 
			    '(synsem local cont key)))))
      (add-discriminant word type :type top)
      (add-discriminant word rel :rel top)))
  (dolist (child (edge-children parse))
    (find-discriminants-in-parse child top)))

(defun add-discriminant (key value type top)
  (let ((old (member (list key value)
		     *discrs*
		     :test #'(lambda (x y)
			       (equal x
				      (list (discr-key y)
					    (discr-value y)))))))
    (if old
	(pushnew top (discr-in (car old)))
      (push (make-discr :key key
			:value value
			:in (list top)
			:out nil
			:type type
			:toggle :unknown)
	    *discrs*))))

;;; **********************************************************************
;;; Application frame for comparison window

(clim:define-application-frame compare-frame ()
  ((parses :initform nil
	   :accessor compare-frame-parses)
   (discrs :initform nil
	   :accessor compare-frame-discrs)
   (trees :initform nil
	  :accessor compare-frame-trees)
   (in :initform nil
       :accessor compare-frame-in-parses)
   (out :initform nil
	:accessor compare-frame-out-parses)
   (trees-stream :initform nil
		:accessor compare-frame-trees-stream))
  (:panes
   (trees  
    (clim:outlining (:thickness 1)
      (clim:spacing (:thickness 1)  
	(clim:scrolling (:scroll-bars :both)
	  (clim:make-pane 'clim:application-pane
			  :display-function 'draw-trees-window
			  :text-cursor nil
			  :width 400
			  ;; :height 100
			  :text-style (clim:parse-text-style 
				       (list :sans-serif :roman 7))
			  :end-of-line-action :allow
			  :end-of-page-action :allow
			  :borders nil
			  ;; :incremental-redisplay '(t :check-overlapping nil)
			  :display-time nil
			  :background clim:+white+
			  :foreground clim:+black+)))))
   (display  
    (clim:outlining (:thickness 1)
      (clim:spacing (:thickness 1)  
	(clim:scrolling (:scroll-bars :both)
	  (clim:make-pane 'clim:application-pane
			  :display-function 'draw-compare-window
			  :text-cursor nil
			  :width 400
			  :height 600
			  :text-style (clim:parse-text-style 
				       (list :sans-serif :roman 12))
			  :end-of-line-action :allow
			  :end-of-page-action :allow
			  :borders nil
			  :incremental-redisplay t
			  :background clim:+white+
			  :foreground clim:+black+))))))
  (:layouts
   (:default (clim:horizontally () trees display))))

(define-compare-frame-command (com-exit-compare-frame :menu "Quit")
    ()
  (clim:frame-exit clim:*application-frame*))

(define-compare-frame-command (com-clear-compare-frame :menu "Clear")
    ()
  (clim:with-application-frame (frame)
    (setf (compare-frame-in-parses frame) (compare-frame-parses frame))
    (setf (compare-frame-out-parses frame) nil)
    (dolist (d (compare-frame-discrs frame))
      (setf (discr-toggle d) :unknown))
    (recompute-in-and-out frame)
    (update-trees frame)))

(define-compare-frame-command (com-done-compare-frame :menu "Done")
    ()
  (clim:frame-exit clim:*application-frame*))


;;; **********************************************************************
;;; Stuff for mini tree pane

(defstruct ptree 
  ;; Top node of parse tree
  top					
  ;; Top edge of parse
  parse					
  ;; Output record of tree
  output-record				
  ;; Current color of tree
  ink)
  
(defun draw-trees-window (window stream)
  (setf (compare-frame-trees-stream window) stream)
  (dolist (tree (compare-frame-trees window))
    (setf (ptree-ink tree) clim:+foreground-ink+)
    (setf (ptree-output-record tree)
      (clim:with-new-output-record (stream)
	(clim:with-output-recording-options (stream :record t)
	  (clim:with-output-as-presentation 
	      (stream tree 'ptree :single-box t)
	    (clim:format-graph-from-root
	     (ptree-top tree)
	     #'(lambda (node stream)
		 (multiple-value-bind (s bold-p) 
		     (get-string-for-edge node)
		   (clim:with-text-face (stream (if bold-p :bold :roman))
		     (write-string s stream))))
	     #'(lambda (node) (get node 'daughters))
	     :graph-type :parse-tree
	     :stream stream 
	     :merge-duplicates nil
	     :orientation :vertical
	     :generation-separation 7
	     :move-cursor t
	     :within-generation-separation 7
	     :center-nodes nil)))
	(terpri stream))))
  (update-trees window))

(define-compare-frame-command (com-tree-menu)
    ((tree 'ptree :gesture :select))
  (let ((command (clim:menu-choose
		  '(("Select" :value select :active t)
		    ("Show tree" :value show)))))
    (when command
      (handler-case
	  (ecase command
	    (select 
	     (clim:with-application-frame (frame)
	       (dolist (d (compare-frame-discrs frame))
		 (setf (discr-toggle d) :unknown))
	       (setf (compare-frame-in-parses frame) (list (ptree-parse tree)))
	       (setf (compare-frame-out-parses frame) 
		 (remove (ptree-parse tree) 
			 (compare-frame-parses frame) 
			 :test #'eq))
	       (update-trees frame)))
	    (show (draw-new-parse-tree (ptree-top tree)
				       "Parse tree" nil)))
	(error (condition) 
	  (declare (ignore condition) )
	  nil)))))

(defun update-trees (window)
  (let ((done-p nil)
	(stream (compare-frame-trees-stream window)))
    (dolist (tree (compare-frame-trees window))
      (let ((ink (cond ((member (ptree-parse tree) 
				(compare-frame-out-parses window)
				:test #'eq)
			clim:+red+)
		       ((and (not (cdr (compare-frame-in-parses window)))
			     (eq (car (compare-frame-in-parses window))
				 (ptree-parse tree)))
			clim:+green+)
		       (t clim:+foreground-ink+))))
	(when (eq ink clim:+green+)
	  (setq done-p t))
	(unless (eq ink (ptree-ink tree))
	  (setf (ptree-ink tree) ink)
	  (recolor-tree (ptree-output-record tree) ink)
	  (clim:replay (ptree-output-record tree) stream))))
    (setf (clim:command-enabled 'com-done-compare-frame window) done-p)))

(defun recolor-tree (record ink)
  (labels ((recolor-node (node) 
	     (when (clim:displayed-output-record-p node)
	       (setf (clim:displayed-output-record-ink node) ink))
	     (clim:map-over-output-records #'recolor-node node)))
    (declare (dynamic-extent recolor-node))
    (recolor-node record)))
  
  
;;; **********************************************************************
;;; Stuff for constituent pane

(defun draw-compare-window (window stream)
  (let ((discrs (compare-frame-discrs window)))
    (clim:updating-output (stream :cache-value t)
      (format stream "~a~%~%" 
	      (edge-leaves (car (compare-frame-parses window)))))
    (clim:updating-output (stream) 
      (format stream "~a parse~:p in, ~a parse~:p out~%~%" 
	      (length (compare-frame-in-parses window))
	      (length (compare-frame-out-parses window))))
    (clim:formatting-table (stream :x-spacing "XX")
      (dolist (d discrs)
	(clim:formatting-row (stream)
	  (clim:with-output-as-presentation
	      (stream d 'discr)
	    (clim:updating-output (stream :cache-value (discr-toggle d))
	      (clim:formatting-cell (stream :align-x :center)
		(write-string (cond ((eq (discr-toggle d) t)
				     "+")
				    ((null (discr-toggle d))
				     "-")
				    (t "?"))
			      stream)))
	    (clim:formatting-cell (stream :align-x :left)
	      (write-string (discr-key d) stream))
	    (clim:formatting-cell (stream :align-x :left)
	      (format stream "~A" (discr-value d)))))))))

(define-compare-frame-command (com-discr-menu)
    ((discr 'discr :gesture :select))
  (let ((command (clim:menu-choose
		  '(("Yes" :value yes)
		    ("No" :value no)
		    ("Unknown" :value unknown)
		    ("Show in parses" :value in)
		    ("Show out parses" :value out)))))
    (when command
      (handler-case
	  (ecase command
	    (yes (setf (discr-toggle discr) t))
	    (no (setf (discr-toggle discr) nil))
	    (unknown (setf (discr-toggle discr) :unknown))
	    (in (dolist (p (discr-in discr))
		  (display-parse-tree p nil)))
	    (out (dolist (p (discr-out discr))
		   (display-parse-tree p nil))))
	(error (condition) 
	  (declare (ignore condition) )
		   nil))
      (recompute-in-and-out clim:*application-frame*)
      (update-trees clim:*application-frame*))))

;; Apply inference rules from Carter (1997) until nothing changes

(defun recompute-in-and-out (frame)
  (setf (compare-frame-in-parses frame) (compare-frame-parses frame))
  (setf (compare-frame-out-parses frame) nil)
  (let ((done-p nil))
    (loop until done-p
	do
	  (setq done-p t)
	  (dolist (d (compare-frame-discrs frame))
	    (cond (;; R1
		   (null (discr-toggle d))
		   (mark-out (discr-in d) frame))
		  (;; R2
		   (eq (discr-toggle d) t)
		   (mark-out (discr-out d) frame))))
	  (setf (compare-frame-in-parses frame) 
	    (set-difference (compare-frame-parses frame)
			    (compare-frame-out-parses frame)
			    :test #'eq))
	  (dolist (d (compare-frame-discrs frame))
	    (cond (;; R3
		   (null (intersection (discr-in d) 
				       (compare-frame-in-parses frame)
				       :test #'eq))
		   (when (discr-toggle d)
		     (setf (discr-toggle d) nil)
		     (setq done-p nil)))
		  (;; R4
		   (subsetp (compare-frame-in-parses frame) 
			    (discr-in d)
			    :test #'eq)
		   (when (not (discr-toggle d))
		     (setf (discr-toggle d) t)
		     (setq done-p nil))))))))
	
(defun mark-out (parses frame)
  (dolist (p parses)
    (pushnew p (compare-frame-out-parses frame) :test #'eq)
    (setf (compare-frame-in-parses frame)
      (remove p (compare-frame-in-parses frame)))))

