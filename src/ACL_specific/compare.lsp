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

(defstruct discr in out toggle)

(defstruct (syn-discr (:include discr)) label yield)

(defstruct (sem-discr (:include discr)) word rel)

(defvar *discrs*)

(defun find-discriminants (parses)
  (let ((*discrs* nil))
    ;; Collect all constituents
    (dolist (parse parses)
      (find-syn-discriminants-in-parse parse parse))
    ;; Filter out discriminants that are implied by other longer discriminants
    (setq *discrs*
      (delete-if #'(lambda (x) 
		     (some #'(lambda (y)
			       (and (equal (discr-in x)
					   (discr-in y))
				    (> (length (syn-discr-yield y))
				       (length (syn-discr-yield x)))))
			   *discrs*))
		 *discrs*))
    ;; Collect all word meanings
    (dolist (parse parses)
      (find-sem-discriminants-in-parse parse parse))
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
				(if (syn-discr-p n)
				    (length (syn-discr-yield n))
				  0)))))

(defun find-sem-discriminants-in-parse (parse top)
  (if (edge-children parse) 
      (dolist (child (edge-children parse))
	(find-sem-discriminants-in-parse child top))
    (let* ((rel (type-of-fs (existing-dag-at-end-of 
			     (tdfs-indef (edge-dag parse)) 
			     '(synsem local cont key))))
	   (word (car (edge-leaves parse)))
	   (old (member (list word rel)
			*discrs*
			:test #'(lambda (x y)
				  (equal x
					 (list (sem-discr-word y)
					       (sem-discr-rel y)))))))
      (if old
	  (pushnew top (discr-in (car old)))
	(push (make-sem-discr :word word
			      :rel rel
			      :in (list top)
			      :out nil
			      :toggle :unknown)
	      *discrs*)))))

(defun find-syn-discriminants-in-parse (parse top)
  (let* ((label (find-category-abb (edge-dag parse)))
	 (yield (edge-leaves parse))
	 (old (member (list label yield)
		      *discrs*
		      :test #'(lambda (x y)
				(equal x
				       (list (syn-discr-label y)
					     (syn-discr-yield y)))))))
    (if old
	(pushnew top (discr-in (car old)))
      (push (make-syn-discr :label label
			    :yield yield
			    :in (list top)
			    :out nil
			    :toggle :unknown)
	    *discrs*))
    (dolist (child (edge-children parse))
      (find-syn-discriminants-in-parse child top))))

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
	:accessor compare-frame-out-parses))
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
			  :incremental-redisplay '(t :check-overlapping nil)
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
    (setf (compare-frame-in-parses frame) nil)
    (setf (compare-frame-out-parses frame) nil)
    (dolist (d (compare-frame-discrs frame))
      (setf (discr-toggle d) :unknown))
    (recompute-in-and-out frame)))

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
  (let ((done-p nil))
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
	(clim:updating-output (stream :unique-id tree
				      :cache-value ink)
	  (clim:with-drawing-options (stream :ink ink)
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
    (setf (clim:command-enabled 'com-done-compare-frame window) done-p)))


(define-compare-frame-command (com-tree-menu)
    ((tree 'ptree :gesture :select))
  (let ((command (clim:menu-choose
		  '(("Select" :value select :active nil)
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
		 (delete (ptree-parse tree) 
			 (compare-frame-parses frame) 
			 :test #'eq))))
	    (show (draw-new-parse-tree (ptree-top tree)
				       "Parse tree" nil)))
	(error (condition) 
	  (declare (ignore condition) )
	  nil)))))

(defun update-trees (window stream)
  (dolist (tree (compare-frame-trees window))
    (with-slots (output-record ink) tree
      (let ((new-ink (if (member (ptree-parse tree) 
				 (compare-frame-out-parses window)
				 :test #'eq)
			 clim:+red+
		       clim:+foreground-ink+)))
	(unless (eq new-ink ink)
	  (setf (clim:displayed-output-record-ink output-record) new-ink)
	  (setf ink new-ink)
	  (clim:replay output-record stream))))))

;;; **********************************************************************
;;; Stuff for constituent pane

(defun draw-compare-window (window stream)
  (let ((discrs (compare-frame-discrs window)))
    (format stream "~a~%~%" (edge-leaves (car (compare-frame-parses window))))
    (format stream "~a parse~:p in, ~a parse~:p out~%~%" 
	    (length (compare-frame-in-parses window))
	    (length (compare-frame-out-parses window)))
    (clim:formatting-table (stream :x-spacing "XX")
      (dolist (d discrs)
	(clim:formatting-row (stream)
	  (clim:with-output-as-presentation
	      (stream d 'discr)
	    (clim:formatting-cell (stream :align-x :center)
	      (write-string (cond ((eq (discr-toggle d) t)
				   "+")
				  ((null (discr-toggle d))
				   "-")
				  (t "?"))
			    stream))
	    (clim:formatting-cell (stream :align-x :left)
	      (if (syn-discr-p d)
		  (write-string (syn-discr-label d) stream)
		(write-string (sem-discr-word d) stream)))
	    (clim:formatting-cell (stream :align-x :left)
	      (if (syn-discr-p d)
		  (format stream "~A" (syn-discr-yield d))
		(format stream "~A" (sem-discr-rel d))))))))))



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
      (recompute-in-and-out clim:*application-frame*))))

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
      (delete p (compare-frame-in-parses frame)))))

