;;; Copyright Ann Copestake 1992-1997
;;; All Rights Reserved.
;;; No use or redistribution without permission.
;;; 
;;; Ann Copestake
        
;;; ACL version

(in-package :user)


;; Dialect specific stuff

(defparameter *parse-window-width* 400
  "Initial width of tree window")

(defparameter *parse-window-height* 400
  "Initial height of tree window")

(defparameter *ptree-text-style* 
    (clim:parse-text-style (list :sans-serif :roman 
				 (or *parse-tree-font-size* 9)))
  "Text style for node labels.")

(defparameter *ptree-node-sep* 6
  "Spacing between nodes in a single generation.")

(defparameter *ptree-level-sep* 12
  "Spacing between levels in the tree.")                    
    
;;
;; Define a frame class for our parse tree window
;;

(clim:define-application-frame parse-tree ()
  ((nodes :initform nil
   :accessor parse-tree-nodes))
  (:panes
   (display  
    (clim:outlining (:thickness 1)
      (clim:spacing (:thickness 1)  
	(clim:scrolling (:scroll-bars :both)
	  (clim:make-pane 'clim:application-pane
			  :display-function 'draw-parse-tree
			  :text-cursor nil
			  :width *parse-window-width* 
			  :height *parse-window-height*
			  :text-style *ptree-text-style*
			  :borders nil
			  :background clim:+white+
			  :foreground clim:+black+
			  :display-time nil))))))
  (:layouts
    (:default display)))

(defun draw-new-parse-tree (topnode title horizontalp)
  (declare (ignore horizontalp))
  (let ((pframe (clim:make-application-frame 'parse-tree)))
    (setf (parse-tree-nodes pframe) topnode)
    (mp:process-run-function title 
                             #'clim:run-frame-top-level pframe)))



(defun draw-parse-tree (ptree-frame stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let ((node-tree (parse-tree-nodes ptree-frame)))
    (clim:format-graph-from-root
     node-tree
     #'(lambda (node stream)
	 (multiple-value-bind (s bold-p) 
	     (get-string-for-edge node)
	   (clim:with-text-face (stream (if bold-p :bold :roman))
	     (let ((cont (get node 'edge-record)))
	       (if cont
		   (clim:with-output-as-presentation 
		       (stream cont 'edge)
		     (write-string s stream))
		 (write-string s stream))))))
     #'(lambda (node) (get node 'daughters))
     :graph-type :parse-tree
     :stream stream 
     :merge-duplicates nil
     :orientation :vertical
     :generation-separation *ptree-level-sep*
     :within-generation-separation *ptree-node-sep*
     :center-nodes nil)))

(defun get-string-for-edge (edge-symbol)
   (let ((edge-record (get edge-symbol 'edge-record)))
      (if edge-record
         (values (tree-node-text-string (or 
                                    (find-category-abb (edge-dag edge-record))
                                    (edge-category edge-record))) nil)
         (values (tree-node-text-string edge-symbol) t))))

;;; menus

;; 
;; Add [EXIT] button
;;

(define-parse-tree-command (com-exit-parse-tree :menu "Close")
    ()
  (clim:frame-exit clim:*application-frame*))


;;
;; Make nodes active
;;

(define-parse-tree-command (com-parse-tree-menu)
    ((edge-record 'edge :gesture :select))
  (let* ((rule-name (edge-rule-number edge-record))
	 (command (clim:menu-choose
		  `((,(format nil "Edge ~A" (edge-id edge-record))
		     :value edge)
		    (,(format nil "Rule ~A" (or rule-name ""))
		     :value rule)))))
    (when command
      (handler-case
	  (ecase command
	    (edge (display-fs (edge-dag edge-record)
			    (format nil "Edge ~A ~A - FS" 
				    (edge-id edge-record)
				    (if (gen-chart-edge-p edge-record) 
					"G" 
				      "P")))
		(display-edge-in-chart edge-record))
	    (rule 
	     (let* ((rule-name (edge-rule-number edge-record))
		    (rule (or (get-grammar-rule-entry rule-name)
			      (get-lex-rule-entry rule-name))))
	       (if rule
		   (display-fs (rule-full-fs rule)
			       (format nil "~A" rule-name))
		 (let ((alternative (get-tdfs-given-id rule-name)))
		   (when alternative
		     (display-fs alternative
				 (format nil "~A" rule-name))))))))
	(error (condition)
	  (format clim-user:*lkb-top-stream*  
		  "~%Error: ~A~%" condition))))))


