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

(def-lkb-parameter *ptree-text-style* 
    (list :sans-serif :roman (or *parse-tree-font-size* 9))
  "Text style for node labels.")

(def-lkb-parameter *ptree-node-sep* 6
  "Spacing between nodes in a single generation.")

(def-lkb-parameter *ptree-level-sep* 12
  "Spacing between levels in the tree.")                    


;;
;; Define a frame class for our parse tree window
;;

(define-lkb-frame parse-tree
    ((nodes :initform nil
	    :accessor parse-tree-nodes))
  :display-function 'draw-parse-tree
  :width *parse-window-width* 
  :height *parse-window-height*)

(defun draw-new-parse-tree (topnode title horizontalp)
  (declare (ignore horizontalp))
  (let ((pframe (clim:make-application-frame 'parse-tree)))
    (setf (parse-tree-nodes pframe) topnode)
    (mp:process-run-function title 
                             #'clim:run-frame-top-level pframe)))

(defun draw-parse-tree (ptree-frame stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let ((node-tree (parse-tree-nodes ptree-frame)))
    (clim:with-text-style (stream *ptree-text-style*)
      (clim:format-graph-from-root
       node-tree
       #'(lambda (node stream)
	   (multiple-value-bind (s bold-p) 
	       (get-string-for-edge node)
	     (clim:with-text-face (stream (if bold-p :bold :roman))
	       (if (get node 'edge-record)
		   (clim:with-output-as-presentation (stream node 'symbol)
		     (write-string s stream))
		 (write-string s stream)))))
       #'(lambda (node) (get node 'daughters))
       :graph-type :parse-tree
       :stream stream 
       :merge-duplicates nil
       :orientation :vertical
       :generation-separation *ptree-level-sep*
       :within-generation-separation *ptree-node-sep*
       :center-nodes nil))))


;;; menus

;;
;; Make nodes active
;;

(define-parse-tree-command (com-parse-tree-menu)
    ((edge-symbol 'symbol :gesture :select))
  (let* ((edge-record (get edge-symbol 'edge-record))
	 (edge-fs (get edge-symbol 'edge-fs))
	 (rule-name (edge-rule-number edge-record)))
    (pop-up-menu
     `((,(format nil "Edge ~A" (edge-id edge-record))
	:value edge)
       (,(format nil "Rule ~A" (or rule-name ""))
	:value rule))
     (edge (display-fs edge-fs
		       (format nil "Edge ~A ~A - Tree FS" 
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
			  (format nil "~A" rule-name))))))))))



