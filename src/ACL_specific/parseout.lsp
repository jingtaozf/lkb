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

(defun lkb-parse-tree-font nil
  (list :sans-serif :roman (or *parse-tree-font-size* 9)))

; replaces  *ptree-text-style* 

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
    (clim:with-text-style (stream (lkb-parse-tree-font))
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
       #'find-children
       :graph-type :parse-tree
       :stream stream 
       :merge-duplicates nil
       :orientation :vertical
       :generation-separation *ptree-level-sep*
       :within-generation-separation *ptree-node-sep*
       :center-nodes nil))))

;; Find the children of a node, respecting various conditional display flags

(defun find-children (node)
  (let ((edge-record (get node 'edge-record))
        (dtrs (get node 'daughters)))
    (cond ((and (or *dont-show-morphology*
                    *dont-show-lex-rules*)
                (null edge-record))
           ;; Leaf node
           nil)
          ((and *dont-show-lex-rules*
                edge-record
                (lexical-rule-p (edge-rule edge-record)))
           ;; Lexical rule node
           (mapcar #'find-leaf dtrs))
          (t dtrs))))

;; Given a node, return the first leaf node dominated by it.  Assumes
;; that this node and all nodes under it are unary branching/

(defun find-leaf (node)
  (if (null (get node 'edge-record))
      node
    (find-leaf (car (get node 'daughters)))))

;;; menus

;;
;; Make nodes active
;;

(define-parse-tree-command (com-parse-tree-menu)
    ((edge-symbol 'symbol :gesture :select))
  (let* ((edge-record (get edge-symbol 'edge-record))
	 (edge-fs (get edge-symbol 'edge-fs))
	 (item (edge-rule edge-record))
         (rule-name (if (rule-p item) (rule-id item) item)))
    (pop-up-menu
     `((,(format nil "Feature structure - Edge ~A" (edge-id edge-record))
	:value edge)
       (,(format nil "Rule ~A" (or rule-name ""))
	:value rule)
       (,(format nil "Lex ids ~A" (edge-lex-ids edge-record))
	:value rule)
       )
     (edge (display-fs edge-fs
		       (format nil "Edge ~A ~A - Tree FS" 
			       (edge-id edge-record)
			       (if (g-edge-p edge-record) 
				   "G" 
				 "P")))
	   (display-edge-in-chart edge-record))
     (rule 
      (let* ((item (edge-rule edge-record))
             (rule (and (rule-p item) item)))
            (if rule
               (display-fs (rule-full-fs rule)
                  (format nil "~A" (rule-id rule)))
               (let ((alternative (get-tdfs-given-id item)))
                  (when alternative
                     (display-fs alternative
                        (format nil "~A" item))))))))))




;;; ***** Single parse display window ********
;;; because it's very annoying to get zillions of windows when there's
;;; a highly ambiguous sentence, the following code displays a
;;; single window with lots of little parse trees.  It is
;;; based on Rob's compare code (see compare.lsp)


(defvar *main-chart-frame* nil)
(defvar *sub-chart-window-frames* nil)

;;; globals are set in chartout which is therefore read in
;;; after this file

(defstruct prtree 
  ;; Top node of parse tree
  top					
  ;; Output record of tree
  output-record
  ;; edge
  edge)

(define-lkb-frame parse-tree-frame
  ((trees :initform nil
	  :accessor parse-tree-frame-trees))
  :display-function 'draw-res-trees-window
  :text-style (clim:parse-text-style 
               (list :sans-serif :roman 7))
   :width :compute
   :height :compute)

#|
(show-parse-tree-frame *parse-record*)
|#


(defun invalidate-chart-commands nil
  (clim:map-over-frames 
   #'(lambda (frame) 
       (when (and (eql (clim:frame-name frame) 'parse-tree-frame)
                  (member (clim:frame-state frame) '(:enabled :shrunk)))
         (setf (clim:command-enabled 'com-multiple-tree-menu frame) nil) 
         (setf (clim:command-enabled 'com-show-chart-from-tree frame) nil)))))

(defun show-parse-tree-frame (parses)
  (let ((frame (clim:make-application-frame 'parse-tree-frame)))
    (set-up-parse-tree-frame parses frame)
    (setf (clim:frame-pretty-name frame) 
      (format nil "~{~a ~}" (edge-leaves (car parses))))
    (mp:process-run-function "Parse results" 
                             #'clim:run-frame-top-level frame)))

(defun set-up-parse-tree-frame (parses frame)
  (setf (parse-tree-frame-trees frame)
    (mapcar #'(lambda (p) 
		(make-prtree :top (make-new-parse-tree p 1)
                             :edge p))
	    parses)))

(defun draw-res-trees-window (window stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (dolist (tree (parse-tree-frame-trees window))
    (setf (prtree-output-record tree)
      (clim:with-new-output-record (stream)
	(clim:with-output-recording-options (stream :record t)
	  (clim:with-output-as-presentation 
	      (stream tree 'prtree :single-box t)
	    (clim:format-graph-from-root
	     (prtree-top tree)
	     #'(lambda (node stream)
		 (multiple-value-bind (s bold-p) 
		     (get-string-for-edge node)
		   (clim:with-text-face (stream (if bold-p :bold :roman))
		     (write-string s stream))))
	     #'find-children
	     :graph-type :parse-tree
	     :stream stream 
	     :merge-duplicates nil
	     :orientation :vertical
	     :generation-separation 5
	     :move-cursor t
	     :within-generation-separation 5
	     :center-nodes nil)))
	(terpri stream)))))


(define-parse-tree-frame-command (com-multiple-tree-menu)
    ((tree 'prtree :gesture :select))
  (let ((command (clim:menu-choose
		  '(("Show enlarged tree" :value show)
                    ("Highlight chart nodes" :value chart) 
                    ))))
    (when command
      (handler-case
	  (ecase command
	    (show (draw-new-parse-tree (prtree-top tree)
				       "Parse tree" nil))
            (chart
             (cond ((and *main-chart-frame* 
                         (eql (clim:frame-state *main-chart-frame*) :enabled))
                    nil)
                   ((and *main-chart-frame* 
                         (eql (clim:frame-state *main-chart-frame*) :shrunk))
                    (clim:raise-frame *main-chart-frame*))
                   (t (show-chart) 
                      (mp:process-wait-with-timeout "Waiting" 
                                                    5 #'chart-ready)))
             (display-edge-in-chart
              (prtree-edge tree)))
            )
	(error (condition) 
	  (declare (ignore condition) )
	  nil)))))

(defun chart-ready nil
  (and *main-chart-frame* 
       (eql (clim:frame-state *main-chart-frame*) :enabled)))

(define-parse-tree-frame-command (com-show-chart-from-tree :menu "Show chart")
    ()   
    (show-chart))

    
