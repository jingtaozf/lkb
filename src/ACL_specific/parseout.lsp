;;; Copyright Ann Copestake 1992-1997
;;; All Rights Reserved.
;;; No use or redistribution without permission.
;;; 
;;; Ann Copestake
        
;;; ACL version

(in-package :cl-user)

;; Dialect specific stuff

(defparameter *parse-window-width* 400
  "Initial width of tree window")

(defparameter *parse-window-height* 400
  "Initial height of tree window")

(defparameter *parse-tree-font* nil)

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
	:value fs)
       ("Show edge in chart" :value edge)
       (,(format nil "Rule ~A" (or rule-name ""))
	:value rule)
       ("Generate from edge" :value generate)
       (,(format nil "Lex ids ~A" (edge-lex-ids edge-record))
	:value nil))
     (fs (display-fs edge-fs
		       (format nil "Edge ~A ~A - Tree FS" 
			       (edge-id edge-record)
			       (if (g-edge-p edge-record) 
				   "G" 
				 "P"))))
     (edge (let ((chart-frame (reuse-frame 'chart-window)))
	     (when chart-frame
	       (highlight-edge edge-record chart-frame :scroll-to t))))
     (rule 
      (let* ((item (edge-rule edge-record))
             (rule (and (rule-p item) item)))
	(if rule
	    (display-fs (rule-full-fs rule)
			(format nil "~A" (rule-id rule))
                        (rule-id rule))
	  (let ((alternative (get-tdfs-given-id item)))
	    (when alternative
	      (display-fs alternative 
                          (format nil "~A" item)
                          item))))))
     (generate 
      (really-generate-from-edge edge-record)))))




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
		  `(("Show enlarged tree" :value show)
                    ("Highlight chart nodes" :value chart) 
                    ("Generate" :value generate :active ,*mrs-loaded*)
                    ("MRS" :value mrs :active ,*mrs-loaded*)
                    ("Indexed MRS" :value indexed :active ,*mrs-loaded*)
                    ("Scoped MRS" :value scoped :active ,*mrs-loaded*)
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
            (generate (really-generate-from-edge (prtree-edge tree)))
            (mrs (show-mrs-window (prtree-edge tree)))
            (indexed (show-mrs-indexed-window (prtree-edge tree)))
            (scoped (show-mrs-scoped-window (prtree-edge tree)))
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


