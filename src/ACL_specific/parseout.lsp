;;; Copyright (c) 1991--2002
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

;;; ACL version

(in-package :lkb)

;; Dialect specific stuff

(defparameter *parse-window-width* 400
  "Initial width of tree window")

(defparameter *parse-window-height* 400
  "Initial height of tree window")

(defparameter *parse-tree-font* nil)

(defun lkb-parse-tree-font nil
  (list :sans-serif :roman (or *parse-tree-font-size* 9)))

(defun lkb-summary-tree-font ()
  (list :sans-serif :roman (or *summary-tree-font-size* 7)))

; replaces  *ptree-text-style* 

(def-lkb-parameter *ptree-node-sep* 6
  "Spacing between nodes in a single generation.")

(def-lkb-parameter *ptree-level-sep* 12
  "Spacing between levels in the tree.")                    

;;; Globals used to keep track of parse chart

(defvar *main-chart-frame* nil)
(defvar *sub-chart-window-frames* nil)

;;; globals are set in chartout which is therefore read in
;;; after this file


;;
;; Define a frame class for our parse tree window
;;

(define-lkb-frame parse-tree
    ((nodes :initform nil
	    :accessor parse-tree-nodes)
     (current-chart :initform nil
                    :accessor parse-tree-current-chart))
  :display-function 'draw-parse-tree
  :width *parse-window-width* 
  :height *parse-window-height*)

(defun draw-new-parse-tree (topnode title horizontalp &optional counter)
  (declare (ignore horizontalp))
  (mp:run-function title 
                           #'draw-new-parse-tree-really 
                           topnode counter))

(defun draw-new-parse-tree-really (topnode counter)
  (let ((pframe (clim:make-application-frame 'parse-tree)))
    (setf (parse-tree-nodes pframe) topnode)
    (setf (parse-tree-current-chart pframe) 
      (or counter
          *chart-generation-counter*))
    (clim:run-frame-top-level pframe)))


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
       ("Show edge in chart" 
        :value edge
        :active ,(and (not (g-edge-p edge-record))
                      (or
                       (not (parse-tree-current-chart clim:*application-frame*))
                       (eql (parse-tree-current-chart clim:*application-frame*)
                            *chart-generation-counter*))))
       (,(format nil "Rule ~A" (or rule-name ""))
	:value rule)
       ("Generate from edge" :value generate
                             :active ,(and *mrs-loaded*
                                       (not (g-edge-p edge-record))))
       ;;; FIX - actually crashes Lisp to select this with a generator edge
       (,(format nil "Lex ids ~A" (edge-lex-ids edge-record))
	:value nil))
     (fs (display-fs edge-fs
		       (format nil "Edge ~A ~A - Tree FS" 
			       (edge-id edge-record)
			       (if (g-edge-p edge-record) 
				   "G" 
				 "P"))))
     (edge
          (progn
            (cond ((and *main-chart-frame* 
                        (eql (clim:frame-state *main-chart-frame*) 
                             :enabled))
                   nil)
                  ((and *main-chart-frame* 
                        (eql (clim:frame-state *main-chart-frame*) 
                             :shrunk))
                   (clim:raise-frame *main-chart-frame*))
                  (t (show-chart) 
                     (mp:process-wait-with-timeout "Waiting" 
                                                   5 #'chart-ready)))
            (highlight-edge edge-record *main-chart-frame* :scroll-to t)))
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
     (generate (funcall 'really-generate-from-edge edge-record)))))




;;; ***** Single parse display window ********
;;; because it's very annoying to get zillions of windows when there's
;;; a highly ambiguous sentence, the following code displays a
;;; single window with lots of little parse trees.  It is
;;; based on Rob's compare code (see compare.lsp)


(eval-when #+:ansi-eval-when (:load-toplevel :compile-toplevel :execute)
           #-:ansi-eval-when (load eval compile)
  (defstruct prtree 
    ;; Top node of parse tree
    top					
    ;; Output record of tree
    output-record
    ;; edge
    edge))

(define-lkb-frame parse-tree-frame
    ((trees :initform nil
	    :accessor parse-tree-frame-trees)
  (current-chart :initform nil
	    :accessor parse-tree-frame-current-chart))
  :display-function 'draw-res-trees-window
  :text-style (clim:parse-text-style  (lkb-summary-tree-font))
  :width :compute
  :height :compute)



(defun invalidate-chart-commands nil
  ;;; this function should be redundant now because of *chart-generation-counter*
  nil)
#|
  (clim:map-over-frames 
   #'(lambda (frame) 
       (when (and (eql (clim:frame-name frame) 'parse-tree-frame)
                  (member (clim:frame-state frame) '(:enabled :shrunk)))
         (setf (clim:command-enabled 'com-multiple-tree-menu frame) nil) 
         (setf (clim:command-enabled 'com-show-chart-from-tree frame) nil)))))
|#

(defun show-parse-tree-frame (parses)
  (mp:run-function "Parse results" 
                             #'show-parse-tree-frame-really parses)) 

(defun show-parse-tree-frame-really (parses)
  (let ((frame (clim:make-application-frame 'parse-tree-frame)))
    (set-up-parse-tree-frame parses frame)
    (setf (clim:frame-pretty-name frame) 
      (format nil "~{~a ~}" (edge-leaves (car parses))))
    (clim:run-frame-top-level frame)))


(defun set-up-parse-tree-frame (parses frame)
  (setf (parse-tree-frame-current-chart frame) *chart-generation-counter*)
  (setf (parse-tree-frame-trees frame)
    (mapcar #'(lambda (p) 
		(make-prtree :top (make-new-parse-tree p 1)
                             :edge p))
	    parses)))

(defun draw-res-trees-window (window stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (dolist (tree (parse-tree-frame-trees window))
    (setf (prtree-output-record tree)
    (clim:with-text-style (stream (lkb-summary-tree-font))
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
	(terpri stream))))))


(define-parse-tree-frame-command (com-multiple-tree-menu)
    ((tree 'prtree :gesture :select))
  (let ((command (clim:menu-choose
		  `(("Show enlarged tree" :value show)
                    ("Highlight chart nodes" :value chart) 
                    ("Generate" :value generate :active ,*mrs-loaded*)
                    ("MRS" :value mrs :active ,*mrs-loaded*)
                    ("Prolog MRS" :value prolog :active ,*mrs-loaded*)
                    ("Indexed MRS" :value indexed :active ,*mrs-loaded*)
                    ("Scoped MRS" :value scoped :active ,*mrs-loaded*)
                    ("Dependencies" :value dependencies :active ,*mrs-loaded*)
                    ("Transfer" :value transfer :active ,*mrs-loaded*)
                    ))))
    (when command
      (handler-case
	  (ecase command
	    (show (draw-new-parse-tree (prtree-top tree)
				       "Parse tree" nil 
                                       (parse-tree-frame-current-chart 
                                        clim:*application-frame*)))
            (chart
             (if (or (not (parse-tree-frame-current-chart 
                           clim:*application-frame*))
                     (eql (parse-tree-frame-current-chart 
                           clim:*application-frame*)
                     *chart-generation-counter*))
                 (progn
                   (cond ((and *main-chart-frame* 
                               (eql (clim:frame-state *main-chart-frame*) 
                                    :enabled))
                          nil)
                         ((and *main-chart-frame* 
                               (eql (clim:frame-state *main-chart-frame*) 
                                    :shrunk))
                          (clim:raise-frame *main-chart-frame*))
                         (t (show-chart) 
                            (mp:process-wait-with-timeout "Waiting" 
                                                          5 #'chart-ready)))
                   (display-edge-in-chart
                    (prtree-edge tree)))
               (lkb-beep)))
            ;; funcall avoids undefined function warnings
            (generate (funcall 'really-generate-from-edge (prtree-edge tree)))
            (mrs (funcall 'show-mrs-window (prtree-edge tree)))
            (indexed (funcall 'show-mrs-indexed-window (prtree-edge tree)))
            (prolog (funcall 'show-mrs-prolog-window (prtree-edge tree)))
            (scoped (funcall 'show-mrs-scoped-window (prtree-edge tree)))
            (dependencies 
             (funcall 'show-mrs-dependencies-window (prtree-edge tree)))
            (transfer
             (funcall 'transfer (prtree-edge tree))))
        (storage-condition (condition)
          (with-output-to-top ()
            (format t "~%Memory allocation problem: ~A~%" condition)))
	(error (condition)
	  (with-output-to-top ()
	    (format t "~%Error: ~A~%" condition)))
        (serious-condition (condition)
          (with-output-to-top ()
            (format t "~%Something nasty: ~A~%" condition)))))))


(defun chart-ready nil
  (and *main-chart-frame* 
       (eql (clim:frame-state *main-chart-frame*) :enabled)))

(define-parse-tree-frame-command (com-show-chart-from-tree :menu "Show chart")
    ()   
  (if (or (not (parse-tree-frame-current-chart clim:*application-frame*))
          (eql (parse-tree-frame-current-chart clim:*application-frame*)
               *chart-generation-counter*))
      (show-chart)
    (lkb-beep)))


