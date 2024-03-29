;;; Copyright (c) 1991--2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Frederik Fouvry;
;;;   see `LICENSE' for conditions.

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
                           topnode counter title))

(defun draw-new-parse-tree-really (topnode counter &optional title)
  (let ((pframe (clim:make-application-frame 'parse-tree)))
    (setf (parse-tree-nodes pframe) topnode)
    (setf (parse-tree-current-chart pframe) 
      (or counter
          *chart-generation-counter*))
    (setf (clim:frame-pretty-name pframe) (or title "Parse Tree"))
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
     `(
       ("Feature structure"
	:value fs)
       ("Unfilled feature structure"
	:value ufs)
       (,(format nil "Feature structure - Chart edge ~A" (edge-id edge-record))
	:value edge-fs)
       (,(format nil "Unfilled feature structure - Chart edge ~A" (edge-id edge-record))
	:value edge-ufs)
       ("Show edge in chart" 
        :value edge
        :active ,(and (not (g-edge-p edge-record))
                      (or
                       (not (parse-tree-current-chart clim:*application-frame*))
                       (eql (parse-tree-current-chart clim:*application-frame*)
                            *chart-generation-counter*))))
       (,(format nil "Rule ~A" (or rule-name ""))
	:value rule)
       ("Sement"
	:value sement
	:active ,(mrs::algebra-available-p))
       ("Rule sement"
	:value rule-sement
	:active ,(mrs::algebra-available-p))
       ("Check algebra"
	:value check-algebra
	:active ,(mrs::algebra-available-p)) 
       ("Generate from edge" :value generate
                             :active ,(and *mrs-loaded*
                                       (not (g-edge-p edge-record))))
       ;;; FIX - actually crashes Lisp to select this with a generator edge
       (,(format nil "Lex ids ~A" (edge-lex-ids edge-record))
	:value nil))
     (fs (display-fs edge-fs
		       (format nil "Edge ~A ~A - Tree FS" 
			       (edge-id edge-record)
			       (if (g-edge-p edge-record) "G" "P"))))
     (ufs (display-fs (unfilled-tdfs (copy-tdfs-completely edge-fs))
		       (format nil "Edge ~A ~A - Tree Unfilled FS" 
			       (edge-id edge-record)
			       (if (g-edge-p edge-record) "G" "P"))))
     (edge-fs
      (let ((tdfs (and (edge-p edge-record) (edge-dag edge-record))))
        (when (tdfs-p tdfs)
          (display-fs tdfs
                      (format nil "Edge ~A ~A - Edge FS" 
                              (edge-id edge-record)
                              (if (g-edge-p edge-record) "G" "P"))))))
     (edge-ufs
      (let ((tdfs (and (edge-p edge-record) (edge-dag edge-record))))
        (when (tdfs-p tdfs)
          (display-fs (unfilled-tdfs (copy-tdfs-completely tdfs))
                      (format nil "Edge ~A ~A - Edge Unfilled FS" 
                              (edge-id edge-record)
                              (if (g-edge-p edge-record) "G" "P"))))))
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
     (sement       
      ;;; we have to use the tdfs as used by the parser here, because
      ;;; otherwise we have things on the rels list that shouldn't be 
      ;;; there because of the diff list
      (let ((parse-tdfs (and (edge-p edge-record) (edge-dag edge-record))))
        (when (tdfs-p parse-tdfs)
	  (show-mrs-sement-window parse-tdfs edge-fs edge-record
				  (format nil "Edge ~A ~A - Sement" 
					  (edge-id edge-record)
					  (if (g-edge-p edge-record) "G" "P"))))))
     (rule-sement       
      ;;; this is to get the contribution of the rule - for cases where
      ;;; it has a c-cont
      (let ((rule-tdfs (and (edge-p edge-record) 
			    (rule-p (edge-rule edge-record))
			    (rule-full-fs (edge-rule edge-record)))))
        (when (tdfs-p rule-tdfs)
	  (show-mrs-rule-sement-window rule-tdfs
				  (format nil "Edge ~A ~A - Rule sement" 
					  (edge-id edge-record)
					  (if (g-edge-p edge-record) "G" "P"))))))
     (check-algebra       
      (let ((parse-tdfs (if (edge-p edge-record) (edge-dag edge-record))))
        (when (tdfs-p parse-tdfs)
	  (show-mrs-sement-check-window parse-tdfs edge-fs edge-record
				  (format nil "Edge ~A ~A - algebra check" 
					  (edge-id edge-record)
					  (if (g-edge-p edge-record) "G" "P")))))) 
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

(defun show-parse-tree-frame (parses &optional (title "Parse results"))
  (if #+:lui (lui-status-p :tree) #-:lui nil
    (let ((input (format nil "~{~a~^ ~}" (edge-leaves (first parses)))))
      #+:lui (lui-show-parses parses input) #-:lui nil)
    (mp:run-function title #'show-parse-tree-frame-really parses title)) )

(defun show-parse-tree-frame-really (parses &optional title)
  (let ((frame (clim:make-application-frame 'parse-tree-frame)))
    (set-up-parse-tree-frame parses frame)
    (setf (clim:frame-pretty-name frame) 
      (or title (format nil "~{~a ~}" (edge-leaves (car parses)))))
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
		    ("Partial chart" :value partial-chart)
                    ("Generate" :value generate :active ,*mrs-loaded*)
                    ("MRS" :value mrs :active ,*mrs-loaded*)
                    ("Prolog MRS" :value prolog :active ,*mrs-loaded*)
                    ("RMRS" :value rmrs :active ,*mrs-loaded*)
		    ("DMRS" :value dmrs :active ,*mrs-loaded*)
                    ("Indexed MRS" :value indexed :active ,*mrs-loaded*)
                    ("Scoped MRS" :value scoped :active ,*mrs-loaded*)
                    #+:logon
                    ("UTool MRS" :value utool :active ,*mrs-loaded*)
                    ("Dependencies" :value dependencies :active ,*mrs-loaded*)
		    ("FOL approximation" 
		     :value fol :active ,*mrs-loaded*)
                    ("Rephrase" :value rephrase :active ,*mrs-loaded*)
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
	    (partial-chart
	     (if (or (not (parse-tree-frame-current-chart 
                           clim:*application-frame*))
                     (eql (parse-tree-frame-current-chart 
                           clim:*application-frame*)
                     *chart-generation-counter*))
                 (multiple-value-bind (root subframe-p)
                   (cond ((and *main-chart-frame* 
                               (eql (clim:frame-state *main-chart-frame*) 
                                    :enabled))
			  (values
			   (chart-window-root *main-chart-frame*)
			   t))
                         ((and *main-chart-frame* 
                               (eql (clim:frame-state *main-chart-frame*) 
                                    :shrunk))
			  (values
			   (chart-window-root *main-chart-frame*)
			   t))
                         (t (values (construct-chart-no-display)
				    nil)))
		   (display-partial-chart root (prtree-edge tree)
					  subframe-p))
               (lkb-beep)))
            ;; funcall avoids undefined function warnings
            (generate (funcall 'really-generate-from-edge (prtree-edge tree)))
            (mrs (funcall 'show-mrs-window (prtree-edge tree)))
            (indexed (funcall 'show-mrs-indexed-window (prtree-edge tree)))
            (prolog (funcall 'show-mrs-prolog-window (prtree-edge tree)))
            (scoped (funcall 'show-mrs-scoped-window (prtree-edge tree)))
            #+:logon
            (utool (funcall 'show-mrs-utool-window (prtree-edge tree)))
            (rmrs (funcall 'show-mrs-rmrs-window (prtree-edge tree)))
	    (dmrs (funcall 'show-mrs-dmrs-window (prtree-edge tree)))
            (dependencies 
             (funcall 'show-mrs-dependencies-window (prtree-edge tree)))
	    (fol 
             (funcall 'show-mrs-fol-window (prtree-edge tree)))
            (rephrase
             (let ((symbol (when (find-package :mt)
                             (find-symbol "REPHRASE" :mt))))
               (when (and symbol (fboundp symbol))
                 (funcall symbol (prtree-edge tree))))))
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

(define-parse-tree-frame-command 
    (com-compare-from-tree :name "Compare" :menu t)
    ()
  
  (clim:with-application-frame (frame)
    (let ((edges (loop
                     for tree in (parse-tree-frame-trees frame)
                     collect (prtree-edge tree))))
      (compare-parses edges))))
