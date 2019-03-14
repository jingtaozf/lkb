;; Copyright (c) 2003--2018
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `LICENSE' for conditions.

(in-package :lkb)

;;; RMRS windows

;;; Frames

(define-lkb-frame rmrs-ordinary
    ((rmrs :initform nil
	   :accessor rmrs-ordinary-rmrs))
  :display-function 'show-rmrs-ordinary  
  :text-style (lkb-parse-tree-font)
  :width :compute 
  :height :compute)

(define-lkb-frame mrs-rmrs
    ((mrsstruct :initform nil
                :accessor mrs-rmrs-mrsstruct)
     (rmrs :initform nil
                :accessor mrs-rmrs-rmrs))
  :display-function 'show-mrs-rmrs  
  :text-style (lkb-parse-tree-font)
  :width :compute 
  :height :compute)

;;; compare functionality

(define-lkb-frame mrs-rmrs-compare
    ((rmrs1 :initform nil
	    :accessor mrs-rmrs-compare-rmrs1)
     (rmrs2 :initform nil
	    :accessor mrs-rmrs-compare-rmrs2)
     (comparison-record :initform nil
	    :accessor mrs-rmrs-compare-comparison-record))
  :display-function 'show-mrs-rmrs-compare
  :text-style (lkb-parse-tree-font)
  :width (* 2 *parse-window-width*) 
  :height *parse-window-height*)

;;; DMRS

(define-lkb-frame dmrs-ordinary
    ((dmrs :initform nil
	   :accessor dmrs-ordinary-dmrs))
  :display-function 'show-dmrs-ordinary  
  :text-style (lkb-parse-tree-font)
  :width :compute 
  :height :compute)

(define-lkb-frame mrs-dmrs
    ((mrsstruct :initform nil
                :accessor mrs-dmrs-mrsstruct)
     (rmrs :initform nil
	   :accessor mrs-dmrs-dmrs))
  :display-function 'show-mrs-dmrs  
  :text-style (lkb-parse-tree-font)
  :width :compute 
  :height :compute)

;;; end frames

(define-rmrs-ordinary-command (com-select-rmrs :menu "Select") 
    ()
  (set-selected-rmrs-from-menu 
   (rmrs-ordinary-rmrs clim:*application-frame*)))

(define-mrs-rmrs-command (com-select-mrs-rmrs :menu "Select") 
    ()
  (set-selected-rmrs-from-menu 
   (mrs-rmrs-rmrs clim:*application-frame*)))

;;; As MRS - XML output

(define-rmrs-ordinary-command (com-output-rmrs-xml :menu "Save as XML") 
    ()
  (save-rmrs-as-xml (rmrs-ordinary-rmrs clim:*application-frame*)))

(define-mrs-rmrs-command (com-output-mrs-rmrs-xml :menu "Save as XML") 
    ()
  (save-rmrs-as-xml (mrs-rmrs-rmrs clim:*application-frame*)))

(define-mrs-dmrs-command (com-output-dmrs-xml :menu "Save as XML") 
    ()
  (save-dmrs-as-xml (mrs-dmrs-dmrs clim:*application-frame*)))

(define-mrs-dmrs-command (com-output-dmrs-rmrs :menu "Convert to RMRS") 
    ()
  (show-rmrs-ordinary-window
   (mrs::dmrs-to-rmrs (mrs-dmrs-dmrs clim:*application-frame*))
   "Converted from DMRS"))

(define-mrs-dmrs-command (com-output-dmrs-latex :menu "Save as LaTeX") 
        ()
  (save-dmrs-as-latex (mrs-dmrs-dmrs clim:*application-frame*)))

(define-mrs-dmrs-command (com-output-dmrs-svg :menu "Save as SVG") 
        ()
  (save-dmrs-as-svg (mrs-dmrs-dmrs clim:*application-frame*)))

(define-mrs-dmrs-command (com-output-dmrs-simplify :menu "Simplify") 
    ()
  (show-dmrs-ordinary-window
   (mrs::dmrs-simplify-complex-names (mrs-dmrs-dmrs clim:*application-frame*))
   "Simplified DMRS"))

(defparameter *rmrs-xml-output-file* nil)

(defun save-rmrs-as-xml (rmrsstruct)
  (let ((file-name (if *rmrs-xml-output-file*
		       (let ((use-existing-p 
			      (lkb-y-or-n-p (format nil "Append to ~A?"
						    *rmrs-xml-output-file*))))
			 (if use-existing-p
			     *rmrs-xml-output-file*
			   (ask-user-for-new-pathname 
			    "New file for RMRS XML dumps")))
		       (ask-user-for-new-pathname "File for RMRS XML dumps"))))
    (setf *rmrs-xml-output-file* file-name)
    (when file-name
      (mrs::output-rmrs
       rmrsstruct
       'mrs::xml file-name))))

(defparameter *dmrs-xml-output-file* nil)

(defun save-dmrs-as-xml (dmrsstruct)
  (let ((file-name (if *dmrs-xml-output-file*
		       (let ((use-existing-p 
			      (lkb-y-or-n-p (format nil "Append to ~A?"
						    *dmrs-xml-output-file*))))
			 (if use-existing-p
			     *dmrs-xml-output-file*
			   (ask-user-for-new-pathname 
			    "New file for DMRS XML dumps")))
		       (ask-user-for-new-pathname "File for DMRS XML dumps"))))
    (setf *dmrs-xml-output-file* file-name)
    (when file-name
      (mrs::output-dmrs
       dmrsstruct
       'mrs::dxml file-name))))

(defparameter *dmrs-latex-output-file* nil)

(defun save-dmrs-as-latex (dmrsstruct)
  (let ((file-name (if *dmrs-latex-output-file*
		       (let ((use-existing-p 
			      (lkb-y-or-n-p (format nil "Overwrite ~A?"
						    *dmrs-latex-output-file*))))
			 (if use-existing-p
			     *dmrs-latex-output-file*
			   (ask-user-for-new-pathname 
			    "New file for DMRS LaTeX")))
		       (ask-user-for-new-pathname "File for DMRS LaTeX"))))
    (setf *dmrs-latex-output-file* file-name)
    (when file-name
      (with-open-file (ostream file-name :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
	(mrs::layout-dmrs dmrsstruct :latex ostream)))))

(defparameter *dmrs-svg-output-file* nil)

(defun save-dmrs-as-svg (dmrsstruct)
  (let ((file-name (if *dmrs-svg-output-file*
		       (let ((use-existing-p 
			      (lkb-y-or-n-p (format nil "Overwrite ~A?"
						    *dmrs-svg-output-file*))))
			 (if use-existing-p
			     *dmrs-svg-output-file*
			   (ask-user-for-new-pathname 
			    "New file for DMRS svg")))
		       (ask-user-for-new-pathname "File for DMRS svg"))))
    (setf *dmrs-svg-output-file* file-name)
    (when file-name
      (with-open-file (ostream file-name :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
	(mrs::layout-dmrs dmrsstruct :svg ostream)))))

;;; ordinary window for one RMRS

;;; calling function (called from emacs)

(defun display-rmrs-from-string (str)
  (with-package (:mrs)
    (let ((rmrs (mrs::read-single-rmrs-from-string str)))
      (when (and rmrs (mrs::rmrs-p rmrs))
	(show-rmrs-ordinary-window rmrs "RMRS")))))

;;; windows functions

(defun show-rmrs-ordinary-window (rmrs title)
  (mp:run-function "RMRS ORDINARY"
   #'show-rmrs-ordinary-window-really :rmrs rmrs :title title))
  
(defun show-rmrs-ordinary-window-really (&key rmrs title)
  (let ((mframe (clim:make-application-frame 'rmrs-ordinary)))
    (setf (rmrs-ordinary-rmrs mframe) 
      rmrs)
    (setf (clim:frame-pretty-name mframe) (or title "Robust MRS"))
    (clim:run-frame-top-level mframe)))

(defun show-rmrs-ordinary (mframe stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let ((rmrs (rmrs-ordinary-rmrs mframe)))
      (mrs::output-rmrs1 rmrs 'mrs::compact stream t)))

;;; Window from parser

(defun show-mrs-rmrs-window (edge &key mrs rmrs title)
  (mp:run-function "RMRS"
   #'show-mrs-rmrs-window-really edge :mrs mrs :rmrs rmrs :title title))
  
(defun show-mrs-rmrs-window-really (edge &key mrs rmrs title)
  (let ((mframe (clim:make-application-frame 'mrs-rmrs))
        (mrsstruct (or mrs (when edge (mrs::extract-mrs edge)))))
    (setf (mrs-rmrs-mrsstruct mframe) 
      mrsstruct)
    (setf (mrs-rmrs-rmrs mframe) 
      (or rmrs (mrs::mrs-to-rmrs mrsstruct)))
    (setf (clim:frame-pretty-name mframe) (or title "Robust MRS"))
    (clim:run-frame-top-level mframe)))

(defun show-mrs-rmrs (mframe stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let ((rmrs (mrs-rmrs-rmrs mframe)))
    (if rmrs
      (mrs::output-rmrs1 rmrs 'mrs::compact stream t)
      (format stream "~%::: RMRS structure could not be extracted~%"))))

;;; RMRS comparison

(defun show-mrs-rmrs-compare-window (rmrs1 rmrs2 comparison-record title)
    (mp:run-function "RMRS comparison" 
		     #'show-mrs-rmrs-compare-window-really 
		     rmrs1 rmrs2 comparison-record title))

(defun show-mrs-rmrs-compare-window-really (rmrs1 rmrs2 comparison-record
					    title)
  (let ((mframe (clim:make-application-frame 'mrs-rmrs-compare)))
    (setf (mrs-rmrs-compare-rmrs1 mframe) 
      rmrs1)
    (setf (mrs-rmrs-compare-rmrs2 mframe) 
      rmrs2)
    (setf (mrs-rmrs-compare-comparison-record mframe) 
      comparison-record)
    (setf (clim:frame-pretty-name mframe) (or title "RMRS comparison"))
    (clim:run-frame-top-level mframe)))

(defun show-mrs-rmrs-compare (mframe stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let ((rmrs1 (mrs-rmrs-compare-rmrs1 mframe))
	(rmrs2 (mrs-rmrs-compare-rmrs2 mframe))
	(comparison-record (mrs-rmrs-compare-comparison-record mframe)))
    (if (and rmrs1 rmrs2 comparison-record)
        (clim:with-output-recording-options (stream :draw nil :record t)
	  (let  
	    ((pts1 (mrs::output-rmrs1 rmrs1 'mrs::compact-g stream t t)))
	    (move-to-x-y stream 0 0)
	    (let ((pts2
		    (mrs::output-rmrs1 rmrs2 'mrs::compact-two stream t t)))
	      (add-comparison-pointers stream pts1 pts2 
				       comparison-record))))
        (format stream "~%::: RMRS structures could not be extracted~%"))))

(defun add-comparison-pointers (stream pts1 pts2 comparison-record)
  (when (and pts1 pts2 comparison-record
	     (mrs::rmrs-comparison-record-matched-rels comparison-record))
    ;;; assume nothing to add if no matched-rels
    (let ((top-pt1 (mrs::rmrs-position-record-top pts1))
	  (top-pt2 (mrs::rmrs-position-record-top pts2))
	  (ep-pts1 (mrs::rmrs-position-record-eps pts1))
	  (ep-pts2 (mrs::rmrs-position-record-eps pts2))
	  (arg-pts1 (mrs::rmrs-position-record-args pts1))
	  (arg-pts2 (mrs::rmrs-position-record-args pts2))
	  (ing-pts1 (mrs::rmrs-position-record-ings pts1))
	  (ing-pts2 (mrs::rmrs-position-record-ings pts2))
	  (hcons-pts1 (mrs::rmrs-position-record-hcons pts1))
	  (hcons-pts2 (mrs::rmrs-position-record-hcons pts2)))
      (let ((match (mrs::rmrs-comparison-record-matched-top 
		    comparison-record)))
	(when match
	  (let* ((l1 (mrs::match-top-record-label1 match))
		 (l2 (mrs::match-top-record-label2 match))
		 (ink (determine-comparison-line-colour match)))
	    (draw-rmrs-matching-line l1 (list top-pt1)
				     l2 (list top-pt2) 
				     ink stream))))
      (loop for match in 
	    (mrs::rmrs-comparison-record-matched-rels comparison-record)
	  do
	    (let ((ep1 (mrs::match-rel-record-rel1 match))
		  (ep2 (mrs::match-rel-record-rel2 match))
		  (ink (determine-comparison-line-colour match)))
	      (draw-rmrs-matching-line ep1 ep-pts1
				       ep2 ep-pts2 ink stream)))
      (loop for match in 
	    (mrs::rmrs-comparison-record-matched-args comparison-record)
	  do
	    (let ((arg1 (mrs::match-arg-record-arg1 match))
		  (arg2 (mrs::match-arg-record-arg2 match))
		  (ink (determine-comparison-line-colour match)))
	      (draw-rmrs-matching-line arg1 arg-pts1
				       arg2 arg-pts2 ink stream)))
      (loop for match in 
	    (mrs::rmrs-comparison-record-matched-ings comparison-record)
	  do
	    (let ((ing1 (mrs::match-ing-record-ing1 match))
		  (ing2 (mrs::match-ing-record-ing2 match))
		  (ink (determine-comparison-line-colour match)))
	      (draw-rmrs-matching-line ing1 ing-pts1
				       ing2 ing-pts2 ink stream)))
      (loop for match in 
	    (mrs::rmrs-comparison-record-matched-hcons comparison-record)
	  do
	    (let ((hcons1 (mrs::match-hcons-record-hcons1 match))
		  (hcons2 (mrs::match-hcons-record-hcons2 match))
		  (ink (determine-comparison-line-colour match)))
	      (draw-rmrs-matching-line hcons1 hcons-pts1
				       hcons2 hcons-pts2 ink stream))))))
		       

(defun draw-rmrs-matching-line (object1 pts1 object2 pts2 ink stream)
  (let ((found-object1 (find object1 pts1 
			     :key #'mrs::rmrs-object-position-object))
	(found-object2 (find object2 pts2 
			     :key #'mrs::rmrs-object-position-object)))
    (when (and found-object1 found-object2)
      (let ((object1-pos 
	     (mrs::rmrs-object-position-position found-object1))
	    (object2-pos 
	     (mrs::rmrs-object-position-position found-object2)))
	(clim:draw-line* 
	 stream
	 (+ (position-x object1-pos) 10)
	 (+ (position-y object1-pos) 10)
	 (- (position-x object2-pos) 10)
	 (+ (position-y object2-pos) 10)
	 :ink ink)))))
	 
(defun determine-comparison-line-colour (match)
  ;;; if we assume that the deep-grammar derived RMRS is
  ;;; in position 1, then the :sub2 and :comp cases are
  ;;; generally unexpected - hence use red/magenta for these
  (cond ((mrs::match-top-record-p match) +green-flipping-ink+)
	((mrs::match-rel-record-p match)
	 (let ((pred-comp-type (mrs::match-rel-record-pred-comp-status match))
	       (var-comp-type (mrs::match-rel-record-var-comp-status match)))
	   (declare (ignore var-comp-type))
	   (ecase pred-comp-type
	     (:equal +green-flipping-ink+)
	     (:sub1 +blue-flipping-ink+)
	     (:sub2 +red-flipping-ink+)
	     (:comp +magenta-flipping-ink+))))
	((mrs::match-arg-record-p  match)
	 (let ((comp-type (mrs::match-arg-record-comp-status match)))
	   (ecase comp-type
	     (:equal +green-flipping-ink+)
	     (:sub1 +blue-flipping-ink+)
	     (:sub2 +red-flipping-ink+)
	     (:comp +magenta-flipping-ink+))))
	((mrs::match-ing-record-p match) +green-flipping-ink+)
	((mrs::match-hcons-record-p match) +green-flipping-ink+)
	(t (error "Unexpected record type ~A" match))))
	


;;; New top level function for comparison calling
;;;

(defvar *selected-rmrs* nil
  "can be set from menu or via emacs")

(defvar *compared-rmrs* nil
  "can be set from menu or via emacs")

(defvar *rmrs-input-str* nil
  "just for display purposes, set by parser")

(defun select-rmrs-from-emacs (str)
  (with-package (:mrs)
    (let ((rmrs (mrs::read-single-rmrs-from-string str)))
      (when (and rmrs (mrs::rmrs-p rmrs))
	    (set-selected-rmrs-from-menu rmrs)))))
    
(defun set-selected-rmrs-from-menu (rmrs)
  (when (and rmrs (mrs::rmrs-p rmrs))
    (cond ((or (null *selected-rmrs*)
	       (and *selected-rmrs* *compared-rmrs*))
	   (setf *compared-rmrs* nil)
	   (setf *selected-rmrs* rmrs)
	   (with-output-to-top () 
	       (format t "~%Selected RMRS 1")))
	  (t (setf *compared-rmrs* rmrs)
	     (with-output-to-top ()
		 (format t "~%Selected RMRS 2"))))))


(defun compare-rmrs-interactive nil  
  (let ((rmrs1 *selected-rmrs*)
        (rmrs2 *compared-rmrs*)
        (strpos-p *pos-sensitive-rmrs-p*)
        (input (or *rmrs-input-str* "Unknown")))
    (unless (and (mrs::rmrs-p rmrs1)
		 (mrs::rmrs-p rmrs2))
      (error "No RMRSs to compare"))
    (dolist (comparison-record (mrs::compare-rmrs rmrs1 rmrs2 strpos-p))
      (show-mrs-rmrs-compare-window rmrs1 rmrs2 
				    comparison-record input))))




;;; ****** Comparison of RMRSs based on data stored in the fine system

;; code moved to rmrs/compare.lisp since it isn't actually ACL specific

;;; DMRS code

;;; 

(defun display-dmrs-from-string (str)
  (with-package (:mrs)
    (let ((dmrs (mrs::read-single-dmrs-from-string str)))
      (when (and dmrs (mrs::dmrs-p dmrs))
	(show-dmrs-ordinary-window dmrs "DMRS")))))

(defun show-dmrs-ordinary-window (dmrs title)
  (mp:run-function "DMRS ORDINARY"
		   #'show-dmrs-ordinary-window-really :dmrs dmrs :title title))

(defun show-dmrs-ordinary-window-really (&key dmrs title)
  (let ((mframe (clim:make-application-frame 'dmrs-ordinary)))
    (setf (dmrs-ordinary-dmrs mframe) 
      dmrs)
    (setf (clim:frame-pretty-name mframe) (or title "Dependency MRS"))
    (clim:run-frame-top-level mframe)))

(defun show-dmrs-ordinary (mframe stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let ((dmrs (dmrs-ordinary-dmrs mframe)))
    (mrs::layout-dmrs dmrs :clim stream)))

;;; DMRS display from parser

(defun show-mrs-dmrs-window (edge &key mrs dmrs title)
  (mp:run-function "DMRS"
    #'show-mrs-dmrs-window-really edge :mrs mrs :dmrs dmrs :title title))

(defparameter *dmrs-grammar-p* nil)

(defun show-mrs-dmrs-window-really (edge &key mrs dmrs title)
  (let ((mframe (clim:make-application-frame 'mrs-dmrs)))
    (if *dmrs-grammar-p*
	(or dmrs
	    (setf dmrs
	      (when edge
		(mrs::extract-dmrs edge))))
      (let ((mrsstruct (or mrs (when edge 
				 (mrs::extract-mrs edge)))))
	(setf (mrs-dmrs-mrsstruct mframe) 
	  mrsstruct)))
    (setf (mrs-dmrs-dmrs mframe) 
      (or dmrs
	  (let ((rmrs (mrs::mrs-to-rmrs (mrs-dmrs-mrsstruct mframe))))
	    (if rmrs
		(mrs::rmrs-to-dmrs rmrs)))))
    (setf (clim:frame-pretty-name mframe) (or title "Dependency MRS"))
    (clim:run-frame-top-level mframe)))

(defun show-mrs-dmrs (mframe stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let ((dmrs (mrs-dmrs-dmrs mframe)))
    (if dmrs
	(mrs::layout-dmrs dmrs :clim stream)
        (format stream "~%::: DMRS structure could not be extracted~%"))))

;;; layout-dmrs is in dmrs.lisp and produces an abstract 
;;; layout of the nodea and links
;;; for CLIM purposes
;;; a) the y coordinates start with 0 at the top
;;; b) the character position calculations are multiplied by 5
;;;    for the default text (this needs FIXing)
;;; c) the first arc line is at 20, subsequently at intervals of 30
;;; d) the arc labels are at the arc line +6

(defun construct-dmrs-clim-diagram (nodes links stream)
  (clim:with-output-recording-options (stream :draw nil :record t)
    (dolist (node nodes)
      (move-to-x-y stream (* 5 (mrs::dmrs-layout-node-start-x node)) 0)
      (format stream "~A~A" 
	      (mrs::dmrs-layout-node-label node)
	      (if (mrs::dmrs-layout-node-ltop node) "*" "")))
    (dolist (link links)
      (let* ((y-pos (+ 20 (* 30 (mrs::dmrs-layout-link-y link))))
	    (arrow-direction (mrs::dmrs-layout-link-direction link))
	    (left-x (+ (* 5 (mrs::dmrs-layout-link-left-x link)) 5))
	    (right-x (- (* 5 (mrs::dmrs-layout-link-right-x link)) 5))
	    (mid-point (+ left-x (truncate (- right-x left-x) 2)))
	    (label (mrs::dmrs-layout-link-label link)))
	;;; draw the label
	(move-to-x-y stream (- mid-point (* 5 (truncate (length label) 2)))
		     (+ y-pos 6))
	(format stream "~A" label)
	;;; draw the arrow or line
	(if arrow-direction
	    (clim:draw-arrow*   
	 ;;; draw-arrow* x1 y1 x2 y2 - from-head is x1 y1
	     stream
	     left-x
	     y-pos
	     right-x
	     y-pos
	     :from-head (eql arrow-direction :l)
	     :to-head (eql arrow-direction :r))
	  (clim:draw-line*   
	     stream
	     left-x
	     y-pos
	     right-x
	     y-pos))))))
