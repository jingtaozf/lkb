;;; Copyright (c) 2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

;;; RMRS windows

;;;    RMRS comparison - to-do
;;;
;;;    put on a menu somewhere 
;;;    ("RMRS compare" :value rmrs-compare :active ,*mrs-loaded*)
;;; 
;;;    (rmrs-compare (funcall 'show-mrs-rmrs-compare-window rmrs1 rmrs2 
;;;      comparison-record title))
;;;

(define-lkb-frame mrs-rmrs
    ((mrsstruct :initform nil
                :accessor mrs-rmrs-mrsstruct)
     (rmrs :initform nil
                :accessor mrs-rmrs-rmrs))
  :display-function 'show-mrs-rmrs  
  :width *parse-window-width* 
  :height *parse-window-height*)

;;; new compare functionality

(define-lkb-frame mrs-rmrs-compare
    ((rmrs1 :initform nil
	    :accessor mrs-rmrs-compare-rmrs1)
     (rmrs2 :initform nil
	    :accessor mrs-rmrs-compare-rmrs2)
     (comparison-record :initform nil
	    :accessor mrs-rmrs-compare-comparison-record))
  :display-function 'show-mrs-rmrs-compare
  :width (* 2 *parse-window-width*) 
  :height *parse-window-height*)

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

(defun show-mrs-rmrs-compare-window (rmrs1 rmrs2 comparison-record title)
    (mp:run-function "RMRS comparison" 
		     #'show-mrs-rmrs-compare-window-really 
		     rmrs1 rmrs2 comparison-record title))

(defun show-mrs-rmrs-compare-window-really (rmrs1 rmrs2 comparison-record
					    title)
  (let ((mframe (clim:make-application-frame 'mrs-rmrs-compare)))
    (setf *normal* (clim:parse-text-style (make-active-fs-type-font-spec)))
    (setf *bold* (clim:merge-text-styles '(nil :bold nil) *normal*))
    (setf (mrs-rmrs-compare-rmrs1 mframe) 
      rmrs1)
    (setf (mrs-rmrs-compare-rmrs2 mframe) 
      rmrs2)
    (setf (mrs-rmrs-compare-comparison-record mframe) 
      comparison-record)
    (setf (clim:frame-pretty-name mframe) (or title "RMRS comparison"))
    (clim:run-frame-top-level mframe)))


(defun show-mrs-rmrs (mframe stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let ((rmrs (mrs-rmrs-rmrs mframe)))
    (if rmrs
      (clim:with-text-style (stream (lkb-parse-tree-font))
        (mrs::output-rmrs1 rmrs 'mrs::compact stream t))
      (format stream "~%::: RMRS structure could not be extracted~%"))))

(defun show-mrs-rmrs-compare (mframe stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let ((rmrs1 (mrs-rmrs-compare-rmrs1 mframe))
	(rmrs2 (mrs-rmrs-compare-rmrs2 mframe))
	(comparison-record (mrs-rmrs-compare-comparison-record mframe)))
    (if (and rmrs1 rmrs2 comparison-record)
        (clim:with-text-style (stream *normal*)
	  (clim:with-output-recording-options (stream :draw nil :record t)
	    (let  
		((ep-pts1 (mrs::output-rmrs1 rmrs1 
					     'mrs::compact-chars stream t)))
	      (move-to-x-y stream 0 0)
	      (let ((ep-pts2
		     (mrs::output-rmrs1 rmrs2 'mrs::compact-two stream t)))
		(add-comparison-pointers stream ep-pts1 ep-pts2 
					 comparison-record)))))
      (format stream "~%::: RMRS structures could not be extracted~%"))))

(defun add-comparison-pointers (stream ep-pts1 ep-pts2 comparison-record)
  (when (and ep-pts1 ep-pts2 comparison-record)
    (loop for match in (mrs::rmrs-comparison-record-matched-rels comparison-record)
	do
	  (let* ((ep1 (mrs::match-rel-record-rel1 match))
		 (found-ep1 (find ep1 ep-pts1 :key #'mrs::rmrs-position-ep))
		 (ep2 (mrs::match-rel-record-rel2 match))
		 (found-ep2 (find ep2 ep-pts2 :key #'mrs::rmrs-position-ep)))
	    (when (and found-ep1 found-ep2)
	      (let ((ep1-pos (mrs::rmrs-position-position found-ep1))
		    (ep2-pos (mrs::rmrs-position-position found-ep2)))
		(clim:draw-line* 
		 stream
		 (+ (position-x ep1-pos) 10)
		 (+ (position-y ep1-pos) 10)
		 (- (position-x ep2-pos) 10)
		 (+ (position-y ep2-pos) 10)
		 :ink +red-flipping-ink+)))))))

;;; ************* Temporary !!!! ************************

(defparameter *rmrs-test-suite*
    '("It rained"
      "Abrams barked" 
      "The window opened"
      "Abrams chased Browne"
      "Abrams handed Browne the cigarette" 
      "Abrams handed the cigarette to Browne"
      "Abrams bet Browne a cigarette that it rained"
      "Abrams knew that it rained"
      "Abrams intended to bark" 
      "Abrams intended Browne to bark"
      "Every cat barked"
      "Every cat chased some dog"
      "My cat barked"
      "It barked"
      "The cat chased it"
      "The cat chased itself"
      "The cat chased one"
      "Mine barked"
      "That opened"
      "Cats bark"))

(defun compare-eg (egnum)
  (let* ((input (nth (- egnum 1)
			     *rmrs-test-suite*))
	(rasp-mrs 
	 (nth (- egnum 1)
	      (mrs::read-rmrs-file "xxx" :rasp)))
       (erg-mrs
	(let ((*show-parse-p* nil))
	  (do-parse-tty input)
	  (unless *parse-record*
	    (error "Parse failed"))
	  (let ((mrs (mrs::extract-mrs (car *parse-record*))))
	    (unless mrs (error "~%Can't extract MRS"))
	    (mrs::mrs-to-rmrs mrs)))))
    (dolist (comparison-record (mrs::compare-rmrs erg-mrs rasp-mrs t input))
      (show-mrs-rmrs-compare-window erg-mrs rasp-mrs 
				    comparison-record input))))


