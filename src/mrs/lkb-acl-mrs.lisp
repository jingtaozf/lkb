;;; Copyright (c) 1998--2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

;;; MRS and RMRS windows

;;;    RMRS comparison - to-do
;;;
;;;    put on a menu somewhere 
;;;    ("RMRS compare" :value rmrs-compare :active ,*mrs-loaded*)
;;; 
;;;    (rmrs-compare (funcall 'show-mrs-rmrs-compare-window rmrs1 rmrs2 
;;;      comparison-record title))
;;;

(define-lkb-frame mrs-simple
    ((mrsstruct :initform nil
                :accessor mrs-simple-mrsstruct))
  :display-function 'show-mrs-simple
  :width *parse-window-width* 
  :height *parse-window-height*)

(define-lkb-frame mrs-indexed
    ((mrsstruct :initform nil
	    :accessor mrs-indexed-mrsstruct))
  :display-function 'show-mrs-indexed
  :width *parse-window-width* 
  :height *parse-window-height*)

(define-lkb-frame mrs-prolog
    ((mrsstruct :initform nil
                :accessor mrs-prolog-mrsstruct))
  :display-function 'show-mrs-prolog  
  :width *parse-window-width* 
  :height *parse-window-height*)

(define-lkb-frame mrs-scoped
    ((mrsstruct :initform nil
                :accessor mrs-scoped-mrsstruct)
     (scoped :initform nil
                :accessor mrs-scoped-scoped))
  :display-function 'show-mrs-scoped  
  :width *parse-window-width* 
  :height *parse-window-height*)

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


(define-lkb-frame mrs-dependencies
    ((mrsstruct :initform nil
                :accessor mrs-dependencies-mrsstruct))
  :display-function 'show-mrs-dependencies  
  :width *parse-window-width* 
  :height (round *parse-window-height* 2))


(defstruct mrs-type-thing value)

(define-mrs-simple-command (com-type-mrs-menu)
    ((mrs-type-thing 'mrs-type-thing :gesture :select))
  (mrs-type-thing-command mrs-type-thing))

(defun add-mrs-pred-region (stream val)
  (let ((pred-rec
         (make-mrs-type-thing :value val)))
    (clim:with-text-style (stream *bold*)
      (clim:with-output-as-presentation 
	  (stream pred-rec 'mrs-type-thing)
	(write-string (string-downcase val) stream)))))

(defun mrs-type-thing-command (mrs-type-thing)
  (let* ((type (mrs-type-thing-value mrs-type-thing))
         (type-entry (if type (get-type-entry type))))
    (when (and type type-entry)
      (pop-up-menu
       `(("Type hierarchy" :value hier
			    :active 
			    ,(type-constraint type-entry))
         ("Show source" :value source
			:active ,(source-available-p type))
	 ("Type definition" :value def
			    :active 
			    ,(type-constraint type-entry))
	 ("Expanded type" :value exp
			  :active ,(type-constraint type-entry)))
       (hier (display-type-in-tree type))
       (source (edit-source type))
       (def (show-type-spec-aux type type-entry))
       (exp (show-type-aux type type-entry))))))
    


(defun show-mrs-window (edge &optional mrs title)
  (if #+:lui (lui-status-p :mrs) #-:lui nil
    (lui-display-mrs (or mrs (mrs::extract-mrs edge)))
    (mp:run-function "Simple MRS" #'show-mrs-window-really edge mrs title)))

(defun show-mrs-window-really (edge &optional mrs title)
  (let ((mframe (clim:make-application-frame 'mrs-simple)))
    (setf *normal* (clim:parse-text-style (make-active-fs-type-font-spec)))
    (setf *bold* (clim:merge-text-styles '(nil :bold nil) *normal*))
    (setf (mrs-simple-mrsstruct mframe) 
      (or mrs (and edge (mrs::extract-mrs edge))))
    (setf (clim:frame-pretty-name mframe) (or title "Simple MRS"))
    (clim:run-frame-top-level mframe)))

(defun show-mrs-indexed-window (edge &optional mrs title)
  (mp:run-function "Indexed MRS"
   #'show-mrs-indexed-window-really edge mrs title))

(defun show-mrs-indexed-window-really (edge &optional mrs title)
  (let ((mframe (clim:make-application-frame 'mrs-indexed)))
    (setf (mrs-indexed-mrsstruct mframe) 
      (or mrs (mrs::extract-mrs edge)))
    (setf (clim:frame-pretty-name mframe) (or title "Indexed MRS"))
    (clim:run-frame-top-level mframe)))

(defun show-mrs-prolog-window (edge &optional mrs title)
  (mp:run-function "Prolog MRS"
   #'show-mrs-prolog-window-really edge mrs title))

(defun show-mrs-prolog-window-really (edge &optional mrs title)
  (let ((mframe (clim:make-application-frame 'mrs-prolog)))
    (setf (mrs-prolog-mrsstruct mframe) 
      (or mrs (mrs::extract-mrs edge)))
    (setf (clim:frame-pretty-name mframe) (or title "Prolog MRS"))
    (clim:run-frame-top-level mframe)))

(defun show-mrs-scoped-window (edge &optional mrs title)
  (mp:run-function "Scoped MRS"
   #'show-mrs-scoped-window-really edge mrs title))
  
(defun show-mrs-scoped-window-really (edge &optional mrs title)
  (let ((mframe (clim:make-application-frame 'mrs-scoped))
        (mrsstruct (or mrs (mrs::extract-mrs edge))))
    (setf (mrs-scoped-mrsstruct mframe) 
      mrsstruct)
    (setf (mrs-scoped-scoped mframe) 
      (mrs::make-scoped-mrs mrsstruct))
    (setf (clim:frame-pretty-name mframe) (or title "Scoped MRS"))
    (clim:run-frame-top-level mframe)))

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


(defun show-mrs-dependencies-window (edge &optional mrs title)
  (mp:run-function "Elementary Dependencies"
   #'show-mrs-dependencies-window-really edge mrs title))
  
(defun show-mrs-dependencies-window-really (edge &optional mrs title)
  (let ((mframe (clim:make-application-frame 'mrs-dependencies))
        (mrsstruct (or mrs (mrs::extract-mrs edge))))
    (setf (mrs-dependencies-mrsstruct mframe) 
      mrsstruct)
    (setf (clim:frame-pretty-name mframe) (or title "Elementary Dependencies"))
    (clim:run-frame-top-level mframe)))

(defun show-mrs-simple (mframe stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let ((mrsstruct (mrs-simple-mrsstruct mframe)))
    (if mrsstruct
      (if #+:lui (lui-status-p :mrs) #-:lui nil
        (lui-display-mrs mrsstruct)
        (clim:with-text-style (stream *normal*)
	  (clim:with-output-recording-options (stream :draw nil :record t)
            (mrs::output-mrs1 mrsstruct 'mrs::active-t stream))))
      (format stream "~%::: MRS structure could not be extracted~%"))))

(defun show-mrs-indexed (mframe stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let ((mrsstruct (mrs-indexed-mrsstruct mframe)))
    (if mrsstruct
        (clim:with-text-style (stream (lkb-parse-tree-font))
          (mrs::output-mrs1 mrsstruct 'mrs::indexed stream))
      (format stream "~%::: MRS structure could not be extracted~%"))))

(defun show-mrs-prolog (mframe stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let ((mrsstruct (mrs-prolog-mrsstruct mframe)))
    (if mrsstruct
        (clim:with-text-style (stream (lkb-parse-tree-font))
          (mrs::output-mrs1 mrsstruct 'mrs::prolog stream))
      (format stream "~%::: MRS structure could not be extracted~%"))))

(defun show-mrs-scoped (mframe stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let ((mrsstruct (mrs-scoped-mrsstruct mframe))
        (binding-sets (mrs-scoped-scoped mframe)))
    (if binding-sets
        (clim:with-text-style (stream (lkb-parse-tree-font))
          (loop for binding in binding-sets
               do
               (setf mrs::*canonical-bindings* (mrs::canonical-bindings binding))
               (mrs::output-scoped-mrs mrsstruct :stream stream)))
      (format stream "~%::: MRS structure does not scope~%"))))

(defun show-mrs-rmrs (mframe stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let ((rmrs (mrs-rmrs-rmrs mframe)))
    (if rmrs
      (clim:with-text-style (stream (lkb-parse-tree-font))
        (mrs::output-rmrs1 rmrs 'mrs::compact stream))
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
	      ((ep-pts1 (mrs::output-rmrs1 rmrs1 'mrs::compact stream t)))
	      (move-to-x-y stream 0 0)
	      (let ((ep-pts2
		     (mrs::output-rmrs1 rmrs2 'mrs::compact-two stream t)))
		(add-comparison-pointers stream ep-pts1 ep-pts2 
					 comparison-record)))))
      (format stream "~%::: RMRS structures could not be extracted~%"))))

(defun add-comparison-pointers (stream ep-pts1 ep-pts2 comparison-record)
  (when (and ep-pts1 ep-pts2 comparison-record)
    (clim:draw-line stream
		     (mrs::rmrs-position-position (car ep-pts1))
		     (mrs::rmrs-position-position  (car ep-pts2))
		    :ink +red-flipping-ink+)))


(defun show-mrs-dependencies (mframe stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let* ((mrsstruct (mrs-dependencies-mrsstruct mframe))
         (eds (mrs::ed-convert-psoa mrsstruct)))
    (if eds
      (let ((record (clim:with-new-output-record (stream)
                      (clim:with-text-style (stream (lkb-parse-tree-font))
                        (format stream "~a~%" eds))))
            (status (mrs::ed-suspicious-p eds))
            (orange (or (clim:find-named-color
                         "orange" (clim:frame-palette mframe) :errorp nil)
                        clim:+yellow+)))
        (cond
         ((member :cyclic status) (recolor-record record clim:+red+))
         ((member :fragmented status) (recolor-record record orange)))
        (clim:replay record stream))
      (format 
       stream 
       "~%::: Dependencies structure could not be extracted~%"))))
