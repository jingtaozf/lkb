;;; Copyright (c) 1998--2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

;;; MRS windows

(define-lkb-frame mrs-simple
    ((mrsstruct :initform nil
                :accessor mrs-simple-mrsstruct))
  :display-function 'show-mrs-simple
  :width (* *parse-window-width* 1.5)
  :height (* *parse-window-height* 1.2))

(define-lkb-frame mrs-indexed
    ((mrsstruct :initform nil
	    :accessor mrs-indexed-mrsstruct))
  :display-function 'show-mrs-indexed
  :width *parse-window-width* 
  :height *parse-window-height*)

(define-lkb-frame mrs-sement
    ((mrsstruct :initform nil
	    :accessor mrs-sement-mrsstruct))
  :display-function 'show-mrs-sement
  :width *parse-window-width* 
  :height *parse-window-height*)

(define-lkb-frame mrs-sement-check
    ((messages :initform nil
	       :accessor mrs-sement-algebra-messages)
     (sement :initform nil
	    :accessor mrs-sement-algebra-sement))
  :display-function 'show-mrs-sement-messages
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

(define-lkb-frame mrs-dependencies
    ((mrsstruct :initform nil
                :accessor mrs-dependencies-mrsstruct))
  :display-function 'show-mrs-dependencies  
  :width *parse-window-width* 
  :height (round *parse-window-height* 2))

;;; XML output - eventually we want to do this generally for all windows

;;; Assume the output is to a file which is saved between calls

(define-mrs-simple-command (com-output-mrs-xml :menu "Save as XML") 
    ()
  (save-mrs-as-xml (mrs-simple-mrsstruct clim:*application-frame*)))

(define-mrs-indexed-command (com-output-mrs-indexed-xml :menu "Save as XML") 
    ()
  (save-mrs-as-xml (mrs-indexed-mrsstruct clim:*application-frame*)))

(defparameter *mrs-xml-output-file* nil)

(defun save-mrs-as-xml (mrsstruct)
  (let ((file-name (if *mrs-xml-output-file*
		       (let ((use-existing-p 
			      (lkb-y-or-n-p (format nil "Append to ~A?"
						    *mrs-xml-output-file*))))
			 (if use-existing-p
			     *mrs-xml-output-file*
			   (ask-user-for-new-pathname 
			    "New file for MRS XML dumps")))
		       (ask-user-for-new-pathname "File for MRS XML dumps"))))
    (setf *mrs-xml-output-file* file-name)
    (when file-name
      (mrs::output-mrs
       mrsstruct
       'mrs::mrs-xml file-name))))

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
        (if (stringp val)
          (format stream "~s" val)
          (format stream "~(~a~)" val))))))

(defun mrs-type-thing-command (mrs-type-thing)
  (let* ((type (mrs-type-thing-value mrs-type-thing))
         (type-entry (if type (get-type-entry type))))
    (when (and type type-entry)
      (pop-up-menu
       `(("Type hierarchy" :value hier
			    :active 
			    ,(ltype-constraint type-entry))
         #+:allegro
         ("Show source" :value source
			:active ,(source-available-p type))
	 ("Type definition" :value def
			    :active 
			    ,(ltype-constraint type-entry))
	 ("Expanded type" :value exp
			  :active ,(ltype-constraint type-entry)))
       (hier (display-type-in-tree type))
       #+:allegro
       (source (edit-source type))
       (def (show-type-spec-aux type type-entry))
       (exp (show-type-aux type type-entry))))))
    


(defun show-mrs-window (edge &optional mrs title)
  (let ((mrs (or mrs (edge-mrs edge) (mrs::extract-mrs edge))))
    (if #+:lui (lui-status-p :mrs) #-:lui nil
      (lui-display-mrs mrs)
      (mp:run-function "Simple MRS" #'show-mrs-window-really edge mrs title))))

(defun show-mrs-window-really (edge &optional mrs title)
  (let ((mframe (clim:make-application-frame 'mrs-simple)))
    (setf *normal* (clim:parse-text-style (make-active-fs-type-font-spec)))
    (setf *bold* (clim:merge-text-styles '(nil :bold nil) *normal*))
    (setf (mrs-simple-mrsstruct mframe) 
      (or mrs (and edge (mrs::extract-mrs edge))))
    (setf (clim:frame-pretty-name mframe) (or title "Simple MRS"))
    (clim:run-frame-top-level mframe)))

(defun show-mrs-indexed-window (edge &optional mrs title)
  (let ((mrs (or mrs (edge-mrs edge) (mrs::extract-mrs edge))))
    (mp:run-function "Indexed MRS"
     #'show-mrs-indexed-window-really edge mrs title)))

(defun show-mrs-indexed-window-really (edge &optional mrs title)
  (let ((mframe (clim:make-application-frame 'mrs-indexed)))
    (setf (mrs-indexed-mrsstruct mframe) 
      (or mrs (mrs::extract-mrs edge)))
    (setf (clim:frame-pretty-name mframe) (or title "Indexed MRS"))
    (clim:run-frame-top-level mframe)))

(defun show-mrs-sement-window (parse-fs edge-record title)
  (declare (ignore edge-record))
  (let ((sement (mrs::extract-sement parse-fs)))
    (mp:run-function "Sement MRS"
     #'show-mrs-sement-window-really sement title)))

(defun show-mrs-sement-window-really (sement title)
  (let ((mframe (clim:make-application-frame 'mrs-sement)))
    (setf (mrs-sement-mrsstruct mframe) 
      sement)
    (setf (clim:frame-pretty-name mframe) (or title "Sement"))
    (clim:run-frame-top-level mframe)))

(defun show-mrs-sement-check-window (parse-fs edge-record title)
  (multiple-value-bind (messages sement)
      (mrs::extract-and-check-sement parse-fs edge-record)
    (mp:run-function "Sement check"
		     #'show-mrs-sement-check-window-really 
		     messages sement title)))

(defun show-mrs-sement-check-window-really (messages sement title)
  (let ((mframe (clim:make-application-frame 'mrs-sement-check)))
    (setf (mrs-sement-algebra-messages mframe) 
      messages)
    (setf (mrs-sement-algebra-sement mframe) 
      sement)
    (setf (clim:frame-pretty-name mframe) (or title "Check algebra"))
    (clim:run-frame-top-level mframe)))


(defun show-mrs-prolog-window (edge &optional mrs title)
  (let ((mrs (or mrs (edge-mrs edge) (mrs::extract-mrs edge))))
    (mp:run-function "Prolog MRS"
     #'show-mrs-prolog-window-really edge mrs title)))

(defun show-mrs-prolog-window-really (edge &optional mrs title)
  (let ((mframe (clim:make-application-frame 'mrs-prolog)))
    (setf (mrs-prolog-mrsstruct mframe) 
      (or mrs (mrs::extract-mrs edge)))
    (setf (clim:frame-pretty-name mframe) (or title "Prolog MRS"))
    (clim:run-frame-top-level mframe)))

(defun show-mrs-scoped-window (edge &optional mrs title)
  (let ((mrs (or mrs (edge-mrs edge) (mrs::extract-mrs edge))))
    (mp:run-function "Scoped MRS"
     #'show-mrs-scoped-window-really edge mrs title)))
  
(defun show-mrs-scoped-window-really (edge &optional mrs title)
  (let ((mframe (clim:make-application-frame 'mrs-scoped))
        (mrsstruct (or mrs (mrs::extract-mrs edge))))
    (setf (mrs-scoped-mrsstruct mframe) 
      mrsstruct)
    ;;
    ;; _fix_me_
    ;; the error reporting, probably, should move into struggle-on-error() and
    ;; distinguish an additional mode, where we _do_ report but not cerror().
    ;;                                                           (5-mar-04; oe)
    (multiple-value-bind (result condition)
        (ignore-errors (mrs::make-scoped-mrs mrsstruct))
      (when condition
        (format
         #+:allegro excl:*initial-terminal-io* #-:allegro t
         "make-scoped-mrs(): `~a'~%" 
         (normalize-string (format nil "~a" condition)))
        (return-from show-mrs-scoped-window-really))
      (setf (mrs-scoped-scoped mframe) result)
      (setf (clim:frame-pretty-name mframe) (or title "Scoped MRS"))
      (clim:run-frame-top-level mframe))))


(defun show-mrs-dependencies-window (edge &optional mrs title)
  (let ((mrs (or mrs (edge-mrs edge) (mrs::extract-mrs edge))))
    (mp:run-function "Elementary Dependencies"
     #'show-mrs-dependencies-window-really edge mrs title)))
  
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

(defun show-mrs-sement (mframe stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let ((sement (mrs-sement-mrsstruct mframe)))
    (if sement
        (clim:with-text-style (stream (lkb-parse-tree-font))
          (mrs::output-algebra-sement1 sement 'mrs::simple-indexed stream))
      (format stream "~%::: Sement structure could not be extracted~%"))))

(defun show-mrs-sement-messages (mframe stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let ((messages (mrs-sement-algebra-messages mframe))
	(sement (mrs-sement-algebra-sement mframe)))
    (clim:with-text-style (stream (lkb-parse-tree-font))
      (if sement
	  (clim:with-text-style (stream (lkb-parse-tree-font))
	    (mrs::output-algebra-sement1 sement 'mrs::simple-indexed stream))
	(format stream "~%::: Sement structure could not be extracted~%"))
      (if messages
	    (dolist (message messages)
	      (format stream "~%~A" message stream))
	(format stream "~%::: Sement structure checked without problems~%")))))


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
