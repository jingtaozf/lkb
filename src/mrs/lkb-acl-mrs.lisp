(in-package :cl-user)

;;; MRS windows

(define-lkb-frame mrs-simple
    ((mrsstruct :initform nil
                :accessor mrs-simple-mrsstruct))
  :display-function 'show-mrs-simple
  :width *parse-window-width* 
  :height *parse-window-height*)

(defstruct mrs-type-thing value)

(define-mrs-simple-command (com-type-mrs-menu)
    ((mrs-type-thing 'mrs-type-thing :gesture :select))
  (mrs-type-thing-command mrs-type-thing))

(defun add-mrs-type-region (stream val)
  (let ((type-rec
         (make-mrs-type-thing :value val)))
    (clim:with-text-style (stream *bold*)
      (clim:with-output-as-presentation 
	  (stream type-rec 'mrs-type-thing)
	(write-string (string-downcase val) stream)))))

(defun mrs-type-thing-command (mrs-type-thing)
  (let* ((type (mrs-type-thing-value mrs-type-thing))
         (type-entry (if type (get-type-entry (if (listp type) (car type) type)))))
    (when (and type (atom type) type-entry)
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
    
(define-lkb-frame mrs-indexed
    ((mrsstruct :initform nil
	    :accessor mrs-indexed-mrsstruct))
  :display-function 'show-mrs-indexed
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


(defun show-mrs-window (edge)
  (let ((mframe (clim:make-application-frame 'mrs-simple)))
    (setf *normal* (clim:parse-text-style (make-active-fs-type-font-spec)))
    (setf *bold* (clim:merge-text-styles '(nil :bold nil) *normal*))
    (setf (mrs-simple-mrsstruct mframe) 
      (car (mrs::extract-mrs (list edge))))
    (setf (clim:frame-pretty-name mframe) "Simple MRS")
    (mp:process-run-function "Simple MRS"
                             #'clim:run-frame-top-level mframe)))

(defun show-mrs-indexed-window (edge)
  (let ((mframe (clim:make-application-frame 'mrs-indexed)))
    (setf (mrs-indexed-mrsstruct mframe) 
      (car (mrs::extract-mrs (list edge))))
    (setf (clim:frame-pretty-name mframe) "Indexed MRS")
    (mp:process-run-function "Indexed MRS"
                             #'clim:run-frame-top-level mframe)))

(defun show-mrs-scoped-window (edge)
  (let ((mframe (clim:make-application-frame 'mrs-scoped))
        (mrsstruct (car (mrs::extract-mrs (list edge)))))
    (setf (mrs-scoped-mrsstruct mframe) 
      mrsstruct)
    (setf (mrs-scoped-scoped mframe) 
      (mrs::make-scoped-mrs mrsstruct))
    (setf (clim:frame-pretty-name mframe) "Scoped MRS")
    (mp:process-run-function "Scoped MRS"
                             #'clim:run-frame-top-level mframe)))

(defun show-mrs-simple (mframe stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let ((mrsstruct (mrs-simple-mrsstruct mframe)))
    (if mrsstruct
        (clim:with-text-style (stream *normal*)
	  (clim:with-output-recording-options (stream :draw nil :record t)
            (mrs::output-mrs1 mrsstruct 'mrs::active-t stream)))
      (format stream "~%::: MRS structure could not be extracted~%"))))

(defun show-mrs-indexed (mframe stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let ((mrsstruct (mrs-indexed-mrsstruct mframe)))
    (if mrsstruct
        (clim:with-text-style (stream (lkb-parse-tree-font))
          (mrs::output-mrs1 mrsstruct 'mrs::indexed stream))
      (format stream "~%::: MRS structure could not be extracted~%"))))

(defun show-mrs-scoped (mframe stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let ((mrsstruct (mrs-scoped-mrsstruct mframe))
        (binding-sets (mrs-scoped-scoped mframe)))
    (if binding-sets
        (clim:with-text-style (stream (lkb-parse-tree-font))
          (for binding in binding-sets
               do
               (setf mrs::*canonical-bindings* (mrs::canonical-bindings binding))
               (mrs::output-scoped-mrs mrsstruct :stream stream)))
      (format stream "~%::: MRS structure does not scope~%"))))

