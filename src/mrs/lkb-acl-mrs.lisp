(in-package :lkb)

;;; MRS windows

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
    


(defun show-mrs-window (edge  &optional mrs title)
  (mp:run-function "Simple MRS"
                           #'show-mrs-window-really edge mrs title))

(defun show-mrs-window-really (edge &optional mrs title)
  (let ((mframe (clim:make-application-frame 'mrs-simple)))
    (setf *normal* (clim:parse-text-style (make-active-fs-type-font-spec)))
    (setf *bold* (clim:merge-text-styles '(nil :bold nil) *normal*))
    (setf (mrs-simple-mrsstruct mframe) 
      (or mrs (and edge (mrs::extract-mrs edge))))
    (setf (clim:frame-pretty-name mframe) (or title "Simple MRS"))
    (clim:run-frame-top-level mframe)))



(defun show-mrs-indexed-window (edge)
  (mp:run-function "Indexed MRS"
   #'show-mrs-indexed-window-really edge))

(defun show-mrs-indexed-window-really (edge)
  (let ((mframe (clim:make-application-frame 'mrs-indexed)))
    (setf (mrs-indexed-mrsstruct mframe) 
      (mrs::extract-mrs edge))
    (setf (clim:frame-pretty-name mframe) "Indexed MRS")
    (clim:run-frame-top-level mframe)))

(defun show-mrs-prolog-window (edge)
  (mp:run-function "Prolog MRS"
   #'show-mrs-prolog-window-really edge))

(defun show-mrs-prolog-window-really (edge)
  (let ((mframe (clim:make-application-frame 'mrs-prolog)))
    (setf (mrs-prolog-mrsstruct mframe) 
      (mrs::extract-mrs edge))
    (setf (clim:frame-pretty-name mframe) "Prolog MRS")
    (clim:run-frame-top-level mframe)))

(defun show-mrs-scoped-window (edge)
  (mp:run-function "Scoped MRS"
   #'show-mrs-scoped-window-really edge))
  
(defun show-mrs-scoped-window-really (edge)
  (let ((mframe (clim:make-application-frame 'mrs-scoped))
        (mrsstruct (mrs::extract-mrs edge)))
    (setf (mrs-scoped-mrsstruct mframe) 
      mrsstruct)
    (setf (mrs-scoped-scoped mframe) 
      (mrs::make-scoped-mrs mrsstruct))
    (setf (clim:frame-pretty-name mframe) "Scoped MRS")
    (clim:run-frame-top-level mframe)))

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
          (for binding in binding-sets
               do
               (setf mrs::*canonical-bindings* (mrs::canonical-bindings binding))
               (mrs::output-scoped-mrs mrsstruct :stream stream)))
      (format stream "~%::: MRS structure does not scope~%"))))

