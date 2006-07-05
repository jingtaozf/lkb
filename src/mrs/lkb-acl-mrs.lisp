;;; Copyright (c) 1998--2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

;;; MRS windows

(define-lkb-frame mrs-ordinary
    ((mrs :initform nil
	   :accessor mrs-ordinary-mrs))
  :display-function 'show-mrs-ordinary  
  :width *parse-window-width* 
  :height *parse-window-height*)

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
    ((global-messages :initform nil
	     :accessor mrs-sement-algebra-global-messages)
     (results :initform nil
	     :accessor mrs-sement-algebra-results)
     (actual-sement :initform nil
	    :accessor mrs-sement-algebra-actual-sement))
  :display-function 'show-mrs-sement-check-results
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

;;; generate

(define-mrs-ordinary-command (com-generate-mrs-ordinary-xml :menu "Generate") 
    ()
  (generate-from-mrs-window (mrs-ordinary-mrs clim:*application-frame*)))


(define-mrs-simple-command (com-generate-mrs-xml :menu "Generate") 
    ()
  (generate-from-mrs-window (mrs-simple-mrsstruct clim:*application-frame*)))

(define-mrs-indexed-command (com-generate-mrs-indexed-xml :menu "Generate") 
    ()
  (generate-from-mrs-window (mrs-indexed-mrsstruct clim:*application-frame*)))

(defun generate-from-mrs-window (input-sem)
  (with-output-to-top ()
    (if (and input-sem (mrs::psoa-p input-sem)
	     (mrs::psoa-liszt input-sem))
	(progn
	  (close-existing-chart-windows)
	  (generate-from-mrs input-sem)
	  (show-gen-result))
      (show-message-window
       (format nil "Not valid MRS")))))



;;; for menus associated with types

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
    

;;; for menus associated with slots in sements

(define-mrs-sement-command (com-output-sement 
			    :menu "Combine with slot") 
    ()
  (mrs::select-sement-dtr-interactive 
   (mrs-sement-mrsstruct clim:*application-frame*)))

(defstruct slot-thing value)

(define-mrs-sement-command (com-slot-menu)
    ((slot-thing 'slot-thing :gesture :select))
  (let ((name (slot-thing-value slot-thing)))
    (when name
      (clim:with-application-frame (frame)
	(pop-up-menu
	 `(("Select slot" :value slot-select))
	 (slot-select (mrs::select-sement-slot-interactive 
		       name 
		       (mrs-sement-mrsstruct frame))))))))

(defun slot-region (stream name)
  ;;; called from the display functions
  (let ((pred-rec
         (make-slot-thing :value name)))
    (clim:with-text-style (stream *bold*)
      (clim:with-output-as-presentation 
	  (stream pred-rec 'slot-thing)
        (if (stringp name)
          (format stream "~s" name)
          (format stream "~(~a~)" name))))))

    
;;; windows

(defun show-mrs-window (edge &optional mrs title)
  (let ((mrs (or mrs (edge-mrs edge) (mrs::extract-mrs edge))))
    (if #+:lui (lui-status-p :mrs :simple) #-:lui nil
      (lui-display-mrs mrs title)
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
    (if #+:lui (lui-status-p :mrs :indexed) #-:lui nil
      (lui-display-mrs mrs title :indexed)
      (mp:run-function
       "Indexed MRS" #'show-mrs-indexed-window-really edge mrs title))))

(defun show-mrs-indexed-window-really (edge &optional mrs title)
  (let ((mframe (clim:make-application-frame 'mrs-indexed)))
    (setf (mrs-indexed-mrsstruct mframe) 
      (or mrs (mrs::extract-mrs edge)))
    (setf (clim:frame-pretty-name mframe) (or title "Indexed MRS"))
    (clim:run-frame-top-level mframe)))

(defun show-mrs-sement-window (parse-fs reconstructed-fs edge-record title)
  (declare (ignore edge-record))
  (let ((sement (mrs::extract-sement parse-fs reconstructed-fs)))
    (mp:run-function "Sement MRS"
		     #'show-mrs-sement-window-really sement title)))

(defun show-mrs-sement-result-window (sement)
  (mp:run-function "Sement MRS"
		   #'show-mrs-sement-window-really sement 
		   "Resulting sement"))

(defun show-mrs-rule-sement-window (rule-fs title)
  (declare (ignore edge-record))
  (let ((sement (mrs::extract-rule-sement rule-fs)))
    (mp:run-function "Sement MRS"
     #'show-mrs-sement-window-really sement title)))

(defun show-mrs-sement-window-really (sement title)
  (let ((mframe (clim:make-application-frame 'mrs-sement)))
    (setf (mrs-sement-mrsstruct mframe) 
      sement)
    (setf (clim:frame-pretty-name mframe) (or title "Sement"))
    (clim:run-frame-top-level mframe)))

(defun show-mrs-sement-check-window (parse-fs reconstructed-fs edge-record title)
  (multiple-value-bind (global-messages results actual-sement)
      (mrs::extract-and-check-sement parse-fs reconstructed-fs edge-record)
    (mp:run-function "Sement check"
		     #'show-mrs-sement-check-window-really 
		     global-messages results actual-sement title)))

(defun show-mrs-sement-check-window-really (global-messages results
					    actual-sement title)
  (let ((mframe (clim:make-application-frame 'mrs-sement-check)))
    (setf (mrs-sement-algebra-global-messages mframe) 
      global-messages)
    (setf (mrs-sement-algebra-results mframe) 
      results)
    (setf (mrs-sement-algebra-actual-sement mframe) 
      actual-sement)
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


(defun show-mrs-utool-window (edge &optional mrs title)
  (let ((mrs (or mrs (edge-mrs edge) (mrs::extract-mrs edge))))
    (mp:run-function
     "UTool MRS"
     #'show-mrs-utool-window-really mrs title)))

(defun show-mrs-utool-window-really (mrs &optional title)
  (let ((frame (clim:make-application-frame 'mrs-scoped)))
    (setf (mrs-scoped-mrsstruct frame) mrs)
    ;;
    ;; _fix_me_
    ;; the error reporting, probably, should move into struggle-on-error() and
    ;; distinguish an additional mode, where we _do_ report but not cerror().
    ;;                                                           (5-mar-04; oe)
    (multiple-value-bind (result condition)
        (ignore-errors (mt:utool-process mrs :action :solve))
      (when condition
        (format
         #+:allegro excl:*initial-terminal-io* #-:allegro t
         "utool-process(): `~a'~%" 
         (normalize-string (format nil "~a" condition)))
        (return-from show-mrs-utool-window-really))
      (setf (mrs-scoped-scoped frame) result)
      (setf (clim:frame-pretty-name frame) (or title "UTool Scoped MRS"))
      (clim:run-frame-top-level frame))))

(defun show-mrs-dependencies-window (edge &optional mrs title)
  (let ((mrs (or mrs (edge-mrs edge) (mrs::extract-mrs edge))))
    (if #+:lui (lui-status-p :mrs :dependencies) #-:lui nil
      (lui-display-mrs mrs title :dependencies)
      (mp:run-function "Elementary Dependencies"
       #'show-mrs-dependencies-window-really edge mrs title))))
  
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
          (mrs::output-algebra-sement1 sement 'mrs::active-slot stream))
      (format stream "~%::: Sement structure could not be extracted~%"))))

;;; results are reconstruction-result structures as defined in algebra.lisp
#|
(defstruct (reconstruction-result)
  match-p reconstructed-sement messages)
|#

(defun show-mrs-sement-check-results (mframe stream &key max-width max-height)
  ;;; messages all come from extract and check sement
  (declare (ignore max-width max-height))
  (let ((global-messages (mrs-sement-algebra-global-messages mframe))
	(actual-sement (mrs-sement-algebra-actual-sement mframe))
	(results (mrs-sement-algebra-results mframe)))
    (clim:with-text-style (stream (lkb-parse-tree-font))
      (dolist (message global-messages)
	(format stream "~%~A" message))
      (if results
	  (progn
	    (format stream "~%Reconstructed sement(s)~%~%")
	    (dolist (result results)
	      (let ((sement (mrs::reconstruction-result-reconstructed-sement
			     result))
		    (messages (mrs::reconstruction-result-messages
			       result)))
		(format stream "~%")
		(when sement
		  (mrs::output-algebra-sement1 
		   sement 'mrs::simple-indexed stream))
		(format stream "~%")
		(dolist (message messages)
		  (mrs::do-comparison-message message 'mrs::simple-indexed stream)))))
	(format stream "~%::: No sement structure was reconstructed~%"))
      (if actual-sement
	  (progn
	    (format stream "~%~%Extracted sement~%")
	    (mrs::output-algebra-sement1 
	     actual-sement 'mrs::simple-indexed stream))
	(format stream "~%~%No extracted sement~%")))))


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

;;; calling function (called from emacs)

(defun display-mrs-from-string (str)
  (with-package (:mrs)
    (let ((mrs (mrs::read-single-mrs-xml-from-string str)))
      (when (and mrs (mrs::psoa-p mrs))
	(show-mrs-ordinary-window mrs "MRS")))))

;;; Window from emacs

(defun show-mrs-ordinary-window (mrs title)
  (mp:run-function "MRS ORDINARY"
   #'show-mrs-ordinary-window-really :mrs mrs :title title))
  
(defun show-mrs-ordinary-window-really (&key mrs title)
  (let ((mframe (clim:make-application-frame 'mrs-ordinary)))
    (setf (mrs-ordinary-mrs mframe) 
      mrs)
    (setf (clim:frame-pretty-name mframe) (or title "MRS"))
    (clim:run-frame-top-level mframe)))

(defun show-mrs-ordinary (mframe stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let ((mrs (mrs-ordinary-mrs mframe)))
      (clim:with-text-style (stream (lkb-parse-tree-font))
        (mrs::output-mrs1 mrs 'mrs::simple stream t))))
