;;; Copyright (c) 1991-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions


(in-package :lkb)

;;; Most of the functions in here are implementation specific

;;; Dialogs
;;; Some general purpose functions

(defun ask-user-for-existing-pathnames (prompt)
  ; scruffy
   (let ((pathnames nil))
      (loop 
         (let 
            ((pathname (ask-user-for-existing-pathname prompt)))
            (unless pathname (return))
            (push pathname pathnames)))
      (nreverse pathnames)))

;; clim-user:*lkb-top-frame* is set up in the ACL specific file topmenu.lsp

(defun ask-user-for-existing-pathname (prompt)
  (loop for filename = (clim:select-file clim-user:*lkb-top-frame* 
					 :title prompt
					 :directory clim-user:*last-directory*)
      do (when filename
	   (setq clim-user:*last-directory* 
             (directory-namestring (pathname filename))))
      until (or (null filename)
		(and (probe-file filename)
                     ;; Make sure file isn't really a directory
                     (pathname-name filename)))
      finally (return filename)))

(defun ask-user-for-new-pathname (prompt)
  (loop for filename = (clim:select-file clim-user:*lkb-top-frame*
					 #+:mswindows :dialog-type #+:mswindows :save
     ;;; fix for bug in Windows that disallows new file names
     ;;; undocumented feature - see release notes for 5.0.1
     ;;; (which doesn't explain what it does, but this seems to work ...)
					 :title prompt
					 :directory clim-user:*last-directory*)
      do (when filename
	   (setq clim-user:*last-directory* 
             (directory-namestring (pathname filename))))
      until (or (null filename)
		(not (probe-file filename))
                (when (clim:notify-user clim-user:*lkb-top-frame*
                                        (format nil 
                                                "File ~a exists.~%Overwrite?" 
                                                filename)
                                        :style :question)
                  (delete-file filename)))
      finally (return filename)))

(defun y-or-n-p-general (query-string)
  ;;; to avoid dialect specific stuff going in main files
  (loop
   (let ((result 
          (clim:menu-choose '(("Yes" :value chosen-yes) 
                        ("No" :value chosen-no))
                       :label query-string :associated-window
                       clim-user:*lkb-top-stream*)))
    (when result 
          (return 
           (ecase result
                  (chosen-yes t)
                  (chosen-no nil)))))))


;;; ask-for-strings-movable takes a title and a list of 
;;; prompt . initial-value pairs
;;; A dialog is built which contains two buttons
;;; :ok and :cancel and a series of non-editable editable text
;;; pairs corresponding to the argument list
;;; When the ok box is clicked the amended vales are returned
;;; when the cancel box is clicked, nil is returned
;;; The dialog box built is sized appropriately

#|
(clim:define-presentation-type cmpl (choices))

(clim:define-presentation-method clim:accept
    ((type cmpl) stream view &key)
  (declare (ignore view))
  (multiple-value-bind (obj success string)
      (clim::complete-input
       stream
       #'(lambda (string action)
	   (clim:complete-from-possibilities
	    string choices '(#\_) :name-key #'string 
	    :value-key #'string
	    :action action)))
    (print string)
    string))

(clim:define-gesture-name :complete :keyboard #\$ :unique t)
|#

(defun ask-for-strings-movable (title prompt-init-pairs 
				&optional width choices)
  (let* ((history nil)
	 (stream #+:allegro t #-:allegro clim-user:*lkb-top-stream*) ; jac
	 (result (loop for p-i-p in prompt-init-pairs
		     for count from 0
		     collect (cond ((equal (cdr p-i-p) ":CHECK-BOX")
				    nil) ; Default for checkbox is nil
				   ((and (consp (rest p-i-p))
					 (eq (second p-i-p) 
					     :TYPEIN-MENU))
				    (setf (getf history count) (cddr p-i-p))
				    (third p-i-p))
				   (t (cdr p-i-p))))))
    (restart-case
	(clim:accepting-values (stream :own-window t 
;                                       :roman 18
				       :scroll-bars (when 
							(> (length result) 10)
						      :vertical)
				       :height (when (> (length result) 10)
						 300)
				       :label title)
	  (clim:formatting-table (stream)
	    (loop for p-i-p in prompt-init-pairs
		for count from 0
		do 
		  (clim:formatting-row (stream)
		    (clim:formatting-cell (stream :align-y :center)
		      (write-string (car p-i-p) stream))
		    (clim:formatting-cell (stream :align-y :center)
		      (setf (elt result count)
			(if (typep (elt result count) 'boolean)
			    (clim:accept 'clim:boolean :stream stream
					 :default (elt result count)
					 :query-identifier count
					 :prompt nil
					 :view 'clim:toggle-button-view)
			  (if (and choices
                                   (< (length choices) 
                                      *maximum-list-pane-items*))
                              ;; AAC - FIX eventually
                              ;; - I think the code below is supposed to
                              ;; do completions, but it doesn't seem
                              ;; to work and does annoying errors
			      (clim:accept 
			       `((clim:member-sequence 
				  ,choices
				  :test equal)
				 :allow-any-input t
				 :partial-completers 
				 '(#\space #\- #\_ )
				 :name-key string
				 :value-key string)
			       :stream stream
			       :default (intern (elt result count))
			       :view (if (and choices
					      (< (length choices) 
						 *maximum-list-pane-items*))
					 'clim:list-pane-view
                                       (if (and width (numberp width))
                                           `(clim:text-field-view :width ,width)
                                         `(clim:text-field-view)))
			       :query-identifier count
			       :prompt nil)
			    (clim:accept 'string :stream stream
					 :default (elt result count)
					 :view 
                                         (if (and width (numberp width))
                                          `(clim:text-field-view :width ,width)
                                          `(clim:text-field-view))
					 :query-identifier count
					 :prompt nil)))))
		    (clim:formatting-cell (stream :align-y :center)
		      (when (getf history count)
			(clim:accept-values-command-button (stream) "Prev"
			  (let ((choice (clim:menu-choose
					 (getf history count))))
			    (when choice
			      (setf (elt result count) choice))))))))))
      (abort () ;; User selected "Cancel", so bail out
	(return-from ask-for-strings-movable nil)))
    ;; User selected "OK", so return the result
    result))

(defun ask-for-lisp-movable (title prompt-init-pairs 
			     &optional expected-width choices)
  ;; Procyon version called a special dialog item - no known equivalnet in MCL
  ;; so coerce the cdrs of the prompt-init pairs to strings and coerce the
  ;; results back to s-expressions
  (with-package (:lkb)
    (let ((new-prompt-init-pairs 
           (mapcar #'(lambda (p-i-p)
                       (cons (car p-i-p) (format nil "~S" (cdr p-i-p))))
                   prompt-init-pairs))) 
      (mapcar #'(lambda (x) 
                  (unless (equal x "") 
                    (if (stringp x)
                        (read-from-string x)
                      x)))
              (ask-for-strings-movable title new-prompt-init-pairs 
                                       expected-width choices)))))

;;; temporary for ACL - doesn't work in Windows XP

(defun ask-user-for-multiple-choice (question-string &rest args)
    (loop
     (let ((result 
          (clim:menu-choose args
                       :label question-string :associated-window
                        clim-user:*lkb-top-stream*)))
       (when result 
	 (return result)))))

;;;
;;; Print options dialog
;;;

(defvar *print-destination* :printer)
(defvar *print-orientation* :portrait)
(defvar *print-scale* nil)
(defvar *print-filename* "~")

(defun get-print-options ()
  (let ((destination *print-destination*)
	(orientation *print-orientation*)
	(scale *print-scale*)
	(file *print-filename*)
	(stream #+:allegro t #-:allegro clim-user:*lkb-top-stream*)) ; jac
    (restart-case
	(clim:accepting-values (stream :own-window t :label "Print options")
	  (clim:formatting-table (stream)
	    (clim:formatting-row (stream)
	      (clim:formatting-cell (stream :align-y :center)
		(write-string "Destination" stream))
	      (clim:formatting-cell (stream :align-y :center)
		(setq destination
		  (clim:accept '((member :printer :file)) 
			       :stream stream
			       :prompt nil
			       :view '(clim:radio-box-view)
			       :default destination)))
	      (clim:formatting-cell (stream :align-y :center)
		(clim:accept-values-command-button (stream) "File..."
		  (let ((filename 
			 (clim:select-file clim:*application-frame*
					   :directory (directory-namestring 
						       (pathname file)))))
		    (when filename
		      (setq file filename))))))
	    (clim:formatting-row (stream)
	      (clim:formatting-cell (stream :align-y :center)
		(write-string "Orientation" stream))
	      (clim:formatting-cell (stream :align-y :center)
		(setq orientation
		  (clim:accept '((member :portrait :landscape)) 
			       :stream stream
			       :prompt nil
			       :default orientation))))
	    (clim:formatting-row (stream)
	      (clim:formatting-cell (stream :align-y :center)
		(write-string "Use multiple pages?" stream))
	      (clim:formatting-cell (stream :align-y :center)
		(setq scale
		  (clim:accept 'clim:boolean
			       :stream stream
			       :prompt nil
			       :default scale))))))
      (abort () (return-from get-print-options)))
    (setq *print-destination* destination)
    (setq *print-orientation* orientation)
    (setq *print-scale* scale)
    (setq *print-filename* file)
    (values *print-destination* *print-orientation* *print-scale* 
	    *print-filename*)))

