;;; Copyright Ann Copestake 1991/2. All Rights Reserved.
;;; No use or redistribution without permission.
;;; 
;;; Ann Copestake
;;; Computer Laboratory, University of Cambridge
;;; Pembroke Street
;;; Cambridge, UK

(in-package :user)

;;; Most of the functions in here are implementation specific
;;; This has been extensively modified for MCL

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

;;; Becuase it's necessary to be able to move windows around
;;; to examine entries etc before making decisions the ordinary
;;; pop-up dialogs are unsuitable


   ;; like y-or-n-p but allows windows to be moved around etc
   ;; before making the choice
   ;; If reverse-default is missing or nil then yes is the
   ;; default option - otherwise no is
   ;; request dialog is a continuous dialogue - so it appears
   ;; as soon as open-dialog is called
   ;; There are two buttons yes and no
   ;; Clicking on either of these buttons or hitting return causes the
   ;; loop to terminate and the appropriate value to be returned

(defun y-or-n-p-movable (query-string &optional reverse-default)
  ; temporary hack
  (declare (ignore reverse-default))
  (y-or-n-p-general query-string))

;;; ask-for-strings-movable takes a title and a list of 
;;; prompt . initial-value pairs
;;; A dialog is built which contains two buttons
;;; :ok and :cancel and a series of non-editable editable text
;;; pairs corresponding to the argument list
;;; When the ok box is clicked the amended vales are returned
;;; when the cancel box is clicked, nil is returned
;;; The dialog box built is sized appropriately

(defun ask-for-strings-movable (title prompt-init-pairs &optional width)
  (let* ((history nil)
	 (stream t)
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
			    (clim:accept 'boolean :stream stream
					 :default (elt result count)
					 :query-identifier count
					 :prompt nil
					 :view 'clim:toggle-button-view)
			  (clim:accept 'string :stream stream
				       :default (elt result count)
				       :view `(clim:text-field-view 
					       :width ,width)
				       :query-identifier count
				       :prompt nil))))
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

(defun ask-for-lisp-movable (title prompt-init-pairs &optional expected-width)
  ;; Procyon version called a special dialog item - no known equivalnet in MCL
  ;; so coerce the cdrs of the prompt-init pairs to strings and coerce the
  ;; results back to s-expressions
  (let ((new-prompt-init-pairs 
         (mapcar #'(lambda (p-i-p)
                     (cons (car p-i-p) (format nil "~S" (cdr p-i-p))))
		 prompt-init-pairs))) 
    (mapcar #'(lambda (x) 
		(unless (equal x "") 
		  (if (typep x 'boolean)
		      x
		    (read-from-string x))))
            (ask-for-strings-movable title new-prompt-init-pairs 
				     expected-width))))

;;; temporary for ACL

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

#+(and :allegro (not (version>= 5 0) :ignore))
(defun get-print-options ()
  (let ((destination *print-destination*)
	(orientation *print-orientation*)
	(scale *print-scale*)
	(file *print-filename*)
	(stream t))
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
		  (clim:accept 'boolean
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

