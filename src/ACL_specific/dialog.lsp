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

;; clim-user:*lkb-top-frame* is set up in the ACL specific file frame.lsp

(defvar *last-directory* nil)

(defun ask-user-for-existing-pathname (prompt)
  ;; This still isn't right, since it allows the user to select a
  ;; directory rather than a file.
  (loop for filename = (clim:select-file clim-user:*lkb-top-frame* 
					 :title prompt
					 :directory *last-directory*)
      do (when filename
	   (setq *last-directory* (directory-namestring (pathname filename))))
      until (or (null filename)
		(probe-file filename))
      finally (return filename)))

(defun ask-user-for-new-pathname (prompt)
  ;; to match Procyon def. this isn't adequate yet - it doesn't check for
  ;; duplications and doesn't allow the user to change directory etc
  (clim:accepting-values (nil :own-window t)
   (clim:accept 'clim:pathname :prompt prompt)))

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

#+allegro-v4.3.1
(defun ask-for-strings-movable (title prompt-init-pairs &optional width)
  (let* ((history nil)
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
    (flet ((accepting-values-body (stream)
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
				   (setf (elt result count) choice)))))))))))
      (declare (dynamic-extent #'accepting-values-body))
      (restart-case
	  (clim-internals::invoke-accepting-values t #'accepting-values-body
						   :own-window t 
						   :label title)
	(abort () ;; User selected "Cancel", so bail out
	  (return-from ask-for-strings-movable nil)))
      ;; User selected "OK", so return the result
      result)))

#+allegro-v5.0
(clim:define-application-frame lkb-dialog (clim-internals::accept-values-own-window)
  ((prompt-init-pairs :initform nil
		      :accessor dialog-pairs)
   (width :initform nil
	  :accessor dialog-width))
  (:menu-bar nil)
  (:panes 
   (dialog :accept-values
	   :width :compute
	   :height :compute
	   :scroll-bars nil
	   :display-function `(clim:accept-values-pane-displayer
			       :displayer display-dialog)))
  (:layouts
   (default dialog))
  (:command-table (lkb-dialog :inherit-from (clim:accept-values-pane))))

#+allegro-v5.0
(defun ask-for-strings-movable (title prompt-init-pairs &optional width)
  (let ((frame (clim:make-application-frame 'lkb-dialog)))
    (setq x frame)
    (setf (clim:frame-pretty-name frame) title)
    (setf (dialog-pairs frame) prompt-init-pairs)
    (setf (dialog-width frame) width)
    (catch 'dialog
      (clim:run-frame-top-level frame))))

#+allegro-v5.0
(defun display-dialog (frame stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let* ((history nil)
	 (result (loop for p-i-p in (dialog-pairs frame)
		     for count from 0
		     collect (cond ((equal (cdr p-i-p) ":CHECK-BOX")
				    nil) ; Default for checkbox is nil
				   ((and (consp (rest p-i-p))
					 (eq (second p-i-p) 
					     :TYPEIN-MENU))
				    (setf (getf history count) (cddr p-i-p))
				    (third p-i-p))
				   (t (cdr p-i-p))))))
    (clim:formatting-table (stream)
      (loop for p-i-p in (dialog-pairs frame)
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
					 :width ,(dialog-width frame))
				 :query-identifier count
				 :prompt nil))))
	      (clim:formatting-cell (stream :align-y :center)
		(when (getf history count)
		  (clim:accept-values-command-button (stream) "Prev"
		    (let ((choice (clim:menu-choose
				   (getf history count))))
		      (when choice
			(setf (elt result count) choice)))))))))
    (terpri stream)
    (clim:formatting-table (stream)
      (clim:formatting-row (stream)
	(clim:formatting-cell (stream)
	  (clim:accept-values-command-button 
	      (stream :view '(clim:push-button-view 
			      :show-as-default t
			      :client frame
			      :id :exit))
	      "OK"
	    (throw 'dialog result)))
	(clim:formatting-cell (stream)
	  (clim:accept-values-command-button (stream) "Cancel"
	    (throw 'dialog nil)))))
    (terpri stream)))


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
  

#|
(ask-user-for-multiple-choice "Well?" "foobar" "wombat" "aardvark"
"arrdwolf" "weeble")

(ask-user-for-multiple-choice "Well?" 'foobar 'weeble)

(ask-user-for-multiple-choice "Well?" 1 2 3 4 5 6 7 8)

returns the lisp object corresponding to the button
|#