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

;;; clim-user:*lkb-top-frame* is set up in the ACL specific file
;;; frame.lsp

(defun ask-user-for-existing-pathname (prompt)
  ; to match Procyon def
  ; but isn't certain to return a valid file ...
  (let ((filename 
           (clim:select-file clim-user:*lkb-top-frame* :title prompt)))
        filename))

(defun ask-user-for-new-pathname (prompt)
  ; to match Procyon def
  ; this isn't adequate yet - it doesn't check for duplications
  ; and doesn't allow the user to change directory etc
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
;;; when the cancel box is clicked the original defaults are returned
;;; The dialog box built is sized appropriately

;;; the following, extraordinarily messy function, is due
;;; to accepting-values being a macro


(defun ask-for-strings-movable (title prompt-init-pairs 
				&optional expected-width)
  (declare (special *temp-result* *abort-query*))
  (setf *abort-query* nil)
  (setf *temp-result* (loop for p-i-p in prompt-init-pairs
			  collect (cond ((equal (cdr p-i-p) ":CHECK-BOX")
					 nil) ; Default for checkbox is nil
					((and (consp (rest p-i-p))
					      (eq (second p-i-p) :TYPEIN-MENU))
					 (third p-i-p))
					(t (cdr p-i-p)))))
  ;; this has to be a special, because eval doesn't take any notice of lexical
  ;; environment
  (let* ((count 0)
	 (accepting-values-body
	  (for p-i-p in prompt-init-pairs
	       append
	       (incf count)
	       (list '(terpri stream)
		     `(setf (elt *temp-result* ,(- count 1))
			(if (typep (elt *temp-result* ,(- count 1)) 'boolean)
			    (clim:accept 'boolean :stream stream
					 :default 
					 (elt *temp-result* ,(- count 1))
					 :prompt ,(car p-i-p)
					 :view 'clim:toggle-button-view)
			  (clim:accept 'string :stream stream
				       :default 
				       (elt *temp-result* ,(- count 1))
				       :view '(clim:text-field-view :width ,expected-width)
				       :prompt ,(car p-i-p))))))))
	 
    (eval
     `(let ((stream t))
	(restart-case
	    ,(cons 'clim:accepting-values  
		   (cons `(stream :own-window t 
				  :label ,title
				  :align-prompts :left)
                         (cdr accepting-values-body)))
	  (abort () (setf *abort-query* t) nil))
	(if *abort-query* nil *temp-result*)))))
     

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