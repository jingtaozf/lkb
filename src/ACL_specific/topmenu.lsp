>;;; Copyright Ann Copestake 1992-1997. All Rights Reserved.
;;; No use or redistribution without permission.
;;; 

;;; MCL port
;;; split old toplevel.lsp into toplevel.lsp which should be generic CL
;;; and this file which has the commands to create the actual menu

;;; ACL port - redefine menu commands
;;; split file again - menus.lsp is independent between ACL and MCL

;;; Note - this file now must be read in before any of the other
;;; CLIM files which associate menus etc with *lkb-top-frame*

(in-package :clim-user)

(eval-when
 (compile load eval)
 (export '(*lkb-top-frame* *lkb-top-stream* set-up-lkb-interaction
                           enable-type-interactions disable-type-interactions))
)

(defvar *lkb-top-frame* nil)

(defvar *lkb-top-stream* nil)

(defvar *lkb-top-process* nil)

;;; Top level menus etc

(defvar *lkb-menu-disabled-list* nil
  "Kludge because of MCL bug!!!!")

(defvar *lkb-menu* nil)

;;; near vacuous class definition, to allow compatability
;;; of menu construction with MCL

(defclass menu () ((menu-title :initarg :menu-title :type string) 
                   (menu-items :initarg :menu-items)))

;;; Define make-menu-item for leaf items only


(defun make-menu-item (&key name value available-p)
  (let ((menu 
          (list name :value value)))
    (unless available-p 
      (push (intern (concatenate 'string "COM-" name)) 
            *lkb-menu-disabled-list*))
    menu))

(defstruct (lkb-menu-item) menu-title menu-items)


(defun make-lkb-submenu-item (&key menu-title menu-items available-p)
  (let ((menu (make-lkb-menu-item :menu-title menu-title
                             :menu-items menu-items)))
    (unless available-p 
            (push (intern (concatenate 'string "COM-" menu-title)) 
                          *lkb-menu-disabled-list*))
    menu))



(defun set-up-lkb-interaction (system-type)
  (when (find-command-table 'lkb-top :errorp nil)
        (map-over-command-table-commands 
         #'(lambda (name)
             (remove-command-from-command-table name  
                           (find-command-table 'lkb-top)))
           (find-command-table 'lkb-top)
           :inherited nil))
  ; remove any old commands
  (setf *lkb-menu-disabled-list* nil)
  (ecase system-type
    (:core (create-mini-lkb-system-menu))
    (:big  (create-big-lkb-system-menu)))
;    (:full (create-lkb-system-menu))
;    (:yadu (create-yadu-system-menu)))
; in this version most of the work has to be done
; by messing around with the values in *lkb-menu*
  (lkb-menu-install *lkb-menu*) 
;  (setf *lkb-top-frame* nil)
  (start-lkb-frame))

(defun lkb-menu-install (menu)
  (eval `(define-application-frame lkb-top ()
           (clim:standard-application-frame) 
           (:panes 
	    (display
	     (clim:outlining (:thickness 1 :record-p t)
	       (clim:spacing (:thickness 1 :record-p t)  
		 (clim:scrolling (:scroll-bars :both :record-p t)
		   (clim:make-pane 'clim:application-pane
				   :name "lkb-pane"
				   :text-cursor nil
				   :end-of-line-action :allow
				   :borders nil
				   :background clim:+white+
				   :foreground clim:+black+
				   :draw t
				   :record-p t
				   :display-time t))))))
           (:layouts
            (default display))
           (:command-table lkb-top-command-table)
           (:disabled-commands ,*lkb-menu-disabled-list*)))
  ;; make sure we have a way out
  (define-command (com-quit :menu t :command-table lkb-top-command-table) ()
    (frame-exit *application-frame*))
  (dolist (submenu (slot-value menu 'menu-items))
    (if (lkb-menu-item-p submenu)
	(eval `(define-command
		   (,(intern (concatenate 'string "COM-" 
					  (lkb-menu-item-menu-title submenu)))
		    :command-table lkb-top-command-table
		    :menu ,(lkb-menu-item-menu-title submenu))
		   ()
		 (let* ((command (menu-choose 
				  (quote ,(lkb-menu-item-menu-items submenu))))) 
		   (when command
		     (handler-case
			 (funcall command)
		       (error (condition)
		       (format t  "~%Error: ~A~%" condition)))))))
      (if (and (listp submenu)
	       (stringp (car submenu))
	       (eql (cadr submenu) :VALUE))
	  (eval `(define-lkb-top-command
		     (,(intern (concatenate 'string "COM-" 
					    (car submenu)))
		      :menu ,(car submenu))
		     ()
		   (handler-case
		       (funcall (quote ,(caddr submenu)))
		     (error (condition)
		       (format t "~%Error: ~A~%" condition)))))))))


(defun start-lkb-frame ()
  (when *lkb-top-frame*
    (execute-frame-command *lkb-top-frame* 
                                '(com-quit)))
  (let ((frame (make-application-frame 'lkb-top)))
    (setf *lkb-top-frame* frame)
    (setf *lkb-top-stream* (get-frame-pane *lkb-top-frame* 'display))
    (setf *lkb-top-process*
      (mp:process-run-function "start-lkb-frame" #'run-lkb-top-menu frame))))

(defun run-lkb-top-menu (frame)
  ;;; define this function so that stuff can be called on exit
  ;;; from LKB
  (unwind-protect
      (run-frame-top-level frame)
    (user::write-psort-index-file)))

(defun restart-lkb-function nil
  (user::read-psort-index-file)
  (set-up-lkb-interaction :core)
  (enable-type-interactions))

(defun dump-lkb nil
  (let ((image-location 
         (user::ask-user-for-new-pathname 
          "File for image (local file strongly advised)")))
    (when image-location
      (setf excl:*restart-init-function* #'restart-lkb-function) 
      (user::write-psort-index-file)
      (user::clear-expanded-lex)
      (user::clear-type-cache)
      (user::unexpand-leaf-types)
      (excl:dumplisp :name image-location)
      (user::check-for-open-psorts-stream)
      (user::lkb-beep)
      (format t "~%Image saved~%")
      nil)))

(defun enable-type-interactions nil
  ;;; it may only work from within the application frame
  (dolist (command *lkb-menu-disabled-list*)
    (setf (command-enabled command *lkb-top-frame*) t)))

(defun disable-type-interactions nil
  ;;; this is called when a type file is being redefined
  ;;; it may only work from within the application frame
  (dolist (command *lkb-menu-disabled-list*)
    (setf (command-enabled command *lkb-top-frame*) nil)))

(defun parse-sentences-batch nil
  ;;; for MCL this can just be parse-sentences
  (mp:process-run-function "Batch parse" #'parse-sentences))

(defun do-parse-batch nil
  ;;; for MCL this can just be do-parse
  (mp:process-run-function "Parse" #'do-parse))

