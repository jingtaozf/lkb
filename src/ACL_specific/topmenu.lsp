;;; Copyright Ann Copestake 1992-1997. All Rights Reserved.
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

(defparameter *lkb-top-frame* nil)

(defparameter *lkb-top-stream* nil)

;;; Top level menus etc

(defvar *lkb-menu-items* nil)

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
    (:core (create-mini-lkb-system-menu)))
;    (:full (create-lkb-system-menu))
;    (:yadu (create-yadu-system-menu)))
; in this version most of the work has to be done
; by messing around with the values in *lkb-menu*
  (lkb-menu-install *lkb-menu*) 
  (setf *lkb-top-frame* nil)
  (start-lkb-frame))

(defun lkb-menu-install (menu)
  (eval `(define-application-frame lkb-top ()
           (clim:standard-application-frame) 
           (:panes 
            (display :application))
           (:layouts
            (default display))
           (:command-table lkb-top-command-table)
           (:disabled-commands ,*lkb-menu-disabled-list*)))
  (define-command (com-quit :menu t :command-table lkb-top-command-table) ()
    (frame-exit *application-frame*))
; make sure we have a way out
  (cl-user::for submenu cl-user::in (slot-value menu 'menu-items)
       cl-user::do
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
  (let ((frame (or *lkb-top-frame*
                   (make-application-frame 'lkb-top))))
    (setf *lkb-top-frame* frame)
    (mp:process-run-function "start-lkb-frame" #'run-frame-top-level frame)
    (setf *lkb-top-stream* (get-frame-pane *lkb-top-frame* 'display))))



; ignore disabling for now and cleaning up the temp file
; also ignore dumping the image

(defun dump-lkb nil
  (format t "~%Dump-lkb is not yet implemented")
  nil)

(defun enable-type-interactions nil
  ;;; it may only work from within the application frame
  (cl-user::for command cl-user::in *lkb-menu-disabled-list*
      cl-user::do
      (setf (command-enabled command *lkb-top-frame*) t)))

(defun disable-type-interactions nil
  ;;; this is called when a type file is being redefined
  ;;; it may only work from within the application frame
  (cl-user::for command cl-user::in *lkb-menu-disabled-list*
      cl-user::do
      (setf (command-enabled command *lkb-top-frame*) nil)))

(defun parse-sentences-batch nil
  ;;; for MCL this can just be parse-sentences
  (mp:process-run-function "Batch parse" #'parse-sentences))

(defun do-parse-batch nil
  ;;; for MCL this can just be do-parse
  (mp:process-run-function "Parse" #'do-parse))



