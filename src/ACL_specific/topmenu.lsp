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

(eval-when (compile load eval)
  (setf *default-text-style*
    (merge-text-styles '(:sans-serif nil nil) *default-text-style*))
  (export '(*lkb-top-frame* *lkb-top-stream*
            *last-directory*
            set-up-lkb-interaction 
            enable-type-interactions disable-type-interactions)))

(defvar *lkb-menu-type* :core)

(defvar *lkb-top-frame* nil)

(defvar *lkb-top-stream* nil)

(defvar *lkb-top-process* nil)

(defvar *last-directory* nil)

(defvar *complete-lisp-close* nil)

;;; Top level menus etc

(defvar *lkb-menu-disabled-list* nil
  "Kludge because of MCL bug!!!!")

(defvar *lkb-menu-grammar-file-list* nil)

(defvar *lkb-menu-mrs-list* nil)

(defvar *lkb-menu* nil)

;; Classes for abstract menus, with methods for turning them into real
;; clim or emacs menus

(defclass menu-thing () 
  ((menu-title :initarg :menu-title 
	       :type string
	       :accessor menu-title)
   (available-p :initarg :available-p
		:accessor available-p)))


(defclass menu (menu-thing) 
  ((menu-items :initarg :menu-items
	       :accessor menu-items)))

(defclass menu-item (menu-thing)
  ((menu-value :initarg :value
	       :accessor menu-value)))

;; Create a leaf menu item

(defun make-menu-item (&key name value available-p)
  (let ((menu (make-instance 'menu-item
		:menu-title name
		:value value
		:available-p available-p)))
    (unless (or (eql available-p :always) 
                (and (eql available-p :grammar-file)
                     lkb::*current-grammar-load-file*))
      (push (intern (concatenate 'string "COM-" name))
            *lkb-menu-disabled-list*))
    (when (eql available-p :grammar-file)
      (push (intern (concatenate 'string "COM-" name))
            *lkb-menu-grammar-file-list*))
    (when (eql available-p :mrs) 
      (push (intern (concatenate 'string "COM-" name))
            *lkb-menu-mrs-list*))
    menu))

;; Create a sub-menu

(defun make-lkb-submenu-item (&key menu-title menu-items available-p)
  (let ((menu (make-instance 'menu 
		:menu-title menu-title
		:menu-items menu-items
		:available-p available-p)))
    (unless (eql available-p :always)
      (push (intern (concatenate 'string "MENU-" menu-title))
	    *lkb-menu-disabled-list*))
    (when (eql available-p :mrs) 
      (push (intern (concatenate 'string "MENU-" menu-title))
            *lkb-menu-mrs-list*))
    menu))

;; Process menu description

(defgeneric construct-menu (menu type &optional rest))

(defmethod construct-menu ((menu menu) (type (eql :clim)) &optional table)
  (let ((new-table (make-command-table 
		    (intern (concatenate 'string "MENU-" (menu-title menu)))
		    :errorp nil)))
    (push new-table (command-table-inherit-from 
		     (find-command-table 'lkb-top-command-table)))
    (add-menu-item-to-command-table table
				    (menu-title menu)
				    :menu
				    new-table
				    :errorp nil)
    (mapc #'(lambda (submenu)
	      (construct-menu submenu :clim new-table))
	  (menu-items menu))))

(defmethod construct-menu ((menu menu-item) (type (eql :clim)) 
			   &optional table)
  (let ((name (intern (concatenate 'string "COM-" (menu-title menu)))))
    (eval `(define-command (,name
			    :menu ,(menu-title menu)
			    :command-table ,table) ()
	     (handler-case
		 (funcall (quote ,(menu-value menu)))
               #+:allegro
               (excl:interrupt-signal (condition)
		 (format t "~%Interrupted"))
               ;; placeholder - we need a way
               ;; of generating an interrupt which will
               ;; affect these processes
               (storage-condition (condition)
		 (format t "~%Memory allocation problem: ~A~%" condition))
               (error (condition)
		 (format t "~%Error: ~A~%" condition))
	       (serious-condition (condition)
		 (format t "~%Something nasty: ~A~%" condition)))))))

#|
(defun construct-menu (menu)
  (apply #'concatenate 'string
	 (nconc
	  (list "("
		(prin1-to-string (slot-value menu 'menu-title))
		" ")
	  (mapcar #'construct-menu-1 (slot-value menu 'menu-items))
	  (list ")"))))

(defun construct-menu-1 (menu)
  (if (lkb-menu-item-p menu)
      (apply #'concatenate 'string
	     (nconc
	      (list "("
		    (prin1-to-string (lkb-menu-item-menu-title menu))
		    " ")
	      (mapcar #'construct-menu-1 
		      (lkb-menu-item-menu-items menu))
	      (list ")")))
    (format nil "[ ~S ~A t ]"
	    (first menu)
	    (string-downcase (string (third menu))))))
|#

;; Create lkb interaction frame

(defun expand-lkb-menu nil
  (setf *lkb-menu-type* :big)
  (set-up-lkb-interaction))

(defun shrink-lkb-menu nil
  (setf *lkb-menu-type* :core)
  (set-up-lkb-interaction))

(defun set-up-lkb-interaction (&optional system-type)
  (unless system-type 
    (setf system-type (or *lkb-menu-type* :core)))
  ;; remove any old commands
  (setf *lkb-menu-disabled-list* nil)
  (setf *lkb-menu-mrs-list* nil)
  (ecase system-type
    (:core (create-mini-lkb-system-menu))
    (:big  (create-big-lkb-system-menu)))
  #|
  (:full (create-lkb-system-menu)) 
  (:yadu (create-yadu-system-menu)))
  |#
  (if (and nil #+:allegro (lep:lep-is-running))
    (set-up-emacs-interaction)
    (set-up-clim-interaction)))

(defun set-up-emacs-interaction ()
  #+:allegro
  (lkb::add-lkb-menu (construct-menu *lkb-menu*)))

(defun set-up-clim-interaction ()
  ;; Flush old commands
  (let ((table (find-command-table 'lkb-top-command-table :errorp nil)))
    (when table
      (let ((menu-items nil))
        ;; Note - removing items inside the mapping function
        ;; does not always work - hence the fudge
        (map-over-command-table-menu-items 
         #'(lambda (name char item)
             (declare (ignore char item))
             (pushnew name menu-items))
         table)
        (dolist (name menu-items)
             (remove-menu-item-from-command-table table name)))
      (map-over-command-table-commands 
       #'(lambda (name)
	   (unless (eql name 'COM-CLOSE-TO-REPLACE)
	     (remove-command-from-command-table name table)))
       table
       :inherited nil)))
  (setf (command-table-inherit-from 
	 (find-command-table 'lkb-top-command-table))
    nil)
  ;; make sure we have a way out
  (setf (command-table-inherit-from 
	 (find-command-table 'lkb-top-command-table))
    (list (make-command-table 'menu-quit :errorp nil)))    
  (define-command (com-quit :menu "Click to confirm quit"
			    :command-table menu-quit) ()
    (setq *complete-lisp-close* t)
    (frame-exit *application-frame*))
  (add-menu-item-to-command-table 'lkb-top-command-table
				  "Quit"
				  :menu 
				  'menu-quit
				  :errorp t)
  (define-command 
      (com-close-to-replace :command-table lkb-top-command-table) ()
    (frame-exit *application-frame*))
  ;; add correct menu items
  (dolist (submenu (menu-items *lkb-menu*))
    (construct-menu submenu :clim 'lkb-top-command-table))
  ;; go to it
  (start-lkb-frame))

;; Top-level CLIM frame

(define-application-frame lkb-top ()
  (standard-application-frame) 
  (:panes 
   (display
    (outlining (:thickness 1 :record-p t)
      (spacing (:thickness 1 :record-p t)  
	(scrolling (:scroll-bars :both :record-p t)
	  (make-pane 'application-pane
		     :name "lkb-pane"
		     :text-cursor nil
		     :end-of-line-action :allow
		     :borders nil
		     :background +white+
		     :foreground +black+
		     :draw t
		     :record-p t
		     :display-time t))))))
  (:layouts
   (default display))
  (:geometry :width 400 :height 200)
  (:command-table lkb-top-command-table))

(defun start-lkb-frame ()
  (let ((old-frame *lkb-top-frame*))
    (setf *lkb-top-process*
      (mp:run-function "start-lkb-frame"
                               #'run-lkb-top-menu 
                               #+:allegro
                               excl::*initial-terminal-io*
                               #-:allegro *terminal-io*))
    ;; note - if this is being called from a command in the old frame it's
    ;; important this is the last action ...
    (when old-frame
      (execute-frame-command old-frame '(com-close-to-replace)))))

(defun run-lkb-top-menu (background-stream)
  ;; define this function so that stuff can be called on exit from LKB
  (let ((frame (make-application-frame 'lkb-top)))
    (dolist (command *lkb-menu-disabled-list*)
      (setf (command-enabled command frame) nil))
    (setf *lkb-top-frame* frame)
    (setf *lkb-top-stream* (get-frame-pane *lkb-top-frame* 'display))
    ;; crude way of seeing whether this is being called when we already have a
    ;; grammar
    (when lkb::*current-grammar-load-file*
      (enable-type-interactions))
    (setf lkb::*lkb-background-stream* background-stream)
    (unwind-protect
        (run-frame-top-level frame)
      (when *complete-lisp-close*
        #+:allegro
        (excl:exit 0 :no-unwind t)
        #+:lispworks
        (lw:quit :ignore-errors-p t)
        #-(or :allegro :lispworks)
        (error "no known mechanism to shutdown Lisp (see `topmenu.lsp'")))))

#|
(defun user-exit-lkb-frame (frame)
  ;; Check if user really wants to do this.  By default, exit Lisp as
  ;; well. For stand-alone application, always exit Lisp as well.
  (if (lep:lep-is-running)
      (let ((result (lkb::ask-user-for-multiple-choice "Really exit?" 
							'Lisp 'LKB 'Cancel)))
        (cond ((eq result 'lkb) (frame-exit frame))
              ((eq result 'lisp) 
               (setf *complete-lisp-close* t)
               (frame-exit frame))
              (t nil)))
    (when (lkb::lkb-y-or-n-p "Really exit the system?")
      (setf *complete-lisp-close* t)
      (frame-exit frame))))

(defun restart-lkb-function nil
  (lkb::read-psort-index-file)
  (setf *last-directory* nil)
  (set-up-lkb-interaction))

|#

(defun restart-lkb-window nil
  (setf *last-directory* nil)
  (set-up-lkb-interaction))

#+:allegro
(defun dump-lkb nil
  (if lkb::*current-grammar-load-file*
      (progn (lkb::lkb-beep)
             (format t "~%Dump system will not work after a grammar has been loaded"))
    (let ((image-location 
           (lkb::ask-user-for-new-pathname
            (format nil 
                    "File for image (local file advised)"))))
    (when image-location
      ;;; apparently 5.0 requires that the file be .dxl
      ;;; this lets the user give another type - since they may know more
      ;;; than I do, but issues a warning message
      #+(and :allegro (version>= 5 0))
      (let ((image-type (pathname-type image-location)))
        (unless image-type 
          (setf image-location 
            (merge-pathnames image-location
                             (make-pathname :type "dxl"))))
        (when image-type
          (unless (equal image-type "dxl")
            (format t 
                    "~%Warning - image type was ~A when dxl was expected" 
                    image-type))))
      (setf excl:*restart-init-function* #'restart-lkb-window)
      (excl:dumplisp :name image-location)
      (lkb::lkb-beep)
      (format t "~%Image saved~%")
      nil))))

(defun enable-type-interactions nil
  ;; it may only work from within the application frame
  (dolist (command *lkb-menu-disabled-list*)
    (if (or lkb::*mrs-loaded* (not (member command *lkb-menu-mrs-list*)))
        (setf (command-enabled command *lkb-top-frame*) t))))

(defun disable-type-interactions nil
  ;; this is called when a type file is being redefined it may only
  ;; work from within the application frame
  (dolist (command *lkb-menu-disabled-list*)
    (unless (member command *lkb-menu-grammar-file-list*)
      (setf (command-enabled command *lkb-top-frame*) nil))))

(defun enable-grammar-reload-interactions nil
  (dolist (command *lkb-menu-grammar-file-list*)
    (setf (command-enabled command *lkb-top-frame*) t)))

(defun enable-mrs-interactions nil
  (when lkb::*mrs-loaded*
    (dolist (command *lkb-menu-mrs-list*)
      (setf (command-enabled command *lkb-top-frame*) t))))

;;; functions called from top level menu which are time
;;; consuming 
    
(defun parse-sentences-batch nil
  ;;; for MCL this can just be parse-sentences
  (mp:run-function "Batch parse" #'lkb::parse-sentences))

#|
(defun do-parse-batch nil
  ;;; for MCL this can just be do-parse
  (mp:run-function "Parse" #'lkb::do-parse))
|#

(defun do-parse-batch nil
  (lkb::do-parse))

;; Direct output to LKB window, if present

(defun invoke-with-output-to-top (body)
  (unwind-protect
      (let ((*standard-output* *lkb-top-stream*)
	    (*debug-io* *lkb-top-stream*)
	    ;; (*terminal-io* *lkb-top-stream*)
	    (*standard-input* *lkb-top-stream*)
	    (*error-output* *lkb-top-stream*)
	    (*query-io* *lkb-top-stream*)
	    (*trace-output* *lkb-top-stream*))
	(when (not (eq mp:*current-process* *lkb-top-process*))
	  (mp:process-add-arrest-reason *lkb-top-process* :output))
	(setf (stream-recording-p *standard-output*) t)
	(funcall body))
    (mp:process-revoke-arrest-reason *lkb-top-process* :output)))
