;;; Copyright (c) 1991-2018 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see LICENSE for conditions


;;; When porting to MCL, old toplevel.lsp was split into toplevel.lsp which should
;;; be generic CL and this file which has the commands to create the actual menu

;;; ACL port - redefine menu commands
;;; split file again - menus.lsp is independent between ACL and MCL

;;; Note - this file now must be read in before any of the other
;;; CLIM files which associate menus etc with *lkb-top-frame*

(in-package :clim-user)

(eval-when (compile load eval)
  ;; the CLIM2 standard specifies *default-text-style* as a constant so it can't be
  ;; setf'ed - in McCLIM the default family is already :sans-serif
  (unless (constantp '*default-text-style*)
    (setf (symbol-value '*default-text-style*)
      (merge-text-styles (make-text-style :sans-serif nil nil) *default-text-style*)))
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
;; clim (or emacs?) menus

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
                (and (eql available-p :grammar)
                     lkb::*current-grammar-load-file*))
      (push (intern (concatenate 'string "COM-" name))
            *lkb-menu-disabled-list*))
    (when (eql available-p :grammar)
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

(defgeneric construct-menu (menu &optional rest))

(defmethod construct-menu ((menu menu) &optional table)
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
	      (construct-menu submenu new-table))
	  (menu-items menu))))

(defmethod construct-menu ((menu menu-item) &optional table)
  (let (#+:sbcl (sb-ext:*muffled-warnings* t) ; suppress COM- redefinition warnings
        (name (intern (concatenate 'string "COM-" (menu-title menu)))))
    (eval `(define-command (,name
			    :menu ,(menu-title menu)
			    :command-table ,table) ()
	     (lkb::execute-menu-command
		 (funcall (quote ,(menu-value menu)))
               (format t "~%While attempting to execute menu command ~A" ,(menu-title menu)))))))


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
    (:big  (create-big-lkb-system-menu))
    #|
    (:full (create-lkb-system-menu)) 
    (:yadu (create-yadu-system-menu))
    |#
    )
  (unless (make:getenv "LKB_GUI_EXTERNAL") ; are we running inside Trollet or similar?
    (set-up-clim-interaction)))

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
    (construct-menu submenu 'lkb-top-command-table))
  ;; go to it
  (start-lkb-frame))


;;; Top-level CLIM frame

(defclass lkb-top-pane (application-pane) ())

(define-application-frame lkb-top ()
  ()
  #+:mcclim (:menu-bar lkb-top-command-table) ; apparently not necessary in Allegro CLIM
  (:panes
    (lkb-top-pane
      (make-pane 'lkb-top-pane
		 :text-cursor nil
		 :end-of-line-action :wrap
		 :end-of-page-action :scroll
		 :borders nil
		 :background +white+
		 :foreground +black+
                 :text-style (lkb::lkb-dialog-font)
		 :display-time t)))
  (:layouts
    (default
      (scrolling (#+:mcclim :scroll-bar #-:mcclim :scroll-bars ; CLIM spec ambiguous
	 	  :both
	          :x-spacing 3)
        lkb-top-pane)))
  (:command-table #+:mcclim (lkb-top-command-table) #-:mcclim lkb-top-command-table))

(defmethod clim:frame-exit :before ((frame lkb-top)
                                    #+:allegro &rest #+:allegro keys)
  ;; !!! the &rest argument in Allegro CLIM is undocumented and conflicts with the CLIM 2 spec
  #+:allegro (declare (ignore keys))
  ;; deal gracefully with attempts to send output to a closed lkb-top frame
  (setq *lkb-top-stream* lkb::*lkb-background-stream*))

(defun start-lkb-frame ()
  (let ((old-frame *lkb-top-frame*))
    ;; put frame in the same place on the screen as old frame, if any
    (multiple-value-bind (left top)
        (if old-frame (lkb::frame-screen-boundary old-frame))
      (setq *lkb-top-frame* nil) ; the old frame is no longer LKB Top
      (setf *lkb-top-process*
        (mp:run-function "start-lkb-frame"
                         #'run-lkb-top-menu 
                         #+:allegro excl:*initial-terminal-io*
                         #-:allegro lkb::*initial-terminal-io*
                         (or left 200)
                         (if top (- top lkb::+window-manager-top-offset+) 120)))
      ;; note - if this is being called from a command in the old frame it's
      ;; important this is the last action ...
      (when old-frame
        (execute-frame-command old-frame '(com-close-to-replace))))))

(defun run-lkb-top-menu (background-stream left top)
  ;; define this function so that stuff can be called on exit from LKB
  (let ((frame
          (make-application-frame 'lkb-top
                                  :pretty-name "Lkb Top"
                                  :left left :top top
                                  :width 600 :height 250)))
    (dolist (command *lkb-menu-disabled-list*)
      (setf (command-enabled command frame) nil))
    ;; JAC - we can set *lkb-top-frame* here but not *lkb-top-stream* since there
    ;; is no guarantee that the panes have yet been attached to the frame, so
    ;; do it instead in a run-frame-top-level :before method
    (setf *lkb-top-frame* frame)
    ;; crude way of seeing whether this is being called when we already have a
    ;; grammar
    (when lkb::*current-grammar-load-file*
      (enable-type-interactions))
    (setf lkb::*lkb-background-stream* background-stream)
    (unwind-protect
        (run-frame-top-level frame)
      (when *complete-lisp-close*
        ;;
        ;; with the latest set of CLIM patches, it appears we need to rebind the
        ;; standard streams to avoid an `operation on closed stream' error(),
        ;; while shutting down the Lisp.  not quite sure why, but alas.
        ;;                                                        (8-feb-08; oe)
        #+:allegro
        (let* ((stream excl:*initial-terminal-io*)
               (*standard-output* stream)
               (*debug-io* stream)
               (*terminal-io* stream)
               (*standard-input* stream)
               (*error-output* stream)
               (*query-io* stream)
               (*trace-output* stream))
          (excl:exit 0 :no-unwind t :quiet t))
        #+:lispworks
        (lw:quit :ignore-errors-p t)
        #+:ccl
        (ccl:quit 0)
        #+:sbcl
        (sb-ext:exit)
        #-(or :allegro :lispworks :ccl :sbcl)
        (error "no known mechanism to shutdown Lisp (see `topmenu.lsp'")))))

(defmethod run-frame-top-level :before ((frame lkb-top) &key &allow-other-keys)
  ;; !!! With McCLIM and SBCL, in some unexplained circumstances the window may not draw
  ;; itself fully - in that case calling (sleep 0.2) here seems to fix the problem
  (setq *lkb-top-stream* (find-pane-named frame 'lkb-top-pane)))

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

#+(or :ccl :sbcl)
(defun dump-lkb (image-location)
  (cond
    (lkb::*current-grammar-load-file*
      (lkb::lkb-beep)
      (warn "Dump system call ignored since a grammar has been loaded"))
    #+:mcclim
    (*lkb-top-frame*
      (lkb::lkb-beep)
      (warn "Dump system call ignored since CLIM frames have been created"))
    (t
      (let ((build-date
              (string-trim '(#\newline)
                (with-output-to-string (str) (lkb::write-time-readably str)))))
        (flet
          ((restore-dumped-lkb ()
               (format t "Welcome to LKB ~A (built with ~A, ~A)~%~%"
                 cl-user::*lkb-version* (lisp-implementation-type)
                 build-date) 
               (force-output *standard-output*)
               #+:sbcl (sb-debug::enable-debugger)
               #+:sbcl (setf sb-impl::*descriptor-handlers* nil)
               #+(or :sbcl :ccl) (make::set-lkb-memory-management-parameters)
               (in-package :lkb)
               (setq *print-pretty* nil)
               (setq lkb::*initial-terminal-io* *terminal-io*)
               ;; reset portable utilities temp directory since it's machine-specific
               (uiop/stream:setup-temporary-directory)
               ;; allow McCLIM to find alternative default fonts
               #+:mcclim (mcclim-truetype::autoconfigure-fonts)
               (unless (probe-file (lkb::lkb-tmp-dir))
                 (warn "Temporary files directory ~A does not exist" (lkb::lkb-tmp-dir)))
               (lkb::start-lkb)))
          #+:ccl
          (progn
            (setq ccl::*inhibit-greeting* t)
            (setq ccl:*restore-lisp-functions* (list #'restore-dumped-lkb))
            (ccl:save-application image-location :prepend-kernel t)) ; then lisp quits
          #+:sbcl
          (progn
            (setq sb-int:*repl-read-form-fun* #'make::sbcl-repl-read-form)
            (setq sb-ext:*init-hooks* (list #'restore-dumped-lkb))
            (sb-debug::disable-debugger)
            (sb-ext:save-lisp-and-die
              (pathname image-location) :executable t :save-runtime-options t)
            (sb-ext:exit)))))))

(defun enable-type-interactions nil
  ;; it may only work from within the application frame
  (dolist (command *lkb-menu-disabled-list*)
    (if (or lkb::*mrs-loaded* (not (member command *lkb-menu-mrs-list*)))
        (setf (command-enabled command *lkb-top-frame*) t))))

(defun disable-type-interactions nil
  (when *lkb-top-frame*
    ;; this is called when a type file is being redefined it may only
    ;; work from within the application frame
    (dolist (command *lkb-menu-disabled-list*)
      (unless (member command *lkb-menu-grammar-file-list*)
	(setf (command-enabled command *lkb-top-frame*) nil)))))

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
  (mp:run-function "Batch parse" #'lkb::parse-sentences))

;; Direct ordinary output to LKB Top window, if present

(defun invoke-with-output-to-top (body)
  (multiple-value-prog1
    (unwind-protect
      (let ((*standard-output* *lkb-top-stream*)
	    (*terminal-io* *lkb-top-stream*)
            ;;
            ;; _fix_me_
            ;; we believe that debug output from the CLIM patches may cause a
            ;; force-output() on *debug-io* to raise an error(), when running 
            ;; in a background process.                        (13-feb-08; oe)
            ;;
            #-(or :logon :mcclim)
	    (*debug-io* *lkb-top-stream*) ; JAC - LKB Top inappropriate for input/debugging
            #-:mcclim
            (*trace-output* *lkb-top-stream*)
	    #-:mcclim
            (*standard-input* *lkb-top-stream*) 
	    (*error-output* *lkb-top-stream*)
	    #-:mcclim
	    (*query-io* *lkb-top-stream*))
        #+:allegro
        (when (not (eq mp:*current-process* *lkb-top-process*))
	  (mp:process-add-arrest-reason *lkb-top-process* :output))
	;; JAC - previously we had
	;; (setf (stream-recording-p *standard-output*) t)
        ;; but surely it's unnecessary since Lkb Top is always recording?
	;; In any case, if the user has closed Lkb Top then it's incorrect
        (funcall body))
      #+:allegro
      (mp:process-revoke-arrest-reason *lkb-top-process* :output))
    (force-output)))
