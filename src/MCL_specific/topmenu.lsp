;;; Copyright (c) 1991-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions


;;; MCL port
;;; split old toplevel.lsp into toplevel.lsp which should be generic CL
;;; and this file which has the commands to create the actual menu

;;; 1996 - split file again - menus.lsp is independent between ACL and MCL

;;; Top level menus etc


;;; Modified 03/93 to incorporate LKB morphology system - BEMJ
;;;
;;; mods to: set-up-lkb-interaction  (extra menu items added)
;;;          apply-lex

(in-package :lkb)


(defvar *lkb-menu-type* :core)

(defvar *lkb-menu-grammar-file-list* nil)

(defvar *lkb-menu-mrs-list* nil)

;;; Define make-menu-item for leaf items only

;;; eval-enqueue puts things on the toploop queue
;;; they can therefore be interupted etc

(defun make-menu-item (&key name value (available-p :grammar))
  (let* ((available-p
          (if (fboundp value) available-p nil)) ; function may not exist
         (menu 
          (make-instance 'menu-item :menu-item-title  name
                        :menu-item-action 
                        #'(lambda nil (eval-enqueue `(,value))) 
                        :disabled (not (eql available-p :always)))))
    (when (eql available-p :grammar)
      (push menu *lkb-menu-grammar-file-list*))
    (when (eql available-p :mrs) 
      (push menu *lkb-menu-mrs-list*))
    menu))


 (defun make-lkb-submenu-item (&key menu-title menu-items (available-p :grammar))
  (let ((menu (make-instance 'menu :menu-title menu-title
                             :menu-items menu-items)))
    (unless (eql available-p :always)
      (menu-disable menu))
    (when (eql available-p :grammar) 
      (push menu *lkb-menu-grammar-file-list*))
    (when (eql available-p :mrs) 
      (push menu *lkb-menu-mrs-list*))
    menu))


(defvar *lkb-menu* nil)

(defun expand-lkb-menu nil
  (setf *lkb-menu-type* :big)
  (set-up-lkb-interaction))

(defun shrink-lkb-menu nil
  (setf *lkb-menu-type* :core)
  (set-up-lkb-interaction))

(defun set-up-lkb-interaction (&optional system-type)
  (unless system-type 
    (setf system-type (or *lkb-menu-type* :core)))
  (when *lkb-menu*
   (menu-deinstall *lkb-menu*)) ; reset if we've loaded the 
                                ; LKB before in this session
  (setq *lkb-menu-grammar-file-list* nil)
  (setq *lkb-menu-mrs-list* nil)
  (ecase system-type
    (:core (create-mini-lkb-system-menu))
    (:big  (create-big-lkb-system-menu))   
;;;    (:full (create-lkb-system-menu))
;;;    (:yadu (create-yadu-system-menu))
    )
  (menu-install *lkb-menu*)
  (disable-type-interactions)
  (when *current-grammar-load-file*
     (enable-type-interactions))
  (pushnew 'lkb-exit-function *lisp-cleanup-functions*))

(defun lkb-exit-function (&optional dump)
  (declare (ignore dump))
  nil)


(defun dump-lkb (&optional file)
  (let* ((fresh-p (not *current-grammar-load-file*))
         (pathname (if file (pathname file)
                     (ask-user-for-new-pathname 
                       (format nil 
                            "File for image ~A grammar" 
                            (if fresh-p "without" "with"))))))
    (when pathname
      (pushnew (if fresh-p 'restart-lkb-window 'lkb-restart-function)
               *lisp-startup-functions*)
      (clear-expanded-lex)
      (clear-type-cache)
      (unexpand-leaf-types)
      (pushnew 'lkb-exit-function *lisp-cleanup-functions*)
      (save-application pathname
         :excise-compiler (not (y-or-n-p-general "Include lisp compiler in image?")))
      ;; lisp quits now so no tidying up to do
      )))

(defun lkb-restart-function nil
  ;;(read-psort-index-file)
  (set-up-lkb-interaction)
  ;; crude way of seeing whether this is being 
  ;; called when we already have a grammar
  (when *current-grammar-load-file*
    (enable-type-interactions))
  )

(defun restart-lkb-window nil
  (set-up-lkb-interaction))
        
 
(defun enable-type-interactions nil
   ;; called when a type file has been read in successfully
   (enable-grammar-reload-interactions)
   (enable-mrs-interactions))

(defun disable-type-interactions nil
   ;; called when a type file has been cleared
   ;; disables everthing that was originally disabled
   (dolist (submenu *lkb-menu-grammar-file-list*)
      (menu-item-disable submenu))
   (dolist (submenu *lkb-menu-mrs-list*)
      (menu-item-disable submenu)))


(defun enable-grammar-reload-interactions nil
   ;; called when a script file load has been attempted
   (dolist (submenu *lkb-menu-grammar-file-list*)
      (menu-item-enable submenu)))

(defun enable-mrs-interactions nil
  (when lkb::*mrs-loaded*
    (dolist (submenu *lkb-menu-mrs-list*)
      (menu-item-enable submenu))))


;;; functions called from top level menu which are time
;;; consuming - don't need to do anything special for MCL?

(defun parse-sentences-batch nil
  (parse-sentences))

(defun do-parse-batch nil
  (do-parse))

