;;; Copyright Ann Copestake 1992-1997. All Rights Reserved.
;;; No use or redistribution without permission.
;;; 

;;; MCL port
;;; split old toplevel.lsp into toplevel.lsp which should be generic CL
;;; and this file which has the commands to create the actual menu

;;; 1996 - split file again - menus.lsp is independent between ACL and MCL

;;; Top level menus etc


;;; Modified 03/93 to incorporate LKB morphology system - BEMJ
;;;
;;; mods to: set-up-lkb-interaction  (extra menu items added)
;;;          apply-lex


(defvar *lkb-menu-disabled-list* nil
  "Kludge because of MCL bug!!!!")


;;; Define make-menu-item for leaf items only

;;; eval-enqueue puts things on the toploop queue
;;; they can therefore be interupted etc

(defun make-menu-item (&key name value available-p)
  (let ((menu 
          (make-instance 'menu-item :menu-item-title  name
                        :menu-item-action 
                        #'(lambda nil (eval-enqueue `(,value))) 
                        :disabled (not available-p))))
    (unless available-p 
      (push menu *lkb-menu-disabled-list*))
    menu))

#|
;;; The following function doesn't work - there's a bug 
;;; which causes menus to be enabled when they are added to
;;; the menu bar even though menu-enabled-p still returns
;;; nil.  In this state it is not possible to disable the menu
;;; via menu-disable - see bug.lsp

 (defun make-lkb-submenu-item (&key menu-title menu-items available-p)
  (let ((menu (make-instance 'menu :menu-title menu-title
                             :menu-items menu-items)))
    (unless available-p 
      (menu-disable menu))
    menu))

;;; Alternative kludge is below
|#


(defun make-lkb-submenu-item (&key menu-title menu-items available-p)
  (let ((menu (make-instance 'menu :menu-title menu-title
                             :menu-items menu-items)))
    (unless available-p 
      (push menu *lkb-menu-disabled-list*))
    menu))

(defvar *lkb-menu* nil)

(defun expand-lkb-menu nil
  (setf *lkb-menu-type* :big)
  (set-up-lkb-interaction))

(defun shrink-lkb-menu nil
  (setf user::*lkb-menu-type* :core)
  (set-up-lkb-interaction))

(defun set-up-lkb-interaction (&optional system-type)
  (unless system-type 
    (setf system-type (or *lkb-menu-type* :core)))
  (when *lkb-menu*
   (menu-deinstall *lkb-menu*))  ; reset if we've loaded the 
                                ; LKB before in this session
  (setf *lkb-menu-disabled-list* nil)
  (ecase system-type
    (:core (create-mini-lkb-system-menu))
    (:big  (create-big-lkb-system-menu))   
;;;    (:full (create-lkb-system-menu))
;;;    (:yadu (create-yadu-system-menu))
    )
  (menu-install *lkb-menu*)
  (unless *current-grammar-load-file*
    (for submenu in *lkb-menu-disabled-list*
         do (menu-item-disable submenu)) ; get round bug by disabling after
                                        ; installation, but only if
                                        ; we don't have a grammar
    )
  (pushnew 'lkb-exit-function *lisp-cleanup-functions*))

(defun lkb-exit-function (&optional dump)
  (declare (ignore dump))
  (store-cached-lex *lexicon*)
  nil)



(defun dump-lkb nil
  (let* ((fresh-p (not *current-grammar-load-file*))
         (pathname (ask-user-for-new-pathname 
                    (format nil 
                            "File for image ~A grammar" 
                            (if fresh-p "without" "with")))))
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
  (read-psort-index-file)
  (set-up-lkb-interaction))

(defun restart-lkb-window nil
  (set-up-lkb-interaction))
        
 
(defun enable-type-interactions nil
   ;; called when a type file has been read in successfully
   ;; just enables everything!
   ;; use the kludge
   (for submenu in *lkb-menu-disabled-list*
        do (menu-item-enable submenu)))

(defun disable-type-interactions nil
   ;; called when a type file has been cleared
   ;; disables everthing that was originally disabled
   ;; use the kludge
   (for submenu in *lkb-menu-disabled-list*
        do (menu-item-disable submenu)))
 
