;;; Copyright Ann Copestake 1992-1998. All Rights Reserved.
;;; No use or redistribution without permission.
;;; 

(in-package :cl-user)

;;; ACL-WIN port 

;;; Top level menus etc

(defvar *lkb-menu* nil)

(defvar *lkb-real-menu* nil)

(defvar *lkb-real-menu-item* nil)

(defvar *lkb-menu-disabled-list* nil)


;;; The class menu is a
;;; near vacuous class definition, to allow compatability
;;; of menu construction with MCL and CLIM so that
;;; menus can be a non-graphics specific file
;;; For instance, the first part of create-mini-lkb-system-menu
;;; is as follows

#|
(setf *lkb-menu*
   (make-instance 'menu :menu-title "Lkb" :menu-items
                  (list
                   (make-lkb-submenu-item :menu-title "Load"
                                          :menu-items
                                          (list
                                           (make-menu-item :name "Complete grammar..."
                                                           :value 'read-script-file 
                                                           :available-p t)
                                           
 |#

;;; NOTE - there is a class `menu' in CG which shouldn't
;;; be confused with this one.  Note this file is in the
;;; CL-USER package!

(defclass menu () ((menu-title :initarg :menu-title :type string) 
                   (menu-items :initarg :menu-items)))

;;; Define make-menu-item for leaf items only

(defun make-menu-item (&key name value available-p)
  (unless available-p (pushnew name *lkb-menu-disabled-list* :test #'equal))
  (aclwin:make-menu-item :name name
               :value value :available-p available-p))

;;; items which are themselves menus are created as follows

(defun make-lkb-submenu-item (&key menu-title menu-items available-p)
   (unless available-p 
      (pushnew menu-title *lkb-menu-disabled-list* :test #'equal))
   (make-menu-item :name menu-title      
     :value
     (open-top-level-menu menu-items)
     :available-p available-p))

(defun open-top-level-menu (list-of-menu-items)
   (cg:open-menu
      list-of-menu-items
      'cg:pop-up-menu
       aclwin:*lisp-main-window*
       :selection-function #'lkb-funcall-menu-item))

(defun expand-lkb-menu nil
  (setf user::*lkb-menu-type* :big)
  (set-up-lkb-interaction))

(defun shrink-lkb-menu nil
  (setf user::*lkb-menu-type* :core)
  (set-up-lkb-interaction))

(defun set-up-lkb-interaction (&optional system-type)
  (unless system-type 
    (setf system-type (or *lkb-menu-type* :core)))
;;; Create the Lkb menu and sub-menus.
;;; version to be used when Lkb is running in ACL environment
  (ecase system-type
    (:core (create-mini-lkb-system-menu))
    (:big  (create-big-lkb-system-menu)))
;    (:full (create-lkb-system-menu))
;    (:yadu (create-yadu-system-menu)))
   ; this sets the value of *lkb-real-menu* to be an instance of the
   ; real menu class
      (multiple-value-bind
       (menu menu-item)
       (cg:open-menu
        (slot-value *lkb-menu* 'menu-items) 
        'cg:pop-up-menu aclwin:*lisp-menu-bar*
        :title (slot-value *lkb-menu* 'menu-title)               
        :selection-function #'lkb-funcall-menu-item
        :add-to-menu :end)
       (setf *lkb-real-menu* menu)
       (setf *lkb-real-menu-item* menu-item))
   (when user::*ordered-type-list*
    (enable-type-interactions)))

(defun lkb-funcall-menu-item (menu item window)
   (declare (ignore menu window))
   (funcall (aclwin:menu-item-value item)))

; FIX
; ignore disabling for now and cleaning up the temp file
; also ignore dumping the image

(defun dump-lkb nil
  (format t "~%Dump-lkb is not yet implemented")
  nil)

(defun enable-type-interactions nil
  (enable-defined-interactions 
   *lkb-real-menu*))

(defun disable-type-interactions nil
  ;;; this is called when a type file is being redefined
   (disable-defined-interactions 
    *lkb-real-menu*))
     
    
(defun enable-defined-interactions (menu)
   (for menu-item in 
      (cg:menu-items menu)      
      do
      (let ((value (aclwin:menu-item-value menu-item)))
         (when value
               (setf (aclwin:menu-item-available-p menu-item) t)
                (if (cg:menup value)
                   (enable-defined-interactions value))
                ))))

(defun disable-defined-interactions (menu)
   (for menu-item in 
      (cg:menu-items menu)      
      do
      (let ((name (aclwin:menu-item-name menu-item))
            (value (aclwin:menu-item-value menu-item)))
         (when (and value
                   (member name *lkb-menu-disabled-list* 
                     :test #'equal))
               (setf (aclwin:menu-item-available-p menu-item) nil))
          (when (and value (cg:menup value))
             (disable-defined-interactions value)))))
    
(defun parse-sentences-batch nil
  ;;; for MCL this can just be parse-sentences
  (mp:process-run-function "Batch parse" #'parse-sentences))

(defun do-parse-batch nil
  ;;; for MCL this can just be do-parse
  (mp:process-run-function "Parse" #'do-parse))

