;;; Copyright (c) 1991-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions


(in-package :lkb)

;;; ACL-WIN port 

;;; Top level menus etc
;;; two versions 

;;; 1) for developers' use - it adds a menu to the 
;;; toolbar, and directs output to the debug window.  Feature
;;; structure windows etc are attached to the main window as
;;; parent.  This avoids cluttering up the interface with another
;;; window
;;;
;;; 2) for the executable - it constructs an LKB window
;;; 
;;; these are distinguished by *lkb-exe-p* which should be set
;;; before making an executable

(defparameter *lkb-exe-p* nil
  "controls the appearance of the LKB interface")

(defvar *lkb-menu-type* :core)

(defvar *lkb-menu* nil)

(defvar *lkb-real-menu* nil)

(defvar *lkb-real-menu-item* nil)

(defvar *lkb-menu-disabled-list* nil)

(defvar *lkb-menu-mrs-list* nil)

(defvar *lkb-top-frame* nil
  "only relevant for the exe version")

(defun lkb-parent-stream nil
   aclwin:*lisp-main-window*)

(defun lkb-screen-stream nil
   (cg:screen cg:*system*))


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
  (unless (eql available-p :always) 
    (pushnew name *lkb-menu-disabled-list* :test #'equal))
  (when (eql available-p :mrs) 
    (pushnew name *lkb-menu-mrs-list* :test #'equal))
  (make-instance 'cg:menu-item :name name
               :value value :available-p available-p))

;;; items which are themselves menus are created as follows

(defun make-lkb-submenu-item (&key menu-title menu-items available-p)
  (unless (eql available-p :always) 
    (pushnew menu-title *lkb-menu-disabled-list* :test #'equal))
  (when (eql available-p :mrs) 
    (pushnew menu-title *lkb-menu-mrs-list* :test #'equal))
  (make-menu-item :name menu-title      
                  :value
                  (open-top-level-menu menu-items)
                  :available-p available-p))

(defun open-top-level-menu (list-of-menu-items)
   (cg:open-menu
      list-of-menu-items
      'cg:pop-up-menu
       (lkb-parent-stream)
       :selection-function #'lkb-funcall-menu-item))

(defun expand-lkb-menu nil
  (setf *lkb-menu-type* :big)
  (set-up-lkb-interaction))

(defun shrink-lkb-menu nil
  (setf *lkb-menu-type* :core)
  (set-up-lkb-interaction))

(defun set-up-lkb-interaction (&optional system-type)
  (setf *lkb-menu-disabled-list* nil)
  (setf *lkb-menu-mrs-list* nil)
  (unless system-type 
    (setf system-type (or *lkb-menu-type* :core)))
;;; Create the Lkb menu and sub-menus.
;;; version to be used when Lkb is running in ACL environment
  (ecase system-type
    (:core (create-mini-lkb-system-menu))
    (:big  (create-big-lkb-system-menu)))
   (if *lkb-exe-p* (make-lkb-top-frame)
      (make-lkb-top-menu))
   (when lkb::*current-grammar-load-file*
      (enable-type-interactions)))


(defun make-lkb-top-frame nil
   ;;; for the exe version
   ;;; still incomplete
   (unless (and *lkb-top-frame* (cg:windowp *lkb-top-frame*))
      (setf *lkb-top-frame*
            (cg:make-window 'lkbtop)))
   (setf (cg:menu *lkb-top-frame*)
         (cg:open-stream 'cg:menu-bar 
           *lkb-top-frame*
           :io
           :menu-items
           (slot-value *lkb-menu* 'menu-items)
           :title (slot-value *lkb-menu* 'menu-title)               
           :selection-function #'lkb-funcall-menu-item)))

(defun make-lkb-top-menu nil
   ;;; for the development version
   ;; this sets the value of *lkb-real-menu* to be an instance of the
   ;; real menu class
   (multiple-value-bind
    (menu menu-item)
    (cg:open-menu
     (slot-value *lkb-menu* 'menu-items) 
     'cg:pop-up-menu (development-environment::development-menu-bar cg:*system*)
     :title (slot-value *lkb-menu* 'menu-title)               
     :selection-function #'lkb-funcall-menu-item
     :add-to-menu :end)
    (when *lkb-real-menu-item*
       (setf
        (cg::menu-items
         (development-environment::development-menu-bar cg:*system*))
        (remove *lkb-real-menu-item*
          (cg::menu-items
           (development-environment::development-menu-bar cg:*system*)))))
    (setf *lkb-real-menu* menu)
    ;;; Franz bug
    ;;; - the value of menu-item ought to be set to
    ;;; a menu item, but actually the second value returned is t
    ;;; So, annoyingly, we have to set the value of *lkb-real-menu-item*
    ;;; to the last thing on the menu-bar, which should be safe enough,
    ;;; but is a bit nasty
    (setf *lkb-real-menu-item* 
          (car (last 
                 (cg::menu-items
                  (development-environment::development-menu-bar cg:*system*)))))))



(defun lkb-funcall-menu-item (menu item window)
   (declare (ignore menu window))
   (funcall (cg:value item)))

; FIX
; ignore disabling for now and cleaning up the temp file
; ignore dumping the image

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

(defun enable-mrs-interactions nil
  (when lkb::*mrs-loaded*
    (enable-defined-interactions 
     *lkb-real-menu*)))
     
    
(defun enable-defined-interactions (menu)
   (loop for menu-item in 
      (cg:menu-items menu)      
      do
      (let ((name (cg:name menu-item))
            (value (cg:value menu-item)))
        (when value
          (when (or lkb::*mrs-loaded* 
                    (not (member name *lkb-menu-mrs-list*)))
            (setf (cg:available menu-item) t)
            (if (cg:menup value)
                (enable-defined-interactions value))
            )))))

(defun disable-defined-interactions (menu)
   (loop for menu-item in 
      (cg:menu-items menu)      
      do
      (let ((name (cg:name menu-item))
            (value (cg:value menu-item)))
         (when (and value
                   (member name *lkb-menu-disabled-list* 
                     :test #'equal))
               (setf (cg:available menu-item) nil))
          (when (and value (cg:menup value))
            (disable-defined-interactions value)))))

;;; functions called from top level menu which are time
;;; consuming 
    
(defun parse-sentences-batch nil
  ;;; for MCL this can just be parse-sentences
  (mp:process-run-function "Batch parse" #'parse-sentences))



