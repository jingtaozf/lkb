;;; Copyright (c) 1991-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions


;;; outputing active lists in a window - pass in an alist of string / data pairs,
;;; a window title, and an alist of menu command string / function pairs.
;;; Each function should take 1 argument, the data item associated with
;;; the string that is chosen

(in-package :lkb)


;;; *parse-tree-font-size* is in globals.lsp. This is a function so users
;;; can change font sizes after code has loaded

(defun lkb-list-font nil
   (list :bold "Helvetica" (or *parse-tree-font-size* 9)))


(defclass active-list-pop-up-field (ccl::pop-up-field)
  ())

(defclass active-list-window  (ccl::picture-window) () )

(defun draw-active-list (string-and-item-list title menu-command-and-action-list)
   (let*
      ((font (lkb-list-font))
       (line-spacing (* (font-height (font-info font)) 2))
       (max-x
          (reduce #'max string-and-item-list :key
             #'(lambda (string-and-item)
                 (string-width (car string-and-item) font))))
       (max-y
          (* (1+ (length string-and-item-list)) line-spacing))
       (offset-x 2)
       (fake-window 
          (make-instance 'picture-field-window
             :view-font font :view-size (make-point (+ max-x offset-x) max-y)))
       (n 0))
      (dolist (string-and-item string-and-item-list)
         (move-to fake-window offset-x (* (incf n) line-spacing))
         (let ((start-pos (current-position fake-window)))
            (write-string (car string-and-item) fake-window)
            (add-active-list-region
               string-and-item fake-window start-pos menu-command-and-action-list)))
      (let*
         ((fields (fields fake-window))
          (pict (window-close fake-window))
          (real-window
               (make-instance 'active-list-window
                  :window-title title
                  :pict pict
                  :field-size 
                  (make-point max-x max-y)
                  :view-size
                  (make-point
                     (min (max (+ 50 max-x) 150) (- *screen-width* 100)) 
                     (min (+ 50 max-y) (- *screen-height* 100)))
                  :close-box-p t
                  :view-font font)))
      (apply #'add-subviews (cons (ccl::my-scroller real-window) fields))
      (invalidate-view real-window)
      real-window)))


;;; menus

(defun add-active-list-region (item stream start-pos menu-command-and-action-list)
  (let ((menu
          (create-list-menu item
             (subtract-points start-pos (make-point 0 (font-ascent stream)))
             menu-command-and-action-list)))
    (when menu
      (push menu (fields stream)))))


(defun create-list-menu (string-and-item view-pos menu-command-and-action-list)
   (let* ((menu (make-instance 'active-list-pop-up-field
                       :view-position view-pos
                       :item-display (car string-and-item)
                       :view-font (cons :bold (lkb-type-font)))))
      (apply #'add-menu-items menu
         (pop-up-list-menu-items (cdr string-and-item) menu-command-and-action-list))
      menu))

(defmethod set-pop-up-menu-default-item ((menu active-list-pop-up-field) num)
   ;; don't allow the menu mechanism to mark a menu item as default
   (declare (ignore num))
   nil)


(defun pop-up-list-menu-items (item menu-command-and-action-list)
   (mapcar
      #'(lambda (command-and-action)
          (make-instance 'menu-item
             :menu-item-title (car command-and-action)
             :menu-item-action
             #'(lambda ()
                 (funcall (cdr command-and-action) item))))
      menu-command-and-action-list))
