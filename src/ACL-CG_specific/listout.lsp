;;; Copyright (c) 1991-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions

;;; 
;;; version for Common Graphics

(in-package :lkb)

;;; outputing active lists in a window - pass in an alist of string / data pairs,
;;; a window title, and an alist of menu command string / function pairs.
;;; Each function should take 1 argument, the data item associated with
;;; the string that is chosen

;;; dialect specific from this point

;;; *parse-tree-font-size* is in globals.lsp. This is a function so users
;;; can change font sizes after code has loaded


(defun lkb-list-font nil
   (let ((pix (cg:stream-units-per-inch (lkb-screen-stream))))
      (cg:make-font :roman :|COURIER NEW| 
        (ceiling (* (or *parse-tree-font-size* 9) pix) 72) 
        '(:bold))))

(defun draw-active-list (string-and-item-list title menu-command-and-action-list)
   (let*
      ((font (lkb-list-font))
       (line-spacing (* (cg:font-line-height font) 2))
       (max-x
          (reduce #'max string-and-item-list :key
             #'(lambda (string-and-item)
                 (cg:font-string-width font (car string-and-item)))))
       (max-y
          (* (1+ (length string-and-item-list)) line-spacing))
       (stream
               (cg:make-window (next 'listout)
               :device 'cg-user::active-list-window
               :window-interior
                  (cg:make-box
                   100 100
                   (min (+ 100 max-x) 
                     (- (cg:width (lkb-screen-stream)) 100))
                   (min (+ 100 max-y) 
                     (- (cg:height (lkb-screen-stream)) 100)))
               :page-width max-x
               :page-height max-y
               :font font
               :scrollbars t
               :title title))
       (n 0))
      (dolist (string-and-item string-and-item-list)
         (cg:move-to-x-y stream 2 (* (incf n) line-spacing))
         (let ((start-pos (current-position stream)))
            (write-string (car string-and-item) stream)
            (add-active-list-region
               string-and-item stream start-pos 
             (cg:current-position stream)
             menu-command-and-action-list)))
      (cg:invalidate stream)
      stream))


;;; menus

(defun add-active-list-region (item stream start-pos 
                                end-pos menu-command-and-action-list)
   (let ((box (create-box-for-fs-region start-pos end-pos
               stream))) ;; defined in activefs
      (cg-user::add-active-list-region-record
       item stream box menu-command-and-action-list)))

(defun perform-list-node-action (window string-and-item 
                                  menu-command-and-action-list)
   (declare (ignore window))
   (let ((menu 
            (cg:open-menu
              menu-command-and-action-list 
             ; conses are coerced into menu items
             'cg:pop-up-menu (lkb-parent-stream)
             :selection-function 
             #'(lambda (menu mitem window)
                 (declare (ignore menu window))
                 (funcall (cg:value mitem) 
                   (cdr string-and-item))))))    
      (let ((result (cg:pop-up-menu menu)))
         (close menu)
         result)))   
   
