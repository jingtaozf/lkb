;;; Copyright Ann Copestake 1992-8 
;;; All Rights Reserved.
;;; No use or redistribution without permission.
;;; 
;;; This version for Allegro 5.0 on Windows NT/95/98 is
;;; partly based on the version for Allegro 3.0.1 by
;;; Anthony Hull
 
(in-package :lkb)

;;; following mentioned in graph.lsp - eventually to be defined
;;; in the right place

(defvar *chart-display* nil)

;;; parse output functions

;;; dialect specific from this point

;;; *parse-tree-font-size* is in globals.lsp. This is a function so users
;;; can change font sizes after code has loaded


(defun lkb-parse-tree-font nil
   (let ((pix (cg:stream-units-per-inch (lkb-screen-stream))))
      (cg:make-font :roman :|COURIER NEW| 
        (ceiling (* (or *parse-tree-font-size* 9) pix) 72) nil)))

(defun draw-new-parse-tree (node title horizontalp)
   (let*
      ((font (lkb-parse-tree-font))
       (description
          (graph-display-layout node
             #'find-children
             #'(lambda (node) (cg:font-string-width font
                                (get-string-for-edge node) ))
             (cg:font-size font)
             horizontalp))
       (max-x (graph-description-max-x description))
       (max-y (graph-description-max-y description))
       (stream
               (cg:open-stream 'cg-user::active-parse-tree-window
                  (lkb-parent-stream) :io :title title
                 :scrollbars t
                 :font font
                  :window-interior
                  (cg:make-box 100 100 (min (+ 100 max-x) 600)
                     (min (+ 100 max-y) 300))
                  :page-width max-x
                  :page-height max-y)))
    (graph-display-output stream description
         #'(lambda (str edge-symbol)
            (multiple-value-bind (s bold-p) 
                                 (get-string-for-edge edge-symbol)
              (let ((start-pos (current-position str)))
                ;; display word nodes in bold
                (if bold-p
                   (with-bold-output str
                      (cg::device-write-string str s 0 (length (the string s))))
                   ;;; FIX - device-write-string is no longer external
                   (cg::device-write-string str s 0 (length (the string s))))
                (add-active-parse-region edge-symbol str start-pos 
                  (current-position str))))))))



;;; menus

(defun add-active-parse-region (edge-symbol stream start-pos end-pos)
   (let ((box (create-box-for-fs-region start-pos end-pos
               stream))) ;; defined in activefs
      (cg-user::add-parse-tree-region-record
       stream box edge-symbol)))

(defun perform-view-node-action (stream edge-symbol)
   (let ((edge-record (get edge-symbol 'edge-record)))
      (if edge-record
         (pop-up-parse-tree-menu stream edge-symbol edge-record))))

(defun pop-up-parse-tree-menu (stream edge-symbol edge-record)
   (declare (ignore stream))
   (let ((menu 
            (cg:open-menu
               (list
                  (make-instance 'cg:menu-item :name (format nil 
                                          "Feature structure - Edge ~A" 
                                          (edge-id edge-record))
                     :value 
                     #'(lambda ()
                         (display-fs (get edge-symbol 'edge-fs)
                           (format nil "Edge ~A ~A - Tree FS" 
                             (edge-id edge-record)
                             (if (g-edge-p edge-record) 
                                "G" 
                                "P")))
                         (display-edge-in-chart edge-record)))
                (make-instance 'cg:menu-item 
                  :name (format nil "Rule ~A" 
                          (let ((item (edge-rule edge-record)))
                          	 (if (rule-p item) (rule-id item) (or item ""))))                  
                  :available-p 
                     (let ((item (edge-rule edge-record)))
                           (and item (not (stringp item))))
                     :value 
                     #'(lambda ()
                         (let* ((item (edge-rule edge-record))
                                (rule (and (rule-p item) item)))
                            (if rule
                               (display-fs (rule-full-fs rule)
                                  (format nil "~A" (rule-id rule)))
                               (let ((alternative (get-tdfs-given-id item)))
                                  (when alternative
                                     (display-fs alternative
                                        (format nil "~A" item)))))))))  
             'cg:pop-up-menu (lkb-parent-stream)
             :selection-function #'lkb-funcall-menu-item)))
      (let ((result (cg:pop-up-menu menu)))
         (close menu)
         result)))

