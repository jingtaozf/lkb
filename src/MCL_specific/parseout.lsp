;;; Copyright Ann Copestake 1992-8 
;;; All Rights Reserved.
;;; No use or redistribution without permission.
;;; 
;;; Ann Copestake / John Carroll

;;; parse output functions - split from parse.lsp
;;; and extensively rewritten for MCL

;;; dialect specific from this point

;;; *parse-tree-font-size* is in globals.lsp. This is a function so users
;;; can change font sizes after code has loaded

(defun lkb-parse-tree-font nil
   (list "Helvetica" (or *parse-tree-font-size* 9)))


(defclass active-tree-pop-up-field (ccl::pop-up-field)
  ())

(defclass active-parse-tree-window  (ccl::picture-window) () )

(defun draw-new-parse-tree (node title horizontalp)
   (let*
      ((font (lkb-parse-tree-font))
       (ascent (font-info font))
       (description
          (graph-display-layout node
             #'(lambda (node) (get node 'daughters))
             #'(lambda (node) (string-width (get-string-for-edge node) font))
             (font-height font)
             horizontalp))
       (max-x (graph-description-max-x description))
       (max-y (graph-description-max-y description))
       (fake-window 
          (make-instance 'picture-field-window
               :view-font font :view-size #@(10000 10000))))
      (graph-display-output fake-window description
         #'(lambda (str edge-symbol)
            (move str 0 ascent)
            (multiple-value-bind (s bold-p) 
                                 (get-string-for-edge edge-symbol)
              (let ((start-pos (current-position str)))
                ;; display word nodes in bold
                (if bold-p
                   (with-bold-output str
                      (stream-write-string str s 0 (length (the string s))))
                   (stream-write-string str s 0 (length (the string s))))
                (add-active-parse-region edge-symbol str start-pos)))))
      (let*
         ((fields (fields fake-window))
         (pict (window-close fake-window))
         (real-window
               (make-instance 'active-parse-tree-window
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


(defun get-string-for-edge (edge-symbol)
   (let ((edge-record (get edge-symbol 'edge-record)))
      (if edge-record
         (values (tree-node-text-string (or 
                                    (find-category-abb (edge-dag edge-record))
                                    (edge-category edge-record))) nil)
         (values (tree-node-text-string edge-symbol) t))))


;;; find-category-abb is in tree-nodes.lsp

(defun add-active-parse-region (edge-symbol stream start-pos)
  (let ((menu
          (create-parse-tree-menu edge-symbol
             (subtract-points start-pos (make-point 0 (font-ascent stream))))))
    (when menu
      (push menu (fields stream)))))


(defun create-parse-tree-menu (edge-symbol view-pos)
  (let ((edge-record (get edge-symbol 'edge-record)))
      (if edge-record
        (let* ((menu (make-instance 'active-tree-pop-up-field
                       :view-position view-pos
                       :item-display (get-string-for-edge edge-symbol)
                       :view-font (cons :bold (lkb-type-font)))))
          (apply #'add-menu-items menu
                 (pop-up-parse-tree-menu-items edge-record))
          menu))))

(defmethod set-pop-up-menu-default-item ((menu active-tree-pop-up-field) num)
   ;; don't allow the menu mechanism to mark a menu item as default
   (declare (ignore num))
   nil)


(defun pop-up-parse-tree-menu-items (edge-record)
  (list
   (make-instance 'menu-item
     :menu-item-title "Feature structure"
     :menu-item-action
     #'(lambda ()
         (display-fs (edge-dag edge-record)
            (format nil "Edge ~A ~A - FS" (edge-id edge-record)
               (if (gen-chart-edge-p edge-record) "G" "P")))))
   (make-instance 'menu-item
     :menu-item-title (format nil "Edge ~A" (edge-id edge-record))
     :menu-item-action
     #'(lambda ()
         (display-parse-tree edge-record nil)))
   (make-instance 'menu-item
     :menu-item-title 
     (format nil "Rule ~A" 
             (or (edge-rule-number edge-record) ""))
     :disabled 
     (let ((rule-name (edge-rule-number edge-record)))
       (not (and rule-name
                 (not (stringp rule-name)))))
     :menu-item-action
     #'(lambda ()
         (let* ((rule-name (edge-rule-number edge-record))
                (rule (or (get-grammar-rule-entry rule-name)
                          (get-lex-rule-entry rule-name))))
           (when rule
             (display-fs (rule-full-fs rule)
                         (format nil "~A" rule-name)))))))) 
