;;; Copyright (c) 1991-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see LICENSE for conditions


;;; parse output functions - split from parse.lsp
;;; and extensively rewritten for MCL

(in-package :lkb)


;;; *parse-tree-font-size* is in globals.lsp. This is a function so users
;;; can change font sizes after code has loaded

(defun lkb-parse-tree-font nil
   (list (if (ccl:osx-p) "Lucida Grande" "Helvetica") *parse-tree-font-size*))

(defvar *tree-records* nil)

(defvar *mid-grey-colour* (+ (ash 160 16) (ash 160 8) 160)
   ; halfway between *gray-color* and *light-gray-color*
   )


;;; active parse tree

(defstruct parse-tree-record
   start-pos end-pos node top-node-p display (clicked-p nil))


(defclass active-parse-tree-window (ccl::picture-window)
  ()
  (:default-initargs 
   :scroller-class 'active-parse-tree-window-pane))

(defclass active-parse-tree-window-pane (ccl::picture-window-pane)
   ((tree-nodes :initarg tree-nodes :initform nil :accessor tree-nodes))
   )

(defclass active-tree-pop-up-field (ccl::pop-up-field)
  ())


;;; Entry point

(defun draw-new-parse-tree (node title horizontalp)
   (let*
      ((*tree-records* nil)
       (font (lkb-parse-tree-font))
       (bold-font (cons :bold font))
       (ascent (font-info font))
       (description
          (graph-display-layout node
             #'find-children
             #'(lambda (edge-symbol)
                 (multiple-value-bind (s bold-p) 
                       (get-string-for-edge edge-symbol)
                    (string-width s (if bold-p bold-font font))))
             (font-height font)
             horizontalp))
       (max-x (graph-description-max-x description))
       (max-y (graph-description-max-y description))
       (fake-window 
          (make-instance 'picture-field-window
             :view-font font :view-size (make-point max-x max-y))))
      (graph-display-output fake-window description
         #'(lambda (str edge-symbol)
             (move str 0 ascent)
             (multiple-value-bind (s bold-p) 
                  (get-string-for-edge edge-symbol)
               (let ((start-pos (current-position str)))
                  ;; display word nodes in bold and don't record a menu for them
                  (if bold-p
                     (with-bold-output str
                        (stream-write-string str s 0 (length (the string s))))
                     (progn
                        (stream-write-string str s 0 (length (the string s)))
                        (add-active-parse-region edge-symbol s
                           start-pos (current-position str) (eq edge-symbol node)))))))
         #'(lambda (str parent child x1 y1 x2 y2 reversep)
             (declare (ignore reversep))
             (if (edge-mod-edge-p parent child)
                ;; show this as being a generator intersective modifier link
                (with-focused-view str
                   (with-fore-color *mid-grey-colour*
                      (draw-line-x-y str x1 y1 x2 y2 nil)))
                (draw-line-x-y str x1 y1 x2 y2 nil))))
      (let*
         ((pict (window-close fake-window))
          (real-window
             (make-instance 'active-parse-tree-window
                :window-title title
                :pict pict
                :view-font (lkb-dialog-font)
                :field-size (make-point max-x max-y)
                :close-box-p t
                :view-size
                (make-point
                   (min (max (+ 50 max-x) 150) (- *screen-width* 100)) 
                   (min (+ 50 max-y) (- *screen-height* 100))))))
         (setf (tree-nodes (ccl::my-scroller real-window)) *tree-records*)
         (invalidate-view real-window)
         real-window)))


(defun add-active-parse-region (node s start-pos end-pos top-node-p)
   (push
      (make-parse-tree-record :start-pos start-pos :end-pos end-pos
         :node node :top-node-p top-node-p :display s)
      *tree-records*))


;;; Pop up menus are created as separate views in the right position
;;; but only on the first click near where the parse tree node is

(defmethod view-click-event-handler ((pane active-parse-tree-window-pane) where)
  (let ((x-pos-click (point-h where))
        (y-pos-click (point-v where))
        (ascent (font-ascent pane))
        (eps 2))
    (dolist (record (tree-nodes pane))
      (when
        (let ((x-pos-node (point-h (parse-tree-record-start-pos record)))
              (y-pos-node (point-v (parse-tree-record-start-pos record))))
            (and (> y-pos-click (- y-pos-node ascent eps))
                 (< y-pos-click (+ y-pos-node eps))
                 (> x-pos-click (- x-pos-node eps))
                 (< x-pos-click
                    (+ (point-h (parse-tree-record-end-pos record)) eps))))
        (unless (parse-tree-record-clicked-p record)
           (add-subviews pane (create-parse-tree-menu record ascent))
           (setf (parse-tree-record-clicked-p record) t))
        (return nil)))
    (call-next-method pane where)))


(defun create-parse-tree-menu (record ascent)
  (let* ((menu-pos (make-point (point-h (parse-tree-record-start-pos record)) 
                               (- (point-v (parse-tree-record-start-pos record))
                                  ascent)))
         (node (parse-tree-record-node record))
         (edge-record (get node 'edge-record))
         (display (parse-tree-record-display record))
         (menu (make-instance 'active-tree-pop-up-field
                 :view-position menu-pos
                 :item-font (lkb-parse-tree-font)
                 :item-string display
                 :view-font (lkb-dialog-font))))
     (apply #'add-menu-items menu
        (create-parse-tree-menu-items
           node edge-record (parse-tree-record-top-node-p record)))
     menu))


(defmethod set-pop-up-menu-default-item ((menu active-tree-pop-up-field) num)
   ;; don't allow the menu mechanism to mark a menu item as default
   (declare (ignore num))
   nil)


(defun create-parse-tree-menu-items (edge-symbol edge-record top-edge-p)
  (list*
    (make-instance 'menu-item
      :menu-item-title
      (format nil "Feature structure - Edge ~A" (edge-id edge-record))
      :menu-item-action
      #'(lambda ()
          (display-fs (get edge-symbol 'edge-fs)
                      (format nil "Edge ~A ~A - Tree FS" 
                              (edge-id edge-record)
                              (if (g-edge-p edge-record) "G" "P")))))
    (make-instance 'menu-item
      :menu-item-title
      (format nil "Unfilled feature structure - Edge ~A" (edge-id edge-record))
      :menu-item-action
      #'(lambda ()
          (display-fs (unfilled-tdfs
                         (copy-tdfs-completely (get edge-symbol 'edge-fs))) 
                      (format nil "Edge ~A ~A - Tree Unfilled FS" 
                              (edge-id edge-record)
                              (if (g-edge-p edge-record) "G" "P")))))
    (make-instance 'dynamic-enable-menu-item
      :menu-item-title 
      "Show edge in chart"
      :menu-item-action
      #'(lambda () (display-edge-in-chart edge-record)) ; !!! in generator chart only works for lexical edges
      :enable-function
      ;; all chart windows are closed before parsing a new sentence, but this
      ;; parse window might have been hanging around so would not correspond to
      ;; a new chart window
      #'(lambda nil (front-chart-window)))
    (make-instance 'menu-item
      :menu-item-title 
      (format nil "Rule ~A" 
                  (let ((item (edge-rule edge-record)))
                     (if (rule-p item) (rule-id item) (or item ""))))
      :menu-item-action
      #'(lambda ()
         (let* ((item (edge-rule edge-record))
                (rule (and (rule-p item) item)))
               (if rule
                   (display-fs (rule-full-fs rule)
                      (format nil "~A" (rule-id rule))
                      (rule-id rule))
                   (let ((alternative (get-tdfs-given-id item)))
                      (when alternative
                         (display-fs alternative
                            (format nil "~A" item) item))))))
      :disabled (not (rule-p (edge-rule edge-record))))
    (make-instance 'menu-item
      :menu-item-title 
      (let ((str (format nil "Lex ids~{~^  ~A~}" (edge-lex-ids edge-record))))
         (if (> (length str) 253)
            (concatenate 'string (subseq str 0 252) (string (code-char 201)))
            str))
      :menu-item-action #'(lambda () nil)
      :disabled t)
    (if top-edge-p
      (list
       (make-instance 'menu-item
         :menu-item-title "Generate from edge"
         :menu-item-action
         #'(lambda ()
             (eval-enqueue
                `(really-generate-from-edge ',edge-record)))
         :disabled
         ;; would get error if select this with a generator edge
         (or (not top-edge-p) (not *mrs-loaded*) (g-edge-p edge-record)))
        (make-instance 'menu-item
          :menu-item-title "MRS"
          :menu-item-action
          #'(lambda () ; use funcall so don't get undefined function warning
             (funcall 'show-mrs-window edge-record))
          :disabled (not *mrs-loaded*))
        (make-instance 'menu-item
          :menu-item-title "Indexed MRS"
          :menu-item-action
          #'(lambda ()
             (funcall 'show-mrs-indexed-window edge-record))
          :disabled (not *mrs-loaded*))
        (make-instance 'menu-item
          :menu-item-title "Scoped MRS"
          :menu-item-action
          #'(lambda ()
             (funcall 'show-mrs-scoped-window edge-record))
          :disabled (not *mrs-loaded*))))))

      
