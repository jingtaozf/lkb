;;; Copyright (c) 1998-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions


(in-package :lkb)


;;; use *parse-tree-font-size* from globals.lsp. These are functions so users
;;; can change font sizes after code has loaded

(defun lkb-chart-font nil
   (list (if (ccl:osx-p) "Lucida Grande" "Helvetica") *parse-tree-font-size*))

(defun lkb-chart-bold-font nil
   (list (if (ccl:osx-p) "Lucida Grande" "Helvetica") *parse-tree-font-size* :bold))


(defvar *chart-font* nil)

(defvar *chart-bold-font* nil)

(defvar *chart-display* nil)

(defvar *chart-records* nil)

(defstruct chart-record
   position
   width
   edge)

(defclass active-chart-scroll-bar (ccl::scroll-bar-dialog-item)
  ()
  (:default-initargs 
    :scroll-size 33))

(defclass active-chart-window (ccl::picture-window)
  ()
  (:default-initargs 
   :scroller-class 'active-chart-window-pane))

(defclass active-chart-window-pane (ccl::picture-window-pane)
   ((chart-records :initform nil :accessor chart-records)
    (current-chart-edge :initform nil :accessor current-chart-edge)
    (root :initform nil :accessor active-chart-window-pane-root))
   (:default-initargs 
    :scroll-bar-class 'active-chart-scroll-bar))

(defclass active-chart-pop-up-field (ccl::pop-up-field)
  ())


;;; close charts after a parse


(defun close-existing-chart-windows nil
   (dolist (w (windows :class 'active-chart-window))
      (window-close w)))
  
;;;

(defun draw-chart-lattice (node title &optional (horizontalp t))
   (let*
      ((*chart-display* t)
       (*chart-records* nil)
       (*chart-font* (lkb-chart-font))
       (*chart-bold-font* (lkb-chart-bold-font))
       (ascent (font-info *chart-font*))
       (description
          (graph-display-layout node
             #'(lambda (node) (get node 'chart-edge-descendents))
             #'chart-node-text-string-width
             (font-height *chart-font*)
             horizontalp))
       (max-x (graph-description-max-x description))
       (max-y (graph-description-max-y description))
       (fake-window 
          (make-instance 'picture-field-window
             :view-font *chart-font* :view-size (make-point max-x max-y))))
      (graph-display-output fake-window description
         #'(lambda (str node)
            (move str 0 ascent)
            (multiple-value-bind (s bold-p)
                                 (chart-node-text-string node)
               (let ((start-pos (current-position str)))
                  (if bold-p
                     (with-bold-output str
                        (stream-write-string str s 0 (length (the string s))))
                     (stream-write-string str s 0 (length (the string s))))
                  (add-active-chart-region
                     node str start-pos (current-position str))))))
      (let*
         ((fields (fields fake-window))
          (pict (window-close fake-window))
          (real-window
            (make-instance 'active-chart-window
               :window-title title
               :pict pict
               :field-size (make-point max-x max-y)
               :view-size
               (make-point
                  (min (max (+ 50 max-x) 200) (- *screen-width* 100)) 
                  (min (+ 50 max-y) (- *screen-height* 100)))
               :close-box-p t
               :view-font *chart-font*)))
         (apply #'add-subviews (cons (ccl::my-scroller real-window) fields))
         (setf (active-chart-window-pane-root (ccl::my-scroller real-window)) node)
         (setf (current-chart-edge (ccl::my-scroller real-window)) nil)
         (setf (chart-records (ccl::my-scroller real-window)) *chart-records*)
         (invalidate-view real-window)
         real-window)))

(defun chart-node-text-string (x)
   (let ((edge-record (get x 'chart-edge-contents)))
      (if edge-record
         (values
            (format nil "~A [~A] ~A"
               (get x 'chart-edge-span)
               (edge-id edge-record)
	       (tree-node-text-string
		  (let ((item (edge-rule edge-record)))
		     (if (rule-p item) (rule-id item) item))))
            nil)
         (values (tree-node-text-string x) t))))

(defun chart-node-text-string-width (x)
   (multiple-value-bind (s bold-p) (chart-node-text-string x)
      (string-width s (if bold-p *chart-bold-font* *chart-font*))))


;;;

(defun add-active-chart-region (edge-symbol stream start-pos end-pos)
  (let ((edge-record
            (get edge-symbol 'chart-edge-contents))
        (view-pos (subtract-points start-pos (make-point 0 (font-ascent stream)))))
     (if edge-record
        (let* ((menu (make-instance 'active-chart-pop-up-field
                       :view-position view-pos
                       :item-string (chart-node-text-string edge-symbol)
                       :item-font (lkb-chart-font)
                       :view-font (lkb-dialog-font))))
           (apply #'add-menu-items menu
              (pop-up-chart-menu-items edge-record))
           (push menu (fields stream))
           (push
              (make-chart-record
                 :position start-pos
                 :width (- (point-h end-pos) (point-h start-pos))
                 :edge edge-record)
              *chart-records*)))))


(defmethod view-click-event-handler :before ((menu active-chart-pop-up-field) (where t))
   ;; used to blank out label in case it was highlighted, but wasn't correct
   )

(defmethod set-pop-up-menu-default-item ((menu active-chart-pop-up-field) num)
   ;; don't allow the menu mechanism to mark a menu item as default
   (declare (ignore num))
   nil)


(defun pop-up-chart-menu-items (edge-record)
  (list
   (make-instance 'menu-item
     :menu-item-title "Feature structure"
     :menu-item-action
     #'(lambda ()
         (display-fs (edge-dag edge-record)
            (format nil "Edge ~A ~A - FS" (edge-id edge-record)
               (if (g-edge-p edge-record) "G" "P")))))
   (make-instance 'menu-item
     :menu-item-title (format nil "Tree" (edge-id edge-record))
     :menu-item-action
     #'(lambda ()
         (display-parse-tree edge-record nil)))
   (make-instance 'menu-item
     :menu-item-title 
     (format nil "Rule ~A" 
        (if (rule-p (edge-rule edge-record)) (rule-id (edge-rule edge-record)) ""))
     :disabled 
     (not (rule-p (edge-rule edge-record)))
     :menu-item-action
     #'(lambda ()
         (let* ((item (edge-rule edge-record))
		(rule (and (rule-p item) item)))
	     (when rule
	       (display-fs (rule-full-fs rule)
			   (format nil "~A" (rule-id rule))
                           (rule-id rule)
                           )))))
   (make-instance 'menu-item
     :menu-item-title "Highlight nodes"
     :menu-item-action
     #'(lambda () (display-edge-in-chart edge-record)))
   (make-instance 'menu-item
     :menu-item-title "New chart"
     :menu-item-action
     #'(lambda () (display-edge-in-new-window edge-record)))
   (make-instance 'dynamic-enable-menu-item
     :menu-item-title (format nil "Unify")
     :menu-item-action
     #'(lambda () (try-unify-fs-in-chart (edge-dag edge-record)))
     :enable-function
     #'(lambda nil
         (and *selected-fs-node*
              (listp (selected-fs-node-path *selected-fs-node*)))))
   ))

(defun try-unify-fs-in-chart (fs)
  ;;; very similar to the function in activefs
  (let* ((sel1 *selected-fs-node*)
         (path1 (selected-fs-node-path sel1)))
    (when (listp path1)
      (let ((result 
             (unify-paths-with-fail-messages 
              (create-path-from-feature-list path1)
              (selected-fs-node-fs sel1)
              (create-path-from-feature-list nil)
              (tdfs-indef fs)
            :selected1 path1 :selected2 nil)))
         (when result
            (display-fs result "Unification result")))
         (setq *selected-fs-node* nil))))


;;; Highlight current edge, if there is one at the moment - and any ancestor
;;; and descendent edges that are visible

(defmethod view-draw-contents :after ((pane active-chart-window-pane))
   (view-draw-edge-highlighting pane))

(defun view-draw-edge-highlighting (pane)
   (let ((record (current-chart-edge pane)))
      (when record 
         (highlight-chart-edge-subs (chart-record-edge record) pane)
         (highlight-chart-edge-and-supers (chart-record-edge record) pane))))


(defun highlight-chart-edge-subs (edge pane)
   (dolist (subsumed-edge (edge-children edge))
      (when subsumed-edge
         (let ((record (display-chart-edge-record subsumed-edge pane)))
            (when record ; check that not an active edge suppressed in display
               (highlight-chart-edge record pane)))
         (highlight-chart-edge-subs subsumed-edge pane))))

(defun highlight-chart-edge-and-supers (edge pane)
   (labels
      ((highlight-chart-edge-path-p (e)
          ;; path from e recursively through children to edge?
          (and e
             (or (eq e edge)
                (some #'highlight-chart-edge-path-p (edge-children e))))))
      (dolist (record (chart-records pane))
         (when (highlight-chart-edge-path-p (chart-record-edge record))
            (highlight-chart-edge record pane)))))

(defun highlight-chart-edge (record pane)
   (invert-text-box pane
      (chart-record-position record)
      (+ (chart-record-position record) (chart-record-width record))))


;;; called from display-parse-tree - when it is called to display an edge
;;; find topmost chart window on screen,
;;; and ask for chart window to be scrolled so given edge is visible
;;; in centre, and the edge highlighted
;;; doesn't open a new chart window if not one, since user can do this from
;;; toplevel parse menu

(defun front-chart-window nil
   (front-window :class 'active-chart-window))

(defun display-edge-in-chart (edge)
   (let ((existing (front-chart-window)))
      (when existing
         (let*
            ((pane (and existing (ccl::my-scroller existing)))
             (edge-record (and pane (display-chart-edge-record edge pane))))
            (if edge-record
               (reposition-edge-in-window edge pane)
               (lkb-beep))))))


;;; create a new chart window and display just the descendents and ancestors of
;;; the edge in it

(defun display-edge-in-new-window (edge)
   (let ((existing (front-chart-window)))
      (when existing
         (let*
            ((pane (and existing (ccl::my-scroller existing)))
             (edge-record (and pane (display-chart-edge-record edge pane))))
            (if edge-record
               (draw-chart-lattice
                  (filtered-chart-lattice
                     (active-chart-window-pane-root pane) edge nil)
                  (string (gentemp (format nil "~A-" (ccl:window-title existing))))
                  t)
               (lkb-beep))))))


;;; If edge is not well inside current
;;; view area, scroll so it's centred in the view area. Then highlight it

(defun reposition-edge-in-window (edge pane)
   (let* ((edge-record (display-chart-edge-record edge pane))
          (edge-pos
             (and edge-record (chart-record-position edge-record))))
      (when edge-pos
         (unless
            (let ((eps (make-point 15 15)))
               (inside-box-p edge-pos
                  ;; make slightly smaller box than full area of visible pane
                  (cons (add-points (view-scroll-position pane) eps)
                     (subtract-points
                        (add-points (view-scroll-position pane) (view-size pane))
                        eps))))
            (set-view-scroll-position pane
               (max 0 (- (point-h edge-pos)
                         (truncate (point-h (view-size pane)) 2)))
               (max 0 (- (point-v edge-pos)
                         (truncate (point-v (view-size pane)) 2)))))
         (progn
            (when (current-chart-edge pane)
               ;; remove existing highlighting
               (view-draw-edge-highlighting pane))
            (setf (current-chart-edge pane) edge-record)
            ;; make new highlighting appear
            (view-draw-edge-highlighting pane)))))


(defun display-chart-edge-record (edge pane)
   (find edge (chart-records pane) :key #'chart-record-edge :test #'eq))
