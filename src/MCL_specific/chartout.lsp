;;; Copyright John Carroll 1998 All Rights Reserved.
;;; No use or redistribution without permission.
;;; 

;;; Graphical display of generator chart (show-gen-chart) (show-gen-chart t)

(defun show-gen-chart (&optional all-p) 
   (let ((root (make-symbol "")))
      (create-gen-chart-pointers root all-p)
      (draw-chart-lattice root
         (format nil "Generator Chart (~A edges)" (if all-p "all" "inactive"))
         t)
      root))


(defun create-gen-chart-pointers (root all-p)
   (let ((edge-symbols nil))
      (dolist (entry *gen-chart*)
         (dolist (e (cdr entry))
            (push
               (list* (gen-chart-edge-id e)
                  (make-symbol (write-to-string (gen-chart-edge-id e)))
                  (gen-chart-edge-needed e))
               edge-symbols)))
      (dolist (entry *gen-chart*)
         (let ((chart-index (string-downcase (symbol-name (car entry)))))
            (dolist (e (cdr entry))
               (let ((edge-symbol
                        (cadr (assoc (gen-chart-edge-id e) edge-symbols))))
                  (setf (get edge-symbol 'chart-edge-span)
                     (if (gen-chart-edge-needed e)
                        (concatenate 'string chart-index " A") chart-index))
                  (setf (get edge-symbol 'chart-edge-contents) e)
                  (if (gen-chart-edge-children e)
                     (dolist (c (gen-chart-edge-children e))
                        (when c
                           (push edge-symbol
                              (get (cadr (assoc (gen-chart-edge-id c) edge-symbols))
                                 'chart-edge-descendents))))
                     (push edge-symbol (get root 'chart-edge-descendents)))))))
      (unless all-p
         (dolist (pair edge-symbols)
            (setf (get (cadr pair) 'chart-edge-descendents)
               (create-gen-chart-pointers-collapse
                  (get (cadr pair) 'chart-edge-descendents)
                  edge-symbols))))))


(defun create-gen-chart-pointers-collapse (nodes edge-symbols)
   (mapcan
      #'(lambda (node)
          (if (cddr (find node edge-symbols :key #'cadr))
             (create-gen-chart-pointers-collapse
                (get node 'chart-edge-descendents) edge-symbols)
             (list node)))
      nodes))


;;; Graphical display of parse chart (show-chart)

(defun show-chart nil 
   (unwind-protect
      (let ((root (make-symbol "")))
         (setf (get root 'chart-edge-descendents)
            (make-array *chart-limit* :initial-element nil))
         (let*
            ((end (create-chart-pointers root))
             (word-alt-sets
                ;; each element is a set to allow for multi-word lexical entries
                ;; at each position in input
                (coerce (subseq (get root 'chart-edge-descendents) 0 end) 'list)))
            (setf (get root 'chart-edge-descendents) (apply #'append word-alt-sets))
            (draw-chart-lattice root
               (format nil "Parse Chart for \"~A...\""
                  (car (get root 'chart-edge-descendents)))
               t)
            root))
      (clear-chart-pointers)))


(defun create-chart-pointers (root)
   (dotimes (vertex (1- *chart-limit*))
      (if (aref *chart* (1+ vertex)) 
         (dolist (span (chart-entry-configurations (aref *chart* (1+ vertex))))
            (create-chart-pointers1 span (1+ vertex) root))
         (return vertex))))

(defun create-chart-pointers1 (span right-vertex root)
   (let*
      ((edge (chart-configuration-edge span))
       (edge-id (make-edge-symbol (edge-id edge))))
      (setf (get edge-id 'chart-edge-span)
         (format nil "~A-~A" (chart-configuration-begin span) right-vertex))
      (if (edge-children edge)
         (dolist (child (edge-children edge))
            (push edge-id
               (get (make-edge-symbol (edge-id child)) 'chart-edge-descendents)))
         (progn
            (dolist (lex (edge-leaves edge))
               (push edge-id (get (intern lex) 'chart-edge-descendents)))
            (pushnew (intern (car (last (edge-leaves edge))))
               (aref (get root 'chart-edge-descendents) (1- right-vertex)))))
      (setf (get edge-id 'chart-edge-contents) edge)))


(defun clear-chart-pointers ()
   (dotimes (vertex (1- *chart-limit*))
      (if (aref *chart* (1+ vertex)) 
         (dolist (span (chart-entry-configurations (aref *chart* (1+ vertex))))
            (let ((edge (chart-configuration-edge span)))
               (if (edge-children edge)
                  (dolist (child (edge-children edge))
                     (setf (get (make-edge-symbol (edge-id child)) 'chart-edge-descendents)
                        nil))
                  (dolist (lex (edge-leaves edge))
                     (setf (get (intern lex) 'chart-edge-descendents) nil)))
               (setf (get (make-edge-symbol (edge-id edge)) 'chart-edge-contents)
                  nil)))
         (return nil))))


;;; dialect specific from this point

;;; use *parse-tree-font-size* from globals.lsp

(defparameter *chart-font* (list "Helvetica" (or *parse-tree-font-size* 9)))
(defparameter *chart-bold-font* (list "Helvetica" (or *parse-tree-font-size* 9) :bold))

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
    (current-chart-edge :initform nil :accessor current-chart-edge))
   (:default-initargs 
    :scroll-bar-class 'active-chart-scroll-bar))

(defclass active-chart-pop-up-field (ccl::pop-up-field)
  ())


;;;

(defun draw-chart-lattice (node title horizontalp)
   (let*
      ((*chart-display* t)
       (*chart-records* nil)
       (font *chart-font*)
       (ascent (font-info font))
       (description
          (graph-display-layout node
             #'(lambda (node) (get node 'chart-edge-descendents))
             #'chart-node-text-string-width
             (font-height font)
             horizontalp))
       (max-x (graph-description-max-x description))
       (max-y (graph-description-max-y description))
       (fake-window 
          (make-instance 'picture-field-window
             :view-font font :view-size #@(10000 10000))))
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
                  (add-active-chart-region node str start-pos)))))
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
               :view-font font)))
         (apply #'add-subviews (cons (ccl::my-scroller real-window) fields))
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
               (tree-node-text-string (edge-rule-number edge-record)))
            nil)
         (values (tree-node-text-string x) t))))

(defun chart-node-text-string-width (x)
   (multiple-value-bind (s bold-p) (chart-node-text-string x)
      (string-width s (if bold-p *chart-bold-font* *chart-font*))))


;;;

(defun add-active-chart-region (edge-symbol stream start-pos)
  (let ((edge-record
            (get edge-symbol 'chart-edge-contents))
        (view-pos (subtract-points start-pos (make-point 0 (font-ascent stream)))))
     (if edge-record
        (let* ((menu (make-instance 'active-chart-pop-up-field
                       :view-position view-pos
                       :item-display (chart-node-text-string edge-symbol)
                       :view-font *chart-font*)))
           (apply #'add-menu-items menu
              (pop-up-chart-menu-items edge-record))
           (push menu (fields stream))
           (push
              (make-chart-record
                 :position start-pos
                 :width (chart-node-text-string-width edge-symbol)
                 :edge edge-record)
              *chart-records*)))))


(defmethod view-click-event-handler :before ((menu active-chart-pop-up-field) (where t))
   ;; before menu gets popped up blank out label - in case it was highlighted,
   ;; because if it was it won't get redrawn properly
   (erase-rect (view-container menu) (view-position menu)
      (add-points (view-position menu)
         (make-point
            (string-width 
             #+powerpc(pop-up-menu-item-display menu) 
             #-powerpc(ccl::pop-up-menu-item-display menu)
             (view-font (view-container menu)))
            (+ 2 (font-ascent (view-container menu)))))))

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


;;; Highlight current edge, if there is one at the moment

(defmethod view-draw-contents ((pane active-chart-window-pane))
   (call-next-method)
   (let ((edge-record (current-chart-edge pane)))
      (when edge-record 
         (highlight-current-chart-edge edge-record pane))))

(defun highlight-current-chart-edge (edge-record pane)
   (invert-text-box pane
      (chart-record-position edge-record)
      (+ (chart-record-position edge-record)
         (chart-record-width edge-record)))
   (dolist (parent-edge (edge-children (chart-record-edge edge-record)))
      (let ((parent-record (display-chart-edge-record parent-edge pane)))
         (when parent-record
            (highlight-current-chart-edge parent-record pane)))))


;;; called from display-parse-tree - when it is called to display an edge
;;; find topmost chart window on screen,
;;; and ask for type hierarchy window to be scrolled so given edge is visible
;;; in centre, and the edge highlighted
;;; doesn't open a new chart window if not one, since user can do this from
;;; toplevel parse menu

(defun display-edge-in-chart (edge)
   (let ((existing (front-window :class 'active-chart-window)))
      (when existing
         (let*
            ((pane (and existing (ccl::my-scroller existing)))
             (edge-record (and pane (display-chart-edge-record edge pane))))
            (if edge-record
               (reposition-edge-in-window edge (ccl::my-scroller existing))
               (lkb-beep))))))


;;; If edge is not well inside current
;;; view area, scroll so it's centred in the view area. Then highlight it

(defun reposition-edge-in-window (edge pane)
   (let* ((edge-record (display-chart-edge-record edge pane))
          (edge-pos
             (and edge-record (chart-record-position edge-record))))
      (when edge-pos
         (unless
            (let ((eps (make-point 30 15)))
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
               (highlight-current-chart-edge (current-chart-edge pane) pane))
            (setf (current-chart-edge pane) edge-record)
            ;; make new highlighting appear
            (highlight-current-chart-edge edge-record pane)
            ))))


(defun display-chart-edge-record (edge pane)
   (find edge (chart-records pane) :key #'chart-record-edge :test #'eq))
