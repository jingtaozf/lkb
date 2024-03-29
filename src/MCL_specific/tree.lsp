;;; Copyright (c) 1991-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see LICENSE for conditions


;;; 1995 modified for MCL port

;;; 1996 modified to allow trees to be displayed without automatically
;;; added glb types

;;; 1998 substantially rewritten

(in-package :lkb)


;;; This is a function so users can change font sizes after code has loaded

(defun lkb-type-tree-font nil
   (list (if (ccl:osx-p) "Lucida Grande" "Helvetica") *type-tree-font-size*))

(defvar *type-display* nil)

(defvar *type-records* nil)


;;; active type hierarchy

(defstruct type-hier-record
   position node (clicked-p nil))


(defclass active-type-scroll-bar (ccl::scroll-bar-dialog-item)
  ()
  (:default-initargs 
    :scroll-size 33))

(defclass active-type-hier-window (ccl::picture-window)
  ()
  (:default-initargs 
   :scroller-class 'active-type-hier-window-pane))

(defclass active-type-hier-window-pane (ccl::picture-window-pane)
   ((type-nodes :initarg type-nodes :initform nil :accessor type-nodes)
    (current-type-node :initarg type-tree :initform nil :accessor current-type-node)
    (top-type-node :initarg top-type-node :initform nil :accessor top-type-node)
    (show-all-p :initarg show-all-p :initform nil :accessor show-all-p))
   (:default-initargs 
    :scroll-bar-class 'active-type-scroll-bar))

(defclass active-type-pop-up-field (ccl::pop-up-field)
  ())


;;; Entry points - (time (create-type-hierarchy-tree))
;;; (create-type-hierarchy-tree 'sign)
;;; (create-type-hierarchy-tree 'nom_rel nil t)

(defun create-type-hierarchy-tree (&optional (type *toptype*) old-window show-all-p)
   ;; if show-all-p is true then we never hide any nodes. If it's false then we
   ;; call hide-in-type-hierarchy-p on each type to see whether it should
   ;; be hidden
   (dolist (name *type-names*)
      (unless (symbolp name)
         (let ((real-thing name))
            (setq name (intern (princ-to-string name)))
            (setf (get name 'real-thing) real-thing))) 
      (setf (get name 'daughters) nil))
   (clear-type-visibility)
   (propagate-visibility-in-type-tree type)
   (when old-window
      (setq show-all-p (show-all-p (ccl::my-scroller old-window))))
   (let ((node
           (car (make-new-type-tree type show-all-p t))))
      (draw-new-type-tree node
         (format nil "Type hierarchy below ~(~A~)" type) t old-window show-all-p)))

(defun close-existing-type-hierarchy-trees nil
   (dolist (w (windows :class 'active-type-hier-window))
      (window-close w)))


;;; initially all nodes are marked not visible. If we're not a shrunk node,
;;; go on to attempt to mark each daughter as visible
;;; If we're marked as visible then daughters must have been done already
;;; If we start below a shrunk node then nodes are visible despite this

(defun propagate-visibility-in-type-tree (type)
   (let ((type-record (get-type-entry type)))
      (when (and (not (type-shrunk-p type-record))
                 (not (type-visible-p type-record)))
         (dolist (daughter (type-daughters type-record))
            (propagate-visibility-in-type-tree daughter)))
      (setf (type-visible-p type-record) t)))


(defun make-new-type-tree (type show-all-p toplevel-p)
   ;; make sure that top type is not hidden, no matter what
   ;; hide-in-type-hierarchy-p function says - otherwise we may end up
   ;; displaying no hierarchy at all (if all descendents are hidden), or just
   ;; one branch rather than all
   (let ((type-record (get-type-entry type)))
      (when (type-visible-p type-record)
         (let ((node
                 (if (symbolp type) type
                    (intern (princ-to-string type)))))
            (unless (get node 'daughters)
               (setf (get node 'daughters)
                  (delete-duplicates
                     (mapcan
                        #'(lambda (d) (copy-list (make-new-type-tree d show-all-p nil)))
                        (type-daughters type-record))
                     :test #'eq)))
            (if (and (not toplevel-p) (not show-all-p)
                   (fboundp 'hide-in-type-hierarchy-p)
                   (funcall (symbol-function 'hide-in-type-hierarchy-p) type))
               (get node 'daughters)
               (list node))))))


;;;

(defun draw-new-type-tree (node title horizontalp existing show-all-p)
   (when existing
      (let ((pane (ccl::my-scroller existing)))
         (setf (type-nodes pane) nil)                   ; disable node menus while
         (apply #'remove-subviews pane (subviews pane)) ; redrawing takes place
         (event-dispatch) ; get remove-subviews redrawing over and done with
         (erase-region existing (clip-region existing))))
   (let*
      ((*type-display* t)
       (*type-records* nil)
       (font (lkb-type-tree-font))
       (ascent (font-info font))
       (description
         (graph-display-layout node
            #'(lambda (node) (get node 'daughters))
            #'(lambda (node) (type-node-text-string-width node font))
            (font-height font)
            horizontalp))
       (max-x (graph-description-max-x description))
       (max-y (graph-description-max-y description))
       (fake-window 
         (make-instance 'picture-field-window
            :view-font font :view-size (make-point max-x max-y))))
      (graph-display-output fake-window description
         #'(lambda (str node)
              (with-focused-view str
                 (move-by-y str ascent) ; move down - mcl string origin is bottom left
                 (let ((s (type-node-text-string node))
                       (start-pos (current-position str)))
                    (with-fore-color *red-color*
                       (stream-write-string str s 0 (length s)))
                    (add-active-type-region
                       node str start-pos (current-position str))))))
      (let*
         ((page-width (min (max (+ 50 max-x) 200) 600))
          (page-height (min (+ 50 max-y) 400))
          (pict (window-close fake-window))
          (real-window
            (if existing
               (let ((pane (ccl::my-scroller existing)))
                  (ccl::kill-picture (ccl::pict-data pane))
                  (setf (ccl::pict-data pane) pict)
                  (setf (slot-value pane 'ccl::field-size) ; !!! ugh
                     (make-point max-x max-y))
                  (ccl::update-scroll-bars pane :length t :position t)
                  (reinitialize-instance existing
                     :view-size (make-point page-width page-height))
                  existing)
               (make-instance 'active-type-hier-window
                 :window-title title
                 :pict pict
                 :view-font font
                 :view-position #@(6 44)
                 :field-size (make-point max-x max-y)
                 :close-box-p t ; was nil
                 :view-size (make-point page-width page-height)))))
         (setf (top-type-node (ccl::my-scroller real-window)) node)
         (setf (current-type-node (ccl::my-scroller real-window)) nil)
         (setf (type-nodes (ccl::my-scroller real-window)) *type-records*)
         (setf (show-all-p (ccl::my-scroller real-window)) show-all-p)
         (if existing
            (invalidate-view real-window)
            (reposition-type-in-window node (ccl::my-scroller real-window) nil))
         real-window)))


(defun add-active-type-region (node stream start-pos end-pos)
   ;; add the relevant field to the stream
   (push
      (make-type-hier-record :position start-pos :node node)
      *type-records*) 
   (when
      (type-shrunk-p
         (or (get-type-entry node) (get-type-entry (get node 'real-thing))))
      (frame-text-box stream start-pos end-pos)))


;;; Take a type name and return a downcased string representing it; also
;;; compute the string's length wrt a given font

(defparameter *node-text-scratch-string*
   (make-array 30 :element-type 'base-character :fill-pointer 0))

(defun type-node-text-string (node)
   (without-interrupts ; the code in here isn't re-entrant
      (let* ((str *node-text-scratch-string*)
             (full-string (symbol-name node))
             (full-length (length full-string))
             (len (min full-length 30)))
         (setf (fill-pointer str) len)
         (dotimes (n len)
            (setf (char str n) (char-downcase (char full-string n))))
         (when (> full-length 30) (setf (char str 29) (code-char 201))) ; '...'
         str)))

(defun type-node-text-string-width (node font)
   (without-interrupts ; not re-entrant
      (string-width (type-node-text-string node) font)))


;;; Highlight current node, if there is one at the moment

(defmethod view-draw-contents ((pane active-type-hier-window-pane))
  (call-next-method)
  (let ((record (current-type-node pane)))
     (when record 
        (highlight-current-type-node record pane))))

(defun highlight-current-type-node (record pane)
   (invert-text-box pane
      (type-hier-record-position record)
      (+ (type-hier-record-position record)
         (type-node-text-string-width
            (type-hier-record-node record) (view-font pane)))))


;;; Pop up menus are created as separate views in the right position
;;; but only on the first click near where the type is

(defmethod view-click-event-handler ((pane active-type-hier-window-pane) where)
  (let ((x-pos-click (point-h where))
        (y-pos-click (point-v where))
        (font (view-font pane))
        (ascent (font-ascent pane))
        (eps 2))
    (dolist (record (type-nodes pane))
      (when
        (let ((x-pos-node (point-h (type-hier-record-position record)))
              (y-pos-node (point-v (type-hier-record-position record))))
            (and (> y-pos-click (- y-pos-node ascent eps))
                 (< y-pos-click (+ y-pos-node eps))
                 (> x-pos-click (- x-pos-node eps))
                 (< x-pos-click
                    (+ x-pos-node eps
                       (type-node-text-string-width (type-hier-record-node record)
                          font)))))
        (unless (type-hier-record-clicked-p record)
           (add-subviews pane (create-type-in-tree-menu record ascent))
           (setf (type-hier-record-clicked-p record) t))
        (return nil)))
    (call-next-method pane where)))


(defun create-type-in-tree-menu (record ascent)
  (let* ((menu-pos (make-point (point-h (type-hier-record-position record)) 
                               (- (point-v (type-hier-record-position record))
                                  ascent)))
         (node (type-hier-record-node record))
         (type-entry
            (or (get-type-entry node)
                (get-type-entry (get node 'real-thing))))
         (menu (make-instance 'active-type-pop-up-field
                 :view-position menu-pos
                 :item-string (format nil "~(~A~) " node) ; there's a 1-off error
                 :view-font (cons :bold (lkb-type-font)))))
     (apply #'add-menu-items menu
        (type-in-tree-menu-items node type-entry menu))
     menu))


(defmethod view-click-event-handler :before ((menu active-type-pop-up-field) (where t))
   ;; used to blank out label in case it was highlighted, but wasn't correct
   )

(defmethod set-pop-up-menu-default-item ((menu active-type-pop-up-field) num)
   ;; don't allow the menu mechanism to mark a menu item as default
   (declare (ignore num))
   nil)


(defun type-in-tree-menu-items (node type-entry menu)
   (list
     (make-instance 'menu-item
       :menu-item-title "Help"
       :menu-item-action
       #'(lambda ()
           (display-type-comment node (type-comment type-entry)))
       :disabled (not (type-comment type-entry)))
     (make-instance 'menu-item
       :menu-item-title "Shrink/expand"
       :menu-item-action 
       #'(lambda () 
           (setf (type-shrunk-p type-entry) (not (type-shrunk-p type-entry)))
           (let* ((pane (view-container menu))
                  (record (display-type-node-record node pane))
                  (node-pos (type-hier-record-position record))
                  (view-pos (view-scroll-position pane)))
                 (create-type-hierarchy-tree  
                    (top-type-node pane) (view-container pane))
                 (reposition-type-in-window
                    node pane nil (subtract-points node-pos view-pos))))
       :disabled (null (type-daughters type-entry)))
     (make-instance 'menu-item
       :menu-item-title "Type definition"
       :menu-item-action 
       #'(lambda () (show-type-spec-aux node type-entry)))
     (make-instance 'menu-item
       :menu-item-title "Expanded type"
       :menu-item-action
       #'(lambda () (show-type-aux node type-entry)))
     (make-instance 'menu-item
       :menu-item-title "New hierarchy..."
       :menu-item-action
       #'(lambda ()
           (let ((*last-type-name* (type-name type-entry)))
                 (declare (special *last-type-name*))
              (multiple-value-bind (type show-all-p)
		         (ask-user-for-type nil 
				  '("Show all types?" . :check-box)
				  '("Ignore 300 descendant limit" . :check-box))
                 (when type
                   (let ((type-entry (get-type-entry type)))
                     (when type-entry 
                       (create-type-hierarchy-tree type nil show-all-p)))))))
       :disabled (null (type-daughters type-entry)))))



;;; NB Problems caused by having only 1 field per type for shrunk and visible
;;; flags and allowing multiple type windows on screen at once:
;;; shrinking/expanding a type in one window will give inconsistent
;;; expand/shrink behavour of that type if it appears in another window.
;;; A type may be expanded automatically in the process of highlighting one of
;;; its descendents, which could also cause confusion wrt another window

;;; called from top level menu commands etc
;;; Try to make type visible by unshrinking any ancestors if necessary - up
;;; to top type for this window if we currently have one on screen,
;;; and ask for type hierarchy window to be scrolled so given type is visible
;;; in centre, and the type highlighted
;;; If we're looking in an existing window and the type isn't a descendent of
;;; the window's top type then we give up immediately
;;; If there's not a hierarchy onscreen give up. User can always open one up
;;; from toplevel view menu

(defun front-type-hierarchy-window nil
   (front-window :class 'active-type-hier-window))

(defun display-type-in-tree (node &optional scroll-onlyp)
   (let ((type-entry
            (or (get-type-entry node)
                (get-type-entry (get node 'real-thing)))))
      (when type-entry
         (let* ((type (type-name type-entry))
                (existing (front-type-hierarchy-window))
                (pane (and existing (ccl::my-scroller existing)))
                (top-type
                   (if existing (top-type-node pane) *toptype*)))
           (if (and existing
                    (or (eq type top-type)
                       (member type-entry 
                         (retrieve-descendants top-type) :test #'eq)))
                  (progn
                     ;; we want to see if type is not visible in this window.
                     ;; We can't just test for visible-p on the type since another
                     ;; part of the hierarchy in which this type is not present may
                     ;; have just been drawn in another window (so visible-p will be
                     ;; false, whether it's visible or not in the current window).
                     ;; We also can't test to see if an ancestor is shrunk since the
                     ;; ancestor might have been expanded again since this window was
                     ;; drawn. But checking for node record for the type from the last
                     ;; time the window was drawn is reliable
                     (unless (display-type-node-record node pane)
                        ;; it's a descendent of shrunk node(s) and no others, and/or
                        ;; a 'hidden' node (if so change window to a show-all-p) -
                        ;; we can't tell which is the case
                        (unshrink-ancestors type-entry top-type)
                        (when (and (not (show-all-p pane))
                                 (fboundp 'hide-in-type-hierarchy-p)
                                 (funcall (symbol-function 'hide-in-type-hierarchy-p)
                                    type))
                           (setf (show-all-p pane) t))
                        (create-type-hierarchy-tree top-type existing))
                     (reposition-type-in-window node pane t nil))
             (unless scroll-onlyp
               (create-type-hierarchy-tree type nil t)))))))
                  

(defun unshrink-ancestors (type-entry top-type)
   ;; can't just use type-ancestors list since we have to stop at top-type arg
   (unless (eql (type-name type-entry) top-type)
      (dolist (parent (type-parents type-entry))
         (let ((parent-entry (get-type-entry parent)))
            (setf (type-shrunk-p parent-entry) nil)
            (unshrink-ancestors parent-entry top-type)))))


;;; If view-pos arg supplied, scroll window so type is at point view-pos relative
;;; to current view coordinates. If not supplied and type is not well inside current
;;; view area, scroll so it's centred in the view area. Highlight it if highlightp
;;; is true

(defun reposition-type-in-window (node pane &optional highlightp view-pos)
   (let* ((node-record (display-type-node-record node pane))
          (node-pos
             (and node-record (type-hier-record-position node-record))))
      (when node-pos
         (if view-pos
            (set-view-scroll-position pane
               (max 0 (- (point-h node-pos) (point-h view-pos)))
               (max 0 (- (point-v node-pos) (point-v view-pos))))
            (unless
               (let ((eps (make-point 15 15)))
                  (inside-box-p node-pos
                     ;; make slightly smaller box than full area of visible pane
                     (cons (add-points (view-scroll-position pane) eps)
                        (subtract-points
                           (add-points (view-scroll-position pane) (view-size pane))
                           eps))))
               (set-view-scroll-position pane
                  (max 0 (- (point-h node-pos)
                            (truncate (point-h (view-size pane)) 2)))
                  (max 0 (- (point-v node-pos)
                            (truncate (point-v (view-size pane)) 2))))))
         (when highlightp
            (when (current-type-node pane)
               ;; remove existing highlighting
               (highlight-current-type-node (current-type-node pane) pane))
            (setf (current-type-node pane) node-record)
            ;; make new highlighting appear
            (highlight-current-type-node node-record pane)
            ))))


(defun display-type-node-record (node pane)
   (find node (type-nodes pane) :key #'type-hier-record-node))

