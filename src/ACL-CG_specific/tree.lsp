;;; Copyright (c) 1991-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions


;;; This version for Allegro 5.0 on Windows NT/95/98 is
;;; partly based on the version for Allegro 3.0.1 by
;;; Anthony Hull

(in-package :lkb)

;;; This is a function so users can change font sizes after code has loaded

(defun lkb-type-tree-font nil
   (let ((pix (cg:stream-units-per-inch (lkb-screen-stream))))
      (cg:make-font :roman :|COURIER NEW| 
        (ceiling (* (or *type-tree-font-size* 9) pix) 72) nil)))

(defvar *type-display* nil)

(defvar *hier-window-number* 0)

(defvar *type-hierarchy-windows* nil)

(defun create-new-hier-name nil
   (incf *hier-window-number*)
   (let ((new-name (intern 
                     (format nil "HIERARCHY-WINDOW~A" 
                       *hier-window-number*))))
      (push new-name *type-hierarchy-windows*)
      new-name))

(defun create-type-hierarchy-tree (&optional (type *toptype*) old-window show-all-p)
   ;; if show-all-p is true then we never hide any nodes. If it's false then we
   ;; call hide-in-type-hierarchy-p on each type to see whether it should
   ;; be hidden
   (when (is-valid-type type)
      (loop for name in *type-names*
        do 
        (unless (symbolp name)
           (let ((real-thing name))
              (setq name (intern (princ-to-string name)))
              (setf (get name 'real-thing) real-thing))) 
        (setf (get name 'daughters) nil))
      (clear-type-visibility)
      ;;; if we don't have an existing stream, name one here
      ;;; so we can specify (in)visibility wrt a window name
      (let ((window-name (if old-window
                            (cg:name old-window)
                            (create-new-hier-name))))
         (propagate-visibility-in-type-tree type window-name)
         (when old-window
            (setq show-all-p (cg-user::show-all-p old-window)))
         (let ((node
                (car (make-new-type-tree type show-all-p t window-name))))
            (draw-new-type-tree window-name node         
              t old-window show-all-p type)))))


(defun close-existing-type-hierarchy-trees nil
   (dolist (w *type-hierarchy-windows*)
      (let ((window (cg:find-window w)))
         (when (and window (cg:windowp window))
            ;; closed windows aren't windowp
            (close window)))))

;;; initially all nodes are marked not visible. If we're not a shrunk node,
;;; go on to attempt to mark each daughter as visible
;;; If we're marked as visible then daughters must have been done already
;;; If we start below a shrunk node then nodes are visible despite this

(defun propagate-visibility-in-type-tree (type stream-name)
   (let ((type-record (get-type-entry type)))
      (when (and type-record
                 (not (member stream-name (type-shrunk-p type-record)))
                 (not (member stream-name (type-visible-p type-record))))
         (loop for daughter in (type-daughters type-record)
            do
            (propagate-visibility-in-type-tree daughter stream-name)))
      (pushnew stream-name (type-visible-p type-record))))

(defun make-new-type-tree (type show-all-p toplevel-p stream-name)
   ;; make sure that top type is not hidden, no matter what hide-in-type-hierarchy-p
   ;; function says - otherwise we may end up displaying no hierarchy at all (if all
   ;; descendents are hidden), or just one branch rather than all
   (let ((type-record (get-type-entry type)))
      (cond
         ((not (member stream-name (type-visible-p type-record)))
            nil)
         ((and (not toplevel-p) (not show-all-p)
             (fboundp 'hide-in-type-hierarchy-p)
             (funcall (symbol-function 'hide-in-type-hierarchy-p) type))
            (mapcan
               #'(lambda (d) (make-new-type-tree d show-all-p nil stream-name))
               (type-daughters type-record)))
         (t
            (let ((node
                   (if (symbolp type) type
                      (let ((new (intern (princ-to-string type))))
                         (setf (get new 'real-thing) type)
                         new))))
               (unless (get node 'daughters)
                  (setf (get node 'daughters)
                     (delete-duplicates
                        (mapcan
                           #'(lambda (d) 
                               (make-new-type-tree d show-all-p nil stream-name))
                           (type-daughters type-record)) :test #'eq)))
               (list node))))))


(defun draw-new-type-tree (name node horizontalp existing show-all-p type)
   (let*
      ((*type-display* t)
       (font (lkb-type-tree-font))
       (description
         (graph-display-layout node
            #'(lambda (node) (get node 'daughters))
            #'(lambda (node) (type-node-text-string-width node font))
            (cg:font-size font)
            horizontalp))
       (max-x (graph-description-max-x description))
       (max-y (graph-description-max-y description))
       (stream 
         (or existing
             (cg:make-window name
               :device 'cg-user::active-type-hier-window
               :window-interior
                  (cg:make-box 100 100 (min (+ 100 max-x) 600)
                     (min (+ 100 max-y) 400))
               :page-width max-x
               :page-height max-y
               :scrollbars t
               :font font
               :title (format nil "Type hierarchy below ~(~A~)" type))))) 
    ;; FIX - make window smaller is hierarchy is very little ...
    (when existing
       (setf (cg-user::active-type-hier-window-active-menus existing)
             nil)
       (cg:clear-page stream))
    (graph-display-output stream description
      #'(lambda (str node)
          (let ((s (type-node-text-string node))
                (start-pos (cg:current-position str)))
             (cg:with-foreground-color (str cg:red)
              (cg::device-write-string str s 0 (length (the string s))))
             (add-active-type-region
              node str start-pos (cg:current-position str) name))))
    (setf (cg-user::top-type-node stream) node)
    (setf (cg-user::current-type-node stream) nil)
    (setf (cg-user::show-all-p stream) show-all-p)
    (when existing
       (cg:invalidate stream))
    (reposition-type-in-window node stream nil)
    stream))

(defun add-active-type-region (node stream start-pos end-pos name)
   (let ((box (create-box-for-fs-region start-pos end-pos
               stream))) ;; defined in activefs
      (cg-user::add-type-hier-region-record
       stream box node)
      ;; shrunk data is a list of streams in which a type
      ;; is shrunk, not just a boolean
      (let ((shrunk-data
             (type-shrunk-p
              (or (get-type-entry node) 
                  (get-type-entry (get node 'real-thing))))))
         (when (member name shrunk-data)
            (cg:draw-box stream box)))))

;;; Take a type name and return a downcased string representing it; also
;;; compute the string's length wrt a given font

(defparameter *node-text-scratch-string*
   (make-array 30 :element-type 'character :fill-pointer 0))

(defun type-node-text-string (node)
   (excl:without-interrupts ; the code in here isn't re-entrant
      (let* ((str *node-text-scratch-string*)
             (full-string (symbol-name node))
             (full-length (length full-string))
             (len (min full-length 30)))
         (setf (fill-pointer str) len)
         (dotimes (n len)
            (setf (char str n) (char-downcase (char full-string n))))
         (when (> full-length 30) (setf (char str 29) (code-char 133))) ; '...'
         ;;; 133 is ... for NT
         ;;; Unicode is 2026
         str)))

(defun type-node-text-string-width (node font)
   (excl:without-interrupts ; not re-entrant
      (cg:font-string-width  font (type-node-text-string node))))

(defun perform-type-hier-node-action (stream window node)
   (if node
      (pop-up-type-hier-menu stream window node 
       (or (get-type-entry node) 
            (get-type-entry (get node 'real-thing))))))

(defun pop-up-type-hier-menu (stream window node type-entry)
   (let ((menu 
           (cg:open-menu
            (list
             (make-instance 'cg:menu-item :name "Help"                 
               :available-p (type-comment type-entry)
               :value               
               #'(lambda ()
                   (display-type-comment node (type-comment type-entry))))
             (make-instance 'cg:menu-item :name "Shrink/expand"
               :value 
               #'(lambda ()
                   (let ((stream-name (cg:name stream)))
                      (if (member stream-name (type-shrunk-p type-entry))
                         (setf (type-shrunk-p type-entry)
                               (delete stream-name (type-shrunk-p type-entry)))
                         (push stream-name (type-shrunk-p type-entry)))
                      (create-type-hierarchy-tree  
                       (cg-user::top-type-node window) 
                       window)
                      (reposition-type-in-window
                       node window nil)))
               :available-p (type-daughters type-entry))               
             (make-instance 'cg:menu-item :name "Type definition"                 
               :value               
               #'(lambda () (show-type-spec-aux node type-entry)))
             (make-instance 'cg:menu-item :name "Expanded type"                 
               :value               
               #'(lambda () (show-type-aux node type-entry)))
             (make-instance 'cg:menu-item :name "New hierarchy..."                 
               :value               
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
                                  (create-type-hierarchy-tree 
                                   type nil show-all-p)))))))
               :available-p (type-daughters type-entry)))            
             'cg:pop-up-menu (lkb-parent-stream)
            :selection-function #'lkb-funcall-menu-item)))
      (let ((result (cg:pop-up-menu menu)))
         (close menu)
         result)))

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
   ;; return the most recent window
   (dolist (wname *type-hierarchy-windows*)
      (let ((window (cg:find-window wname)))
         (when (and window (cg:windowp window))
            (return window)))))

(defun display-type-in-tree (node &optional scroll-onlyp)
   (let ((type-entry
            (or (get-type-entry node)
                (get-type-entry (get node 'real-thing)))))
      (when type-entry
         (let* ((type (type-name type-entry))
                (existing (front-type-hierarchy-window))
                (top-type
                   (if existing (cg-user::top-type-node existing) *toptype*)))
            (when existing
               (if (or (eq type top-type)
                       (member type-entry 
                         (retrieve-descendants top-type) :test #'eq))
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
                     ;; FIX - maybe could simplify this now?
                     (unless (display-type-node-record node existing)
                        ;; it's a descendent of shrunk node(s) and no others, and/or
                        ;; a 'hidden' node (if so change window to a show-all-p) -
                        ;; we can't tell which is the case
                        (unshrink-ancestors type-entry top-type 
                          (cg:name existing))
                        (when (and (not (cg-user::show-all-p existing))
                                 (fboundp 'hide-in-type-hierarchy-p)
                                 (funcall 
                                   (symbol-function 'hide-in-type-hierarchy-p)
                                    type))
                          (setf (cg-user::show-all-p existing) t))
                        (unless scroll-onlyp
                          (create-type-hierarchy-tree top-type existing)))
                     (reposition-type-in-window node existing t))
                  (lkb-beep)))))))

(defun unshrink-ancestors (type-entry top-type stream-name)
   ;; can't just use type-ancestors list since we have to stop at top-type arg
   (unless (eql (type-name type-entry) top-type)
      (loop for parent in (type-parents type-entry)
        do
        (let ((parent-entry (get-type-entry parent)))
           (setf (type-shrunk-p parent-entry) 
                 (remove stream-name (type-shrunk-p parent-entry)))
           (unshrink-ancestors parent-entry top-type stream-name)))))


;;; If view-pos arg supplied, scroll window so type is at point view-pos relative
;;; to current view coordinates. If not supplied and type is not well inside current
;;; view area, scroll so it's centred in the view area. Highlight it if highlightp
;;; is true

(defun reposition-type-in-window (node window &optional highlightp)
   (let* ((node-record (display-type-node-record node window))
          (pane (cg:frame-child window))
          (node-pos
             (and node-record (cg-user::type-hier-record-box node-record))))
      (when node-pos
         (unless (cg:inside-box-p node-pos
                   (cg:visible-box pane)) 
             ; FIX - want to allow for
             ; types which are on the periphery
               (cg:scroll-to pane
                 (cg:make-position
                  (max 0 (- (cg:position-x node-pos)
                            (truncate (cg:interior-width window) 2)))
                  (max 0 (- (cg:position-y node-pos)
                            (truncate (cg:interior-height window) 2))))))
         (when highlightp
            (when (cg-user::current-type-node window)
               ;; remove existing highlighting
               (cg-user::highlight-current-type-node 
                (cg-user::current-type-node window) window))
            (setf (cg-user::current-type-node window) node-record)
            ;; make new highlighting appear
            (cg-user::highlight-current-type-node node-record window)))))


(defun display-type-node-record (node window)
   (find node (cg-user::active-type-hier-window-active-menus window) 
     :key #'cg-user::type-hier-record-node))



