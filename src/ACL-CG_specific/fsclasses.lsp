;;; Classes and methods for various types of graphical
;;; object in ACL 5.0 with common graphics.
;;; Kept in a separate file, because it's easier if these
;;; are in the common-graphics user package
;;; - the rest of the CG specific files are in CL-USER


(in-package :common-graphics-user)

;;; ****** active feature structure display ********************

;;; record for a FS associated wirh a window

(defstruct edit-fs-record
   box path value type-p shrunk-p top-p)

(defclass active-fs-window (frame-with-single-child) 
    ((feature-structure :accessor active-fs-window-feature-structure
       :initform nil) 
     (active-menus :accessor active-fs-window-active-menus
       :initform nil)))

(defclass active-fs-pane (basic-pane) ())

(defmethod default-pane-class ((w active-fs-window)) 'active-fs-pane)

(defmethod redisplay-window 
    ((stream active-fs-pane)
     &optional box)
   (call-next-method stream box)
   (lkb::display-fs-main (parent stream) 
    box))

(defmethod mouse-right-down
    ((stream active-fs-pane)
     button-state position)
   (declare (ignore button-state))
   (let*
        ((window (parent stream))
         (fs-record (active-fs-window-feature-structure window))
         (found
            (dolist (record
                      (active-fs-window-active-menus window))
               (when (inside-box-p position 
                          (edit-fs-record-box record))
                (return record)))))
      (when found
         (lkb::perform-view-fs-action window
            fs-record
            (edit-fs-record-path found) 
            (edit-fs-record-value found)
            (edit-fs-record-type-p found)
            (edit-fs-record-shrunk-p found)
            (edit-fs-record-top-p found)))))

#|
;;; if there are artefacts in the FS display, then it's
;;; useful to be able to redisplay quickly, but the windows
;;; consultant says this should be F5 
(defmethod mouse-left-down
    ((stream active-fs-pane)
     button-state position)
   (declare (ignore button-state position))
   (redisplay-window stream (visible-box (parent stream))))
|#

(defun add-fs-region-record (stream box path
       val type-p shrunk-p top-box)
   (push
    (make-edit-fs-record :box
     box
     :path path :value val 
     :type-p type-p :shrunk-p shrunk-p
     :top-p top-box)
    (active-fs-window-active-menus stream)))

;;; ************** parse tree display ***********************

(defstruct parse-tree-record 
   box value)

(defclass active-parse-tree-window (bitmap-window) 
    ((active-menus :accessor active-parse-tree-window-active-menus
       :initform nil)))

;;; this was a frame with single child, and the pane inherited from
;;; bitmap-pane plus comtab:comtab-mixin - which seems to have gone
;;; Since I can't think why this could have been right anyway
;;; I've just changed everything to bitmap-window for now

(defmethod default-pane-class ((w active-parse-tree-window))
   'active-parse-tree-pane)

(defclass active-parse-tree-pane (bitmap-pane) ())

(defmethod mouse-right-down
    ((stream active-parse-tree-pane)
     button-state position)
   (declare (ignore button-state))
   (let*
        ((window (parent stream))
         (found
            (dolist (record
                      (active-parse-tree-window-active-menus window))
               (when (inside-box-p position 
                          (parse-tree-record-box record))
                (return record)))))
      (when found
         (lkb::perform-view-node-action window
            (parse-tree-record-value found)))))

(defun add-parse-tree-region-record (stream box edge-symbol)
   (push
     (make-parse-tree-record :box box :value edge-symbol)
    (active-parse-tree-window-active-menus stream)))


;;; ************** type hierarchy display ***********************

(defstruct type-hier-record
   box node)

(defclass active-type-hier-window (bitmap-window) 
    ((active-menus :accessor active-type-hier-window-active-menus
       :initform nil :initarg active-type-hier-window-active-menus)
     (current-type-node :initarg type-tree :initform nil 
      :accessor current-type-node)
     (top-type-node :initarg top-type-node :initform nil 
      :accessor top-type-node)
     (show-all-p :initarg show-all-p :initform nil 
      :accessor show-all-p)))

(defmethod default-pane-class ((w active-type-hier-window))
   'active-type-hier-pane)

(defclass active-type-hier-pane (bitmap-pane) ())

#|
;;; Commented out because 
;;; redisplay of bitmap is not happening on NT if this is defined
 
;;; Highlight current node, if there is one at the moment

(defmethod redisplay-window ((pane active-type-hier-pane) &optional box)
  (call-next-method pane box)
  (let ((record (current-type-node (parent pane))))
     (when record 
        (highlight-current-type-node record pane))))
|#

(defun highlight-current-type-node (record pane)
   (cg:invert-box pane
      (type-hier-record-box record)))

(defmethod mouse-right-down
    ((stream active-type-hier-pane)
     button-state position)
   (declare (ignore button-state))
   (let*
        ((window (parent stream))
         (found
            (dolist (record
                      (active-type-hier-window-active-menus window))
               (when (inside-box-p position 
                          (type-hier-record-box record))
                (return record)))))
      (when found
         (lkb::perform-type-hier-node-action stream window
            (type-hier-record-node found)))))

(defun add-type-hier-region-record (stream box node)
   (push
     (make-type-hier-record :box box :node node)
    (active-type-hier-window-active-menus stream)))


;;; ***** list output ********


(defstruct active-list-record
   box item menu)

(defclass active-list-window (bitmap-window) 
    ((active-menus :accessor active-list-window-active-menus
       :initform nil :initarg active-list-window-active-menus)))

(defmethod default-pane-class ((w active-list-window))
   'active-list-pane)

(defclass active-list-pane (bitmap-pane) ())

(defmethod mouse-right-down
    ((stream active-list-pane)
     button-state position)
   (declare (ignore button-state))
   (let*
        ((window (parent stream))
         (found
            (dolist (record
                      (active-list-window-active-menus window))
               (when (inside-box-p position 
                          (active-list-record-box record))
                (return record)))))
      (when found
         (lkb::perform-list-node-action window
          (active-list-record-item found)
          (active-list-record-menu found)))))

(defun add-active-list-region-record (item stream box menu)
   (push
     (make-active-list-record :box box :item item :menu menu)
    (active-list-window-active-menus stream)))
