;;; Copyright (c) 1991-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see LICENSE for conditions


;;; Modified 1995 for TDFS
;;; 
;;; Modified Jan 1995 to avoid unnecessary duplication in code
;;; and to allow for tdfs structures

;;; Port to MCL requires extensive rewrite
;;; very similar to the lkb4+ version


;;; The code is somewhat complex, because of the messiness associated with
;;; the scrolling windows and pop up menus, and because the basic functions are
;;; called in multiple different ways.

;;; the main entry points are a series of functions with names like
;;; display-basic-fs
;;; these functions create a window of type 'picture-field-window
;;; with a size of #@(10000 10000) which is supposed to be big
;;; enough to deal with any likely fs.  They then call
;;; display-fs-main
;;; this has two main functions
;;; 1) calls display-dag1 (and any other ancilliary display functions)
;;;  these draw the FS into the picture-field-window and
;;;  have the side effects of putting pop up menus onto the record associated
;;;  with the picture-field-window.  
;;; 2) the picture-field-window is then closed returning a pict
;;;   and the real window is created which is an active-fs-window
;;;   which displays the pict within a limited view-size
;;;   The scroller associated with the active-fs-window (i.e. the
;;;   window-pane) has its feature-structure field set to the feature structure
;;;   which we are displaying and the pop-up-menus which were originally
;;;   attached to the picture-field window are added to the window-pane
;;;   as subviews.

(in-package :lkb)


;;; This is the font for the pop up menu and the display windows. They
;;; are functions so users can change font sizes after code has loaded

(defun lkb-type-font nil
   (list (if (ccl:osx-p) "Lucida Grande" "Helvetica") *fs-type-font-size*))

(defun lkb-title-font nil
   (if (ccl:osx-p) (list "Lucida Grande" 13) (list "Chicago" *fs-title-font-size*)))


;;; ***** Records and classes *******

(defclass picture-field-window (ccl::picture-maker)
   ;; the class for the temporary windows from which the pict is derived
   ((fields :initarg fields :initform nil :accessor fields)))


(defclass active-fs-scroll-bar (ccl::scroll-bar-dialog-item) ()
   ;; scrolls one line at a time
   (:default-initargs
    :scroll-size 11))

(defclass active-fs-window (ccl::picture-window) ()
   ;; the class for the whole scrollable FS window
   ;; this contains the active-fs-window-pane
   (:default-initargs
    :scroller-class 'active-fs-window-pane))

(defclass active-fs-window-pane (ccl::picture-window-pane)
   ;; active fs window panes
   ;; these have an associated fs-record which allows manipulation of the fs being
   ;; displayed
   ;; plus a slot for the fields which contains the info needed to make a pop-up-menu
   ;; when the relevant region is clicked
   ((fields :initarg fields :initform nil :accessor fields)
    (feature-structure :initarg feature-structure :initform nil :accessor feature-structure))
   (:default-initargs 
    :scroll-bar-class 'active-fs-scroll-bar))

(defclass active-fs-pop-up-field (ccl::pop-up-field)
  ())


(defclass dynamic-enable-menu-item (menu-item)
   ;; a menu-item with an additional field containing a function of zero args
   ;; called just before menu containing item is popped up, to see if item
   ;; should be enabled or disabled at that time
   ((enable-function :initarg :enable-function :initform nil
                     :accessor dynamic-enable-menu-item-function)))

(defmethod menu-item-update ((item dynamic-enable-menu-item))
   (let ((fn (dynamic-enable-menu-item-function item)))
      (if (funcall fn) (menu-item-enable item) (menu-item-disable item))))


;;;

(defstruct fs-display-record 
   ;; the record of the FS associated with a window
   fs title paths parents type-fs-display id)

(defun set-associated-fs (stream fs title &optional paths parents type-fs-display id)
   (setf (feature-structure stream)
      (make-fs-display-record :fs fs :title title :paths paths 
         :parents parents :type-fs-display type-fs-display :id id)))


(defstruct click-field 
   ;; the record from which a pop-up-menu is constructed
   view-pos end-pos (clicked-p nil))

(defstruct (type-click-field (:include click-field))
   ;; YADU - here and below, the slot full-tdfs is needed so that
   ;; lexical rules can be displayed in the type -> type format
   ;; with the full fs associated with full-tdfs
   type-label-list type shrunk-p atomic-p top-box full-tdfs)

(defstruct (reentrancy-click-field (:include click-field))
   label valuep)

(defstruct (title-click-field (:include click-field))
   title fs)

(defstruct (psort-click-field (:include click-field))
   psort)


;;; Highlight current fs node, if there is one at the moment

(defvar *selected-fs-node* nil)

(defstruct selected-fs-node
   pane record fs path)

(defmethod view-draw-contents ((pane active-fs-window-pane))
  (call-next-method)
  (let ((selection-pane
           (and *selected-fs-node* (selected-fs-node-pane *selected-fs-node*))))
     (when (eq pane selection-pane) 
        (highlight-current-fs-node
           (selected-fs-node-record *selected-fs-node*) pane))))

(defun highlight-current-fs-node (record pane)
   (invert-text-box pane
      (type-click-field-view-pos record)
      (type-click-field-end-pos record)))


;;; pop up menus are created as separate views in the right position
;;; but only on the first click near where the node is

(defmethod view-click-event-handler ((pane active-fs-window-pane) where)
  (let ((x-pos-click (point-h where))
        (y-pos-click (point-v where))
        (ascent (font-ascent pane))
        (eps 2))
    (dolist (field (fields pane))
      (when (click-field-p field)
        (let ((x-pos-node (point-h (click-field-view-pos field)))
              (y-pos-node (point-v (click-field-view-pos field))))
          (when
              (and (> y-pos-click (- y-pos-node ascent eps))
                   (< y-pos-click (+ y-pos-node eps))
                   (> x-pos-click (- x-pos-node eps))
                   (< x-pos-click (+ (point-h (click-field-end-pos field)) eps)))
              (unless (click-field-clicked-p field)
                 (add-subviews pane
                    (create-active-fs-pop-up field
                       (make-point x-pos-node (- y-pos-node ascent))))
                 (setf (click-field-clicked-p field) t))
             (return nil)))))
    (call-next-method pane where)))


(defun create-active-fs-pop-up (field menu-pos)
  (cond ((reentrancy-click-field-p field)
           (create-active-fs-pop-up-reentrancy field menu-pos))
        ((type-click-field-p field) (create-active-fs-pop-up-type field menu-pos))
        ((title-click-field-p field) (create-active-fs-pop-up-title field menu-pos))
        ((psort-click-field-p field) (create-active-fs-pop-up-psort field menu-pos))
        (t (error "Unknown class of pop up object"))))


;;; **** display function entry points ****

;;; some redundancy here - clean up sometime

(defparameter +fs-window-view-width+ 2400)
(defparameter +fs-window-view-height+ 32000)

(defun display-basic-fs (fs title &optional parents paths old-window id)
   (when old-window (erase-region old-window (clip-region old-window)))
   (let ((fs-window 
            (make-instance 'picture-field-window
               :view-font (lkb-type-font)
               :view-size (make-point +fs-window-view-width+ +fs-window-view-height+))))
      (display-fs-main fs-window fs title parents paths
         old-window id)))

(defun display-fs (fs title &optional id)
   (display-basic-fs fs title nil nil nil id))

(defun display-fs-and-parents (fs title parents &optional id)
   (display-basic-fs fs title parents nil nil id))

(defun display-fs-and-paths (fs title paths &optional id)
   (display-basic-fs fs title nil paths nil id))

(defun redisplay-fs (old-window fs title &optional id)
   (display-basic-fs fs title nil nil old-window id))

(defun redisplay-fs-and-parents (old-window fs title parents &optional id)
   (display-basic-fs fs title parents nil old-window id))

(defun redisplay-fs-and-paths (old-window fs title paths &optional id)
   (display-basic-fs fs title nil paths old-window id))


;;; **** main display function ****

;;; very crude attempt to avoid windows displaying on top
;;; of eachother - obviously any sensible stuff would
;;; have to keep track of where windows were moved etc

(defparameter *display-positions* 
  '(#@(56 44) #@(106 44) #@(156 44) #@(206 44) #@(256 44)))

(defun find-best-position nil
  (or
  (some #'(lambda (position) 
            (if (notany #'(lambda (window)
                           (equal (view-position window)
                                  position))
                         (windows :class 'active-fs-window))
              position))
        *display-positions*)
  (car *display-positions*)))


(defun display-fs-main (fs-window fs title parents paths &optional
                        existing-window id)
   (draw-active-title fs-window fs title parents paths id)
   (let ((parents-width (if parents 
              (display-active-parents parents fs-window) 0))
         (dag-width (or (if (tdfs-p fs) (display-dag2 fs 'edit fs-window)
                              (display-dag1 fs 'edit fs-window)) 0))
         (path-width (if paths (display-active-dpaths paths fs-window) 0)))
      (let*
         ((max-x (max dag-width path-width parents-width 100))
          (full-width (min max-x +fs-window-view-width+))
          (page-width 
             (min (- *screen-width* 100)
                (max (+ 30 full-width)
                   (+ (if (ccl:osx-p) 110 80) (string-width title (lkb-title-font))))))
          (max-y (point-v (current-position fs-window)))
          (full-height
             (max 150
                (+ (if (<= 0 max-y +fs-window-view-height+) max-y +fs-window-view-height+)
                   10))) ; + 10 is a fudge factor for YADU tails
          (page-height (min full-height (- *screen-height* 100)))
          (fields (fields fs-window))
          (pict (window-close fs-window))
          (real-window
             (if existing-window
                (let ((w (view-container existing-window)))
                   (ccl::kill-picture (ccl::pict-data (ccl::my-scroller w)))
                   (setf (ccl::pict-data (ccl::my-scroller w)) pict)
                   (setf (slot-value existing-window 'ccl::field-size) ; !!! ugh
                      (make-point full-width full-height))
                   (ccl::update-scroll-bars existing-window :length t :position t)
                   (reinitialize-instance w
                      :view-size (make-point page-width page-height))
                   w)
                (make-instance 'active-fs-window
                   :window-title title
                   :pict pict
                   :view-font (lkb-type-font)
                   :view-position (find-best-position)
                   :field-size (make-point full-width full-height)
                   :close-box-p t
                   :view-size (make-point page-width page-height)))))
        (set-associated-fs (ccl::my-scroller real-window) fs title paths parents
           (if existing-window
              (fs-display-record-type-fs-display
                 (feature-structure (ccl::my-scroller real-window)))
              *type-fs-display*))
        (setf (fields (ccl::my-scroller real-window)) (nreverse fields))
        (invalidate-view real-window)
        real-window)))


(defun display-active-dpaths (dpath-list ostream)
   (let ((max-width 0))
      (dolist (unif dpath-list)
         (output-unif unif ostream t)
         (setf max-width (max max-width (current-position-x ostream))))
      max-width))


(defun add-active-fs-region (stream start-pos end-pos type-label-list type shrunk-p 
      atomic-p &optional top-box full-tdfs)
   ;; record info about position of data in active window
   (push
      (make-type-click-field :view-pos start-pos :end-pos end-pos
         :type-label-list type-label-list :type type :shrunk-p shrunk-p
         :atomic-p atomic-p :top-box top-box :full-tdfs full-tdfs)
      (fields stream)))


;;; the following is called from functions in output(td)fs.lsp

(defun add-type-and-active-fs-region (stream start-pos type-label-list val
                                      shrunk-p atomic-p &optional top-box full-tdfs)
   (with-text-style-bold-face (stream)
      (write-string (string-downcase (string val)) stream))
   (add-active-fs-region stream start-pos (current-position stream) type-label-list val
      shrunk-p atomic-p top-box full-tdfs))


;;; **** displaying parents and paths ***

(defun display-active-parents (parents ostream)
   ;; this function is dedicated to my Mother and Father
   (format ostream "~%Parents = ")
   (flet ((display-individual-parent (parent ostream endp)
            (let ((start-pos (current-position ostream)))
               (with-text-style-bold-face (ostream)
                  (format ostream "~(~A~)" parent)
                  (add-active-fs-region ostream start-pos (current-position ostream)
                     nil parent nil t)
                  (unless endp (format ostream "  " parent))))))
      (dolist (parent parents)
         (if (consp parent) ; it's actually a list of non-glbtype parents
            (progn
               (format ostream "(")
               (mapl
                  #'(lambda (ptail)
                     (display-individual-parent (car ptail) ostream (null (cdr ptail))))
                  parent)
               (format ostream ")  "))
            (display-individual-parent parent ostream nil)))
      (let ((max-width (current-position-x ostream)))
         (format ostream "~%")
         max-width)))


;;; ***** Pop up menu creation *****
;;;
;;; There are several sorts of pop up menu which can occur
;;;
;;; 1. pop up menus for types - normally within FSs but also as parents
;;; 2. pop up menu on the top of a FS window - allows output of the
;;;    FS as a whole in TeX, storage as a psort etc
;;; 3. pop up menus for psorts displayed in paths


;;; **** pop up menus for types in FSs ****

(defun create-active-fs-pop-up-type (field menu-pos)
  (let* ((type-label-list (type-click-field-type-label-list field))
         (type (type-click-field-type field))
         (shrunk-p (type-click-field-shrunk-p field))
         (atomic-p (type-click-field-atomic-p field))
         (full-structure (type-click-field-full-tdfs field))
         (type-entry (get-type-entry type))
         (type-p (if atomic-p :atomic :fs))
        (menu (make-instance 'active-fs-pop-up-field
                 :view-position menu-pos
                 :item-font (lkb-type-font)
                 :item-string (format nil "~(~A~)" type)
                 :view-font (lkb-dialog-font)
                 :shrunk-p shrunk-p)))
    (apply #'add-menu-items menu
          (pop-up-fs-menu-items (if (listp type) (car type) type) field
                                type-entry shrunk-p type-p menu type-label-list
                                full-structure))
    menu))


(defmethod view-click-event-handler :before ((menu active-fs-pop-up-field) (where t))
   ;; used to blank out label in case it was highlighted, but wasn't correct
   )

(defmethod set-pop-up-menu-default-item ((menu active-fs-pop-up-field) num)
   ;; don't allow the menu mechanism to mark a menu item as default
   (declare (ignore num))
   nil)


(defun pop-up-fs-menu-items (type field type-entry shrunk-p type-p menu
                             type-label-list full-structure)
  (if type-entry
  (list
   (make-instance 'menu-item
     :menu-item-title "Hierarchy"
     :menu-item-action
     #'(lambda ()
         (display-type-in-tree type))
     :disabled (not (type-constraint type-entry)))
   (make-instance 'menu-item
     :menu-item-title "Help"
     :menu-item-action
     #'(lambda ()
         (display-type-comment type (type-comment type-entry)))
     :disabled (not (type-comment type-entry)))
   (make-instance 'menu-item
     :menu-item-title "Shrink/expand"
               :menu-item-action #'(lambda ()
                              (shrink-fs-action (view-container menu) 
                                                (if shrunk-p :expand :shrink)
                                                type-label-list))
                   :disabled (not (eql type-p :fs)))
   ;; (make-instance 'menu-item
   ;;   :menu-item-title "Show source"
   ;;   )
   (make-instance 'menu-item
     :menu-item-title "Type definition"
     :menu-item-action 
      #'(lambda () (show-type-spec-aux type type-entry)))
   (make-instance 'menu-item
     :menu-item-title "Expanded type"
     :menu-item-action
     #'(lambda () (show-type-aux type type-entry)))
   (make-instance 'menu-item 
     :menu-item-title "Full structure"
     :menu-item-action
     #'(lambda ()
         (display-basic-fs full-structure
                           (format nil "LR constraint")))
     :disabled (not full-structure))
   (make-instance 'menu-item
     :menu-item-title "Select"
     :menu-item-action 
     #'(lambda ()
         (select-fs (view-container menu) field (reverse type-label-list))))
   (make-instance 'dynamic-enable-menu-item
     :menu-item-title "Unify"
     :menu-item-action 
     #'(lambda ()
         (try-unify-fs (view-container menu) field (reverse type-label-list)))
     :enable-function
     #'(lambda nil
         (and *selected-fs-node*
              (listp (selected-fs-node-path *selected-fs-node*)))))
   )))


(defun shrink-fs-action (window action path)
  (let* ((fs-record (feature-structure window))
         (fs (fs-display-record-fs fs-record))
         (title (fs-display-record-title fs-record))
         (parents (fs-display-record-parents fs-record))
         (paths (fs-display-record-paths fs-record))
         (type-fs-display (fs-display-record-type-fs-display fs-record)))
    (set-dag-display-value fs (reverse path) action type-fs-display)
            (cond 
               ((tdfs-p fs) ; YADU
                  (redisplay-fs window fs title))
               (parents
                  (redisplay-fs-and-parents window fs title parents))
               (paths (redisplay-fs-and-paths window fs title paths))
               (t (redisplay-fs window fs title)))))

(defun display-type-comment (type comment-string &optional parent-stream)
   (let ((existing (find-window "Explanation of types")))
      (unless existing
         (setf existing
                   (make-instance 'fred-window :window-title "Explanation of types"
                                               :view-position #@(287 540)
                                               :view-size #@(450 74))))
      (format existing "~%~A ~A" type comment-string) 
      (invalidate-view existing)
      (set-window-layer existing *windoid-count*)
      (when parent-stream
         (window-select parent-stream))))



;;; **** the title or top pop up menu ****

(defun draw-active-title (stream fs title parents paths id)
   ;; creates a pop up menu 
   (format stream "~%")
   (let ((start-pos (current-position stream))
         (short-title (subseq title 0 (position #\Space title)))
         (fs-record
            (make-fs-display-record :fs fs :title title :paths paths 
                                    :parents parents :id id)))
     (with-underlined-output stream
        (format stream "~A" short-title))
     (push
        (make-title-click-field :view-pos start-pos
                                :end-pos (current-position stream)
                                :title short-title
                                :fs fs-record)
        (fields stream))
     (terpri stream)))


(defun create-active-fs-pop-up-title (field menu-pos)
  (let* ((title (title-click-field-title field))
         (fs-record (title-click-field-fs field))
         (menu
            (make-instance 'active-fs-pop-up-field
                           :view-position menu-pos
                           :item-font (lkb-list-font)
                           :item-string (format nil "~A" title)
                           :view-font (lkb-dialog-font))))
     (apply #'add-menu-items menu
            (top-fs-action fs-record))
     menu))
   

(defun top-fs-action (fs-record)
  (let ((id (fs-display-record-id fs-record)))
  (list
;;;   (make-instance 'menu-item
;;;     :menu-item-title "Print shrunk"
;;;    :menu-item-action
;;;     #'(lambda () (eval-enqueue  `(print-fs-plus ,fs-record t)))
;;;        :disabled t)
;;;   (make-instance 'menu-item
;;;     :menu-item-title "Print expanded"
;;;     :menu-item-action
;;;     #'(lambda () (eval-enqueue  `(print-fs-plus ,fs-record)))
;;;     :disabled t)
   (make-instance 'menu-item
     :menu-item-title "Output TeX..."
     :menu-item-action 
     #'(lambda () (eval-enqueue `(output-fs-in-tex ',fs-record))))
   (make-instance 'menu-item
     :menu-item-title "Apply lex rule ..."
     :menu-item-action 
     #'(lambda () (eval-enqueue `(apply-lex ',id)))
     :disabled (not (and id (get-lex-entry-from-id id))))
   (make-instance 'menu-item
     :menu-item-title "Apply all lex rules"
     :menu-item-action 
     #'(lambda () (eval-enqueue `(apply-lex-rules ',id)))
     :disabled (not (and id (get-lex-entry-from-id id))))
;;;   (make-instance 'menu-item
;;;     :menu-item-title "Store fs..."
;;;     :menu-item-action 
;;;     #'(lambda () (eval-enqueue `(store-as-psort ,fs-record))))
;;;   (make-instance 'menu-item
;;;     :menu-item-title "LDB entry"
;;;     :menu-item-action 
;;;     #'(lambda () (show-ldb-entry fs-record))
;;;     :disabled (not (boundp '*dictionaries-available*)))
)))


(defun output-fs-in-tex (fs-record)
   (let ((fs (fs-display-record-fs fs-record)))
      (when fs
         (let ((file-name 
                  (ask-user-for-new-pathname "File for LaTeX macros?")))                          
            (when file-name
               (with-open-file (stream file-name :direction :output)
                  (if (tdfs-p fs)
                     (progn (format stream "~%% Indef dag~%")
                        (display-dag1 (tdfs-indef fs) 'tex stream)
                        (format stream "~%% Def dag~%")
                        (display-dag1 (yadu-winner fs) 'tex stream))
                     (display-dag1 fs 'tex stream))))))))


(defun store-as-psort (fs-record)
   (let ((psort-name 'no-name)
         (fs (fs-display-record-fs fs-record)))
      (when fs
           (setf psort-name 
             (car
              (with-package (:lkb)
               (ask-for-lisp-movable "Current Interaction" 
                  `(("Lex-id?" . ,psort-name))
                  nil))))
            (if psort-name
              (or 
               (store-temporary-psort *lexicon* psort-name fs)
               (cerror "Try Again" "Name already used"))))))



;;; **** pop up menus for psorts (called when paths are displayed) *****
;;; shouldn't happen in YADU

(defun display-active-psort (psort ostream)
   (let ((start-pos (current-position ostream)))
     (with-text-style-bold-face (ostream)
        (format ostream "~A  " psort))
     (push
        (make-psort-click-field :view-pos start-pos
                                :end-pos (current-position ostream)
                                :psort psort)
        (fields ostream))))

(defun create-active-fs-pop-up-psort (field menu-pos)
  (let* ((psort (psort-click-field-psort field))
         (menu (make-instance 'active-fs-pop-up-field
                 :view-position menu-pos
                 :item-string (format nil "~A" psort)
                 :item-font (cons :bold (lkb-type-font))
                 :view-font (lkb-dialog-font))))
    (apply #'add-menu-items menu
      (let ((lex-entry (if psort (get-lex-entry-from-id psort))))
        (if lex-entry 
          (pop-up-psort-menu-items psort lex-entry)
          (let ((lex-rule-entry 
                  (get-lex-rule-entry psort)))
            (if lex-rule-entry
               (pop-up-lex-rule-menu-items psort lex-rule-entry))))))
    menu))


(defun pop-up-psort-menu-items (psort lex-entry)
  (list
; following removed for YADU to avoid compiler warning
; this fn should never get called anyway
;     (make-instance 'menu-item
;     :menu-item-title "Entry definition"
;               :menu-item-action 
;               #'(lambda ()
;                       (display-unexpanded-lex-entry psort lex-entry)))
    (make-instance 'menu-item
     :menu-item-title "Expanded entry"
     :menu-item-action 
     #'(lambda ()
         (display-fs (lex-entry-full-fs lex-entry) 
                     (format nil "~(~A~) - expanded" psort)
                     psort))))) 


(defun pop-up-lex-rule-menu-items (psort rule-entry)
  (declare (ignore psort))
   (list
     (make-instance 'menu-item
     :menu-item-title "Show rule"
     :menu-item-action 
     #'(lambda ()
         (display-fs (rule-full-fs rule-entry) 
                     (format nil "~(~A~)" (rule-id rule-entry))
                     (rule-id rule-entry)
                     )))))



;;; Support for interactive unification check

(defun pane-toplevel-dag (pane)
   (let ((fs (fs-display-record-fs (feature-structure pane))))
      (if (tdfs-p fs)
         (tdfs-indef fs)
         fs)))

(defun select-fs (pane current-fs-field path)
   ;; remove existing highlighting in any window
   (highlight-current-fs-node-any-window)
   (setf *selected-fs-node*
      (make-selected-fs-node
       :pane pane :record current-fs-field 
       :fs (pane-toplevel-dag pane) :path path))
   ;; make new highlighting appear
   (highlight-current-fs-node-any-window))


(defun try-unify-fs (pane current-fs-field path2)
   (declare (ignore current-fs-field))
   (let* ((sel1 *selected-fs-node*)
          (path1 (selected-fs-node-path sel1)))
      (when (listp path1)
        (let ((result 
         (unify-paths-with-fail-messages 
            (create-path-from-feature-list path1)
            (selected-fs-node-fs sel1)
            (create-path-from-feature-list path2)
            (pane-toplevel-dag pane)
            :selected1 path1 :selected2 path2)))
          (terpri)
         (when result
            (display-fs result "Unification result")))
         (highlight-current-fs-node-any-window)
         (setq *selected-fs-node* nil))))


(defun highlight-current-fs-node-any-window nil
   (when *selected-fs-node*
      (dolist (w (windows :class 'active-fs-window))
         (when (eq (ccl::my-scroller w) (selected-fs-node-pane *selected-fs-node*))
            (highlight-current-fs-node
               (selected-fs-node-record *selected-fs-node*) (ccl::my-scroller w))))))


;;; **********************************************************************

(defun create-active-fs-pop-up-reentrancy (field menu-pos)
  (let* ((label (reentrancy-click-field-label field))
         (valuep (reentrancy-click-field-valuep field))
         (menu (make-instance 'active-fs-pop-up-field
                  :view-position menu-pos
                  :item-string (format nil "<~A>" label)
                  :item-font (cons :bold (lkb-type-font))
                  :view-font (lkb-dialog-font))))
      (apply #'add-menu-items menu
         (list
            (make-instance 'menu-item
               :menu-item-title "Find value"
               :menu-item-action 
               #'(lambda ()
                   (select-fs-node-label label (view-container menu) nil))
               :disabled valuep)
            (make-instance 'menu-item
               :menu-item-title "Find next"
               :menu-item-action 
               #'(lambda ()
                   (select-fs (view-container menu) field t)
                   (select-fs-node-label label (view-container menu) field)))))
      menu))


(defun select-fs-node-label (label pane current)
   (let* ((passed-current-p nil)
          (record
            (find-if
               #'(lambda (r)
                   (and (reentrancy-click-field-p r)
                      (eql (reentrancy-click-field-label r) label)
                      (if current
                         (if passed-current-p t
                            (progn
                               (when (eq r current)
                                  (setq passed-current-p t))
                               nil))
                         (reentrancy-click-field-valuep r))))
               (fields pane))))
      (when record
         (let ((node-pos (reentrancy-click-field-view-pos record)))
            (unless
               (let ((eps (make-point 15 15)))
                  (inside-box-p node-pos
                     ;; make slightly smaller box than full area of visible pane
                     (cons (add-points (view-scroll-position pane) eps)
                        (subtract-points
                           (add-points (view-scroll-position pane) (view-size pane))
                           eps))))
               (set-view-scroll-position pane 0
                  (max 0 ; only scroll vertically
                     (- (point-v node-pos)
                        (truncate (point-v (view-size pane)) 2)))))
            (select-fs pane record t)))))


(defun add-active-pointer (stream position pointer ignore valuep)
   (declare (ignore ignore))
   (write-char #\< stream) (princ pointer stream) (write-char #\> stream)
   (let ((record
            (make-reentrancy-click-field :view-pos position
               :end-pos (current-position stream)
               :label pointer :valuep valuep)))
      (push record (fields stream))
      (when valuep
         (write-string " = " stream))))

;;; ******* Dummy functions - only used in Common Graphics version **********

(defun store-fs-record-data (stream rpath flag pointer)
  (declare (ignore stream rpath flag pointer))
  nil)
  
(defun store-fs-record-data-label (stream rpath)
  (declare (ignore stream rpath))
  nil)
    
(defun store-fs-record-data-end (stream rpath)
    (declare (ignore stream rpath))
  nil)

(defun store-fs-redisplay (stream rpath box)
    (declare (ignore stream rpath box))
    nil)
