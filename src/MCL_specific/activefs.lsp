;;; Copyright Ann Copestake 1991-7 All Rights Reserved.
;;; No use or redistribution without permission.

;;; Modified 1995 for TDFS
;;; 
;;; Modified Jan 1995 to avoid unnecessary duplication in code
;;; and to allow for tdfs structures

;;; Port to MCL requires extensive rewrite
;;; very similar to the lkb4+ version


;;; The code is somewhat complex, because of the messiness associated with
;;; the scrolling windows and pop up menus, and because the basic functions are
;;; called in multiple different ways.

;;; the following is called from functions in output(td)fs.lsp

(defun add-type-and-active-fs-region (stream start-pos type-label-list val
                                             shrunk-p atomic-p &optional top-box full-tdfs)
  (with-bold-output stream (format stream "~(~A~)" val))
  (add-active-fs-region stream start-pos type-label-list val shrunk-p atomic-p 
                        top-box full-tdfs))

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

;;; This is the font for the pop up menu and the display windows

(defparameter *type-font* (list "Helvetica" *fs-type-font-size*))

(defparameter *title-font* (list "Chicago" *fs-title-font-size*))

;;; ***** Records and classes *******

(defclass picture-field-window (ccl::picture-maker)
;;; the class for the temporray windows from which the pict is derived
  ((fields :initarg fields :initform nil :accessor fields)))


(defclass active-fs-scroll-bar (ccl::scroll-bar-dialog-item) ()
  ;; scrolls one line at a time
  (:default-initargs 
    :scroll-size 11))

(defclass active-fs-window (ccl::picture-window) ()
;;; the class for the whole scrollable FS window
;;; this contains the active-fs-window-pane
  (:default-initargs 
   :scroller-class 'active-fs-window-pane))

(defclass active-fs-window-pane (ccl::picture-window-pane)
;;; active fs window panes
;;; these have an associated fs-record which allows manipulation of the fs being
;;; displayed
;;; plus a slot for the fields which contains the info needed to make a pop-up-menu
;;; when the relevant region is clicked
   ((fields :initarg fields :initform nil :accessor fields)
    (feature-structure :initarg feature-structure :initform nil :accessor feature-structure))
   (:default-initargs 
    :scroll-bar-class 'active-fs-scroll-bar))

(defstruct fs-display-record 
;;; the record of the FS associated with a window
   fs title paths parents)

(defun set-associated-fs (stream fs title &optional paths parents)
   (setf (feature-structure stream)
      (make-fs-display-record :fs fs :title title :paths paths 
         :parents parents)))

(defstruct click-field 
  ;;; the record from which a pop-up-menu is constructed
  view-pos (clicked-p nil))

;;; YADU - here and below, the slot full-tdfs is needed so that
;;; lexical rules can be displayed in the type -> type format
;;; with the full fs associated with full-tdfs

(defstruct (type-click-field (:include click-field))
    type-label-list type shrunk-p atomic-p top-box full-tdfs)

(defstruct (title-click-field (:include click-field))
  title fs)

(defstruct (psort-click-field (:include click-field))
  psort)

(defmethod view-click-event-handler ((item active-fs-window-pane) where)
  (let ((x-pos-click (point-h where))
        (y-pos-click (point-v where)))
  (dolist (field (fields item))
    (when (click-field-p field)
      (when
        (let* ((menu-pos (click-field-view-pos field))
                 (x-pos-menu (point-h menu-pos))
                 (y-pos-menu (point-v menu-pos)))
            (and (> x-pos-click (- x-pos-menu 5))
                 (< x-pos-click (+ x-pos-menu 80))
                 (> y-pos-click (- y-pos-menu 2))
                 (< y-pos-click (+ y-pos-menu 10))))
        (unless (click-field-clicked-p field)
          (add-subviews item (create-active-fs-pop-up field))
          (setf (click-field-clicked-p field) t))
      (return nil))))
  (call-next-method item where)))

(defun create-active-fs-pop-up (field)
  (cond ((type-click-field-p field) (create-active-fs-pop-up-type field))
        ((title-click-field-p field) (create-active-fs-pop-up-title field))
        ((psort-click-field-p field) (create-active-fs-pop-up-psort field))
        (t (error "Unknown class of pop up object"))))

;;; **** display function entry points ****

;;; some redundancy here - clean up sometime

(defun display-basic-fs (fs title &optional parents paths output-fs old-window)
   (when old-window (erase-region old-window (clip-region old-window)))
   (let ((fs-window 
            (make-instance 'picture-field-window
               :view-font *type-font* :view-size #@(1600 32000))))
      (display-fs-main fs-window fs title parents paths output-fs
         old-window)))

(defun display-fs (fs title)
   (display-basic-fs fs title))

(defun display-fs-and-parents (fs title parents)
   (display-basic-fs fs title parents))

(defun display-fs-and-paths (fs title paths)
   (display-basic-fs fs title nil paths))

(defun redisplay-fs (old-window fs title)
   (display-basic-fs fs title nil nil nil old-window))

(defun redisplay-fs-and-parents (old-window fs title parents)
   (display-basic-fs fs title parents nil nil old-window))

(defun redisplay-fs-and-paths (old-window fs title paths)
   (display-basic-fs fs title nil paths nil old-window))


(defun display-lrule-window (input-tdfs output-tdfs title)
   (let ((fs-window 
          (make-instance 'picture-field-window
               :view-font *type-font* :view-size #@(10000 10000))))
      (display-fs-main fs-window input-tdfs title nil nil output-tdfs)))



;;; *** main display function ***

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

(defun display-fs-main (fs-window fs title parents paths &optional lrule-out
                        existing-window)
  ;;; extra argument (optional) for B+C96 lrule which causes different display
  ;;; function to be called
   (draw-active-title fs-window fs title parents paths)
   (let* ((parents-width (if parents 
               (display-active-parents parents fs-window) 0))
         (dag-width (or (if (tdfs-p fs) (display-dag2 fs 'edit fs-window)
                            (if lrule-out
                              (display-lrule fs lrule-out fs-window)
                              (display-dag1 fs 'edit fs-window))) 0))
         (path-width (if paths (display-active-dpaths paths fs-window) 0))
         (max-width (max dag-width path-width parents-width 100)))
      (terpri fs-window)
      (let* ((full-height (max 150 (+ 10 (point-v (current-position fs-window)))))
             ; + 10 is a fudge factor for YADU tails
             (page-height (min full-height (- *screen-height* 100)))
             (page-width 
              (min (- *screen-width* 100)
                 (max (+ 30 max-width)
                    (+ 80 (string-width title *title-font*)))))
             (fields (fields fs-window))
             (pict (window-close fs-window))
             (real-window
                (if existing-window
                   (let ((w (view-container existing-window)))
                      (ccl::kill-picture (ccl::pict-data (ccl::my-scroller w)))
                      (setf (ccl::pict-data (ccl::my-scroller w)) pict)
                      (setf (slot-value existing-window 'ccl::field-size) ; !!! ugh
                         (make-point page-width full-height))
                      (ccl::update-scroll-bars existing-window :length t :position t)
                      (reinitialize-instance w
                         :view-size (make-point page-width full-height))
                      w)
                   (make-instance 'active-fs-window
                      :window-title title
                      :pict pict
                      :view-font *type-font*
                      :view-position (find-best-position)
                      :field-size (make-point page-width full-height)
                      :close-box-p t
                      :view-size (make-point page-width page-height)))))
        (set-associated-fs (ccl::my-scroller real-window) fs title paths parents)
        (setf (fields (ccl::my-scroller real-window)) fields)
        (invalidate-view real-window)
        real-window)))

;;; **** displaying parents and paths ***

(defun display-active-parents (parents ostream)
   ;;; this function is dedicated to my Mother and Father
  (format ostream "~%Parents = ")
    (for parent in parents
         do
         (let ((start-pos (current-position ostream)))
           (with-bold-output ostream
             (format ostream "~(~A~)   " parent))
           (add-active-fs-region ostream start-pos nil parent
                                 nil t)))
    (let ((max-width (current-position-x ostream)))
      (format ostream "~%")
      max-width))




(defun display-active-dpaths (dpath-list ostream)
   (let ((max-width 0))
      (for unif in dpath-list
         do
         (output-unif unif ostream t)
         (setf max-width (max max-width (current-position-x ostream))))
      max-width))


;;; YADU
;;;
;;; This displays lexical rules as input type -> output type
;;; but adds an extra menu item to the type menu so that the 
;;; full tdfs can be displayed.

(defun display-lrule (input output ostream)
   (let ((max-width 0) (input-type (indef-type-of-tdfs input))
         (output-type (indef-type-of-tdfs output)))
      (format ostream "~% ")
      (let ((start-pos (current-position ostream)))
         (with-bold-output ostream
             (format ostream "~(~A~)   " input-type))
         (add-active-fs-region ostream start-pos nil input-type nil t nil input))
      (format ostream "  ->  ")       
      (let ((start-pos (current-position ostream)))
         (with-bold-output ostream
             (format ostream "~(~A~)   " output-type))
         (add-active-fs-region ostream start-pos nil output-type nil t nil output))
      (setf max-width 
         (max max-width (current-position-x ostream)))
      (format ostream "~%")
      max-width))


;;; ***** Pop up menu creation *****
;;;
;;; There are several sorts of pop up menu which can occur
;;;
;;; 1. pop up menus for types - normally within FSs but also as parents
;;; 2. pop up menu on the top of a FS window - allows output of the
;;;    FS as a whole in TeX, storage as a psort etc
;;; 3. pop up menus for psorts displayed in paths

;;; **** pop up menus for types in FSs ****

;;; add-active-fs-region is called by the fns in outputfs.lsp
;;; it creates a new pop-up-menu which is added to the fields
;;; of the temporary window which is being created at this point
;;; eventually the pop-up-menu is added to the subviews of the 
;;; active-fs-window-pane

(defun add-active-fs-region (stream start-pos type-label-list val shrunk-p 
      atomic-p &optional top-box full-tdfs)
  ;;; YADU --- full-tdfs for lrule display
    (push (new-field (subtract-points start-pos (make-point 0 (font-ascent stream)))
                   type-label-list val shrunk-p atomic-p top-box full-tdfs)
          (fields stream)))

(defun new-field (view-pos type-label-list type shrunk-p atomic-p top-box full-tdfs)
  ;;; YADU --- full-tdfs for lrule display
  (make-type-click-field :view-pos view-pos :type-label-list type-label-list
                    :type type :shrunk-p shrunk-p :atomic-p atomic-p
                    :top-box top-box :full-tdfs full-tdfs))


(defun create-active-fs-pop-up-type (field)
  ;;; YADU --- full-tdfs for lrule display
  (let* ((view-pos (type-click-field-view-pos field))
         (type-label-list (type-click-field-type-label-list field))
         (type (type-click-field-type field))
         (shrunk-p (type-click-field-shrunk-p field))
         (atomic-p (type-click-field-atomic-p field))
;         (top-box (type-click-field-top-box field))
         (full-structure (type-click-field-full-tdfs field))
         (type-entry (get-type-entry (if (listp type) (car type) type)))
         (type-p (if atomic-p :atomic :fs))
        (menu (make-instance 'ccl::pop-up-field
                 :view-position view-pos
                 :item-display (format nil "~(~A~)" type)
                 :view-font (cons :bold *type-font*)
                 :shrunk-p shrunk-p)))
    (apply #'add-menu-items menu
          (pop-up-fs-menu-items (if (listp type) (car type) type) 
                                type-entry shrunk-p type-p menu type-label-list
                                full-structure))
    menu))


(defun pop-up-fs-menu-items (type type-entry shrunk-p type-p menu type-label-list
                                  full-structure)
  ;;; YADU --- full-tdfs for lrule display
  (if type-entry
  (list
   (make-instance 'menu-item
     :menu-item-title "Hierarchy"
     :menu-item-action
     #'(lambda ()
         (display-type-in-tree type)))
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
   (make-instance 'menu-item 
     :menu-item-title "Full structure"
                     :menu-item-action #'(lambda ()
                                (display-basic-fs full-structure
                                 (format nil 
                                    "LR constraint")))
                     :disabled (not full-structure))
   (make-instance 'menu-item
     :menu-item-title "Type indef defn"
     :menu-item-action 
     #'(lambda ()
         (if (type-constraint type-entry)
           (display-fs-and-parents (type-local-constraint type-entry) 
                                   (format nil 
                                           "~(~A~)  - definition" 
                                           type)
                                   (type-parents type-entry))
           (format t "~%No constraint for type ~A" type))))
   (make-instance 'menu-item
     :menu-item-title "Expanded type"
     :menu-item-action
     #'(lambda ()
         (if (type-tdfs type-entry) ; for YADU instead of type-constraint
           (display-basic-fs (type-tdfs type-entry) 
                                (format nil 
                                        "~(~A~) - TDFS" 
                                        type))
           (format t "~%No tdfs for type ~A" type)))))))


;;; ***** pop up menu actions for types ******

(defun shrink-fs-action (window action path)
  (let* ((fs-record (feature-structure window))
         (fs (fs-display-record-fs fs-record))
         (title (fs-display-record-title fs-record))
         (parents (fs-display-record-parents fs-record))
         (paths (fs-display-record-paths fs-record)))
    (set-dag-display-value fs (reverse path) action)
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




;;; *** the title or top pop up menu ****

(defun draw-active-title (stream fs title parents paths)
  ;;; creates a pop up menu 
   (let ((start-pos (current-position stream))
         (short-title (subseq title 0 (position #\Space title)))
         (fs-record (make-fs-display-record :fs fs :title title :paths paths 
         :parents parents)))
     (with-underlined-output stream
      (format stream "~%~A~%" short-title))
      (push (create-fs-top-menu short-title start-pos fs-record)
        (fields stream))))


(defun create-fs-top-menu (title view-pos fs-record)
  (make-title-click-field :view-pos view-pos :title title
                          :fs fs-record))


(defun create-active-fs-pop-up-title (field)
  (let* ((view-pos (title-click-field-view-pos field))
         (title (title-click-field-title field))
         (fs-record (title-click-field-fs field))
         (menu
     (make-instance 'ccl::pop-up-field
                 :view-position view-pos
                 :item-display (format nil "~A" title))))
      (apply #'add-menu-items menu
          (top-fs-action fs-record))
    menu))
   

(defun top-fs-action (fs-record)
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
     :menu-item-title "Output TeX"
     :menu-item-action 
     #'(lambda () (eval-enqueue `(output-fs-in-tex ,fs-record))))
   (make-instance 'menu-item
     :menu-item-title "Store fs"
     :menu-item-action 
     #'(lambda () (eval-enqueue `(store-as-psort ,fs-record))))
;;;   (make-instance 'menu-item
;;;     :menu-item-title "LDB entry"
;;;     :menu-item-action 
;;;     #'(lambda () (show-ldb-entry fs-record))
;;;     :disabled (not (boundp '*dictionaries-available*)))
))


;;; **** pop up menus for psorts (called when paths are displayed) *****
;;; shouldn't happen in YADU

;;; display-fs-spec etc are in lexinput.lsp

(defun display-active-psort (psort ostream)
   (let ((start-pos (current-position ostream)))
     (with-bold-output ostream
     (format ostream "~A  " psort))
         (push (new-psort-field 
                (subtract-points start-pos (make-point 0 (font-ascent ostream)))
                   psort)
        (fields ostream))))

(defun new-psort-field (view-pos psort)
  (make-psort-click-field :view-pos view-pos :psort psort))

(defun create-active-fs-pop-up-psort (field)
  (let* ((view-pos (psort-click-field-view-pos field))
         (psort (psort-click-field-psort field))
         (menu (make-instance 'ccl::pop-up-field
                 :view-position view-pos
                 :item-display (format nil "~A" psort)
                 :view-font (cons :bold *type-font*))))
    (apply #'add-menu-items menu
      (let ((lex-entry (if psort (get-psort-entry psort))))
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
;     :menu-item-title "Psort definition"
;               :menu-item-action 
;               #'(lambda ()
;                       (display-unexpanded-lex-entry psort lex-entry)))
    (make-instance 'menu-item
     :menu-item-title "Expanded psort"
     :menu-item-action 
     #'(lambda ()
         (display-fs (lex-or-psort-full-fs lex-entry) 
                     (format nil "~(~A~) - expanded" psort)))))) 


(defun pop-up-lex-rule-menu-items (psort rule-entry)
  (declare (ignore psort))
   (list
     (make-instance 'menu-item
     :menu-item-title "Show rule"
     :menu-item-action 
     #'(lambda ()
         (display-fs (rule-full-fs rule-entry) 
                     (format nil "~(~A~)" (rule-id rule-entry)))))))

;;;  ***** TeX macros  ******


(defun output-fs-in-tex (fs-record)
   (let ((fs (fs-display-record-fs fs-record)))
      (when fs
         (let ((file-name 
                  (ask-user-for-new-pathname "File for LaTeX macros?")))                          
            (when file-name
               (with-open-file (stream file-name :direction :output)
                     (display-dag fs 'tex stream)))))))


;;; ***** Other title menu functions *****

(defun store-as-psort (fs-record)
   (let ((psort-name 'no-name)
         (fs (fs-display-record-fs fs-record)))
      (when fs
           (setf psort-name 
               (car
               (ask-for-lisp-movable "Current Interaction" 
                  `(("Lex-id?" . ,psort-name))
                  150)))
            (if psort-name
              (or 
               (store-temporary-psort psort-name fs)
               (cerror "Try Again" "Name already used"))))))

