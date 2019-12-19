;;; Copyright (c) 2017-2018 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; Author: John Carroll
;;; Version: 12-Dec-2018
;;; Distributed as part of the LKB System, under the MIT License
;;; see LICENSE file and http://moin.delph-in.net/LkbCopyright for conditions

#-lispworks
(require
  #+(and :allegro (not :mswindows)) :climxm
  #+(and :allegro :mswindows) :climnt
  #-:allegro :mcclim)

;;; If it's not already present, pick up some version of FAD - old versions should be OK

#-(or :fad :cl-fad)
(eval-when (:compile-toplevel :load-toplevel :execute) (require :cl-fad))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp (intern "PATHNAME-ROOT-P" :fad))
    ;; old version missing some of the latest functions
    (pushnew :fad-old-version *features*)))


(defpackage :select-file
  (:nicknames #:sf)
  (:use :clim-lisp :clim :clim-sys)
  (:export #:select-file #:file-selector #:list-directory #:list-places #:list-devices))

(in-package :select-file)


;;; A file selector for CLIM 2, including some programmable customization features.
;;; Developed and tested in mcclim-20181018 / SBCL 1.3.18 and in
;;; Allegro CLIM 2.1 / Allegro CL 10.0. Not (yet) tested in LispWorks or Liquid CLIM.
;;; Takes inspiration and some implementation ideas from the nice file browsers in the
;;; McCLIM Lisp Listener, Gsharp, and Ernestine.
;;;
;;;   select-file &rest args
;;;               &key (frame-name 'file-selector)
;;;		       title (prompt "Name:")
;;;                    (directory (user-homedir-pathname))
;;;                    (dialog-type :save)
;;;                    show-hidden-p (ok-label "OK")
;;;		  &allow-other-keys
;;;
;;; Other keyword arguments are passed through to make-application-frame, so the programmer
;;; may also call select-file with arguments accepted by that function. These include:
;;;
;;;   left top right bottom
;;;   width height
;;;   icon properties
;;;   user-specified-position-p user-specified-size-p (only in Allegro CLIM)
;;;   text-style foreground background (only relevant for Allegro CLIM, since it passes these two
;;;     arguments down to gadget panes)
;;;
;;; A calling program may specialize the file-selector class and pass the new class name via
;;; the :frame-name argument. The generic functions list-directory, list-places and list-devices
;;; can be specialised on this new file selector class. E.g.
;;;
;;;   (defclass my-file-selector (sf:file-selector) ())
;;;   (defmethod sf:list-places ((frame my-file-selector))
;;;     (append (call-next-method) (list #P"~/common-lisp/mcclim/")))
;;;   (sf:select-file :frame-name 'my-file-selector)
;;;
;;; The following potential issues have been tested: unreadable directories, 'hidden' files
;;; and directories, large directories (e.g. /usr/bin/), filenames containing non-printing
;;; characters, reentrancy (more than one concurrent dialog), 'places' that are files
;;; not directories. It would have been good to have a drop-down menu containing the path to
;;; the directory being displayed, but unfortunately the obvious choice of using an option-list
;;; pane doesn't work since the items in an option-list cannot be dynamically updated.
;;;
;;; Distinctive features:
;;;
;;; * When dialog-type is :save the user may type in a file name (for use as a "save file"
;;; dialog), when it is :open the user may only select existing files, and when it is
;;; :directory the user may only select existing directories.
;;; * If the user resizes the dialog, the files/directories pane updates its layout to make
;;; best use of the horizontal space.
;;; * The programmer can change the pre-supplied lists of files/directories, places and
;;; devices in the left-hand pane.
;;;
;;; Bugs:
;;;
;;; * In Allegro CLIM, clicking in the left or right browsing pane and then typing causes the
;;; characters to be written into the frame not the filename text field.
;;;
;;; Examples:
;;;
;;; (sf:select-file)
;;; (sf:select-file :title "Open" :prompt "File:" :dialog-type :open :ok-label "Open")
;;; (sf:select-file :dialog-type :save :show-hidden-p t)
;;; (sf:select-file :dialog-type :directory)
;;; (sf:select-file :directory "/usr/bin/" :left 300 :top 200 :width 400 :height 600)

(defun select-file (&rest args
		    &key (frame-name 'file-selector)
			 (title nil)
			 (prompt "Name:") ; "Save As:" is appropriate for dialog-type :save
			 (directory (user-homedir-pathname)) 
			 (dialog-type :save)
			 (show-hidden-p nil)
			 (ok-label "OK") ; "Open", "Load" or "Save" may also be appropriate
		    &allow-other-keys)
  (check-type frame-name symbol)
  (check-type title (or string null))
  (check-type prompt (or string null))
  (check-type directory (or string pathname))
  (check-type dialog-type (member :open :save :directory))
  (check-type ok-label (or string null))
  (unless title
    (setq title
      (ecase dialog-type
        (:open "Select File")
        (:save "Specify File Name")
        (:directory "Select Directory"))))
  (unless prompt (setq prompt "Name:"))
  (setq directory (ensure-valid-directory directory))
  (unless ok-label (setq ok-label "OK"))
  (setq args
    (loop for (k v) on args by #'cddr
      unless (or (eq k :frame-name) (eq k :title))
      nconc (list k v)))
  (let ((frame
          (apply #'make-application-frame frame-name
                                          :pretty-name title
                                          :prompt prompt
                                          :directory directory
                                          :dialog-type dialog-type
                                          :show-hidden-p show-hidden-p
                                          :ok-label ok-label
					  args)))
    (setf (file-selector-files-dirs frame)
      (list-directory frame directory show-hidden-p))
    (run-frame-top-level frame)
    (file-selector-result frame)))

(defun ensure-valid-directory (x)
  (if x
    (cl-fad:pathname-as-directory (pathname x))
    (user-homedir-pathname)))


(defparameter +outline-gray+ #+:mcclim climi::*3d-dark-color* #-:mcclim (make-gray-color 0.59))

(defparameter +text-gray+ (make-gray-color 0.66))

(defparameter *cancel-button-string* "  Cancel  ")

(defclass files-dirs-application-pane (application-pane)
  ;; inheriting instead from clim-stream-pane might be more appropriate, but in that case in
  ;; McCLIM we don't get scroll wheel support (we don't for either in Allegro CLIM)
  ())

(define-application-frame file-selector ()
  ((prompt :initform nil
           :initarg :prompt
           :reader file-selector-prompt)
   (directory :initform ""
              :initarg :directory
              :reader file-selector-directory)
   (dialog-type :initform nil
                :initarg :dialog-type
                :reader file-selector-dialog-type)
   (show-hidden-p :initform nil
                  :initarg :show-hidden-p
                  :reader file-selector-show-hidden-p)
   (ok-label :initform nil
	     :initarg :ok-label
             :reader file-selector-ok-label)
   (files-dirs :initform nil
               :accessor file-selector-files-dirs)
   (last-margin :initform nil
                :accessor file-selector-last-margin)
   (last-ncolumns :initform 0
                  :accessor file-selector-last-ncolumns)
   (result :initform nil
           :accessor file-selector-result))                  
  (:menu-bar nil)
  (:panes 
    (places-devices-pane
      (make-pane 'application-pane
	:foreground +black+
	:background +white+
	:text-cursor nil
 	;; *** :borders nil
 	:max-width 150 ; in Allegro CLIM, this is completely overridden by the hbox-pane spec
 	:display-time nil
	:display-function #'display-places-devices))
    (files-dirs-pane
      (make-pane 'files-dirs-application-pane
	:foreground +black+
	:background +white+
	:text-cursor nil
	;; *** :borders nil
 	:display-time nil
	:display-function #'display-files-dirs))
    (prompt-pane
      (make-pane 'label-pane
	:label (file-selector-prompt *application-frame*)
	#+:mcclim :max-width #+:mcclim '(:relative 0))) ; prevent the label stretching
    (selection-pane
      (make-pane 'text-field
	:foreground +black+
	:background +white+
	:editable-p
	(if (member (file-selector-dialog-type *application-frame*) '(:open :directory)) nil t)
        :value (namestring (file-selector-directory *application-frame*))
	:value-changed-callback
	#'(lambda (gadget new-value)
	    (declare (ignore gadget))
	    (with-application-frame (frame) (update-ok-button frame new-value)))
	;; *** :borders nil
	:max-width +fill+))
    (ok-button
      (make-pane 'push-button
	:label
	(concatenate 'string "   " (file-selector-ok-label *application-frame*) "   ")
	:align-x :center
        :y-spacing 5
        #-:mcclim :show-as-default #+:mcclim :show-as-default-p t ; incorrect keyword in McCLIM
	:activate-callback #'ok-callback))
    (cancel-button
      (make-pane 'push-button
	:label *cancel-button-string*
	:align-x :center
        :y-spacing 5
        :activate-callback #'close-callback)))
  (:geometry :left 100 :top 100 :width 600 :height 400) ; default placement and size
  (:layouts
    (default
      (spacing (:thickness 15)
        (vertically (:y-spacing 15)
          (horizontally (:x-spacing 10 :equalize-height t)
            (1/4
	      (clim:outlining (:thickness 1 #+:mcclim :background #+:mcclim +outline-gray+)
                ;; in Allegro CLIM, outlining is grey by default - and indeed if we specify
                ;; the colour then the scroll bar also picks it up, which we don't want
	        (scrolling (:scroll-bar :vertical :scroll-bars :vertical) ; CLIM spec ambiguous
                  places-devices-pane)))
            (3/4
	      (clim:outlining (:thickness 1 #+:mcclim :background #+:mcclim +outline-gray+)
	        (scrolling (:scroll-bar :vertical :scroll-bars :vertical)
                  files-dirs-pane))))
          (horizontally (:x-spacing 10 :align-y :center :equalize-height nil)
            prompt-pane
            ;; in McCLIM only, wrap whitespace and outline around text-field gadget, otherwise
            ;; it looks too tight and flat
            #-:mcclim selection-pane
            #+:mcclim
            (outlining (:thickness 1 :background +outline-gray+)
              (outlining (:thickness 3 :background +white+) selection-pane)))
          ;; For two equal width push-buttons on the left side of the frame, an approach
          ;; that works in Allegro CLIM is an hbox-pane split 50-50 inside another hbox
          ;; with a right fill. Unfortunately, this doesn't work in McCLIM since the button
          ;; with the narrower label fails to expand to its allotted half. Instead, we use
          ;; a grid-pane (which is not implemented in Allegro CLIM), adding spacing manually
          ;; since the McCLIM grid-pane ignores :x-spacing. However there's still a bug
          ;; (probably in grid-pane allocate-space): if the left button label is wider than
          ;; the right, the right button won't move over to the right or grow to match it.
          (horizontally ()
            #+:mcclim
            (make-pane 'grid-pane
              :contents
              (list (list (horizontally () ok-button 5) (horizontally () 5 cancel-button))))
            #-:mcclim
            (horizontally (:x-spacing 10) (1/2 ok-button) (1/2 cancel-button))
            :fill)
          )))))

#+:mcclim
(defmethod clim-extensions:find-frame-type ((frame file-selector))
  ;; make file selector have more dialog-like window controls (e.g. no maximize button)
  :dialog)

#+:mcclim
(defmethod frame-standard-output ((frame file-selector))
  ;; direct keyboard input to the text-field gadget without user having to click on it
  ;; - only for McCLIM since nothing analogous seems to work in Allegro CLIM
  (let ((selection-pane (find-pane-named frame 'selection-pane)))
    (or
      (ignore-errors
        (climi::substrate selection-pane)) ; !!! wish we didn't need an undocumented function
      (call-next-method))))

(defmethod run-frame-top-level :before ((frame file-selector) &key &allow-other-keys)
  ;; !!! The order in which threads run in McCLIM/CLX/X11 may prevent a new frame from opening
  ;; at the top of the stack, requiring the user to click on it to activate it. I can't pin
  ;; down the circumstances in which this happens, but calling sleep here seems to be an
  ;; effective workaround.
  #+:mcclim (sleep 0.05)
  (update-ok-button
    frame (gadget-value (find-pane-named frame 'selection-pane))))

(defun update-ok-button (frame new-value)
  (let ((ok-button
          (find-pane-named frame 'ok-button))
        (require-directory-p
    	  (eq (file-selector-dialog-type frame) :directory)))
    (when ok-button ; the gadget might not be associated with this frame yet
      (if (eq (and (cl-fad:directory-pathname-p new-value) t) require-directory-p)
        (activate-gadget ok-button)
        (deactivate-gadget ok-button)))))

(defun ok-callback (button)
  (declare (ignore button))
  (with-application-frame (frame)
    (setf (file-selector-result frame)
      (pathname (gadget-value (find-pane-named frame 'selection-pane))))
    (frame-exit frame)))

(defun close-callback (button)
  (declare (ignore button))
  (with-application-frame (frame) (frame-exit frame)))


;;; Detect resizing of the dialog, by an :after method on allocate-space. A more direct
;;; solution might be be a handle-event method on window-configuration-event; however
;;; handle-event is called on the frame's top level sheet and it doesn't seem possible to
;;; specialise that sheet portably.

(define-file-selector-command com-resize-panes ((frame 'file-selector))
  (display-files-dirs
    frame (find-pane-named frame 'files-dirs-pane) t))

(defmethod allocate-space :after ((pane files-dirs-application-pane) width height)
  (declare (ignore width height))
  ;; in McCLIM, a horizontal resize of an application / scroll pane combo updates the
  ;; application pane's text-margin if the scroll pane has a horizontal scroll bar,
  ;; but not otherwise - I think it should always
  #+:mcclim
  (setf (stream-text-margin pane)
    (bounding-rectangle-width (pane-viewport-region pane)))
  (let ((margin (stream-text-margin pane)))
    (with-application-frame (frame)
      (with-slots (last-margin) frame
        (cond
          ((null last-margin) ; a brand new frame, not a resize?
            (setf last-margin margin))
          ((> (abs (- margin last-margin)) 10) ; significant change since last redisplay attempt?
            (setf last-margin margin)
            (execute-frame-command frame `(com-resize-panes ,frame))))))))


;;; Files, folders and devices icon patterns

(defparameter +folder-icon+
  (make-pattern
#2A((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
    (0 1 2 2 2 2 1 0 0 0 0 0 0 0 0 0)
    (1 2 2 2 2 2 2 1 1 1 1 1 1 1 1 0)
    (1 3 3 3 3 3 3 3 3 3 3 3 3 3 1 4)
    (1 3 3 3 3 3 3 3 3 3 3 3 3 3 1 4)
    (1 5 5 5 5 5 5 5 5 5 5 5 5 5 1 4)
    (1 5 5 5 5 5 5 5 5 5 5 5 5 5 1 4)
    (6 5 5 5 5 5 5 5 5 5 5 5 5 5 6 4)
    (6 7 7 7 7 7 7 7 7 7 7 7 7 7 6 4)
    (6 7 7 7 7 7 7 7 7 7 7 7 7 7 6 4)
    (8 7 7 7 7 7 7 7 7 7 7 7 7 7 8 4)
    (8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 4)
    (0 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4)
    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
    (list
      #+:mcclim +transparent-ink+ #-:mcclim +white+ ; Allegro CLIM pattern limitation
      (make-rgb-color 160/255 153/255 123/255)
      (make-rgb-color 239/255 229/255 186/255)
      (make-rgb-color 239/255 227/255 174/255)
      (make-rgb-color 173/255 173/255 173/255)
      (make-rgb-color 237/255 224/255 158/255)
      (make-rgb-color 145/255 138/255 103/255)
      (make-rgb-color 234/255 223/255 147/255)
      (make-rgb-color 119/255 113/255 85/255))))

(defparameter +document-icon+
  (make-pattern
#2A((0 0 1 1 1 1 1 1 1 1 1 0 0 0 0 0)
    (0 0 1 2 2 2 2 2 2 2 1 1 0 0 0 0)
    (0 0 1 2 2 2 2 2 2 2 1 3 1 0 0 0)
    (0 0 1 2 2 2 2 2 2 2 1 3 3 1 0 0)
    (0 0 1 2 2 2 2 2 2 2 1 1 1 1 4 0)
    (0 0 1 2 2 2 2 2 2 2 2 4 4 5 4 0)
    (0 0 1 2 2 2 2 2 2 2 2 2 2 1 4 0)
    (0 0 1 2 2 2 2 2 2 2 2 2 2 1 4 0)
    (0 0 1 2 2 2 2 2 2 2 2 2 2 1 4 0)
    (0 0 1 2 2 2 2 2 2 2 2 2 2 1 4 0)
    (0 0 1 2 2 2 2 2 2 2 2 2 2 1 4 0)
    (0 0 1 2 2 2 2 2 2 2 2 2 2 1 4 0)
    (0 0 1 2 2 2 2 2 2 2 2 2 2 1 4 0)
    (0 0 1 2 2 2 2 2 2 2 2 2 2 1 4 0)
    (0 0 1 1 1 1 1 1 1 1 1 1 1 1 4 0)
    (0 0 0 4 4 4 4 4 4 4 4 4 4 4 4 0))
    (list
      #+:mcclim +transparent-ink+ #-:mcclim +white+ ; Allegro CLIM pattern limitation
      (make-rgb-color 112/255 112/255 112/255)
      (make-rgb-color 232/255 232/255 232/255)
      (make-rgb-color 255/255 255/255 255/255)
      (make-rgb-color 137/255 137/255 137/255)
      (make-rgb-color 99/255 99/255 99/255))))

(defparameter +up-folder-icon+
  (make-pattern
#2A((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
    (0 1 2 2 2 2 1 0 0 0 0 0 0 0 0 0)
    (1 2 2 2 2 2 2 1 1 1 1 1 1 1 1 0)
    (1 3 3 3 4 3 3 3 3 3 3 3 3 3 1 5)
    (1 3 3 4 4 4 3 3 3 3 3 3 3 3 1 5)
    (1 6 4 6 4 6 4 6 6 6 6 6 6 6 1 5)
    (1 6 6 6 4 6 6 6 6 6 6 6 6 6 1 5)
    (7 6 6 6 4 6 6 6 6 6 6 6 6 6 7 5)
    (7 8 8 8 8 4 8 8 8 8 8 8 8 8 7 5)
    (7 8 8 8 8 8 4 4 4 4 4 8 8 8 7 5)
    (9 8 8 8 8 8 8 8 8 8 8 8 8 8 9 5)
    (9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 5)
    (0 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5)
    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
    (list
      #+:mcclim +transparent-ink+ #-:mcclim +white+ ; Allegro CLIM pattern limitation
      (make-rgb-color 109/255 158/255 176/255)
      (make-rgb-color 189/255 232/255 252/255)
      (make-rgb-color 176/255 229/255 253/255)
      (make-rgb-color 0/255 0/255 0/255)
      (make-rgb-color 188/255 188/255 188/255)
      (make-rgb-color 154/255 219/255 253/255)
      (make-rgb-color 81/255 133/255 152/255)
      (make-rgb-color 140/255 211/255 251/255)
      (make-rgb-color 58/255 96/255 109/255))))

(defparameter +device-icon+
  (make-pattern
#2A((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 1 1 1 1 1 1 1 1 1 0 0 0 0)
    (0 0 1 4 4 4 4 4 4 4 4 4 1 6 0 0)
    (0 1 3 3 3 3 3 3 3 3 3 3 3 1 6 0)
    (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 6)
    (1 2 2 2 2 2 2 2 2 2 2 2 2 2 1 6)
    (1 2 2 2 2 2 2 2 2 2 2 5 5 2 1 6)
    (1 2 2 2 2 2 2 2 2 2 2 5 5 2 1 6)
    (1 2 2 2 2 2 2 2 2 2 2 2 2 2 1 6)
    (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 6)
    (0 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6)
    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
    (list
      #+:mcclim +transparent-ink+ #-:mcclim +white+ ; Allegro CLIM pattern limitation
      (make-rgb-color 112/255 112/255 112/255)
      (make-rgb-color 190/255 190/255 190/255)
      (make-rgb-color 220/255 220/255 220/255)
      (make-rgb-color 240/255 240/255 240/255)
      (make-rgb-color 48/255 240/255 48/255)
      (make-rgb-color 173/255 173/255 173/255))))


;;; Files and directories list pane

(define-presentation-type file-dir-namestring ())

(define-file-selector-command com-select-file-dir
    ((data 'file-dir-namestring :gesture :select))
  (select-file-dir data))

(defun select-file-dir (data &optional relist-if-file-p)
  (let* ((p (cadr data))
         (parent *application-frame*)
         (selection-pane
  	   (find-pane-named parent 'selection-pane)))
    (setf (gadget-value selection-pane) (namestring p))
    (update-ok-button parent p)
    (when (and relist-if-file-p (not (cl-fad:directory-pathname-p p)))
      (setq p (cl-fad:pathname-as-directory p)))
    (when (cl-fad:directory-pathname-p p)
      (with-slots (files-dirs show-hidden-p) parent
        (setf files-dirs
          (list-directory parent p show-hidden-p)))
      (display-files-dirs
        parent (find-pane-named parent 'files-dirs-pane)))))


(defun display-files-dirs (frame stream &optional lazyp)
  (let*
    ((files-dirs (file-selector-files-dirs frame))
     (items
      (nconc
        (if (car files-dirs)
          (list (list* "Parent directory" (car files-dirs) 'parent))
          nil)
        (mapcar
          #'(lambda (p)
             (list* (file-dir-namestring p) p
               (if (cl-fad:directory-pathname-p p) 'folder 'document)))
          (cdr files-dirs))))
     (max-width
       (+ 12 ; x-spacing - see display-fs-items / draw-namestring for these spacing values
         (max
           (+ 16 4
             (reduce #'max items
               :key #'(lambda (x) (text-size stream (if (atom x) x (car x)))))
             2)
           120))) ; min cell width
     (margin
       ;; we don't need all of the last column to be visible so extend right margin a bit
       (+ (stream-text-margin stream) (floor max-width 4) 3 12))
     (ncolumns
       (min (max (floor margin max-width) 1) (length items))))
    ;; the lazyp flag indicates that if there are the same number of columns as were displayed
    ;; last time around then no redisplay is necessary
    (when (or (not lazyp) (not (eql ncolumns (file-selector-last-ncolumns frame))))
      (setf (file-selector-last-ncolumns frame) ncolumns)
      (display-fs-items items stream 'file-dir-namestring ncolumns max-width))))


;;; Each item is of the form (namestring pathname . type) where type is one of the symbols
;;; {device, parent, folder, document}. This function doesn't use the formatting-table
;;; :multiple-columns facility because it's not guaranteed to use all the columns - but we
;;; want that since it might bring into view items near the bottom of the table that otherwise
;;; would have to be scrolled down to.

(defun display-fs-items (items stream presentation-type ncolumns &optional (col-width 0))
  (let*
    ((x-margin 3)
     (y-margin (stream-vertical-spacing stream))
     (row-height
       (+ (max 16 (stream-line-height stream)) (stream-vertical-spacing stream)))
     (record
       (with-output-recording-options (stream :draw nil :record t)
         (with-new-output-record (stream)
           (window-clear stream)
           (loop
             with last-row-index = (1- (ceiling (length items) ncolumns))
             for item in items
             for ri = 0 then (if (= ri last-row-index) 0 (1+ ri))
             for ci = 0 then (if (zerop ri) (1+ ci) ci) ; display in column-wise order
             do
             (stream-set-cursor-position
               stream (+ (* ci col-width) x-margin) (+ (* ri row-height) y-margin))
             (if (atom item)
      	       (draw-heading stream item)
      	       (with-output-as-presentation (stream item presentation-type :single-box t)
	         (let ((icon
                         (ecase (cddr item)
                           (device +device-icon+)
                           (parent +up-folder-icon+)
                           (folder +folder-icon+)
                           (document +document-icon+))))
	           (draw-namestring stream icon (car item))))))
           (scroll-extent stream 0 0)))))
    (replay record stream)
    ;; in McCLIM only, need to indicate that the content of this pane may have a new height
    ;; so the scroller should update - and a new width to prevent the pane being scrollable
    ;; horizontally (e.g. with touchpad) if it has got narrower
    #+:mcclim
    (multiple-value-bind (w h)
        (bounding-rectangle-size (stream-output-history stream))
      (change-space-requirements stream :width w :height (+ h 4)))))

(defun draw-namestring (stream pattern text)
  (flet
    ((backup/lock-namestring (s)
       ;; starts with ~$ or ends with ~ / # / .bak
       (let ((len (length s)))
         (and (> len 1)
           (or (string= s "~$" :end1 2)
               (member (char s (1- len)) '(#\~ #\#))
               (and (> len 4) (string-equal s ".bak" :start1 (- len 4))))))))
    ;; !!! in McCLIM only, draw invisible points immediately to the left and right, otherwise
    ;; presentation highlighting does not give enough room
    (multiple-value-bind (x y)
        (stream-cursor-position stream)
      #+:mcclim
      (progn
        (draw-point* stream x (1- y) :line-thickness 1 :ink +white+)
        (incf x))
      (draw-pattern* stream pattern x y)
      ;; (draw-rectangle* stream x y (+ x 16) (+ y 16) :ink +gray+) ; !!! A LOT faster...why?
      (incf x (+ (pattern-width pattern) 4))
      (draw-text* stream text x (+ y 7)
        :align-y :center
        :ink (if (backup/lock-namestring text) +text-gray+ +black+))
      #+:mcclim
      (progn
        (incf x (1+ (text-size stream text)))
        (draw-point* stream x (1- y) :line-thickness 1 :ink +white+)))))

(defun draw-heading (stream text)
  (multiple-value-bind (x y)
      (stream-cursor-position stream)
    (draw-text* stream text x y
      :align-y :top :ink +text-gray+
      :text-style
      (merge-text-styles (make-text-style nil :bold :smaller) (medium-text-style stream)))))


;;; Create a list of pathnames in current directory, headed by the pathname of the
;;; parent of this directory (or nil if we're at the root of this file system).

(defgeneric list-directory (frame dir &optional show-hidden-p)
  (:documentation "Returns a list of pathnames, the first being the parent directory of dir (or NIL if dir is the root of a file system) and the rest being the contents of dir. The show-hidden-p argument is passed through from the top-level call, and may be interpreted to filter out file names starting with a period."))

(defmethod list-directory ((frame file-selector) dir &optional show-hidden-p)
  ;; we need list-directory to follow symlinks otherwise it's not possible to follow a
  ;; symbolic link to a directory (e.g. tmp -> private/tmp, since in this case tmp
  ;; looks like a normal file)
  (flet
    ((sorted-filtered-ls (d)
        ;; macOS always encodes file names as Unicode NFD no matter what the locale setting
        ;; is, so avoid a possible error here (in sbcl it's sb-int:c-string-decoding-error)
        ;; by ignoring a possibly misleading setting
        (let* (#+(and :sbcl :darwin) (sb-alien::*default-c-string-external-format* :utf-8)
               (items (cl-fad:list-directory d)))
          (sort
            (if show-hidden-p
              items
              (remove-if #'(lambda (p) (eql (char (file-dir-namestring p) 0) #\.)) items))
            #'string<
            :key #'file-dir-namestring))))
    (cons
      (if (pathname-root-p dir)
         nil
         (pathname-parent-directory dir))
      (handler-case
        (sorted-filtered-ls dir)
        (error (e)
          (warn "Unable to list directory ~A: ~A" dir e)
          nil)))))

(defun file-dir-namestring (x &optional homedir-tilde-p)
  (cond
    ((not (cl-fad:directory-pathname-p x))
      (file-namestring x))
    ((pathname-root-p x) ; NB could encounter root here via a symbolic link
      (directory-namestring x))
    ((and homedir-tilde-p (equal x (user-homedir-pathname)))
      (let ((dir (car (last (pathname-directory x)))))
        (concatenate 'string #+:unix "~" (if (and dir (stringp dir)) dir "home"))))
    (t
      (car (last (pathname-directory x))))))

(defun pathname-root-p (p)
  ;; this function is missing from old versions of cl-fad
  #-:fad-old-version
  (cl-fad:pathname-root-p p)
  #+:fad-old-version
  (equal (pathname-directory p) '(:absolute)))

(defun pathname-parent-directory (p)
  ;; this function is missing from old versions of cl-fad
  ;; p known not to be root
  #-:fad-old-version
  (cl-fad:pathname-parent-directory p)
  #+:fad-old-version
  (make-pathname :directory (butlast (pathname-directory p)) :defaults p))


;;; 'Places' and 'devices' pane containing user home directory, any useful files or directories
;;; (achieved by specialising list-places), and roots of file systems on mounted devices

(define-presentation-type place-device-namestring ())

(define-file-selector-command com-select-place-device-namestring
    ((data 'place-device-namestring :gesture :select))
  (display-places-devices
    *application-frame*
    (find-pane-named *application-frame* 'places-devices-pane))
  (select-file-dir data t))

(defun display-places-devices (frame stream)
  (display-fs-items
    (nconc
      (list "PLACES")
      (mapcar
        #'(lambda (p)
           (list* (file-dir-namestring p t) p 
             (if (cl-fad:directory-pathname-p p) 'folder 'document)))
        (list-places frame))
      (list " " "DEVICES")
      (mapcar
        #'(lambda (p) (list* (place-device-namestring p) p 'device))
        (list-devices frame)))
    stream 'place-device-namestring 1))

(defun place-device-namestring (x)
  ;; return name containing pathname device and last component of directory (if any) - to
  ;; look like a "device" as might be shown to the user on the desktop
  (let ((dev (pathname-device x))
        (dir (car (last (pathname-directory x)))))
    (when dir
      (unless (stringp dir) (setq dir nil)))
    (cond
      ((and dev
         (or (stringp dev) (symbolp dev) (characterp dev))
         (not (member dev '(:unspecific :unc))))
        (concatenate 'string (string dev) ":" dir))
      (dir)
      (t (directory-namestring x)))))


;;; Create a list of pathnames representing common places (= directories) in which the user
;;; might want to select files.

(defgeneric list-places (frame)
  (:documentation "Returns a list of pathnames, each of which is a regularly-used directory in which the user might want to select files."))

(defmethod list-places ((frame file-selector))
  (list (user-homedir-pathname)))


;;; Return a sorted list of pathnames representing roots of all mounted file systems. In
;;; Allegro CL on Windows there is a built-in function that does most of the work. On macOS,
;;; file systems are available under /Volumes/

(defgeneric list-devices (frame)
  (:documentation "Returns a list of pathnames, each of which is the root of a currently mounted file system - either local or via a network."))

(defmethod list-devices ((frame file-selector))
  (sort
    #+(and :mswindows :allegro)
    (mapcar
      #'(lambda (dev) (make-pathname :device (string dev) :directory '(:absolute)))
      (windows:file-systems)) ; Allegro CL only, defined in the :winapi module
    #+(and :mswindows (not :allegro))
    (list (make-pathname :device "C" :directory '(:absolute)))
    #+:darwin ; = macOS
    (cl-fad:list-directory "/Volumes/")
    #+:linux
    (cons (make-pathname :directory '(:absolute)) (cl-fad:list-directory "/mnt/"))
    #-(or :mswindows :darwin :linux)
    (list (make-pathname :directory '(:absolute)))
    #'(lambda (p1 p2)
        (let ((p1-dev (pathname-device p1)) (p1-dir (car (last (pathname-directory p1))))
              (p2-dev (pathname-device p2)) (p2-dir (car (last (pathname-directory p2)))))
          (cond
            ((and p1-dev (null p2-dev)) t)
            ((and p2-dev (null p1-dev)) nil)
            ((or p1-dev p2-dev) (string< (string p1-dev) (string p2-dev)))
            ((and p1-dir (symbolp p1-dir)) t)
            ((and p2-dir (symbolp p2-dir)) nil)
            (t (string< (string p1-dir) (string p2-dir))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :select-file-lkb *features*))

