;;; Copyright (c) 1991-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions


;;; Graphical fs drawing 
;;; CL dialect specific
;;; This version for Allegro CL - CLIM2

(in-package :lkb)

(eval-when
    (compile load eval)
  (proclaim '(special clim-user::*lkb-top-frame* clim-user::*lkb-top-stream*)))

(defmacro with-output-to-top (() &body body)
  (let ((func (gensym)))
    `(let ((,func #'(lambda () ,@body)))
       (declare (dynamic-extent ,func))
       (if clim-user::*lkb-top-stream*
	   (clim-user::invoke-with-output-to-top ,func)
	 (funcall ,func)))))

 ;;; SIMPLE DRAWING

;;; All these position functions only affect the text-cursor in CLIM
;;; CLIM2 manual 1994 ed 19.3

(defun move-to-x-y (stream x y)
  (clim:stream-set-cursor-position stream x y))

(defun current-position-y (stream)
  (multiple-value-bind (x y)
     (clim:stream-cursor-position stream)
     (declare (ignore x))
     y))


(defun current-position-x (stream)
  (multiple-value-bind (x y)
     (clim:stream-cursor-position stream)
     (declare (ignore y))
     x))
  

(defun current-position (stream)
  (multiple-value-bind (x y)
     (clim:stream-cursor-position stream)
     (clim:make-point x y)))

(defun position-y (point)
  (clim:point-y point))

(defun position-x (point)
  (clim:point-x point))

;;; added for RMRS output

(defun make-position-record (x y)
  (clim:make-point x y))
 

;;; RECTANGLE DRAWING

#|
;;; not used ?
(defun make-box-relative-from-corner (start-position w h)
  (cons start-position (make-point (+ (point-x start-position) w)
                                   (+ (point-y start-position) h))))
|#

(defun frame-text-box (stream start-pos end-pos)
  ;;; given a start position and an end position from
  ;;; some text output, draws a suitable frame round it
  (let* ((start-x (position-x start-pos)) 
         (start-y (position-y start-pos))
         (x1 (- start-x 1)) ; upper-left point's x
         (y1 (- start-y 1)) ; upper-left point's y
         (x2 (+ (position-x end-pos) 1)) ; lower-right point's x
         (y2 (+ start-y 1 (text-font-height stream))))
                                        ; lower-right point's y
    (clim:draw-rectangle* stream x1 y1 x2 y2 :filled nil
                          :line-dashes t :line-thickness 2)))


                              
;;; actually draw-rectangle* is more efficient, but leave for now  
(defun draw-box (stream box)
  (clim:draw-rectangle stream (car box) (cdr box) :filled nil))

#|
;;; not used ?
(defun inside-box-p (position box)
  (let ((xpos (point-x position)) (ypos (point-y position)))
    (and (>= xpos (point-x (car box)))
         (<= xpos (point-x (cdr box)))
         (>= ypos (point-y (car box)))
         (<= ypos (point-y (cdr box))))))
|#

;;; FONT HANDLING

(defun actual-text-style (stream)
  ;;; check that this stream supports text styles
  ;;; otherwise just return NIL
  (if (clim:extended-output-stream-p stream)
      (if (clim:medium-text-style stream)
          (clim:merge-text-styles (clim:medium-text-style stream)
                             (clim:medium-default-text-style stream))
          (clim:medium-default-text-style stream))))
  


#|
;;; not used ?
(defun font (stream)
  (actual-text-style stream))

;;; can't do this one, because in CLIM all text styles
;;; are relative to a medium
(defun font-height (font-spec)
 (error "Untranslatable function"))
|#

(defun text-font-height (stream)
   (clim:text-style-height (actual-text-style stream) stream)) 

(defun font-ascent (stream)
  (clim:text-style-ascent (actual-text-style stream) stream))

(defun font-descent (stream)
  (clim:text-style-descent (actual-text-style stream) stream))

#|
;;; luckily it's not used - can't do it in CLIM apparently
(defun font-leading (stream)
 (error "Untranslatable function"))
|#
  

(defun stream-string-width (stream string)
  (clim:stream-string-width stream string))

;;; The functions called from format statements won't work, because
;;; it appears that Allegro CL hasn't implemented user-defined
;;; format-directives.  
;;; We also have to be careful,
;;; because we apprently can't change text-style
;;; on some windows.  So the stuff that uses
;;; FB etc will all have to be rewritten

(defmacro with-bold-output (ostream body)
  `(clim:with-text-style (,ostream '(nil :bold nil)) ,body))


(defun lkb-y-or-n-p (strg)
  ;;; define so it uses y-or-n-p-dialog 
  (y-or-n-p-general strg))


;;; ========================================================================
;;; Macros for pop-up menus

(defmacro pop-up-menu (menu &body cases)
  (let ((command (gensym)))
    `(let ((,command (clim:menu-choose ,menu)))
       (when ,command
	 (handler-case
	     (ecase ,command
	       ,@cases)
           (storage-condition (condition)
             (with-output-to-top ()
               (format t "~%Memory allocation problem: ~A~%" condition)))
           (error (condition)
             (with-output-to-top ()
               (format t "~%Error: ~A~%" condition)))
           (serious-condition (condition)
             (with-output-to-top ()
               (format t "~%Something nasty: ~A~%" condition))))))))

;;; ========================================================================
;;; Define general frame class for LKB frames

(clim:define-application-frame lkb-frame ()
  ((class-frames :initform nil
		 :accessor class-frames
		 :allocation :class)
   (selected :initform nil
	     :accessor frame-selected)
   (doc-pane :initform nil
	     :accessor lkb-window-doc-pane)))


(defparameter *lkb-frame-lock* (mp:make-process-lock))

;; Register frames of each class when they are created

(defmethod clim:run-frame-top-level :before ((frame lkb-frame) &key)
  (mp:with-process-lock (*lkb-frame-lock*)
    (push frame (getf (class-frames frame) (class-of frame)))))

;; Find and raise the most recently created frame of a given class

(defun reuse-frame (class)
  (let ((frame (clim:find-application-frame class :create nil :activate nil)))
    (when frame
      (mp:with-process-lock (*lkb-frame-lock*)
	(let ((latest (car (getf (class-frames frame) (find-class class)))))
	  (when latest
	    (clim:enable-frame latest)
	    (clim:raise-frame latest)
	    latest))))))

;; Disable right button

(clim:delete-gesture-name :menu)

;; Add a [Close] button

(define-lkb-frame-command (com-close-frame :menu "Close") 
    ()
  (clim:with-application-frame (frame)
    (unhighlight-objects frame)
    (mp:with-process-lock (*lkb-frame-lock*)
      (setf (getf (class-frames frame) (class-of frame))
	(delete frame (getf (class-frames frame) (class-of frame)) 
		:test #'eq)))
    (clim:frame-exit frame)))

;; Add a [Close All] button

(define-lkb-frame-command (com-close-all-frame :menu "Close All") 
    ()
  (mp:with-process-lock (*lkb-frame-lock*)
    (clim:with-application-frame (frame)
      (dolist (f (getf (class-frames frame) (class-of frame)))
	;; Make sure we close ourself last
	(unless (eq f frame)
	  (clim:execute-frame-command f '(com-close-frame))))
      (clim:execute-frame-command frame '(com-close-frame)))))
  
;; Add a [Print] button

(define-lkb-frame-command (com-print-frame :menu "Print") 
    ()
  (clim:with-application-frame (frame)
    (with-output-to-top ()
      (multiple-value-bind (dest orient scale filename)
	  (get-print-options)
	(case dest
	  (:printer (format t "~%Printing to printer not implemented yet."))
	  (:file	
	   (when (or (not (probe-file filename))
		     (clim:notify-user clim-user::*lkb-top-frame*
				       (format nil 
					       "File ~a exists.~%Overwrite?" 
					       filename)
				       :style :question))
	     (handler-case
		 (with-open-file (ps-stream filename 
				  :direction :output 
				  :if-exists :supersede)
		   (clim:with-output-to-postscript-stream 
		       (stream ps-stream 
			       :scale-to-fit (not scale) 
			       :multi-page scale
			       :orientation orient)
		     (funcall (clim-internals::pane-display-function 
			       (clim-internals::find-frame-pane-of-type 
				frame 'clim:application-pane))
			      frame stream)))
               (storage-condition (condition)
		 (format t "~%Memory allocation problem: ~A~%" condition))
               (error (condition)
		 (format t "~%Error: ~A~%" condition))
	       (serious-condition (condition)
		 (format t "~%Something nasty: ~A~%" condition))))))))))


(defmacro define-lkb-frame (frame-class slots 
			    &rest pane-options 
			    &key (info-bar nil)
			    &allow-other-keys)
  `(clim:define-application-frame ,frame-class (lkb-frame)
     ,slots
     (:command-table (,frame-class :inherit-from (lkb-frame)
				   :inherit-menu t))
     (:panes
      (display 
       (clim:vertically ()
	 (clim:outlining (:thickness 1)
	   (clim:spacing (:thickness 1)  
	     (clim:scrolling (:scroll-bars :both)
	       (clim:make-pane 'clim:application-pane
			       :name :lkb-pane
			       :text-cursor nil
			       :end-of-line-action :allow
			       :end-of-page-action :allow
			       :borders nil
			       :background clim:+white+
			       :foreground clim:+black+
			       :display-time nil
			       ,@pane-options))))
	 ,@(when info-bar
	     '((clim:spacing (:thickness 1)
		 (clim:make-pane 'clim:application-pane
				 :name :path
				 :text-cursor nil
				 :end-of-line-action :allow
				 :end-of-page-action :allow
				 :borders nil
				 :height 1
				 :record nil
				 :scroll-bars nil)))))))
     (:layouts
      (default display))))


;; Provide a way to describe an object when the pointer is over it

(defmacro define-info-bar (type vars &body body)
  #+:allegro
  `(clim:define-presentation-method clim:highlight-presentation 
       ((dummy ,type) record stream state)
     state
     (multiple-value-bind (xoff yoff)
	 (clim:convert-from-relative-to-absolute-coordinates 
	  stream (clim:output-record-parent record))
       (let* ((,(first vars) (clim:presentation-object record))
	      (,(second vars) (lkb-window-doc-pane (clim:pane-frame stream))))
	 (if (eq state :highlight)
	     ,@body
	   (clim:window-clear ,(second vars))))
       (clim:with-bounding-rectangle* (left top right bottom) record
	 (clim:draw-rectangle* stream
			       (+ left xoff) (+ top yoff)
			       (+ right xoff) (+ bottom yoff)
			       :filled nil
			       :ink clim:+flipping-ink+)))))

;; Highlight a list of objects

(defun highlight-objects (things frame)
  (let ((stream (clim:frame-standard-output frame)))
    (unhighlight-objects frame)
    (setf (frame-selected frame) 
      (list
       (clim:with-new-output-record (stream)
	 (clim:with-output-recording-options (stream :record t)
	   (dolist (thing things)
	     (when thing
	       (multiple-value-bind (x1 y1 x2 y2)
		   (clim:bounding-rectangle* 
		    (clim:output-record-parent thing))
		 (clim:draw-rectangle* stream x1 y1 x2 y2 :
				       :ink clim:+flipping-ink+ 
				       :filled t))))))))))

;; Highlight a list of objects, making the first one red

(defconstant +red-flipping-ink+ 
    (clim:make-flipping-ink clim:+green+ clim:+foreground-ink+))

(defun highlight-objects-mark (things frame)
  (let ((stream (clim:frame-standard-output frame)))
    (unhighlight-objects frame)
    (highlight-objects (cdr things) frame)
    (push 
     (clim:with-new-output-record (stream)
       (clim:with-output-recording-options (stream :record t)
	 (when (car things)
	   (multiple-value-bind (x1 y1 x2 y2)
	       (clim:bounding-rectangle* 
		(clim:output-record-parent (car things)))
	     (clim:draw-rectangle* stream x1 y1 x2 y2 
				   :ink +red-flipping-ink+ 
				   :filled t)))))
     (frame-selected frame))))

;; Clear highlighting from a particular frame

(defun unhighlight-objects (frame)
  (with-slots (selected) frame
    (when selected
      (dolist (record selected)
	(clim:erase-output-record record 
				  (clim:frame-standard-output frame) nil)))
    (setf selected nil)))

;; Clear highlighting all frames of a particular class

(defun unhighlight-class (frame)
  (mapc #'unhighlight-objects (getf (class-frames frame) (class-of frame))))

;; Find a frame of this class with something highlighted

(defun highlighted-class (frame)
  (find-if #'frame-selected (getf (class-frames frame) (class-of frame))))

;;; Search the display list for an object

(defun find-object (stream test)
  (catch 'find-object
    (find-object-1 (slot-value stream 'clim:output-record) stream test)))

(defun find-object-1 (rec stream test)
  (clim:map-over-output-records 
   #'(lambda (rec)
       (when (clim:presentationp rec) 
	 (if (funcall test (clim:presentation-object rec))
	     (throw 'find-object rec)))
       (dolist (q (clim:output-record-children rec)) 
	 (find-object-1 q stream test)))
   rec))

;;; Center the viewport on object

(defun scroll-to (record stream)
  (let* ((vp-width (clim:bounding-rectangle-width 
		    (clim:pane-viewport-region stream)))
         (vp-height (clim:bounding-rectangle-height
		     (clim:pane-viewport-region stream)))
	 (x-pos (clim:point-x (clim:bounding-rectangle-center 
			       (clim:output-record-parent record))))
	 (y-pos (clim:point-y (clim:bounding-rectangle-center 
			       (clim:output-record-parent record))))
	 (x-max (clim:bounding-rectangle-max-x stream))
	 (y-max (clim:bounding-rectangle-max-y stream)))
    (clim:scroll-extent stream
			(max 0 (min (- x-pos (floor vp-width 2))
				    (- x-max vp-width)))
			(max 0 (min (- y-pos (floor vp-height 2))
				    (- y-max vp-height))))))
