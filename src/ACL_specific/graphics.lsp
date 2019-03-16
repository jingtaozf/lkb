;;; Copyright (c) 1991-2018 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see LICENSE for conditions


;;; Graphical fs drawing 
;;; Graphics toolkit-specific code
;;; This version for CLIM2

(in-package :lkb)

(eval-when
    (compile load eval)
  (proclaim '(special clim-user::*lkb-top-frame* clim-user::*lkb-top-stream*)))

(defmacro with-output-to-top (() &body body)
  ;; Called by graphical operations to make sure any diagnostic output goes to the
  ;; Lkb Top window - if it is open. Lkb Top menu commands and pop-up menus get here
  ;; via execute-menu-command.
  ;; NB Functions invoked at the LKB tty prompt and commands invoked from emacs menus
  ;; should not use this macro but instead output diagnostic info to the LKB tty,
  ;; i.e. standard-output, since that is where the user's focus is.
  (let ((func (gensym)))
    `(let ((,func #'(lambda () ,@body)))
       (declare (dynamic-extent ,func))
       (if clim-user::*lkb-top-stream*
	   (clim-user::invoke-with-output-to-top ,func)
	   (funcall ,func)))))

(defmacro with-dialog-positioning ((left top) dialog-width &body body)
  (let ((screen-width (gensym)) (screen-height (gensym)))
    `(multiple-value-bind (,screen-width ,screen-height)
         (display-screen-size)
       (let
         ((,left (- (round ,screen-width 2) (round ,dialog-width 2)))
          (,top (min (- (round ,screen-height 2) 200) 250))) ; assume dialog is up to 400 units high
         ,@body))))


;;; Disable menu on secondary mouse button

(clim:delete-gesture-name :menu)


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
 (error "Unimplemented function"))
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
 (error "Unimplemented function"))
|#
  

(defun stream-string-width (stream string)
  (clim:stream-string-width stream string))


(defun lkb-dialog-font ()
  (clim:make-text-style :sans-serif :roman (or *dialog-font-size* 12)))

(declaim (notinline lkb-dialog-font))


;;; Output in bold / at a different size. Allows for both the case where the pane's text
;;; style follows the CLIM 2.0 font protocol, as well as when it is based on the McCLIM
;;; extended text styles protocol (in which case it is not possible to determine the bold
;;; font variant, nor may it be possible to get exactly the size requested).

(defmacro with-text-style-bold-face ((stream . rest) &body body) 
  #+:mcclim
  (let ((s (gensym)) (bold-style (gensym)) (x (gensym)) (y (gensym)))
    `(let* ((,s ,stream)
            (,bold-style (text-style-bold-face ,(if rest (car rest) `(clim:pane-text-style ,s)))))
       (if (eq ,bold-style (clim:pane-text-style ,s))
         ;; a distinct bold face was not found so produce faux bold
         (multiple-value-bind (,x ,y)
             (clim:stream-cursor-position ,s)
           (progn ,@body)
           (clim:stream-set-cursor-position ,s (+ ,x 0.1) (+ ,y 0.1))
           ,@body)
         (clim:with-text-style (,s ,bold-style) ,@body)))
   #-:mcclim
   `(clim:with-text-face (,stream :bold) ,@body)))

#+:mcclim 
(defun text-style-bold-face (style)
  ;; return the style in :bold - except in the case of an extended text style
  (if (member (clim:text-style-face style) '(:roman :italic (:bold :italic) nil) :test #'equal)
    (clim:merge-text-styles (clim:make-text-style nil :bold nil) style)
    style))
  

(defmacro with-text-style-new-size ((stream size) &body body)
  #+:mcclim
  `(clim:with-text-style
       (,stream (text-style-new-size (clim:pane-text-style ,stream) ,size))
     ,@body)
   #-:mcclim
   `(clim:with-text-size (,stream ,size) ,@body))

#+:mcclim
(defvar *cached-extended-text-style-sizes* nil) 

(defun text-style-new-size (style size)
  #+:mcclim
  (if (member (clim:text-style-family style) '(:fix :serif :sans-serif nil) :test #'eq)
    (clim:merge-text-styles (clim:make-text-style nil nil size) style)
    ;; this is a McCLIM extended text style, which is only available in a limited range
    ;; of sizes
    (progn
      (unless *cached-extended-text-style-sizes*
        (setq *cached-extended-text-style-sizes* ; something like (8 10 12 14 18 24 48 72)
          (sort
            (copy-list
              (clim-extensions:font-face-all-sizes
                (find (clim:text-style-face style)
                  (clim-extensions:font-family-all-faces
                    (find (clim:text-style-family style)
                      (clim-extensions:port-all-font-families (clim:find-port))
                      :key #'clim-extensions:font-family-name
                      :test #'equal))
                  :key #'clim-extensions:font-face-name
                  :test #'equal)))
            #'<)))
      (let*
        ((nsize
           (if (symbolp size) (getf clim-clx::*clx-text-sizes* size 12) size)) ; !!!
         (new-size ; find nearest cached size from those available
           (loop for (a b) on *cached-extended-text-style-sizes*
             unless b return a
             when (<= nsize a) return a
             when (< a nsize b) return (if (> (- nsize a) (- b nsize)) b a))))
        (clim:merge-text-styles (list nil nil new-size) style))))
  #-:mcclim
  (clim:merge-text-styles (clim:make-text-style nil nil size) style))


(defun lkb-y-or-n-p (str)
  ;; graphical version of y-or-n-p, using a dialog box
  (y-or-n-p-general str))


;;; ========================================================================
;;; Macro for pop-up menus
;;;
;;; There are two kinds of call to this macro: (1) the body is a sequence of
;;; clauses with the key of each clause being a command name, or (2) the body is 
;;; a function form or the name of a function taking the name of the selected
;;; command as its single argument. The menu choice is checked since at least
;;; with McCLIM the user could sneakily mouse over to another menu and select an
;;; item from that instead.

(defmacro pop-up-menu (menu &body cases)
  (let ((menu-var (gensym)) (command-var (gensym)))
    `(let*
       ((,menu-var ,menu)
        (,command-var
           (clim:menu-choose
             ;; not enough horizontal padding around menu items in McCLIM
             #+:mcclim
             (mapcar
               #'(lambda (item)
                  (if (consp item)
                     (cons (format nil " ~A " (car item)) (cdr item))
                     (cons (format nil " ~A " item) item)))
               ,menu-var)
             #-:mcclim ,menu-var
             :scroll-bars nil :y-spacing '(4 :point))))
       (when ,command-var
         (execute-menu-command
            ,(if (and (eql (length cases) 1) (consp (car cases))
	               (member (caar cases) '(quote function)))
	         `(if
	            (member ,command-var ; check that command is actually in this menu
	              (map 'list
	                #'(lambda (item)
	                   (cond ((atom item) item)
	                         ((atom (cdr item)) (cdr item))
	                         (t (getf (cdr item) :value))))
	                ,menu-var))
	            (funcall ,(car cases) ,command-var)
	            (error "Command ~A ignored since it comes from another menu" ,command-var))
	         `(case ,command-var
	            ,@cases))
            (format t "~%While attempting to execute menu command ~A" ,command-var))))))

(defmacro execute-menu-command (form context-msg)
  `(with-output-to-top ()
     (handler-case
         (prog1 ,form
           (force-output *lkb-background-stream*)) ; flush any diagnostic messages
       ;; placeholder - we need a way of generating an interrupt which will
       ;; affect these processes
       #+:allegro
       (excl:interrupt-signal (condition)
         (format t "~%Interrupted: ~A~%" condition))
       #+:ccl
       (ccl:interrupt-signal-condition (condition)
         (format t "~%Interrupted: ~A~%" condition))
       (storage-condition (condition)
         ,context-msg
         (format t "~%Memory allocation problem: ~A~%" condition))
       (error (condition)
         ,context-msg
         (format t "~%Error: ~A~%" condition))
       (serious-condition (condition)
         ,context-msg
         (format t "~%Unexpected problem: ~A~%" condition)))))


;;; ========================================================================
;;; Define general frame class for LKB frames

(clim:define-application-frame lkb-frame ()
  ((class-frames :initform nil
		 :accessor class-frames
		 :allocation :class)
   (selected :initform nil
	     :accessor frame-selected))
  (:layouts (default))) ; to avoid an empty ecase warning in sbcl


;;; Register frames of each class when they are created, and deregister when they are
;;; closed (whether by a menu command or a window manager-placed button on the window)

(defparameter *lkb-frame-lock* (mp:make-process-lock))
(defvar *last-frame* nil)

(defvar *manage-window-placement*
  ;; if the CLIM implementation or the window manager does a good job of window placement then
  ;; this should be set to nil, otherwise the LKB computes window placement itself
  #+:mcclim t #-:mcclim nil)

(defparameter +frame-cascade-offset+
  ;; vertical and horizontal offset from previous frame in cascade
  22)

(defparameter +window-manager-top-offset+
  ;; actual top coordinate of a frame opened with :top 0 due to title bar - when we
  ;; request top position p we actually get (+ p +window-manager-top-offset+)
  22)


(defmethod clim:run-frame-top-level :before ((frame lkb-frame) &key)
  (mp:with-process-lock (*lkb-frame-lock*)
    (push frame (getf (class-frames frame) (class-of frame)))))

(defmethod clim:frame-exit :before ((frame lkb-frame)
                                    #+:allegro &rest #+:allegro keys)
  ;; !!! the &rest argument in Allegro CLIM is undocumented and conflicts with the CLIM 2 spec
  #+:allegro (declare (ignore keys))
  (mp:with-process-lock (*lkb-frame-lock*)
    ;; if this frame was the last to be created then deregister it
    (when (eq frame *last-frame*)
      (setq *last-frame* nil))
    (setf
      (getf (class-frames frame) (class-of frame))
      (delete frame (getf (class-frames frame) (class-of frame))))))

;;; Find a sensible position on the screen for a new frame

(defmethod initialize-instance :around ((frame lkb-frame) &rest initargs)
  (if *manage-window-placement*
    (multiple-value-bind (left top width height)
        (compute-frame-position-and-size frame)
      (apply #'call-next-method
        frame :left left :top top :width width :height height initargs))
    (call-next-method)))

(let ((last-frame-position (clim:make-point 0 #+:darwin 1 #-:darwin 24))
      (next-frame-position (clim:make-point 0 #+:darwin 1 #-:darwin 24))
      (cascade-initial-top-left 0))
  (defun compute-frame-position-and-size (frame)
    (mp:with-process-lock (*lkb-frame-lock*)
      (multiple-value-bind (screen-width screen-height)
           (display-screen-size)
        ;; see whether we can reuse the last frame's position
        (let ((last *last-frame*))
	  (cond
	    ((null last)
	      ;; last frame no longer exists - NB doesn't work to check whether last frame's
	      ;; state is :disowned since that's also true of frames in process of initialisation
	      (setq next-frame-position last-frame-position))
	    ((and (eq (ignore-errors (clim:frame-state last)) ; allow for incomplete initialisation
	              :enabled)
	          (not (eql (frame-screen-boundary last) 0)) ; probably completely initialised
	          (not (frame-position-close-p last last-frame-position)))
	      ;; last frame is on screen but not near the position it was opened at
              (setq next-frame-position last-frame-position))))
        ;; if the next position is too far down or right then start a new cascade near top
        (multiple-value-bind (too-right-p too-low-p)
            (position-near-boundary-p next-frame-position screen-width screen-height)
          (when (or too-low-p too-right-p)
            (setf next-frame-position
              (clim:make-point
                (if too-right-p
                  (setq cascade-initial-top-left
                     (rem (+ cascade-initial-top-left (floor (- screen-height 400) 4))
                          (- screen-width 400)))
                  (- (clim:point-x next-frame-position) (floor (- screen-height 400) 2)))
                #+:darwin 1 #-:darwin 24))))
        ;; set up for next frame's position, and reduce height/width if near screen boundary
        ;; !!! the height/width might end up being increased over the default, but there doesn't
        ;; seem to be a way of finding this out before the frame is fully initialized
        (let ((left (clim:point-x next-frame-position))
              (top (clim:point-y next-frame-position)))
	  (setq *last-frame* frame
	        last-frame-position next-frame-position
	        next-frame-position
	        (clim:make-point (+ left +frame-cascade-offset+) (+ top +frame-cascade-offset+)))
          (values left top
            (if (> (+ left 500) screen-width) (- screen-width left) nil)
            (if (> (+ top +window-manager-top-offset+ +frame-cascade-offset+ 500) screen-height)
              (- screen-height top +window-manager-top-offset+)
              nil))))))
)

(defun frame-screen-boundary (frame)
  (let ((sheet (clim:frame-top-level-sheet frame)))
    (clim:with-bounding-rectangle* (left top right bottom)
	(clim:sheet-region sheet)
      (clim:transform-rectangle* (clim:sheet-transformation sheet) left top right bottom))))

(defun frame-position-close-p (frame left-top)
  (multiple-value-bind (f-left f-top)
      (frame-screen-boundary frame)
    (and
      (<= (- f-left 20) (clim:point-x left-top) (+ f-left 20))
      (<= (- f-top +window-manager-top-offset+ 20) (clim:point-y left-top) (+ f-top 20)))))

(defun display-screen-size ()
  ;; Return as multiple values the width and height of the graft associated
  ;; with the LKB-Top frame, i.e. the screen size
  (clim:bounding-rectangle-size (clim:sheet-region (clim:graft clim-user::*lkb-top-frame*)))
  ;; testing values below
  ; (values 1024 746)
  ; (values 1280 1002)
  )

(defun position-near-boundary-p (left-top screen-width screen-height)
  ;; would fewer than 400 device units be visible at left or top of window?
  (values
    (> (clim:point-x left-top) (- screen-width 400))
    (> (clim:point-y left-top) (- screen-height 400))))


;;; Find and raise the most recently created frame of a given class

(defun reuse-frame (class)
  (let
    ((frame ; previously called clim:find-application-frame but not as robust against zombies
       (block nil
         (clim:map-over-frames #'(lambda (f) (when (typep f class) (return-from nil f)))))))
    (when frame
      (let ((latest
              (mp:with-process-lock (*lkb-frame-lock*)
	        (car (getf (class-frames frame) (find-class class))))))
	(when latest
	  (clim:enable-frame latest)
	  (clim:raise-frame latest)
	  latest)))))


;;; The general LKB frame itself

(defclass lkb-pane (clim:application-pane) ())

(defclass doc-pane (clim:application-pane) ())

(defmacro define-lkb-frame (frame-class slots
                            &rest pane-options 
			    &key (info-bar nil) &allow-other-keys)
  (let ((sc-options nil))
    ;; !!! in McCLIM, :width and :height values to an application-pane inside a scroller-pane
    ;; are not respected - fixed by giving them to the scroller instead; or in the case of
    ;; :compute values, changing them to :max- options and increasing the default
    ;; (undocumented) :suggested- options in the scroller
    #+:mcclim
    (cond
      ((eq (getf pane-options :width) :compute)
        (setf (getf sc-options :suggested-width) 800)
        (setf (getf pane-options :max-width) :compute)
        (remf pane-options :width))
      (t
        (setf (getf sc-options :width) (getf pane-options :width))
        (remf pane-options :width)))
    #+:mcclim
    (cond
      ((eq (getf pane-options :height) :compute)
        (setf (getf sc-options :suggested-height) 600)
        (setf (getf pane-options :max-height) :compute)
        (remf pane-options :height))
      (t
        (setf (getf sc-options :height) (getf pane-options :height))
        (remf pane-options :height)))
    (remf pane-options :info-bar)
    `(progn
       (clim:define-application-frame ,frame-class (lkb-frame)
         ,slots
         #+:mcclim (:menu-bar ,frame-class) ; apparently not necessary in Allegro CLIM
         (:command-table (,frame-class :inherit-from (lkb-frame)
	  			       :inherit-menu t))
         (:panes
           (lkb-pane
	     (clim:make-pane 'lkb-pane
			     :text-cursor nil
			     :end-of-line-action :allow
			     :end-of-page-action :allow
			     ;; *** :borders nil
			     :background clim:+white+
			     :foreground clim:+black+
			     :display-time nil
			     ,@pane-options)) ; :display-function, :width, :height etc
           ,@(when info-bar
	       `((doc-pane
                   (clim:make-pane 'doc-pane
				   :text-cursor nil
				   :end-of-line-action :allow
				   :end-of-page-action :allow
				   ;; *** :borders nil
				   #+:mcclim :background #+:mcclim climi::*3d-normal-color*
				   ;; in Allegro CLIM, specifying 1.1 lines avoids clipping
				   ;; - and yes, all 3 height specs are needed
				   :height '(#+:mcclim 1 #-:mcclim 1.1 :line)
				   :min-height '(#+:mcclim 1 #-:mcclim 1.1 :line)
				   :max-height '(#+:mcclim 1 #-:mcclim 1.1 :line)
				   ;; *** :record nil
				   :scroll-bars nil
                                   ,@(when (getf pane-options :text-style)
                                       `(:text-style ,(getf pane-options :text-style))))))))
         (:layouts
           (default
             (clim:vertically ()
	       (clim:scrolling (#+:mcclim ,@sc-options)
                 #+:mcclim
                 (clim:spacing (:thickness 3 :background clim:+white+) lkb-pane)
                 #-:mcclim
                 lkb-pane) ; in Allegro CLIM, spacing would stop the scroller working
	       ,@(when info-bar
	          '(#+:mcclim (clim:spacing (:thickness 1) doc-pane) ; c.f. doc-pane :height
                    #-:mcclim doc-pane))))))
      ;; in McCLIM, add the lkb-frame menu commands (Close, Close all, Print) to the
      ;; command menu for this kind of frame - not clear why this is necessary, but on
      ;; the other hand the CLIM 2 spec is baroque and underspecified in this area
      #+:mcclim
      (clim:map-over-command-table-menu-items
        #'(lambda (name char item)
           (declare (ignore char))
           (clim:add-menu-item-to-command-table ',frame-class
             name :command (clim:command-menu-item-value item) :errorp nil))
        'lkb-frame))))

(defmethod clim:frame-standard-output ((frame lkb-frame))
  ;; identify the main lkb-pane, otherwise the doc-pane could get picked up since it's
  ;; also an application pane
  (clim:find-pane-named frame 'lkb-pane))


;;; Allow the info bar to describe an object when the pointer is over it

(defmacro define-info-bar (type vars &body body)
  `(clim:define-presentation-method clim:highlight-presentation 
       ((type ,type) record stream state)
     state
     ;; convert-from-relative-to-absolute-coordinates is only mentioned in passing in
     ;; in the CLIM 2 spec; McCLIM has nothing to do since it keeps output records in
     ;; stream coordinates
     (multiple-value-bind (xoff yoff)
	 #+:mcclim (values 0 0)
	 #-:mcclim
         (clim:convert-from-relative-to-absolute-coordinates
           stream (clim:output-record-parent record))
       (let* ((,(first vars) (clim:presentation-object record))
	      (,(second vars) (clim:find-pane-named (clim:pane-frame stream) 'doc-pane)))
	 (if (eq state :highlight)
	     (progn
               (setf (clim:stream-cursor-position ,(second vars)) (values 2 0))
               ,@body)
	     (clear-doc-pane ,(second vars))))
       ;; draw/erase rectangle around presentation
       (clim:with-bounding-rectangle* (left top right bottom) record
	 (clim:draw-rectangle* stream
			       (+ left xoff) (+ top yoff)
			       (+ right xoff) (+ bottom yoff)
			       :filled nil
			       :ink clim:+flipping-ink+)))))

(defun clear-doc-pane (pane)
  #-:mcclim
  (clim:window-clear pane)
  #+:mcclim
  (progn
    ;; !!! do it by steam since window-clear causes a small scroll to left in lkb-pane
    (clim:stream-close-text-output-record pane)
    (clim:clear-output-record (clim:stream-output-history pane))
    (clim:window-erase-viewport pane)))


;;; Commands for [Close], [Close All] and [Print]

(define-lkb-frame-command (com-close-frame :menu "Close") 
    ()
  (clim:with-application-frame (frame)
    (unhighlight-objects frame)
    (clim:frame-exit frame)))

(define-lkb-frame-command (com-close-all-frame :menu "Close All") 
    ()
  (mp:with-process-lock (*lkb-frame-lock*)
    (clim:with-application-frame (frame)
      (let ((frames
              (getf (class-frames frame) (class-of frame))))
        (dolist (f frames)
	  ;; Make sure we close ourself last
	  (unless (eq f frame)
	    (clim:execute-frame-command f '(com-close-frame))))
        ;; short delay so front window close does not overtake ones beneath (if any) - if
        ;; it does, then each window can end up uselessly redrawing previously obscured
        ;; content just before it closes
        (when (cdr frames) (sleep 0.2))
        (clim:execute-frame-command frame '(com-close-frame))))))
  

(define-lkb-frame-command (com-print-frame :menu "Print") 
    ()
  (clim:with-application-frame (frame)
    (with-output-to-top ()
      (print-pane-to-postscript frame (clim:find-pane-named frame 'lkb-pane)))))


;;; Postscript printing

(defvar *last-cursor-position-y* 0)

(defun print-pane-to-postscript (frame pane)
  ;; NB if converting PS files to PDF and the selected paper size was not US Letter, then
  ;; the user will probably need something like ps2pdf -sPAPERSIZE=a4 out.ps out.pdf
  (multiple-value-bind (dest size orient multi file)
      (get-print-options)
    (case dest
      (:printer (show-message-window "Direct printing not yet implemented"))
      (:file	
	  (when (cond
	           ((null (pathname-name file))
	             (show-message-window "No destination file specified") nil)
	           ((not (probe-file file)) t)
		   (t
		     (lkb-y-or-n-p
		       (format nil "File `~a' already exists.~%Overwrite it?" file))))
	     (execute-menu-command
		(with-open-file (ps-stream file 
				  :direction :output 
				  :if-exists :supersede)
		   (clim:with-output-to-postscript-stream 
		       (stream ps-stream 
			       #+:mcclim :device-type #+:mcclim size
                               :scale-to-fit (not multi) 
			       :multi-page multi
			       :orientation orient)
                     ;; !!! McCLIM kludge for textual output e.g. feature structures. Page
                     ;; breaks are not automatically inserted in a stream of postscript
                     ;; output, so pretend to wrap when output reaches bottom of page, but then
                     ;; when cursor position goes back to low y position request a new page.
                     ;; Also, avoid line wrap otherwise sometimes get invalid PS
                     #+:mcclim (setf (clim:stream-end-of-page-action stream) :wrap)
                     #+:mcclim (setf (clim:stream-end-of-line-action stream) :allow)
		     (let ((*last-cursor-position-y* 0))
                       (funcall (clim-internals::pane-display-function pane)
			        frame stream))))
		(format t "~%While attempting to execute menu command ~A" "Print")))))))

#+:mcclim
(defmethod clim:stream-set-cursor-position :around ((stream clim-postscript::postscript-stream)
                                                    x y)
  ;; !!! at end of displaying an FS, must reset *last-cursor-position-y* in case we're about
  ;; to start displaying another - see fs-output-record-end in io-general/outputfs.lsp
  (when (boundp '*last-cursor-position-y*)
    (when (< y *last-cursor-position-y*) (clim:new-page stream))
    (setq *last-cursor-position-y* y))
  (call-next-method))

#+:mcclim
(defmethod clim:window-clear ((stream clim-postscript::postscript-stream))
  ;; do nothing
  )

#+:mcclim
(defmethod clim:change-space-requirements ((stream clim-postscript::postscript-stream)
                                           &key &allow-other-keys)
  ;; do nothing
  )

#+:mcclim 
(defmethod clim:pane-text-style ((stream clim-postscript::postscript-stream))
  ;; !!! this should really use the text-style of the pane to be printed
  clim:*default-text-style*)


;;; Highlight a list of objects, making the first one red

(defun highlight-objects (things frame)
  (let ((stream (clim:frame-standard-output frame)))
    (unhighlight-objects frame)
    (setf (frame-selected frame) 
      (list
       (clim:with-new-output-record (stream)
	 (clim:with-output-recording-options (stream :record t)
	   (map nil ; things is a sequence but not necessarily a list
	     #'(lambda (thing)
	        (when thing
	          (multiple-value-bind (x1 y1 x2 y2)
		      (clim:bounding-rectangle* 
		       (clim:output-record-parent thing))
		    (clim:draw-rectangle* stream x1 y1 x2 y2
				          :ink clim:+flipping-ink+ 
				          :filled t))))
	     things)))))))

;;; NB These flipping inks cannot be constants since clim:make-flipping-ink
;;; does not guarantee EQ compile and load time results given the same arguments.
;;; Also, both args must be colors not inks themselves
;;; *** temporary test, to allow this file to be loaded into an old LOGON LKB session

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp '+magenta-flipping-ink+)
(defparameter +magenta-flipping-ink+ 
    (clim:make-flipping-ink clim:+green+ clim:+white+))

(defparameter +cyan-flipping-ink+ 
    (clim:make-flipping-ink clim:+red+ clim:+white+))

(defparameter +blue-flipping-ink+ 
    (clim:make-flipping-ink clim:+yellow+ clim:+white+))

(defparameter +green-flipping-ink+ 
    (clim:make-flipping-ink clim:+magenta+ clim:+white+))

(defparameter +red-flipping-ink+ 
    (clim:make-flipping-ink clim:+cyan+ clim:+white+))

(defparameter +yellow-flipping-ink+ 
    (clim:make-flipping-ink clim:+blue+ clim:+white+))
  ))

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
    (find-object-1
      #+:mcclim (clim:stream-current-output-record stream)
      #-:mcclim (slot-value stream 'clim:output-record) ; Allegro CLIM undocumented interface
      stream test)))

(defun find-object-1 (rec stream test)
  (clim:map-over-output-records 
   #'(lambda (rec)
       (when (clim:presentationp rec) 
	 (if (funcall test (clim:presentation-object rec))
	     (throw 'find-object rec)))
       (map nil
         #'(lambda (q) (find-object-1 q stream test))
         (clim:output-record-children rec)))
   rec))


;;; Center the viewport on object

(defun scroll-to (record stream)
  #+:mcclim
  (when (typep stream 'clim-postscript::postscript-stream) ; are we printing?
    (return-from scroll-to nil))
  #+:mcclim (setq stream (clim:sheet-parent stream)) ; !!! skip 1 level up to actual scrollee
  (let* ((vp-width (clim:bounding-rectangle-width 
		    (clim:pane-viewport-region stream)))
         (vp-height (clim:bounding-rectangle-height
		     (clim:pane-viewport-region stream)))
	 (x-pos (bounding-rectangle-center-x (clim:output-record-parent record)))
	 (y-pos (bounding-rectangle-center-y (clim:output-record-parent record)))
	 (x-max (clim:bounding-rectangle-max-x stream))
	 (y-max (clim:bounding-rectangle-max-y stream)))
    (clim:scroll-extent stream
			(max 0 (min (- x-pos (floor vp-width 2))
				    (- x-max vp-width)))
			(max 0 (min (- y-pos (floor vp-height 2))
				    (- y-max vp-height))))))

(defun bounding-rectangle-center-x (br)
   ;; unfortunately the CLIM 2 spec does not include bounding-rectangle-center
   (/ (+ (clim:bounding-rectangle-min-x br) (clim:bounding-rectangle-max-x br)) 2))

(defun bounding-rectangle-center-y (br)
   (/ (+ (clim:bounding-rectangle-min-y br) (clim:bounding-rectangle-max-y br)) 2))


;;; Generic message window
;;; 
;;; To display some of the messages which used to appear in the LKB Top etc and were often
;;; not noticed by users. Deals sensibly with long messages by breaking them at spaces
;;; and/or every 80 characters.
;;;
;;; E.g
;;; (show-message-window "Could not perform action")
;;; (show-message-window "Do you really want to do that?" '("Yes" "No") "Confirm")

(defun show-message-window (str &optional buttons title)
  (with-dialog-positioning (left top) 600
    (let ((frame
   	   (clim:make-application-frame 'notification-dialog
    	     :pretty-name (or title "Notification")
    	     :left left :top top
    	     :string str
    	     :buttons (or buttons '("OK")))))
      (clim:run-frame-top-level frame)
      (notification-dialog-result frame))))

(clim:define-application-frame notification-dialog ()
  ((string :initform "" :initarg :string :reader notification-window-string)
   (buttons :initform nil :initarg :buttons :reader notification-window-buttons)
   (result :initform nil :accessor notification-dialog-result))
  (:menu-bar nil)
  (:pane
    (clim:spacing (:thickness 15)
      (clim:horizontally (:x-spacing 30
                          :equalize-height nil 
                          #+:mcclim :max-width #+:mcclim '(:relative 0)) ; prevent any stretch
        (clim:make-pane 'clim:vbox-pane
          :min-width 350
          :contents
          (mapcar
            #'(lambda (str) (clim:make-pane 'clim:label-pane :label str))
            (append
              (split-at-linefeeds-and-squeeze
                (notification-window-string clim:*application-frame*))
              '(""))))
        #-:mcclim :fill ; in Allegro CLIM, can't prevent horizontal stretch so make it here
        (clim:make-pane #+:mcclim 'clim:vrack-pane #-:mcclim 'clim:vbox-pane
          :y-spacing 10
          :contents
          (cons 
            (clim:make-pane 'clim:push-button
              :label
              (format nil "~10:@< ~A ~>"
                (first (notification-window-buttons clim:*application-frame*)))
              :align-x :center
              :y-spacing 5
              #-:mcclim :show-as-default #+:mcclim :show-as-default-p t ; keyword discrepancy
              :activate-callback
              #'(lambda (button)
                  (declare (ignore button))
                  (clim:with-application-frame (frame)
                    (setf (notification-dialog-result frame) t)
                    (clim:frame-exit frame))))
            (append
              (if (cdr (notification-window-buttons clim:*application-frame*))
                (list
                  (clim:make-pane 'clim:push-button
                    :label
                    (format nil "~10:@< ~A ~>"
                      (second (notification-window-buttons clim:*application-frame*)))
                    :align-x :center
                    :y-spacing 5
                    :activate-callback #'dialog-close-callback)
                  #+:mcclim 1)) ; otherwise bottom of button may be clipped off
              (list :fill))))))))

#+:mcclim
(defmethod clim-extensions:find-frame-type ((frame notification-dialog))
  ;; make dialogs have more dialog-like window controls (e.g. no maximize button)
  :dialog)

(defun dialog-close-callback (button)
  (declare (ignore button))
  (clim:with-application-frame (frame) (clim:frame-exit frame)))


(defun split-at-linefeeds-and-squeeze (s)
  ;; split string every 80 characters and at newlines (squeezing repeats) -
  ;; counts characters whereas it should really count device units
  (setq s (string-left-trim '(#\space #\tab) s))
  (let ((p (position #\newline s)))
    (cond
      ((equal s "") nil)
      ((eql p 0) (split-at-linefeeds-and-squeeze (subseq s (1+ p))))
      ((or (null p) (> p 80))
         (if (> (length s) 80)
             (cons (subseq s 0 80) (split-at-linefeeds-and-squeeze (subseq s 80)))
             (list s)))
      (t (cons (subseq s 0 p) (split-at-linefeeds-and-squeeze (subseq s (1+ p))))))))

