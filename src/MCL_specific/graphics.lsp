;;; Copyright (c) 1991-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see LICENSE for conditions

;;; Graphical fs drawing 
;;; CL dialect specific

(in-package :lkb)

(eval-when (:compile-toplevel :load-toplevel :execute)
   (require 'quickdraw))


;;; the following is currently a no-op - it's
;;; defined for the CLIM version - 

(defmacro with-output-to-top (() &body body)
  `(progn ,@body))


;;; SIMPLE DRAWING

(defun move-to-x-y (stream x y)
  (with-focused-view stream
     (#_MoveTo :long (make-point x y)))
  nil)

(defun draw-line-x-y (stream from-x from-y to-x to-y dashing)
  (with-focused-view stream
     (#_MoveTo :long (make-point from-x from-y))
     (if dashing
       ;; make pen colour and pattern grey - this seems to look best
       (with-port (wptr stream)
          (#_PenPat *gray-pattern*)
          (with-fore-color *dark-gray-color*
             (#_LineTo :long (make-point to-x to-y)))
          (#_PenNormal))
       (#_LineTo :long (make-point to-x to-y))))
  nil)

(defun move-by-y (stream y)
  (with-focused-view stream
     (#_Move :long (make-point 0 y)))
  nil)

(defun draw-circular-link-x-y (stream centre-x center-y radius horizontalp)
   ;; shouldn't get called in lkb
   (declare (ignore stream centre-x center-y radius horizontalp))
   (error "~A shouldn't have been called" 'draw-circular-link-x-y))


(defun current-position-y (stream)
  (point-v (current-position stream)))

(defun current-position-x (stream)
  (point-h (current-position stream)))

(defun current-position (stream)
  (with-focused-view stream
     (rlet ((pos :long))
       (#_GetPen pos)
       (%get-long pos))))

 
;;; RECTANGLE DRAWING

(defun frame-text-box (stream start-pos end-pos)
  ;;; given a start position and an end position from some text output,
  ;;; draws a suitable box round it
  (perform-op-on-rect 'frame-rect stream start-pos end-pos))

(defun invert-text-box (stream start-pos end-pos)
  ;;; given a start position and an end position from some text output,
  ;;; inverts the region enclosing it
  (perform-op-on-rect 'invert-rect stream start-pos end-pos))

(defun perform-op-on-rect (op stream start-pos end-pos)
  ;; note stupid coordinate system - frame-rect and invert-region
  ;; take an upper left corner and a lower right
  ;; and the origin is the top left of a window - getting greater going
  ;; down
  ;; note also that we have to adjust for the font stuff - the start position
  ;; is the base of the text line ignoring descenders
  (funcall op stream
           (make-point (- (point-h start-pos) 1)
                       (- (- (point-v start-pos) (font-ascent stream)) 1))
           (add-points end-pos (make-point 1 (1+ (font-descent stream))))))


;;; For efficiency the box is passed as a dotted pair of points
(defun make-box-relative-from-corner (start-position w h)
  (cons start-position (make-point (+ (point-h start-position) w)
                                   (+ (point-v start-position) h))))
                              
  
(defun draw-box (stream box)
  (frame-rect stream (car box) (cdr box)))

(defun inside-box-p (position box)
  (let ((xpos (point-h position)) (ypos (point-v position)))
    (and (>= xpos (point-h (car box)))
         (<= xpos (point-h (cdr box)))
         (>= ypos (point-v (car box)))
         (<= ypos (point-v (cdr box))))))


;;; FONT HANDLING

(defun font (stream)
  (view-font stream))

(defun font-height (font-spec)
  ;;; copied from ref manual page 
  (multiple-value-bind (ascent descent max-width leading)
                       (font-info font-spec)
    (declare (ignore max-width))
    (+ ascent descent leading)))

(defun font-ascent (stream)
  (multiple-value-bind (ascent descent max-width leading)
                       (font-info (view-font stream))
    (declare (ignore max-width descent leading))
    ascent))

(defun font-descent (stream)
  (multiple-value-bind (ascent descent max-width leading)
                       (font-info (view-font stream))
    (declare (ignore max-width ascent leading))
    descent))

(defun font-leading (stream)
  (multiple-value-bind (ascent descent max-width leading)
                       (font-info (view-font stream))
    (declare (ignore max-width ascent descent))
    leading))

  
(defun stream-string-width (stream str)
  (string-width str (view-font stream)))


;;; BOLD AND UNDERLINED OUTPUT

(defmacro with-text-style-bold-face ((ostream) &rest body)
  `(let ((.current-codes. (multiple-value-list (view-font-codes ,ostream))))
     (set-stream-font-spec ,ostream '(:bold))
     ,@body
     (apply #'set-view-font-codes ,ostream .current-codes.)))

(defmacro with-underlined-output (ostream &rest body)
  `(let ((.current-codes. (multiple-value-list (view-font-codes ,ostream))))
     (set-stream-font-spec ,ostream '(:underline))
     ,@body
     (apply #'set-view-font-codes ,ostream .current-codes.)))
    

(defun set-stream-font-spec (stream font-spec)
   (apply #'set-view-font-codes 
      stream (multiple-value-list (font-codes font-spec))))


;;;

(defun lkb-y-or-n-p (strg)
   (y-or-n-dialog strg :cancel-text nil))


;;;

(defun show-message-window (message)
   ;; could do this by (message-dialog message :title "") but dialog
   ;; box wouldn't match the y/n one
   (y-or-n-dialog message :yes-text "OK" :no-text nil :cancel-text nil)
   nil)



