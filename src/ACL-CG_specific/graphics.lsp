;;; Copyright (c) 1991-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions


(in-package :lkb)

;;; the following is currently a no-op - it's
;;; defined for the CLIM version - 
;;; something similar may be needed here eventually

(defmacro with-output-to-top (() &body body)
  `(progn
     ,@body
     (terpri)))

(defun move-to-x-y (stream x y)
   (cg:move-to-x-y stream x y))

(defun draw-line-x-y (stream from-x from-y to-x to-y dashing)
  (declare (ignore dashing))
  (cg:draw-line stream (cg:make-position from-x from-y) 
    (cg:make-position to-x to-y)))

(defun move-by-y (stream y)
   (cg:move-by-x-y stream 0 y))

(defun current-position-y (stream)
  (cg:current-position-y stream))

(defun current-position-x (stream)
   (cg:current-position-x stream))

(defun current-position (stream)
  (cg:current-position stream))


 
;;; RECTANGLE DRAWING

(defun frame-text-box (stream start-pos end-pos)
  ;;; given a start position and an end position from some text output,
  ;;; draws a suitable box round it
  ;;; 
  ;;; maybe use draw-box?
  (declare (ignore stream start-pos end-pos))
   nil)
   
(defun invert-text-box (stream start-pos end-pos)
  ;;; given a start position and an end position from some text output,
  ;;; inverts the region enclosing it
   (declare (ignore stream start-pos end-pos))
   nil)
  
;;; FONT HANDLING

(defun font (stream)
  (cg:font stream))

(defun font-height (font-spec)
   (cg:font-height font-spec))
   
(defun font-ascent (stream)
  (cg:font-ascent (cg:font stream)))

(defun font-descent (stream)
  (cg:font-descent (cg:font stream)))



(defun stream-string-width (stream str)
  (cg:stream-string-width stream str))


;;; BOLD AND UNDERLINED OUTPUT

(defmacro with-bold-output (ostream body)
  `(let ((current-font (cg:font ,ostream)))
      (setf (cg:font ,ostream) (cg:vary-font current-font 
                             :style (cons :bold (cg:font-style current-font))))
      (eval ,body)
      (setf (cg:font ,ostream) current-font)))

(defmacro with-underlined-output (ostream body)
  `(let ((current-font (cg:font ,ostream)))
      (setf (cg:font ,ostream) (cg:vary-font current-font 
                             :style (cons :underline (cg:font-style current-font))))
      (eval ,body)
      (setf (cg:font ,ostream) current-font)))


(defun lkb-y-or-n-p (strg)
   ;; define so it uses y-or-n-p-dialog 
   (cg:y-or-n-dialog strg :cancel-text nil))


;;; FIX - add this sometime

(defun show-message-window (message)
  (declare (ignore message))
  nil)
