(in-package :cl-user)

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
  `(let ((current-font (font ,ostream)))
      (setf (font ,ostream) (vary-font current-font 
                             :style (cons :bold (font-style current-font))))
      (eval ,body)
      (setf (font ,ostream) current-font)))

(defmacro with-underlined-output (ostream body)
  `(let ((current-font (font ,ostream)))
      (setf (font ,ostream) (vary-font current-font 
                             :style (cons :underline (font-style current-font))))
      (eval ,body)
      (setf (font ,ostream) current-font)))


(defun lkb-y-or-n-p (strg)
   ;; define so it uses y-or-n-p-dialog 
   (cg:y-or-n-dialog strg :cancel-text nil))


