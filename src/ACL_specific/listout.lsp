;;; Copyright (c) 1998-2018
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see LICENSE for conditions

;;; Ann Copestake / John Carroll

;;; output active lists in a window - pass in an alist of string /
;;; data pairs, a window title, and an alist of menu command string /
;;; function pairs.  Each function should take 1 argument: the data
;;; item associated with the string that is chosen

(in-package :lkb)


;;; Appearance parameters

(defun active-list-item-font ()
  (clim:make-text-style :sans-serif :roman (or *list-item-font-size* 12)))

(declaim (notinline active-list-item-font))


;;; The active window

(eval-when #+:ansi-eval-when (:load-toplevel :compile-toplevel :execute)
           #-:ansi-eval-when (load eval compile)
  (defstruct thing object))

(clim:define-presentation-type thing ())

(define-lkb-frame list-window
    ((pairs :initform nil
	    :accessor list-window-pairs)
     (menu :initform nil
	   :accessor list-window-menu))
  :display-function 'draw-list-window
  :text-style (active-list-item-font)
  :width :compute
  :height *parse-window-height*) ; not :compute so window isn't excessively tall or short

(defun draw-active-list (pairs title menu)
  (mp:run-function title #'draw-active-list-really pairs title menu))
                           
(defun draw-active-list-really (pairs title menu)                           
  (let ((frame (clim:make-application-frame 'list-window)))
    (setf (list-window-pairs frame) pairs)
    (setf (list-window-menu frame) menu)
    (setf (clim:frame-pretty-name frame) title)
    (clim:run-frame-top-level frame)))

(defun draw-list-window (frame stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (dolist (pair (list-window-pairs frame))
    (clim:with-output-as-presentation (stream (make-thing :object (cdr pair))
					      'thing)
      (write-line (car pair) stream))))

(define-list-window-command (com-list-window-menu)
    ((object 'thing :gesture :select))
  (when (thing-p object)
    (clim:with-application-frame (frame)
      (pop-up-menu
         (loop for item in (list-window-menu frame)
	       for command upfrom 0
	       collecting (list (car item) :value command))
         #'(lambda (command)
	     (funcall (cdr (nth command (list-window-menu frame)))
		      (thing-object object)))))))

