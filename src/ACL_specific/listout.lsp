;;; Copyright John Carroll 1998
;;; All Rights Reserved.
;;; No use or redistribution without permission.
;;; 
;;; Ann Copestake / John Carroll

;;; outputing active lists in a window - pass in an alist of string /
;;; data pairs, a window title, and an alist of menu command string /
;;; function pairs.  Each function should take 1 argument, the data
;;; item associated with the string that is chosen

;;; dialect specific from this point

(in-package :user)

(def-lkb-parameter *lkb-list-font*
    (list :bold "Helvetica" (or *parse-tree-font-size* 9)))

(defstruct thing object)

(define-lkb-frame list-window
    ((pairs :initform nil
	    :accessor list-window-pairs)
     (menu :initform nil
	   :accessor list-window-menu))
  :display-function 'draw-list-window
  :width *parse-window-width* 
  :height *parse-window-height*)

(defun draw-active-list (pairs title menu)
  (let ((frame (clim:make-application-frame 'list-window)))
    (setf (list-window-pairs frame) pairs)
    (setf (list-window-menu frame) menu)
    (setf (clim:frame-pretty-name frame) title)
    (mp:process-run-function title #'clim:run-frame-top-level frame)))

(defun draw-list-window (frame stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (dolist (pair (list-window-pairs frame))
    (terpri stream)
    (clim:with-output-as-presentation (stream (make-thing :object (cdr pair))
					      'thing)
      (write-line (car pair) stream))))

(define-list-window-command (com-list-window-menu)
    ((object 'thing :gesture :select))
  (when (thing-p object)
    (clim:with-application-frame (frame)
      (let ((command (clim:menu-choose 
		      (loop for item in (list-window-menu frame)
			  for count upfrom 0
			  collecting (list (car item) :value count)))))
	(when command
	  (handler-case
	      (funcall (cdr (nth command (list-window-menu frame)))
		       (thing-object object))
	    (error (condition)
	      (with-output-to-top ()
		(format t "~%Error: ~A~%" condition)))))))))
