;;; Copyright (c) 1998--2002.
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

;; A thread-safe first-in first-out queue

#+(or :allegro :lispworks)
(defmacro with-queue-lock ((queue) &body body)
  (let ((lock (gensym)))
    `(let ((,lock (queue-lock ,queue)))
       (mp:with-process-lock (,lock)
	 ,@body))))

#-(or :allegro :lispworks)
(defmacro with-queue-lock ((queue) &body body)
  (declare (ignore queue))
  `(progn
     ,@body))

(defstruct (queue (:constructor x-make-queue ()))
  lock
  head
  tail)

(defun make-queue ()
  (let ((queue (x-make-queue)))
    #+(or :allegro :lispworks) (setf (queue-lock queue) (mp:make-process-lock))
    (setf (queue-head queue) (cons nil nil))
    (setf (queue-tail queue) (queue-head queue))
    queue))

(defmethod enqueue (queue item)
  (with-queue-lock (queue)
    (setf (cdr (queue-tail queue)) (cons item nil))
    (setf (queue-tail queue) (cdr (queue-tail queue)))))

(defmethod dequeue (queue)
  (with-queue-lock (queue)
    (unless (queue-empty-p queue)
      (prog1 
	  (cadr (queue-head queue))
	(setf (queue-head queue) (cdr (queue-head queue)))))))

(defun queue-empty-p (queue) 
  (with-queue-lock (queue)
    (eq (queue-tail queue) (queue-head queue))))

;; Perform some process asynchronously.  Source is a function that returns
;; items to be processed one at a time, returning :eof when there are no more
;; items.  Sink is a function that takes an item to be processed as an
;; argument, and processes it. This should be useful for interleaving slow I/O
;; operations with CPU intensive processing, e.g., when expanding lots of
;; lexical entries.

#+(or :allegro :lispworks)
(defun process-queue (source sink)
  (let ((queue (make-queue)))
    (let ((child
	   (mp:run-function 
	    "source"
	    #'(lambda ()
		(loop for item = (funcall source)
		    do (enqueue queue item)
		    until (eq item :eof))))))
      (unwind-protect
	  (loop for item = (progn	 
			     (mp:process-wait "waiting" 
					      #'(lambda () 
						  (not (queue-empty-p queue))))
			     (dequeue queue))
	      until (eq item :eof)
	      do (funcall sink item))
	(mp:process-kill child)))))

#-(or :allegro :lispworks)
(defun process-queue (source sink)
  (loop for item = (funcall source)
      until (eq item :eof)
      do (funcall sink item)))
