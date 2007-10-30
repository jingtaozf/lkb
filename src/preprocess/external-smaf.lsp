;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LKB -*-

;; bmw20@hf.ntnu.no
;; (based in part on oe's SPPP implementation)

;; code to invoke external SAF preprocessor at runtime
;; configure with
;;
;;  (setf saf::*e-application* "~/lkb/src/preprocess/external-smaf.sh")
;;
;; initialize with
;;
;;  (saf::e-initialize)
;;
;; now sentence text will be sent thru external SAF preprocessor
;;  
;; PROTOCOL:
;;  (-> external processor) sentence text terminated with #\newline \#page #\newline
;;  (<- external processor) SMAF XML terminated with #\newline \#page #\newline
;;
;; shutdown with
;;
;;  (saf::e-shutdown)
;;
;; to see debugging messages:
;;
;;  (setf smaf::*e-debug-p* t)
;;

(in-package :saf)

(defvar *e-pid* nil)
(defvar *e-stream* nil)
(defvar *e-application* nil)

(defparameter *e-input-buffer* 
  (make-array 2048 :element-type 'character :adjustable nil :fill-pointer 0))

(defvar *e-debug-p* nil)

;; INITIALIZATION
(defun e-initialize ()
  (unless *e-application*
    (error "*e-application* unset")
    (return-from e-initialize))
  (e-shutdown)
  (let (foo)
    (when *e-debug-p*
      (format t "~%[smaf] starting up external processor..."))    
    (multiple-value-setq (*e-stream* foo *e-pid*)
      (lkb::run-process *e-application*
			:wait nil
			:output :stream :input :stream :error-output nil))
    (when *e-debug-p*
      (format t " (~a)" *e-pid*))    
     (when foo (setf foo foo))))

;; SHUTDOWN
(defun e-shutdown ()
  (when *e-stream*
    (when *e-debug-p*
      (format t "~%[smaf] shutting down stream to external processor"))
    (close *e-stream*)
    (setf *e-stream* nil))
  (when *e-pid*
    (when *e-debug-p*
      (format t "~%[smaf] shutting down external process (~a)" *e-pid*))
    (lkb::run-process (format nil "kill -KILL ~d" *e-pid*) 
		      :wait t)
    ;#+:allegro
    ;(sys:os-wait nil *e-pid*)
    (setf *e-pid* nil)
    ))

;; PROCESS DATA
(defun e (text &key (stream *e-stream*))
  (unless (streamp stream)
    (error "[smaf] no external processor running"))
  (unless (output-stream-p stream)
    (error "[smaf] no output stream to external processor"))
  #+:allegro 
  (setf (stream-external-format stream) (excl:find-external-format :utf-8))
  (when *e-debug-p*
    (format t "~&SENT: ~a" text))
  (format stream "~a~%~%" text)
  (force-output stream)
  #+:allegro
  ;(setf (stream-external-format stream) (excl:find-external-format :utf-8))
  (loop
      with size = (array-dimension *e-input-buffer* 0)
      initially (setf (fill-pointer *e-input-buffer*) 0)
      for n from 1
      for c = (read-char stream nil nil)
      when (null c) do 
	(format
	 t
	 "sppp(): premature end of file (after ~a characters)~%" 
	 n)
	(return)
      when (= n size) do
	(incf size size)
	(setf *e-input-buffer* 
	  (adjust-array *e-input-buffer* size))
      when (char= c #\page) do
	(progn
	  (when *e-debug-p*
	    (format t "~&RETURNED: ~a" *e-input-buffer*))
	  (when (and (numberp n) (> n 1))
	    (return
	      (xml-to-saf-object *e-input-buffer*))))
      while c do (vector-push c *e-input-buffer*)))

