;; -*- mode: common-lisp; package: user -*-
;;
;; Copyright (c) 1993 Franz Inc, Berkeley, CA
;;
;; Auto zoom on error.
;;
;; Licensed users of Allegro CL may include the following macro in their
;; product, provided that product is only compiled with a licensed Allegro
;; CL compiler.
;;
;; $Header$

(in-package :user)

(defmacro with-auto-zoom-and-exit ((place &key (exit 't) (no-unwind 't))
				   &body body)
  (let ((p (gensym)))
    `(let ((,p ,place))
       (handler-bind
	   ((error (lambda (e)
		     (with-standard-io-syntax
		       (let ((*print-readably* nil)
			     (*print-miser-width* 40)
			     (*print-pretty* t)
			     (tpl:*zoom-print-circle* t)
			     (tpl:*zoom-print-level* nil)
			     (tpl:*zoom-print-length* nil))
			 (ignore-errors	;prevent recursion
			  (format *terminal-io* "~
~@<An unhandled error condition has been signalled:~3I ~a~I~:@>~%~%"
				  e))
			 (ignore-errors	;prevent recursion
			  (flet ((zoom (s)
				   (let ((*terminal-io* s)
					 (*standard-output* s))
				     (tpl:do-command "zoom"
				       :from-read-eval-print-loop nil
				       :count t :all t))))
			    (if* (streamp ,p)
			       then (zoom ,p)
			       else (with-open-file (s ,p :direction :output
						     :if-exists :supersede)
				      (zoom s)))))
			 (when ,exit
			   (exit 1 :no-unwind ,no-unwind)))))))
	 ,@body))))
