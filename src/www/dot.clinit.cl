(in-package :common-lisp)

;;;
;;; make sure we have enough old- and new-space available for real grammars
;;;
(system:resize-areas :old (* 320 1024 1024) :new (* 512 1024 1024))

(setf (sys:gsgc-parameter :expansion-free-percent-new) 5)
(setf (sys:gsgc-parameter :free-percent-new) 2)
(setf (sys:gsgc-parameter :expansion-free-percent-old) 5)

(let ((excl:*enable-package-locked-errors* nil))

  (defun verbose-gc (&key (verbose t) tenure)
    (when verbose
      (when (find-package :tsdb)
	(set (find-symbol "*TSDB-GC-VERBOSITY*" :tsdb) '(:verbose :stats)))
      (setf (sys:gsgc-switch :print) t)
      (setf (sys:gsgc-switch :stats) t)
      (setf (sys:gsgc-switch :verbose) t))
    (unless verbose
      (setf (sys:gsgc-switch :print) nil)
      (setf (sys:gsgc-switch :stats) nil)
      (setf (sys:gsgc-switch :verbose) nil))
    (setf (sys:gsgc-parameter :auto-step) tenure))

  (export '(verbose-gc)))
