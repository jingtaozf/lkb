(in-package :common-lisp)

;;;
;;; make sure we have enough old- and new-space available for real grammars
;;;
(system:resize-areas :old (* 128 1024 1024) :new (* 256 1024 1024))

;;;
;;; the top-level directory of the DELPH-IN tree, software and grammars
;;;
(defparameter %delphin% 
  (let ((root (system:getenv "DELPHINHOME")))
    (when root (namestring (parse-namestring root)))))

(let ((excl:*enable-package-locked-errors* nil))

 (defun llkb (&optional forcep)
    (load (format nil "~a/lkb/src/general/loadup.lisp" %delphin%))
    (apply (intern "COMPILE-SYSTEM" :make) (list "lkb" :force forcep)))

 (defun ltsdb (&optional forcep)
    (pushnew :lkb *features*)
    (load (format nil "~a/lkb/src/general/loadup.lisp" %delphin%))
    (apply (intern "COMPILE-SYSTEM" :make) (list "tsdb" :force forcep)))

 (defun rsa (&optional name)
    (unless (find-package :lkb) 
      (ltsdb (eq name t))
      (sleep 5))
    (let ((script (if (and (stringp name) (find #\/ name))
                    name
                    (let ((name (if (stringp name) name "erg")))
                      (format nil "~a/~a/lkb/script" %delphin% name)))))
      (apply (intern "READ-SCRIPT-FILE-AUX" :lkb) (list script)))
    (when (eq name t)
      (funcall (intern "INDEX-FOR-GENERATOR" :lkb))))
 
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

  (export '(llkb ltsdb rsa verbose-gc)))
