;;; -*- Mode: LISP; Package: MAKE -*-

(in-package "COMMON-LISP-USER")

;;;
;;; load the portable defsystem() from CMU
;;;

#-:mk-defsystem
(load
 (make-pathname :device %sys-device% :directory general-dir :name "defsystem"))

(in-package "MAKE")

;;; Set up binary path names based on information about this implementation,
;;; and add a hook to defsystem so that it creates the necessary directories
;;; as it goes.  This eliminates the need to create a .?asl tree by hand and
;;; it ought to be completely portable.

(defvar %BINARY-DIR-NAME% 
  (concatenate 'string "." (machine-type) "-" (software-type) "-"
	       (lisp-implementation-version)))

(defun compile-file-in-dir (source &key output-file #+CMU error-file)
  (progn
    (ensure-directories-exist output-file :verbose t)
    (compile-file source :output-file output-file 
		  #+CMU :error-file #+CMU error-file)))

(define-language :lisp
  :compiler #'compile-file-in-dir
  :loader #'load
  :source-extension (car *filename-extensions*)
  :binary-extension (cdr *filename-extensions*))

;;;
;;; determine the system type (in terms of hardware and os) in order to set
;;; `bin-dir' (the location of external platform-specific executables)
;;; accordingly (6-feb-96 -- oe@csli)
;;;

(defvar %system-binaries%
  #+(or :prism :hppa) "hppa"
  #+:linux86 "linux"
  #+:sunos4 "sunos"
  #+(and :sun :svr4) "solaris"
  #+:alpha "osf"
  #+:mswindows "mswindows"
  #-(or :prism :hppa :sunos4 (and :sun :svr4) :alpha :linux86 :mswindows)
  (error "~&loadup: unable to determine system type; see file ~
          `allegro-patches.lisp'.~%"))

