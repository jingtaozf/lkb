;;; -*- Mode: LISP; Package: MAKE -*-

(in-package "COMMON-LISP-USER")

;;;
;;; load the portable defsystem() from CMU
;;;

#-:mk-defsystem
(load
 (make-pathname :device %sys-device% :directory general-dir :name "defsystem"))

(in-package "MAKE")

(defvar %BINARY-DIR-NAME% 
    (remove-if-not #'(lambda (x) (or (alphanumericp x) (char= x #\-)
                                     (char= x #\_)))
                   (substitute #\_ #\space
                               (concatenate 'string 
                                 (or (machine-type) "") "-" 
                                 (or (software-type) "") "-"
                                 (or (lisp-implementation-version) "")))))

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

