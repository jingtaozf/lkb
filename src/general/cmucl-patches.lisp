;;; -*- Mode: LISP; Package: MAKE -*-

(in-package "COMMON-LISP-USER")

;;;
;;; apparently, contemporary versions of CMUCL come with a newer, incompatible
;;; version of defsystem(); make sure we always load our own (and look into the
;;; newer version at some point in the future).                (13-aug-03; oe)
;;;
(setf *features* (delete :mk-defsystem *features*))

;;;
;;; load the portable defsystem() from CMU
;;;

#-:mk-defsystem
(load
 (make-pathname :device %sys-device% :directory general-dir :name "defsystem"))

(in-package "MAKE")

(defvar %binary-dir-name% 
  (or
   #+:hppa ".huf"
   #+(and :x86 :linux) ".luf"
   #+:sparc ".suf"
   #+:alpha ".auf"
   #+(and :x86 (not :linux)) ".wuf"
   ".cuf"))
   
;;;
;;; determine the system type (in terms of hardware and os) in order to set
;;; `bin-dir' (the location of external platform-specific executables)
;;; accordingly (6-feb-96 -- oe@csli)
;;;

(defvar %system-binaries%
  #+:hppa "hppa"
  #+(and :x86 :linux) "linux"
  #+:sunos "sunos"
  #+:solaris "solaris"
  #+:alpha "osf"
  #+(and :x86 (not :linux)) "windows"
  #-(or :hppa :x86 :sunos :solaris :alpha)
  (error "~&loadup: unable to determine system type; see file ~
          `cmucl-patches.lisp'.~%"))

