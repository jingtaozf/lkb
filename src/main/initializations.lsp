(in-package "COMMON-LISP-USER")

;;;
;;; because ann used the term `type' the way she does well before it became a
;;; bloody part of the bloody common-lisp language |:-}.
;;;
#+:allegro 
(setf excl:*enable-package-locked-errors* nil)

(defparameter *lkb-source-dir* (pathname-directory (get-sources-dir "lkb")))

(defparameter *lkb-fasl-dir* (pathname-directory (get-binaries-dir "lkb")))

(defparameter *grammar-directory* nil)

(import '(enable-type-interactions disable-type-interactions))

#+:allegro (setq tk-silica::*use-clim-gc-cursor* t)

(defpackage "MRS" (:use "COMMON-LISP" "MAKE"))
(defpackage "MAIN" (:use "COMMON-LISP" "MAKE"))

