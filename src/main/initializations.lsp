(in-package "COMMON-LISP-USER")

;;;
;;; because ann used the term `type' the way she does well before it became a
;;; *censored* part of the *censored* common-lisp language |:-}.
;;;
;;; censorship by ann ...
;;;
#+:allegro 
(setf excl:*enable-package-locked-errors* nil)

(defparameter *lkb-source-dir* (pathname-directory (get-sources-dir "lkb")))

(defparameter *lkb-fasl-dir* (pathname-directory (get-binaries-dir "lkb")))

(defparameter *grammar-directory* nil)

(import '(enable-type-interactions disable-type-interactions))

#+(and :allegro :clim) (setq tk-silica::*use-clim-gc-cursor* t)

(defpackage "MRS" (:use "COMMON-LISP" "MAKE"))
(defpackage "MAIN" (:use "COMMON-LISP" "MAKE"))

