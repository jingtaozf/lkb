(in-package "COMMON-LISP-USER")

;;; the following is just for the Windows version because we're trying
;;; to use the built-in project mechanism for that

#+:mswindows
(unless (find-package "MAKE") 
  (make-package "MAKE" :nicknames '("MK") :use '("COMMON-LISP")))

#+:mswindows
(pushnew :lkb *features*)

;;; if for some reason, the tty mode is desirable, use the following
;;; (pushnew :tty *features*)

;;;
;;; because ann used the term `type' the way she does well before it became a
;;; *censored* part of the *censored* common-lisp language |:-}.
;;;
;;; censorship by ann ...
;;;
#+:allegro 
(setf excl:*enable-package-locked-errors* nil)

(defparameter *grammar-directory* nil)

(defparameter *lkb-background-stream* *terminal-io*)

(import '(enable-type-interactions disable-type-interactions))

#+(and :allegro :clim) (setq tk-silica::*use-clim-gc-cursor* t)

(defpackage "MRS" (:use "COMMON-LISP" "MAKE"))
(defpackage "MAIN" (:use "COMMON-LISP" "MAKE"))

