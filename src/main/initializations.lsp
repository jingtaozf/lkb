(in-package :lkb)

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

#+:lispworks
(setf hcl:*packages-for-warn-on-redefinition*
  (loop
     with key = (find-package :common-lisp)
     for name in hcl:*packages-for-warn-on-redefinition*
     for package = (find-package name)
     unless (eq key package) collect name))

(defparameter *grammar-directory* nil)

#-:allegro
(defparameter *lkb-background-stream* *terminal-io*)

#+:allegro
(defparameter *lkb-background-stream* excl::*initial-terminal-io*)

(import '(enable-type-interactions disable-type-interactions))

#+(and :allegro :clim (not :mswindows)) 
(setq tk-silica::*use-clim-gc-cursor* t)

(defmacro with-package ((package) &body body)
  `(let ((*package* (find-package ,package)))
     ,@body))


(defun start-lkb ()
  #+(or :clim :common-graphics :mcl)
  (let ((building-image-p (find-symbol "*BUILDING-IMAGE-P*" :make)))
    (unless (and building-image-p (boundp building-image-p)
                 (symbol-value building-image-p))
      (let ((*package* (find-package #+:clim :clim-user #-:clim :lkb)))
        #+:clim
        (clim-user::set-up-lkb-interaction :core)
        #-:clim
        (lkb::set-up-lkb-interaction :core)))))







