;;; Copyright (c) 1991-2018 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see LICENSE for conditions


(in-package :lkb)

;;; misc functions for compatability

(defun lkb-beep nil
  ;; doesn't seem to have an effect on eon ...
  ;; standard-output must be a CLIM medium for this to have an effect in McCLIM
  (let (#+:mcclim
        (*standard-output* (or clim-user::*lkb-top-stream* *standard-output*)))
    (clim:beep)
    #+:mcclim (force-output *standard-output*)
    ))

;;; a bit silly, but I want to avoid ACL specific stuff
;;; in type code files

(defun enable-type-interactions nil
  (when clim-user::*lkb-top-frame*
    (clim-user::enable-type-interactions)))

(defun disable-type-interactions nil
  (when clim-user::*lkb-top-frame*
    (clim-user::disable-type-interactions)))

(defun enable-grammar-reload-interactions nil
  (when clim-user::*lkb-top-frame*
    (clim-user::enable-grammar-reload-interactions)))

#|
;;; The following is not used, and would be difficult to simulate in other lisp
;;; implementations which have different ways of managing locales.

;; macro to bind *locale*
(defmacro with-locale (locale &body body)
  `(let ((excl::*locale* ,locale)
     ,@body))
|#
