;;; Copyright (c) 1991-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see LICENSE for conditions


(in-package :lkb)

;;; misc functions for compatability

(defun lkb-beep nil
;; for Procyon (beep *screen*)
;; doesn't seem to have an effect on eon ...
  (clim:beep))

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

;; macro to bind *locale*
(defmacro with-locale (locale &body body)
  `(let ((excl::*locale* ,locale))
     ,@body))
