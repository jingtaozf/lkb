;;; misc functions for compatability

(defun lkb-beep nil
;; for Procyon (beep *screen*)
;; doesn't seem to have an effect on eon ...
  (clim:beep))

;;; a bit silly, but I want to avoid ACL specific stuff
;;; in type code files

(defun enable-type-interactions nil
   (clim-user::enable-type-interactions))

(defun disable-type-interactions nil
  (clim-user::disable-type-interactions))

;;; MCL function that JAC uses

(defun whitespacep (char) 
  (member char '(#\space #\tab #\newline #\page #\return)))
