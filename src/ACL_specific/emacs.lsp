(in-package :cl-user)

;;; Record source locations for things, and provide an interface to
;;; display sources in emacs

(defun edit-source (thing)
  (let ((source (ignore-errors (excl:source-file thing :lkb))))
    (when source
      (edit-file thing source))))

(defun source-available-p (thing)
  (and (lep:lep-is-running)
       (ignore-errors (excl:source-file thing :lkb))))

(defun edit-file (thing file)
  (when (lep:lep-is-running)
    (lep::eval-in-emacs (format nil "(find-tdl-definition \"~a\" \"~a\")"
				thing file))))

(defun redefine-type (definition)
  (let ((*readtable* (make-tdl-break-table))
	(*standard-output* clim-user::*lkb-top-stream*))
    (with-input-from-string (istream definition)
      (read-tdl-type-stream istream t)))
  (let ((*standard-output clim-user:*lkb-top-stream*))
    (when (patch-type-table) 
      (canonicalise-feature-order)           
      (set-up-type-interactions)
      t)))

