;;; Copyright (c) 1998-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions


(in-package :lkb)

;;; Record source locations for things, and provide an interface to
;;; display sources in emacs

(defun record-source (type stream position)
   (declare (ignore position))
   (setf (excl:source-file type :lkb) (excl::filename stream)))

(defun edit-source (thing)
  (let ((source (ignore-errors (excl:source-file thing :lkb))))
    (when source
      (edit-file thing source))))

#-:runtime-standard
(defun source-available-p (thing)
  (and (lep:lep-is-running)
       (ignore-errors (excl:source-file thing :lkb))))

#+:runtime-standard
(defun source-available-p (thing)
  (declare (ignore thing))
  nil)

#-:runtime-standard
(defun edit-file (thing file)
  (when (lep:lep-is-running)
    (lep::eval-in-emacs (format nil "(find-tdl-definition \"~a\" \"~a\")"
				thing file))))

(defun redefine-type (definition)
  (let ((*readtable* (make-tdl-break-table)))
    (with-input-from-string (istream definition)
      (read-tdl-type-stream istream t)))
    (when (patch-type-table) 
      (canonicalise-feature-order)           
      (set-up-type-interactions)
      t))
