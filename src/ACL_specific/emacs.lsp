(in-package :cl-user)

;;; Record source locations for things, and provide an interface to
;;; display sources in emacs

(defun record-source (type stream position)
  (declare (ignore position))
  (setf (excl:source-file type :lkb) (excl::filename stream)))

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
