;;; Copyright Ann Copestake 1991-1998. All Rights Reserved.
;;; No use or redistribution without permission.

;;; Functions moved from io-paths/typeinput.lsp



(defun check-for (character istream name)
   (let ((next-char (peek-char t istream nil 'eof)))
      (unless (char= next-char character)
         (error "~%~AExpected and not found in ~A" character name))
      (read-char istream)))


(defun define-break-characters (char-list)
   (let ((temporary-readtable (copy-readtable *readtable*)))
      (dolist (break-char char-list)
         (set-macro-character break-char
            #'(lambda (stream x) (declare (ignore stream)) x)
            nil
            temporary-readtable))
      temporary-readtable))

(defun set-up-type-interactions nil
   (enable-type-interactions)
;   (initialise-type-menus)
   (close-existing-type-hierarchy-trees)
   (create-type-hierarchy-tree))


(defparameter *ordered-type-list* nil)

