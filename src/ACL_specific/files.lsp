;;; Copyright (c) 1998-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions

(in-package :lkb)

;;; Low level stuff to get list of files

(defun get-from-shell (stream)
  (let ((strlist nil))
    (do ((ch (read-char-no-hang stream)
           (read-char-no-hang stream))) 
        ((null ch)) 
        (push ch strlist))
    (coerce 'string (nreverse strlist))))
