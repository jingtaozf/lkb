
;;; Low level stuff to get list of files

(defun get-from-shell (stream)
  (let ((strlist nil))
    (do ((ch (read-char-no-hang stream)
           (read-char-no-hang stream))) 
        ((null ch)) 
        (push ch strlist))
    (coerce 'string (nreverse strlist))))
