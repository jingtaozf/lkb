(in-package :cl-user)


;;; currently have no need for signature only output in TDL, so don't
;;; support it here

(defun output-type-as-tdl (name type-struct stream)
  (let* ((status (assoc name *tdl-status-info*))
         (def (type-local-constraint type-struct))
         (parents (type-parents type-struct)))
    (format stream "~%~A :" (string-downcase name))
    (if (null def)
        (if (cdr parents)
            (progn (format stream "= ~A" (string-downcase (car parents)))
                   (for parent in (cdr parents)
                        do
                        (format stream " & ~A" (string-downcase parent))))
          (format stream "< ~A" (string-downcase (car parents))))
      (progn (format stream "= ~A" (string-downcase (car parents)))
             (for parent in (cdr parents)
                  do
                  (format stream " & ~A" (string-downcase parent)))
             (format stream " &")
             (display-dag1 def 'tdl stream nil t)))
    ;;; need fifth argument to block the first type being
    ;;; output
    (when status (format stream ", ~A" (cdr status)))
    (format stream ".")))



(defun output-instance-as-tdl (name lex-struct stream local-p)
  (let ((fs (if local-p (lex-or-psort-local-fs lex-struct)
                (tdfs-indef (lex-or-psort-full-fs lex-struct)))))
    ;;; assume no defaults
    (format stream "~%:begin :instance.~%")
    (format stream "~%~A :=" (string-downcase name))
    (display-dag1 fs 'tdl stream)
    (format stream ".")
    (format stream "~%:end :instance.~%")))


(defun output-derived-instance-as-tdl (str fs stream id1 id2)
  (let ((indef-fs (tdfs-indef fs)))
    ;;; assume no defaults
    (format stream "~%:begin :instance.~%")
    (format stream "~%~A_~A :=" id1 id2)
    (display-dag1 indef-fs 'tdl stream)
    (format stream ".")
    (format stream "~%:end :instance.~%")))




                  