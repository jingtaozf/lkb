;;; Copyright (c) 1998-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions


(in-package :lkb)


;;; currently have no need for signature only output in TDL, so don't
;;; support it here

(defun output-type-as-tdl (name type-struct stream)
  (let* ((def (ltype-local-constraint type-struct))
         (parents (ltype-parents type-struct)))
    (format stream "~%~A :" (string-downcase name))
    (if (null def)
        (if (cdr parents)
            (progn (format stream "= ~A" (string-downcase (car parents)))
                   (loop for parent in (cdr parents)
                        do
                        (format stream " & ~A" (string-downcase parent))))
          (format stream "= ~A" (string-downcase (car parents))))
      (progn (format stream "= ~A" (string-downcase (car parents)))
             (loop for parent in (cdr parents)
                  do
                  (format stream " & ~A" (string-downcase parent)))
             (format stream " &")
             (display-dag1 def 'tdl stream nil t)))
    ;;; need fifth argument to block the first type being
    ;;; output
    (format stream ".")))



(defun output-instance-as-tdl (name lex-struct stream local-p)
  (let ((fs (if local-p (lex-entry-local-fs lex-struct)
                (tdfs-indef (lex-entry-full-fs lex-struct)))))
    ;;; assume no defaults
    (format stream "~%:begin :instance.~%")
    (format stream "~%~A :=" (string-downcase name))
    (display-dag1 fs 'tdl stream)
    (format stream ".")
    (format stream "~%:end :instance.~%")))


(defun output-derived-instance-as-tdl (str fs stream id1 id2)
  (declare (ignore str))
  (let ((indef-fs (tdfs-indef fs)))
    ;;; assume no defaults
    (format stream "~%:begin :instance.~%")
    (format stream "~%~A_~A :=" id1 id2)
    (display-dag1 indef-fs 'tdl stream)
    (format stream ".")
    (format stream "~%:end :instance.~%")))




                  