(in-package :cl-user)

#|
(output-tdl-types)
(output-tdl-lex)
(output-tdl-rules)
(output-tdl-lrules)
|#


(defun output-tdl-types nil
  (let ((file-name 
         (ask-user-for-new-pathname "Output file?")))
    (when file-name 
      (with-open-file 
        (ostream file-name :direction :output)
        (for type-name in (reverse *ordered-type-list*)
             do
             (output-type-as-tdl (get-type-entry type-name)
                                 ostream)
             (format ostream "~%"))))))
      


(defun output-type-as-tdl (type-struct stream)
  (let ((name (type-name type-struct))
        (def (type-local-constraint type-struct))
        (parents (type-parents type-struct)))
    (format stream "~%~A :" (string-downcase name))
    (if (null def)
      (if (cdr parents)
        (progn (format stream "= ~A" (string-downcase (car parents)))
               (for parent in (cdr parents)
                    do
                    (format stream " & ~A" (string-downcase parent)))
               (format stream "."))
        (format stream "< ~A." (string-downcase (car parents))))
      (progn (format stream "= ~A" (string-downcase (car parents)))
             (for parent in (cdr parents)
                  do
                  (format stream " & ~A" (string-downcase parent)))
             (format stream " &")
             (display-dag1 def 'tdl stream)
             (format stream ".")))))

(defun output-tdl-lex nil
  (let ((file-name 
         (ask-user-for-new-pathname "Output file?")))
    (when file-name 
      (with-open-file 
        (ostream file-name :direction :output)
        (for lex-name in (reverse *ordered-lex-list*)
             do
             (output-lex-as-tdl (get-psort-entry lex-name)
                                 ostream)
             (format ostream "~%"))))))
      


(defun output-lex-as-tdl (lex-struct stream)
  (let ((name (lex-or-psort-id lex-struct))
        (def (lex-or-psort-local-fs lex-struct)))
    (format stream "~%:begin :instance.~%")
    (format stream "~%~A :=" (string-downcase name))
    (display-dag1 def 'tdl stream)
    (format stream ".")
    (format stream "~%:end :instance.~%")))



(defun output-tdl-rules nil
  (let ((file-name 
         (ask-user-for-new-pathname "Output file?")))
    (when file-name 
      (with-open-file 
        (ostream file-name :direction :output)
        (for rule-name in (reverse *ordered-rule-list*)
             do
             (output-lex-as-tdl (get-grammar-rule-entry rule-name)
                                 ostream)
             (format ostream "~%"))))))

(defun output-tdl-lrules nil
  (let ((file-name 
         (ask-user-for-new-pathname "Output file?")))
    (when file-name 
      (with-open-file 
        (ostream file-name :direction :output)
        (for rule-name in (reverse *ordered-lrule-list*)
             do
             (output-lex-as-tdl (get-lex-rule-entry rule-name)
                                 ostream)
             (format ostream "~%"))))))