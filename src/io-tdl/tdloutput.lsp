(in-package :cl-user)

#|
(output-tdl-types)
(output-tdl-lex)
(output-tdl-rules)
(output-tdl-lrules)
|#


(defun output-tdl-types (&optional file-name)
  (unless file-name 
    (setf file-name
         (ask-user-for-new-pathname "Output file?")))
  (when file-name 
    (with-open-file 
        (ostream file-name :direction :output :if-exists :supersede)
      (for type-name in (append (reverse *ordered-type-list*)
                                *ordered-glbtype-list*)
           ;; because glbtypes are effectively defined bottom up
           ;; there will be fewer warnings from PAGE if we don't reverse
           ;; them
           do
           (output-type-as-tdl (get-type-entry type-name)
                               ostream)))))
   ;; don't want this to be especially readable ...
   ;;             (format ostream "~%")))))
      


(defun output-type-as-tdl (type-struct stream)
  (let* ((name (type-name type-struct))
         (status (assoc name *tdl-status-info*))
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
             (display-dag1 def 'tdl stream)))
    (when status (format stream ", ~A" (cdr status)))
    (format stream ".")))

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


(defun expand-local-only-constraints nil
   (let ((ok t))
     (unmark-type-table)
     (determine-atomic-types)
     (for node in *type-names*
          do
          (let ((type-entry (get-type-entry node)))
            (unless 
                (expand-local-only-constraint node type-entry)
              (setf ok nil))))
     ok))
         

(defun expand-local-only-constraint (node type-entry)
  (let* ((*unify-debug-cycles* t)       ; turn on cyclic dag warning messages
         (constraint-spec (type-constraint-spec type-entry))
         (local-constraint 
          (if constraint-spec (process-unifications constraint-spec))))
    (if (and constraint-spec (null local-constraint))
        (progn
          (format t "~%Type ~A has an invalid constraint specification" node)
          nil)
     (progn
       (setf (type-local-constraint type-entry) local-constraint)
       t))))



                  