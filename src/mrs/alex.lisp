(in-package :mrs)

(setf *alex-munge* t)
(setf *mrs-to-vit* nil)

(setf cl-user::*do-something-with-parse* 'show-mrs)

(defun alex-munge (mrs-psoa)
  (when mrs-psoa
    (unless *ordered-mrs-rule-list*
      (error "~%No munging rules defined"))
    (let* ((mrsstruct
           (if (boundp '*ordered-mrs-rule-list*)
               (munge-mrs-struct mrs-psoa *ordered-mrs-rule-list*)
             mrs-psoa))
           (binding-sets (make-scoped-mrs mrs-psoa)))
      (setf *canonical-bindings* nil)
      (format t "~%Premunged form")
      (output-mrs mrs-psoa 'indexed)
      (format t "~%Unscoped form")
      (output-mrs mrsstruct 'indexed)
      (if binding-sets
          (format t "~%~A scoped form(s) ~A~%" (length binding-sets)
                  (if (> (length binding-sets) 10)
                      "only printing first 10" 
                    ""))
        (format t "~%WARNING: No valid scopes~%"))
      (for binding in (subseq binding-sets 0 
                              (min (length binding-sets) 10))
           do
           (setf *canonical-bindings* (canonical-bindings binding))
           (output-connected-mrs mrsstruct 'indexed)
           (output-scoped-mrs mrsstruct)))))


#|

(in-package :cl-user)

(read-mrs-rule-file-aux "~aac/grammar/data/alex.rules")

|#