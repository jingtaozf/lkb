(in-package "MRS")

;;; LKB specific

(defvar *mrs-record* nil)

#| 
(mrs::output-mrs-after-parse *parse-record*)
|#

(defun output-mrs-after-parse (&optional edges)
  ;;; this is most likely to be useful in an emacs window
  (when (or *mrs-to-vit* *mrs-scoping*
                 *mrs-output-p*)
    (unless edges (setf edges *parse-record*))
    (setf *mrs-record*
      (extract-mrs edges))
    (let ((*print-circle* nil))
      (for mrs in *mrs-record* 
           do
           (format t "~%~A~%" (cl-user::parse-tree-structure (car edges)))
           (setf edges (cdr edges))
           (treat-mrs mrs t)))))

(defun treat-mrs (mrs-struct simplep)
  (format t "~%~{~A ~}" cl-user::*sentence*)
  (cond (*mrs-to-vit*
         (mrs-to-vit-convert mrs-struct))
        (*mrs-scoping*
         (process-mrs-struct mrs-struct nil 10 simplep))
        (t (output-mrs mrs-struct 'simple))))

(defun process-mrs-struct (mrs-psoa &optional sentence (maximum 10) simplep)
  (when mrs-psoa
    (when sentence
      (format t "~%~A" sentence))
    (when simplep
      (output-mrs mrs-psoa 'simple))
    (when (and (boundp '*ordered-mrs-rule-list*)
               *ordered-mrs-rule-list*)
      (format t "~%Premunged form")
      (output-mrs mrs-psoa 'indexed))
    (let ((mrsstruct
            (if (and (boundp '*ordered-mrs-rule-list*)
                     *ordered-mrs-rule-list*)
                (munge-mrs-struct mrs-psoa *ordered-mrs-rule-list*)
              mrs-psoa)))
      (format t "~%Unscoped form")
      (output-mrs mrsstruct 'indexed)
      (setf *canonical-bindings* nil)
      (let ((binding-sets (make-scoped-mrs mrsstruct)))
        (if binding-sets
            (progn 
              (format t "~%~A scoped form(s)" (length binding-sets))
              (when (and maximum (> (length binding-sets) maximum)
                         (format t " only printing first ~A" maximum)))
              (format t "~%"))
          (format t "~%WARNING: No valid scopes~%"))
        (for binding in (subseq binding-sets 0 
                                (min (length binding-sets) 10))
             do
             (setf *canonical-bindings* (canonical-bindings binding))
             (output-connected-mrs mrsstruct 'indexed)
             (output-scoped-mrs mrsstruct))))))


