(in-package "MRS")

;;; LKB specific

#| 
(mrs::output-mrs-after-parse *parse-record*)
|#

(defun output-mrs-after-parse (&optional edges stream)
  ;;; for ACL this is most likely to be useful in an emacs window
  ;;; the need to use *lkb-background-stream* is because 
  ;;; of the complexity with background streams in ACL
  ;;; it's set in topmenu.lsp
  (when (or *mrs-to-vit* *mrs-scoping*
            *mrs-output-p*)
    (unless stream (setf stream cl-user::*lkb-background-stream*))
    (unless edges (setf edges *parse-record*))
    (let ((*print-circle* nil))
      (for edge in edges 
           do
           (let ((mrs (extract-mrs edge)))
             (format stream "~%Edge number ~A" 
                     (cl-user::edge-id edge))
             (format stream "~%~A~%" 
                     (cl-user::parse-tree-structure edge))
             (treat-mrs mrs t stream))))))

(defvar *mrs-debug* nil)

(defun treat-mrs (mrs-struct simplep stream)
  (format stream "~%~A " cl-user::*sentence*)
  (setf *mrs-debug* mrs-struct)
  (cond (*mrs-to-vit*
         (mrs-to-vit-convert mrs-struct t stream))
        (*mrs-scoping*
         (process-mrs-struct mrs-struct nil 10 simplep stream))
        (t (output-mrs1 mrs-struct 'simple stream))))

(defun process-mrs-struct (mrs-psoa sentence maximum simplep stream)
  (when mrs-psoa
    (when sentence
      (format stream "~%~A" sentence))
    (when simplep
      (output-mrs1 mrs-psoa 'simple stream))
    (when (and (boundp '*ordered-mrs-rule-list*)
               *ordered-mrs-rule-list*)
      (format stream "~%Premunged form")
      (output-mrs1 mrs-psoa 'indexed stream))
    (let ((mrsstruct
            (if (and (boundp '*ordered-mrs-rule-list*)
                     *ordered-mrs-rule-list*)
                (munge-mrs-struct mrs-psoa *ordered-mrs-rule-list*)
              mrs-psoa)))
      (format stream "~%Unscoped form")
      (output-mrs1 mrsstruct 'indexed stream)
      (setf *canonical-bindings* nil)
      (let ((binding-sets (make-scoped-mrs mrsstruct)))
        (show-some-scoped-structures mrsstruct binding-sets
                                     stream maximum)))))



