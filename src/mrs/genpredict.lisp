(in-package "MRS")

;;; Variant of munging rules for generation heuristics for
;;; filtering items with no semantics

;;; (read-mrs-rule-file-aux "~aac/grammar/data/genrules.mrs" t)


(defun predict-for-gen nil
  (format t "~%~{~A ~}" cl-user::*sentence*)
  (dolist (mrs-struct (extract-mrs cl-user::*parse-record*))
    (when mrs-struct
      (unless *ordered-mrs-rule-list*
        (error "~%No heuristic rules defined"))
      (format t "~%~S" 
        (genpredict-mrs-struct mrs-struct *ordered-mrs-rule-list*)))))


(defun genpredict-mrs-struct (mrsstruct rules)
  ;;; takes an mrs structure and a set of generation rules
  ;;; using the output of the rules to predict null semantic
  ;;; items for generation
  ;;; Rules are applied in order and are not applied recursively
  (if rules
      (progn
        (setf *original-variables* nil)
        (let ((null-ids nil))
          (dolist (rule rules)
            (let ((new-results 
                   (match-mrs-rule 
                    mrsstruct  
                    (mrs-munge-rule-input-condition rule))))
              (when new-results
                (pushnew (mrs-munge-rule-output-spec 
                          rule) null-ids))))
          null-ids))
    ;;; NB - we do only want one instance of an id 
    ;;; despite the fact that more than one may be required in 
    ;;; a sentence.  
    (progn
      (format t "~%Warning: no generator prediction rules have been set")
      *empty-semantics-lexical-entries*)))

  


;;; *************** Rule input ****************

#|

(read-mrs-rule-file-aux "~aac/grammar/data/genrules.mrs" t)

|#

(defun construct-gen-rule-from-fs (id fs funny-unifs)
  ;;; input and output are constructed using construct-mrs
  ;;; with a given variable-generator
  (declare (ignore id))
  (when funny-unifs
    (error "Funny unifs not expected in generator rules"))
  (let ((output-fs (path-value fs *mrs-rule-output-path*))
        (condition-fs (path-value fs *mrs-rule-condition-path*)))
      (if (and condition-fs output-fs)
          (let* ((variable-generator (create-variable-generator 1000))
                 (*psoa-rh-cons-path* `( ,(vsym "H-CONS")  ,(vsym "LIST")))
                 (*psoa-liszt-path* `( ,(vsym "LISZT")  ,(vsym "LIST")))
                 (output-spec (construct-output-id output-fs))
                 (condition-spec 
                      (construct-mrs condition-fs variable-generator)))
            (when (and condition-spec output-spec)
              (make-mrs-munge-rule 
               :output-spec output-spec
               :input-condition condition-spec))))))

(defun construct-output-id (fs)
  (let ((res (fs-type fs)))
    (when (listp res) (setf res (car res)))
    (if (stringp res)
        (intern (string-upcase res) :cl-user))))

        


