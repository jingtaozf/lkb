(defun establish-linear-precedence (rule-fs)
   ;;;    A function which will order the features of a rule
   ;;;    to give (mother daughter1 ... daughtern)
   ;;;    
   ;;;  Modification - this must always give a feature
   ;;;  position for the mother - it can be NIL if
   ;;; necessary
  (mapcar #'list
   (sort (remove 'needs-affix (top-level-features-of rule-fs))
         #'(lambda (x y)
             (let ((x-num (if (numberp x) x
                            (parse-integer (string x) :junk-allowed t)))
                   (y-num (if (numberp y) y
                            (parse-integer (string y) :junk-allowed t))))
               (if (and (numberp x-num) (numberp y-num))
                 (< x-num y-num)
                 (not (numberp x-num))))))))
  

(defun spelling-change-rule-p (rule)
;;; a function which is used to prevent the parser 
;;; trying to apply a rule which affects spelling and
;;; which should therefore only be applied by the morphology
;;; system.  
;;; Old test was for something which was a subtype of
;;; *morph-rule-type* - this tests for 
;;; < NEEDS-AFFIX > = true
;;; in the rule
  (let* ((fs (rule-full-fs rule))
         (affix (get-dag-value (if (tdfs-p fs)
                                   (tdfs-indef fs)
                                                fs)
                               'needs-affix)))
    (and affix (equal (type-of-fs affix) '(true)))))

(defun redundancy-rule-p (rule)
;;; a function which is used to prevent the parser 
;;; trying to apply a rule which is only used
;;; as a redundancy rule 
;;; this version tests for 
;;; < PRODUCTIVE > = false
;;; in the rule
  (let* ((fs (rule-full-fs rule))
         (affix (get-dag-value (if (tdfs-p fs)
                                   (tdfs-indef fs)
                                   fs) 
                               'productive)))
    (and affix (equal (type-of-fs affix) '(false)))))


(defun make-orth-tdfs (orth)
  (let ((unifs nil)
        (tmp-orth-path *orth-path*))
    (loop for orth-value in (split-into-words orth)
         do
         (let ((opath (create-path-from-feature-list 
                       (append tmp-orth-path *list-head*))))
           (push (make-unification :lhs opath                    
                                   :rhs
                                   (make-u-value 
                                    :types (list orth-value)))
                 unifs)
           (setq tmp-orth-path (append tmp-orth-path *list-tail*))))
    (let ((indef (process-unifications unifs)))
      (when indef
        (setf indef (create-wffs indef))
        (make-tdfs :indef indef)))))


(defun preprocess-sentence-string (str)
  (string-trim '(#\space #\* #\.) str))

