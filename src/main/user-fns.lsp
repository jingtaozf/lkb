;;; User defined functions - from old globals file


(defun make-sense-unifications (sense-string id language)
   ;; < orth : hd > = sense
   ;; < lang > = language
  (declare (ignore sense-string id language))
   nil)

(defparameter *sense-unif-fn* #'make-sense-unifications)


;;;   (when sense-string
;;;    (list 
;;;       (make-unification :lhs
;;;          (create-path-from-feature-list '(orth hd))
;;;          :rhs (make-u-value :types (list sense-string)))
;;;       (make-unification :lhs
;;;          (create-path-from-feature-list '(lang))
;;;          :rhs (make-u-value :types (list language)))
;;;        (make-unification :lhs
;;;           (create-path-from-feature-list '(fs-id))
;;;           :rhs (make-u-value :types (list id)))
;;;       )))


(defun make-orth-tdfs (orth)
  (let ((unifs nil)
        (tmp-orth-path *orth-path*))
    (for orth-value in (split-into-words orth)
         do
         (let ((opath (create-path-from-feature-list 
                       (append tmp-orth-path *list-head*))))
           (push (make-unification :lhs opath                    
                                   :rhs
                                   (make-u-value 
                                    :types (list orth-value)))
                 unifs)
           (setq tmp-orth-path (append tmp-orth-path *list-tail*))))
    (push (make-unification :lhs
                            (create-path-from-feature-list tmp-orth-path)
                            :rhs 
                            (make-u-value :types (list *empty-list-type*)))
          unifs)
    (let ((indef (process-unifications unifs)))
      (when indef
        (setf indef (create-wffs indef))
        (make-tdfs :indef indef)))))

  

(defun establish-linear-precedence (rule-fs)
   ;;;    A function which will order the features of a rule
   ;;;    to give (mother daughter1 ... daughtern)
   ;;;    
   ;;;  Modification - this must always give a feature
   ;;;  position for the mother - it can be NIL if
   ;;; necessary
   (sort (remove 'needs-affix (top-level-features-of rule-fs))
         #'(lambda (x y)
             (let ((x-num (if (numberp x) x
                            (parse-integer (string x) :junk-allowed t)))
                   (y-num (if (numberp y) y
                            (parse-integer (string y) :junk-allowed t))))
               (if (and (numberp x-num) (numberp y-num))
                 (< x-num y-num)
                 (not (numberp x-num)))))))


(defun spelling-change-rule-p (rule)
;;; a function which is used to prevent the parser 
;;; trying to apply a rule which affects spelling and
;;; which should therefore only be applied by the morphology
;;; system.  
;;; Old test was for something which was a subtype of
;;; *morph-rule-type* - this tests for 
;;; < NEEDS-AFFIX > = true
;;; in the rule
  (let ((affix (get-dag-value (rule-full-fs rule) 'needs-affix)))
    (and affix (equal (type-of-fs affix) '(true)))))

(defun redundancy-rule-p (rule)
;;; a function which is used to prevent the parser 
;;; trying to apply a rule which is only used
;;; as a redundancy rule 
;;; this version tests for 
;;; < PRODUCTIVE > = false
;;; in the rule
  (let ((affix (get-dag-value (rule-full-fs rule) 'productive)))
    (and affix (equal (type-of-fs affix) '(false)))))

;;; return true for types that shouldn't be displayed in type hierarchy
;;; window. Descendents (if any) will be displayed, i.e. non-displayed
;;; types are effectively spliced out

(defun hide-in-type-hierarchy-p (type-name)
  (declare (ignore type-name))
  nil)

