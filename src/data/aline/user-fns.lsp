(defun make-sense-unifications (sense-string id language)
   ;; < orth : hd > = sense
   ;; < lang > = language
  (declare (ignore id))
   (when sense-string
    (list 
       (make-unification :lhs
          (create-path-from-feature-list '(orth hd))
          :rhs (make-u-value :types (list sense-string)))
       (make-unification :lhs
          (create-path-from-feature-list '(lang))
          :rhs (make-u-value :types (list language)))
;;;        (make-unification :lhs
;;;           (create-path-from-feature-list '(fs-id))
;;;           :rhs (make-u-value :types (list id)))
       )))


(defun establish-linear-precedence (rule-fs)
   ;;;    A function which will order the features of a rule
   ;;;    to give (mother daughter1 ... daughtern)
   ;;;    
   ;;;  Modification - this must always give a feature
   ;;;  position for the mother - it can be NIL if
   ;;; necessary
  (cons nil
        (mapcar #'list
                (sort (remove 'node 
                              (remove 'needs-affix 
                                      (top-level-features-of rule-fs)))
                      #'(lambda (x y)
                          (let ((x-num 
                                 (if (numberp x) x
                                   (parse-integer (string x) :junk-allowed t)))
                                (y-num (if (numberp y) y
                                         (parse-integer (string y) 
                                                        :junk-allowed t))))
                            (if (and (numberp x-num) (numberp y-num))
                                (< x-num y-num)
                              (not (numberp x-num)))))))))
  

(defun spelling-change-rule-p (rule)
;;; a function which is used to prevent the parser 
;;; trying to apply a rule which affects spelling and
;;; which should therefore only be applied by the morphology
;;; system.  
  nil)

  
(defun redundancy-rule-p (rule)
;;; a function which is used to prevent the parser 
;;; trying to apply a rule which is only used
;;; as a redundancy rule 
  nil)

