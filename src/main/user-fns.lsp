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
  

 
(defun establish-linear-precedence (rule-fs)
   ;;;    A function which will order the features of a rule
   ;;;    to give (mother daughter1 ... daughtern)
   ;;;    
   ;;;  Modification - this must always give a feature
   ;;;  position for the mother - it can be NIL if
   ;;; necessary
   (sort (top-level-features-of rule-fs)
         #'(lambda (x y)
             (if (and (numberp x) (numberp y))
                 (< x y)
               (not (numberp x))))))


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
;;; window. None of their descendents (if any) will be displayed either

(defun hide-in-type-hierarchy-p (type-name)
   ;; starts with _, or ends with _[0-9][M]LE[0-9]
   (and (symbolp type-name)
      (or
         ;; graphs are pretty unreadable without glbtypes in there as well
         ;; (search "GLBTYPE" (symbol-name type-name))
         (eql (char (symbol-name type-name) 0) #\_)
         (let* ((name (symbol-name type-name))
                (end (length name))
                (cur (position #\_ name :from-end t)))
            ;; wish I had a regexp package available...
            (and cur
               (< (incf cur) end)
               (if (digit-char-p (char name cur)) (< (incf cur) end) t)
               (if (eql (char name cur) #\M) (< (incf cur) end) t)
               (if (eql (char name cur) #\L) (< (incf cur) end))
               (if (eql (char name cur) #\E) (<= (incf cur) end))
               (or (= cur end)
                   (and (digit-char-p (char name cur)) (= (incf cur) end))))))))


