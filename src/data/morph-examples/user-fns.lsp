;;; Common user-fns file for all ESSLLI grammars

(defun establish-linear-precedence (rule-fs)
   ;;;    A function which will order the features of a rule
   ;;;    to give (mother daughter1 ... daughtern)
   ;;;
   ;;;  Modification - this must always give a feature
   ;;;  position for the mother - it can be NIL if
   ;;; necessary
  (let* ((mother NIL)
         (daughter1 (get-value-at-end-of rule-fs '(ARGS FIRST)))
         (daughter2 (get-value-at-end-of rule-fs '(ARGS REST FIRST)))
         (daughter3 (get-value-at-end-of rule-fs '(ARGS REST REST FIRST))))
    (declare (ignore mother))
    (unless daughter1 
      (cerror "Ignore it" "Rule without daughter"))
    (append (list nil '(ARGS FIRST))
            (if daughter2 
                (list '(ARGS REST FIRST)))
            (if daughter3 
                (if daughter2 
                    (list '(ARGS REST REST FIRST)))))))

;;; return true for types that shouldn't be displayed in type hierarchy
;;; window. None of their descendents (if any) will be displayed either

(defun hide-in-type-hierarchy-p (type-name)
  (declare (ignore type-name))
  nil)



(defun make-orth-tdfs (orth)
  ;;; this version should work for grammars where the value
  ;;; of the orthography is simply a string
  (let ((indef (process-unifications 
                (list (make-unification :lhs
                                        (create-path-from-feature-list 
                                         *orth-path*)                    
                                        :rhs
                                        (make-u-value 
                                         :type orth))))))
      (when indef
        (setf indef (create-wffs indef))
        (when indef
          (make-tdfs :indef indef)))))

