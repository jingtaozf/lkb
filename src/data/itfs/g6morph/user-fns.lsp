(defun establish-linear-precedence (rule-fs)
   ;;;    A function which will order the features of a rule
   ;;;    to give (mother daughter1 daughter2 daughter3)
   ;;;    will not work for more than three daughters, though
   ;;;    extension should be obvious
  (let* ((mother NIL)
         (daughter1 (get-value-at-end-of rule-fs '(ARGS FIRST)))
         (daughter2 (get-value-at-end-of rule-fs '(ARGS REST FIRST)))
         (daughter3 (get-value-at-end-of rule-fs '(ARGS REST REST
FIRST))))
    (declare (ignore mother))
    (unless daughter1 
      (cerror "Ignore it" "Rule without daughter"))
    (append (list nil '(ARGS FIRST))
            (if daughter2 
                (list '(ARGS REST FIRST)))
            (if daughter3 
                (if daughter2 
                    (list '(ARGS REST REST FIRST)))))))


(defun spelling-change-rule-p (rule)
  (let ((rule-type (type-of-fs (tdfs-indef (rule-full-fs rule)))))
    (when (or (eql rule-type 'word)
              (subtype-p rule-type 'word)
              (eql rule-type 'lexeme)
              (subtype-p rule-type 'lexeme))
      (let* ((mother (tdfs-indef (rule-full-fs rule)))
             (morth (existing-dag-at-end-of mother *orth-path*))
             (daughter (existing-dag-at-end-of mother '(ARGS FIRST)))
             (dorth (existing-dag-at-end-of daughter *orth-path*)))
        (not (eq morth dorth))))))
  
(defun redundancy-rule-p (rule)
;;; a function which is used to prevent the parser 
;;; trying to apply a rule which is only used
;;; as a redundancy rule 
  (declare (ignore rule))
  nil)
             
;;; return true for types that shouldn't be displayed in type hierarchy
;;; window. None of their descendents (if any) will be displayed either

(defun hide-in-type-hierarchy-p (type-name)
  (declare (ignore type-name))
  nil)

(defun make-orth-tdfs (orth)
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



