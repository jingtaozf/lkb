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
    (unless (and daughter1 (not (eql daughter1 'no-way-through)))
      (cerror "Ignore it" "Rule without daughter"))
    (append (list nil '(ARGS FIRST))
            (if (and daughter2 (not (eql daughter2 'no-way-through)))
                (list '(ARGS REST FIRST)))
            (if (and daughter3 (not (eql daughter3 'no-way-through)))
                (if (and daughter2 (not (eql daughter2 'no-way-through)))
                    (list '(ARGS REST REST FIRST)))))))



(defun spelling-change-rule-p (rule)
  (let ((rule-type (type-of-fs (tdfs-indef (rule-full-fs rule)))))
    (when (or (eql rule-type 'word)
              (subtype-p rule-type 'word))
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



