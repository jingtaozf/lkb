(defun make-sense-unifications (sense-string id language)
   ;; < orth : hd > = sense
   ;; < lang > = language
  (declare (ignore id))
   (when sense-string
    (list 
       (make-unification :lhs
          (create-path-from-feature-list '(orth hd))
          :rhs (make-u-value :type sense-string))
       (make-unification :lhs
          (create-path-from-feature-list '(lang))
          :rhs (make-u-value :type language))
;;;        (make-unification :lhs
;;;           (create-path-from-feature-list '(fs-id))
;;;           :rhs (make-u-value :type id))
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
;;; Old test was for something which was a subtype of
;;; *morph-rule-type* - this tests for the type lrule-infl
  (let ((rule-type (type-of-fs (tdfs-indef (rule-full-fs rule)))))
    (or (eql rule-type 'lrule-infl)
        (subtype-p rule-type 'lrule-infl))))



(defun redundancy-rule-p (rule)
;;; a function which is used to prevent the parser 
;;; trying to apply a rule which is only used
;;; as a redundancy rule 
  (declare (ignore rule))
  nil)
             

;;; return true for types that shouldn't be displayed in type hierarchy
;;; window. None of their descendents (if any) will be displayed either

(defun hide-in-type-hierarchy-p (type-name)
  (and (symbolp type-name)
       (search "GLBTYPE" (symbol-name type-name))))

(defun make-orth-tdfs (orth)
  (let* ((unifs nil)
	(rule-orth-path (cons 'node *orth-path*))
        (tmp-orth-path rule-orth-path))
    (loop for orth-value in (split-into-words orth)
         do
         (let ((opath (create-path-from-feature-list 
                       (append tmp-orth-path *list-head*))))
           (push (make-unification :lhs opath                    
                                   :rhs
                                   (make-u-value 
                                    :type orth-value))
                 unifs)
           (setq tmp-orth-path (append tmp-orth-path *list-tail*))))
    (push (make-unification :lhs  
                            (create-path-from-feature-list 
                             (append (butlast rule-orth-path) '(last)))
                            :rhs
                            (create-path-from-feature-list 
                             tmp-orth-path))
          unifs)
    (let ((indef (process-unifications unifs)))
      (when indef
        (setf indef (create-wffs indef))
        (make-tdfs :indef indef)))))

(defun check-path-convert (check-paths)
  (let ((new-paths nil)
        (combined-paths nil))
    ;; Remove 1 or 2 prefix from paths
    (dolist (thing check-paths)
      (let ((path (car thing))
	    (count (cdr thing)))
	(if (or (eql (car path) '|1|)
                (eql (car path) '|2|))
	    (let ((rest (cdr path)))
		  (push (cons rest count)
			new-paths))
	  (push (cons path count) new-paths))))
    ;; Combine the weights for any paths that became identical after removing
    ;; the prefix
    (dolist (np new-paths)
      (let ((existing (assoc (car np) combined-paths :test #'equal)))
	(if existing 
	    (setf (cdr existing) (+ (cdr existing) (cdr np)))
	  (push np combined-paths))))
    (sort combined-paths #'> :key #'cdr)))




