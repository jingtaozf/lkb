;;; Copyright Ann Copestake 1992. All Rights Reserved.
;;; No use or redistribution without permission.
;;; 
;;; Ann Copestake
;;; Computer Laboratory, University of Cambridge
;;; Pembroke Street
;;; Cambridge, UK


;;; generalisation, equality and subsumption of fs

;;; Jan 1995 - made equal-wffs-p and subsumes-wffs-p work 
;;; on non well typed fs

(defun mark-dag-with-paths (real-dag current-path) 
    ;;; mark nodes with lists of paths 
    ;;; - generalisation will be reentrant in those paths that
    ;;; both the originals were reentrant in
         (push (reverse current-path) (dag-visit real-dag))
         (for label in (top-level-features-of real-dag)
            do 
            (mark-dag-with-paths (get-dag-value real-dag label)
               (cons label current-path))))


#|
;;; !!! Not yet tested properly with quasi-destructive unification

(defvar *reentrant-sets* nil)

(defun generalise (dag1 dag2)
   (invalidate-visit-marks)
   (with-unification-context (ignore)
      (let ((result-dag (create-dag)))
         (mark-dag-with-paths dag1 nil)
         (mark-dag-with-paths dag2 nil)
         (setf *reentrant-sets* nil)
         (generalise-wffs dag1 dag2 result-dag)
         (for reentrant-set in *reentrant-sets*
            do 
            (let ((first-path 
                     (create-path-from-feature-list (car reentrant-set))))
               (for other-path in (cdr reentrant-set)
                  do
                  (unify-paths first-path result-dag 
                     (create-path-from-feature-list other-path)
                     result-dag))))
         (copy-dag result-dag))))


(defun generalise-wffs (dag1 dag2 result-dag)
   ;;; new dag is created by side effects -
   ;;; a list of reentrancy specs is created but not used
   ;;; until the end
   (let* ((real-dag1 (deref-dag dag1))
         (real-dag2 (deref-dag dag2))
         (real-result-dag (deref-dag result-dag))
         (reentrant-labels 
            (intersection 
               (dag-visit real-dag1) (dag-visit real-dag2) 
               :test #'equal)))
      (when (cdr reentrant-labels) 
         (pushnew reentrant-labels *reentrant-sets* :test #'equalp))
      (let*
         ((lcsupertype 
               (find-lcsupertype 
                  (type-of-fs real-dag1)
                  (type-of-fs real-dag2)))
            (constraint (if (and (atom lcsupertype) (not (stringp lcsupertype)))
                  (constraint-of lcsupertype))))
         (set-type real-result-dag lcsupertype)
         (when constraint
            (unify-dags real-result-dag 
               (copy-dag-completely constraint)))
         (unless (or (is-atomic real-dag1) (is-atomic real-dag2))
            (generalise-subparts real-dag1 real-dag2 real-result-dag)))))


(defun generalise-subparts (dag1 dag2 result-dag)
   (for label in (top-level-features-of result-dag)
      do
      (let ((existing-dag1 (get-dag-value dag1 label))
            (existing-dag2 (get-dag-value dag2 label))
            (current-result-dag (get-dag-value result-dag label)))
         (generalise-wffs existing-dag1 existing-dag2 current-result-dag))))


(defun find-lcsupertype (type1 type2)
   (cond ((and (not (listp type1)) (not (listp type2))) 
         (least-common-supertype type1 type2))
      ((not (listp type1)) 
         (if 
            (atomic-type-p type1)
            (lcslists (list type1) type2)
            (reduce #'least-common-supertype (cons type1 type2))))
      ((not (listp type2)) 
         (if 
            (atomic-type-p type2)
            (lcslists type1 (list type2))
            (reduce #'least-common-supertype (cons type2 type1))))
      (t (lcslists type1 type2))))



(defun lcslists (type1 type2)
   ;;; disjunctions are taken into consideration
   ;;; e.g. if 
   ;;; language (top) (OR english dutch italian spanish)
   ;;; 
   ;;; (lcslists '(english) '(dutch))
   ;;; will return '(dutch english)
   ;;; only atomic types may be disjuncts - we check for this above
   (let* ((int-list (union type1 type2 :test #'equal)) ; possibly strings
         (dis-list
            (do* ((done nil (cons initial done))
                  (initial (car int-list) 
                     (car (set-difference new-int-list done :test #'equal)))
                  (new-int-list 
                     (remove-descendants int-list initial)
                     (remove-descendants new-int-list initial)))
               ((null (set-difference new-int-list (cons initial done) :test #'equal))
                  new-int-list)))
         (parent-list 
            (remove-duplicates 
               (mapcar #'(lambda (distype) 
                     (if (stringp distype)
                        (list *string-type*)
                        (retrieve-parents distype)))
                  dis-list) :test #'equal)))
      (if (eql (length dis-list) 1)
         dis-list
         (if (eql (length parent-list) 1)
            (let ((common-parent (car parent-list)))
               (if (eql (length common-parent) 1)
                  (if (or (string-type-p (car common-parent))
                     (set-difference (retrieve-daughters (car common-parent)) 
                        dis-list :test #'equal))
                     dis-list common-parent)
                  (error "~%Multiple parents for disjuncts ~A" dis-list)))
            dis-list))))


(defun remove-descendants (type-list type)
   ;;; takes a list of types and returns a list only containg those that
   ;;; are not descendants of the given type.  Complicated by need to check
   ;;; for strings
   (let ((descendants (unless (stringp type) (retrieve-descendants type))))
      (if (string-type-p type)
         (set-difference (remove-if #'stringp type-list) 
            descendants)
         (set-difference type-list descendants))))

(defun remove-ancestors (int-list)
   (do* ((done nil (cons initial done))
         (initial (car int-list) (car (set-difference new-int-list done)))
         (new-int-list (set-difference int-list (retrieve-ancestors initial))
            (set-difference new-int-list (retrieve-ancestors initial))))
      ((null (set-difference new-int-list (cons initial done)))
            new-int-list)))



;;; equality

(defun dag-equal-p (dag1 dag2)
   (invalidate-visit-marks)
   (mark-dag-with-paths dag1 nil)
   (mark-dag-with-paths dag2 nil)
   (equal-wffs-p dag1 dag2))
        
(defun equal-wffs-p (dag1 dag2)
   (let* ((real-dag1 (follow-pointers dag1))
         (real-dag2 (follow-pointers dag2))
         (reentrancy-difference 
            (or (set-difference 
                  (dag-visit real-dag1) (dag-visit real-dag2) 
                  :test #'equal)
               (set-difference 
                  (dag-visit real-dag2) (dag-visit real-dag1) 
                  :test #'equal))))
      (unless reentrancy-difference
         (if (equal-types (type-of-fs real-dag1)
               (type-of-fs real-dag2))
            (or (is-atomic real-dag1)
               (and (null (set-exclusive-or (top-level-features-of real-dag1)
                        (top-level-features-of real-dag2)))
                  (for label in (top-level-features-of real-dag1)
                     all-satisfy
                     (let ((existing-dag1 (get-dag-value real-dag1 label))
                           (existing-dag2 (get-dag-value real-dag2 label)))
                        (if existing-dag2 
                           (equal-wffs-p existing-dag1 existing-dag2))))))))))


;;; Both this function, and subsume-type, below, implicitly assume that the 
;;; type hierarchy may be incomplete.

(defun equal-types (type1 type2)
   (cond 
      ((and (not (listp type1)) (not (listp type2))) 
         (equal type1 type2))
      ((not (listp type1)) (and (eql (length type2) 1)
            (equal type1 (car type2))))
      ((not (listp type2)) (and (eql (length type1) 1)
            (equal type2 (car type1))))
      (t (not (or (set-difference 
                  type1 type2
                  :test #'equal)
               (set-difference 
                  type2 type1
                  :test #'equal))))))

|#

;;; subsumption - true if dag1 subsumes dag2

(defun dag-subsumes-p (dag1 dag2)
   (invalidate-visit-marks)
   (mark-dag-with-paths dag1 nil)
   (mark-dag-with-paths dag2 nil)
   (subsume-wffs-p dag1 dag2))
        
(defun subsume-wffs-p (real-dag1 real-dag2)
   ;; dag1 only subsumes dag2 if there are no extra reentrancies
   ;; in dag1
   (let (
         (reentrancy-difference
            (set-difference 
               (dag-visit real-dag1) (dag-visit real-dag2) 
               :test #'equal)))
      (unless reentrancy-difference
         (if (subsume-types (type-of-fs real-dag1)
               (type-of-fs real-dag2))
            (or (is-atomic real-dag1)
               (for label in (top-level-features-of real-dag1)
                  all-satisfy
                  (let ((existing-dag1 (get-dag-value real-dag1 label))
                        (existing-dag2 (get-dag-value real-dag2 label)))
                        (and existing-dag2
                           (subsume-wffs-p existing-dag1 existing-dag2)))))))))

(defun subsume-types (type1 type2)
  ;;; make this long-winded in the hope of improving efficiency
  (if (listp type1) 
    (if (not (cdr type1))
      (if (listp type2)
        (if (not (cdr type2))
          (subtype-or-equal (car type2) (car type1)) ; both single element lists
          (every #'(lambda (type-el2)
                     (subtype-or-equal type-el2 (car type1)))
                 type2))               ; type1 single element list
                                       ; type2 multielement list
        (subtype-or-equal type2 (car type1))) ; type1 single element list
                                       ; type2 atomic
      (if (listp type2)
        (if (not (cdr type2))
          (some #'(lambda (type-el1)
                    (subtype-or-equal (car type2) type-el1))
                type1)                 ; type1 multielement 
                                       ; type2 single element
          (every #'(lambda (type-el2)
                      (some 
                         #'(lambda (type-el1)
                              (subtype-or-equal type-el2 type-el1))
                           type1))
                        type2))        ; both multielement
        (some #'(lambda (type-el1)
                  (subtype-or-equal type2 type-el1))
              type1)))                 ; type1 multielement type2 atomic
    (if (listp type2)
      (if (not (cdr type2))
        (subtype-or-equal (car type2) type1) ; type1 atomic type2 singleton
        (every #'(lambda (type-el2)
                   (subtype-or-equal type-el2 type1))
               type2))               ; type1 atomic
                                     ; type2 multielement list
      (subtype-or-equal type2 type1)) ; type1 atomic
                               ; type2 atomic               
         ))
 
            
(defun subtype-or-equal (type1 type2)
   (or (equal type1 type2)
      (subtype-p type1 type2)))


#|
;;; For LDB indexing need to create minimal path value equations

(defvar *canonical-paths* nil)

(defun canonicalise-fs (fs)
   (setf *canonical-paths* nil)
   (canonicalise-fs-aux fs nil nil)
   (nreverse *canonical-paths*))

      
(defun canonicalise-fs-aux (fs predictions path)
   ;; predictions is a list of items of the form
   ;; (list-of-features type)
   ;; e.g. ((NIL NE-ORTH) ((HD-ORTH) WORD-ORTH) ((TL-ORTH) E-ORTH))
   ;; where these are the values for features predicted by the types
   ;; met on this path so far
   ;;
   ;; path is the path so far, in reverse order
   (let* ((real-dag (follow-pointers fs))
         (type (type-of-fs real-dag)))
      (unless 
         (member (if (listp type) type (list type)) 
            (for pred in predictions
               filter 
               (if (null (car pred)) (cdr pred))) :test #'equal)
         (push (make-unification 
               :lhs (create-path-from-feature-list (reverse path))
               :rhs (make-u-value :types 
                  (if (listp type) type (list type))))
            *canonical-paths*))
      (unless
         (is-atomic real-dag)
         (let ((new-predictions 
                  (append
                     (extract-paths-for-canonical-rep (constraint-of type) 
                        nil)
                     predictions)))
            (for label in (top-level-features-of real-dag)
               do
               (let ((new-dag (get-dag-value real-dag label)))
                  (canonicalise-fs-aux new-dag 
                     (update-predictions new-predictions label)
                     (cons label path))))))))

(defun update-predictions (predictions feature)
   (for pred in predictions
      filter
      (if (eql (caar pred) feature)
         (cons (cdar pred) (cdr pred)))))

(defun extract-paths-for-canonical-rep (fs path)
   ;;; given a FS extracts a list of all paths in the form 
   ;;; required by the predictions in canonicalise-fs-aux
   (let* ((real-dag (follow-pointers fs))
         (type (type-of-fs real-dag)))      
         (cons 
            (cons (reverse path)
            (if (listp type) type (list type)))
            (unless
               (is-atomic real-dag)
               (for label in (top-level-features-of real-dag)
                  append
                  (let ((new-dag (get-dag-value real-dag label)))
                     (extract-paths-for-canonical-rep new-dag
                        (cons label path))))))))


(defun query-canonical-rep (path value path-so-far)
   ;;; returns a list of path value pairs which can be treated as disjuncts
   ;;; to query the LDB
   ;;; Currently this will overgenerate queries
   (if path
   (let ((max-type (maximal-type-of (car path))))
      (unless max-type (error "~%Unknown feature ~A~%" (car path)))
      (append (for type in (cons max-type (retrieve-descendants max-type))
               filter 
               (let ((type-rest (extract-paths-for-canonical-rep 
                           (constraint-of type) nil)))
                  (if (member (cons path value) type-rest :test #'equal)
                     (cons (reverse path-so-far) (list type)))))
         (query-canonical-rep (cdr path) value 
            (cons (car path) 
               path-so-far))))))
   
|#
