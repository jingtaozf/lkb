
#|
(dolist (type *type-names*)
  (find-redundancy type))

|#

(defun find-redundancy (type)
  (let ((parents (type-parents (get-type-entry type))))
    (when (cdr parents)
      (for parent in parents
           do
           (for parent2 in parents
                do
                (when (and (not (eql parent parent2))
                           (member parent2 (get-ancestors (get-type-entry parent))))
                  (format t "~%~A: ~A is redundant - it is an ancestor of ~A " type parent2
                          parent)))))))

(defun check-partitions (type)
  (let* ((type-entry (get-type-entry type))
         (daughters (type-daughters type-entry)))
    (when daughters
      (let*
        ((ancestors (type-ancestors type-entry))
         (descendants (type-descendants type-entry))
         (lineage (cons type (append ancestors descendants))))
        (for descendant in descendants
             do
             (let ((desc-entry (get-type-entry descendant)))
               (unless (subsetp (type-ancestors desc-entry)
                                lineage)
                 (format t "~A " descendant))))))))


(defun debug-constraint (node)
   (let ((type-entry (get-type-entry node)))
     (when type-entry
       (let*
          ((constraint-spec (type-constraint-spec type-entry))
           (local-constraint 
                  (if constraint-spec 
                     (process-unifications 
                        constraint-spec))))
            (cond ((and constraint-spec (null local-constraint))
                  (format t "~%Type ~A has an invalid constraint 
                     specification" node)
                     nil)
                  (t    
                     (when local-constraint
                        (unless 
                           (or (eql (type-of-fs local-constraint) *toptype*)
                              (eql (type-of-fs local-constraint) node))
                           (format t 
                         "~%Warning: setting constraint of ~A to have ~A as type"
                          node node))
                        (set-type local-constraint node)
                        (setf (type-local-constraint type-entry)
                                local-constraint)
                        )
                     ; no need to do inheritance when checking
                     ; for maximal introduction
                     (let ((full-constraint 
                              (inherit-constraints-debugging node type-entry 
                                 local-constraint))) 
                        (cond 
                           (full-constraint
                            (check-fs-for-cycles full-constraint node)
                              (setf (type-constraint type-entry)
                                 full-constraint)
                              (setf (type-appfeats type-entry)
                                 (top-level-features-of full-constraint))
                              full-constraint)
                           (t (format t "~%Type ~A's constraint 
                                 specification clashes with its parents'" node) 
                                 nil))))))))
   nil)

(defun inherit-constraints-debugging (node type-entry local-constraint)
   (if (type-atomic-p type-entry)
      (create-atomic-dag node)
      (reduce #'(lambda (x y)
            (unless
               (or (null x) (null y))
               (if (unify-dags-with-fail-messages x y nil)
                    x)))
         (mapcar #'(lambda (parent)
               (let ((constraint
                        (expand-constraint parent
                           (get-type-entry parent))))
                  (if constraint
                     (copy-dag-completely constraint))))
                  (type-parents type-entry))
         :initial-value 
         (if local-constraint (copy-dag-completely local-constraint)
            (create-typed-dag node)))))

(defun unify-dags-with-fail-messages (dag1 dag2 feature-path)
   (let ((real-dag1 (follow-pointers dag1))
         (real-dag2 (follow-pointers dag2)))
      (or (eq real-dag1 real-dag2) 
         (let
            ((gcsubtype 
                  (find-gcsubtype 
                     (type-of-fs real-dag1)
                     (type-of-fs real-dag2))))
            (if gcsubtype
              (progn
               (set-type real-dag1 gcsubtype)
               (cond 
                  ((or (is-atomic real-dag1) (is-atomic real-dag2))
                     (cond ((not (or (has-features real-dag1) 
                                 (has-features real-dag2)))
                           ;; fix 13 March 1992
                           ;; to check for untyped case
                           (put-name real-dag1 
                              (if (listp gcsubtype) gcsubtype
                                 (list gcsubtype)))
                           ; ??
                           (set-dag-value real-dag1 real-dag2))
                        (t 
                         (format t "~%Unification failed due to atomic non-atomic clash at path ~A" 
                        (reverse feature-path))
                         nil)))
                  ((unifiable-with-fail-messages real-dag1 real-dag2 feature-path)
                     (set-dag-value real-dag1 real-dag2))
                  (t nil)))
            (progn 
                (format t "~%Unification of ~A and ~A failed at path ~A" 
                        (type-of-fs real-dag1)  (type-of-fs real-dag2) 
                        (reverse feature-path))
                nil))))))



(defun unifiable-with-fail-messages (dag1 dag2 feature-path)
   (if (is-atomic dag2)
      t
      (for pairlis in (dag-dag-pairs dag2)
         all-satisfy
         (let* ((label (dag-pair-attribute pairlis))
               (existing-dag (get-dag-value dag1 label)))
         (if (or (null existing-dag)
               (unify-dags-with-fail-messages (get-dag-value dag2 label)
                  existing-dag (cons label feature-path)))
            (put-dag-value dag1
               label
               (get-dag-value dag2 label)))))))
