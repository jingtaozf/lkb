(in-package :cl-user)

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
                        (setq local-constraint 
                              (destructively-retype-dag local-constraint node))
                        (setf (type-local-constraint type-entry)
                                local-constraint)
                        )
                     ; no need to do inheritance when checking
                     ; for maximal introduction
                     (let* ((*unify-debug* t)
                            (full-constraint 
                              (inherit-constraints node type-entry 
                                 local-constraint)))
                        (cond 
                           (full-constraint
                            (setf (type-constraint type-entry)
                              full-constraint)
                            (setf (type-appfeats type-entry)
                              (top-level-features-of full-constraint))
                            full-constraint)
                           (t (format t "~%Type ~A's constraint 
                                 specification clashes with its parents'" node) 
                              nil))))))))
   nil)



