;;; Copyright Ann Copestake 1991/2/5. All Rights Reserved.
;;; No use or redistribution without permission.
;;; 
;;; Ann Copestake
;;; Computer Laboratory, University of Cambridge
;;; Pembroke Street
;;; Cambridge, UK

(in-package :cl-user)

;; RPM - the default print-object method for structures in ACL uses format
;; extensively and is much too slow to use to build up the templex file.  This
;; replacement is a lot faster, but doesn't do any error checking and so it
;; potentially risky.

#+allegro
(defmethod common-lisp:print-object ((instance structure-object) stream)
  (let ((class (class-name (class-of instance))))
    (write-string "#S(" stream)
    (unless (eq (symbol-package class) *package*)
      (write-string (package-name (symbol-package class)) stream)
      (write-string "::" stream))
    (write-string (symbol-name class) stream)
    (dolist (slot (mapcar #'clos:slot-definition-name 
                        (clos:class-slots (class-of instance))))
      (write-string " :" stream)
      (write-string (symbol-name slot) stream)
      (write-char #\space stream)
      (write (slot-value instance slot) :stream stream))
    (write-char #\) stream)))


(defstruct (basic-unification) lhs rhs)

(defstruct (unification (:include basic-unification)))

(defstruct (c-identity (:include basic-unification)) )

(defstruct (equality (:include basic-unification)) )

(defstruct (inheritance (:include basic-unification)) )

(defstruct (default-inheritance (:include basic-unification)) )

(defstruct (rule-application) fs rule)

(defstruct (generalise-fs) fs1 fs2)

(defstruct (unify-fs) fs1 fs2)

(defstruct (fs-and-path) fs path)
   

(defstruct (path) typed-feature-list)
; try saving space/time by making typed-feature-list a list of
; features rather than type-feature-pairs

(defstruct (typed-path) typed-feature-list)

(defstruct (type-feature-pair) type feature) 

(defstruct (u-value) types)


(defun create-path-from-feature-list (f-list)
   (make-path 
      :typed-feature-list f-list))


(defun create-typed-path-from-feature-list (f-list)
   (make-typed-path 
      :typed-feature-list
      (for f in f-list
         collect
         (make-type-feature-pair :type *toptype*
            :feature f))))

(defun make-pv-unif (path1 type)
  (make-unification :lhs (create-typed-path-from-feature-list path1)
                    :rhs (make-u-value :types (if (listp type) type
                                                  (list type)))))


(defun path-append (path1 path2)
  (make-path :typed-feature-list 
             (append
              (path-typed-feature-list path1)
              (path-typed-feature-list path2))))
      
(defun path-delete (path1 path2)
  (let ((f1 (path-typed-feature-list path1))
        (f2 (path-typed-feature-list path2)))
    (if (and (> (length f2) (length f1))
             (equal f1 (subseq f2 0 (length f1))))
        (make-path :typed-feature-list 
                   (subseq f2 (length f1)))
      path2)))
        


(defun extend-typed-path (path type feature)
  (let ((new-tvp (make-type-feature-pair :type type :feature feature))
        (old-tvp-list (if (typed-path-p path)
                          (typed-path-typed-feature-list path)
                        nil)))
    (make-typed-path 
     :typed-feature-list 
     (append old-tvp-list (list new-tvp)))))
   

(defun get-unif-features (unif)
  (if (basic-unification-p unif)
      (append (get-path-features (basic-unification-lhs unif))
              (get-path-features (basic-unification-rhs unif)))))

(defun get-path-features (path)
  ;;; returns nil if called on a non-path
  (if (path-p path)
      (path-typed-feature-list path)
    (if (typed-path-p path)
        (for fvp in (typed-path-typed-feature-list path)
             collect
             (type-feature-pair-feature fvp)))))

(defun eval-any-leaf-types (unifs)
  (for unif in unifs
       do
       (when (basic-unification-p unif)
              (eval-unif-half-types (basic-unification-lhs unif))
              (eval-unif-half-types (basic-unification-rhs unif)))))

(defun eval-unif-half-types (path-or-value)
  (cond ((path-p path-or-value) nil)
        ((u-value-p path-or-value) 
         (for type in (u-value-types path-or-value)
              do
              (eval-possible-leaf-type type)))
        ((typed-path-p path-or-value)
         (for fvp in (typed-path-typed-feature-list path-or-value)
              do
              (eval-possible-leaf-type
               (type-feature-pair-type fvp))))
        (t nil)))

(defun eval-possible-leaf-type (type)
  (when type
    (let ((type-entry (get-type-entry type)))
      (when type-entry
        (when (leaf-type-p type-entry)
          (unless (leaf-type-expanded-p type-entry)
            (add-in-leaf-type-entry type-entry)))))))

(defun process-unifications (specific-list)
   ;; if create-wffs-p then create-wffs is called to finish off - if
  ;; it fails then second value of just the non-wff fs is returned
  (when *leaf-types*
    (eval-any-leaf-types specific-list))
   (if specific-list
      (let ((new-dag (create-dag)))
         (with-unification-context (new-dag)
            (if
               (every
                  #'(lambda (unification) 
                      (unify-paths 
                         (basic-unification-lhs unification)
                         new-dag
                         (basic-unification-rhs unification)
                         new-dag))
                  specific-list)
               (copy-dag new-dag)
               (progn 
                  (format t
"~%Unifications specified are invalid or do not unify~%")
                  (for unif in specific-list
                     do
                     (output-unif unif t nil))
                  nil))))))

(defun unify-paths (lhs-path lhs-dag rhs-path rhs-dag)
   ;; follows paths into dags and unifies the dags at the end
   ;; caller sets up unification context and copies result if it wants it
    (let ((dag1 (unify-paths-dag-at-end-of lhs-path lhs-dag)))
       (when dag1
          (let ((dag2 (unify-paths-dag-at-end-of rhs-path rhs-dag)))
             (when dag2
                   (unify-dags dag1 dag2))))))

(defun unify-paths-with-fail-messages (lhs-path lhs-dag rhs-path rhs-dag 
                             lhs-id lhs-features rhs-id rhs-features)
   (with-unification-context (lhs-dag)
      (let ((dag1 (unify-paths-dag-at-end-of lhs-path lhs-dag)))
         (if dag1
            (let ((dag2 (unify-paths-dag-at-end-of rhs-path rhs-dag)))
               (if dag2
                  (when (unify-wffs-with-fail-messages dag1 dag2 nil)
                     (copy-dag lhs-dag))
                  (format t "~%Path ~A is not appropriate for dag ~A" 
                     rhs-features rhs-id)))
           (format t "~%Path ~A is not appropriate for dag ~A" 
              lhs-features lhs-id)))))

;;; Following a path into a dag - recursively descends into the dag,  at each
;;; step choosing the attribute-value pair as defined  by the current point in
;;; the path, returns the dag at the end of the  path.  If no dag exists, a
;;; null dag is created and recursively embedded into a larger dag structure
;;; specified by the path chain.

(defun unify-paths-dag-at-end-of (path-or-value dag-instance)
   #+(and mcl powerpc)(decf ff (CCL::%HEAP-BYTES-ALLOCATED))
   (prog1
     (cond 
      ((path-p path-or-value)
         (unify-paths-dag-at-end-of1 dag-instance 
            (path-typed-feature-list path-or-value)))
      ((typed-path-p path-or-value)
         (unify-typed-paths-dag-at-end-of1 dag-instance 
            (typed-path-typed-feature-list path-or-value)))
      ((u-value-p path-or-value)
         (let* ((types (u-value-types path-or-value))
                (invalid-types (remove-if #'is-valid-type types)))
            (cond 
               (invalid-types
                  (format t "~%Invalid types ~A" 
                     invalid-types)
                  nil)
               ((eql (length types) 1)
                  (let ((type (car types)))
                     (cond 
                        ((atomic-type-p type)
                           (create-atomic-dag types))
                        (t (create-typed-dag type)))))
               ((every #'atomic-type-p types)
                  (create-atomic-dag types))
               (t (format t "~%Disjunction of non-atomic fs ~A" types)
                  nil))))
      (t (error "~%Invalid path specification ~A"
            path-or-value)))
     #+(and mcl powerpc)(incf ff (CCL::%HEAP-BYTES-ALLOCATED))))



(defun unify-paths-dag-at-end-of1 (dag-instance labels-chain)
   (let ((real-dag
           ;; can get called with null path and dag-instance=nil
           (if dag-instance (deref-dag dag-instance) nil)))
      (cond
         ((null labels-chain) real-dag)
         ((is-atomic real-dag) nil)
         (t
          (let* ((next-feature (car labels-chain))
                 (found
                   (unify-arcs-find-arc next-feature
                                        (dag-arcs real-dag)
                                        (dag-comp-arcs real-dag))))
              (if found
                  (unify-paths-dag-at-end-of1
                   (dag-arc-value found) (cdr labels-chain))
                (let ((one-step-down 
                       (create-typed-dag *toptype*)))
                  (push
                   (make-dag-arc :attribute next-feature
                                 :value one-step-down)
                   (dag-comp-arcs real-dag))
                  (unify-paths-dag-at-end-of1 one-step-down
                                              (cdr labels-chain)))))))))

; original function
(defun unify-typed-paths-dag-at-end-of1 (dag-instance labels-chain)
   (let ((real-dag
           ;; can get called with null path and dag-instance=nil
           (if dag-instance (deref-dag dag-instance) nil)))
      (cond
         ((null labels-chain) real-dag)
         ((is-atomic real-dag) nil)
         (t
            (let ((next-type (type-feature-pair-type (car labels-chain))))
               (if (is-valid-type next-type)
                  (let ((next-feature 
                           (type-feature-pair-feature (car labels-chain)))
                        (gcs (greatest-common-subtype 
                                next-type (unify-get-type real-dag))))
                     (when gcs 
                        (setf (dag-new-type real-dag) gcs)
                        (let ((found
                                 (unify-arcs-find-arc next-feature
                                    (dag-arcs real-dag)
                                    (dag-comp-arcs real-dag))))
                           (if found
                              (unify-typed-paths-dag-at-end-of1
                                 (dag-arc-value found) (cdr labels-chain))
                              (let ((one-step-down 
                                      (create-typed-dag *toptype*)))
                                 (push
                                    (make-dag-arc :attribute next-feature
                                       :value one-step-down)
                                    (dag-comp-arcs real-dag))
                                 (unify-typed-paths-dag-at-end-of1 one-step-down
                                    (cdr labels-chain)))))))
                  (format t "~%Invalid type ~A" next-type)))))))

;;; called from parse.lsp - create-temp-parsing-tdfs

(defun unify-list-path (flist big-fs little-fs)
  ;;; take a new-fs and a path - either a single feature or a 
  ;;; list of features, but not nil
  ;;; and creates the path, putting the little-fs at the end
  (let ((dag1
         (if (listp flist)
             (unify-new-paths big-fs flist)
           (let ((one-step-down 
                  (create-typed-dag *toptype*)))
             (push
              (make-dag-arc :attribute flist
                            :value one-step-down)
              (dag-comp-arcs big-fs))
             one-step-down))))
    (unify-dags dag1 little-fs)))

(defun unify-new-paths (dag-instance labels-chain)
  (if labels-chain
      (let ((next-feature (car labels-chain))
            (one-step-down 
             (create-typed-dag *toptype*)))
        (push
         (make-dag-arc :attribute next-feature
                       :value one-step-down)
         (dag-comp-arcs dag-instance))
        (unify-new-paths one-step-down (cdr labels-chain)))
      dag-instance))

;;; To be called (only) outside context of a (set of) unifications. We can't
;;; by default create subdags at end of path if not there since that would
;;; be destructive. Code above does that sort of thing

(defun existing-dag-at-end-of (real-dag labels-chain)
   (cond 
      ((null labels-chain) real-dag)
      ((is-atomic real-dag) nil)
      (t
         (let ((one-step-down
                  (get-dag-value real-dag
                     (car labels-chain))))
            (if one-step-down
               (existing-dag-at-end-of one-step-down
                  (cdr labels-chain))
               nil)))))

(defun existing-dag-at-end-of-with-error (dag-instance labels-chain)
   (or (existing-dag-at-end-of dag-instance labels-chain)
       (error "dag not found at end of path ~:A" labels-chain)))


