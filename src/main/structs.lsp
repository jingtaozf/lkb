;;; Copyright (c) 1991--2002
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see licence.txt for conditions.


(in-package :lkb)

;; RPM - the default print-object method for structures in ACL uses format
;; extensively and is much too slow to use to build up the templex file.  This
;; replacement is a lot faster, but doesn't do any error checking and so is
;; potentially risky.

#+:allegro
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
      (write (slot-value instance slot) :stream stream :length nil :level nil))
    (write-char #\) stream)))


(defstruct (basic-unification) lhs rhs)

(defstruct (unification (:include basic-unification)))

(defstruct (inequality (:include basic-unification)))

;;; following substructures are semi-obsolete, but retaining them
;;; would make it easier to reintegrate old version of the LKB
;;; if anyone ever decides to do this

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

(defstruct (u-value) type)


(defun create-path-from-feature-list (f-list)
   (make-path 
      :typed-feature-list f-list))


(defun create-typed-path-from-feature-list (f-list)
   (make-typed-path 
      :typed-feature-list
      (loop for f in f-list
         collect
         (make-type-feature-pair :type *toptype*
            :feature f))))

(defun make-pv-unif (path1 type)
  (make-unification :lhs (create-typed-path-from-feature-list path1)
                    :rhs (make-u-value :type type)))


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
        
(defun get-unif-features (unif)
  (if (basic-unification-p unif)
      (append (get-path-features (basic-unification-lhs unif))
              (get-path-features (basic-unification-rhs unif)))))

(defun get-path-features (path)
  ;;; returns nil if called on a non-path
  (if (path-p path)
      (path-typed-feature-list path)
    (if (typed-path-p path)
        (loop for fvp in (typed-path-typed-feature-list path)
             collect
             (type-feature-pair-feature fvp)))))

(defun eval-any-leaf-types (unifs)
  (loop for unif in unifs
       do
       (when (basic-unification-p unif)
              (eval-unif-half-types (basic-unification-lhs unif))
              (eval-unif-half-types (basic-unification-rhs unif)))))

(defun eval-unif-half-types (path-or-value)
  (cond ((path-p path-or-value) nil)
        ((u-value-p path-or-value) 
         (eval-possible-leaf-type *leaf-types* 
                                  (u-value-type path-or-value)))
        ((typed-path-p path-or-value)
         (loop for fvp in (typed-path-typed-feature-list path-or-value)
              do
              (eval-possible-leaf-type *leaf-types*
               (type-feature-pair-type fvp))))
        (t nil)))

(defun process-unifications (specific-list)
  (eval-any-leaf-types specific-list)
  (if specific-list
      (let* ((new-dag (create-dag))
             (res 
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
                  nil))))
            (or res
                (progn 
                  (format t
                          "~%Unifications specified are invalid or do not unify")
                  nil)))))

(defun unify-paths (lhs-path lhs-dag rhs-path rhs-dag)
   ;; follows paths into dags and unifies the dags at the end
   ;; caller sets up unification context and copies result if it wants it
    (let ((dag1 (unify-paths-dag-at-end-of lhs-path lhs-dag)))
       (when dag1
          (let ((dag2 (unify-paths-dag-at-end-of rhs-path rhs-dag)))
             (when dag2
                   (unify-dags dag1 dag2))))))

(defun unify-paths-with-fail-messages (lhs-path lhs-dag rhs-path rhs-dag 
				       lhs-id lhs-features rhs-id rhs-features 
				       &optional window-p)
  (with-unification-context (lhs-dag)
    (let ((dag1 (unify-paths-dag-at-end-of lhs-path lhs-dag)))
      (if dag1
	  (let ((dag2 (unify-paths-dag-at-end-of rhs-path rhs-dag)))
	    (if dag2
		(when (unify-wffs-with-fail-messages dag1 dag2 nil window-p)
		  (let ((*unify-debug* (if window-p :window t)))
		    (copy-dag lhs-dag)))
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
   #+:mclprofile (decf ff (ccl::%heap-bytes-allocated))
   (prog1
     (cond 
      ((path-p path-or-value)
         (unify-paths-dag-at-end-of1 dag-instance 
            (path-typed-feature-list path-or-value)))
      ((typed-path-p path-or-value)
         (unify-typed-paths-dag-at-end-of1 dag-instance 
            (typed-path-typed-feature-list path-or-value)))
      ((u-value-p path-or-value)
         (let* ((type (u-value-type path-or-value)))
            (cond 
               ((not (is-valid-type type))
                  (format t "~%Invalid type ~A" type)
                  nil)
               (t
                 (create-typed-dag type)))))
      (t (error "~%Invalid path specification ~A"
            path-or-value)))
     #+:mclprofile (incf ff (ccl::%heap-bytes-allocated))))



(defun unify-paths-dag-at-end-of1 (dag-instance labels-chain)
   (let ((real-dag
           ;; can get called with null path and dag-instance=nil
           (if dag-instance (deref-dag dag-instance) nil)))
      (cond
          ((null labels-chain) real-dag)
          ((atomic-type-p (unify-get-type real-dag)) nil)
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
         ((atomic-type-p (unify-get-type real-dag)) nil)
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
      (t
         (let ((one-step-down
                  (get-dag-value real-dag
                     (car labels-chain))))
            (if one-step-down
               (existing-dag-at-end-of one-step-down
                  (cdr labels-chain))
               nil)))))

(defun existing-dag-at-end-of-with-error (dag-instance labels-chain)
  (or (if *within-unification-context-p*
        (x-existing-dag-at-end-of (deref-dag dag-instance) labels-chain)
        (existing-dag-at-end-of dag-instance labels-chain))
       (error "dag not found at end of path ~:A" labels-chain)))

;;; Code that walks over an existing dag looking for subdags
;;; introduced by the feature parameter.  Returns an a-list
;;; with the subdag associated with all the paths that lead to it

(defun collect-subdags-for-feature (fs feature ignore-feats ignore-paths)
  (collect-subdags-for-feature-aux fs feature ignore-feats 
				   (loop for path in ignore-paths
				       collect
				       (reverse path))
				   nil nil))

(defun collect-subdags-for-feature-aux (fs feature ignore-feats ignore-paths 
					results path-so-far)
  (dolist (arc (dag-arcs fs))
    (let ((dag-feature (dag-arc-attribute arc)))
      (unless (or (member dag-feature ignore-feats)
		  (member path-so-far ignore-paths :test #'equal)) 
	(let ((next-dag (dag-arc-value arc))
	      (new-path (cons dag-feature path-so-far)))
	  (if (eql dag-feature feature)
	      (let ((already-found (assoc next-dag results))
		    (rev-path (reverse new-path)))
		(if already-found 
		    (push rev-path (cdr already-found))
		  (push (cons next-dag (list rev-path))
			results)))
	    (setf results
	      (collect-subdags-for-feature-aux 
	       next-dag feature ignore-feats ignore-paths 
	       results new-path)))))))
  results)

;;; as above, but looks for dags of a particular type

(defun collect-subdags-for-type (fs type ignore-feats ignore-paths)
  (collect-subdags-for-type-aux fs type ignore-feats 
				   (loop for path in ignore-paths
				       collect
				       (reverse path))
				   nil nil))

(defun collect-subdags-for-type-aux (fs type ignore-feats ignore-paths 
					results path-so-far)
  (dolist (arc (dag-arcs fs))
    (let ((dag-feature (dag-arc-attribute arc)))
      (unless (or (member dag-feature ignore-feats)
		  (member path-so-far ignore-paths :test #'equal)) 
	(let ((next-dag (dag-arc-value arc))
	      (new-path (cons dag-feature path-so-far)))
	  (if (and next-dag (eql (type-of-fs next-dag) type))
	      (let ((already-found (assoc next-dag results))
		    (rev-path (reverse new-path)))
		(if already-found 
		    (push rev-path (cdr already-found))
		  (push (cons next-dag (list rev-path))
			results)))
	    (setf results
	      (collect-subdags-for-type-aux 
	       next-dag type ignore-feats ignore-paths 
	       results new-path)))))))
  results)
