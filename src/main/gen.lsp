;;; Copyright (c) 1992-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions

;;; Rewritten for new unifier

(in-package :lkb)

;;; generalisation, equality and subsumption of fs

;;; Jan 1995 - made equal-wffs-p and subsumes-wffs-p work 
;;; on non well typed fs

;;;
;;; establish global counter for calls to subsumption test.  (14-jul-99  -  oe)
;;;
(defparameter *subsumptions* 0)
(declaim (type fixnum *subsumptions*))

(defun mark-dag-with-backwards-paths (dag firstp backwards-path) 
   ;; mark nodes with lists of paths - generalisation will be reentrant
   ;; in those paths that both the originals were reentrant in
   ;; Unmarking afterwards is done implicitly at next invalidation of visit
   ;; marks, before the visit mark fields are used next time
   ;; We may be inside a unification context, so we have to deref. The dag
   ;; may possibly be cyclic, so we check for that
   ;; !!! note that paths put on nodes are backwards, so they have to be
   ;; reversed if they are actually used as paths rather than just for
   ;; comparison
   (macrolet ((mark-subdags (arcs)
                `(dolist (arc ,arcs)
                    (mark-dag-with-backwards-paths (dag-arc-value arc) firstp
                       (cons (dag-arc-attribute arc) backwards-path)))))
      (setq dag (deref-dag dag))
      (cond
         ((eq (dag-copy dag) :inside)
            (when (or *unify-debug* *unify-debug-cycles*)
               (format t "~%Generalisation failed: cycle found at < ~{~A ~^: ~}>" 
                  (reverse backwards-path)))
            (throw '*fail* nil))
         (t
            ;; take into account that a subdag may be shared between the two
            ;; input dags, possibly in different respective places - deal with
            ;; this by storing separate sets of paths for each input dag in the
            ;; two halves of a cons cell
            (let* ((visit-cell
                     (or (dag-visit dag) (setf (dag-visit dag) (cons nil nil))))
                   (already
                     (if firstp (car visit-cell) (cdr visit-cell))))
               (if firstp
                  (setf (car visit-cell) (cons backwards-path already))
                  (setf (cdr visit-cell) (cons backwards-path already)))
               (unless already
                  (setf (dag-copy dag) :inside)
                  (mark-subdags (dag-arcs dag))
                  (mark-subdags (dag-comp-arcs dag))
                  (setf (dag-copy dag) nil)))))))


;;; generalisation - a new dag is returned and neither dag1 nor dag2 is
;;; modified
;;; Could return nil in case where a circularity is detected in one of the
;;; input dags. This is checked when marking nodes with paths

(defvar *reentrant-sets* nil)

(defun generalise-dags (dag1 dag2)
   (if *within-unification-context-p*
      (progn
         #+:mclprofile (decf jj (ccl::%heap-bytes-allocated))
         (prog1
            (catch '*fail*
               (invalidate-visit-marks)
               (mark-dag-with-backwards-paths dag1 t nil)
               (mark-dag-with-backwards-paths dag2 nil nil)
               (let ((result-dag (create-dag))
                     (*reentrant-sets* nil))
                  (generalise-dags-1 dag1 dag2 result-dag nil)
                  ;; (mapc #'print *reentrant-sets*)
                  (loop for reentrant-set in *reentrant-sets*
                     do 
                     (let ((first-path 
                             (create-path-from-feature-list
                                (reverse (car reentrant-set)))))
                        (loop for other-path in (cdr reentrant-set)
                           do
                           (unify-paths first-path result-dag 
                              (create-path-from-feature-list (reverse other-path))
                              result-dag))))
                  (copy-dag result-dag)))
            #+:mclprofile (incf jj (ccl::%heap-bytes-allocated))
            ))
      (with-unification-context (dag1) (generalise-dags dag1 dag2))))


(defun generalise-dags-1 (dag1 dag2 result-dag path)
   ;; new dag is created by side effects - a list of reentrancy specs is
   ;; created but not used until the end
   ;; only the result-dag is modified destructively - neither input dag is
   ;; changed (apart from working data put in the visit fields)
   (setq dag1 (deref-dag dag1))
   (setq dag2 (deref-dag dag2))
   (setq result-dag (deref-dag result-dag))
   ;; we can't take a short-cut here if the two dags are eq, and just insert
   ;; the contents of one of them into the result, since we may already
   ;; have a partial structure for the result (built by the unification in of
   ;; type constraints)
   (generalise-dags-2 dag1 dag2 result-dag path))


(defun generalise-dags-2 (dag1 dag2 result-dag path)
   (let* ((dag-type1 (unify-get-type dag1))
          (dag-type2 (unify-get-type dag2))
          (reentrant-labels 
             ;; intersect dag1 paths in dag1 with dag2 paths in dag2
             (generalise-path-intersection
                (car (dag-visit dag1)) (cdr (dag-visit dag2)))))
      (when (cdr reentrant-labels) 
         (pushnew reentrant-labels *reentrant-sets* :test #'equal))
      (let*
         ((lcsupertype
             (least-common-supertype dag-type1 dag-type2))
          (constraint
             (if (symbolp lcsupertype) (may-copy-constraint-of lcsupertype))))
         (setf (dag-new-type result-dag) lcsupertype)
         (when constraint
            (let ((res
                    (catch '*fail*
                       (progn (unify1 result-dag constraint path) t))))
               (unless res
                  (error "Unification with constraint of type ~A (lcsupertype ~
                          of ~A and ~A) failed at path < ~{~A ~^: ~}>"
                     lcsupertype dag-type1 dag-type2 (reverse path))))
            ;; result-dag might just have been forwarded so dereference it again
            (setq result-dag (deref-dag result-dag)))
         (when (and (dag-arcs dag1) (dag-arcs dag2))
            (generalise-subparts dag1 dag2 result-dag path)))))

(defun generalise-path-intersection (paths1 paths2)
   (let ((res nil))
      (dolist (p1 paths1)
         (dolist (p2 paths2)
            (when (equal p1 p2) (push p1 res) (return))))
      res))


(defun generalise-subparts (dag1 dag2 real-result-dag path)
  (let ((arcs1 (dag-arcs dag1))
        (comp-arcs1 (dag-comp-arcs dag1))
        (arcs2 (dag-arcs dag2))
        (comp-arcs2 (dag-comp-arcs dag2)))
      (macrolet
         ((generalise-arcs (arcs)
             `(dolist (arc ,arcs)
                  (let* ((label (dag-arc-attribute arc))
                         (elem1 (unify-arcs-find-arc label arcs1 comp-arcs1))
                         (elem2 (unify-arcs-find-arc label arcs2 comp-arcs2))
                         (new-path (cons label path)))
                     (declare (dynamic-extent new-path))
                     (if (and elem1 elem2)
                        (generalise-dags-1
                           (dag-arc-value elem1) (dag-arc-value elem2)
                           (dag-arc-value arc) new-path)
                        (format t "~&Attribute ~A missing in one or both inputs ~
                                   to ~A" label 'generalise-dags)
                        )))))
         (generalise-arcs (dag-arcs real-result-dag))
         (generalise-arcs (dag-comp-arcs real-result-dag)))))



;;; subsumption - first value true if dag1 subsumes dag2, second value true
;;; if reverse relation holds

;;; One-pass algorithm simultaneously traversing the two dags: at each node
;;; visited in dag1 insert pointer to
;;; corresponding dag2 node. If we reach a dag1 node that already has a pointer
;;; this corresponds to a reentrancy in dag1 - if the pointer
;;; isn't eq to the current dag2 node then return false (i.e. this is a reentrancy
;;; in dag1 that isn't present in dag2, so dag1 can't subsume dag2). Also of
;;; course check for type subsumption on each node

(defun dag-subsumes-p (dag1 dag2)
  ;; assume not circular, and not called within a unification context
  (incf *subsumptions*)
  (with-unification-context (dag1)
    (catch '*fail* (subsume-wffs-p dag1 dag2 t t))))

(defun dag-equal-p (dag1 dag2)
  ;; assume not circular, and not called within a unification context
  (multiple-value-bind (sub1 sub2)
      (with-unification-context (dag1)
        (catch '*fail* (subsume-wffs-p dag1 dag2 t t)))
    (and sub1 sub2)))


(defun subsume-wffs-p (real-dag1 real-dag2 forwardp backwardp)
  ;; forwardp, backwardp are true when it's possible that dag1 subsumes dag2
  ;; and vice-versa respectively. When the possibility has been ruled out the
  ;; appropriate variable is set to false. Fail as soon as they are both false
  (when forwardp
    (cond
     ((null (dag-copy real-dag1))
      (setf (dag-copy real-dag1) real-dag2))
     ((not (eq (dag-copy real-dag1) real-dag2))
      (setq forwardp nil))
     ;; ((eq real-dag1 real-dag2)
     ;;    (return-from subsume-wffs-p t))
     ;; There isn't much equality of structures between different edges, so
     ;; not worth testing for it since it involves extra complication: if we
     ;; stop here and don't perform full processing inside the structure
     ;; (since all nodes inside it will be eq between dag1 and dag2, and any
     ;; strictly internal reentrancies will necessarily be the same) we would
     ;; still need to assign pointers inside it so that any external
     ;; reentrancies into the structure would be treated correctly
     ))
  (when backwardp
    (cond
     ((null (dag-copy real-dag2))
      (setf (dag-copy real-dag2) real-dag1))
     ((not (eq (dag-copy real-dag2) real-dag1))
      (setq backwardp nil))))
  (unless (or forwardp backwardp) (throw '*fail* nil))
  (let ((type1 (type-of-fs real-dag1))
        (type2 (type-of-fs real-dag2)))
    (unless (eq type1 type2)
      (when (and forwardp (not (subtype-or-equal type2 type1)))
        (setq forwardp nil))
      (when (and backwardp (not (subtype-or-equal type1 type2)))
        (setq backwardp nil))
      (unless (or forwardp backwardp) (throw '*fail* nil)))
    (dolist (arc1 (dag-arcs real-dag1))
      (let* ((label (dag-arc-attribute arc1))
             (existing-dag2 (get-dag-value real-dag2 label)))
        (when existing-dag2
          (multiple-value-setq (forwardp backwardp)
            (subsume-wffs-p
             (dag-arc-value arc1) existing-dag2 forwardp backwardp)))))
    (values forwardp backwardp)))

;;;
;;; test for forward subsumption only, return failure path
;;;
(defun dag-subsumes-debug (dag1 dag2)
  (with-unification-context (dag1)
    (let ((result (catch '*fail* (subsume-wffs-debug dag1 dag2 t nil nil))))
      (when (and (consp result) (eq (first result) :path))
        (nreverse (rest result))))))

(defun subsume-wffs-debug (dag1 dag2 forwardp backwardp path)
  (when forwardp
    (cond
     ((null (dag-copy dag1))
      (setf (dag-copy dag1) dag2))
     ((not (eq (dag-copy dag1) dag2))
      (setq forwardp nil))))
  (when backwardp
    (cond
     ((null (dag-copy dag2))
      (setf (dag-copy dag2) dag1))
     ((not (eq (dag-copy dag2) dag1))
      (setq backwardp nil))))
  (unless (or forwardp backwardp) (throw '*fail* (cons :path path)))
  (let ((type1 (type-of-fs dag1))
        (type2 (type-of-fs dag2)))
    (unless (eq type1 type2)
      (when (and forwardp (not (subtype-or-equal type2 type1)))
        (setq forwardp nil))
      (when (and backwardp (not (subtype-or-equal type1 type2)))
        (setq backwardp nil))
      (unless (or forwardp backwardp) (throw '*fail* (cons :path path))))
    (dolist (arc1 (dag-arcs dag1))
      (let* ((label (dag-arc-attribute arc1))
             (existing-dag2 (get-dag-value dag2 label)))
        (when existing-dag2
          (multiple-value-setq (forwardp backwardp)
            (subsume-wffs-debug
             (dag-arc-value arc1) existing-dag2 
             forwardp backwardp
             (cons label path))))))
    (values forwardp backwardp)))

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
         (member type 
            (loop for pred in predictions
               when (null (car pred))
               collect (cdr pred)))
         (push (make-unification 
               :lhs (create-path-from-feature-list (reverse path))
               :rhs (make-u-value :type type)) 
            *canonical-paths*))
      (unless
         (is-atomic real-dag)
         (let ((new-predictions 
                  (append
                     (extract-paths-for-canonical-rep (constraint-of type) 
                        nil)
                     predictions)))
            (loop for label in (top-level-features-of real-dag)
               do
               (let ((new-dag (get-dag-value real-dag label)))
                  (canonicalise-fs-aux new-dag 
                     (update-predictions new-predictions label)
                     (cons label path))))))))

(defun update-predictions (predictions feature)
   (loop for pred in predictions
      when (eql (caar pred) feature)
      collect (cons (cdar pred) (cdr pred))))

(defun extract-paths-for-canonical-rep (fs path)
   ;;; given a FS extracts a list of all paths in the form 
   ;;; required by the predictions in canonicalise-fs-aux
   (let* ((real-dag (follow-pointers fs))
         (type (type-of-fs real-dag)))      
         (cons 
            (cons (reverse path)
            type
            (unless
               (is-atomic real-dag)
               (loop for label in (top-level-features-of real-dag)
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
      (append (loop for type in (cons max-type
                                 (mapcar #'ltype-name
                                         (retrieve-descendants max-type)))
               nconc 
               (let ((type-rest (extract-paths-for-canonical-rep 
                           (constraint-of type) nil)))
                  (if (member (cons path value) type-rest :test #'equal)
                     (list (cons (reverse path-so-far) (list type))))))
         (query-canonical-rep (cdr path) value 
            (cons (car path) 
               path-so-far))))))
   
|#
