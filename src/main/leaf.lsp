;;; Copyright Ann Copestake 1998 All Rights Reserved.
;;; No use or redistribution without permission.
;;;
;;;
;;; Leaf types - these functions are effectively simplified versions
;;; of those in checktypes.lsp, but that file was getting too large
;;; so this is kept separate
;;; The idea is to allow incremental and efficient addition of
;;; `leaf' types - conditions below, but basically types
;;; whose addition has no effect on the other types in the hierarchy
;;; with the exception of the daughters and descendants features

; *leaf-types* is defined in types.lsp

;;; add-leaf-type will only work properly under certain (quite restrictive)
;;; conditions
;;; 1. The type is a leaf type in the hierarchy - i.e. it has no
;;;    daughters (not checked for here)
;;; 2. The type does not introduce any new features (checked for here)
;;; 3. The type is not used in the constraint of another type
;;;    - not checked for, and will in fact be OK, provided the
;;;    referring type is defined after the leaf type and the leaf
;;; type is not deleted - otherwise there will be an undefined type error
;;; 4. The type only has one (genuine) parent - it may
;;;    have a template-parent.  

#|
(add-leaf-type 'foo '(rel) nil nil nil)
|#

(defun add-leaf-type (name parents constraint default comment daughters)
  (if daughters
      (format t "~%Error: leaf type ~A declared with daughters")
    (let ((existing-type (get-type-entry name))
          (real-parents nil)
          (template-parents nil))
      (if (null parents)
          (format t "~%Error: type ~A has no parents specified")
        (if (and existing-type
                 (not (member name *leaf-types*)))
            (format t "~%Error: attempt to redefine non-leaf type ~
~A as leaf type" name)
          (progn
            (when existing-type
              (remove-leaf-type name)
              (format t "~%Type ~A redefined" name))
            ;; template parents are allowed, because templates
            ;; are guaranteed not to introduce features
            (when (and *templates* (cdr parents))
              (for parent in parents
                   do
                   (if (member parent *templates* :test #'eq)
                       (push parent template-parents)
                     (push parent real-parents)))
              (unless real-parents
                (setf real-parents (list (car template-parents)))
                (setf template-parents (cdr template-parents))))
            (unless real-parents
              (setf real-parents parents))
            (if (cdr real-parents)
                (format t "~%Error: type ~A cannot be a leaf type - it has ~
multiple parents ~A" name real-parents)
              (if (not (get-type-entry (car real-parents)))
                  (format t "~%Error: type ~A has non-existant parent ~A" 
                          name (car real-parents))
                (let ((new-features (append (new-features-in constraint)
                                            (new-features-in default))))
                  (if new-features
                      (format t "~%Error: type ~A cannot be a leaf type - it ~
introduces new features ~A" name new-features)
                    (let ((new-type
                           (make-leaf-type :name name 
                                           :parents real-parents 
                                           :template-parents template-parents
                                           :daughters nil
                                           :comment comment
                                           :constraint-spec constraint
                                           :default-spec default
                                           :enumerated-p nil)))
                      (pushnew name *type-names* :test #'eq)
                      (pushnew name *leaf-types* :test #'eq)    
                      (set-type-entry name new-type))))))))))))
                      
(defun add-in-leaf-type-entry (new-type)   
  (let ((name (type-name new-type)))
    (create-mark-field new-type)
   ;;; we're going to use some of the same code as in
   ;;; checktypes.lsp and assume all the real types are marked
   ;;; as seen           
    (if (expand-leaf-type-entry 
         name (car (type-parents new-type)) new-type)
        (setf (leaf-type-expanded-p new-type) t)
      (progn 
        (format t "~%Invalid type ~A not added")
        (remove-leaf-type name)
        nil))))

(defun unexpand-leaf-types nil
  (clear-expanded-lex)
  (clear-type-cache)
  (maphash #'(lambda (k v)
               (when (leaf-type-p v)
                 (when (leaf-type-expanded-p v)
                   (delete-non-local-uses k v)
                   (setf (leaf-type-local-constraint v) nil)
                   (setf (leaf-type-appfeats v) nil)
                   (setf (leaf-type-marks v) nil)
                   (setf (leaf-type-constraint v) nil)
                   (setf (leaf-type-tdfs v) nil)
                   (setf (leaf-type-ancestors v) nil)
                   (setf (leaf-type-constraint-mark v) nil)
                   (setf (leaf-type-expanded-p v) nil))))
           *types*))

(defun remove-leaf-type (name)
  ;; defined either for the case where the leaf type turns out
  ;; to be invalid, or when we have leaf types in a file and
  ;; want to delete the cached entries completely 
  (clear-type-cache)  
  ;; assumes this won't be called in places where this is going to severely
  ;; impair efficiency         
  (let ((type-entry (get-type-entry name)))
    (when (leaf-type-p type-entry)
      (remove-type-entry name)
      (setf *leaf-types* (delete name *leaf-types*))
      (setf *type-names* (delete name *type-names*))
      (delete-non-local-uses name type-entry))))

(defun delete-non-local-uses (name type-entry)
  (let* ((parent (car (type-parents type-entry)))
         (parent-entry (get-type-entry parent)))
    (when parent-entry
      (setf (type-daughters parent-entry)
        (delete name (type-daughters parent-entry)))
      (setf (type-descendants parent-entry)
        (delete name (type-descendants parent-entry)))
      (for ancestor in (type-ancestors parent-entry)
           do
           (let ((ancestor-entry (get-type-entry ancestor)))
             (setf (type-descendants ancestor-entry)
               (delete name (type-descendants ancestor-entry))))))))  

(defun new-features-in (unif-list)
  (for unif in unif-list
       append
       (for feature in (get-unif-features unif)
            filter
            (unless 
                (maximal-type-of feature) ; i.e. it's already known
              feature))))

(defun expand-leaf-type-entry (name parent type-entry)
  (let ((parent-entry (get-type-entry parent)))
    (add-leaf-to-hierarchy name parent parent-entry type-entry)
    ; no new features, so got to be the same as parent
    (setf (type-appfeats type-entry) (type-appfeats parent-entry))
  (if (and (null (type-constraint-spec type-entry))
           (null (type-default-spec type-entry))
           (null (type-template-parents type-entry)))
      ;; the easy case
      (copy-parent-fs-slots name type-entry parent-entry)
    ;; there is a local constraint
    (if (type-atomic-p parent-entry)
        (progn 
          (format t 
                  "~%Error: leaf type ~A introduces features not found on ~A"
                  name parent)
          nil)
    ;; return nil if there's a unification failure
        (expand-leaf-type-constraint name type-entry parent-entry)))))
      
(defun add-leaf-to-hierarchy (name parent parent-entry entry)
    ;;; deals with all the slots relating to the hierarchy itself
  (pushnew name 
        (type-daughters parent-entry) :test #'eq)
  (pushnew name 
        (type-descendants parent-entry) :test #'eq)
  (let ((ancestors (type-ancestors parent-entry)))
    (for ancestor in ancestors
         do
         (let ((ancestor-entry (get-type-entry ancestor)))
           (pushnew name 
                 (type-descendants ancestor-entry) :test #'eq)))
    (setf (type-ancestors entry) (cons parent ancestors))))


(defun copy-parent-fs-slots (name entry parent-entry)
  (setf (type-atomic-p entry) (type-atomic-p parent-entry))
  (setf (type-constraint entry)
    (retype-dag
     (type-constraint parent-entry) name))
  (setf (type-tdfs entry)
    (make-tdfs :indef (type-constraint entry)
               :tail (for element in (tdfs-tail (type-tdfs parent-entry))
                          collect
                           element))))
     
(defun expand-leaf-type-constraint (node type-entry parent-entry)
  (let* ((*unify-debug-cycles* t)       ; turn on cyclic dag warning messages
         (constraint-spec (type-constraint-spec type-entry))
         (local-constraint 
          (process-unifications constraint-spec)))
    (if (and constraint-spec (null local-constraint))
        (progn
          (format t "~%Error: ~A has an invalid constraint specification" node)
          nil)
      (progn
        (when local-constraint
          (unless 
              (or (eq (type-of-fs local-constraint) *toptype*)
                  (eq (type-of-fs local-constraint) node))
            (format t 
                    "~%Warning: setting constraint of ~A to have ~A as type"
                    node node))
          (setq local-constraint 
            (destructively-retype-dag local-constraint node))
          (setf (type-local-constraint type-entry) local-constraint))
        (if (and local-constraint
                 (not (subsetp (top-level-features-of local-constraint)
                               (type-appfeats parent-entry))))
            (progn
              (format t "~%Error: ~A introduces features not on its parent" 
                      node)
              nil))
        (let ((full-constraint 
               (inherit-leaf-constraints node type-entry local-constraint)))
          (if full-constraint
              (progn
                (setf (type-constraint type-entry) full-constraint)
                (if (nth-value 1 (wf-constraint-of node))
                    (progn (clear-marks type-entry)
                           (expand-default-constraint node type-entry))
          ; wf-constraint-of and expand-default-constraint 
          ; are the standard checktypes code, which should be OK
          ; assuming the marks are all set to seen on existing types  
                  nil))
            (progn
              (format t "~%Type ~A's constraint ~
                                 specification clashes with its parent's" 
                      node) 
              nil)))))))

(defun inherit-leaf-constraints (node type-entry local-constraint)
  (reduce
   #'(lambda (x y) (if (and x y) (unify-dags x y)))
   (mapcar #'(lambda (parent)
               (let ((constraint
                      (type-constraint (get-type-entry parent))))
                 (if constraint
                           ;; hack to allow for templates
                     (retype-dag constraint *toptype*))))
           (append (type-template-parents type-entry)
                   (type-parents type-entry)))
         :initial-value (or local-constraint (create-typed-dag node))))
    