;;; Copyright Ann Copestake 1998 All Rights Reserved.
;;; No use or redistribution without permission.
;;;

(in-package :cl-user)

;;;
;;; Leaf types - these functions are effectively simplified versions
;;; of those in checktypes.lsp, but that file was getting too large
;;; so this is kept separate
;;; The idea is to allow incremental and efficient addition of
;;; `leaf' types - conditions below, but basically types
;;; whose addition has no effect on the other types in the hierarchy
;;; with the exception of the daughters and descendants features

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

(defclass leaf-database () 
  ((leaf-types :initform nil)))

(defgeneric read-cached-leaf-types (leaf-db filenames))

(defgeneric store-cached-leaf-types (leaf-db))

(defgeneric is-leaf-type (leaf-db name))

(defgeneric store-leaf-type (leaf-db name type-def))

(defgeneric remove-leaf-type (leaf-db name))

(defgeneric clear-leaf-types (leaf-db))

(defgeneric eval-possible-leaf-type (leaf-db type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod remove-leaf-type ((leaf-db leaf-database) name)
  ;; defined either for the case where the leaf type turns out to be invalid,
  ;; or when we have leaf types in a file and want to delete the cached
  ;; entries completely
  (clear-type-cache)  
  ;; assumes this won't be called in places where this is going to severely
  ;; impair efficiency
  (let ((type-entry (get-type-entry name)))
    (when (leaf-type-p type-entry)
      (remove-type-entry name)
      (setf (slot-value leaf-db 'leaf-types) 
        (slot-value leaf-db 'leaf-types))
      ;;; FIX ? is this right
      (setf *type-names* (delete name *type-names*))
      (delete-non-local-uses name type-entry))))

(defmethod clear-leaf-types ((leaf-db leaf-database))
  (dolist (leaf-type (slot-value leaf-db 'leaf-types))
    (when (gethash leaf-type *types*)
      (setf (gethash leaf-type *types*) nil)))
  (setf (slot-value leaf-db 'leaf-types) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass trivial-leaf-database (leaf-database)
  ())

;;; For mysterious reasons, the fix of setting mode binary 
;;; for windows doesn't fix the leaf type use of CDB although it
;;; does fix the lexicon.  More investigation needed.

#+:mswindows
(setf *leaf-types* (make-instance 'trivial-leaf-database))

(defmethod read-cached-leaf-types ((leaf-db trivial-leaf-database) filenames)
  (declare (ignore filenames))
  (format t "~%Cached leaf types missing or out-of-date: reading leaf type source files.")
  nil)

(defmethod store-cached-leaf-types ((leaf-db trivial-leaf-database))
  t)

(defmethod is-leaf-type ((leaf-db trivial-leaf-database) name)
  (member name (slot-value leaf-db 'leaf-types) :test #'eq))

(defmethod store-leaf-type ((leaf-db trivial-leaf-database) name type-def)
  (with-slots (leaf-types) leaf-db
    (pushnew name *type-names* :test #'eq)
    (pushnew name leaf-types :test #'eq)    
    (set-type-entry name type-def)))

(defmethod eval-possible-leaf-type ((leaf-db trivial-leaf-database) type)
  (when type
    (let ((type-entry (get-type-entry type)))
      (when type-entry
        (when (leaf-type-p type-entry)
          (unless (leaf-type-expanded-p type-entry)
	    (add-in-leaf-type-entry type-entry)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass cdb-leaf-database (leaf-database)
  ((leaf-db :initform nil :accessor leaf-db)
   (ready-p :initform nil :accessor leaf-db-ready-p)))

#-:mswindows
(setf *leaf-types* (make-instance 'cdb-leaf-database))

(defmethod read-cached-leaf-types ((leaf-db cdb-leaf-database) filenames)
  (set-temporary-lexicon-filenames)
  (when (up-to-date-p filenames (list *leaf-temp-file*))
    (format t "~%Reading in cached leaf types")
    (when (handler-case 
	      (setf (leaf-db leaf-db) (cdb:open-read *leaf-temp-file*))
	    (error (condition)
	      (format t "~%Error: ~A~%" condition)
	      (delete-temporary-lexicon-files)
	      nil))
      (setf (leaf-db-ready-p leaf-db) t)
      (format t "~%Cached leaf types read")
      (return-from read-cached-leaf-types t)))
  (format t "~%Cached leaf types missing or out-of-date: reading leaf type source files")
  nil)

(defmethod clear-leaf-types :after ((leaf-db cdb-leaf-database))
  (when (leaf-db leaf-db)
    (cdb:close-read (leaf-db leaf-db))
  (setf (leaf-db-ready-p leaf-db) nil)
  (setf (leaf-db leaf-db) nil)))

(defmethod store-cached-leaf-types ((leaf-db cdb-leaf-database))
  (cdb:close-write (leaf-db leaf-db))
  (setf (leaf-db-ready-p leaf-db) t)
  (setf (leaf-db leaf-db) nil))

(defmethod is-leaf-type ((leaf-db cdb-leaf-database) name)
  (preload-leaf-type leaf-db name)
  (member name (slot-value leaf-db 'leaf-types) :test #'eq))

(defmethod store-leaf-type ((leaf-db cdb-leaf-database) name type-def)
  (unless (leaf-db leaf-db)
    (setf (leaf-db leaf-db) (cdb:open-write *leaf-temp-file*)))
  (let ((*print-pretty* nil)
	(*print-readably* t))
    (cdb:write-record (leaf-db leaf-db) (string name) 
		      (write-to-string type-def))))

(defmethod eval-possible-leaf-type ((leaf-db cdb-leaf-database) type)
  (unless (stringp type)
    (let ((type-entry (get-type-entry type)))
      (when (or (null type-entry)
		(leaf-type-p type-entry))
	(preload-leaf-type leaf-db type)
	(let ((type-entry (get-type-entry type)))
	  (when (and type-entry
		     (leaf-type-p type-entry)
		     (not (leaf-type-expanded-p type-entry)))
	    (add-in-leaf-type-entry type-entry)))))))

(defun preload-leaf-type (leaf-db type)
  (when (leaf-db-ready-p leaf-db)
    (unless (leaf-db leaf-db)
      (setf (leaf-db leaf-db) (cdb:open-read *leaf-temp-file*)))
    (with-slots (leaf-types) leaf-db
      (unless (get-type-entry type)
	;; We take the last entry that's returned
	(let ((entry (car (last (cdb:read-record (leaf-db leaf-db) 
						 (string type)))))
	      (*readtable* #+allegro excl::std-lisp-readtable
                           #-allegro (copy-readtable nil)))
	  (when entry
	    (pushnew type *type-names* :test #'eq)
	    (pushnew type leaf-types :test #'eq)    
	    (set-type-entry type (read-from-string entry))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-leaf-type (name parents constraint default comment daughters)
  (if daughters
      (format t "~%Error: leaf type ~A declared with daughters")
    (let ((existing-type (get-type-entry name))
          (real-parents nil)
          (template-parents nil))
      (cond ((null parents)
	     (format t "~%Error: type ~A has no parents specified"))
	    ((and existing-type
		  (not (is-leaf-type *leaf-types* name)))
	     (format t "~%Error: attempt to redefine non-leaf type ~A as leaf type" name))
	    (t 
	     (when existing-type
	       (remove-leaf-type *leaf-types* name)
	       (format t "~%Type ~A redefined" name))
	     ;; template parents are allowed, because templates are guaranteed
	     ;; not to introduce features
	     (when (and *templates* (cdr parents))
	       (dolist (parent parents)
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
		   (format t "~%Error: type ~A has non-existent parent ~A" 
			   name (car real-parents))
		 (let ((new-features (append (new-features-in constraint)
					     (new-features-in default))))
		   (if new-features
		       (format t "~%Error: type ~A cannot be a leaf type - it ~
introduces new features ~A" name new-features)
		     (store-leaf-type *leaf-types*
				      name
				      (make-leaf-type :name name 
						      :parents real-parents
						      :real-parents real-parents
						      :template-parents template-parents
						      :daughters nil
						      :comment comment
						      :constraint-spec constraint
						      :default-spec default
						      :enumerated-p nil)))))))))))
                      
(defun add-in-leaf-type-entry (new-type)   
  (let ((name (type-name new-type)))
    (create-mark-field new-type)
    ;; we're going to use some of the same code as in checktypes.lsp and
    ;; assume all the real types are marked as seen
    (if (expand-leaf-type-entry name (car (type-parents new-type)) new-type)
        (setf (leaf-type-expanded-p new-type) t)
      (progn 
        (format t "~%Invalid type ~A not added" name)
        (remove-leaf-type *leaf-types* name)
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

(defun delete-non-local-uses (name type-entry)
  (let* ((parent (car (type-parents type-entry)))
         (parent-entry (get-type-entry parent)))
    (when parent-entry
      (setf (type-daughters parent-entry)
        (delete name (type-daughters parent-entry)))
      (setf (type-descendants parent-entry)
        (delete type-entry (type-descendants parent-entry) :test #'eq))
      (for ancestor-entry in (type-ancestors parent-entry)
           do
           (setf (type-descendants ancestor-entry)
             (delete type-entry 
                     (type-descendants ancestor-entry)
                     :test #'eq))))))

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
  (declare (ignore parent))
  ;; deals with all the slots relating to the hierarchy itself
  (pushnew name (type-daughters parent-entry) :test #'eq)
  (pushnew entry (type-descendants parent-entry) :test #'eq)
  (let ((ancestors (type-ancestors parent-entry)))
    (dolist (ancestor ancestors)
      (pushnew entry (type-descendants ancestor) :test #'eq))
    (setf (type-ancestors entry) (cons parent-entry ancestors))))


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
                (if (progn (setf *well-formed-trace* nil)
                           (nth-value 1 (wf-constraint-of node)))
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
    