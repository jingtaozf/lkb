;;; Copyright (c) 1998-2003 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Benjamin Waldron
;;; see licence.txt for conditions


(in-package :lkb)

;;; modifications by bmw (dec-03)
;;; - internal reworking of cdb-lex-database + cdb-leaf-database classes 
;;;   and associated script functions

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
;;; 4. The type only has one parent

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
 ;     (setf (slot-value leaf-db 'leaf-types) 
 ;       (slot-value leaf-db 'leaf-types))
      ;;; FIX ? is this right
      (setf *type-names* (delete name *type-names*))
      (delete-non-local-uses name type-entry))))

(defmethod clear-leaf-types ((leaf-db leaf-database))
  (dolist (leaf-type (slot-value leaf-db 'leaf-types))
    (when (gethash leaf-type *types*)
      (setf (gethash leaf-type *types*) nil)))
  (setf (slot-value leaf-db 'leaf-types) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;(defclass trivial-leaf-database (leaf-database)
;;;  ())
;;;
;;;;;; For mysterious reasons, the fix of setting mode binary 
;;;;;; for windows doesn't fix the leaf type use of CDB although it
;;;;;; does fix the lexicon.  More investigation needed.
;;;;;; --- we believe the new, binary-only CDB implementation does solve this
;;;;;; problem.                                        (19-jan-00  -  oe@yy)
;;;
;;;(setf *leaf-types* (make-instance 'trivial-leaf-database))
;;;
;;;(defmethod read-cached-leaf-types ((leaf-db trivial-leaf-database) filenames)
;;;  (declare (ignore filenames))
;;;  (format t "~%Cached leaf types missing or out-of-date: reading leaf type source files.")
;;;  nil)
;;;
;;;(defmethod store-cached-leaf-types ((leaf-db trivial-leaf-database))
;;;  t)
;;;
;;;(defmethod is-leaf-type ((leaf-db trivial-leaf-database) name)
;;;  (member name (slot-value leaf-db 'leaf-types) :test #'eq))
;;;
;;;(defmethod store-leaf-type ((leaf-db trivial-leaf-database) name type-def)
;;;  (with-slots (leaf-types) leaf-db
;;;    (pushnew name *type-names* :test #'eq)
;;;    (pushnew name *ordered-type-list* :test #'eq)
;;;    (pushnew name leaf-types :test #'eq)    
;;;    (set-type-entry name type-def)))
;;;
;;;(defmethod eval-possible-leaf-type ((leaf-db trivial-leaf-database) type)
;;;  (when type
;;;    (let ((type-entry (get-type-entry type)))
;;;      (when type-entry
;;;        (when (leaf-type-p type-entry)
;;;          (unless (leaf-type-expanded-p type-entry)
;;;	    (add-in-leaf-type-entry type-entry)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass cdb-leaf-database (leaf-database)
  ((leaf-db :initform nil :accessor leaf-db)
;   (ready-p :initform nil :accessor ready-p)
   (mode :initform nil :accessor mode)
   (temp-file :initform nil
	      :accessor temp-file
	      :initarg :temp-file)))

(setf *leaf-types* (make-instance 'cdb-leaf-database))

(defmethod read-cached-leaf-types ((leaf-db cdb-leaf-database) filenames)
  (with-slots (temp-file) leaf-db
    (cond 
     ((up-to-date-p filenames (list temp-file))
      (format t "~%Reading in cached leaf types")
      (when (open-read leaf-db)
	(format t "~%Cached leaf types read")
	t))
     (t
      (format t "~%Cached leaf types missing or out-of-date: reading leaf type source files")
      nil))))

(defmethod clear-leaf-types :after ((leaf-db cdb-leaf-database))
  (close-leaf-db leaf-db))

(defmethod store-cached-leaf-types ((leaf-db cdb-leaf-database))
  (close-leaf-db leaf-db))

(defmethod open-p ((db cdb-leaf-database))
  (temp-file db))

(defmethod open-leaf-db ((db cdb-leaf-database) temp-file)
  (if (open-p db)
      (close-leaf-db db))
  (setf (temp-file db) temp-file))

(defmethod close-leaf-db ((db cdb-leaf-database))
  (close-read-write db)
  (setf (temp-file db) nil))

(defmethod open-read-p ((db cdb-leaf-database))
  (eq (mode db) :read))

(defmethod open-write-p ((db cdb-leaf-database))
  (eq (mode db) :write))

(defmethod open-read-write-p ((db cdb-leaf-database))
  (or (open-read-p db)
      (open-write-p db)))

(defmethod open-read ((db cdb-leaf-database))
  (if (open-read-write-p db)
      (close-read-write db))
  (with-slots (leaf-db temp-file mode) db
    (handler-case
	(and (setf leaf-db (cdb:open-read temp-file))
	     (setf mode :read)
	     t)
      (error (condition)
	(format t "~%Error: ~A~%" condition)
	(delete-temporary-lexicon-files db)
	(delete-temporary-lexicon-files *lexicon*) ;;fix_me
	nil))))

(defmethod open-write ((db cdb-leaf-database))
  (if (open-read-write-p db)
      (close-read-write db))
  (with-slots (leaf-db temp-file mode) db
    (handler-case
	(and (setf leaf-db (cdb:open-write temp-file))
	     (setf mode :write)
	     t)
      (error (condition)
	(format t "~%Error: ~A~%" condition)
	(delete-temporary-lexicon-files db)
	(delete-temporary-lexicon-files *lexicon*) ;;fix_me
	nil))))

(defmethod close-read-write ((db cdb-leaf-database))
  (with-slots (leaf-db mode) db
    (handler-case
	(progn
	  (when leaf-db
	     (cdb:close-cdb leaf-db)
	     (setf leaf-db nil))
	  (setf mode nil)
	  t)
      (error (condition)
	(format t "~%Error: ~A~%" condition)
	(delete-temporary-lexicon-files db)
	(delete-temporary-lexicon-files *lexicon*) ;;fix_me
	nil))))

(defmethod is-leaf-type ((db cdb-leaf-database) name)
  (preload-leaf-type db name)
  (member name (slot-value db 'leaf-types) :test #'eq))

;; db is open for writing...
(defmethod store-leaf-type ((db cdb-leaf-database) name type-def)
  (cdb:write-record (leaf-db db) (string name) 
		    (with-standard-io-syntax (write-to-string type-def))))

(defmethod eval-possible-leaf-type ((db cdb-leaf-database) type)
  (unless (stringp type)
    (let ((type-entry (get-type-entry type)))
      (when (or (null type-entry)
		(leaf-type-p type-entry))
	(preload-leaf-type db type)
	(let ((type-entry (get-type-entry type)))
	  (when (and type-entry
		     (leaf-type-p type-entry)
		     (not (leaf-type-expanded-p type-entry)))
	    (add-in-leaf-type-entry type-entry)))))))

(defmethod preload-leaf-type ((db cdb-leaf-database) type)
  (with-slots (leaf-types mode leaf-db) db
    (when (eq mode :read)
      (unless (get-type-entry type)
	;; We take the last entry that's returned
	(let ((entry (car (last (cdb:read-record leaf-db 
						 (string type)))))
	      (*readtable* #+allegro excl::std-lisp-readtable
                           #-allegro (copy-readtable nil)))
	  (when entry
	    (pushnew type *type-names* :test #'eq)
	    (pushnew type *ordered-type-list* :test #'eq)
	    (pushnew type leaf-types :test #'eq)    
	    (set-type-entry type 
              (with-package (:lkb) (read-from-string entry)))))))))


(defmethod delete-temporary-lexicon-files ((db cdb-leaf-database))
  (with-slots (temp-file) db
    (when (and temp-file
	       (probe-file temp-file))
      (delete-file temp-file))
    t)) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-leaf-type (name parents constraint default comment daughters)
  (if daughters
      (format t "~%Error: leaf type ~A declared with daughters")
    (let ((existing-type (get-type-entry name)))
      (cond ((null parents)
	     (format t "~%Error: type ~A has no parents specified"))
	    ((and existing-type
		  (not (is-leaf-type *leaf-types* name)))
	     (format t "~%Error: attempt to redefine non-leaf type ~A as leaf type" name))
	    (t 
	     (when existing-type
	       (remove-leaf-type *leaf-types* name)
	       (format t "WARNING: ~%Type `~A' redefined." name))
	     (if (cdr parents)
		 (format t "~%Error: type ~A cannot be a leaf type - it has ~
multiple parents ~A" name parents)
	       (if (not (get-type-entry (car parents)))
		   (format t "~%Error: type ~A has non-existent parent ~A" 
			   name (car parents))
		 (let ((new-features (append (new-features-in constraint)
					     (new-features-in default))))
		   (if new-features
		       (format t "~%Error: type ~A cannot be a leaf type - it ~
introduces new features ~A" name new-features)
		     (store-leaf-type *leaf-types*
				      name
				      (make-leaf-type :name name 
						      :parents parents
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
      (loop for ancestor-entry in (type-ancestors parent-entry)
           do
           (setf (type-descendants ancestor-entry)
             (delete type-entry 
                     (type-descendants ancestor-entry)
                     :test #'eq))))))

(defun new-features-in (unif-list)
  (loop for unif in unif-list
       append
       (loop for feature in (get-unif-features unif)
            unless 
                (maximal-type-of feature) ; i.e. it's already known
            collect feature)))

(defun expand-leaf-type-entry (name parent type-entry)
  (let ((parent-entry (get-type-entry parent)))
    (add-leaf-to-hierarchy name parent parent-entry type-entry)
    ; no new features, so got to be the same as parent
    (setf (type-appfeats type-entry) (type-appfeats parent-entry))
  (if (and (null (type-constraint-spec type-entry))
           (null (type-default-spec type-entry)))
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
               :tail (loop for element in (tdfs-tail (type-tdfs parent-entry))
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
               (inherit-leaf-constraints node local-constraint
                                         parent-entry)))
          (if full-constraint
              (progn
                (setf (type-inherited-constraint type-entry) full-constraint)
                (if (progn (setf *well-formed-trace* nil)
                           (wf-constraint-of node))
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

(defun inherit-leaf-constraints (node local-constraint
                                 parent-entry)
    (let ((parent-constraint
           (type-constraint parent-entry)))
      ;; unify-dags itself will put this inside a unification context
      (unify-dags (or local-constraint (create-typed-dag node))
                  ;; retype-dag also does a copy - I don't think
                  ;; the retyping is actually needed, but probably
                  ;; faster this way
                  (retype-dag parent-constraint *toptype*))))

