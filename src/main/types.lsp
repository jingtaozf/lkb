;;; Copyright (c) 1991-2003 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions


;;; modifications for YADU - April 1997
;;; bug fixes etc 1995
;;; July 1996 - cacheing glbs
;;; structure mod to allow glbs to be calculated 

(in-package :lkb)

;;;
;;; generic assoc() and member() are mightier than is typically needed; supply
;;; simplified versions instead.                           (27-sep-99  -  oe)
;;;

(defmacro sassoc (element list)
  `(loop
       for foo in (the list ,list)
       when (eq (first (the cons foo)) ,element) return foo))

(defmacro smember (element list)
  `(loop for foo in (the list ,list) thereis (eq ,element foo)))


;;(defmethod print-object ((obj type) stream) (write-char #\~ stream) (prin1 (type-name obj) stream))
;;;
;;; For each type we need:
;;; 
;;; name 
;;; parents - ie immediate supertypes - a set of types
;;; constraint - a feature structure stored either in a fully
;;;              expanded form or just as the feature structure 
;;;              specific to the type
;;; tdfs -       the full typed default feature structure constraint
;;;
;;; For implementation purposes we also have:
;;;
;;; constraint-mark - unification generation last time the constraint
;;;                   was returned not completely-copied
;;; daughters - immediate subtypes - a set of types.
;;; appfeats - appropriate features - a set of features which can be
;;; derived from the constraint (top-level-features-of constraint)
;;; but cached in order to type untyped feature structures efficiently.
;;; ancestors - all the supertypes of a type - immediate or otherwise
;;; marks - see marks.lsp
;;; constraint-spec - the user specified unifications
;;; default-spec - the user specified default unifications
;;; local constraint - the fs derived from the user specified 
;;;                    unifications
;;; inherited constraint - the fs after inheritance but before
;;;                     type inference - for debugging - zeroed 
;;;                     after expanding all constraints
;;; atomic-p - t if the type has no appropriate features and none of
;;;            its subtypes have any appropriate features
;;; July 1996 
;;; glbp - t if type was automatically created
;;; May 1997
;;; descendants - for glb stuff

(defstruct type 
           name parents constraint (constraint-mark nil) tdfs
           comment
           daughters appfeats enumerated-p ancestors marks
           constraint-spec default-spec local-constraint 
           inherited-constraint
           atomic-p glbp
           descendants
           shrunk-p visible-p          ; for display in type hierarchy
           bit-code               ; for glb computation
           )
           
(defmethod common-lisp:print-object ((instance type) stream)
  (if *print-readably*
      ;; print so object can be read back into lisp
      (call-next-method)
    ;; usual case
    (progn 
      (write-string "#Type<" stream)
      (write-string (string (type-name instance)) stream)
      (write-char #\> stream))))


(defstruct (leaf-type (:include type))
  (expanded-p nil))
        
(defvar *types* (make-hash-table :test #'eq))

(defparameter *ordered-type-list* nil)

(defparameter *ordered-glbtype-list* nil)

(defparameter *default-abbreviations* nil)

(defvar *types-changed* nil)
(defvar *lexicon-changed* nil)

(defvar *type-reload-p* nil)

(defun clear-types nil
   (clear-type-cache) ; must be done before types table is cleared
   (disable-type-interactions)
   (clrhash *types*) 
   (setf *ordered-type-list* nil)
   (setf *ordered-glbtype-list* nil)
;   (clear-leaf-types *leaf-types*) ;; no longer needed here
   (clear-feature-table)
   (clear-expanded-lex)
   #+:allegro 
   (when (and *gc-before-reload* *type-reload-p*) (excl:gc t))
   (setf *type-reload-p* t))

(defun clear-types-for-patching-constraints nil
   (clear-type-cache)
   (clear-feature-table)
   (clear-expanded-lex))


(defun clear-type-visibility nil
   (maphash 
      #'(lambda (key entry)
          (declare (ignore key))
         (setf (type-visible-p entry) nil))
      *types*))

(defun collect-type-names nil
   (let ((type-names nil))
      (maphash #'(lambda (name entry) 
                   (declare (ignore entry))
                   (push name type-names))
         *types*)
      type-names))

(defmacro get-type-entry (name)
   `(gethash ,name *types*))

(defun is-valid-type (name)
   (or (get-type-entry name)
      (stringp name)))

(defun string-type-p (type-name)
  ;; AAC 30/12/94
  ;; allow for no string type
  (and *string-type*
       (or (eq type-name *string-type*)
	   (member (get-type-entry type-name)
		   (retrieve-ancestors *string-type*) :test #'eq))))

(defun get-known-type-entry (name)
   (or (gethash name *types*)
      (error "~%Unknown type ~A" name)))

(defun set-type-entry (name new-entry)
  (setf (gethash name *types*) new-entry))

(defun remove-type-entry (name)
  ;;; effectively invalidates type but
  ;;; assumes all the tricky stuff is taken care of elsewhere ...
   (setf (gethash name *types*) nil))

(defun constraint-spec-of (type-name)
   (let ((type-record (get-type-entry type-name)))
      (if type-record 
         (type-constraint-spec type-record)
         (error "~%~A is not a valid type" type-name))))

(defun constraint-of (type-name)
   (let ((type-record (get-type-entry type-name)))
      (if type-record 
         (type-constraint type-record)
         (error "~%~A is not a valid type" type-name))))

(defun tdfs-of (type-name)
   (let ((type-record (get-type-entry type-name)))
      (if type-record 
         (type-tdfs type-record)
         (error "~%~A is not a valid type" type-name))))

(defun default-spec-of (type-name)
   (let ((type-record (get-type-entry type-name)))
      (if type-record 
         (type-default-spec type-record)
         (error "~%~A is not a valid type" type-name))))


(defun appropriate-features-of (type-name)
   (let ((type-record (get-type-entry type-name)))
      (if type-record 
         (type-appfeats type-record)
         (error "~%~A is not a valid type" type-name))))

(defun retrieve-ancestors (type-name)
  (let ((type-record (get-type-entry type-name)))
    (if type-record 
	(type-ancestors type-record)
      (error "~%~A is not a valid type" type-name))))

(defun retrieve-descendants (type-name)
  (let ((type-record (get-type-entry type-name)))
    (if type-record 
	(type-descendants type-record)
      (error "~%~A is not a valid type" type-name))))

(defun retrieve-parents (type-name)
  (let ((type-record (get-type-entry type-name)))
    (if type-record 
	(type-parents type-record)
      (error "~%~A is not a valid type" type-name))))

(defun retrieve-daughters (type-name)
  (let ((type-record (get-type-entry type-name)))
    (if type-record 
	(type-daughters type-record)
      (error "~%~A is not a valid type" type-name))))

(defun subtype-p (type1 type2)
  (if (stringp type1) 
      (string-type-p type2)
    (member (get-type-entry type2) (retrieve-ancestors type1)
		:test #'eq)))

(defun safe-subtype-p (type1 type2)
  ;;
  ;; be robust on invalid type names and fail gracefully (i.e. no error())
  ;;
  (ignore-errors (subtype-p type1 type2)))

(defun atomic-type-p (type-name)
  (or (stringp type-name)
      (let ((type-record (get-type-entry type-name)))
	(if type-record 
            (type-atomic-p type-record)
	  (error "~%~A is not a valid type" type-name)))))

;;; glb computation - entry point is greatest-common-subtype

;;; Type unification performed by lookup in a global vector. Index is
;;; numeric combination of sxhash values of the two type names
;;; (symbols): (sxhash(t1)&&1023)<<10 + sxhash(t2)&&1023 where the two
;;; types are ordered on their sxhash values - so that either order results
;;; in the same table entry being retrieved. Must also test the types
;;; themselves in case another pair of types has the same key. So a vector
;;; entry is (type1 type2 . (subype . constraintp))

(defparameter *type-cache* (make-array (* 1024 1024) :initial-element nil))

(defun clear-type-cache nil
   ;; it's probably best to clear this out occasionally - definitely after
   ;; loading a grammar and before parsing since different pairs of types will
   ;; be exercised
   (let ((arr *type-cache*))
      (declare (simple-vector arr))
      (dotimes (x (* 1024 1024)) (setf (svref arr x) nil))))


(defmacro sxhash-one-symbol (x)
   `(#+mcl ccl::%%eqhash #-mcl sxhash (the symbol ,x)))

(defmacro type-cache-index (x)
   ;; least significant 10 bits of sxhash(x)
   `(logand (the fixnum (sxhash-one-symbol ,x)) 1023))

(defmacro type-cache-entry (x y)
   `(svref *type-cache*
       (the fixnum (+ (the fixnum (ash (the fixnum ,x) 10)) (the fixnum ,y)))))

(defmacro cached-greatest-common-subtype (type1 type2)
  `(let ((t1 ,type1)
         (t2 ,type2))
      (if (eq t1 *toptype*)
         ;; avoid caching on top type - result will always be other type
         t2
         (if (eq t2 *toptype*)
            t1
            (let* ((i1 (type-cache-index t1))
                   (i2 (type-cache-index t2)))
               (when (> (the fixnum i2) (the fixnum i1))
                  (rotatef i1 i2) (rotatef t1 t2))
               (let* ((entry (type-cache-entry i1 i2))
                      (found
                         (dolist (e entry)
                            (when (and (eq (car e) t1) (eq (cadr e) t2))
                               (return (cddr e))))))
                   (if found
                      (values (car found) (cdr found))
                      (multiple-value-bind (subtype constraintp)
                           (full-greatest-common-subtype t1 t2)
                         (setf (type-cache-entry i1 i2)
                            (nconc entry
                              (list (list* t1 t2 (cons subtype constraintp)))))
                         (values subtype constraintp)))))))))


#|
(defparameter *type-cache* nil)

(defun clear-type-cache nil
   ;; it's probably best to clear this out occasionally - definitely after
   ;; loading a grammar and before parsing since different pairs of types will
   ;; be exercised
   (maphash
      #'(lambda (type defn)
          (declare (ignore defn))
          (remprop type :type-cache))
      *types*))

(defmacro cached-greatest-common-subtype (type1 type2 type1-atomic-p)
  `(let ((t1 ,(if type1-atomic-p `(car ,type1) `,type1))
	 (t2 ,type2))
      (if (eq t1 *toptype*)
         ;; avoid caching on top type - result will always be other type
          t2
        (if (eq t2 *toptype*)
            t1
        (let* ((entry (get t1 :type-cache))
               (found (cdr (dolist (foo entry)
                             (when (eq (first foo) t2) (return foo))))))
          (if found
              (values (car found) (cdr found))
            (multiple-value-bind (subtype constraintp)
                (full-greatest-common-subtype t1 t2)
              ,@(when type1-atomic-p
	          `((when subtype (setq subtype (list subtype)))))
              (setf (get t1 :type-cache) 
                ;; put new result at end of cache - very slightly faster than failures
                ;; at start and successes at end. Other 2 alternatives are appreciably
                ;; slower
                (nconc entry
                       (list (cons t2 (cons subtype constraintp)))))
              (values subtype constraintp))))))))
|#

#|
;;; investigate effectiveness of subtype cache
(let ((max 0) (longest nil))
   (maphash
      #'(lambda (type defn)
          (declare (ignore defn))
          (let ((entry (get type :type-cache)))
             (when (> (length entry) max)
                (setq max (length entry) longest (cons type entry)))))
      *types*)
   (values max longest))
|#


(defun greatest-common-subtype (type1 type2)
  ;; implemented as a memo function. In practice we won't see anything
  ;; like all possible combinations of arguments so best not to
  ;; attempt to pre-compute the cache contents
  ;;
  ;; we expect both args to be lisp atoms, but either or both could be
  ;; strings/string type.  String types are not cached
  (cond 
   ((eq type1 type2) type1)
   ((arrayp type1)			; a string?
    (when (or (equal type1 type2) (string-type-p type2))
      type1))
   ((arrayp type2) 
    (when (string-type-p type1) type2))
   (t
    (cached-greatest-common-subtype type1 type2))))

(defun full-greatest-common-subtype (type1 type2)
  (let ((t1 (get-type-entry type1))
	(t2 (get-type-entry type2)))
    (cond 
     ((eq type1 type2) type1)
     ((member t2 (type-ancestors t1) :test #'eq)
      type1)
     ((member t1 (type-ancestors t2) :test #'eq)
      type2)
     (t (let* ((type1-desc (type-descendants t1))
               (type2-desc (type-descendants t2))
               (common-subtypes 
                (intersection type1-desc type2-desc :test #'eq)))
	  (when common-subtypes
	    (let ((greatest-common-subtype-list
		   (intersection
		    common-subtypes 
		    (reduce #'intersection 
			    (mapcar #'(lambda (subtype)
					(cons subtype 
					      (type-ancestors subtype)))
				    common-subtypes))
                    :test #'eq)))
	      (cond ((not (cdr greatest-common-subtype-list))
		     (let ((gcsubtype-entry (car greatest-common-subtype-list)))
		       (values (type-name gcsubtype-entry)
                               (if (extra-constraint-p 
                                    gcsubtype-entry
                                    t1 t2) t))))
		    ;; return true as the second value if there is a
		    ;; constraint that may have to be unified in
		    (greatest-common-subtype-list
		     (error "~%~A and ~A have multiple common subtypes ~A"
			    type1 type2
                            (mapcar #'(lambda (x) (type-name x)) 
                                    greatest-common-subtype-list)))
		    (t (error 
			"~%Error found in type hierarchy"))))))))))

(defun extra-constraint-p (gcsubtype t1 t2)
  ;;; test is whether any ancestor of the gcsubtype which
  ;;; isn't also an ancestor of the types being unified
  ;;; or the gcsubtype itself introduce any extra information 
  ;;; on the constraint.
  (or (type-local-constraint gcsubtype)
      (let ((t1ancs (type-ancestors t1))
            (t2ancs (type-ancestors t2)))
        (dolist (type (type-ancestors gcsubtype))
          (when 
              (and (not (eq type t1))
                   (not (eq type t2))
                   (type-local-constraint type)
                   (not (member type t1ancs :test #'eq))
                   (not (member type t2ancs :test #'eq)))
            (return t))))))

;;; called from generalisation 

(defun least-common-supertype (x y)
  (cond 
   ((equal x y) x)
   ((stringp x) 
    (if (stringp y)
	*string-type*
      (least-common-supertype *string-type* y)))
   ((stringp y) 
    (least-common-supertype *string-type* x))
   ((subtype-p x y) y)
   ((subtype-p y x) x)
   (t
    (let ((z (intersection (cons x (mapcar #'type-name (retrieve-ancestors x)))
			   (cons y (mapcar #'type-name (retrieve-ancestors y))))))
      (cond ((null z) 
	     (error "~%Types ~A and ~A have no common ancestor" x y))
	    ((= (length z) 1) (car z))
	    ((member x z) x)
	    ((member y z) y)
	    (t (let ((lcs-list (remove-ancestors z)))
		 (cond ((null lcs-list) 
			(error "~%Types ~A and ~A have no common ancestor" x y))
		       ((= (length lcs-list) 1) (car lcs-list))
		       (t (error 
			   "~%Types ~A and ~A have multiple common ancestors 
                              ~A" x y lcs-list))))))))))

(defun remove-ancestors (int-list)
   (do* ((done nil (cons initial done))
         (initial (car int-list) (car (set-difference new-int-list done)))
         (new-int-list
            (set-difference int-list (mapcar #'type-name (retrieve-ancestors initial)))
            (set-difference new-int-list (mapcar #'type-name (retrieve-ancestors initial)))))
      ((null (set-difference new-int-list (cons initial done)))
         new-int-list)))


;;; The following utility functions assume that no cycles are present

(defun get-real-types (type)
  (let ((type-entry (get-type-entry type)))
    (if (type-glbp type-entry)
        (loop for parent in (type-parents type-entry)
             append
             (get-real-types parent))
      (list type))))

;;; We need a record of the maximal type at which a particular
;;; feature is introduced.  The following are called from functions
;;; in checktypes.lsp

(defvar *feature-list* (make-hash-table :test #'eq))

(defun clear-feature-table nil
   (clrhash *feature-list*))

(defun maximal-type-of (feature)
   (gethash feature *feature-list*))

(defun set-feature-entry (feature type)
   (setf (gethash feature *feature-list*) type))

(defun check-feature-table nil
   (let ((ok t))
      (maphash
         #'(lambda (feature type-list)
            (cond ((> (length type-list) 1)
                  (format t
                     "~%Feature ~A is introduced at multiple types ~A"
                     feature type-list)
                  (setf ok nil))
               (t
               (set-feature-entry feature (car type-list)))))
         *feature-list*)
      ok))

(defun maximal-type-of-list (features)
   (when features
            (reduce #'(lambda (x y)
                  (unless (or (null x) (null y))
                     (greatest-common-subtype x y)))
               (mapcar #'maximal-type-of features))))
      
;; Remove obsolete pointers from type constraints so that the garbage
;; collector can purge the structures they point to.

(defun gc-types nil
  (maphash #'(lambda (name type)
	       (declare (ignore name))
	       (when (type-tdfs type)
		 (compress-dag (tdfs-indef (type-tdfs type))))
	       (compress-dag (type-constraint type))
	       (compress-dag (type-local-constraint type)))
	   *types*))

;;; Try to reduce the amount of space used by the expanded type hierarchy

(defun clear-glbs nil
  (gc-types)
  (maphash #'(lambda (name type)
	       (when (search "GLBTYPE" (symbol-name name))
		 (setf (type-constraint type) nil)
		 (setf (type-tdfs type) nil)))
	   *types*))

(defun used-types (type)
  (let ((used (mapcar #'(lambda (x) (u-value-type (unification-rhs x)))
		      (type-constraint-spec type))))
    (when used
      (remove-duplicates used))))

(defun purge-constraints nil
  (gc-types)
  (let* ((leaves (mapcar #'(lambda (x) (gethash x *types*)) 
			 (slot-value *leaf-types* 'leaf-types)))
	 (parents 
	  (reduce #'union (mapcar #'type-parents leaves)))
	 (referred
	  (reduce #'union (mapcar #'used-types leaves)))
	 (save (union parents referred)))
    (maphash #'(lambda (name type)
		 (unless (member (symbol-name name) save)
		   ;; (setf (type-constraint type) nil)
		   (setf (type-tdfs type) nil)))
	     *types*)))

(defun types-to-xml (&key (stream t) file)
  (loop
      with stream = (if file
                      (open file
                            :direction :output :if-exists :supersede
                            :if-does-not-exist :create)
                      stream)
      for type being each hash-value in *types*
      for name = (type-name type)
      for parents = (type-parents type)
      for daughters = (type-daughters type)
      do
        (format stream "<type name=\"~(~a~)\">~%  <parents>~%" name)
        (loop
            for parent in parents
            do
              (format stream "    <type name=\"~(~a~)\"/>~%" parent))
        (format stream "  </parents>~%  <children>~%")
        (loop
            for daughter in daughters
            do
              (format stream "    <type name=\"~(~a~)\"/>~%" daughter))
        (format stream "  </children>~%</type>~%")
      finally (when file (close stream))))
