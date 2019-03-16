;;; Copyright (c) 1991--2018 
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `LICENSE' for conditions.


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

(defstruct ltype 
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
           
(defmethod common-lisp:print-object ((instance ltype) stream)
  (if *print-readably*
      ;; print so object can be read back into lisp
      (call-next-method)
    ;; usual case
    (progn 
      (write-string "#<Type " stream)
      (write-string (string (ltype-name instance)) stream)
      (write-char #\> stream))))


(defstruct (leaf-type (:include ltype))
  (expanded-p nil))
        
(defvar *types* (make-hash-table :test #'eq))
#+:sbcl (declaim (sb-ext:always-bound *types* *toptype*))

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
         (setf (ltype-visible-p entry) nil))
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

(defun is-valid-type (x)
   (typecase x
      (null nil)
      (symbol (get-type-entry x))
      (string t)))

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
         (ltype-constraint-spec type-record)
         (error "~%~A is not a valid type" type-name))))

(defun constraint-of (type-name)
   (let ((type-record (get-type-entry type-name)))
      (if type-record 
         (ltype-constraint type-record)
         (error "~%~A is not a valid type" type-name))))

(defun tdfs-of (type-name)
   (let ((type-record (get-type-entry type-name)))
      (if type-record 
         (ltype-tdfs type-record)
         (error "~%~A is not a valid type" type-name))))

(defun default-spec-of (type-name)
   (let ((type-record (get-type-entry type-name)))
      (if type-record 
         (ltype-default-spec type-record)
         (error "~%~A is not a valid type" type-name))))


(defun appropriate-features-of (type-name)
   (let ((type-record (get-type-entry type-name)))
      (if type-record 
         (ltype-appfeats type-record)
         (error "~%~A is not a valid type" type-name))))

(defun retrieve-ancestors (type-name)
  (let ((type-record (get-type-entry type-name)))
    (if type-record 
	(ltype-ancestors type-record)
      (error "~%~A is not a valid type" type-name))))

(defun retrieve-descendants (type-name)
  (let ((type-record (get-type-entry type-name)))
    (if type-record 
	(ltype-descendants type-record)
      (error "~%~A is not a valid type" type-name))))

(defun retrieve-parents (type-name)
  (let ((type-record (get-type-entry type-name)))
    (if type-record 
	(ltype-parents type-record)
      (error "~%~A is not a valid type" type-name))))

(defun retrieve-daughters (type-name)
  (let ((type-record (get-type-entry type-name)))
    (if type-record 
	(ltype-daughters type-record)
      (error "~%~A is not a valid type" type-name))))

(defun subtype-p (type1 type2)
  ;; is type1 a strict subtype of type2?
  ;; robust on invalid type names: if either of the args is not a type, the function
  ;; returns nil and does not signal an error
  (cond
    ((not (symbolp type2)) nil)
    ((symbolp type1)
      ;; an alternative using the type unification machinery would be
      ;; (and (not (eq type1 type2)) (eq (greatest-common-subtype type1 type2) type1))
      ;; but that assumes the args are actually types, and can end up polluting the cache
      ;; with lots of lexical types
      (let ((t2 (get-type-entry type2)))
        (and t2
             (ltype-descendants t2) ; chance to return immediately
             (let ((t1 (get-type-entry type1)))
               (and t1
                    (member t2 (ltype-ancestors t1) :test #'eq))))))
    ((stringp type1)
      (string-type-p type2))))

(defun atomic-type-p (type-name)
  (or (stringp type-name)
      (let ((type-record (get-type-entry type-name)))
	(if type-record 
            (ltype-atomic-p type-record)
	  (error "~%~A is not a valid type" type-name)))))

;;; glb computation - entry point is greatest-common-subtype

;;; Type unification performed by lookup in a global vector. Index is
;;; numeric combination of sxhash values of the two type names
;;; (symbols): sxhash(t1) ^ sxhash(t2) mod 49157 where the two
;;; types are ordered on their sxhash values - so that either order results
;;; in the same table entry being retrieved. Must also test the types
;;; themselves in case another pair of types has the same key

(defconstant +type-cache-size+ (expt 2 16))

(#+:sbcl sb-ext:defglobal #-:sbcl defvar *type-cache*
  (make-array +type-cache-size+ :initial-element nil))

(defstruct type-cache-entry t1 t2 sub con)

(defun clear-type-cache nil
   ;; For consistency this cache must be cleared before (re-)loading a grammar. It's
   ;; probably best also to clear it after loading a grammar and before batch parsing
   ;; since different pairs of types will be exercised
   (fill *type-cache* nil)
   nil)

(defun greatest-common-subtype (type1 type2)
  ;; The args should be symbols or strings. For non-string types the results are cached.
  ;; In practice we encounter only a very small proportion of the possible combinations
  ;; of types, so it would be a bad idea to precompute all results
  (flet
     ((greatest-common-subtype-symbols (t1 t2)
        (declare (symbol t1 t2))
        (cond
           ((eq t1 *toptype*) t2) ; don't cache on top type - result is always the other type
           ((eq t2 *toptype*) t1)
           (t
              (let* ((h1 (sxhash t1))
                     (h2 (sxhash t2))
                     (index (logand (logxor h1 h2) (1- +type-cache-size+))) ; xor commutative
                     (cache *type-cache*))
                 (declare (fixnum h1 h2 index) (simple-vector cache) (optimize (safety 0)))
                 (when (> h1 h2) (rotatef t1 t2)) ; impose a canonical ordering
                 (loop
                    with entries = (svref cache index)
                    for e of-type type-cache-entry in entries
                    when (and (eq (type-cache-entry-t1 e) t1) (eq (type-cache-entry-t2 e) t2))
                    return (values (type-cache-entry-sub e) (type-cache-entry-con e))
                    finally
                    (multiple-value-bind (subtype constraintp)
                         (full-greatest-common-subtype t1 t2)
                       (setf (svref cache index)
                          (cons
                             (make-type-cache-entry :t1 t1 :t2 t2 :sub subtype :con constraintp)
                             entries))
                       (return (values subtype constraintp)))))))))
     (cond
        ((eq type1 type2) type1)
        ((and (symbolp type1) (symbolp type2))
           (greatest-common-subtype-symbols type1 type2))
        ((stringp type1)
           (when (or (and (stringp type2) (string= type1 type2))
                     (string-type-p type2))
              type1))
        ((stringp type2)
           (when (string-type-p type1) type2))
        (t
           (error "Inconsistency - unexpected arguments ~A and ~A to ~S"
              type1 type2 'greatest-common-subtype)))))

#|
;;; investigate effectiveness of greatest common subtype cache
(loop for entries across *type-cache*
   for len = (length entries)
   with stats = nil
   do (let ((x (assoc len stats))) (if x (incf (cdr x)) (push (cons len 1) stats)))
   finally (return (sort stats #'> :key #'car)))
(loop for n from 0 below (length *type-cache*)
   for entries = (svref *type-cache* n)
   when (= (length entries) 4)
   do
   (print n)
   (print (loop for e in entries
                collect (cons (type-cache-entry-t1 e) (type-cache-entry-t2 e)))))
(clear-type-cache)
|#


(defun full-greatest-common-subtype (type1 type2)
  (flet ((intersection-eq (set1 set2)
           (and set1 set2
             (let ((set1-len (length set1)) (set2-len (length set2)))
               (when (> set2-len set1-len)
                 (rotatef set1 set2) ; make set1 be the larger one
                 (rotatef set1-len set2-len))
               (if (> set2-len 20) ; avoid poor performance if both contain >20 elements
                   (let ((table (make-hash-table :test #'eq :size set2-len)) ; the smaller one
                         (res nil))
                     (dolist (e2 set2) (setf (gethash e2 table) t))
                     (dolist (e1 set1 res) (when (gethash e1 table) (push e1 res))))
                   (loop for e2 in set2 ; the smaller one
                     when (member e2 set1 :test #'eq)
                     collect e2))))))
    (let ((t1 (get-type-entry type1))
          (t2 (get-type-entry type2)))
      (cond
        ((eq type1 type2) type1)
        ((member t2 (ltype-ancestors t1) :test #'eq)
           type1)
        ((member t1 (ltype-ancestors t2) :test #'eq)
           type2)
        (t
          (let ((common-subtypes
                  (intersection-eq (ltype-descendants t1) (ltype-descendants t2))))
            (when common-subtypes
              (let
                ((gcsubtype-entries
                   ;; find subtype whose own descendant list is shorter by just 1 (itself)
                   (loop for ty in common-subtypes
                     with sub-len = (1- (length common-subtypes))
                     when (= (length (ltype-descendants ty)) sub-len)
                     collect ty)))
                (cond
                  ((null gcsubtype-entries)
                    (error
"Type hierarchy inconsistent: ~A and ~A have common subtypes but descendant lists are contradictory"
                      type1 type2))
                  ((cdr gcsubtype-entries)
                    (error
"Type hierarchy inconsistent: ~A and ~A have common subtypes but no unique greatest common subtype"
                      type1 type2))
                  (t
                    ;; return true as the second value if there is a constraint that may
                    ;; have to be unified in
                    (values (ltype-name (car gcsubtype-entries))
                            (if (extra-constraint-p (car gcsubtype-entries) t1 t2)
                                t))))))))))))

(defun extra-constraint-p (gcsubtype t1 t2)
  ;;; test is whether any ancestor of the gcsubtype which
  ;;; isn't also an ancestor of the types being unified
  ;;; or the gcsubtype itself introduce any extra information 
  ;;; on the constraint.
  (or (ltype-local-constraint gcsubtype)
      (let ((t1ancs (ltype-ancestors t1))
            (t2ancs (ltype-ancestors t2)))
        (dolist (type (ltype-ancestors gcsubtype))
          (when 
              (and (not (eq type t1))
                   (not (eq type t2))
                   (ltype-local-constraint type)
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
    (let ((z (intersection 
              (cons x (mapcar #'ltype-name (retrieve-ancestors x)))
              (cons y (mapcar #'ltype-name (retrieve-ancestors y))))))
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
            (set-difference int-list (mapcar #'ltype-name (retrieve-ancestors initial)))
            (set-difference new-int-list (mapcar #'ltype-name (retrieve-ancestors initial)))))
      ((null (set-difference new-int-list (cons initial done)))
         new-int-list)))


;;; The following utility functions assume that no cycles are present

(defun get-real-types (type)
  (let ((type-entry (get-type-entry type)))
    (if (ltype-glbp type-entry)
        (loop for parent in (ltype-parents type-entry)
             append
             (get-real-types parent))
      (list type))))

;;; We need a record of the maximal type at which a particular
;;; feature is introduced.  The following are called from functions
;;; in checktypes.lsp

(defvar *feature-list* (make-hash-table :test #'eq))

(defvar *feature-minimal-type* (make-hash-table :test #'eq))

(defun clear-feature-table nil
   (clrhash *feature-minimal-type*)
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
	       (when (ltype-tdfs type)
		 (compress-dag (tdfs-indef (ltype-tdfs type))))
	       (compress-dag (ltype-constraint type))
	       (compress-dag (ltype-local-constraint type)))
	   *types*))

;;; Try to reduce the amount of space used by the expanded type hierarchy

(defun clear-glbs nil
  (gc-types)
  (maphash #'(lambda (name type)
	       (when (search "GLBTYPE" (symbol-name name))
		 (setf (ltype-constraint type) nil)
		 (setf (ltype-tdfs type) nil)))
	   *types*))

(defun used-types (type)
  (let ((used (mapcar #'(lambda (x) (u-value-type (unification-rhs x)))
		      (ltype-constraint-spec type))))
    (when used
      (remove-duplicates used))))

(defun purge-constraints nil
  (gc-types)
  (let* ((leaves (mapcar #'(lambda (x) (gethash x *types*)) 
			 (slot-value *leaf-types* 'leaf-types)))
	 (parents 
	  (reduce #'union (mapcar #'ltype-parents leaves)))
	 (referred
	  (reduce #'union (mapcar #'used-types leaves)))
	 (save (union parents referred)))
    (maphash #'(lambda (name type)
		 (unless (member (symbol-name name) save)
		   ;; (setf (ltype-constraint type) nil)
		   (setf (ltype-tdfs type) nil)))
	     *types*)))

(defun types-to-xml (&key (stream t) file)
  (loop
      with stream = (if file
                      (open file
                            :direction :output :if-exists :supersede
                            :if-does-not-exist :create)
                      stream)
      for type being each hash-value in *types*
      for name = (ltype-name type)
      for parents = (ltype-parents type)
      for daughters = (ltype-daughters type)
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
