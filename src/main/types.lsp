;;; Copyright Ann Copestake 1991-1997. All Rights Reserved.
;;; No use or redistribution without permission.
;;; 

;;; modifications for YADU - April 1997
;;; bug fixes etc 1995
;;; July 1996 - cacheing glbs
;;; structure mod to allow glbs to be calculated 


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
;;; atomic-p - t if the type has no appropriate features and none of
;;;            its subtypes have any appropriate features
;;; July 1996 
;;; glbp - t if type was automatically created
;;; May 1997
;;; descendants - for glb stuff
;;; Feb 1998
;;; template-parents - hack so that if multiple parents are
;;;        specified, under certain conditions (user-controlled global) 
;;;        all but one of them can be treated as a template
;;;        thus simplifying the hierarchy


(defstruct type 
           name parents constraint (constraint-mark nil) tdfs
           comment
           daughters appfeats enumerated-p ancestors marks
           constraint-spec default-spec local-constraint atomic-p glbp
           descendants
           template-parents
           shrunk-p visible-p)          ; for display in type hierarchy

(defstruct (leaf-type (:include type))
  (expanded-p nil))
        
(defvar *types* (make-hash-table :test #'eq))

(defparameter *default-abbreviations* nil)

(defvar *types-changed* nil)
(defvar *lexicon-changed* nil)
(defvar *sign-types* nil)

(defvar *leaf-types* nil)

(defun clear-types nil
   (disable-type-interactions)
   (setf *toptype* nil)
   (setf *sign-types* nil)
   (clrhash *types*)
   (setf *leaf-types* nil)
   (clear-type-cache)
   (clear-feature-table)
   (clear-expanded-lex))

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

; slightly kludgy function for parser compilation

(defun get-possible-sign-types nil
   (or *sign-types*
      (let ((sign-entry (get-type-entry *sign-type*)))
         (if sign-entry 
            (setf *sign-types* 
               (cons *sign-type* (get-descendants sign-entry)))
            (maphash 
               #'(lambda (type value)
                  (declare (ignore value))
                  (push type *sign-types*))
               *types*))
         *sign-types*)))

; turn some of the following into macros when everything seems to work OK

(defun collect-type-names nil
   ;; work around Procyon bug - see checktypes.lsp
   (let ((type-names nil))
      (maphash #'(lambda (name entry) 
                   (declare (ignore entry))
                   (push name type-names))
         *types*)
      type-names))

;; Making this a macro cuts 20 seconds off loading time for the LinGO grammar

#|
(defun get-type-entry (name)
   (gethash name *types*))
|#

(defmacro get-type-entry (name)
   `(gethash ,name *types*))

(defun get-any-type-entry (name)
  ;;; allows for instance types
  (let ((type-parent-name (instance-type-parent name)))
    (gethash (or type-parent-name name) *types*)))
  
(defun is-valid-type (name)
   (or (get-type-entry name)
      (stringp name)
      (instance-type-parent name)))

; instance-types added for MT removed for GLB stuff
(defun instance-type-parent (name)
   (locally
      (declare (optimize (speed 3) (safety 0)) (inline symbolp symbol-name schar))
      (and (symbolp name)
         ;; they start with % so we can quickly test without searching the
         ;; property list
         (eql (the character
                 (schar (the simple-string (symbol-name (the symbol name))) 0))
              #\%)
         (get name 'root-template))))

(defun string-type-p (type-name)
   ;;; AAC 30/12/94
   ;;; allow for no string type
   (and *string-type*
      (or (eq type-name *string-type*)
         (member type-name (retrieve-ancestors *string-type*) :test #'eq))))

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

(defun retrieve-ancestors-any-type (type-name)
   (let ((type-record (get-type-entry type-name)))
      (cond 
         (type-record 
            (type-ancestors type-record))
         ((stringp type-name)
            (if *string-type*
              (cons *string-type* 
                 (retrieve-ancestors *string-type*))))
;;;         ((instance-type-parent type-name) ?)
         (t 
            (error "~%~A is not a valid type" type-name)))))

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
      (let ((ptype (instance-type-parent type1)))
         (if ptype
            (or (eq ptype type2)
               (member type2 (retrieve-ancestors ptype)))
            (member type2 (retrieve-ancestors type1))))))


(defun atomic-type-p (type-name)
   (or (stringp type-name)
      (let ((type-record (get-type-entry type-name)))
         (if type-record 
            (type-atomic-p type-record)
            (let ((ptype (instance-type-parent type-name)))
               (if ptype 
                  (atomic-type-p ptype)
                  (error "~%~A is not a valid type" type-name)))))))


;;; glb computation - entry point is greatest-common-subtype

(defparameter *type-cache* nil)

(defun clear-type-cache nil
   ;; it's probably best to clear this out occasionally - definitely after loading
   ;; a grammar and before parsing since different pairs of types will be exercised
   (setf *type-cache* nil))

#|
;;; investigate effectiveness of subtype cache
(let ((max 0) (longest nil))
   (maphash
      #'(lambda (key entry)
           (when (hash-table-p entry) (print (list key entry)))
           (when (and (consp entry) (> (length entry) max))
              (setq max (length entry) longest (cons key entry))))
      *type-cache*)
   (values max longest))
|#

(defun greatest-common-subtype (type1 type2)
   ;; implemented as a memo function. In practice we won't see anything like all
   ;; possible combinations of arguments so best not to attempt to pre-compute the
   ;; cache contents
   ;;
   ;; we expect both args to be lisp atoms, but either or both could be strings/
   ;; string type, or instance types. The latter are cached, but not the former
   (cond 
      ((eq type1 type2) type1)
      ((arrayp type1) ; a string?
         (when (or (equal type1 type2) (string-type-p type2))
            type1))
      ((arrayp type2) 
         (when (string-type-p type1) type2))
      (t
         (cached-greatest-common-subtype type1 type2 nil))))


(defun cached-greatest-common-subtype (type1 type2 type1-atomic-p)
   ;; NB as implemented only works for types which satisfy eq test for equality i.e.
   ;; symbols - types which are integers etc are not cached
   ;; Also minimal extension to deal with first type arg being an atomic type, but
   ;; restricted to containing a single disjuct which itself is just a symbol,
   ;; signalled by 3rd arg being true. In this case the subtype if it exists must
   ;; necessarily be atomic with a single disjunct
   ;; Index first on potentially atomic type, since there are many of more of these
   ;; Implemented as first a hash table, with each bucket being either an alist -
   ;; or if more than 30 entries, a hash table itself
   ;; Returns two values: greatest common subtype of type1 and type2 (or nil) and
   ;; true if subtype result has a constraint which needs to be unified in
   (unless *type-cache*
      ;; try to minimise number of collisions by giving table a generous initial
      ;; size - we really need speed here
      (setq *type-cache*
         (make-hash-table :test #'eq :size (* (hash-table-count *types*) 2))))
   ;; many subtype results are same as type2 - so we could perhaps use a more compact
   ;; storage representation for these cases
   (let*
      ((type1-index (if type1-atomic-p (car type1) type1))
       (entry (gethash type1-index *type-cache*))
       (found (typecase entry
                 (null nil)
                 (cons (cdr (assoc type2 entry :test #'eq)))
                 (t (gethash type2 entry)))))
      (if found
         (values (car found) (cdr found))
         (multiple-value-bind (subtype constraintp)
               (full-greatest-common-subtype type1-index type2)
            (when (and subtype type1-atomic-p) (setq subtype (list subtype)))
            (typecase entry
               ((or cons null) ; entry is an alist
                  (if (> (length entry) 7)
                     ;; convert alist into a hash table since it's got bigger than
                     ;; an average empirical break-even point for gethash vs assoc
                     (let ((new (make-hash-table :test #'eq :size (* (length entry) 2))))
                        (dolist (item entry)
                           (setf (gethash (car item) new) (cdr item)))
                        (setf (gethash type2 new) (cons subtype constraintp))
                        (setf (gethash type1-index *type-cache*) new))
                     ;; add to end of list since it's likely to be less frequent (maybe)
                     (setf (gethash type1-index *type-cache*)
                        (nconc entry
                           (list (cons type2 (cons subtype constraintp)))))))
               (t ; entry is a hash table
                  (setf (gethash type2 entry) (cons subtype constraintp))))
            (values subtype constraintp)))))


(defun full-greatest-common-subtype (type1 type2 &aux ptype1 ptype2)
   ;; atomic types should have been stripped down to their constituent type
   ;; component(s) before this point
   (cond 
      ((eq type1 type2) type1)
      ((and (setq ptype1 (instance-type-parent type1))
            (or (eq ptype1 type2) (member type2 (retrieve-ancestors ptype1) :test #'eq)))
         ;; no need to look for constraint of ptype since it's already the glb
         type1)
      ((and (setq ptype2 (instance-type-parent type2))
            (or (eq ptype2 type1) (member type1 (retrieve-ancestors ptype2) :test #'eq)))
         type2) ; ditto
      ((or ptype1 ptype2)
         nil)
      ((member type2 (retrieve-ancestors type1) :test #'eq)
         type1)
      ((member type1 (retrieve-ancestors type2) :test #'eq)
         type2)
      (t (let ((common-subtypes 
                  (retrieve-common-descendants type1 type2)))
            (if common-subtypes
               (let ((greatest-common-subtype-list
                        (intersection
                           common-subtypes 
                           (reduce #'intersection 
                              (mapcar #'(lambda (subtype)
                                    (cons subtype 
                                       (retrieve-ancestors subtype)))
                                 common-subtypes)))))
                  (cond ((eql (length greatest-common-subtype-list) 1)
                           (let ((gcsubtype (car greatest-common-subtype-list)))
                              (values gcsubtype (if (constraint-of gcsubtype) t))))
                     ;; return true as the second value if there is a constraint
                     ;; that may have to be unified in
                     (greatest-common-subtype-list
                        (error "~%~A and ~B have multiple common subtypes "
                           greatest-common-subtype-list))
                     (t (error 
                           "~%Error found in type hierarchy")))))))))


;;; when called from generalisation this should only take non-atomic types 
;;; as arguments and so disjunctions are not taken into consideration
;;; e.g. if 
;;; language (top) (OR english dutch italian spanish)
;;; 
;;; (least-common-supertype 'english 'dutch)
;;; will return language

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
      ((instance-type-parent x) 
         (least-common-supertype (instance-type-parent x) y))
      ((instance-type-parent y) 
         (least-common-supertype x (instance-type-parent y)))
      (t
         (let ((z (intersection (cons x (retrieve-ancestors x))
                     (cons y (retrieve-ancestors y)))))
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

;;; The following utility functions assume that no cycles are present

(defun retrieve-common-descendants (type1 type2)
   (intersection (retrieve-descendants type1)
      (retrieve-descendants type2)))

(defun get-ancestors (type-entry)
   (let ((parents (type-parents type-entry)))
      (if parents
         (union parents
            (reduce #'union
               (mapcar #'(lambda (parent)
                     (get-ancestors (get-type-entry parent)))
                  parents))))))

(defun get-descendants (type-entry)
      (let ((daughters (type-daughters type-entry)))
      (if daughters
         (union daughters
            (reduce #'union
               (mapcar #'(lambda (daughter)
                     (get-descendants (get-type-entry daughter)))
                  daughters))))))



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
      

