(in-package "USER")

(defun index-for-generator nil
  (index-lexicon)
  (index-lexical-rules)
  (index-grammar-rules)
  (mrs::instantiate-null-semantic-items)
  nil)
 

(defun index-lexicon nil
  (mrs::clear-semantic-indices)
   (setf *batch-mode* t)
   (let ((ids nil))
     (maphash 
      #'(lambda (name val)
          (declare (ignore name))
          (for id in val
               do
               (pushnew id ids)))
      ; because of multiple lexical entries,
      ; an id may be indexed by multiple orthographies
      *lexical-entries*)
     (for id in ids
          do
          (let* ((hash-table-entry (gethash id *psorts*))
                 (file-pointer (cadr hash-table-entry)))
            (when (integerp file-pointer)
              (let* 
                  ((entry 
                    (cdr (read-psort-entry-from-file 
                          file-pointer id)))
                   (lex-id (lex-or-psort-id entry)))
                (expand-psort-entry entry)
                (let ((new-fs (lex-or-psort-full-fs entry)))
                  (if new-fs
                      (mrs::extract-lexical-relations entry)
                    (format t "~%No feature structure for ~A" lex-id))))
              (setf (cddr (gethash id *psorts*)) nil)))) ; clear structure
     (setf *batch-mode* nil)))

  
(defun index-lexical-rules nil
  (mrs::clear-lrule-globals)
  (maphash #'(lambda (id entry)
               (let* ((tdfs (rule-full-fs entry))
                      (fs (if tdfs (tdfs-indef tdfs))))
                 (if fs
                     (mrs::extract-lex-rule-rels id fs))))
           *lexical-rules*))           
                           

(defun index-grammar-rules nil
  (mrs::clear-grule-globals)
  (maphash #'(lambda (id entry)
               (let* ((tdfs (rule-full-fs entry))
                      (fs (if tdfs (tdfs-indef tdfs))))
                 (if fs
                     (mrs::extract-grammar-rule-rels id fs))))
           *rules*))
                 
;;; actually used by lexlookup, but convenient to define in USER

(defun make-mrs-unifs (fvplist initial-features)
  (let ((initial-path (create-typed-path-from-feature-list initial-features)))
    (for fvp in fvplist
         filter
         (let ((value (mrs:fvpair-value fvp)))
           (if (listp value)
               nil
             (make-unification :lhs (make-typed-path 
                                     :typed-feature-list
                                     (append (path-typed-feature-list
                                              initial-path)
                                             (path-typed-feature-list
                                              (mrs:fvpair-feature fvp))))
                               :rhs (make-u-value :types
                                                  (list (deasterisk value)))))))))

(defun deasterisk (value)
  (or
   (and (not (stringp value))
        (let* ((ancestors (mapcar #'type-name (retrieve-ancestors value)))
               (strict-type
                (if ancestors
                    (find-strict-type (cons value ancestors)))))
          (if strict-type
              (find-gcsubtype strict-type value))))
   value))

(defparameter *strict-pairs*
    '((pernum . strict_pn)
      (gender . strict_gen)
      (mood . strict_mood)
      (conj . strict-conj) ; sic - dash not underscore
      (tense . strict_tense)))
      
(defun find-strict-type (types)
  (dolist (type types)
    (let ((strict-type
           (dolist (strp *strict-pairs*)
             (when (eq type (car strp))
               (return (cdr strp))))))
      (when strict-type
        (return strict-type)))))
       


(in-package "MRS")

;;; semantics record is intended for all possible semantics 
;;; bearing structures

(defstruct (semantics-record)
  id
  main-relations           ; main (lexicon, lrules, grules)
  alternate-relations      ; alternate (lexicon only)
  message-relations        ; message (lexicon, lrules, grules)
  c-cont-relations)        ; grammar rules


;;; lexical rules

(defvar *contentful-lrs* nil)
(defvar *contentless-lrs* nil)

(defvar *lrule-rel-index* nil)

(defun clear-lrule-globals nil
  (setf *contentful-lrs* nil)
  (setf *contentless-lrs* nil)
  (setf *lrule-rel-index* nil))

(defun extract-lex-rule-rels (id fs)
  (let* ((main-semantics-fs (path-value fs *main-semantics-path*))
         (main-rels (if main-semantics-fs
                        (extract-relations-from-liszt 
                         main-semantics-fs id)))
; alternate semantics should never be relevant for
; lexical or grammar rules
         (message-semantics-fs (path-value fs *message-semantics-path*))
         (message-rels
          (if message-semantics-fs
              (extract-relations-from-liszt 
               message-semantics-fs id))))
    (if (or main-rels message-rels)
        (let* ((new-record
                (make-semantics-record
                 :id id
                 :main-relations main-rels
                 :message-relations message-rels)))
          (for rel in  main-rels
               do
               (push (cons (relation-record-relation rel) new-record) 
                     *lrule-rel-index*))
          (for rel in message-rels
               do
               (push (cons (relation-record-relation rel) new-record) 
                     *lrule-rel-index*))
          (if main-rels
            (push id *contentful-lrs*)
            (push id *contentless-lrs*)))
        (push id *contentless-lrs*))))


;;; grammar rules

(defvar *grule-rel-index* nil)

(defvar *contentful-grs* nil)
(defvar *contentless-grs* nil)

(defun clear-grule-globals nil
  (setf *contentful-grs* nil)
  (setf *contentless-grs* nil)
  (setf *grule-rel-index* nil))

(defun extract-grammar-rule-rels (id fs)
  (let* ((construction-semantics-fs 
          (path-value fs *construction-semantics-path*))
         (construction-rels (if construction-semantics-fs
                        (extract-relations-from-liszt 
                         construction-semantics-fs id)))
         (main-semantics-fs (path-value fs *main-semantics-path*))
         (main-rels (if main-semantics-fs
                        (extract-relations-from-liszt 
                         main-semantics-fs id)))
; alternate semantics should never be relevant for
; lexical or grammar rules
         (message-semantics-fs (path-value fs *message-semantics-path*))
         (message-rels
          (if message-semantics-fs
              (extract-relations-from-liszt 
               message-semantics-fs id))))
    (if (or main-rels message-rels construction-rels)
        (let* ((new-record
                (make-semantics-record
                 :id id
                 :c-cont-relations construction-rels
                 :main-relations main-rels
                 :message-relations message-rels)))
          (for rel in construction-rels
               do
               (push (cons (relation-record-relation rel) new-record) 
                     *grule-rel-index*))
          (for rel in  main-rels
               do
               (push (cons (relation-record-relation rel) new-record) 
                     *grule-rel-index*))
          (for rel in message-rels
               do
               (push (cons (relation-record-relation rel) new-record) 
                     *grule-rel-index*))
          (if (or main-rels construction-rels)
            (push id *contentful-grs*)))
        (push id *contentless-grs*))))

;;; indexing and retrieving a lexical entry based on some input semantics

;;; Quite grammar specific (though actual feature paths etc are
;;; in mrsglobasl-eng), this assumes there are three
;;; possible locations for semantic information in a lexical
;;; entry, which have to be kept distinct
;;; 1. main relation LISZT
;;; 2. an alternative LISZT which is used so that
;;;    two semantic structures can be `packed' into a single
;;;    representation
;;; 3. a `message' - essentially this is contributed
;;;    by a construction, but in some cases has to be supplied
;;;    to the construction by a particular lexical entry
;;;
;;; All these locations may have `dummy' relations
;;; used to block parses, which will never turn up in the real 
;;; semantics.  These are ignored when indexing.
;;;
;;; If a lexical entry has no detectable semantics, a
;;; warning message is issued and the id is stored on
;;; *empty-semantics-lexical-entries*

(defvar *empty-semantics-lexical-entries* nil)


(defstruct (relation-record)
  relation                 ; i.e. the actual rel
  feature-string)          ; for proper names etc

(defvar *semantic-table* (make-hash-table)
 "semantics associated with each instance (psort)")
;;; indexed by identifier, values are semantics-record

(defun add-semantics-record (id record)
  (setf (gethash id *semantic-table*)
        record))

(defvar *relation-index* (make-hash-table)
 "associates relations with instances") 
;;; indexed by relation - values are either simply a list of 
;;; identifiers of lexical entries which have this relation
;;; in their semantic record or, in the case
;;; of string-featured-relations, a secondary hash table
;;; which gives identifiers of relations with those
;;; strings/symbols as values of the associated
;;; feature

;;; Following are called from a function in main/batch-check

(defun clear-semantic-indices nil
  (clrhash *semantic-table*)
  (clrhash *relation-index*)
  (setf *empty-semantics-lexical-entries* nil))

; indexing extracted semantics

(defun index-semantics-record (rel id)
  (let* ((rel-name (relation-record-relation rel))
        (rel-string (relation-record-feature-string rel))
        (rel-value (gethash rel-name *relation-index*)))
    (if rel-string
        (if rel-value
            (if (hash-table-p rel-value)
                (pushnew id (gethash rel-string rel-value))                  
              (error "~%Existing value of ~A in *relation-index* 
                        isn't a hash table"
                     rel-name))
          (progn (setf rel-value (make-hash-table :test #'equal))
                 (setf (gethash rel-name *relation-index*)
                       rel-value)
                 (pushnew id (gethash rel-string rel-value))))
      (if (or (null rel-value) (consp rel-value))
          (pushnew id (gethash rel-name *relation-index*))
          (error "~%Existing value of ~A in *relation-index* 
                        isn't a cons" rel-name)))))

;;; extracting semantics from expanded lexical entries
;;; Note that rels are kept in the order they have in the entries


; (mrs::extract-lexical-relations (get-psort-entry 'andes_1))
; (mrs::extract-lexical-relations (get-psort-entry 'kim_1))


(defun extract-lexical-relations (lex-entry)
  (let* ((fs (tdfs-indef (lex-or-psort-full-fs lex-entry)))
         (id  (lex-or-psort-id lex-entry))
         (main-semantics-fs (path-value fs *main-semantics-path*))
         (main-rels (if main-semantics-fs
                        (extract-relations-from-liszt 
                         main-semantics-fs id)))
         (alternate-semantics-fs (path-value fs *external-semantics-path*))
         (alternate-rels (if alternate-semantics-fs
                        (extract-relations-from-liszt 
                         alternate-semantics-fs id)))
         (message-semantics-fs (path-value fs *message-semantics-path*))
         (message-rels
          (if message-semantics-fs
              (extract-relations-from-liszt 
               message-semantics-fs id))))
    (if (or main-rels alternate-rels message-rels)
        (let* ((new-record
                (make-semantics-record
                 :id id
                 :main-relations main-rels
                 :alternate-relations alternate-rels
                 :message-relations message-rels)))
          (add-semantics-record id new-record)
          (for rel in  main-rels
               do
               (index-semantics-record rel id))
          (for rel in alternate-rels
               do
               (index-semantics-record rel id))
          (for rel in message-rels
               do
               (index-semantics-record rel id)))
      (progn (format t "~%Warning ~A has no semantics" id)
             (pushnew id *empty-semantics-lexical-entries*)))))


(defun extract-relations-from-liszt (fs id &optional last-fs path)
  ;;; similar to the mrsoutput fn, construct-liszt
  (if (is-valid-fs fs)
      (let ((label-list (fs-arcs fs)))
        (if label-list
            (let* ((first-part (assoc (car *liszt-first-path*)
                                     label-list))
                   (rel (if first-part
                            (extract-relation-from-fs 
                             (cdr first-part) id)))
                   (rest-part (assoc (car *liszt-rest-path*)
                                    label-list)))
              (if rel
                  (cons rel
                        (if rest-part
                            (extract-relations-from-liszt
                             (cdr rest-part) id last-fs path)
                            (format t 
                                    "~%Warning: ~A has a defective ~A" id path)))
                (if rest-part
                    (progn
                    (format t "~%Warning: ~A has a gap in its ~A" id path)
                    nil))))
            (when last-fs
              (unless (eq last-fs fs)
                (format t 
                        "~%Warning: ~A has a non-terminated ~A" id path)
                nil))))))
                  
(defun extract-relation-from-fs (fs id)
  ;;; two cases - normal relation and string-valued relation
  (if (is-valid-fs fs)
      (let* ((reln_type (extract-relation-type fs))
             (real-type (create-type reln_type)))
        (when real-type
            (when (listp real-type)
              (error "~%Disjunction not expected in ~A" id))
            (unless (member real-type *dummy-relations*)
              (let* ((label-list (fs-arcs fs))
                     (string-values
                      (for pair in label-list
                           filter
                           (if (member (car pair) 
                                       *value-feats*)
                             (create-type
                              (fs-type (cdr pair)))))))
                (when (cdr string-values)
                  (error "~%Multiple string values not expected in ~A
                      values ~A labels ~A" id string-values label-list)) 
                (make-relation-record 
                 :relation real-type
                 :feature-string (car string-values))))))))


(defun extract-relation-type (fs)
  (if *rel-name-path-only* 
    (let ((reln-res (assoc (car *rel-name-path*) (fs-arcs fs))))
      (if reln-res (fs-type (cdr reln-res))))
    (fs-type fs)))
