(in-package "MRS")

;;; semantics record is intended for all possible semantics 
;;; bearing structures

(defstruct (semantics-record)
  id
  relations)

(defstruct (relation-record)
  relation                 ; i.e. the actual rel
  feature-string)          ; for proper names etc


;;; lexical rules

(defvar *contentless-lrs* nil)
(defvar *contentful-lrs* nil)


(defvar *lrule-rel-index* nil)

(defun clear-lrule-globals nil
  (setf *contentful-lrs* nil)
  (setf *contentless-lrs* nil)
  (setf *lrule-rel-index* nil))


;;; grammar rules

(defvar *grule-rel-index* nil)

(defvar *contentless-grs* nil)
(defvar *contentful-grs* nil)

(defun clear-grule-globals nil
  (setf *contentless-grs* nil)
  (setf *contentful-grs* nil)
  (setf *grule-rel-index* nil))

(defun extract-rule-rels (id fs entry lexicalp)
  (let* ((construction-semantics-fs 
          (path-value fs *construction-semantics-path*))
         (construction-rels 
          (if construction-semantics-fs
              (extract-relations-from-liszt 
               construction-semantics-fs id 
               *construction-semantics-path* fs))))
    (if construction-rels
        (let* ((new-record
                (make-semantics-record
                 :id id
                 :relations construction-rels)))
          (for rel in (find-index-rels (mapcar #'relation-record-relation
                                               construction-rels) id)
               do
               (let ((res (cons rel new-record)))
                 (if lexicalp (push res *lrule-rel-index*) 
                   (push res *grule-rel-index*))))
          (if lexicalp (push new-record *contentful-lrs*)
            (push  new-record *contentful-grs*)))
      (if lexicalp (push entry *contentless-lrs*)
        (push entry *contentless-grs*)))))


(defparameter *maximum-genindex-relations* 100 
  "maximum number of relation types allowed in indexing")

(defun find-index-rels (reltype-list id)
  ;;; returns a list of all the types under the
  ;;; maximal relation-type (e.g., relation) which
  ;;; are compatible with one or more members of the type list
  ;;; Because it's easy for Dan to make errors, by introducing underspecified
  ;;; types where he shouldn't, there's a maximum number of relations
  ;;; allowed, which is set quite high, because of all the glbtypes
  (let ((returned-rels nil))
    (for reltype in reltype-list
         do
         (for compatible-rel in (cl-user::get-compatible-rels reltype)
              do
              (pushnew compatible-rel returned-rels :test #'eq)))
    (when (and *maximum-genindex-relations* 
               (> (length returned-rels) *maximum-genindex-relations*))
      (warn "~%Too many subtypes of relation in ~A: ignored" id)
      (setf returned-rels nil))
    returned-rels))

;;; indexing and retrieving a lexical entry based on some input semantics

;;; Quite grammar specific (though actual feature paths etc are
;;; in mrsglobals-eng)
;;;
;;; In this version there is now only one possible location for the 
;;; semantics (hurrah!!!)
;;; it may have `dummy' relations which will never turn up in the real 
;;; semantics.  These are ignored when indexing.
;;;
;;; If a lexical entry has no detectable semantics, a
;;; warning message is issued and the id is stored on
;;; *empty-semantics-lexical-entries*

(defvar *empty-semantics-lexical-entries* nil)

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

(defun index-simple-semantics-record (rel-name id)
  (let ((rel-value (gethash rel-name *relation-index*)))
    (if (or (null rel-value) (consp rel-value))
        (pushnew id (gethash rel-name *relation-index*) :test #'eq)
      (progn (warn "~%Ignoring entry ~A - Existing value of ~A in *relation-index* isn't a cons" id rel-name)
             nil))))

(defun index-complex-semantics-record (rel id)
  ;;; just index these things on the specific relation rather
  ;;; than all compatible-semantic-types
  (let* ((rel-name (relation-record-relation rel))
        (rel-string (relation-record-feature-string rel))
        (rel-value (gethash rel-name *relation-index*)))
    (unless rel-string
      (error "~%index-complex-semantics-record called on non-complex relation ~A" rel))
    (if rel-value
        (if (hash-table-p rel-value)
            (pushnew id (gethash rel-string rel-value))  
          (progn 
            (warn "~%Ignoring entry ~A - Existing value of ~A in *relation-index* isn't a hash table" 
                  id rel-name)
            nil))
      (progn (setf rel-value (make-hash-table :test #'equal))
             (setf (gethash rel-name *relation-index*)
               rel-value)
             (pushnew id (gethash rel-string rel-value))))))


;;; extracting semantics from expanded lexical entries
;;; Note that rels are kept in the order they have in the entries


;;; (mrs::extract-lexical-relations (get-psort-entry 'andes_1))
;;; (mrs::extract-lexical-relations (get-psort-entry 'kim_1))
;;; (mrs::extract-lexical-relations (get-psort-entry 'to_do_with))



(defun extract-lexical-relations (lex-entry)
  (let* ((fs (tdfs-indef (lex-or-psort-full-fs lex-entry)))
         (id  (lex-or-psort-id lex-entry))
         (main-semantics-fs (path-value fs *main-semantics-path*))
         (main-rels (if main-semantics-fs
                        (extract-relations-from-liszt 
                         main-semantics-fs id 
                         *main-semantics-path* fs))))
    (if main-rels
        (let* ((new-record
                (make-semantics-record
                 :id id
                 :relations main-rels)))
          (add-semantics-record id new-record)
          (for rel in (find-index-rels 
                       (mapcar #'relation-record-relation 
                               (remove-if #'relation-record-feature-string 
                                          main-rels)) id)
               do
               (index-simple-semantics-record rel id))
          (for rel in (remove-if-not #'relation-record-feature-string 
                                          main-rels)
               do
               (index-complex-semantics-record rel id)))
      (progn (format t "~%Warning ~A has no semantics" id)
             (pushnew id *empty-semantics-lexical-entries*)))))



(defun extract-relations-from-liszt (fs id path old-fs)
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
                  (if (empty-diff-list-p fs path old-fs)
                      (progn
                        (format t 
                                "~%Warning: ~A has an empty LISZT with a relation in it" id)
                        nil)
                    (cons rel
                          (if rest-part
                              (extract-relations-from-liszt
                               (cdr rest-part) id path old-fs)
                            (format t 
                                    "~%Warning: ~A has a defective ~A" id path))))
                (if rest-part
                    (progn
                      (format t "~%Warning: ~A has a gap in its ~A" id path)
                      nil))))))))

(defun empty-diff-list-p (fs full-path oldfs)
  (if (eql (car (last full-path)) user::*diff-list-list*)
      (eq (path-value oldfs (append (butlast full-path) 
                                    (list user::*diff-list-last*)))
          fs)))
                  
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
