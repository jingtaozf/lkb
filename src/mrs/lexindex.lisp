(in-package "MRS")

;;; semantics record is intended for all possible semantics 
;;; bearing structures

(defstruct (semantics-record)
  id
  main-relations           ; main (lexicon, lrules, grules)
  message-relations        ; message (lexicon, lrules, grules)
  c-cont-relations)        ; grammar rules


;;; lexical rules

(defvar *contentless-lrs* nil)
(defvar *contentful-lrs* nil)


(defvar *lrule-rel-index* nil)

(defun clear-lrule-globals nil
  (setf *contentful-lrs* nil)
  (setf *contentless-lrs* nil)
  (setf *lrule-rel-index* nil))

(defun extract-lex-rule-rels (id fs entry)
  (let* ((main-semantics-fs (path-value fs *main-semantics-path*))
         (main-rels (if main-semantics-fs
                        (extract-relations-from-liszt 
                         main-semantics-fs id *main-semantics-path*)))
         (message-semantics-fs (path-value fs *message-semantics-path*))
         (message-rels
          (if message-semantics-fs
              (extract-relations-from-liszt 
               message-semantics-fs id *message-semantics-path*))))
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
          (push new-record *contentful-lrs*))
        (push entry *contentless-lrs*))))


;;; grammar rules

(defvar *grule-rel-index* nil)

(defvar *contentless-grs* nil)
(defvar *contentful-grs* nil)

(defun clear-grule-globals nil
  (setf *contentless-grs* nil)
  (setf *contentful-grs* nil)
  (setf *grule-rel-index* nil))

(defun extract-grammar-rule-rels (id fs entry)
  (let* ((construction-semantics-fs 
          (path-value fs *construction-semantics-path*))
         (construction-rels (if construction-semantics-fs
                        (extract-relations-from-liszt 
                         construction-semantics-fs id 
                         *construction-semantics-path*)))
         ;;; shouldn't be anything on main-semantics path
         ;;; but leave this here for checking
         (main-semantics-fs (path-value fs *main-semantics-path*))
         (main-rels (if main-semantics-fs
                        (extract-relations-from-liszt 
                         main-semantics-fs id *main-semantics-path*)))
         (message-semantics-fs (path-value fs *message-semantics-path*))
         (message-rels
          (if message-semantics-fs
              (extract-relations-from-liszt 
               message-semantics-fs id *message-semantics-path*))))
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
          (push new-record *contentful-grs*))
        (push entry *contentless-grs*))))

;;; indexing and retrieving a lexical entry based on some input semantics

;;; Quite grammar specific (though actual feature paths etc are
;;; in mrsglobasl-eng), this assumes there are two
;;; possible locations for semantic information in a lexical
;;; entry, which have to be kept distinct
;;; 1. main relation LISZT
;;; 2. a `message' - essentially this is contributed
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
(defvar *message-only-rels* nil)


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
  (setf *empty-semantics-lexical-entries* nil)
  (setf *message-only-rels* nil))

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
                         main-semantics-fs id *main-semantics-path*)))
         (message-semantics-fs (path-value fs *message-semantics-path*))
         (message-rels
          (if message-semantics-fs
              (extract-relations-from-liszt 
               message-semantics-fs id *message-semantics-path*))))
    (when (or main-rels message-rels)
        (let* ((new-record
                (make-semantics-record
                 :id id
                 :main-relations main-rels
                 :message-relations message-rels)))
          (add-semantics-record id new-record)
          (for rel in  main-rels
               do
               (index-semantics-record rel id))
          (for rel in message-rels
               do
               (index-semantics-record rel id))
          (unless main-rels
            (push (cons id message-rels) *message-only-rels*))))
    (unless main-rels
      (progn (format t "~%Warning ~A has no semantics" id)
             (pushnew id *empty-semantics-lexical-entries*)))))



(defun extract-relations-from-liszt (fs id path)
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
                             (cdr rest-part) id path)
                            (format t 
                                    "~%Warning: ~A has a defective ~A" id path)))
                (if rest-part
                    (progn
                    (format t "~%Warning: ~A has a gap in its ~A" id path)
                    nil))))))))

                  
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
