(in-package "USER")

(defun index-for-generator nil
  (index-lexicon)
  (index-lexical-rules)
  (mrs::instantiate-null-semantic-items))
 

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
                 (file-pointer (car hash-table-entry)))
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
              (setf (cdr (gethash id *psorts*)) nil)))) ; clear structure
     (setf *batch-mode* nil)))

(defun index-lexical-rules nil
  (maphash #'(lambda (id entry)
               (let* ((tdfs (rule-full-fs entry))
                      (fs (if tdfs (tdfs-indef tdfs))))
                 (if fs
                     (mrs::extract-lex-rule-rels id fs))))
           *lexical-rules*))           
                           
                 
  


(in-package "MRS")


(defparameter *contentful-lrs* nil)
(defparameter *contentless-lrs* nil)

;;; lexical rules
;;; Interim stuff for excluding lexical rules
;;; which have semantics

(defun extract-lex-rule-rels (id fs)
  (let* ((semantic-fs
         (path-value fs *main-semantics-path*))
        (rels      
         (if semantic-fs
             (extract-relations-from-liszt 
              semantic-fs id))))
    (if rels
        (push id *contentful-lrs*)
      (push id *contentless-lrs*))))


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

(defparameter *empty-semantics-lexical-entries* nil)

;;; semantics record is intended for all possible semantics 
;;; bearing structures

(defstruct (semantics-record)
  id
  main-relations           ; main lexical
  alternate-relations      ; alternate lexical
  message-relations         ; message lexical
  c-cont-relations         ; grammar rules (constructional content)
  lrule-cont-relations     ; lexical rules
  root-cont-relations)     ; roots

(defstruct (relation-record)
  relation                 ; i.e. the actual rel
  feature-string)          ; for proper names etc

(defparameter *semantic-table* (make-hash-table)
 "semantics associated with each instance (psort)")
;;; indexed by identifier, values are semantics-record

(defun add-semantics-record (id record)
  (setf (gethash id *semantic-table*)
        record))

(defparameter *relation-index* (make-hash-table)
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


; (mrs::extract-lexical-relations (get-psort-entry 'FORTY_W_COMPS))
#|
(index-lexicon)
|#

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


(defun extract-relations-from-liszt (fs id)
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
                             (cdr rest-part) id)))
                (if rest-part
                    (progn
                    (format t "~%Warning: ~A has a gap in its liszt" id)
                    nil))))))))
                  
(defun extract-relation-from-fs (fs id)
  ;;; two cases - normal relation and string-valued relation
  (if (is-valid-fs fs)
      (let* ((fs-type (fs-type fs))
             (real-type (create-type fs-type)))
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
             :feature-string (car string-values)))))))

