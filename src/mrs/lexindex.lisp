;;; Copyright (c) 1998-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions

(in-package "MRS")

;;; 1. data structures for the storage of semantic information 
;;; associated with relations
;;; - i.e. the semdb (still rather rudimentary)
;;; 2. association of relations with lexical entries, lexical rules
;;; and grammar rules
;;; 3. extraction of relations from lexical entries, lexical rules
;;; and grammar rules

;;; semantics record is intended for all possible semantics 
;;; bearing structures: that is, grammar rules, lexical rules
;;; and lexical entries

(defvar *rel-semdb* (make-hash-table)
  "outline semantics associated with each relation")

(defstruct (semantics-record)
  id
  relations)

;;; the relations are rels, as defined in basemrs.lisp
;;;
;;; (AAC replace the old relation-record with MRS rel structures
;;; because for the full semdb concept we may need all the
;;; information that's in an ordinary MRS)

(defvar *semantic-table* (make-hash-table)
 "semantics associated with each entry")
;;; indexed by identifier, values are semantics-record

(defvar *relation-index* (make-hash-table :test #'equal)
 "associates relations with instances") 
;;; indexed by relation - values are either simple - a 
;;; boolean hash table which has a value t for
;;; instance ids which are compatible with this
;;; relation, or in the case
;;; of string-featured-relations, a list containing
;;; secondary hash tables
;;; which gives identifiers of relations with those
;;; strings as values of the associated
;;; feature (all values coerced to upcase strings)

#| 

simple:

rel -> id -> t
       id2 -> t
       etc

complex:
    rel -> ((FEAT1 .   val1 -> (id1 id2)
                       val2 -> (id3)
                   etc     )
        (FEAT2 . val3 -> (id1)
                 val4 -> (id2 id3) 
                   etc     ))
       
we assume that there will generally only be one feature                   

|#


;;; lexicon

;;; If a lexical entry has no detectable semantics, a
;;; warning message is issued and the id is stored on
;;; *empty-semantics-lexical-entries*

(defvar *empty-semantics-lexical-entries* nil)

(defun clear-semantic-indices nil
  (clrhash *rel-semdb*)
  (clrhash *semantic-table*)
  (clrhash *relation-index*)
  (setf *empty-semantics-lexical-entries* nil))


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



;;; ************************************************
;;;
;;; Indexing extracted semantics
;;;
;;; ************************************************


(defun index-simple-semantics-record (rel-name id)
;;; rel -> id -> t
;;;        id2 -> t
;;;        etc
  (let ((index-value (gethash rel-name *relation-index*)))
    (cond ((null index-value)
           (let ((new-sec-hash (make-hash-table :test #'eq :size 1)))
             (setf (gethash rel-name *relation-index*) 
                   new-sec-hash)
             (setf (gethash id new-sec-hash) t)))
          ((hash-table-p index-value)
             (setf (gethash id index-value) t))
          (t 
           (progn
             (warn "~%Ignoring entry ~A - Existing value of ~A in *relation-index* isn't a simple-index" id rel-name)
             nil)))))

(defun index-complex-semantics-record (rel id)         
;;; rel -> ((FEAT1 .   val1 -> (id1 id2)
;;;                    val2 -> (id3)
;;;                   etc     )
;;;        (FEAT2 . val3 -> (id1)
;;;                 val4 -> (id2 id3) 
;;;                   etc     ))
  (let* ((rel-name (rel-pred rel))
         (rel-strings (rel-parameter-strings rel))
         (index-value (gethash rel-name *relation-index*)))
    (unless rel-strings
      (error "~%index-complex-semantics-record called on non-complex relation ~A" rel))
    (cond ((null index-value)
           (let ((new-index-list
                  (loop for fvp in rel-strings
                      collect
                        (let ((new-table (make-hash-table :test #'equal))
                              (rel-string (fvpair-value fvp)))
                          (push id (gethash 
                                    (string-upcase (string rel-string)) 
                                    new-table))  
                          (cons (fvpair-feature fvp) new-table)))))
             (setf (gethash rel-name *relation-index*)
               new-index-list)))
          ((listp index-value)
                (loop for fvp in rel-strings
                      collect
                      (let* ((rel-string (fvpair-value fvp))
                             (table (cdr (assoc (fvpair-feature fvp) index-value))))
                        (if (hash-table-p table)
                            (pushnew id (gethash (string-upcase rel-string) table))
                            (warn "~%Ignoring ~A - ~A inconsistent in ~A"
                                 id rel-strings rel-name)))))
          (t 
            (warn "~%Ignoring entry ~A - Existing value of ~A in *relation-index* isn't a hash table" 
                  id rel-name)))))

;;; accessing the stored data - called from lexlookup.lisp


(defun find-candidates-from-rel (rel-name parameter-strs rel)
  ;;; matching a relation
  ;;; a) relation sort matches
  ;;; b) if relation sort is special (e.g. named_rel) then
  ;;;    the special feature(s) also match
  (if rel-name
      (let ((matching (gethash rel-name *relation-index*)))
         (cond  ((null matching) nil)
                ((listp matching)
                 (if parameter-strs
                     (access-complex-semantics-record matching parameter-strs)
                   (progn 
                     (cerror "~%fail to match"
                             "~%parameterized rel ~A without parameter string(s)"
                             rel)
                     nil)))
                ((hash-table-p matching)
                  (when parameter-strs
                     (cerror "~%ignore parameter"
                             "~%unparameterised relation ~A has parameter ~A"
                             rel parameter-strs))
                  (access-simple-semantics-record matching))
                (t (error "~%Unexpected value in relation-index ~A:"
                          matching))))))


(defun access-complex-semantics-record (matching parameter-strs)
  ;;; all specified parameter strings have to match
  ;;; parameter strings may be missing in the relation being
  ;;; looked up, but not in the lexicon index
  (let ((id-list nil)
        (first-time t))
    (dolist (fvp parameter-strs)
        (let* ((feature (fvpair-feature fvp))
               (value (string-upcase (string (fvpair-value fvp))))
               (hash-table (cdr (assoc feature matching))))
          (if hash-table
            (let ((ids (gethash value hash-table)))
              (if first-time 
                  (progn (setf id-list ids)
                         (setf first-time nil))
                (setf id-list (intersection id-list ids))))
            (setf id-list nil)))
      (unless id-list (return)))
    id-list))

(defun access-simple-semantics-record (matching)
  ;;; the value is a hash table
  (let ((ids nil))
    (maphash #'(lambda (k v)
                 (when v (push k ids)))
             matching)
    ids))

;;; code for tables indexed by relation 

(defun add-semantics-record (id record)
  (setf (gethash id *semantic-table*)
    record)
  (loop for relation in (semantics-record-relations record)
      do
        (add-rel-semdb-entry relation)))

(defun add-rel-semdb-entry (relation)
  ;;; very crude right now
  (let ((entry (gethash (rel-pred relation) *rel-semdb*)))
    (unless entry
      (setf (gethash (rel-pred relation) *rel-semdb*)
        relation))))

(defun determine-mrs-feature (reln pos)
  (unless (> pos 0) 
    (error "~%Relation ~A called with position ~A out of range" reln pos))
  (let ((entry (gethash reln *rel-semdb*)))
    (if entry
        (let ((fvpair (elt (rel-flist entry) (- pos 1))))
          (if fvpair
              (fvpair-feature fvpair)
            (progn 
              (warn "~%Relation ~A has no feature at position ~A in *rel-semdb*"
                    reln pos)
              'DUMMYF)))
      (progn
        (warn "~%Relation ~A not found in *rel-semdb*" reln)
        'DUMMYF))))


;;; - called from lexlookup

(defun matches-rel-record (rel lexrec)
  (and (rel-p rel)
       (rel-p lexrec)
       (compatible-types (rel-pred rel)
            (rel-pred lexrec))
       (subsetp
        (get-rel-parameter-strings rel)
        (rel-parameter-strings lexrec)
        :test #'fvpair-equal)))          

(defun fvpair-equal (fvp1 fvp2)
  (and (equal (fvpair-feature fvp1) (fvpair-feature fvp2))
       (equal (fvpair-value fvp1) (fvpair-value fvp2))))

(defun get-rel-parameter-strings (rel)
  (get-fvps-parameter-strings (rel-flist rel)))

;;; ******************************************
;;;
;;; Extraction code
;;;
;;; ******************************************


;;; entry points are from lexutils.lisp

;;; Extraction is still a bit
;;; grammar specific (though actual feature paths etc are
;;; in mrsglobals-eng)
;;;
;;; In this version there is now only one possible location for the 
;;; semantics (hurrah!!!)
;;; it may have `dummy' relations which will never turn up in the real 
;;; semantics.  These are ignored when indexing.
;;;
;;;
;;; extract relations from either lexical or grammar rules

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
          (loop for rel in 
                (find-index-rels (mapcar #'rel-pred
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
  ;;; Because it's easy to make errors, by introducing underspecified
  ;;; types by mistake, there's a maximum number of relations
  ;;; allowed, which is set quite high, because of all the glbtypes
  (let ((returned-rels nil))
    (loop for reltype in reltype-list
         do
         (loop for compatible-rel in (lkb::get-compatible-rels reltype)
              do
              (pushnew compatible-rel returned-rels :test #'eq)))
    (when (and *maximum-genindex-relations* 
               (> (length returned-rels) *maximum-genindex-relations*))
      (when lkb::*debugging*
        (warn "~%Too many subtypes of relation in ~A: ignored" id))
      (setf returned-rels nil))
    returned-rels))


;;; extracting semantics from expanded lexical entries
;;; Note that rels are kept in the order they have in the entries


(defun extract-lexical-relations (lex-entry)
  (let* ((fs (tdfs-indef (lex-entry-full-fs lex-entry)))
         (id  (lex-entry-id lex-entry))
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
          (loop
              with top = (let ((top (if *rel-name-path*
                                      *top-pred-type*
                                      *top-semantics-type*)))
                           (and (is-valid-type top) top))
              for rel in main-rels
              for pred = (rel-pred rel)
              when (or (null pred)
                       (is-top-type pred)
                       (and top (equal-or-subtype top pred)))
              do
                (format
                 t 
                 "~%Warning: ~A contains an underdetermined PRED (`~(~a~)')"
                 id pred))
          (add-semantics-record id new-record)
          (loop
              for rel in 
                (find-index-rels
                 (loop for rel-record in main-rels
                     unless (rel-parameter-strings rel-record)
                     collect (rel-pred rel-record))
                 id)
              do
                (index-simple-semantics-record rel id))
  ;;; we assume that nobody will try and do smart things by underspecifying
  ;;; types for relations which have constant-valued arguments
  ;;; so we can index these things on the specific relation rather
  ;;; than all compatible-semantic-types
          (loop for rel in main-rels
               when (rel-parameter-strings rel)
               do
               (index-complex-semantics-record rel id)))
      (progn (unless
               (member id *gen-rule-ids*)
               (format t 
                       "~%Warning: ~A has no semantics and no filter rule" id))
             (pushnew id *empty-semantics-lexical-entries*)))))



(defun extract-relations-from-liszt (fs id path old-fs)
  ;;; similar to the mrsoutput fn, construct-liszt
  (if (is-valid-fs fs)
      (let ((label-list (fs-arcs fs)))
        (if label-list
            (let* ((first-part (assoc (car *first-path*)
                                      label-list))
                   (rel (if first-part
                            (create-rel-struct
                             (cdr first-part) 
                             nil t)))
                   (rest-part (assoc (car *rest-path*)
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
  (if (eql (car (last full-path)) lkb::*diff-list-list*)
      (eq (path-value oldfs (append (butlast full-path) 
                                    (list lkb::*diff-list-last*)))
          fs)))
                  
;;; ************************************************
;;;
;;; Miscellaneous
;;;
;;; ************************************************

(defun check-for-redundant-filter-rules nil
  ;;; called after indexing a lexicon to warn if
  ;;; redundant filter rules have been defined
  (loop for heuristic-id in *gen-rule-ids*
      do
        (unless (member heuristic-id *empty-semantics-lexical-entries*)
          (format t 
                  "~%Warning: filter rule for ~A is redundant" 
                  heuristic-id))))
