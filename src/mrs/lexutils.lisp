;;; Copyright (c) 1998-2003 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Benjamin Waldron
;;; see licence.txt for conditions

(in-package :lkb)

;;; from lexindex

(defparameter *get-compatible-rels-memo* (make-hash-table))

(defun clear-generator-index nil
  (clrhash *get-compatible-rels-memo*)
  (mrs::clear-semantic-indices)
  (mrs::clear-lrule-globals)
  (mrs::clear-grule-globals))

(defun clear-generator-lexicon nil
  (clear-generator-index))

(defun clear-generator-lrules nil
  (clear-generator-index))

(defun clear-generator-grules nil
  (clear-generator-index))
  
(defvar mrs::*semi*)

;;; for the time being, the whole lexicon has to be reindexed whenever
;;; something is altered ...

(defun index-for-generator nil
  (unless (eq (check-generator-environment) :error)
    (index-lexicon)
    (index-lexical-rules)
    (index-grammar-rules)
    (format t "~%Indexing complete")
    nil))

;;; retrieve SEM-I from psql-lexicon if possible
;;; if not, recompile semantic indices as per normal
(defun index-lexicon nil
  (when (typep *lexicon* 'psql-lex-database)
    (format t "~%(caching all lexical records)")
    (cache-all-lex-records-orth *lexicon*))
  (format t "~% (recompiling semantic indices)")
  (mrs::clear-semantic-indices)
  (let ((*batch-mode* t))
    (if mrs::*top-semantics-type*
	(setf mrs::*top-semantics-entry* 
	  (get-type-entry mrs::*top-semantics-type*))
      (progn (cerror "~A will be used (indexing may be inefficient)" 
		     "~%No *top-semantics-type* defined" *toptype*)
	     (setf mrs::*top-semantics-entry*
	       (get-type-entry *toptype*))))
    (unless mrs::*top-semantics-entry*
      (error "~%No entry found for top semantics type ~A" 
	     mrs::*top-semantics-type*))
    (let ((ids-table (make-hash-table :test #'eq)) 
	  (ids nil))
      ;; because of multiple lexical entries, an id may be indexed by
      ;; multiple orthographies
      (dolist (word (lex-words *lexicon*))
	(dolist (inst (lookup-word *lexicon* word :cache nil))
	  (setf (gethash inst ids-table) t)))
      (maphash
       #'(lambda (id val) 
	   (declare (ignore val)) 
	   (push id ids))
       ids-table)
      (process-queue
       #'(lambda ()
	   (let ((id (pop ids)))
	     (if id
		 (read-psort *lexicon* id :cache nil)
	       :eof)))
       #'(lambda (entry)
	   (expand-psort-entry entry)
	   (let ((new-fs (lex-entry-full-fs entry)))
	     (if (and new-fs 
		      (not (eq new-fs :fail)))
		 (mrs::extract-lexical-relations entry)
	       (format t "~%No feature structure for ~A~%" 
		       (lex-entry-id entry))))
	   (forget-psort *lexicon* (lex-entry-id entry))
	   ))
      (mrs::check-for-redundant-filter-rules)))
  (setf *batch-mode* nil)
  (when (typep *lexicon* 'psql-lex-database)
    (mrs::dump-semi-to-psql mrs::*semi*)))

(defun get-compatible-rels (reltype)
  (or (gethash reltype *get-compatible-rels-memo*)
      (let* ((type-entry (get-type-entry reltype))
             (return-value
              (if type-entry
                  (let ((return-types (list type-entry)))
                    (loop for desc in (type-descendants type-entry)
                        do
                          (pushnew desc return-types :test #'eq)
                          (loop for desc-anc in (type-ancestors desc)
                              do
                                (when (member mrs::*top-semantics-entry*
                                              (type-ancestors desc-anc) :test #'eq)
                                  (pushnew desc-anc return-types :test #'eq))))
                    (loop for anc in (type-ancestors type-entry)
                        do
                          (when (member mrs::*top-semantics-entry*
                                        (type-ancestors anc) :test #'eq)
                            (pushnew anc return-types :test #'eq)))
                    (mapcar #'type-name return-types))
                (list reltype))))
        (setf (gethash reltype *get-compatible-rels-memo*)
          return-value))))
         



(defun index-lexical-rules nil
  (mrs::clear-lrule-globals)
  (maphash #'(lambda (id entry)
               (let* ((tdfs (rule-full-fs entry))
                      (fs (if tdfs (tdfs-indef tdfs))))
                 (if fs
                     (mrs::extract-rule-rels id fs entry t))))
           *lexical-rules*))           
                           

(defun index-grammar-rules nil
  (mrs::clear-grule-globals)
  (maphash #'(lambda (id entry)
               (let* ((tdfs (rule-full-fs entry))
                      (fs (if tdfs (tdfs-indef tdfs))))
                 (if fs
                     (mrs::extract-rule-rels id fs entry nil))))
           *rules*))
                 
;;; actually used by lexlookup, but convenient to define in :lkb package

(defun make-mrs-unifs (fvplist initial-features)
  (loop for fvp in fvplist
       collect
       (let ((value (mrs::extrapair-value fvp)))
           (make-unification 
            :lhs 
            (make-path 
             :typed-feature-list
             (append initial-features
                     (extract-mrs-path
                      (mrs::extrapair-feature fvp))))
            :rhs (make-u-value :type
                               (deasterisk value))))))

(defun extract-mrs-path (dotted-feature)
  (let ((feature-string (nreverse (coerce (string dotted-feature) 'list)))
        (current-feat nil)
        (feats nil))
    (dolist (char feature-string)
      (if (char= char #\.)
          (when current-feat
            (push (intern (coerce current-feat 'string) :lkb)
                  feats)
            (setf current-feat nil))
        (push char current-feat)))
    (when current-feat
      (push (intern (coerce current-feat 'string) :lkb)
            feats))
    feats))

  

(defun deasterisk (value)
  (let ((val-string (reverse (string value))))
    (if (char= (elt val-string 0) #\*)
        (let ((new-type (intern (nreverse (subseq val-string 1)) :lkb)))
          (if (is-valid-type new-type)
              new-type
            value))
      value)))
  #|
  (or
   (and (not (stringp value))
        (let* ((ancestors (mapcar #'type-name (retrieve-ancestors value)))
               (strict-type
                (if ancestors
                    (find-strict-type (cons value ancestors)))))
          (if strict-type
              (greatest-common-subtype strict-type value))))
   value))
|#
   
(defparameter *strict-pairs*
    '((pernum . strict_sort)
      (gender . strict_sort)
      (mood . strict_sort)
      (tense . strict_sort)))
      
(defun find-strict-type (types)
  (dolist (type types)
    (let ((strict-type
           (dolist (strp *strict-pairs*)
             (when (eq type (car strp))
               (return (cdr strp))))))
      (when strict-type
        (return strict-type)))))
       



;;; from lexlookup

;;; Testing lookup code.  
;;; If the following is evaluated, then the parse-tsdb-sentences
;;; code will call the fn on each parse
#|
(defparameter *do-something-with-parse* 'batch-check-lex-retrieval)
|#

(defun check-lex-retrieval nil
    (time
     (loop for parse-res in *parse-record*
          do
          (let* ((lrules-and-entries-used (collect-parse-base parse-res))
                 (mrs (mrs::extract-mrs parse-res)))
            (let
                ((identified-entry-sets
                  (mrs::collect-lex-entries-from-mrs mrs)))
              (mrs::output-mrs mrs 'mrs::simple)
              (let ((retrieved-ids
                     (loop for res in identified-entry-sets
                          collect
                          (mrs::found-lex-lex-id-fn (car res))))
                    (overgen nil)
                    (undergen nil))
                (loop for id in retrieved-ids
                     do
                     (unless
                         (member id lrules-and-entries-used :key #'car)
                       (push id overgen)))
                (loop for id-and-rules in lrules-and-entries-used
                     do
                     (unless
                         (member (car id-and-rules) retrieved-ids)
                       (push (car id-and-rules) undergen)))
                (when undergen
                  (format t "~%Entries not retrieved ~{~A ~}" undergen)) 
                (when overgen
                  (format t "~%Extra entries retrieved  ~{~A ~}" overgen))))))))

(defun batch-check-lex-retrieval nil
  (format t "~%~A" *sentence*)
  (loop for parse-res in *parse-record*
       do
       (let* ((lrules-and-entries-used (collect-parse-base parse-res))
              (mrs (mrs::extract-mrs parse-res)))
         (let
             ((identified-entry-sets
               (mrs::collect-lex-entries-from-mrs mrs)))
           (let ((retrieved-ids
                  (loop for res in identified-entry-sets
                       collect
                       (mrs::found-lex-lex-id-fn (car res))))
                 (overgen nil)
                 (undergen nil))
             (loop for id in retrieved-ids
                  do
                  (unless
                      (member id lrules-and-entries-used :key #'car)
                    (push id overgen)))
             (loop for id-and-rules in lrules-and-entries-used
                  do
                  (unless
                      (member (car id-and-rules) retrieved-ids)
                    (push (car id-and-rules) undergen)))
             (when undergen
               (format t "~%Entries not retrieved ~{~A ~}" undergen)) 
             (when overgen
               (format t "~%Extra entries retrieved  ~{~A ~}" overgen)))))))

;;; needs to be made more sophisticated to deal with lex rules etc

(defun quick-check-lex-retrieval nil
     (loop for parse-res in *parse-record*
        do
        (let ((mrs (mrs::extract-mrs parse-res)))
          (mrs::output-mrs mrs 'mrs::simple)
          (let
               ((identified-entry-sets
                (mrs::collect-lex-entries-from-mrs mrs)))
          (loop for res in identified-entry-sets
               do
               (loop for item in res
                    do
                    (format t "~A ~A " (mrs::found-lex-lex-id-fn item)
                                       (mrs::found-lex-rule-list-fn item))
                    (display-dag 
                     (existing-dag-at-end-of 
                      (tdfs-indef (mrs::found-lex-inst-fs-fn item)) 
                      mrs::*main-semantics-path*) 'simple)))))))

(defun mrs-quick-check-lex-retrieval (mrs)
  (mrs::output-mrs mrs 'mrs::simple)
  (let
      ((identified-entry-sets
        (mrs::collect-lex-entries-from-mrs mrs)))
    (loop for res in identified-entry-sets
        do
          (loop for item in res
              do
                (format t "~A ~A " (mrs::found-lex-lex-id-fn item)
                        (mrs::found-lex-rule-list-fn item))
                (display-dag 
                 (existing-dag-at-end-of 
                  (tdfs-indef (mrs::found-lex-inst-fs-fn item)) 
                  mrs::*main-semantics-path*) 'simple)))))


