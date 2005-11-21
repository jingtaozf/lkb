;;; Copyright (c) 1998--2005
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, 
;;;   and Benjamin Waldron; see `licence.txt' for conditions.

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
  (clear-generator-index)
  (cond
   (mrs::*top-semantics-type*
    (setf mrs::*top-semantics-entry* 
      (get-type-entry mrs::*top-semantics-type*)))
   (t
    (cerror "~A will be used (indexing may be inefficient)" 
            "~%No *top-semantics-type* defined" *toptype*)
    (setf mrs::*top-semantics-entry* (get-type-entry *toptype*))))
  (unless (eq (check-generator-environment) :error)
    (index-lexicon)
    (index-lexical-rules)
    (index-grammar-rules)
    (index-generics)
    (format t "~%Indexing complete")
    nil))

(defun index-lexicon nil
  (when (typep *lexicon* 'psql-lex-database)
    (unless (= 0 (sql-get-num *lexdb* "select count(*) from semi_pred"))
      (let ((unindexed-lexids (semi-out-of-date *lexicon*)))
	(cond
	 ((null unindexed-lexids)
	  (format t "~%(retrieving generator indices for lexicon from LexDB)")
	  (mrs::load-generator-indices-from-psql :lexdb *lexicon*)
	  (return-from index-lexicon t))
	 ((< (length unindexed-lexids) 3000)
	  (format t "~%(retrieving generator indices for lexicon from LexDB)")
	  (mrs::load-generator-indices-from-psql :lexdb *lexicon*)
	  (index-new-lex-entries *lexicon*)
	  (format t "~%(dumping generator indices to LexDB)")
	  (mrs::dump-generator-indices-to-psql :lex *lexicon*)
	  (return-from index-lexicon t)))))
    (format t "~%(caching all lexical records)")
    (cache-all-lex-records-orth *lexicon*))
  
  (unless (mrs::restore-semantic-indices)
    (format t "~% (recompiling semantic indices)")
    (mrs::clear-semantic-indices)
    (let ((*batch-mode* t))
      (unless mrs::*top-semantics-entry*
        (error "~%No entry found for top semantics type ~A" 
               mrs::*top-semantics-type*))

      (let ((ids (collect-psort-ids *lexicon*)))

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
             (forget-psort *lexicon* (lex-entry-id entry))))
        
        (mrs::check-for-redundant-filter-rules)))
    (when (typep *lexicon* 'psql-lex-database)
      (mrs::dump-generator-indices-to-psql :lex *lexicon*))
    (when (typep lkb::*lexicon* 'lkb::cdb-lex-database)
      (mrs::serialize-semantics-indices))))

(defun reindex-lexicon nil ; <-- efficiency problem
  (format t "~% (recompiling semantic indices)")
  (mrs::clear-semantic-indices)
  (let ((*batch-mode* t))
    (unless mrs::*top-semantics-entry*
      (error "~%No entry found for top semantics type ~A" 
	     mrs::*top-semantics-type*))
    
    (let ((ids (collect-psort-ids *lexicon*)))
      
      (process-queue
       #'(lambda ()
	   (let ((id (pop ids)))
	     (if id
		 (read-psort *lexicon* id :cache nil)
	       :eof)))
       #'(lambda (entry)
	   (expand-psort-entry entry)
	   (let ((new-fs (lex-entry-full-fs entry)))
	     (cond
	      ((and new-fs 
		    (not (eq new-fs :fail)))
	       (mrs::extract-lexical-relations entry))
	      (t
	       (format t "~%No feature structure for ~A~%" 
		       (lex-entry-id entry)))))
	   (forget-psort *lexicon* (lex-entry-id entry))))      
      (mrs::check-for-redundant-filter-rules))))

(defun get-compatible-rels (reltype)
  (or (gethash reltype *get-compatible-rels-memo*)
      (let* ((type-entry (get-type-entry reltype))
             (return-value
              (if type-entry
                  (let ((return-types (list type-entry)))
                    (loop for desc in (ltype-descendants type-entry)
                        do
                          (pushnew desc return-types :test #'eq)
                          (loop for desc-anc in (ltype-ancestors desc)
                              do
                                (when (member mrs::*top-semantics-entry*
                                              (ltype-ancestors desc-anc) :test #'eq)
                                  (pushnew desc-anc return-types :test #'eq))))
                    (loop for anc in (ltype-ancestors type-entry)
                        do
                          (when (member mrs::*top-semantics-entry*
                                        (ltype-ancestors anc) :test #'eq)
                            (pushnew anc return-types :test #'eq)))
                    (mapcar #'ltype-name return-types))
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
            :rhs (make-u-value :type value)))))

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

;;; old hack for asterisk types removed

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


