(in-package :cl-user)

;;; from lexindex

(defun clear-generator-index nil
  (mrs::clear-semantic-indices)
  (mrs::clear-lrule-globals)
  (mrs::clear-grule-globals))

(defun clear-generator-lexicon nil
  (clear-generator-index))

(defun clear-generator-lrules nil
  (clear-generator-index))

(defun clear-generator-grules nil
  (clear-generator-index))
  

;;; for the time being, the whole lexicon has to be reindexed whenever
;;; something is altered ...

(defun index-for-generator nil
  (index-lexicon)
  (index-lexical-rules)
  (index-grammar-rules)
  (format t "~%Indexing complete")
  nil)

(defun index-lexicon nil
  (mrs::clear-semantic-indices)
  (setf *batch-mode* t)
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
   (let ((ids nil))
     ;; because of multiple lexical entries, an id may be indexed by
      ;; multiple orthographies
     (dolist (word (lex-words *lexicon*))
       (dolist (inst (lookup-word *lexicon* word :cache nil))
	 (pushnew inst ids :test #'eq)))
     (process-queue
      #'(lambda ()
	  (let ((id (pop ids)))
	    (if id
		(read-psort *lexicon* id :cache nil)
	      :eof)))
      #'(lambda (entry)
	  (expand-psort-entry entry)
	  (let ((new-fs (lex-or-psort-full-fs entry)))
	    (if (and new-fs (not (eq new-fs :fail)))
		(mrs::extract-lexical-relations entry)
	      (format t "~%No feature structure for ~A" 
		      (lex-or-psort-id entry))))
	  (unexpand-psort *lexicon* (lex-or-psort-id entry))))
     (setf *batch-mode* nil)))

(defun get-compatible-rels (reltype)
  (let* ((type-entry (get-type-entry reltype))
         (return-types (list type-entry)))
    (for desc in (type-descendants type-entry)
         do
         (pushnew desc return-types :test #'eq)
         (for desc-anc in (type-ancestors desc)
              do
              (when (member mrs::*top-semantics-entry*
                            (type-ancestors desc-anc) :test #'eq)
                (pushnew desc-anc return-types :test #'eq))))
    (for anc in (type-ancestors type-entry)
          do
          (when (member mrs::*top-semantics-entry*
                        (type-ancestors anc) :test #'eq)
            (pushnew anc return-types :test #'eq)))
    (mapcar #'type-name return-types)))
         



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
     (for parse-res in *parse-record*
          do
          (let* ((lrules-and-entries-used (collect-parse-base parse-res))
                 (mrs (car (mrs::extract-mrs (list parse-res) t))))
            (let
                ((identified-entry-sets
                  (mrs::collect-lex-entries-from-mrs mrs)))
              (mrs::output-mrs mrs 'mrs::simple)
              (let ((retrieved-ids
                     (for res in identified-entry-sets
                          collect
                          (mrs::found-lex-lex-id-fn (car res))))
                    (overgen nil)
                    (undergen nil))
                (for id in retrieved-ids
                     do
                     (unless
                         (member id lrules-and-entries-used :key #'car)
                       (push id overgen)))
                (for id-and-rules in lrules-and-entries-used
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
  (for parse-res in *parse-record*
       do
       (let* ((lrules-and-entries-used (collect-parse-base parse-res))
              (mrs (car (mrs::extract-mrs (list parse-res) t))))
         (let
             ((identified-entry-sets
               (mrs::collect-lex-entries-from-mrs mrs)))
           (let ((retrieved-ids
                  (for res in identified-entry-sets
                       collect
                       (mrs::found-lex-lex-id-fn (car res))))
                 (overgen nil)
                 (undergen nil))
             (for id in retrieved-ids
                  do
                  (unless
                      (member id lrules-and-entries-used :key #'car)
                    (push id overgen)))
             (for id-and-rules in lrules-and-entries-used
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
     (for parse-res in *parse-record*
        do
        (let ((mrs (car (mrs::extract-mrs (list parse-res)))))
          (mrs::output-mrs mrs 'mrs::simple)
          (let
               ((identified-entry-sets
                (mrs::collect-lex-entries-from-mrs mrs)))
          (for res in identified-entry-sets
               do
               (for item in res
                    do
                    (format t "~A ~A " (mrs::found-lex-lex-id-fn item)
                                       (mrs::found-lex-rule-list-fn item))
                    (display-dag 
                     (existing-dag-at-end-of 
                      (tdfs-indef (mrs::found-lex-inst-fs-fn item)) 
                      mrs::*main-semantics-path*) 'simple)))))))


