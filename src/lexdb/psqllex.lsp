;;; Copyright (c) 2002-2004 
;;;   Ann Copestake, Fabre Lambeau, Stephan Oepen, Benjamin Waldron;
;;;   see `licence.txt' for conditions.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  PSQL lexical source
;;;

;;; bmw (nov-03)
;;; - move lexdb code to lexdb directory and split large files
;;; - support for generator indexing;
;;; - SEMI
;;; - RH9 default-locale bug workaround

;;; bmw (oct-03)
;;; - MWE support
;;; - public and private database schemas
;;; - 'mixed' type to handle mix of string/symbol values in field mapping

;;; modifications by bmw (sep-03)
;;; - integration w/ Emacs-Postgres interface
;;; - LexDB menu

;;; modifications by bmw (aug-03)
;;; - lexicon loading code
;;; - db scratch space
;;; - fixed code broken by *lexicon*-related changes
;;; - default types of embedded sql fn args may be overridden

;;; aac (aug-12-03)
;;; initialisation now sets to *lexicon* regardless 

;;; modifications by bmw (jul-03)
;;; - db dump and merge
;;; - default port mechanism
;;; - db queries now execute on fixed table 'current grammar'
;;; - postgres login no longer restricted to 'guest'
;;; - timestamp user and id fields set in db

;;; modifications by bmw (jun-03)
;;; - SQL code moved into db and optimized
;;; - script loading
;;; - basic caching (includes nil values)
;;; - basic versioning (based on a view)
;;; - (set-lexical-entry) implemented in conjunction with (export-lexicon) 
;;; - for compliance with other lexical sources, (lex-words) is expected to
;;;     return "ad" and "hoc" separately, rather than just "ad hoc"; (done)
;;; - (lookup-word) fixed: now returns all entries containing given word.
;;; - loading of multi-word lex entries implemented correctly
;;; - minor bug fixes
;;; - (clear-lex) methods implemented fully, and integrated with (link)/(unlink)
;;;
;;; minor modifications by oe (27-mar-02):
;;;
;;;  - provide initialize and de-initialize procedures;
;;;  - rework DB access layer to cache connection in `psql-database' object.
;;;
;;; ToDo
;;;  - rework connection handling to re-open on demand (rather than error());
;;;  - integrate irregular spellings into lexical DB;
;;;

(in-package :lkb)

(defun postgres-user-temp-dir nil  
    (make-pathname :directory (pathname-directory (lkb-tmp-dir))))

(defun initialize-psql-lexicon 
    (&key
     (db (extract-param :db *psql-lexicon-parameters*))
     (host (extract-param :host *psql-lexicon-parameters*))
     (table (extract-param :table *psql-lexicon-parameters*))
     (port (extract-param :port *psql-lexicon-parameters*))
     (user (extract-param :user *psql-lexicon-parameters*))
     (semi (extract-param :semi *psql-lexicon-parameters*)))
  (unless db
    (error "please set :db in *psql-lexicon-parameters*"))
  (let ((part-of))   
    (if *psql-lexicon*
        (setf part-of (part-of *psql-lexicon*))
      (setf *psql-lexicon* 
	(make-instance 'psql-lex-database)))    
    (setf (dbname *psql-lexicon*) db)
    (if host (setf (host *psql-lexicon*) host))
    (if user (setf (user *psql-lexicon*) user))
    (if port (setf (port *psql-lexicon*) port))
    (cond 
     (table
      (setf (fields-tb *psql-lexicon*) table))
     (t
      (setf (fields-tb *psql-lexicon*) (dbname *psql-lexicon*))))
    (setf (semi *psql-lexicon*) semi)
    (when (initialize-lex *psql-lexicon*)
      (mapcar #'(lambda (x) (link *psql-lexicon* x)) part-of)
      *psql-lexicon*)))

(defun load-psql-lexicon-from-script nil
  (close-lex *lexicon*)
  (when *psql-lexicon-parameters*
    (unless (initialize-psql-lexicon)
      (error "~%Load lexicon aborted"))
    (setf *lexicon* *psql-lexicon*)))
  
(defun open-psql-lex (&rest rest)
  "obsolete (keep for script file compatibility)"
  (apply 'open-psql-lexicon rest))

(defun lookup-word-aux (records lexicon)
  (with-slots 
      (psorts record-cache) 
      lexicon
    (let ((name-field 
	    (second 
	     (assoc 
	      :id 
	      (fields-map lexicon)))))
    (loop
	for record in records
	for id = (str-2-symb 
		  (cdr 
		   (assoc name-field 
			  record 
			  :test #'equal)))
	do
	  (unless (gethash id record-cache)
	    (setf (gethash id record-cache) 
	      record))
	  (unless (gethash id psorts)
	    (setf (gethash id psorts) 
	      (make-psort-struct lexicon record)))
	 collect id))))

;(defun lookup-word-aux (query-res lexicon)
;  (with-slots 
;      (psorts record-cache) 
;      lexicon
;    (let* ((records (make-column-map-record query-res))
;	   (name-field 
;	    (second 
;	     (assoc 
;	      :id 
;	      (fields-map lexicon)))))
;    (loop
;	for record in records
;	for id = (str-2-symb 
;		  (cdr 
;		   (assoc name-field 
;			  record 
;			  :test #'equal)))
;	do
;	  (unless (gethash id record-cache)
;	    (setf (gethash id record-cache) 
;	      record))
;	  (unless (gethash id psorts)
;	    (setf (gethash id psorts) 
;	      (make-psort-struct lexicon record)))
;	 collect id))))

;;; create slot entry
(defun make-strucargs-aux (slot-key slot-value slot-path)
  (cond
   ;;: nil path => no unification
   ((equal slot-path "")
    (list slot-key slot-value))
   ;;: atomic value => simple case
   ((atom slot-value)
    (list slot-key
	  (make-unification
	   :lhs (make-path 
		 :typed-feature-list 
		 (work-out-value 'list slot-path))
	   :rhs (make-u-value :type slot-value))))
   ;;: list. eg. (rest first "word") => (... rest first) has val "word"  
   ((listp slot-value)
    (list slot-key
	  (make-unification
	   :lhs (make-path 
		 :typed-feature-list 
		 (append
		  (work-out-value 'list slot-path)
		  (reverse (cdr (reverse slot-value)))))
	   :rhs (make-rhs-val (car (last slot-value))))))
   (T (error "unhandled input"))))
  
(defun make-rhs-val (x)
  (cond
   ((listp x)
    (make-path :typed-feature-list x))
   (t
    (make-u-value :type x))))

(defun set-filter-psql-lexicon (&rest rest)
  (apply #'set-filter 
	 (cons *psql-lexicon* rest)))

#+:bmw20
(defun i (&optional (slot 'record-cache)) (inspect (slot-value *lexicon* slot)))

(defun index-new-lex-entries (lexicon)
  (let ((semi-out-of-date (semi-out-of-date lexicon)))
    (format t "~%(indexing ~a entries)" (length semi-out-of-date))
    (when semi-out-of-date
      (mrs::populate-*semi*-from-psql)
      (index-lexical-rules)
      (index-grammar-rules)
      (mapc 
       #'(lambda (x)
	   (update-semi-entry lexicon x))
       semi-out-of-date)
      (mrs::dump-*semi*-to-psql))))
  
(defun update-semi-entry (lexicon lexid)
  (let* ((entry (read-psort lexicon lexid :cache nil))
	 (new-fs (and
		  (expand-psort-entry entry)
		  (lex-entry-full-fs entry))))
    (if (and new-fs 
	     (not (eq new-fs :fail)))
	(mrs::extract-lexical-relations entry)
      (format t "~%No feature structure for ~A~%" 
	      (lex-entry-id entry))))
    (forget-psort lexicon lexid))

