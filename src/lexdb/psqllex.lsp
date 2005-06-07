;;; Copyright (c) 2002 - 2005 
;;;   Benjamin Waldron, Ann Copestake, Fabre Lambeau, Stephan Oepen;
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

;; call from script file
(defun load-lexdb-from-script nil
  (close-lex *lexicon*)
  (cond
   (*lexdb-params*
    (unless (initialize-lexdb)
      (error "~%Load lexicon aborted"))
    (setf *lexicon* *lexdb*))
   (t
   (error "to use the LexDB you must set *lexdb-params*"))))
  
(defun initialize-lexdb 
    (&key
     (dbname (or (extract-param :dbname *lexdb-params*)
		 (extract-param :db *lexdb-params*) ;;backwards compat
		 (and *lexdb* (dbname *lexdb*))))
     (host (extract-param :host *lexdb-params*))
     (table (extract-param :table *lexdb-params*))
     (port (extract-param :port *lexdb-params*))
     (user (extract-param :user *lexdb-params*))
     (semi (extract-param :semi *lexdb-params*)))
  ;; ensure backwards compat
  (setf dbname
    (or dbname (extract-param :db *lexdb-params*)))
  (unless dbname
    (error "please set :dbname in *lexdb-params*"))
  (let ((part-of))
    ;; we will create a new lexicon then insert it into the lexicon hierarchy as
    ;; a replacement for *lexdb*
    (if *lexdb*
        (setf part-of (part-of *lexdb*))
      (setf *lexdb*
	(make-instance 'psql-lex-database)))
    (setf (dbname *lexdb*) dbname)
    (if host (setf (host *lexdb*) host))
    (if user (setf (user *lexdb*) user))
    (if port (setf (port *lexdb*) port))
    (setf (semi *lexdb*) semi)
    ;; use of table is obsolete
    (cond 
     (table
      (setf (fields-tb *lexdb*) table))
     (t
      (setf (fields-tb *lexdb*) (dbname *lexdb*))))
    ;; insert into lexicon hierarchy
    (when (initialize-lex *lexdb*)
      (mapcar #'(lambda (x) (link *lexdb* x)) part-of)
      *lexdb*)))

(defun open-psql-lex (&rest rest)
  "obsolete (keep for script file compatibility)"
  (apply 'open-lexdb rest))

;; read-psort ->
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
		 (work-out-rawlst slot-path))
	   :rhs (make-u-value :type slot-value))))
   ;;: list. eg. (rest first "word") => (... rest first) has val "word"  
   ((listp slot-value)
    (list slot-key
	  (make-unification
	   :lhs (make-path 
		 :typed-feature-list 
		 (append
		  (work-out-rawlst slot-path)
		  (reverse (cdr (reverse slot-value)))))
	   :rhs (make-rhs-val (car (last slot-value))))))
   (T (error "unhandled input"))))
  
(defun make-rhs-val (x)
  (cond
   ((listp x)
    (make-path :typed-feature-list x))
   (t
    (make-u-value :type x))))

#+:bmw20
(defun i (&optional (slot 'record-cache)) (inspect (slot-value *lexicon* slot)))

