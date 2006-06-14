;;; Copyright (c) 2002 - 2006
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
  
(defun extract-param (param param-list)
  (let ((match (assoc param param-list)))
    (if (> (length match) 2)
	(format t "~%;;; WARNING: malformed param entry '~S' in '~S'" param param-list)
  (second match))))

(defun initialize-lexdb 
    (&key
     (type 'mu-psql-lex-database)
     (dbname (or (extract-param :dbname *lexdb-params*)
		 (and *lexdb* (dbname *lexdb*))))
     (host (or (extract-param :host *lexdb-params*) 
	       "localhost"))
     (table (extract-param :table *lexdb-params*))
     (port (or (extract-param :port *lexdb-params*)
	       (or (getenv "PGPORT") *psql-database-port*)))
     (user (or (extract-param :user *lexdb-params*)
	       (and *lexdb* (user *lexdb*))
	       (user-name)))
     (semi (extract-param :semi *lexdb-params*))
     (quick-load (extract-param :quick-load *lexdb-params*))
     (password (or (extract-param :password *lexdb-params*)
		   (and *lexdb* (password *lexdb*)))))
  (psql-initialize)
  (unless dbname
    (error "please set :dbname in *lexdb-params*"))
  (let (part-of extra-lexicons)
    ;; we will create a new lexicon then insert it into the lexicon hierarchy as
    ;; a replacement for *lexdb*
    (cond
     ((typep *lexdb* type)
      (setf part-of (part-of *lexdb*))
      (setf (part-of *lexdb*) nil) ;; to avoid unlinking
      (setf extra-lexicons (extra-lexicons *lexdb*))
      (setf (extra-lexicons *lexdb*) nil) ;; to avoid unlinking
      )
     (t 
      (setf *lexdb* (make-instance type))))
    
    (setf (dbname *lexdb*) dbname)
    (setf (host *lexdb*) host)
    (setf (user *lexdb*) user)
    (setf (password *lexdb*) password)
    (setf (port *lexdb*) port)
    (setf (semi *lexdb*) semi)
    (setf (quick-load *lexdb*) quick-load)
    ;; use of table is obsolete
    (cond 
     (table
      (setf (fields-tb *lexdb*) table))
     (t
      (setf (fields-tb *lexdb*) (dbname *lexdb*))))
    ;; insert into lexicon hierarchy
    (when (initialize-lex *lexdb*)
      (mapcar #'(lambda (x) (link *lexdb* x)) part-of)
      (mapcar #'(lambda (x) (link x *lexdb*)) extra-lexicons)      
      *lexdb*)))

(defun open-psql-lex (&rest rest)
  "obsolete (keep for script file compatibility)"
  (apply 'open-lexdb rest))


#+:bmw20
(defun i (&optional (slot 'record-cache)) (inspect (slot-value *lexicon* slot)))

(defvar mrs::*semantic-table*)
(defvar mrs::*empty-semantics-lexical-entries*)

(defun concat-str (str-list &key (sep-c #\Space))
  (unless (listp str-list)
    (error "list expected"))
  (let ((sep (string sep-c)))
    (cond
     ((null str-list) "")
     (t (apply 'concatenate
	       (cons
		'string
		(cons
		 (pop str-list)
		 (mapcan #'(lambda (x) (list sep
					     (if x 
						 x
					       "")))
			 str-list))))))))

(defun to-psql-COPY-rec2 (lst &key (delim-char #\tab) (null "\\N"))
  (cond
   ((null lst)
    "")
   (t
    (apply #'concatenate 'string 
	   (append (list (psql-COPY-val (car lst) :delim-char delim-char :null null))
		   (loop
		       with delim = (string delim-char)
		       for x in (cdr lst)
		       collect delim
		       collect (psql-COPY-val x :delim-char delim-char :null null))
		   (list (string 
			  (code-char 10))) ;; newline
		   )))))
     
(defun normalize-orthkeys (recs)
  (loop
      for rec in recs
      do (setf (fourth rec) (normalize-orthkey! (fourth rec))))
  recs)

(defun psql-COPY-val (val &key (delim-char #\tab) (null "\\N"))
  (cond
   ((null val)
    null)
   (t
    (coerce
     (loop
	 with str = (2-str val)
	 for char across str
	 for code = (char-code char)
	 when (= code 8)
	 collect #\\
	 and collect #\b
	 else when (= code 9)
	 collect #\\
	 and collect #\t
	 else when (= code 10)
	 collect #\\
	 and collect #\n
	 else when (= code 11)
	 collect #\\
	 and collect #\v
	 else when (= code 12)
	 collect #\\
	 and collect #\f
	 else when (= code 13)
	 collect #\\
	 and collect #\r
	 else when (eq char #\\)
	 collect #\\
	 and collect #\\
	 else when (eq char delim-char)
	 collect #\\
	 and collect delim-char
	 else	 
	 collect char)
     'string))))

(defun clear-psql-semi (&key (lex *lexdb*))
  (unless (typep lex 'su-psql-lex-database)
    (error "su-psql-lex-database expected"))
  (semi-drop-indices lex)
  (run-command lex "DELETE FROM semi_pred")
  (run-command lex "DELETE FROM semi_frame")
  (run-command lex "DELETE FROM semi_var")
  (run-command lex "DELETE FROM semi_extra")
  (run-command lex "DELETE FROM semi_mod"))

(defun compat-version (lexdb-version)
  (when (stringp lexdb-version)
    (subseq lexdb-version 0 3)))  
    
(defmethod fields-str ((lex psql-lex-database) fields)
  (concat-str
   (mapcar #'(lambda (x) (quote-ident lex x))
	   fields)
   :sep-c #\,))

(defun sql-list (l quote-fn)
  (format nil "(~a)"
    (concat-str
     (mapcar quote-fn l)
     :sep-c #\,)))

(defmethod psql-lex-database-type ((lex psql-lex-database))
  (cond
   ((typep lex 'mu-psql-lex-database)
    'mu-psql-lex-database)
   ((typep lex 'su-psql-lex-database)
    'su-psql-lex-database)
   (t
    (error "unexpected psql-lex-database type: ~S" lex))))
   

(defmacro with-lexdb-user-lexdb ((lexdb-lexdb lexdb) &body body)
  `(with-slots (dbname host port fields-tb) ,lexdb
     (let ((,lexdb-lexdb
	    (make-instance (psql-lex-database-type ,lexdb)
	      :dbname dbname
	      :fields-tb fields-tb
	      :host host
	      :port port
	      :user (db-owner ,lexdb))))
       (open-lex ,lexdb-lexdb)
       ,@body
       (close-lex ,lexdb-lexdb))))

#+:null
(defmacro with-lexdb-user-x ((user lexdb-lexdb lexdb) &body body)
  `(with-slots (dbname host port fields-tb) ,lexdb
     (let ((,lexdb-lexdb
	    (make-instance (psql-lex-database-type ,lexdb)
	      :dbname dbname
	      :fields-tb fields-tb
	      :host host
	      :port port
	      :user ,user)))
       (open-lex ,lexdb-lexdb)
       ,@body
       (close-lex ,lexdb-lexdb))))
  
(defmacro lexdb-time ((start-msg end-msg) &body body)
  `(let (time out)
    (format t "~&(LexDB) ~a ..." ,start-msg)
    (force-output)
    (setf time (get-internal-real-time))
    (setf out (progn ,@body))
    (format t "~&(LexDB) ~a [~F sec]" ,end-msg 
	    (/ (- (get-internal-real-time) time) internal-time-units-per-second))
    out))
  
