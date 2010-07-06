;;; Copyright (c) 2002 - 2006
;;;   Benjamin Waldron, Ann Copestake, Fabre Lambeau, Stephan Oepen;
;;;   see `LICENSE' for conditions.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  PSQL lexical source
;;;

;;; aac (aug-12-03)
;;; initialisation now sets to *lexicon* regardless 


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

(defun type-2-psql-lex-database-type (type)
  (and type
       (case type
	 (:single-user 'su-psql-lex-database)
	 (:multi-user 'mu-psql-lex-database)
	 (t
	  (error "unknown :type ('~a') in LexDB params" type)))))

(defun psql-lex-database-type (lex)
  (cond
   ((typep lex 'su-psql-lex-database) :single-user)
   ((typep lex 'mu-psql-lex-database) :multi-user)))

(defun initialize-lexdb-default-type ()
  (let ((type :multi-user))
    (format t "~%Warning: using LexDB param :type = '~S'" type)
    (format t "~%         (please set this explicitly in *lexdb-params*)" type)
    type))

(defun initialize-lexdb-type (x)
  (or x
      (extract-param :type *lexdb-params*)
      (psql-lex-database-type *lexdb*)
      (initialize-lexdb-default-type)))

(defun initialize-lexdb-dbname (x)
  (or x
      (extract-param :dbname *lexdb-params*)
      (and *lexdb* (dbname *lexdb*))
      (error "please set :dbname in *lexdb-params*")))


(defun initialize-lexdb-host (x)
  (or x 
      (extract-param :host *lexdb-params*)
      (and *lexdb* (host *lexdb*))
      "localhost"))

;(defun initialize-lexdb-table (x)
;  (or x 
;      (extract-param :host *lexdb-params*)
;      (and *lexdb* (fields-tb *lexdb*))
;      (and *lexdb* (dbname *lexdb*))))
      
(defun initialize-lexdb-port (x)
  (or x 
      (extract-param :port *lexdb-params*)
      (and *lexdb* (port *lexdb*))
      (or (getenv "PGPORT") *psql-database-port*)))

(defun initialize-lexdb-user (x)
  (or x 
      (extract-param :user *lexdb-params*)
      (and *lexdb* (user *lexdb*))
      (sys-user-name)))

(defun initialize-lexdb-password (x)
  (or x 
      (extract-param :password *lexdb-params*)
      (and *lexdb* (password *lexdb*))
      (and *lexdb* (user *lexdb*))))
      
(defun initialize-lexdb (&key type dbname host port user semi password)
  ;; warn on use of obsolete semi argument 
  (when semi 
    (format t "~%(LexDB) WARNING: :SEMI argument to *lexdb-params* is obsolete")
    (format t "~%(LexDB)          (please call index-for-generator instead)"))
  
  ;; warn on use of obsolete table argument 
  (when semi 
    (format t "~%(LexDB) WARNING: :TABLE argument to *lexdb-params* is obsolete"))
  
  (let (part-of extra-lexicons)
    (psql-initialize)
    
    ;; username 'lexdb' is illegal in multi-user mode
    (if (and (string= user "lexdb")
	     (eq type :multi-user))
	(error "User 'lexdb' cannot connect as LexDB client. Please pick a different username..."))
    
    ;; obtain unlinked *lexdb*
    (setf type (initialize-lexdb-type type))
    (cond
     ((and *lexdb* (eq type (psql-lex-database-type *lexdb*)))
      (setf part-of (part-of *lexdb*))
      (setf (part-of *lexdb*) nil) ;; to avoid unlinking
      (setf extra-lexicons (extra-lexicons *lexdb*))
      (setf (extra-lexicons *lexdb*) nil) ;; to avoid unlinking
      )
     (t 
      (setf *lexdb* 
	(make-instance (type-2-psql-lex-database-type type)))))
    
    ;; set properties of *lexdb*
    (setf (dbname *lexdb*) (initialize-lexdb-dbname dbname))
    (setf (host *lexdb*) (initialize-lexdb-host host))
    (setf (user *lexdb*) (initialize-lexdb-user user))
    (setf (password *lexdb*) (initialize-lexdb-password password))
    (setf (port *lexdb*) (initialize-lexdb-port port))
;    (setf (fields-tb *lexdb*) (initialize-lexdb-table table))

    ;; open lexdb and insert into lexicon hierarchy
    (when (open-lex *lexdb*)
      (mapcar #'(lambda (x) (link *lexdb* x)) part-of)
      (mapcar #'(lambda (x) (link x *lexdb*)) extra-lexicons)      
      *lexdb*)))

(defun open-psql-lex (&rest rest)
  "obsolete (keep for script file compatibility)"
  (apply 'open-lex rest))


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
     
(defun normalize-orthkeys-aux (recs i)
  (loop
      for rec in recs
      do (setf (nth i rec) (normalize-orthkey! (nth i rec))))
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

#+:null
(defun clear-psql-semi (&key (lex *lexdb*))
  (unless (typep lex 'psql-lex-database)
    (error "psql-lex-database expected"))
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

;(defmethod psql-lex-database-type ((lex psql-lex-database))
;  (cond
;   ((typep lex 'mu-psql-lex-database)
;    'mu-psql-lex-database)
;   ((typep lex 'su-psql-lex-database)
;    'su-psql-lex-database)
;   (t
;    (error "unexpected psql-lex-database type: ~S" lex))))
   

(defmacro with-lexdb-user-lexdb ((lexdb-lexdb lexdb) &body body)
  `(with-slots (dbname host port fields-tb) ,lexdb
     (let ((,lexdb-lexdb
	    (make-instance (type-2-psql-lex-database-type (psql-lex-database-type ,lexdb))
	      :dbname dbname
	      ;:fields-tb fields-tb
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
	    (make-instance (type-2-psql-lex-database-type (psql-lex-database-type ,lexdb))
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
  
