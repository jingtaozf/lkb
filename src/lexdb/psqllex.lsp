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

(defvar *psql-lexicon-parameters*) ;; define in GRAMMAR/globals.lsp

(defvar *postgres-temp-filename* nil)
(defvar *postgres-user-temp-dir* 
    (make-pathname :directory (pathname-directory (lkb-tmp-dir))))

(defvar *psql-db-version* "3.06")
(defvar *psql-fns-version* "1.00")
(defvar *psql-port-default* nil)

(defvar *postgres-tmp-lexicon* nil)
(defvar *psql-lexicon* nil)

(defvar *postgres-debug-stream* t)

(defvar *postgres-mwe-enable* nil)

(defun initialize-psql-lexicon 
    (&key
     (db (extract-param :db *psql-lexicon-parameters*))
     (host (extract-param :host *psql-lexicon-parameters*))
     (table (extract-param :table *psql-lexicon-parameters*))
     (port (extract-param :port *psql-lexicon-parameters*))
     (user (extract-param :user *psql-lexicon-parameters*)))
  (unless (and db host)
    (error "please instantiate db+host in *psql-lexicon-parameters*"))
  (let ((part-of))   
    (if *psql-lexicon*
        (setf part-of (part-of *psql-lexicon*))
      (setf *psql-lexicon* 
	(make-instance 'psql-lex-database)))    
    (setf (dbname *psql-lexicon*) db)
    (setf (host *psql-lexicon*) host)
    (if user (setf (user *psql-lexicon*) user))
    (if port (setf (port *psql-lexicon*) port))
    ;;(setf (lex-tb *psql-lexicon*) table) ;;unused
    (cond 
     (table
      (setf (fields-tb *psql-lexicon*) table))
     (t
      (setf (fields-tb *psql-lexicon*) (dbname *psql-lexicon*))))
    (when (initialize-lex *psql-lexicon*)
      (mapcar #'(lambda (x) (link *psql-lexicon* x)) part-of)
      *psql-lexicon*)))

(defun load-psql-lexicon-from-script nil
  (close-lex *lexicon*)
  (unless (initialize-psql-lexicon)
    (error "~%Load lexicon aborted"))
  (setf *lexicon* *psql-lexicon*))
  
(defun open-psql-lex (&rest rest)
  "obsolete"
  (apply 'open-psql-lexicon rest))

;;;
;;; class declarations
;;;

(defclass sql-database ()
  ((dbname :initform nil :accessor dbname	:initarg :dbname)
   (host :initform "localhost" :accessor host :initarg :host)
   (user :initform (sys:user-name) :accessor user :initarg :user)
   (password :initform "" :accessor password :initarg :password)
   (port :initform (num-2-str *psql-port-default*) :accessor port :initarg :port)))

(defclass external-lex-database (lex-database)
  ((record-cache :initform (make-hash-table :test #'eq))
   ;; flat-table containing the lexical database
   (lexicon-table :initform nil :accessor lex-tb :initarg :lex-tb) ;; unused
   ;; table for mapping the lexicon-table fields to the psort-or-lex structure
   (slot-to-fields-mapping-table 
    :initform nil :accessor fields-tb :initarg :fields-tb)
   ;; a-list for mapping the lexicon-table fields to the psort-or-lex structure
   (slot-to-fields-mapping :initform nil :accessor fields-map)))

(defclass psql-database (sql-database)
  ((connection :initform nil :accessor connection :initarg connection)
   (server-version :initform nil :accessor server-version)
   (fns :initform nil :accessor fns)))

(defclass psql-lex-database (psql-database external-lex-database)
  ())

(defclass sql-query ()
  ((sql-string :accessor sql-string :initarg :sql-string :initform nil)
   (records :initform :unknown :accessor records)
   (columns :initform :unknown :accessor columns)
   ))

(defclass psql-lex-entry ()
  ((fv-pairs :initarg :fv-pairs)))

;;;
;;; --- sql-database methods
;;;

(defmethod close-lex ((lexicon sql-database) &key in-isolation delete)
  (declare (ignore in-isolation delete))
  (with-slots (dbname host user password fns) lexicon
    (setf dbname nil)
    (setf host nil)
    (setf lexicon nil)
    (setf password nil)
    (setf fns nil))			;:todo: unbind functions
  )

;;;
;;; --- psql-database methods
;;;

(defmethod connect ((lexicon psql-database)) 
  (let ((user (user lexicon)))
    (cond
     ((and user 
	   (eq (connect-aux lexicon :user user) 
	       :connection-ok)))
     (t
      (setf user 
	(ask-user-for-x "Connect to PostgreSQL lexicon" 
			(cons 
			 "Username?" 
			 (or user 
			     "guest"))))
      (when user
	(setf (user lexicon) user)
	(connect lexicon))))))
      
(defmethod connect-aux ((lexicon psql-database) &key (user "guest"))
  (with-slots (port host dbname connection) lexicon
    (let (decoded-status password)
      ;; attempt connection w/ default pwd
      (setf connection 
	(connect-aux2 :port port 
		      :host host 
		      :dbname dbname 
		      :user user 
		      :password user))
      (setf decoded-status 
	(pg:decode-connection-status 
	 (pg:status connection)))
      (unless (eq decoded-status 
		  :connection-ok)
	;; attempt connection w/ pwd
	(setf password 
	  (ask-user-for-x "Connect to PostgreSQL lexicon" 
			  (cons 
			   (format nil "Password for ~a?" user) 
			   user)))
	(when password
	  (setf connection 
	    (connect-aux2 :port port 
			  :host host 
			  :dbname dbname 
			  :user user 
			  :password password))))
      (setf decoded-status 
	(pg:decode-connection-status 
	 (pg:status connection)))
      (when (eq decoded-status 
		:connection-ok)
	(setf (server-version lexicon) 
	  (get-server-version lexicon))
	(setf (user lexicon) 
	  user))
      decoded-status)))

(defun connect-aux2 (&key (host) (dbname) (user) (password) (port))
  (let ((connection)
	(decoded-status))
    (setf connection 
      (pg:connect-db-with-handler 
       (concatenate 'string 
	 (if port 
	     (format nil "port='~a' " (sql-escape-string port)))
	 (format nil "host='~a' " (sql-escape-string host))
	 (format nil "dbname='~a' " (sql-escape-string dbname))
	 (format nil "user='~a' " (sql-escape-string user))
	 (if password 
	     (format nil "password='~a'" (sql-escape-string password))
	   ""))))
    (setf decoded-status 
      (pg:decode-connection-status 
       (pg:status connection)))
    ;: in case postgres is running locally w/o TCPIP...
    (unless (eq decoded-status 
		:connection-ok)		
      (setf connection 
	(pg:connect-db-with-handler
	 (concatenate 'string 
           (if port 
	       (format nil "port='~a' " (sql-escape-string port)))
	   (format nil "dbname='~a' " (sql-escape-string dbname))
	   (format nil "user='~a' " (sql-escape-string user))
	   (if password 
	       (format nil "password='~a'" (sql-escape-string password))
	     "")))))
    connection))

(defmethod disconnect ((lexicon psql-database))
  (with-slots (connection) lexicon
  ;:close connection cleanly
    (when connection 
      (pg:finish connection)
      (setf connection nil))))

;;;
;;; --- sql-query methods and functions
;;;

(defmethod print-object ((object sql-query) stream)
  (with-slots 
      (sql-string records columns count-records mapped-recs) 
      object
    (let ((records-c (if (listp records)
			 (length records)
		       records))
	  (columns-c (if (listp columns)
			 (length columns)
		       columns)))
      (format
       stream
       "#[SQL-QUERY: ~a~%  [~a record~p; ~a column~p]]"
       sql-string
       records-c records-c 
       columns-c columns-c
       ))))

(defmethod run-command-stdin ((database psql-database) command filename)
  (let ((connection (connection database)))
    (unless connection
      (error "database ~s has no active connection." database))
    (pg::stdin-command-file connection command filename)))

(defmethod run-command ((database psql-database) command)
  (run-query database 
	     (make-instance 'sql-query :sql-string command)))

(defmethod run-query ((database psql-database) (query sql-query))
  (let ((connection (connection database)))
    (unless connection
      (error "database ~s has no active connection." database))
    (multiple-value-bind (recs cols)
        (pg:sql (sql-string query) :db connection)
      (setf (records query) recs
            (columns query) (mapcar #'str-2-keyword cols)))
    query))

;;; returns _association list_
(defmethod make-column-map-record ((query sql-query))
    (loop 
        for element in (records query)
        collect (mapcar #'cons (columns query) element)))

;;;
;;; --- external-lex-database methods
;;;

(defmethod make-requested-fields ((lexicon external-lex-database))
  ;; constructs the argument string to sql SELECT with all necessary fields
  (let* ((fields 
	  (remove-duplicates 
	   (mapcar #'cadr 
		   (fields-map lexicon))
	   :test #'equal))
         (fields-str 
	  (symb-2-str 
	   (pop fields))))
    (loop 
        for element in fields
        do 
	  (setf fields-str 
	    (concatenate 'string 
	      fields-str 
	      ", " 
	      (symb-2-str element))))
    fields-str))

;; this is to avoid being annoyed when word not in the database.
(defmethod collect-expanded-lex-ids ((lexicon external-lex-database))
  ;(error "collect-expanded-lex-ids(): invalid method on PostGreSQL lexicon")
  )

(defmethod close-lex ((lexicon external-lex-database) &key in-isolation delete)
  (declare (ignore in-isolation delete))
  (with-slots 
      (lex-tb fields-map fields-tb record-cache) 
      lexicon
    (setf lex-tb nil) ;; unused
    (setf fields-map nil)
    (setf fields-tb nil)
    ))

(defmethod empty-cache ((lexicon external-lex-database) &key recurse)
  (declare (ignore recurse))
  (with-slots (record-cache) lexicon
    (clrhash record-cache)
    ))

;;;
;; --- psql-lex-database methods
;;;

(defmethod lookup-word ((lexicon psql-lex-database) orth &key (cache t))
  (setf orth 
    (string-downcase orth))
  (let ((hashed 
	 (gethash 
	  orth 
	  (slot-value 
	   lexicon 
	   'lexical-entries))))
    (cond 
     (hashed
      (if (eq hashed 
	      :EMPTY)
	  (setf hashed 
	    nil))
      hashed)
     (t 
      (let ((value 
	     (lookup-word-psql-lex-database lexicon orth)))
	;;if caching, add entry to cache...
	(when cache
	  (setf (gethash orth 
			 (slot-value lexicon 
				     'lexical-entries)) 
	    (if value 
		value 
	      :EMPTY)))
	value)))))

(defun lookup-word-psql-lex-database (lexicon orth)
  (declare (ignore cache))
  (if (connection lexicon)
       (let* ((orthstr orth)
	      (sql-str 
	       (sql-retrieve-entries-by-orthkey 
		lexicon 
		(make-requested-fields lexicon) 
		orthstr))
	      (query-res 
	       (run-query 
		lexicon 
		(make-instance 
		    'sql-query 
		  :sql-string sql-str)))
	      (ids 
	       (lookup-word-aux 
		query-res 
		lexicon)))
	 ids)))

(defun lookup-word-aux (query-res lexicon)
  (with-slots 
      (psorts record-cache) 
      lexicon
    (let* ((records (make-column-map-record query-res))
	   (name-field 
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

;;; (used to index for generator)
;;; fix_me: inefficient implementation
(defmethod lex-words ((lexicon psql-lex-database))
  (let* ((sql-str (sql-orthography-set lexicon))
         (query-res (run-query 
                     lexicon 
                     (make-instance 
			 'sql-query 
		       :sql-string sql-str))))
    (mapcan 
     #'(lambda (x) 
	 (split-into-words 
	  (string-upcase 
	   (car x))))
     (records query-res))))

(defmethod collect-psort-ids ((lexicon psql-lex-database) &key (cache t) (recurse t))
  (declare (ignore recurse))
  (with-slots (cache-lex-list) 
      lexicon
    (let ((lex-list cache-lex-list))
      (when (null cache-lex-list)
	(setf lex-list 
	  (collect-psort-ids-aux lexicon))
	(if (null lex-list)
	    (setf lex-list :empty))
	(if cache 
	    (setf cache-lex-list lex-list)))
      (case lex-list
	(:empty nil)
	(otherwise lex-list)))))

(defmethod collect-psort-ids-aux ((lexicon psql-lex-database))
  (let* ((sql-str (sql-lex-id-set lexicon))
	 (query-res 
	  (run-query 
	   lexicon 
	   (make-instance 
	       'sql-query 
	     :sql-string sql-str))))
    (mapcar 
     #'(lambda (x) 
	 (str-2-symb (car x)))
     (records query-res))))

(defmethod retrieve-all-records ((lexicon psql-lex-database) &optional reqd-fields)
  (unless (connection lexicon)
    (format t "~%WARNING: no connection to psql-lex-database")
    (return-from retrieve-all-records nil))
  (unless reqd-fields 
    (setf reqd-fields "*"))
  (let* ((sql-str 
	  (sql-retrieve-all-entries 
	   lexicon 
	   reqd-fields))
	 (query-res 
	  (run-query 
	   lexicon 
	   (make-instance 
	       'sql-query 
	     :sql-string sql-str))))
    (when (records query-res) 
      (make-column-map-record query-res))))

(defmethod retrieve-record ((lexicon psql-lex-database) id &key (cache t) reqd-fields)
  (with-slots (record-cache) lexicon
    (let ((hashed (gethash id record-cache)))
      (cond (hashed
	     (unless (eq hashed 
			 :EMPTY)
	       hashed))
	    (t
	     (let* ((record 
		     (retrieve-record-str 
		      lexicon 
		      (symb-2-str id) 
		      reqd-fields)))
	       (when cache
		 (setf (gethash id record-cache)
		   (or record :EMPTY)))
	       record))))))

(defmethod retrieve-record-str 
    ((lexicon psql-lex-database) id-str &optional reqd-fields)
  (unless (connection lexicon)
    (format t "~%WARNING: no connection to psql-lex-database")
    (return-from retrieve-record-str nil))
  (unless reqd-fields 
    (setf reqd-fields "*"))
  (let* ((sql-str 
	  (sql-retrieve-entry 
	   lexicon 
	   reqd-fields 
	   id-str))
	 (query-res 
	  (run-query 
	   lexicon 
	   (make-instance 
	       'sql-query 
	     :sql-string sql-str)))
	 (records 
	  (when (records query-res) 
	    (make-column-map-record query-res))))
    (if (> (length records) 
	   1)
	(error (format nil "database error (too many records returned)"))
      (car records))))

(defmethod retrieve-head-record-str 
    ((lexicon psql-lex-database) id-str &optional reqd-fields)
  (unless (connection lexicon)
    (format t "~%WARNING: no connection to psql-lex-database")
    (return-from retrieve-head-record-str nil))
  (unless reqd-fields 
    (setf reqd-fields "*"))
  (let* ((sql-str 
	  (fn 
	   lexicon 
	   'retrieve-head-entry 
	   reqd-fields 
	   id-str))
	 (query-res 
	  (run-query 
	   lexicon 
	   (make-instance 
	       'sql-query 
	     :sql-string sql-str)))
	 (records (when (records query-res) 
		    (make-column-map-record query-res))))
    (if (> (length records) 
	   1)
	(error (format nil "database error (too many records returned)"))
      (car records))))

(defmethod read-psort 
    ((lexicon psql-lex-database) id &key (cache t) (recurse t) (new-instance nil))
  (declare (ignore recurse))
  (with-slots (psorts) lexicon
    (let ((hashed 
	   (and (not new-instance)
		(gethash id psorts))))
      (cond (hashed
	     (unless (eq hashed 
			 :EMPTY)
	       hashed))
	    (t
	     (let* ((record 
		     (retrieve-record 
		      lexicon id 
		      :reqd-fields (make-requested-fields lexicon)
		      :cache cache))
		    (entry 
		     (if record 
			 (make-psort-struct lexicon record))))
	       (when cache
		 (setf (gethash id psorts)
		   (or entry 
		       :EMPTY)))
	       entry))))))

(defmethod make-psort-struct ((lexicon psql-lex-database) query-res)
  (let* 
      ((strucslots 
	(loop 
	    for (slot-key slot-field slot-path slot-type) 
	    in (fields-map lexicon)
	    for slot-value-list = 
	      (work-out-value 
	       slot-type 
	       (get-val slot-field query-res)
	       :path (work-out-value 
		      'list 
		      slot-path)
	       )		
	      ;; if empty third argument (ie. path), 
	      ;; then add (:key "field")
	    when slot-value-list
	    append (mapcar 
		    #'(lambda (x) 
			(make-psort-struct-aux 
			 slot-key x slot-path
			 )) 
		    slot-value-list)))
       ;; groups slots with same key together in a list
       (strucargs 
	(loop
	    for unique-slot in (remove-duplicates 
				(mapcar #'car strucslots))
	    append
	      (list unique-slot 
		    (let ((values 
			   (loop 
			       for (psort-slot psort-value) 
			       in strucslots
			       when (eql psort-slot unique-slot)
			       collect psort-value)))
		      ;;
		      ;; _fix_me_
		      ;; pretty bad fix to avoid getting lists where 
		      ;; they are not needed
		      ;;
		      (if (> (list-length values) 1)
			  values
			(car values)))))))
    (let ((orth-str (second (member :orth strucargs))))
      (if (and (listp orth-str) 
	       (cdr orth-str))
	  ;; infl-pos is only relevant for multi-word entries
	  (setf strucargs
	    (append 
	     (list :infl-pos
		   (find-infl-pos nil orth-str nil))
	     strucargs))))
    (apply #'make-lex-entry strucargs)))

;;; create slot entry
(defun make-psort-struct-aux (slot-key slot-value slot-path)
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

(defmethod clear-scratch ((lexicon psql-database))
  (fn-get-records lexicon ''clear-scratch))

(defmethod close-lex ((lexicon psql-database) &key in-isolation delete)
  (declare (ignore in-isolation delete))
  (disconnect lexicon))

(defmethod open-lex ((lexicon psql-database) &key name parameters)
  (declare (ignore parameters)) ;; for_now 
  (close-lex lexicon)
  (format t "~%Connecting to lexical database ~a@~a:~a as user ~a" 
          (dbname lexicon)
          (host lexicon)
          (port lexicon)
          (user lexicon))
  (let* ((connection (connect lexicon))
	 (dbversion))
    (setf *postgres-tmp-lexicon* lexicon)
    (cond
     (connection
      (format t "~%Opening ~a" (dbname lexicon))
      (unless (string>= (server-version lexicon) "7.3")
        (error *trace-output* 
               "PostgreSQL server version is ~a. Please upgrade to version 7.3 or above." 
	       (server-version lexicon)))
      (setf dbversion 
	(get-db-version lexicon))
      (unless (string>= (get-db-version lexicon) 
                        *psql-db-version*)
        (if (string>= dbversion "3.00")
            (error "Your database structures (v. ~a) are out of date. Change to directory lkb/src/psql/ and use PSQL tool to import file import.sql. Ignore WARNING/ERROR messages. (NOTE: existing private schemas will be renamed tmpSCHEMANAME.)" dbversion dbversion)
          (error "Your database structures (v. ~a) are too out of date. You must recreate the database: dump the LexDB using LKB, go to shell prompt and 'dropdb ~a' then 'createdb ~a', then import file lkb/src/psql/import.sql using PSQL tool, and finally merge dumped LexDB into new database." dbversion (dbname lexicon))))
      (make-field-map-slot lexicon)
      (retrieve-fn-defns lexicon)
      (initialize-userschema lexicon)
      (setf (name lexicon) name)
      lexicon)
     (t
      (format t "~%unable to connect to ~s:~%  ~a"
              (pg:db 
	       (connection lexicon)) 
	      (pg:error-message (connection lexicon)))
      nil))))
  
(defmethod initialize-lex ((lexicon psql-database))
  (when (open-lex lexicon)
    (build-lex lexicon)))
  
;; lexicon is open
(defmethod load-lex-from-files ((lexicon psql-lex-database) file-names syntax)
  ;;  (setf *lex-file-list* file-names) ;;fix_me
  (setf *ordered-lex-list* nil) ;;fix_me
  (cond
   ((check-load-names file-names 'lexical)
    (let ((*lexicon-in* lexicon)) ;; *lexicon-in* is needed deep inside read-...-file-aux
      (dolist (file-name file-names)
	(ecase syntax
	  (:tdl (read-tdl-lex-file-aux-internal file-name))
	  (:path (read-lex-file-aux-internal file-name)))))
    t)
   (t
    (cerror "Continue" "Lexicon file not found")
    nil)))
;;;
;;; --- psql-lex-entry methods
;;;

(defun make-instance-psql-lex-entry (&rest rest)
  (make-instance 'psql-lex-entry
    :fv-pairs (kwl2alist rest)))

(defmethod clear-vals ((le psql-lex-entry))
  (setf (slot-value le 'fv-pairs) 
    nil))

(defmethod retr-val ((le psql-lex-entry) f)
  (cdr 
   (assoc f (slot-value le 
			'fv-pairs))))

(defmethod set-val ((le psql-lex-entry) f v)
  (with-slots (fv-pairs) le
    (let ((fv-pair (assoc f fv-pairs)))
      (if fv-pair
	  (setf (cdr fv-pair) v)
	(push (cons f v) fv-pairs)))))

;;; set version to next val
(defmethod set-version ((psql-le psql-lex-entry) (lexicon psql-lex-database))
  (set-val psql-le 
	   :version 
	   (next-version 
	    (retr-val psql-le :name) 
	    lexicon)))

;;;
;;; db filter
;;;

(defmethod set-filter ((lexicon psql-lex-database) &rest rest)  
  (let* ((old-filter (get-filter lexicon))
	 (filter
	  (cond
	   ((= (length rest) 0)
	    (ask-user-for-x "Alter filter" 
			    (cons "New filter?" 
				  old-filter)))
	   ((= (length rest) 1)
	    (first rest))
	   (t
	    (error "too many arguments")))))
    (when (or (null filter) (string= filter old-filter))
      (format t "Database filter unchanged")
      (return-from set-filter))
    (when (catch 'pg:sql-error
	    (format *postgres-debug-stream* 
		    "~%(applying new filter to db and rebuilding current grammar)")
	    (fn-get-records lexicon ''initialize-current-grammar filter)
	    (empty-cache lexicon)
	    nil)
      (lkb-beep)
      (set-filter lexicon))
    (format *postgres-debug-stream* 
	    "~%(lexicon filter: ~a )" 
	    (get-filter lexicon))
    (format *postgres-debug-stream* 
	    "~%(active lexical entries: ~a )" 
	    (fn-get-val lexicon ''size-current-grammar))))

(defmethod set-filter-text-only ((lexicon psql-database) filter)
  (fn-get-records lexicon 
		  ''initialize-current-grammar 
		  filter)
  (empty-cache lexicon))

(defun set-filter-psql-lexicon (&rest rest)
  (apply #'set-filter 
	 (cons *psql-lexicon* rest)))

;;;
;;; LexDB menu commands
;;;

(defun command-merge-into-psql-lexicon (&rest rest)
  (let ((filename
	 (cond
	  ((= (length rest) 0)
	   (ask-user-for-existing-pathname "CSV file?"))
	  ((= (length rest) 1)
	   (first rest))
	  (t
	   (error "too many arguments")))))
    (if (and
	 (> (- (length filename) 4) 
	    0)
	 (equal (subseq filename 
			(- (length filename) 
			   4)) 
		".csv"))
	(setf filename (subseq filename 
			       0 
			       (- (length filename) 4))))
    (when filename
      (format t 
	      "~%Merging files ~a.* into lexical database ~a" 
	      filename 
	      (dbname *psql-lexicon*))
      (merge-into-psql-lexicon2 *psql-lexicon* filename)
      (lkb-beep))))
  
(defun command-dump-psql-lexicon (&rest rest)
  (let ((filename
	 (cond
	  ((= (length rest) 0)
	   (ask-user-for-new-pathname "CSV file?"))
	  ((= (length rest) 1)
	   (first rest))
	  (t
	   (error "too many arguments")))))
    (if (and
	 (> (- (length filename) 
	       4) 
	    0)
	 (equal (subseq filename (- (length filename) 
				    4)) 
		".csv"))
	(setf filename 
	  (subseq filename 
		  0 
		  (- (length filename) 
		     4))))
    (when filename
      (format t 
	      "~%Dumping lexical database ~a to files ~a.*" 
	      (dbname *psql-lexicon*) 
	      filename)
      (dump-psql-lexicon filename)
      (lkb-beep))))
  
(defun command-export-lexicon-to-tdl (&rest rest)
  (let ((filename
	 (cond
	  ((= (length rest) 0)
	   (ask-user-for-new-pathname "TDL file?"))
	  ((= (length rest) 1)
	   (first rest))
	  (t
	   (error "too many arguments")))))
    (when filename
      (export-lexicon-to-tdl :file filename)
      (lkb-beep))))
  
(defun command-set-filter-psql-lexicon (&rest rest)
  (apply 'set-filter-psql-lexicon rest)
  (lkb-beep))

(defun command-clear-scratch nil
  (format t "~%Clearing scratch entries")
  (close-scratch-lex)
  (lkb-beep))

(defun command-commit-scratch nil
  (format t "~%Committing scratch entries")
  (commit-scratch-lex)
  (lkb-beep))

(defun command-show-scratch nil
  (format t "~%Contents of scratch: ~a"
	  (mapcar 
	   #'(lambda (x) (cdr (first x))) 
	   (show-scratch *psql-lexicon*)))
  (lkb-beep))

;;;
;;; cache
;;;

(defmethod cache-all-lex-records ((lexicon psql-lex-database))
  (with-slots (record-cache) lexicon
    (clrhash record-cache)
    (mapc
     #'(lambda (record)  
	 (setf (gethash (record-id record) record-cache) 
	   record))
     (retrieve-all-records lexicon 
			   (make-requested-fields lexicon)))))

(defmethod cache-all-lex-records-orth ((lexicon psql-lex-database))
  (with-slots (record-cache lexical-entries) lexicon
    (clrhash lexical-entries)
    (mapc 
     #'(lambda (record) 
	 (let* ((id (record-id record))
		(orth (record-orth record)))
	   (mapc 
	    #'(lambda (y)
		(setf (gethash y lexical-entries) 
		  (cons id 
			(gethash y lexical-entries))))
	    (split-into-words orth))
	   (setf (gethash (record-id record) 
			  record-cache) 
	     record) ))
     (retrieve-all-records lexicon 
			   (make-requested-fields lexicon)))))

(defun i (&optional (slot 'record-cache)) (inspect (slot-value *lexicon* slot)))
