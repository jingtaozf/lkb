;;; Copyright (c) 2002-2003 
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

(defvar *psql-db-version* "3.03")
(defvar *psql-port-default* nil)
(defvar *psql-lexicon-parameters*) ;; define in GRAMMAR/globals.lsp

(defvar *postgres-tmp-lexicon* nil)
(defvar *psql-lexicon* nil)
(defvar *psql-verbose-query* nil) ;;; flag

(defvar *postgres-export-timestamp*) ;;; see lexport.lsp
(defvar *postgres-current-source* "?")
(defvar *postgres-current-user* nil)
(defvar *postgres-current-lang* nil)
(defvar *postgres-current-country* nil)

(defvar *postgres-temp-filename*)
(defvar *postgres-user-temp-dir*)

(defvar *postgres-debug-stream* t)

(defvar *postgres-mwe-enable* nil)

(defun initialize-psql-lexicon (&key
                                (db (extract-param :db *psql-lexicon-parameters*))
                                (host (extract-param :host *psql-lexicon-parameters*))
                                (table (extract-param :table *psql-lexicon-parameters*)))
  (unless (and db host table)
    (error "please instantiate db+host+table in *psql-lexicon-parameters*"))
  (if (extract-param :user *psql-lexicon-parameters*)
      (setf *postgres-current-user* (extract-param :user *psql-lexicon-parameters*))
    (setf *postgres-current-user* (sys:user-name)))
  (let ((part-of))   
    (if *psql-lexicon*
        (setf part-of (part-of *psql-lexicon*))
      (setf *psql-lexicon* (make-instance 'psql-lex-database)))
    (setf (dbname *psql-lexicon*) db)
    (setf (host *psql-lexicon*) host)
    (setf (lex-tb *psql-lexicon*) table) ;;unused
    (setf (fields-tb *psql-lexicon*) table)
    (initialize-lex *psql-lexicon*)
    (mapcar #'(lambda (x) (link *psql-lexicon* x)) part-of)
    *psql-lexicon*))

(defun load-psql-lexicon-from-script nil
  (clear-lex *lexicon* :no-delete t)
  (initialize-psql-lexicon)
  (setf *lexicon* *psql-lexicon*))
  
;; obsolete
(defun open-psql-lex (&rest rest)
  (apply 'initialize-psql-lexicon rest))

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
  (
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
  ((sql-string :accessor sql-string :initarg :sql-string)
   (records :accessor records)
   (columns :accessor columns)
   (count-records :accessor count-recs)
   (mapped-recs :accessor mapped-recs)))

(defclass psql-lex-entry ()
  ((fv-pairs :initarg :fv-pairs)))

;;;
;;; --- sql-database methods
;;;

(defmethod clear-lex ((lexicon sql-database) &rest rest)
  (declare (ignore rest))
  (setf (dbname lexicon) nil)
  (setf (host lexicon) nil)
  (setf (user lexicon) nil)
  (setf (password lexicon) nil)
  (setf (fns lexicon) nil)) ;:todo: unbind functions

;;;
;;; --- psql-database methods
;;;

(defmethod load-lex ((lexicon psql-database) &rest rest)
  (apply 'initialize-psql-lexicon rest))

(defmethod connect ((lexicon psql-database)) 
  (let ((user *postgres-current-user*))
    (cond
     ((and user (eq (connect-aux lexicon :user user) :connection-ok)))
     (t
      (setf user (ask-user-for-x "Connect to PostgreSQL lexicon" 
				 (cons "Username?" (or user "guest"))))
      (unless user 
	(throw 'abort 'connect))
      (when user
	(setf *postgres-current-user* user)
	(connect lexicon))))))
      
(defmethod connect-aux ((lexicon psql-database) &key (user "guest"))
  (with-slots (port host dbname connection) lexicon
    (let ((decoded-status nil)
	  (password nil))
      ;; attempt connection w/o pwd
      (setf connection (connect-aux2 :port port :host host :dbname dbname :user user))
      (setf decoded-status (pg:decode-connection-status (pg:status connection)))
      (unless (eq decoded-status :connection-ok)
	;: attempt connection w/ default pwd
	(setf connection (connect-aux2 :port port :host host :dbname dbname :user user :password user))
	(setf decoded-status (pg:decode-connection-status (pg:status connection)))
	(unless (eq decoded-status :connection-ok)
	;: attempt connection w/ pwd
	  (setf password (ask-user-for-x "Connect to PostgreSQL lexicon" 
					 (cons (format nil "Password for ~a?" user) user)))
	  (when password
	    (setf connection (connect-aux2 :host host :dbname dbname :user user :password password)))))
      (setf decoded-status (pg:decode-connection-status (pg:status connection)))
      (when (eq decoded-status :connection-ok)
	  (setf (server-version lexicon) (get-server-version lexicon))
	  (setf (user lexicon) user))
      decoded-status)))

(defun connect-aux2 (&key (host) (dbname) (user) (password) (port))
  (let ((connection)
	(decoded-status))
    (setf connection 
      (pg:connect-db 
       (concatenate 'string 
	 (if port (format nil "port='~a' " (sql-escape-string port)))
	 (format nil "host='~a' " (sql-escape-string host))
	 (format nil "dbname='~a' " (sql-escape-string dbname))
	 (format nil "user='~a' " (sql-escape-string user))
	 (if password (format nil "password='~a'" (sql-escape-string password))
					""))))
    (setf decoded-status (pg:decode-connection-status (pg:status connection)))
    (unless (eq decoded-status :connection-ok) ;: in case postgres is running locally w/o TCPIP...
      (setf connection 
	(pg:connect-db
	 (concatenate 'string 
           (if port (format nil "port='~a' " (sql-escape-string port)))
	   (format nil "dbname='~a' " (sql-escape-string dbname))
	   (format nil "user='~a' " (sql-escape-string user))
	   (if password (format nil "password='~a'" (sql-escape-string password))
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

(defmethod run-query ((database psql-database) (query sql-query))
  (let ((connection (connection database)))
    (unless connection
      (error "database ~s has no active connection." database))
    (multiple-value-bind (recs cols recs-count)
        (pg:sql (sql-string query) :db connection)
      (setf (records query) recs 
            (columns query) (mapcar #'str-2-keyword cols)
            (count-recs query) recs-count))
    (if *psql-verbose-query*
	(format *trace-output* "~%~a~%=>~%~a" (sql-string query) (records query)))
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
  (let* ((fields (remove-duplicates (mapcar #'cadr (fields-map lexicon))
                                    :test #'equal))
         (fields-str (symb-2-str (pop fields))))
    (loop 
        for element in fields
        do (setf fields-str (concatenate 'string fields-str ", " (symb-2-str element))))
    fields-str))

;; this is to avoid being annoyed when word not in the database.
(defmethod collect-expanded-lex-ids ((lexicon external-lex-database))
  ;(error "collect-expanded-lex-ids(): invalid method on PostGreSQL lexicon")
  )

(defmethod clear-lex ((lexicon external-lex-database) &key no-delete psorts-temp-file)
  (declare (ignore no-delete psorts-temp-file))
  (setf (lex-tb lexicon) nil) ;; unused
  (setf (fields-map lexicon) nil)
  (setf (fields-tb lexicon) nil))

;;;
;; --- psql-lex-database methods
;;;

(defmethod lookup-word ((lexicon psql-lex-database) orth &key (cache t))
  (setf orth (string-downcase orth))
  (let ((hashed (gethash orth 
			 (slot-value lexicon 'lexical-entries))))
    (cond 
     (hashed
      (if (eq hashed 'EMPTY)
	  (setf hashed nil))
      hashed)
     (t 
      (let ((value (lookup-word-psql-lex-database lexicon orth)))
	;;if caching, add entry to cache...
	(when cache
	  (setf (gethash orth (slot-value lexicon 'lexical-entries)) 
	    (if value 
		value 
	      'EMPTY)))
	value)))))

(defun lookup-word-psql-lex-database (lexicon orth)
  (declare (ignore cache))
  (if (connection lexicon)
       (let* ((orthstr orth)
	     (sql-str (sql-retrieve-entries-by-orthkey lexicon 
						       (make-requested-fields lexicon) 
						       orthstr))
	     (query-res (run-query 
			 lexicon 
			 (make-instance 
			     'sql-query 
			   :sql-string sql-str)))
	     (ids (lookup-word-aux query-res lexicon)))
	ids)))

(defun lookup-word-aux (query-res lexicon)
  (with-slots (psorts) lexicon
    (let* ((records (make-column-map-record query-res))
	 (name-field (second (assoc :id (fields-map lexicon)))))
    (loop
	for record in records
	for id = (str-2-symb 
		  (cdr 
		   (assoc name-field record :test #'equal)))
	do
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
                     (make-instance 'sql-query :sql-string sql-str))))
      (mapcan #'(lambda (x) (split-into-words (string-upcase (car x))))
	      (records query-res))))

(defmethod collect-psort-ids ((lexicon psql-lex-database)  &key (recurse t))
  (declare (ignore recurse))
  (let* ((sql-str (sql-lex-id-set lexicon))
          (query-res (run-query 
                     lexicon 
                     (make-instance 'sql-query :sql-string sql-str))))
    (mapcar #'(lambda (x) (str-2-symb (car x)))
            (records query-res))))

(defmethod retrieve-all-records ((lexicon psql-lex-database) &optional reqd-fields)
  (unless (connection lexicon)
    (format t "~%WARNING: no connection to psql-lex-database")
    (return-from retrieve-all-records nil))
  (unless reqd-fields 
    (setf reqd-fields "*"))
  (let* ((sql-str (sql-retrieve-all-entries lexicon reqd-fields))
	 (query-res (run-query 
		     lexicon 
		     (make-instance 'sql-query :sql-string sql-str))))
    (when (records query-res) (make-column-map-record query-res))))

(defmethod retrieve-record ((lexicon psql-lex-database) id &optional reqd-fields)
  (retrieve-record-str lexicon (symb-2-str id) reqd-fields))

(defmethod retrieve-record-str ((lexicon psql-lex-database) id-str &optional reqd-fields)
  (unless (connection lexicon)
    (format t "~%WARNING: no connection to psql-lex-database")
    (return-from retrieve-record-str nil))
  (unless reqd-fields 
    (setf reqd-fields "*"))
  (let* ((sql-str (sql-retrieve-entry lexicon reqd-fields id-str))
	 (query-res (run-query 
		     lexicon 
		     (make-instance 'sql-query :sql-string sql-str)))
	 (records (when (records query-res) (make-column-map-record query-res))))
    (if (> (length records) 1)
	(error (format nil "database error (too many records returned)"))
	      (car records))))

(defmethod read-psort ((lexicon psql-lex-database) id &key (cache t) (recurse t))
  (declare (ignore recurse))
  (with-slots (psorts) lexicon
    (let ((hashed (gethash id psorts)))
      (cond (hashed
	     (unless (eq hashed 'EMPTY)
	       hashed))
	    (t
	     (let* ((record (retrieve-record lexicon id (make-requested-fields lexicon)))
		    (entry (if record (make-psort-struct lexicon record))))
	       (when cache
		 (setf (gethash id psorts)
		   (or entry 'EMPTY)))
	       entry))))))

(defmethod make-psort-struct ((lexicon psql-lex-database) query-res)
  (let* ((strucslots 
          (loop 
              for (slot-key slot-field slot-path slot-type) 
              in (fields-map lexicon)
              for slot-value-list = 
		(work-out-value 
                                slot-type 
                                (get-val slot-field query-res)
				:path slot-path
				)		
                               ;; if empty third argument (ie. path), 
                               ;; then add (:key "field")
	      when slot-value-list
              append (mapcar 
		      #'(lambda (x) (make-psort-struct-aux slot-key x slot-path)) 
		      slot-value-list)))
         ;; groups slots with same key together in a list
         (strucargs 
          (loop
              for unique-slot in (remove-duplicates (mapcar #'car strucslots))
              append
		(list unique-slot 
                           (let ((values (loop 
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
      (if (and (listp orth-str) (cdr orth-str))
	  ;; infl-pos is only relevant for multi-word entries
	  (setf strucargs
	    (append (list :infl-pos
			  (find-infl-pos nil orth-str nil))
		    strucargs))))
    
    (apply #'make-lex-entry strucargs)))

;;; create slot entry
(defun make-psort-struct-aux (slot-key slot-value slot-path)
  (cond
   ;: nil path => no unification
   ((equal slot-path "")
    (list slot-key slot-value))
   ;: atomic value => simple case
   ((atom slot-value)
    (list slot-key
	  (make-unification
	   :lhs (make-path :typed-feature-list 
			   (work-out-value "list" slot-path))
	   :rhs (make-u-value :type slot-value))))
   ;: list. eg. (rest first "word") => (... rest first) has val "word"  
   ((listp slot-value)
    (list slot-key
	  (make-unification
	   :lhs (make-path 
		 :typed-feature-list (append
				      (work-out-value "list" slot-path)
				      (reverse (cdr (reverse slot-value)))))
	   :rhs (make-rhs-val (car (last slot-value))))))
  (T (error "unhandled input"))))

(defun make-rhs-val (x)
  (cond
   ((listp x)
    (make-path :typed-feature-list x))
   (t
    (make-u-value :type x))))

;;; insert lex entry into db
(defmethod set-lex-entry ((lexicon psql-lex-database) (psql-le psql-lex-entry))
  (set-val psql-le :modstamp "NOW")
  (set-val psql-le :userid *postgres-current-user*)
  (set-val psql-le :flags 1)
  
  (set-lex-entry-aux lexicon psql-le))
  
(defmethod set-lex-entry-aux ((lexicon psql-lex-database) (psql-le psql-lex-entry))
  (set-version psql-le lexicon) 
  (if *postgres-export-timestamp* (set-val psql-le :modstamp *postgres-export-timestamp*))
  (set-val psql-le :flags 1)
  (let* ((symb-list '(:type :orthography :orthkey :keyrel :altkey :alt2key :keytag :altkeytag :compkey :ocompkey :comments :exemplars :lang :country :dialect :domains :genres :register :confidence :version :source :flags :modstamp :userid))
	 (symb-list (remove-if #'(lambda (x) (or (null x) 
						 (and (stringp x)
						      (string= x ""))))
			       symb-list
			       :key #'(lambda (x) (retr-val psql-le x))))) 
    (run-query lexicon 
	       (make-instance 'sql-query
		 :sql-string (format nil
				     (fn lexicon 
					 'update-entry 
					 (retr-val psql-le :name) 
					 (sql-select-list-str symb-list) 
					 (sql-val-list-str symb-list psql-le)))))))

(defmethod clear-scratch ((lexicon psql-database))
  (fn-get-records lexicon ''clear-scratch))

(defmethod clear-lex ((lexicon psql-database) &key no-delete psorts-temp-file)
  (declare (ignore no-delete psorts-temp-file))
  (disconnect lexicon))

(defmethod initialize-lex ((lexicon psql-database) &key no-delete psorts-temp-file)
  (declare (ignore no-delete psorts-temp-file))
  (clear-lex lexicon)
  (if *postgres-tmp-lexicon* 
      (clear-lex *postgres-tmp-lexicon*))
  (format t "~%Connecting to lexical database ~a as user ~a" (dbname lexicon) (user lexicon))
  (let* ((connection (connect lexicon))
	 (dbversion)
	 )
      (setf *postgres-tmp-lexicon* lexicon)
      (format t "~%(Re)initializing ~a" (dbname lexicon))
      (cond
       (connection
	(unless (string>= (server-version lexicon) 
			  "7.3")
	  (error *trace-output* 
		 "PostgreSQL server version is ~a. Please upgrade to version 7.3 or above." (server-version lexicon)))
	(setf dbversion (get-db-version lexicon))
	(unless (string>= (get-db-version lexicon) 
			  *psql-db-version*)
	  (if (string>= dbversion "3.00")
	      (error "Your database structures (v. ~a) are out of date. Use PSQL tool to import file lkb/src/psql/import.sql. Ignore WARNING/ERROR messages. (NOTE: existing private schemas will be removed.)" dbversion dbversion)
	    (error "Your database structures (v. ~a) are too out of date. You must recreate the database: dump the LexDB using LKB, go to shell prompt and 'dropdb ~a' then 'createdb ~a', then import file lkb/src/psql/import.sql using PSQL tool, and finally merge dumped LexDB into new database." dbversion (dbname lexicon))))
	(make-field-map-slot lexicon)
	(retrieve-fn-defns lexicon)
	
	(initialize-userschema lexicon)
	
	;;(clear-scratch lexicon)
	
	(format *postgres-debug-stream* "~%(building current_grammar)")
	(fn-get-records lexicon ''initialize-current-grammar (get-filter lexicon))
	)
       (t
	(error 
	 "unable to connect to ~s: ~a"
	 (pg:db (connection lexicon)) (pg:error-message (connection lexicon)))))
      (setf *postgres-tmp-lexicon* nil)
      lexicon))
  
;;; hack (psql-lex-database does not use temporary lexicon files)
(defmethod delete-temporary-lexicon-files ((lexicon psql-lex-database))
  ;;; does nothing
  )

;;;
;;; --- psql-lex-entry methods
;;;

(defun make-instance-psql-lex-entry (&rest rest)
  (make-instance 'psql-lex-entry
    :fv-pairs (kwl2alist rest)))

(defmethod clear-vals ((le psql-lex-entry))
  (setf (slot-value le 'fv-pairs) nil))

(defmethod retr-val ((le psql-lex-entry) f)
  (cdr (assoc f (slot-value le 'fv-pairs))))

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

(defmethod set-filter ((lexicon psql-lex-database))  
  (let* ((old-filter (get-filter lexicon))
	(filter
         (ask-user-for-x "Alter filter" 
                         (cons "New filter?" old-filter))))
    (when (or (null filter) (string= filter old-filter))
      (format t "Database filter unchanged")
      (return-from set-filter))
    (when (catch 'pg:sql-error
	  (format *postgres-debug-stream* "~%(applying new filter to db and rebuilding current grammar)")
	  (fn-get-records lexicon ''initialize-current-grammar filter)
	  (empty-cache lexicon)
	  nil)
      (lkb-beep)
      (set-filter lexicon))
    (format *postgres-debug-stream* "~%(new filter: ~a )" filter)))

(defun set-filter-psql-lexicon nil
  (set-filter *psql-lexicon*))

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
	 (> (- (length filename) 4) 0)
	 (equal (subseq filename (- (length filename) 4)) ".csv"))
	(setf filename (subseq filename 0 (- (length filename) 4))))
    (when filename
      (format t "~%Merging files ~a.* into lexical database ~a" filename (dbname *psql-lexicon*))
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
	 (> (- (length filename) 4) 0)
	 (equal (subseq filename (- (length filename) 4)) ".csv"))
	(setf filename (subseq filename 0 (- (length filename) 4))))
    (when filename
      (format t "Dumping lexical database ~a to files ~a.*" (dbname *psql-lexicon*) filename)
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
      ;;(format t "Dumping lexical database ~a to (TDL format) file ~a" (dbname *psql-lexicon*) filename)
      (export-lexicon-to-tdl :file filename)
      (lkb-beep))))
  
(defun command-set-filter-psql-lexicon (&rest rest)
  (apply 'set-filter-psql-lexicon rest)
  (lkb-beep))

(defun command-load-tdl-to-scratch nil
  (catch 'abort 
    (unless *psql-lexicon*
      (error "~%no *psql-lexicon*!"))
    (let ((filename (ask-user-for-existing-pathname "TDL file?")))
      (when filename
	(let ((lexicon (load-scratch-lex :filename filename)))
	  (setf *postgres-current-source* 
	    (ask-user-for-x 
	     "Export Lexicon" 
	     (cons "Source?" (or (extract-pure-source-from-source *postgres-current-source*) ""))))
	  (unless *postgres-current-source* (throw 'abort 'source))
	  (setf *postgres-current-lang* 
	    (ask-user-for-x 
	     "Export Lexicon" 
	     (cons "Language code?" (or *postgres-current-lang* "EN"))))
	  (unless *postgres-current-lang* (throw 'abort 'lang))
	  (setf *postgres-current-country* 
	    (ask-user-for-x 
	     "Export Lexicon" 
	     (cons "Country code?" (or *postgres-current-country* "UK"))))
	  (unless *postgres-current-country* (throw 'abort 'country))
	  (export-to-db lexicon *psql-lexicon*)
	  (clear-lex lexicon)
	  (lkb-beep))))))

(defun command-clear-scratch nil
  (format t "~%Clearing scratch entries")
  (clear-scratch-lex)
  (lkb-beep))

(defun command-commit-scratch nil
  (format t "~%Committing scratch entries")
  (commit-scratch-lex)
  (lkb-beep))

(defun command-show-scratch nil
  (format t "~%Contents of scratch: ~a"
	  (mapcar #'(lambda (x) (cdr (first x))) (show-scratch *psql-lexicon*)))
  (lkb-beep))

(defun command-generate-semi nil
  (unless (typep *lexicon* 'psql-lex-database)
    (error "You need to load the LexDB before generating the SEMI..."))
  (format *postgres-debug-stream* "~%(caching all lexical entries)")
  (cache-all-lex-entries *lexicon*)

  (format *postgres-debug-stream* "~%(dumping semi files)")
  (dump-obj-semi *lexicon*)
  (format *postgres-debug-stream* "~%(loading semi into db)")
  (fn-get-record *lexicon* ''load-semi)
  
  (format *postgres-debug-stream* "~%(clearing cache)")
  (empty-cache *lexicon*)
  (lkb-beep))

;;;
;;; cache
;;;

(defmethod cache-all-lex-entries ((lexicon psql-lex-database))
  (with-slots (psorts) lexicon
    (mapc 
     #'(lambda (x) 
	 (let ((id (record-id x)))
	   (unless (gethash id psorts) 
	     (setf (gethash id psorts) (make-psort-struct lexicon x)))))
     (retrieve-all-records lexicon (make-requested-fields lexicon)))))

(defmethod cache-all-lex-entries-orth ((lexicon psql-lex-database))
  (with-slots (psorts lexical-entries) lexicon
    (clrhash lexical-entries)
    (mapc 
     #'(lambda (x) 
	 (let* ((id (record-id x))
	       (orth (record-orth x)))
	   (mapc 
	    #'(lambda (y)
		(setf (gethash y lexical-entries) 
		  (cons id (gethash y lexical-entries))))
	    (split-into-words orth))
	   
	   (unless (gethash id psorts) 
	     (setf (gethash id psorts) (make-psort-struct lexicon x)))))
     (retrieve-all-records lexicon (make-requested-fields lexicon)))))

;;CREATE OR REPLACE FUNCTION build_current_grammar () RETURNS boolean AS
;;''
;;
;;SELECT CASE
;;  WHEN
;;    coalesce((SELECT max(modstamp) FROM revision_all),''''infinity'''')
;;    >=
;;    coalesce((SELECT val FROM meta WHERE var=''''build_time'''' LIMIT 1),''''-infinity'''')
;;      THEN build_current_grammar_aux()
;;  ELSE false
;;  END;
;;
;;'' 
;;LANGUAGE SQL;

