;;; Copyright (c) 2002 
;;;   Ann Copestake, Fabre Lambeau, Stephan Oepen, Ben Waldron;
;;;   see `licence.txt' for conditions.

;;; modifications by bmw (aug-03)
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
;;;  - add insertion of additional lexical entries support from LKB; maybe use
;;;    emacs(1) forms for input (and the emacs(1) -- lisp interface, such that
;;;    no extra installation overhead is incurred, e.g. for a web server to
;;;    talk to PostGres);
;;;  - rework connection handling to re-open on demand (rather than error());
;;;  - support for generator indexing;
;;;  - integrate irregular spellings into lexical DB;
;;;

(in-package :lkb)

(defvar *psql-db-version* "1.9")
(defvar *psql-port-default* nil)

(defvar *tmp-lexicon* nil)
(defvar *psql-lexicon* nil)
(defvar *psql-verbose-query* nil) ;;; flag
(defvar *export-counter*) ;;; see lexport.lsp
(defvar *export-timestamp*) ;;; see lexport.lsp

(defvar *current-source* nil)
(defvar *current-user* nil)
(defvar *current-lang* nil)
(defvar *current-country* nil)

;; obsolete...
(defun open-psql-lex (&rest rest)
  (apply 'initialize-psql-lexicon rest))

					;: set up connection
					;: make-field-map-slot
					;: replace existing *psql-lexicon*
					;: clear lexical entries of *lexicon*
					;: link to lexicon
(defun initialize-psql-lexicon (&key
                                (db "lingo")
                                (host "localhost")
                                (table (or 
					(and
					 (typep *psql-lexicon* 'psql-database)
					 (fields-tb *psql-lexicon*)) 
					"erg")))
  
  (format t "~%Connecting to lexical database ~a" db)
  (let* ((lexicon (make-instance 'psql-lex-database 
                    :dbname db :host host
                    :lex-tb table ;;unused 
		    :fields-tb table))
	 (connection (connect lexicon)))
    (if *tmp-lexicon* (clear-lex *tmp-lexicon*))
    (setf *tmp-lexicon* lexicon)
    (cond
     (connection
      (unless (string>= (server-version lexicon) "7.3")
	(format *trace-output* 
		"~%WARNING: PostgreSQL server version is ~a. Please upgrade to version 7.3 or above." (server-version lexicon)))
      (unless (string>= (get-db-version lexicon) *psql-db-version*)
	(error "Your database structures (v. ~a) are out of date. See the latest script import.sql." (get-db-version lexicon)))
      (make-field-map-slot lexicon)
      (retrieve-fn-defns lexicon)
      (when *psql-lexicon* (clear-lex *psql-lexicon*))
      (setf *psql-lexicon* lexicon)
      (fn-get-records lexicon 
		      ''set-current-view 
		      (get-filter *psql-lexicon*)
		      )
      (fn-get-records lexicon ''initialize-current-grammar)
	(setf *lexicon* *psql-lexicon*))
     (t
      (error 
       "unable to connect to ~s: ~a"
       (pg:db (connection lexicon)) (pg:error-message (connection lexicon)))))
    (setf *tmp-lexicon* nil)
    lexicon))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface to a Postgres database
;;;

(defclass sql-database ()
  ((dbname :initform nil :accessor dbname	:initarg :dbname)
   (host :initform "localhost" :accessor host :initarg :host)
   (user :initform (sys:user-name) :accessor user :initarg :user)
   (password :initform "" :accessor password :initarg :password)
   (port :initform (num-2-str *psql-port-default*) :accessor port :initarg :port)
   
   ))

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
  (
   ;(id :initform nil :accessor id)
   (name :initform nil :accessor name :initarg :name)
   (type :initform nil :accessor type :initarg :type)
   (orthography :initform nil :accessor orthography :initarg :orthography)
   (orthkey :initform nil :accessor orthkey :initarg :orthkey)
   ;pronunciation
   (keyrel :initform nil :accessor keyrel :initarg :keyrel)
   (altkey :initform nil :accessor altkey :initarg :altkey)
   (alt2key :initform nil :accessor alt2key :initarg :alt2key)
   (keytag :initform nil :accessor keytag :initarg :keytag)
   (compkey :initform nil :accessor compkey :initarg :compkey)
   (ocompkey :initform nil :accessor ocompkey :initarg :ocompkey)
   ;complete, semclasses, preferences, classifier, selectrest
   (comments :initform nil :accessor comments :initarg :comments)
   (exemplars :initform nil :accessor exemplars :initarg :exemplars)
   ;usages
   (lang :initform nil :accessor lang :initarg :lang)
   (country :initform nil :accessor country :initarg :country)
   (dialect :initform nil :accessor dialect :initarg :dialect)
   (domains :initform nil :accessor domains :initarg :domains)
   (genres :initform nil :accessor genres :initarg :genres)
   (register :initform nil :accessor register :initarg :register)
   (confidence :initform nil :accessor confidence :initarg :confidence)
   (userid :initform nil :accessor userid :initarg :userid)
   (modstamp :initform nil :accessor modstamp :initarg :modstamp)
   (version :initform nil :accessor version :initarg :version)
   (source :initform nil :accessor source :initarg :source)
   (flags :initform nil :accessor flags :initarg :flags)))

;;;
;;; --- sql-database methods
;;;

(defmethod clear-lex ((lexicon sql-database) &optional no-delete)
  (declare (ignore no-delete))
  (setf (dbname lexicon) nil)
  (setf (host lexicon) nil)
  (setf (user lexicon) nil)
  (setf (password lexicon) nil)
  (setf (fns lexicon) nil)) ;:todo: unbind functions

;;;
;;; --- psql-database methods
;;;

(defmethod load-lexicon ((lexicon psql-database))
  (unless (typep (catch 'abort (initialize-psql-lexicon)) 'psql-database)
    (format t "~%... attempting to fall back to .tdl lexicon")
    (setf *lexicon* (make-instance 'cdb-lex-database))
    (load-lexicon *lexicon*)
    ))

(defmethod connect ((lexicon psql-database)) 
  (let ((user *current-user*))
    (cond
     ((and user (eq (connect-aux lexicon :user user) :connection-ok)))
     (t
      (setf user (ask-user-for-x "Connect to PostgreSQL lexicon" 
				 (cons "Username?" (or user "guest"))))
      (unless user 
	(throw 'abort 'connect))
      (when user
	(setf *current-user* user)
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
    (unless (eq decoded-status :connection-ok) ;: temporary hack
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

    ;(time
    
    (multiple-value-bind (recs cols recs-count)
        (pg:sql (sql-string query) :db connection)
      (setf (records query) recs 
            (columns query) cols
            (count-recs query) recs-count))
    
    ;)
    
    (if *psql-verbose-query* ;;; verbosity flag
	(format *trace-output* "~%~a~%=>~%~a" (sql-string query) (records query)))
    query))

;;; returns _association list_
(defmethod make-column-map-record ((query sql-query))
    (loop 
        for element in (records query)
        collect (mapcar #'cons (columns query) element)))

(defun sql-escape-string (string)
  (if (and string (stringp string))
      (loop
          with padding = 128
          with length = (+ (length string) padding)
          with result = (make-array length
                                    :element-type 'character
                                    :adjustable nil :fill-pointer 0)
          for c across string
          when (char= c #\') do
            (vector-push #\\ result)
            (vector-push c result)
            (when (zerop (decf padding))              (setf padding 42)
              (incf length padding)
              (setf result (adjust-array result length)))
          else do
            (vector-push c result)
          finally
            (return result))
    string))

(defun symbol-to-str-format (expr) ;: must be symbol. will not work for num
  (unless (symbolp expr)
    (error "symbol expected"))
  (string-downcase (string expr)))

(defun str-to-keyword-format (expr)
  (unless (stringp expr)
    (error "string exected"))
  (intern (string-upcase expr) :keyword))

(defun str-to-symbol-format (expr)
  (unless (stringp expr)
    (error "string exected"))
  (intern (string-upcase expr) :lkb))

;;; prepare field list for SQL INSERT INTO query
(defun sql-field-list-str (symb-list)
  (concatenate 'string "(" (sql-select-list-str symb-list) ")"))
  
;;; prepare select list for SQL query
(defun sql-select-list-str (symb-list)
  (if (null symb-list) (error (format nil "non-null list expected")))
  (let ((stream (make-string-output-stream)))
    (format stream "~a" (symbol-to-str-format (pop symb-list)))
    (loop 
	while symb-list
	do 
	  (format stream ",~a" (symbol-to-str-format (pop symb-list))))
    (get-output-stream-string stream)))

;;; prepare val list for SQL INSERT INTO query
(defun sql-val-list-str (symb-list psql-le)
  (if (null symb-list) (error (format nil "non-null list expected")))
  (let ((stream (make-string-output-stream)))
    (format stream "~a" (make-sql-val-str 
			 (slot-value psql-le (pop symb-list))))
    (loop 
	while symb-list
	do 
	  (format stream ",~a" (make-sql-val-str 
				(slot-value psql-le (pop symb-list)))))
    (get-output-stream-string stream)))

;;; create val string for SQL query
(defun make-sql-val-str (x)
  (cond 
   ((null x)
    "")
   ((listp x) ;hack! case orthography
    (format nil "'~a'" (sql-escape-string (str-list-2-str x))))
   ((stringp x)
    (format nil "'~a'" (sql-escape-string x)))
   ((numberp x)
    (format nil "~a" x))
   ((symbolp x)
    (format nil "~a" x))
   (t
    (error (format nil "unhandled data type")))))

(defun str-list-2-str (str-list &optional separator)
  (setf separator 
    (if separator
      (format nil separator)
      " "))
  (let ((stream (make-string-output-stream)))
    (if str-list (format stream "~a" (pop str-list)))
    (loop 
	while str-list
	do 
	  (format stream "~a~a" separator (pop str-list)))
    (get-output-stream-string stream)))

(defun str-2-num (str)
  (let ((symb (str-2-symb str)))
    (if (numberp symb)
	symb
      0)))
  
(defun str-2-symb (str)
  (let ((stream (make-string-input-stream str)))
    (read stream nil nil)))

(defun non-null-symb-2-str (symb)
  (if symb
      (symb-2-str symb)))
  
(defun symb-2-str (symb)
  (cond
   ((null symb) "")
   ((numberp symb) (num-2-str symb))
   ((stringp symb) symb)
   (t (string-downcase (string symb)))
   ))
    

(defun num-2-str (num)
  (if num
      (let ((stream (make-string-output-stream)))
        (format stream "~a" num)
        (get-output-stream-string stream))))

(defun char-2-symb (c)
  (str-2-symb (string c)))

(defun get-val (field record)
  (cdr (assoc field record :test #'equal)))

;;;
;;;
;;;

;;;
;; --- external-lex-database methods
;;;

(defmethod make-requested-fields ((lexicon external-lex-database))
  ;; constructs the argument string to sql SELECT with all necessary fields
  (let* ((fields (remove-duplicates (mapcar #'cadr (fields-map lexicon))
                                    :test #'equal))
         (fields-str (pop fields)))
    (loop 
        for element in fields
        do (setf fields-str (concatenate 'string fields-str ", " element)))
    fields-str))

;; this is to avoid being annoyed when word not in the database.
(defmethod collect-expanded-lex-ids ((lexicon external-lex-database))
  ;(error "collect-expanded-lex-ids(): invalid method on PostGreSQL lexicon")
  )

(defmethod clear-lex ((lexicon external-lex-database) &optional no-delete)
  (declare (ignore no-delete))
  (setf (lex-tb lexicon) nil) ;; unused
  (setf (fields-map lexicon) nil)
  (setf (fields-tb lexicon) nil))

;;;
;; --- psql-lex-database methods
;;;

(defmethod lookup-word ((lexicon psql-lex-database) orth &key (cache t))
  (declare (ignore cache))
  (if (connection lexicon)
      (let* (
	     (orthstr (string-downcase (sql-escape-string orth)))
	     (sql-str (sql-retrieve-entries-by-orthkey lexicon (make-requested-fields lexicon) orthstr))
	     (query-res (run-query 
			 lexicon 
			 (make-instance 'sql-query :sql-string sql-str)))
	     (ids (lookup-word-aux query-res lexicon)))
	ids)))

(defun lookup-word-aux (query-res lexicon)
  (with-slots (psorts) lexicon
    (let* (
	 (records (make-column-map-record query-res))
	 (name-field (second (assoc :id (fields-map lexicon))))
	 )
    (loop
	for record in records
	for id = (str-to-symbol-format (cdr (assoc name-field record :test #'equal)))
	do
	  (unless (gethash id psorts)
	    (setf (gethash id psorts) (make-psort-struct lexicon record)))
	 collect id))))

; to be used to control whether the table exists
(defmethod table-existing-p ((lexicon psql-lex-database) tablename)
  (let* ((sql-str (format 
                   nil 
                   "SELECT * FROM pg_tables WHERE tablename='~a';" 
                   tablename))
         (query-res (run-query 
                     lexicon 
                     (make-instance 'sql-query :sql-string sql-str))))
    (when (records query-res) tablename)))

;;; really necessary?
;;; (used to index for generator)
;;; fix_me: inefficient implementation
(defmethod lex-words ((lexicon psql-lex-database))
  (let* (
	 (sql-str (sql-orthography-set lexicon))
         (query-res (run-query 
                     lexicon 
                     (make-instance 'sql-query :sql-string sql-str))))
      (mapcan #'(lambda (x) (split-into-words (string-upcase (car x))))
	      (records query-res))))

(defmethod collect-psort-ids ((lexicon psql-lex-database)  &key (recurse t))
  (declare (ignore recurse))
  (let* (
	 (sql-str (sql-psort-id-set lexicon))
          (query-res (run-query 
                     lexicon 
                     (make-instance 'sql-query :sql-string sql-str))))
    (mapcar #'(lambda (x) (str-to-symbol-format (car x)))
            (records query-res))))

(defmethod retrieve-record ((lexicon psql-lex-database) id)
  (let* (
	 (sql-str (sql-retrieve-entry lexicon (make-requested-fields lexicon) (symbol-to-str-format id)))
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
    (cond ((gethash id psorts))
	  (t
	   (let* ((record (retrieve-record lexicon id))
		  (entry (if record (make-psort-struct lexicon record))))
	     (when (and entry cache)
	       (setf (gethash id psorts) entry))
	     entry)))))
	     
;; (store-psort): not required 

(defmethod make-field-map-slot ((lexicon psql-lex-database))
  ;; stores the mapping of fields to lex-entry structure slots
  (setf (fields-map lexicon)
    (mapcar #'(lambda (x) 
                (cons (str-to-keyword-format (car x)) (cdr x)))
            (records (run-query lexicon 
                                (make-instance 'sql-query
                                  :sql-string (format 
                                               nil 
                                               "SELECT slot,field,path,type FROM defn WHERE mode='~a';"
                                               (fields-tb lexicon))))))))

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

;;; redo later...
;;; insert lex entry into db
(defmethod set-lex-entry ((lexicon psql-lex-database) (psql-le psql-lex-entry))
  (set-version psql-le lexicon) 
  (if *export-timestamp* (setf (modstamp psql-le) *export-timestamp*))
  (let* ((symb-list 
	  (mapcan 
	   #'(lambda (x) (unless (null (slot-value psql-le x)) (list x))) 
	   '(type orthography orthkey keyrel altkey alt2key keytag compkey ocompkey comments exemplars lang country dialect domains genres register confidence version source flags modstamp userid)))) 
    (run-query lexicon 
	       (make-instance 'sql-query
		 :sql-string (format 
			      nil
                              (fn lexicon 'update-entry (name psql-le) (sql-select-list-str symb-list) (sql-val-list-str symb-list psql-le))
                 )))))

(defmethod clear-lex ((lexicon psql-database) &optional no-delete)
  (declare (ignore no-delete))
  (disconnect lexicon))

(defmethod read-cached-lex ((lexicon psql-lex-database) filenames)
  (declare (ignore filenames))
  ;(error "read-cached-lex(): invalid method on PostGreSQL lexicon")
  )

(defmethod unexpand-psort ((lexicon psql-lex-database) id)
  (declare (ignore id))
  ;(error "unexpand-psort(): invalid method on PostGreSQL lexicon")
  )

;;; hack (psql-lex-database does not use temporary lexicon files)
(defmethod delete-temporary-lexicon-files ((lexicon psql-lex-database))
  ;: does nothing
  )

(defun orth-string-to-str-list (string)
  ;;
  ;; break orthography string returned from DB at (one or more) spaces
  ;;
  (unless (stringp string)
    (error "string exected"))

  (loop
      with result = nil
      with word = (make-array 42
                              :element-type 'character
                              :adjustable t :fill-pointer 0)
      with stream = (make-string-input-stream string)
      for c = (read-char stream nil nil)
      while c
      when (and (eql c #\space) (not (zerop (length word)))) do
        (push (copy-seq word) result)
        (setf (fill-pointer word) 0)
      when (not (eql c #\space)) do
        (vector-push-extend c word)
      finally
        (when (not (zerop (length word))) (push word result))
        (return (nreverse result))))

;;; returns _list_ of values of appropriate type
(defun work-out-value (type value &key path)
  (cond ((equal type "symbol") 
	 (unless (equal value "")
	   (list (str-to-symbol-format value))))
	((equal type "string")
	 (unless (equal value "")
	   (list value)))
	((equal type "string-list")
	 (list (orth-string-to-str-list value)))
	((equal type "string-fs")
	 (expand-string-list-to-fs-list (orth-string-to-str-list value)))
	((equal type "string-diff-fs")
	 (expand-string-list-to-fs-diff-list (orth-string-to-str-list value) :path path))
	((equal type "list") (unless (equal value "")
			       (read-from-string value)))
	(t (error "unhandled type during database access"))))

;;; eg. ("w1" "w2") -> ((FIRST "w1") (REST FIRST "w2") (REST REST *NULL*)) 
(defun expand-string-list-to-fs-list (string-list)
  (cond
   ((equal string-list nil) 
    (list (list '*NULL*)))
   (t
    (cons (list 'FIRST (first string-list)) 
	  (mapcar #'(lambda (x) (cons 'REST x))
	  (expand-string-list-to-fs-list (cdr string-list)))))))   

;;; eg. ("w1" "w2") path -> ((LIST FIRST "w1") (LIST REST FIRST "w2") (LIST REST REST path)) 
(defun expand-string-list-to-fs-diff-list (string-list &key path)
   (mapcar #'(lambda (x) (cons 'LIST x))
	   (expand-string-list-to-fs-diff-list-aux string-list :path path)))

;;; eg. ("w1" "w2") path -> ((FIRST "w1") (REST FIRST "w2") (REST REST path)) 
(defun expand-string-list-to-fs-diff-list-aux (string-list &key path)
  (cond
   ((equal string-list nil) 
    (list 
     (list 
      (append (work-out-value "list" path) 
	      (list 'LAST)))))
   (t
    (cons (list 'FIRST (first string-list)) 
	  (mapcar #'(lambda (x) (cons 'REST x))
		  (expand-string-list-to-fs-diff-list-aux (cdr string-list) :path path))))))   

;;; returns version, eg. "7.3.2"
(defmethod get-server-version ((lexicon psql-lex-database))
  (let* 
      ((sql-str "SELECT version();")
       (version-str (caar (records (run-query lexicon (make-instance 'sql-query :sql-string sql-str))))))
    (second (split-on-char version-str))))
    
(defmethod get-db-version ((lexicon psql-lex-database))
  (let* 
      ((sql-str "SELECT val FROM meta WHERE var='db-version' LIMIT 1;"))
    (caar (records (run-query lexicon (make-instance 'sql-query :sql-string sql-str))))))
    
(defmethod get-filter ((lexicon psql-lex-database))
  (let* 
      ((sql-str "SELECT val FROM meta WHERE var='filter' LIMIT 1;"))
    (caar (records (run-query lexicon (make-instance 'sql-query :sql-string sql-str))))))
    
(defmethod next-version (id (lexicon psql-lex-database))
  (let* (
	 (sql-str (sql-next-version lexicon (string-downcase id)))
	 (res (caar (records (run-query 
			      lexicon 
			      (make-instance 'sql-query :sql-string sql-str))))))
    (str-2-num res)))

(defmethod get-records ((lexicon psql-lex-database) sql-string)
  (make-column-map-record 
   (run-query 
    *psql-lexicon* 
    (make-instance 'sql-query :sql-string sql-string))))

(defmethod fn-get-records ((lexicon psql-lex-database) fn-name &rest rest)
  (get-records lexicon (eval (append (list 'fn lexicon fn-name) rest))))

(defmethod fn-get-record ((lexicon psql-lex-database) fn-name &rest rest)
  (let ((res (get-records lexicon (eval (append (list 'fn lexicon fn-name) rest)))))
    (if (> (length res) 1)
        (error "too many records returned")
      (first res))))
  
(defmethod fn-get-val ((lexicon psql-lex-database) fn-name &rest rest)
  (let* ((res (get-records lexicon (eval (append (list 'fn lexicon fn-name) rest))))
         (rec (first res)))
    (if (> (length res) 1)
        (error "too many records returned")
      (if (> (length rec) 1)
          (error "multiple columns returned")
        (cdar rec)))))
  
  
(defmethod fn ((lexicon psql-lex-database) fn-name &rest rest)
  (eval (append (list (cdr (assoc fn-name (fns lexicon)))) rest)))
  
(defmethod sql-next-version ((lexicon psql-lex-database) id)
  (fn lexicon 'next-version id))

(defmethod sql-orthography-set ((lexicon psql-lex-database))
  (fn lexicon 'orthography-set))

(defmethod sql-psort-id-set ((lexicon psql-lex-database))
  (fn lexicon 'psort-id-set))

(defmethod sql-lookup-word ((lexicon psql-lex-database) word)
  (fn lexicon 'lookup-word word))

(defmethod sql-retrieve-entries-by-orthkey ((lexicon psql-lex-database) select-list word)
  (fn lexicon 'retrieve-entries-by-orthkey select-list word))

(defmethod sql-retrieve-entry ((lexicon psql-lex-database) select-list word)
  (fn lexicon 'retrieve-entry select-list word))

(defmethod dump-db ((lexicon psql-lex-database) filename)  
  (fn-get-records lexicon ''dump-db filename))

(defmethod dump-multi-db ((lexicon psql-lex-database) filename)  
  (fn-get-records lexicon ''dump-multi-db filename))

(defmethod merge-into-db ((lexicon psql-lex-database) filename)  
  (fn-get-records lexicon ''merge-into-db filename))

(defmethod merge-multi-into-db ((lexicon psql-lex-database) filename)  
  (fn-get-records lexicon ''merge-multi-into-db filename))

(defun dump-psql-lexicon (filename)
  (dump-db *psql-lexicon* filename))

(defun dump-multi-psql-lexicon (filename)
  (dump-multi-db *psql-lexicon* filename))

(defun merge-into-psql-lexicon (filename)
  (unless
      (and *psql-lexicon* (connection *psql-lexicon*))
    (initialize-psql-lexicon))
  (merge-into-db *psql-lexicon* filename)
  (initialize-psql-lexicon))

(defun merge-multi-into-psql-lexicon (filename)
  (unless
      (and *psql-lexicon* (connection *psql-lexicon*))
    (initialize-psql-lexicon))
  (merge-multi-into-db *psql-lexicon* filename)
  (initialize-psql-lexicon))

(defmethod retrieve-fn-defns ((lexicon psql-lex-database))
  (let* ((sql-str (format nil "SELECT * FROM qry;"))
	 (records (make-column-map-record (run-query 
                     lexicon 
                     (make-instance 'sql-query :sql-string sql-str)))))
    (loop
      for record in records
	do
	  (retrieve-fn-defn lexicon record))))

(defmethod retrieve-fn-defn ((lexicon psql-lex-database) record)
  (let* ((fn (get-val "fn" record))
	 (arity (str-2-num (get-val"arity" record)))
	 (sql-code (get-val "sql_code" record))
	 (sql-str (format nil "SELECT * FROM qrya WHERE fn='~a';" fn))
	 (ergqa-records 
	  (make-column-map-record 
	   (run-query 
	    lexicon 
	    (make-instance 'sql-query :sql-string sql-str))))
	 (type-list 
	  (mapcar #'(lambda (record) 
		      (cons 
		       (str-2-num (get-val "arg" record))
		       (str-2-symb (get-val "type" record))))
		  ergqa-records)))
    (unless (= arity (length type-list))
      (error "wrong number of argument defns"))
    (push (cons (str-2-symb fn) 
		(make-db-access-fn fn sql-code type-list))
	  (fns lexicon))))

(defun make-db-access-fn (str-fn-name-in str type-list)
  (let* ((fn-name (new-fn-name (concatenate 'string "sql-query-string-" (string str-fn-name-in))))
	 (tmp (prepare-db-access-fn str type-list))
	 (format-cmd (append '(format nil) (car tmp)))
	 (args (cdr tmp))
	 (fn-defn (list 'defun fn-name args format-cmd)))
    (print fn-defn)
    (eval fn-defn)))

(defun new-fn-name (str)
  (loop
      with i = 0
      with fn-name
      do
	(setf fn-name (str-2-symb (concatenate 'string str (num-2-str i))))
	(unless (fboundp fn-name)
	  (return fn-name))
	(setf i (1+ i))))
	
(defun prepare-db-access-fn (str type-list)
  (let ((stream (make-string-output-stream))
	(args)
	(arg-vars '(a b c d e f g h i j))
	(arity (length type-list)))
  (loop
      with max = (1- (length str))
      and c
      for i from 0 to max
      with max-arg = -1
      and arg
      and type
      and explicit-type-str
      do
	(setf c (aref str i))
	(cond 
	 ((eq c #\~)
	  (format stream "~~~~"))
	 ((eq c #\\)
	  (if (= i max)
	      (error "invalid string ('\\' cannot be string final)"))
	  (format stream "~a" (aref str (1+ i)))
	  (setf i (1+ i)))
	 ((eq c #\$)
	  (if (= i max)
	      (error "invalid string ('$' cannot be string final)"))
	  (unless (numberp (char-2-symb (aref str (1+ i))))
	    (error "invalid string ('$' can only preceed a digit)"))
	  (setf arg (char-2-symb (aref str (1+ i))))
	  (if (> arg (1- arity))
	      (error "too many arguments"))
	  (setf max-arg (max max-arg arg))
	  (setf type (cdr (assoc arg type-list)))
	  (setf explicit-type-str (get-explicit-type str (1+ i)))
	  (when explicit-type-str
	    (setf type (str-2-symb explicit-type-str))
	    (setf i (+ i 1 (length explicit-type-str))))
	  (cond
	   ((equal type 'text)
	    (format stream "'~~a'")
	    (push (list 'sql-embedded-text (nth arg arg-vars)) args))
	   ((equal type 'int)
	    (format stream "~~a"))
	   ((equal type 'select-list)
	    (format stream "~~a")
	    (push (nth arg arg-vars) args))
	   ((equal type 'value-list)
	    (format stream "~~a")
	    (push (nth arg arg-vars) args))
	   ((equal type 'where-subcls)
	    (format stream "~~a")
	    (push (nth arg arg-vars) args))
	   (t
	    (error "unknown type: ~A" type)))
	  (setf i (1+ i)))
	 (t
	  (format stream "~a" (aref str i)))))
  (cons (cons (get-output-stream-string stream) (reverse args)) 
	(subseq arg-vars 0 arity))))

(defun get-explicit-type (str i)
  (let* ((j (1+ i))
	 (type-str
	  (and (< (1+ j) (length str))
	       (eq (aref str j) #\:)
	       (not (eq (aref str (1+ j)) #\Space))
	       (subseq str (1+ j) (position #\Space str :start j)))))
    type-str))
;;;
;;; --- psql-lex-entry methods and funtions
;;;

;;; set version to next val
(defmethod set-version ((psql-le psql-lex-entry) (lexicon psql-lex-database))
  (setf (version psql-le) (next-version (name psql-le) lexicon)))

(defun split-on-char (string &optional char)
  (unless char (setf char #\Space))
  (loop for i = 0 then (1+ j)
      as j = (position char string :start i)
      collect (subseq string i j)
      while j))

;;;
;;; temp
;;;

(defun time-parse (str)
  ;(setf *psql-verbose-query* t)
  (time
   (parse
    (split-into-words 
     (preprocess-sentence-string str)))))

;;;
;;; db filter
;;;

(defmethod set-filter ((lexicon psql-lex-database))  
  (let ((filter
         (ask-user-for-x "Alter filter" 
                         (cons "New filter?" (get-filter lexicon)))))
    (when filter
      (if (catch 'pg:sql-error 
            (fn-get-records lexicon ''set-current-view filter)
            )
	  (set-filter lexicon))
      )))

(defun set-filter-psql-lexicon nil
  (set-filter *psql-lexicon*)
  (initialize-psql-lexicon))

(defun sql-embedded-text (str)
  (cond
   ((equal str "")
    "")
   ((eq (char str 0) #\')
    (format nil "''~a" (sql-embedded-text (subseq str 1))))
   (t
    (format nil "~a~a" (char str 0) (sql-embedded-text (subseq str 1))))))

;;;
;;; set (uninitialized) lexicon
;;;
(setf *lexicon* (make-instance 'psql-lex-database))

