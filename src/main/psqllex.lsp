;;; Copyright (c) 2002 
;;;   Ann Copestake, Fabre Lambeau, Stephan Oepen, Ben Waldron;
;;;   see `licence.txt' for conditions.

;;; modifications by bmw (jun-03)
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
;;;  - look into Postgres optimization (factor of four or more);
;;;  - find out why we spend an extra 45 seconds or more over the CDB version
;;;    when looking up some 9000 stems;
;;;  - decide on whether to keep using the `definition' table or move that
;;;    information into the globals;
;;;  - add export to TDL file support; probably use `definition' table;
;;;  - generalize import code, maybe use `definition' table;
;;;  - add insertion of additional lexical entries support from LKB; maybe use
;;;    emacs(1) forms for input (and the emacs(1) -- lisp interface, such that
;;;    no extra installation overhead is incurred, e.g. for a web server to
;;;    talk to PostGres);
;;;  - work out interface for PET (not really LKB-related);
;;;  - rework connection handling to re-open on demand (rather than error());
;;;  - support for generator indexing;
;;;  - integrate irregular spellings into lexical DB;
;;;  - versioning of databases, delivery, et al.
;;;

(in-package :lkb)

(defvar *psql-lexicon* nil)
(defvar *psql-verbose-query* nil) ;;; flag
(defvar *export-counter*) ;;; see lexport.lsp

(defvar *current-source* nil)
(defvar *current-user* nil)
(defvar *current-lang* nil)
(defvar *current-country* nil)

(defun open-psql-lex nil
  (read-cached-lex-if-available nil)
  (initialize-psql-lexicon))

					;: set up connection
					;: make-field-map-slot
					;: replace existing *psql-lexicon*
					;: clear lexical entries of *lexicon*
					;: link to lexicon
(defun initialize-psql-lexicon (&key (db "lingo")
                                     (host "localhost")
                                     (user "guest")
                                     (password "guest")
                                     (table "erg")
                                     (definition (format nil "~ad" table)))
  
  (let* ((lexicon (make-instance 'psql-lex-database 
                    :dbname db :host host
                    :user user :password password
                    :lex-tb table :fields-tb definition))
	(connection (connect lexicon)))
    (cond
     ((eq connection :connection-ok)
      (unless (string>= (server-version lexicon) "7.3")
	(format *trace-output* 
		"~%WARNING: PostgreSQL server version is ~a. Please upgrade to version 7.3 or above." (server-version lexicon)))
      (make-field-map-slot lexicon)
      (when *psql-lexicon* (clear-lex *psql-lexicon*))
      (setf *psql-lexicon* lexicon)
      (cond
       ((null *lexicon*)
	(setf *lexicon* *psql-lexicon*))
       (t
	(clrhash (slot-value *lexicon* 'lexical-entries))
	(link *psql-lexicon* *lexicon*))))
     (t
      (error 
       "unable to connect to ~s: ~a"
       (pg:db connection) (pg:error-message connection))))
    lexicon))

;;;
;;; obsolete (use clear-lex instead)
;;;

;(defun close-psql-lexicon (&optional (lexicon *psql-lexicon*))
;  (when (eq (type-of lexicon) 'psql-lex-database)
;    ;;
;    ;; shut down connection to PostGreSQL server
;    ;;
;    (let ((connection (connection lexicon)))
;      (when connection 
;        (pg:finish connection)
;        (setf (connection lexicon) nil)))
;    ;; bmw:
;    ;; unlink from list of secondary lexica
;    ;;
;    (unlink lexicon *lexicon*)
;    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface to a Postgres database
;;;

(defclass sql-database ()
  ((dbname :initform nil :accessor dbname	:initarg :dbname)
   (host :initform "localhost" :accessor host :initarg :host)
   (user :initform (sys:user-name) :accessor user :initarg :user)
   (password :initform "" :accessor password :initarg :password)))

(defclass external-lex-database (lex-database)
  (
   ;; flat-table containing the lexical database
   (lexicon-table :initform nil :accessor lex-tb :initarg :lex-tb)
   ;; table for mapping the lexicon-table fields to the psort-or-lex structure
   (slot-to-fields-mapping-table 
    :initform nil :accessor fields-tb :initarg :fields-tb)
   ;; a-list for mapping the lexicon-table fields to the psort-or-lex structure
   (slot-to-fields-mapping :initform nil :accessor fields-map)))

(defclass psql-database (sql-database)
  ((connection :initform nil :accessor connection :initarg connection)
   (server-version :initform nil :accessor server-version)))

(defclass psql-lex-database (psql-database external-lex-database)
  ())

(defclass sql-query ()
  ((sql-string :accessor sql-string :initarg :sql-string)
   (records :accessor records)
   (columns :accessor columns)
   (count-records :accessor count-recs)
   (mapped-recs :accessor mapped-recs)))

(defclass psql-lex-entry ()
  ((id :initform nil :accessor id)
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
   ;complete, semclasses, preferences, classifier, selectrest, jlink
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
   (moddate :initform 'CURRENT_DATE :accessor moddate :initarg :moddate)
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
  (setf (password lexicon) nil))

;;;
;;; --- psql-database methods
;;;

(defmethod connect ((lexicon psql-database))
  (with-slots (host dbname user password connection) lexicon
    (let ((properties (format 
		       nil 
		       "host='~a' dbname='~a' user='~a' password='~a'"
		       (sql-escape-string host)
		       (sql-escape-string dbname)
		       (sql-escape-string user)
		       (sql-escape-string password)))
	  (decoded-status nil))
      (setf (connection lexicon) (pg:connect-db properties))
      (setf decoded-status (pg:decode-connection-status (pg:status connection)))
      (if (eq decoded-status :connection-ok)
	  (setf (server-version lexicon) (get-server-version lexicon)))
      decoded-status)))

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

    (time
    
    (multiple-value-bind (recs cols recs-count)
        (pg:sql (sql-string query) :db connection)
      (setf (records query) recs 
            (columns query) cols
            (count-recs query) recs-count))
    
    )
    
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

(defun symbol-to-str-format (expr)
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
  (if (null symb-list) (error (format nil "non-null list expected")))
  (let ((stream (make-string-output-stream)))
    (format stream "(")
    (format stream "~a" (symbol-to-str-format (pop symb-list)))
    (loop 
	while symb-list
	do 
	  (format stream ",~a" (symbol-to-str-format (pop symb-list))))
    (format stream ")")
    (get-output-stream-string stream)))

;;; prepare val list for SQL INSERT INTO query
(defun sql-val-list-str (symb-list psql-le)
  (if (null symb-list) (error (format nil "non-null list expected")))
  (let ((stream (make-string-output-stream)))
    (format stream "(")
    (format stream "~a" (make-sql-val-str 
			 (slot-value psql-le (pop symb-list))))
    (loop 
	while symb-list
	do 
	  (format stream ",~a" (make-sql-val-str 
				(slot-value psql-le (pop symb-list)))))
    (format stream ")")
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
  (unless separator (setf separator " "))
  (let ((stream (make-string-output-stream)))
    (format stream "~a" (pop str-list))
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

(defun symb-2-str (symb)
  (if symb
      (let ((stream (make-string-output-stream)))
	(format stream "~a" symb)
	(string-downcase (get-output-stream-string stream)))
    nil))

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
  (setf (lex-tb lexicon) nil)
  (setf (fields-map lexicon) nil)
  (setf (fields-tb lexicon) nil))

;;;
;; --- psql-lex-database methods
;;;

(defmethod lookup-word-OLD ((lexicon psql-lex-database) orth &key (cache t))
  (declare (ignore cache))
  (if (connection lexicon)
      (let* (
	     (orthstr (string-downcase (sql-escape-string orth)))
	     (sql-str (format 
		       nil 
		       "SELECT lookup_word('~a');"
		       orthstr))
	     (query-res (run-query 
			 lexicon 
			 (make-instance 'sql-query :sql-string sql-str))))
	(mapcar #'str-to-symbol-format (mapcar #'car (records query-res))))))

(defmethod lookup-word ((lexicon psql-lex-database) orth &key (cache t))
  (declare (ignore cache))
  (if (connection lexicon)
      (let* (
	     (orthstr (string-downcase (sql-escape-string orth)))
	     (sql-str 
	      (cond
	       ((string< (server-version lexicon) "7.3")
		(format nil "SELECT ~a FROM erg_max_version WHERE name IN (SELECT lookup_word('~a'));" 
			(make-requested-fields lexicon)
			orthstr))
	       (t (format 
		   nil 
		   "SELECT ~a FROM retrieve_entries('~a');"
		   (make-requested-fields lexicon)
		   orthstr))))
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
;;; for compliance with other lexical sources, lex-words() is expected to
;;; return "ad" and "hoc" separately, rather than just "ad hoc"; (done)
;;; fix_me: inefficient implementation
(defmethod lex-words ((lexicon psql-lex-database))
  (let* ((sql-str (format 
                   nil 
                   "SELECT orthography_set();" 
		   ))
         (query-res (run-query 
                     lexicon 
                     (make-instance 'sql-query :sql-string sql-str))))
    ; (remove-duplicates
      (mapcan #'(lambda (x) (split-into-words (string-upcase (car x))))
	      (records query-res))
     ; :test #'equal)
    ))

(defmethod collect-psort-ids ((lexicon psql-lex-database))
  (let* ((sql-str (format 
                   nil 
                   "SELECT psort_id_set();"))
          (query-res (run-query 
                     lexicon 
                     (make-instance 'sql-query :sql-string sql-str))))
    (mapcar #'(lambda (x) (str-to-symbol-format (car x)))
            (records query-res))))

;;; todo: DB fn instead
(defmethod retrieve-record ((lexicon psql-lex-database) id)
  (let* ((sql-str (format nil "SELECT ~a FROM ~a WHERE ~a='~a';" 
			  (make-requested-fields lexicon)
			  "erg_max_version"
			  (second (assoc :id (fields-map lexicon)))
			  (symbol-to-str-format id)))
	 (query-res (run-query 
		     lexicon 
		     (make-instance 'sql-query :sql-string sql-str)))
	 (records (when (records query-res) (make-column-map-record query-res))))
    (if (> (length records) 1)
	(error (format nil "database error (too many records returned)"))
      (car records)))) 

(defmethod read-psort ((lexicon psql-lex-database) id &key (cache t))
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
  ;; stores the mapping of fields to lex-or-psort structure slots
  (setf (fields-map lexicon)
    (mapcar #'(lambda (x) 
                (cons (str-to-keyword-format (car x)) (cdr x)))
            (records (run-query lexicon 
                                (make-instance 'sql-query
                                  :sql-string (format 
                                               nil 
                                               "SELECT * FROM ~a;"
                                               (fields-tb lexicon))))))))

(defmethod make-psort-struct ((lexicon psql-lex-database) query-res)
  (let* ((strucslots 
          (loop 
              for (slot-key slot-field slot-path slot-type) 
              in (fields-map lexicon)
              for slot-value-list = 
		(work-out-value 
                                slot-key slot-type 
                                (cdr (assoc slot-field query-res 
                                               :test #'equal)))		
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
    (apply #'make-lex-or-psort strucargs)))

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
			   (work-out-value 
			    slot-key "list" slot-path))
	   :rhs (make-u-value :type slot-value))))
   ;: list. eg. (rest first "word") => (... rest first) has val "word"  
   ((listp slot-value)
    (list slot-key
	  (make-unification
	   :lhs (make-path 
		 :typed-feature-list (append
				      (work-out-value slot-key "list" slot-path)
				      (reverse (cdr (reverse slot-value)))))
	   :rhs (make-u-value :type (car (last slot-value))))))
  (T (error "unhandled input"))))

;;; redo later...
;;; insert lex entry into db
(defmethod set-lex-entry ((lexicon psql-lex-database) (psql-le psql-lex-entry))
  (set-id psql-le lexicon)
  (set-version psql-le lexicon)  
  (let* ((symb-list 
	  (mapcan 
	   #'(lambda (x) (unless (null (slot-value psql-le x)) (list x))) 
	   '(id name type orthography orthkey keyrel altkey alt2key keytag compkey ocompkey comments exemplars lang country dialect domains genres register confidence userid moddate version source flags))))
    (run-query lexicon 
	       (make-instance 'sql-query
		 :sql-string (format 
			      nil 
			      "INSERT INTO ~a ~a VALUES ~a;"
			      (lex-tb lexicon)
			      (sql-field-list-str symb-list)
			      (sql-val-list-str symb-list psql-le))))))

(defun set-lexical-entry-psql-lex-database-check (orth-string lex-id new-entry) 
  (declare (ignore lex-id))
  (declare (ignore new-entry))
  (if orth-string (format *trace-output* "WARNING: ignoring explicit orth-string")))

(defmethod set-lexical-entry ((lexicon psql-lex-database) orth-string lex-id new-entry)  
  (set-lexical-entry-psql-lex-database-check orth-string lex-id new-entry)
  (let* ((name lex-id)
	 (constraint new-entry)
       (temp (extract-stem-from-unifications constraint))
       (stem (cdr temp))
       (count (car temp))
           (keyrel (extract-key-from-unifications constraint))      
           (keytag (extract-tag-from-unifications constraint))
           (altkey (extract-altkey-from-unifications constraint))
           (alt2key (extract-alt2key-from-unifications constraint))
           (compkey (extract-comp-from-unifications constraint))
           (ocompkey (extract-ocomp-from-unifications constraint))
       (total (+ count 1 
		 (if keyrel 1 0) (if keytag 1 0) (if altkey 1 0)
		 (if alt2key 1 0) (if compkey 1 0) (if ocompkey 1 0)))
       (psql-le
	(make-instance 'psql-lex-entry
	 :name (symb-2-str name)
	 :type (symb-2-str (extract-type-from-unifications constraint))
	 :orthography stem		;list
	 :orthkey (first stem)
	 :keyrel (symb-2-str keyrel)
	 :altkey (symb-2-str altkey)
	 :alt2key (symb-2-str alt2key)
	 :keytag (symb-2-str keytag)
	 :compkey (symb-2-str compkey)
	 :ocompkey (symb-2-str ocompkey)
	 :userid *current-user*
	 :country *current-country*
	 :lang *current-lang*
	 :source *current-source*
	:flags 1)))
  (cond
   ((= total (length constraint))
    (set-lex-entry lexicon psql-le))
   (t
    (format *trace-output* "~%skipping super-rich entry: `~a'~%"  name)
  nil))))

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
(defun work-out-value (key type value)
  (declare (ignore key))
  (cond ((equal type "symbol") 
	 (unless (equal value "")
	   (list (str-to-symbol-format value))))
	((equal type "string")
	 (unless (equal value "")
	   (list value)))
	((equal type "string-list")
	 (list (orth-string-to-str-list value))
	 )
	((equal type "string-fs")
	 (expand-string-list-to-fs-list (orth-string-to-str-list value))
	 )
	((equal type "list") (read-from-string value))
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

;;; returns version, eg. "7.3.2"
(defmethod get-server-version ((lexicon psql-lex-database))
  (let* 
      ((sql-str "SELECT version();")
       (version-str (caar (records (run-query lexicon (make-instance 'sql-query :sql-string sql-str))))))
    (second (split-on-char version-str))))
    
;;; calls DB fn
(defmethod next-id ((lexicon psql-lex-database))
  (let* ((sql-str (format nil "SELECT next_id();"))
	 (res (caar (records (run-query 
                     *psql-lexicon* 
                     (make-instance 'sql-query :sql-string sql-str))))))
    (str-2-num res)))

;;; calls DB fn
(defmethod next-version (id (lexicon psql-lex-database))
  (let* ((sql-str (format nil 
			  "SELECT next_version('~a');"
			  (string-downcase id)))
	 (res (caar (records (run-query 
                     *psql-lexicon* 
                     (make-instance 'sql-query :sql-string sql-str))))))
    (str-2-num res)))

;;;
;;; --- psql-lex-entry methods and funtions
;;;

;;; set id to next available val
(defmethod set-id ((psql-le psql-lex-entry) (lexicon psql-lex-database))
  (setf (id psql-le) 
    (if *export-counter* 
	*export-counter*
      (next-id))))

;;; set version to next available val
(defmethod set-version ((psql-le psql-lex-entry) (lexicon psql-lex-database))
  (setf (version psql-le) (next-version (name psql-le) lexicon)))

(defun split-on-char (string &optional char)
  (unless char (setf char #\Space))
  (loop for i = 0 then (1+ j)
      as j = (position char string :start i)
      collect (subseq string i j)
      while j))
