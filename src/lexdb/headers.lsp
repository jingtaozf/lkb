;;; Copyright (c) 2001 -- 2005
;;;   Ben Waldron, John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

;;

(defvar *lexdb* nil)
(defvar *lexdb-major-version* "4.9")
;;(def-lkb-parameter *lexdb-params* nil :user) ;; see main/globals.lsp

(defvar *lexdb-message-old-server* "(LexDB) PostgreSQL server version is ~a. Please upgrade to version ~a or above.")
(defvar *lexdb-message-old-lkb* "(LexDB) Your LexDB version (~a) is incompatible with this LKB version (requires v. ~ax). Try obtaining a more recent LKB binary.")
(defvar *lexdb-message-old-lexdb* "(LexDB) Your LexDB version (~a) is incompatible with this LKB version (requires v. ~ax). You must load updated setup files. See http://www.cl.cam.ac.uk/~~bmw20/DT/initialize-db.html")

;; connection parameters

#+:allegro
(defvar *lexdb-locale* (excl::find-locale "C.utf8"))
(defvar *psql-database-connect-timeout* 30)
(defvar *psql-database-port* 5432) ;; default

;; dump parameters

(defvar *lexdb-dump-skip-stream* t)
(defvar *lexdb-dump-source* "?")
(defvar *lexdb-dump-timestamp* nil)
(defvar *lexdb-dump-user* nil)
(defvar *lexdb-dump-lang* nil)
(defvar *lexdb-dump-country* nil);; ->globals, def-lkb-param
(defvar *lexdb-dump-tdl* nil) ;; set this to t to force tdl dump to accompany lexdb dump

#+:null
(defvar *lexdb-tmp-lexicon*)

;; MWE (long broken)

#+:mwe
(defvar *postgres-mwe-enable* nil)
#+:mwe
(defvar *postgres-export-multi-separately* nil)
#+:mwe
(defvar *postgres-export-multi-stream* t)

;;;
;;; class declarations
;;;

(defclass external-lex-database (lex-database)
  ((record-cache :initform (make-hash-table :test #'eq))
   (fields-tb :initform nil :accessor fields-tb :initarg :fields-tb)
   ;; a-list for mapping the lexicon-table fields to the psort-or-lex structure
   (dfn :initform nil :accessor dfn)
   (fields :initform nil :accessor fields)))

(defclass psql-database ()
  ((dbname :initform nil :accessor dbname :initarg :dbname)
   (host :initform "localhost" :accessor host :initarg :host)
   (user :initform (user-name) :accessor user :initarg :user)
   (password :initform "" :accessor password :initarg :password)
   (port :initform (or (getenv "PGPORT") *psql-database-port*) :accessor port :initarg :port)
   (connection :initform nil :accessor connection :initarg connection)
   (quote-ident-cache :initform nil :accessor quote-ident-cache)
   ))

(defclass psql-lex-database (psql-database external-lex-database)
  ((lexdb-version :initform nil :accessor lexdb-version)
   (semi :initform nil :accessor semi)
   ))

;; multi-user database
(defclass mu-psql-lex-database (psql-lex-database external-lex-database)
  ((quick-load :initform nil :accessor quick-load)))

;; single-user database
(defclass su-psql-lex-database (psql-lex-database external-lex-database)
  ())

(defclass psql-database-table ()
  ((recs :initarg :recs :accessor recs)
   (cols :initarg :cols :accessor cols)))

;; this should be replaced with a-list records
(defclass psql-lex-entry ()
  ((fv-pairs :initarg :fv-pairs)))

(defmethod print-object ((inst psql-lex-entry) stream)
  (format stream "#[psql-lex-entry ~S]" 
	  (slot-value inst 'fv-pairs)))

;;; dummy definitions to avoid warnings if :psql not compiled in
#-:psql
(progn
  (defun MRS::DUMP-SEMI-TO-PSQL (&rest foo) (declare (ignore foo)))
  (defun LKB::CACHE-ALL-LEX-RECORDS-ORTH (&rest foo) (declare (ignore foo)))
  (defun LKB::CACHE-ALL-LEX-RECORDS (&rest foo) (declare (ignore foo)))
  (defun LKB::RELOAD-ROOTS-MWE (&rest foo) (declare (ignore foo)))
  (defun LKB::SET-LEX-ENTRY (&rest foo) (declare (ignore foo)))
  (defun LKB::MAKE-INSTANCE-PSQL-LEX-ENTRY (&rest foo) (declare (ignore foo)))
  (defun LKB::TO-DB-DUMP (&rest foo) (declare (ignore foo))
    (error "Please compile with :psql"))
  (defun LKB::LOAD-LEXDB-FROM-SCRIPT (&rest foo) (declare (ignore foo))
	 (error "Please set *lexdb-params* to NIL"))
  (defun MRS::DUMP-GENERATOR-INDICES-TO-PSQL (&rest foo) (declare (ignore foo)))
  (defun LKB::INDEX-NEW-LEX-ENTRIES (&rest foo) (declare (ignore foo)))
  (defun MRS::LOAD-GENERATOR-INDICES-FROM-PSQL (&rest foo) (declare (ignore foo)))
  (defun LKB::SEMI-OUT-OF-DATE (&rest foo) (declare (ignore foo)))
  (defun LKB::SQL-GET-NUM (&rest foo) (declare (ignore foo)))
  (defun LKB::TO-DB-DUMP-REV (&rest foo) (declare (ignore foo)))
  (defun LKB::LIBPQ-P (&rest foo) (declare (ignore foo)))
  (defun LKB::ESCAPE-SQL-COPY-STRING (&rest foo) (declare (ignore foo))))

;; initialize interface to postgres library functions
;; fix_me: move to psqllex.lsp ???

(defun psql-initialize ()
  (unless (libpq-p)
    #+:linux
    (let (#+allegro 
	  (excl::*load-foreign-types* 
	   (append '("3" "4") excl::*load-foreign-types*))
	  )
      (load-libpq '("libpq.so" "libpq.so.4" "libpq.so.3")))
    #+:mswindows
    (load-libpq '("libpq.dll"))
    #-(or :linux :mswindows)
    (load-libpq nil)))

(defun load-libpq (lib-names)
  (cond
   (lib-names
    (handler-case (load (car lib-names))
      (file-error ()
	(format t "; ... [file not found]")
	(load-libpq (cdr lib-names)))))
   (t
    (format t ";   Warning: (LexDB) cannot load PostgreSQL client library")
    (format t "~%;            (Is the PostgreSQL library file installed on your machine? If so, please load it manually.)"))))

;; see if calling pq:connectdb returns an error
#+:psql
(defun libpq-p nil
  (handler-case (and (pq:connectdb "") t) (t () nil)))

;; BACKWARDS COMPAT
;; mapping from obsolete names of field-map types
(defvar *lexdb-fmtype-alt*
    '((string . str)
      (symbol . sym)
      (string-list . str-rawlst)
      (string-fs . str-lst)
      (string-diff-fs . str-dlst)
      (mixed-fs . lst)
      (mixed-diff-fs . dlst)))

;;;
;;;
;;;

;; TDL TEXT

;; map unifs to tdl (fragment) text
(defmethod unifs-to-tdl-body (unifs)
  ;; return empy string if no unifs
  (if (null unifs) 
      (return-from unifs-to-tdl-body ""))
  (if (member nil unifs 
	      :key (lambda (x) (path-typed-feature-list (unification-lhs x))))
      ;; TDL = type & ...
      (p-2-tdl (pack-unifs unifs))
    ;; TDL = ...
    (p-2-tdl-aux 0 (pack-unifs unifs))))
	  
;; map tdl (fragment) text to unifs
(defun tdl-to-unifs (tdl-fragment)
  ;; fix_me: unless the string does contain a non-empty tdl fragment we will throw an error later
  (unless (and (stringp tdl-fragment)
	       (> (length tdl-fragment) 0))
    (return-from tdl-to-unifs))
  ;; assume fragment non-empty...
  (let ((*readtable* (make-tdl-break-table)))
    (read-tdl-lex-avm-def (make-string-input-stream 
			   (concatenate 'string tdl-fragment "."))
			  nil)))

;;; RECORD TO PSORT STRUCT

(defun make-psort-struct2 (raw-record cols &key dfn)
  (apply #'make-lex-entry 
	 (make-strucargs2 raw-record cols 
			  :dfn dfn
			  )))

;; provide args to make-lex-entry
(defun make-strucargs2 (raw-record cols &key dfn fields)
  ;; make a-list with empty values
  (let* ((strucargs 
	 (mapcar #'(lambda (x) (list x)) 
		 (remove-duplicates (mapcar #'first dfn)))))
    ;; instantiate values in a messy way
    ;; fix_me
    (loop 
	for (slot-key slot-field slot-path slot-type) in dfn
	for slot-value-list = (work-out-value slot-type 
					      (get-val slot-field raw-record cols)
					      :path (work-out-rawlst slot-path))
	when slot-value-list
	do 
	  (setf (cdr (assoc slot-key strucargs))
	    (append (cdr (assoc slot-key strucargs))
		    (mapcar #'(lambda (x) (make-strucargs-aux x slot-path)) 
			    slot-value-list))))
    ;; messy
    (let ((unifs (cdr (assoc :UNIFS strucargs)))
	  (id (cadr (assoc :ID strucargs)))
	  (orth (cadr (assoc :ORTH strucargs))))
      ;; if using :|_tdl| field (undecomposed TDL) the raw tdl contributes to lex entry
      (when (member :|_tdl| fields)
	(setf unifs 
	  (append unifs (tdl-to-unifs (get-val :|_tdl| raw-record cols)))))
      ;; finally, build the list of arguments
      (list :UNIFS unifs
	    :ID id
	    :ORTH orth
	    :INFL-POS (and (> (length orth) 1)
			   (find-infl-pos nil orth nil))))))

;; read-psort ->
;;; create slot entry
(defun make-strucargs-aux (slot-value slot-path)
  (cond
   ;;: nil path => no unification
   ((equal slot-path "")
    slot-value)
   ;;: atomic value => simple case
   ((atom slot-value)
    (make-unification
	   :lhs (make-path 
		 :typed-feature-list 
		 (work-out-rawlst slot-path))
	   :rhs (make-u-value :type slot-value)))
   ;;: list. eg. (rest first "word") => (... rest first) has val "word"  
   ((listp slot-value)
    (make-unification
	   :lhs (make-path 
		 :typed-feature-list 
		 (append
		  (work-out-rawlst slot-path)
		  (reverse (cdr (reverse slot-value)))))
	   :rhs (make-rhs-val (car (last slot-value)))))
   (T (error "unhandled input"))))
  
(defun make-rhs-val (x)
  (cond
   ((listp x)
    (make-path :typed-feature-list x))
   (t
    (make-u-value :type x))))


;;; SOURCE NAME

(defun extract-pure-source-from-source (source)
  (let* ((end (position #\( source :test #'equal))
	 (pure-source (and end (< 1 end)
			   (subseq source 0 end))))
    (if pure-source
	(string-trim '(#\Space) pure-source)
      source)))

;; DAG DEBUGGING

(defun pprint-dag (&rest rest)
  (format t "~&~%~a"
	  (apply #'pprint-dag-aux rest))
  rest)

(defun pprint-dag-aux (x &key (depth 0) root (max-depth 100))
  (when (and max-depth (> depth max-depth))
    (return-from pprint-dag-aux "..."))
;  (setf x (mrs::path-value x root))
  (setf x (unify-paths-dag-at-end-of1 x root))
  (setf x (deref-dag x))
  (cond
   ((dag-arcs x) 
    (format nil "[~(~s%~s~) ~a]"
	    (dag-type x) (dag-new-type x)
	    (concatenate-strings
	     (loop
		 for (node . val) in (dag-arcs x)
		 collect (format nil "~%~a~a ~a" 
				 (make-string depth :initial-element #\space)
				 node (pprint-dag-aux val :depth (+ 2 depth)))))))
;   ((stringp (dag-type x))
;    (format nil "\"~a\"" (dag-type x)))
   (t
    (format nil "~s%~s" (dag-type x) (dag-new-type x)))
   ))
