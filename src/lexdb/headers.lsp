;;; Copyright (c) 2001 -- 2005
;;;   Ben Waldron, John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

(defvar *lexdb* nil)
;;(def-lkb-parameter *lexdb-params* nil :user) ;; see main/globals.lsp

#+:allegro
(defvar *lexdb-locale* (excl::find-locale "C.utf8"))

(defvar *psql-database-connect-timeout* 30)
(defvar *psql-database-port* 5432)

(defvar *lexdb-major-version* "4.8")

(defvar *lexdb-dump-skip-stream* t)
(defvar *lexdb-dump-source* "?")
(defvar *lexdb-dump-timestamp* nil)
(defvar *lexdb-dump-user* nil)
(defvar *lexdb-dump-lang* nil)
(defvar *lexdb-dump-country* nil)

(defvar *lexdb-dump-tdl* nil) ;; set this to t to force tdl dump to accompany lexdb dump
(defvar *lexdb-message-old-server* "(LexDB) PostgreSQL server version is ~a. Please upgrade to version ~a or above.")
(defvar *lexdb-message-old-lkb* "(LexDB) Your LexDB version (~a) is incompatible with this LKB version (requires v. ~ax). Try obtaining a more recent LKB binary.")
(defvar *lexdb-message-old-lexdb* "(LexDB) Your LexDB version (~a) is incompatible with this LKB version (requires v. ~ax). You must load updated setup files. See http://www.cl.cam.ac.uk/~~bmw20/DT/initialize-db.html")

;; map from obsolete names of field-map types
(defvar *lexdb-fmtype-alt*
    '((string . str)
      (symbol . sym)
      (string-list . str-rawlst)
      (string-fs . str-lst)
      (string-diff-fs . str-dlst)
      (mixed-fs . lst)
      (mixed-diff-fs . dlst)))
(defvar *lexdb-tmp-lexicon*)

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
   (fields-tb 
    :initform nil :accessor fields-tb :initarg :fields-tb)
   ;; a-list for mapping the lexicon-table fields to the psort-or-lex structure
   (dfn :initform nil :accessor dfn)
   (fields :initform nil :accessor fields)))

(defclass psql-database ()
  ((dbname :initform nil :accessor dbname :initarg :dbname)
   (host :initform "localhost" :accessor host :initarg :host)
   (user :initform (user-name) :accessor user :initarg :user)
   (password :initform "" :accessor password :initarg :password)
   (port :initform (or (getenv "PGPORT") *psql-database-port*) :accessor port :initarg :port)
   (connection :initform nil :accessor connection :initarg connection)))

(defclass psql-lex-database (psql-database external-lex-database)
  ((lexdb-version :initform nil :accessor lexdb-version)
   (pub-fns :initform nil :accessor pub-fns)
   (semi :initform nil :accessor semi)
   (quick-load :initform nil :accessor quick-load)
   ))

(defclass psql-database-table ()
  ((recs :initarg :recs :accessor recs)
   (cols :initarg :cols :accessor cols)))

;; this should be replaced with a-list records
(defclass psql-lex-entry ()
  ((fv-pairs :initarg :fv-pairs)))

(defmethod print-object ((inst psql-lex-entry) stream)
  (format stream "#[psql-lex-entry ~S]" 
	  (slot-value inst 'fv-pairs)))

;;; temporary
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
  )

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