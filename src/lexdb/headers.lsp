(in-package :lkb)

(defvar *psql-lexicon-parameters*) ;; define in GRAMMAR/globals.lsp

(defvar *postgres-temp-filename* nil)

(defvar *psql-lexdb-compat-version* "3.1")
(defvar *psql-fns-version* "1.00")
(defvar *psql-port-default* 5432)

(defvar *postgres-tmp-lexicon* nil)
(defvar *psql-lexicon* nil)

(defvar *postgres-debug-stream* t)

(defvar *postgres-mwe-enable* nil)

(defvar *postgres-export-output-lexicon* nil)
(defvar *postgres-export-skip-stream* t)
(defvar *postgres-export-separator* #\,)

(defvar *postgres-export-version* 0)
(defvar *postgres-export-timestamp* nil) ;;; see lexport.lsp
(defvar *postgres-current-source* "?")
(defvar *postgres-current-user* nil)
(defvar *postgres-current-lang* nil)
(defvar *postgres-current-country* nil)

(defvar *postgres-export-multi-separately* nil)
(defvar *postgres-export-multi-stream* t)

(defvar *postgres-debug-stream*)

;;;
;;; class declarations
;;;

(defclass sql-database ()
  ((dbname :initform nil :accessor dbname	:initarg :dbname)
   (host :initform "localhost" :accessor host :initarg :host)
   (user :initform (sys:user-name) :accessor user :initarg :user)
   (password :initform "" :accessor password :initarg :password)
   (port :initform (or (system:getenv "PGPORT") (num-2-str *psql-port-default*)) :accessor port :initarg :port)))

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
   (lexdb-version :initform nil :accessor lexdb-version)
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

