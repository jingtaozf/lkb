;;; Copyright (c) 2001 -- 2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Ben Waldron;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

(defvar *psql-lexicon-parameters*) ;; define in GRAMMAR/globals.lsp

(defvar *postgres-temp-filename* nil)

(defvar *psql-lexdb-compat-version* "3.3")
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

;; set this to nil to prevent tdl dump accompanying lexdb dump
(defvar *lexdb-dump-tdl* t)

(defvar *lexdb-message-old-server* "PostgreSQL server version is ~a. Please upgrade to version ~a or above.")
(defvar *lexdb-message-old-lkb* "Your LexDB version (~a) is incompatible with this LKB version (requires v. ~ax). Try obtaining a more recent LKB binary.")
(defvar *lexdb-message-old-lexdb* "Your LexDB version (~a) is incompatible with this LKB version (requires v. ~ax). You must load updated setup files. See http://www.cl.cam.ac.uk/~~bmw20/DT/initialize-db.html")

;; map from obsolete names of field-map types
(defvar *field-map-type-mneum*
    '((string . str)
      (symbol . sym)
      (string-list . str-rawlst)
      (string-fs . str-lst)
      (string-diff-fs . str-dlst)
      (mixed-fs . lst)
      (mixed-diff-fs . dlst)))

;;;
;;; class declarations
;;;

(defclass sql-database ()
  ((dbname :initform nil :accessor dbname :initarg :dbname)
   (host :initform "localhost" :accessor host :initarg :host)
   (user :initform (sys:user-name) :accessor user :initarg :user)
   (password :initform "" :accessor password :initarg :password)
   (port :initform (or (system:getenv "PGPORT") (num-2-str *psql-port-default*)) :accessor port :initarg :port)))

(defclass external-lex-database (lex-database)
  ((record-cache :initform (make-hash-table :test #'eq))
   (fields-tb 
    :initform nil :accessor fields-tb :initarg :fields-tb)
   ;; a-list for mapping the lexicon-table fields to the psort-or-lex structure
   (fields-map :initform nil :accessor fields-map)
   (fields :initform nil :accessor fields)))

(defclass psql-database (sql-database)
  ((connection :initform nil :accessor connection :initarg connection)))

(defclass psql-lex-database (psql-database external-lex-database)
  ((lexdb-version :initform nil :accessor lexdb-version)
   (ext-fns :initform nil :accessor ext-fns)
   (semi :initform nil :accessor semi)))

(defclass sql-query ()
  ((sql-string :accessor sql-string :initarg :sql-string :initform nil)
   (records :initform :unknown :accessor records)
   (columns :initform :unknown :accessor columns)))

;; this should be replaced with a-list records
(defclass psql-lex-entry ()
  ((fv-pairs :initarg :fv-pairs)))

;;; temporary
#-:psql
(progn
  (defun MRS::DUMP-SEMI-TO-PSQL nil)
  (defun LKB::CACHE-ALL-LEX-RECORDS-ORTH nil)
  (defun LKB::CACHE-ALL-LEX-RECORDS nil)
  (defun LKB::RELOAD-ROOTS-MWE nil)
  (defun LKB::SET-LEX-ENTRY nil)
  (defun LKB::MAKE-INSTANCE-PSQL-LEX-ENTRY nil)
  (defun LKB::TO-DB-DUMP nil
    (error "Please compile with :psql")))

(defun psql-initialize ()
  ;;
  ;; make sure we `mark' the current universe as PSQL-enabled.
  ;;
  (pushnew :psql *features*)
  (handler-case (load "libpq.so") 
    (file-error () 
      ;; some feedback to user
      (format t ";   Warning: cannot load libpq.so")
      (format t "~%;            (PSQL lexicon functionality will be unavailable)")
      (format t "~%;            (hint: are the PostgreSQL libraries installed on your machine?)")
      ;; need this for backward compatibility with ERG script
      ;; (also a good idea anyway)
      (setf *features* (remove :psql *features*)))))
