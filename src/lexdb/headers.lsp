;;; Copyright (c) 2001 -- 2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Ben Waldron;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

(defvar *psql-lexicon-parameters*) ;; define in GRAMMAR/globals.lsp

(defvar *postgres-temp-filename* nil)

(defvar *psql-lkb-version* "2.00")
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

(defvar *postgres-sql-fns*)
(setf *postgres-sql-fns*
  '(
    :clear_scratch
    :commit_scratch
    :complete
    :db_owner
    :dump_db
    :dump_scratch_db
    :filter
    :initialize_current_grammar
    :initialize_user_schema
    :lex_id_set 
    :lexdb_version
    :list_fields
    :lookup_general
    :lookup_general_null
    :lookup_word
    :merge_into_db2
    :merge_defn
    :mneum_f_map
    :next_version 
    :orthography_set
    :retrieve_all_entries
    :retrieve_current_timestamp
    :retrieve_entries_by_orthkey
    :retrieve_entry
    :retrieve_head_entry
    :revision_new
    :retrieve_private_revisions
    :retrieve_semi_extra
    :retrieve_semi_frame
    :retrieve_semi_pred
    :retrieve_semi_var
    :semi_out_of_date
    :semi_setup_post
    :semi_setup_pre
    :semi_up_to_date_p
    :size_current_grammar
    :test_user
    :update_entry
    :user_read_only_p
    :value_set
    :version
    ))

(defvar *lexdb-dump-tdl* t)
(defvar *lexdb-message-old-server* "PostgreSQL server version is ~a. Please upgrade to version ~a or above.")
(defvar *lexdb-message-old-lkb* "Your LexDB version (~a) is incompatible with this LKB version (requires v. ~ax). Try obtaining a more recent LKB binary.")
(defvar *lexdb-message-old-lexdb* "Your LexDB version (~a) is incompatible with this LKB version (requires v. ~ax). You must load updated setup files. See http://www.cl.cam.ac.uk/~~bmw20/DT/initialize-db.html")


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
   (fields-tb 
    :initform nil :accessor fields-tb :initarg :fields-tb)
   ;; a-list for mapping the lexicon-table fields to the psort-or-lex structure
   (fields-map :initform nil :accessor fields-map)
   (mneum-f :initform nil :accessor mneum-f)
   (fields :initform nil :accessor fields)   
   ))

(defclass psql-database (sql-database)
  ((connection :initform nil :accessor connection :initarg connection)
   (server-version :initform nil :accessor server-version)
   ))

(defclass psql-lex-database (psql-database external-lex-database)
  ((lexdb-version :initform nil :accessor lexdb-version)
   (semi :initform nil :accessor semi)
   ))

(defclass sql-query ()
  ((sql-string :accessor sql-string :initarg :sql-string :initform nil)
   (records :initform :unknown :accessor records)
   (columns :initform :unknown :accessor columns)))

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
  (defun LKB::MAKE-INSTANCE-PSQL-LEX-ENTRY nil))

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
