;;; Copyright (c) 2001 -- 2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Ben Waldron;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

(defvar *psql-lexicon-parameters*) ;; define in GRAMMAR/globals.lsp

(defvar *postgres-temp-filename* nil)

(defvar *psql-lkb-version* "2.00")
(defvar *psql-lexdb-compat-version* "3.2")
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

;; temporary
(defvar *postgres-record-features* '(:name :f1 :f2 :f3 :f4 :f5 :f6 :f7 :f8 :f9 :source :lang :country :dialect :domains :genres :register :confidence :comments :exemplars :flags :version :userid :modstamp :orthkey))

(defvar *psql-mneum-f-back-compat-map* 
    '(("type" . "f1")
      ("orthography" . "f2")
      ("keyrel" . "f3") 
      ("altkey" . "f4") 
      ("alt2key" . "f5") 
      ("keytag" . "f6")
      ("altkeytag" . "f7")
      ("compkey" . "f8") 
      ("ocompkey" . "f9")))

(defvar *postgres-sql-fns*)
(setf *postgres-sql-fns*
  '(
    :clear_scratch
    :commit_scratch
    :complete
    :create_schema
    :db_owner
    :dump_db
    :dump_scratch_db
    :filter
    :get_filter 
    :initialize_current_grammar
    :lex_id_set 
    :lexdb_version
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
    :retrieve_semi_extra
    :retrieve_semi_frame
    :retrieve_semi_pred
    :retrieve_semi_var
    :semi_out_of_date
    :semi_setup_1
    :semi_setup_2
    :semi_up_to_date_p
    :show_scratch
    :size_current_grammar
    :test_user
    :update_entry
    :user_read_only_p
    :value_set
    :version
    ))

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
   ;(lex-tb :initform nil :accessor lex-tb :initarg :lex-tb)
   ;; table for mapping the lexicon-table fields to the psort-or-lex structure
   (fields-tb 
    :initform nil :accessor fields-tb :initarg :fields-tb)
   ;; a-list for mapping the lexicon-table fields to the psort-or-lex structure
   (fields-map :initform nil :accessor fields-map)
   (record-features :initform nil :accessor record-features)
   (mneum-f :initform nil :accessor mneum-f)))

(defclass psql-database (sql-database)
  ((connection :initform nil :accessor connection :initarg connection)
   (server-version :initform nil :accessor server-version)
   (fns :initform nil :accessor fns)))

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
