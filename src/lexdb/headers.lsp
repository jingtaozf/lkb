;;; Copyright (c) 2001 -- 2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Ben Waldron;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

(defvar *psql-lexicon* nil) ;; -> *lexdb* ??
;;(def-lkb-parameter *psql-lexicon-parameters* nil :user) ;; see main/globals.lsp

(defvar *psql-database-connect-timeout* 30)
(defvar *psql-database-port* 5432)

(defvar *lexdb-major-version* "3.5")
(defvar *lexdb-dump-skip-stream* t)
(defvar *lexdb-dump-version* 0)
(defvar *lexdb-dump-timestamp* nil)
(defvar *lexdb-dump-source* "?")
(defvar *lexdb-dump-user* nil)
(defvar *lexdb-dump-lang* nil)
(defvar *lexdb-dump-country* nil)
;; set this to nil to prevent tdl dump accompanying lexdb dump
(defvar *lexdb-dump-tdl* t)
;; obsolete after 3.33
(defvar *lexdb-message-old-server* "PostgreSQL server version is ~a. Please upgrade to version ~a or above.")
(defvar *lexdb-message-old-lkb* "Your LexDB version (~a) is incompatible with this LKB version (requires v. ~ax). Try obtaining a more recent LKB binary.")
(defvar *lexdb-message-old-lexdb* "Your LexDB version (~a) is incompatible with this LKB version (requires v. ~ax). You must load updated setup files. See http://www.cl.cam.ac.uk/~~bmw20/DT/initialize-db.html")
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
   (fields-map :initform nil :accessor fields-map)
   (fields :initform nil :accessor fields)))

(defclass psql-database ()
  (
   (dbname :initform nil :accessor dbname :initarg :dbname)
   (host :initform "localhost" :accessor host :initarg :host)
   (user :initform (sys:user-name) :accessor user :initarg :user)
   (password :initform "" :accessor password :initarg :password)
   (port :initform (or (system:getenv "PGPORT") *psql-database-port*) :accessor port :initarg :port)
   (connection :initform nil :accessor connection :initarg connection)))

(defclass psql-lex-database (psql-database external-lex-database)
  ((lexdb-version :initform nil :accessor lexdb-version)
   (ext-fns :initform nil :accessor ext-fns)
   (semi :initform nil :accessor semi)))

(defclass psql-database-table ()
  ((recs :initarg :recs :accessor recs)
   (cols :initarg :cols :accessor cols)))

;; this should be replaced with a-list records
(defclass psql-lex-entry ()
  ((fv-pairs :initarg :fv-pairs)))

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
  (defun LKB::LOAD-PSQL-LEXICON-FROM-SCRIPT (&rest foo) (declare (ignore foo))
    (error "Please set *psql-lexicon-parameters* to NIL"))
  )

(defun psql-initialize ()
  ;;
  ;; make sure we `mark' the current universe as PSQL-enabled.
  ;;
  (pushnew :psql *features*)
  (handler-case (load-libpq-3) 
    (file-error () 
      ;; some feedback to user
      (format t ";   Warning: cannot load libpq.so")
      (format t "~%;            (PSQL lexicon functionality will be unavailable)")
      (format t "~%;            (hint: are the PostgreSQL libraries installed on your machine?)")
      ;; need this for backward compatibility with ERG script
      ;; (also a good idea anyway)
      (setf *features* (remove :psql *features*)))))

(defun load-libpq-3 nil
  (let (#+allegro (excl::*load-foreign-types* (cons "3" excl::*load-foreign-types*)))
    (load "libpq.so.3")))