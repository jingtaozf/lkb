;;; Copyright (c) 2004-2005
;;;   Ben Waldron;
;;;   see `LICENSE' for conditions.

;; interface to libpq (PostgreSQL version 8.0)
;;  for libpq documentation see the chapter "libpq - C Library"
;;  in the PostgreSQL manual


(in-package :pq)

;; 'define-constant' taken from SBCL manual
;; Under ANSI spec, application of defconstant multiple times is undefined
;; unless values are eql. SBCL treats this undefined behaviour as an error.
;; What's worse, in SBCL defconstant takes effect both at load time and at
;; compile time...
(defmacro define-constant (name value &optional doc)
       `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
	  ,@(when doc (list doc))))

(define-constant *lexdb-emacs-other-fns*
    '(initialize-lexdb
      lexdb-fn
      ))

;
;;(def-foreign-type PGconn (:struct))
;;(def-foreign-type PGresult (:struct))
;;(def-foreign-type ExecStatusType :int)
(define-alien-type PGconn (sb-alien:struct nil))
(define-alien-type PGresult (sb-alien:struct nil))
(define-alien-type ExecStatusType (sb-alien:signed))

;
(define-constant exec-status-kw-map
    '((0 . :PGRES_EMPTY_QUERY)		; empty query string was executed
      (1 . :PGRES_COMMAND_OK)		; a query command that doesn't return
					; anything was executed properly by the
					; backend
      (2 . :PGRES_TUPLES_OK)		; a query command that returns tuples was
					; executed properly by the backend,
					; PGresult contains the result tuples
      (3 . :PGRES_COPY_OUT)		; Copy Out data transfer in progress
      (4 . :PGRES_COPY_IN)		; Copy In data transfer in progress
      (5 . :PGRES_BAD_RESPONSE)		; an unexpected response was recv'd from
					; the backend
      (6 . :PGRES_NONFATAL_ERROR)	; notice or warning message
      (7 . :PGRES_FATAL_ERROR)))	; query failed

;
;;(def-foreign-type Oid :unsigned-int)
(define-alien-type Oid (sb-alien:signed))
(define-constant InvalidOid 0) ;; SEE include/postgres_ext.h

;
;; SEE include/catalog/pg_type.h
(define-constant BOOLOID 16)
(define-constant BYTEAOID 17)
(define-constant CHAROID 18)
(define-constant NAMEOID 19)
(define-constant INT8OID 20)
(define-constant INT2OID 21)
(define-constant INT2VECTOROID 22)
(define-constant INT4OID 23)
(define-constant REGPROCOID 24)
(define-constant TEXTOID 25)
(define-constant OIDOID 26)
(define-constant TIDOID 27)
(define-constant XIDOID 28)
(define-constant CIDOID 29)
(define-constant OIDVECTOROID 30)
;;... (SEE include/catalog/pg_type.h)

;
;;(def-foreign-type size_t :int)
(define-alien-type size_t (sb-alien:signed))

;
;;(def-foreign-type ConnStatusType :int)
(define-alien-type ConnStatusType (sb-alien:signed))

;
(define-constant conn-status-kw-map
    '((0 . :CONNECTION_OK)
      (1 . :CONNECTION_BAD)
					; Non-blocking mode only below here
      (2 . :CONNECTION_STARTED)		; Waiting for connection to be made
      (3 . :CONNECTION_MADE)		; Connection OK; waiting to send
      (4 . :CONNECTION_AWAITING_RESPONSE) ;Waiting for a response from the
					; postmaster
      (5 . :CONNECTION_AUTH_OK)		; Received authentication; waiting for
					; backend startup
      (6 . :CONNECTION_SETENV)		; Negotiating environment
      (7 . :CONNECTION_SSL_STARTUP)	; Negotiating SSL
      (8 . :CONNECTION_NEEDED)		; Internal state: connect() needed
      ))

;; DATABASE CONNECTION CONTROL FUNCTIONS

;; conninfo parameter settings separated by whitespace
;; single quotes and backslashes must be escaped
;; parameters:
;;  host (default /tmp [Unix] or localhost [other])
;;  hostaddr
;;  port (port number/socket filename extension)
;;  dbame (defaults to username)
;;  user (defaults to OS login name)
;;  password
;;  connect_timeout (max wait in seconds)
;;  options (command-line options)
;;  sslmode (disable, allow, prefer, require)
;;  requiressl (DEPRECATED)
;;  service (service name in pg_service.conf for additional params)
;; parameters default to corresponding environment variable if set
;; call status after invoking this function

(define-alien-routine ("PQconnectdb" connectdb) (* PGconn)
  (conninfo (sb-alien:c-string)))

;; PQsetdbLogin OBSOLETE
;; PQsetdb OBSOLETE

;(def-foreign-call (connect-start "PQconnectStart")
;    ((conninfo (* :char)))
;  :returning PGconn)

;; PQconnectStart UNUSED
;; PQconnectPoll UNUSED
;; PQconndefaults UNUSED

(define-alien-routine ("PQfinish" finish) void
    (conn (* PGconn)))

(define-alien-routine ("PQreset" reset) void
    (conn (* PGconn)))

;; PQresetStart UNUSED
;; PQresetPoll UNUSED

;; CONNECTION STATUS FUNCTIONS

(define-alien-routine ("PQdb" db) (c-string)
  (conn (* PGconn)))

(define-alien-routine ("PQuser" user) (c-string)
    (conn (* PGconn)))

(define-alien-routine ("PQpass" pass) (c-string)
  (conn (* PGconn)))

(define-alien-routine ("PQhost" host) (c-string)
  (conn (* PGconn)))

(define-alien-routine ("PQport" port) (c-string)
  (conn (* PGconn)))

;; PQtty OBSOLETE

(define-alien-routine ("PQoptions" options) (c-string)
  (conn (* PGconn)))

(define-alien-routine ("PQstatus" status) ConnStatusType
    (conn (* PGconn)))

;; PQtransactionStatus UNUSED

;; parameters reported include:
;;  server_version
;;  server_encoding (see also PQserverVersion)
;;  client_encoding
;;  is_superuser
;;  session_authorization
;;  DateStyle
;;  TimeZone
;;  integer_datetimes
;; -7.3

(define-alien-routine ("PQparameterStatus" parameter-status) (c-string)
    (conn (* PGconn)) (param-name (c-string)))

;;-7.3
;; 3 for 7.4+
;; 2 for pre-7.4
;; 0  for bad connection
(define-alien-routine ("PQprotocolVersion" protocol-version) (signed)
    (conn (* PGconn)))

;;-7.4
;; >= 0 valid
;; -1 no server connection
(define-alien-routine ("PQserverVersion" server-version) (signed)
    (conn (* PGconn)))

(define-alien-routine ("PQerrorMessage" error-message) (c-string)
  (conn (* PGconn)))

(define-alien-routine ("PQsocket" socket) (signed)
  (conn (* PGconn)))

(define-alien-routine ("PQbackendPID" backend-PID) (signed)
  (conn (* PGconn)))

;;PQgetSSL UNUSED

;; COMMAND EXECUTION FUNCTIONS

;; MAIN FUNCTIONS

;; may return null pointer (treat as PGRES_FATAL_ERROR)
;; returns result of last command
(define-alien-routine ("PQexec" exec) (* PGresult)
  (conn (* PGconn)) (command (c-string)))

;;-7.3
;; PQexecParams UNUSED
;;-7.3
;; PQprepare UNUSED
;;-7.3
;; PQexecPrepared UNUSED

;; returns:
;;  PGRES_EMPTY_QUERY
;;  PGRES_COMMAND_OK (command that never returns rows)
;;  PGRES_TUPLES_OK (can retrieve rows returned)
;;  PGRES_COPY_OUT
;;  PGRES_COPY_IN
;;  PGRES_BAD_RESPONSE
;;  PGRES_NONFATAL_ERROR (never returned directly)
;;  PGRES_FATAL_ERROR
(define-alien-routine ("PQresultStatus" result-status) ExecStatusType
  (res (* PGresult)))

(define-alien-routine ("PQresStatus" res-status) (c-string)
  (status ExecStatusType))

(define-alien-routine ("PQresultErrorMessage" result-error-message) (c-string)
  (res (* PGresult)))

;;-7.3
;; PQresultErrorField UNUSED

;; every PGresult should be cleared when no onger needed
(define-alien-routine ("PQclear" clear) void
  (res (* PGresult)))

;; PQmakeEmptyPGResult UNUSED

;; RETRIEVING QUERY RESULT INFORMATION
;; extract info from PGresult with status PGRES_TUPLES_OK

(define-alien-routine ("PQntuples" ntuples) (signed)
  (res (* PGresult)))

(define-alien-routine ("PQnfields" nfields) (signed)
  (res (* PGresult)))

;; column numbers start at 0
(define-alien-routine ("PQfname" fname) (c-string)
  (res (* PGresult)) (column-number (signed)))

;; returns -1 if no match
(define-alien-routine ("PQfnumber" fnumber) (signed)
  (res (* PGresult)) (column-name (c-string)))

;; InvalidOid returned if 
;;  - column number out of range
;;  - or column not simple reference to table column
;;  - or pre-3.0 protocol
;;-7.3
(define-alien-routine ("PQftable" ftable) Oid
  (res (* PGresult)) (column-number (signed)))

;;-7.3
(define-alien-routine ("PQftablecol" ftablecol) (signed)
  (res (* PGresult)) (column-number (signed)))

;; 0=text
;; 1=binary
;;-7.3
(define-alien-routine ("PQfformat" fformat) (signed)
  (res (* PGresult)) (column-number (signed)))

;; (cannot use ftype name)
;; can then query pg_type
(define-alien-routine ("PQftype" pq-ftype) Oid
  (res (* PGresult)) (column-number (signed)))

(define-alien-routine ("PQfmod" fmod) (signed)
  (res (* PGresult)) (column-number (signed)))

;; PQfsize UNUSED

;; 0=text
;; 1=binary
(define-alien-routine ("PQbinaryTuples" binary-tuples) (signed)
  (res (* PGresult)))

(define-alien-routine ("PQgetvalue" getvalue) (c-string)
  (res (* PGresult)) (row-number (signed)) (column-number (signed)))

;; 0=non-null
;; 1=null
(define-alien-routine ("PQgetisnull" getisnull) (signed)
  (res (* PGresult)) (row-number (signed)) (column-number (signed)))

(define-alien-routine ("PQgetlength" getlength) (signed)
  (res (* PGresult)) (row-number (signed)) (column-number (signed)))

;; PQprint UNUSED
;; PQprint UNUSED

;; RETRIEVING RESULT INFORMATION FOR OTHER COMMANDS
;; eg.for non-SELECT results

(define-alien-routine ("PQcmdStatus" cmd-status) (c-string)
  (res (* PGresult)))

;; after INSERT, UPDATE, DELETE, MOVE, FETCH
(define-alien-routine ("PQcmdTuples" cmd-tuples) (c-string)
  (res (* PGresult)))

;; PQoidValue UNUSED
;; PQoidStatus DEPRECATED

;; ESCAPING STRINGS FOR INCLUSION IN SQL COMMANDS

;(def-foreign-call (escape-string "PQescapeString")
;    ((to (* :void)) (from (* :char)) (length size_t))
;  :returning size_t
;  :strings-convert nil)

(define-alien-routine ("PQescapeString" escape-string) size_t
  (to (* char)) (from (c-string)) (length size_t))


;; ESCAPING BINARY STRINGS FOR INCLUSION IN SQL COMMANDS

;(def-foreign-call (escape-bytea "PQescapeBytea")
;    ((from (* :unsigned-char)) (from-length size_t) (to-length (* size_t)))
;  :returning (* :unsigned-char))

;(def-foreign-call (unescape-bytea "PQunescapeBytea")
;    ((from (* :unsigned-char)) (to-length (* size_t)))
;  :returning (* :unsigned-char))

;;-7.3
;; PQfreemem UNUSED
;; needed by MS Windows after use of 
;;  PQunescapeBytea, PQescapeBytea, PQnotifies

;; ASYNCHRONOUS COMMAND PROCESSING

;;PQsendQuery UNUSED
;;-7.3
;;PQsendQueryParams UNUSED
;;-7.4
;;PGsendPrepare UNUSED
;;-7.3
;;PQsendQueryPrepared UNUSED
;;PQgetResult UNUSED
;;PQconsumeInput UNUSED
;;PQisBusy UNUSED
;;PQsetnonblocking UNUSED
;;PQisnonblocking UNUSED
;;PQflush not used

;; CANCELLING QUERIES IN PROGRESS

;;-7.4
;;PQgetCancel UNUSED
;;-7.4
;;PQfreeCancel UNUSED
;;-7.4
;;PQcancel UNUSED
;;-7.4
;;PQrequestCancel UNUSED

;; THE FAST-PATH INTERFACE

;; PQfn OBSOLETE

;; ASYNCHRONOUS NOTIFICATION

;; PQnotify UNUSED

;; FUNCTIONS ASSOCIATED WITH THE COPY COMMAND

;; (already defined)
;; PQnfields (# columnds to be copied)
;; PQbinarytuples (0=text, 1=binary)
;;-7.4
;;PGfformat

;; FUNCTIONS FOR SENDING COPY DATA
;; after COPY FROM STDIN

;; 1=ok
;; 0=blocked
;; -1=error
;;-7.3
(define-alien-routine ("PQputCopyData" put-copy-data) signed
  (conn (* PGconn)) (buffer c-string) (nbytes signed))

;; end COPY IN if errormsg NULL
;;  else force fail with errormsg
;; 1=ok
;; 0=blocked
;; -1=error
;;-7.3
(define-alien-routine ("PQputCopyEnd" put-copy-end) signed
    (conn (* PGconn)) (errormsg c-string))

;; FUNCTIONS FOR RECEIVING COPY DATA

;; get 1 row
;; *buffer points to allocated memory
;;  (must free with PQfreemem)
;; >1=num bytes in data row
;; 0=in progress
;; -1=copy done (req. async != 0)
;; -2=error
;;-7.3
(define-alien-routine ("PQgetCopyData" get-copy-data) signed
    (conn (* PGconn)) (buffer (* c-string)) (async signed))

;; OBSOLETE FUNCTIONS FOR COPY
;; (reqd for 7.3)
;; -> EOF = end of input
;; -> 0 = entire line read
;; -> 1 = buffer full
;; "\." = end of COPY
(define-alien-routine ("PQgetline" getline) signed
    (conn (* PGconn)) (buffer c-string) (length signed))

(define-alien-routine ("PQgetlineAsync" getline-async) signed
    (conn (* PGconn)) (buffer c-string) (length signed))

;; 0=OK
;; EOF=unable to send string
;; "\." to end COPY
(define-alien-routine ("PQputline" putline) signed
    (conn (* PGconn)) (string c-string))

(define-alien-routine ("PQputnbytes" putnbytes) signed
    (conn (* PGconn)) (buffer c-string) (nbytes signed))

;; 0=OK
(define-alien-routine ("PQendcopy" endcopy) signed
    (conn (* PGconn)))

;; CONTROL FUNCTIONS

;;-7.3
;; PQsetErrorVerbosity unused
;; PQtrace unused
;; PQuntrace unused

;; NOTICE PROCESSING

;;-7.3
;; PQsetNoticeReceiver
;; PQsetNoticeProcessor

;; ENVIRONMENT VARIABLES
;;  used by PQconnectdb, PQsetdbLogin, PQsetdb

;; PGHOST
;;-7.3
;; PGHOSTADDR
;; PGPORT
;; PGDATABASE
;; PGUSER
;; PGPASSWORD
;; PGSERVICE
;; PGREALM
;; PGOPTIONS
;; PGTTY obsolete
;;-7.3
;; PGSSLMODE
;; PGCONNECT_TIMEOUT

;; to specify default behaviour for each PostgreSQL session
;; PGDATESTYLE
;; PGTZ
;; PGCLIENTENCODING
;; PGGEQ0

;;THE PASSWORD FILE

;; chmod 0600 ~/.pgpass

;; hostname:port:database:username:password

;; CHARACTER SET SUPPORT

;;8.0

(define-alien-routine ("PQsetClientEncoding" set-client-encoding) signed
    (conn (* PGconn)) (encoding c-string))

(define-alien-routine ("PQsetClientEncoding" client-encoding) signed
    (conn (* PGconn)))

(define-alien-routine ("pg_encoding_to_char" pg_encoding_to_char) c-string
    (encoding-id signed))
