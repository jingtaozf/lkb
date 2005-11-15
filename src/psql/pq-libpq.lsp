;;; Copyright (c) 2004-2005
;;;   Ben Waldron;
;;;   see `licence.txt' for conditions.

;; interface to libpq (PostgreSQL version 8.0)
;;  for libpq documentation see the chapter "libpq - C Library"
;;  in the PostgreSQL manual


(in-package :pq)

;
(def-foreign-type PGconn (:struct))
(def-foreign-type PGresult (:struct))
(def-foreign-type ExecStatusType :int)

;
(defconstant exec-status-kw-map
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
(def-foreign-type Oid :unsigned-int)
(defconstant InvalidOid 0) ;; SEE include/postgres_ext.h

;
;; SEE include/catalog/pg_type.h
(defconstant BOOLOID 16)
(defconstant BYTEAOID 17)
(defconstant CHAROID 18)
(defconstant NAMEOID 19)
(defconstant INT8OID 20)
(defconstant INT2OID 21)
(defconstant INT2VECTOROID 22)
(defconstant INT4OID 23)
(defconstant REGPROCOID 24)
(defconstant TEXTOID 25)
(defconstant OIDOID 26)
(defconstant TIDOID 27)
(defconstant XIDOID 28)
(defconstant CIDOID 29)
(defconstant OIDVECTOROID 30)
;;... (SEE include/catalog/pg_type.h)

;
(def-foreign-type size_t :int)

;
(def-foreign-type ConnStatusType :int)

;
(defconstant conn-status-kw-map
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
(def-foreign-call (connectdb "PQconnectdb")
    ((conninfo (* :char)))
  :returning PGconn
  :strings-convert t)

;; PQsetdbLogin OBSOLETE
;; PQsetdb OBSOLETE

;(def-foreign-call (connect-start "PQconnectStart")
;    ((conninfo (* :char)))
;  :returning PGconn)

;; PQconnectStart UNUSED
;; PQconnectPoll UNUSED
;; PQconndefaults UNUSED

(def-foreign-call (finish "PQfinish")
    ((conn (* PGconn)))
  :returning :void)

(def-foreign-call (reset "PQreset")
    ((conn (* PGconn)))
  :returning :void)

;; PQresetStart UNUSED
;; PQresetPoll UNUSED

;; CONNECTION STATUS FUNCTIONS

(def-foreign-call (db "PQdb")
    ((conn (* PGconn)))
  :returning ((* :char)))

(def-foreign-call (user "PQuser")
    ((conn (* PGconn)))
  :returning ((* :char)))

(def-foreign-call (pass "PQpass")
    ((conn (* PGconn)))
  :returning ((* :char)))

(def-foreign-call (host "PQhost")
    ((conn (* PGconn)))
  :returning ((* :char)))

(def-foreign-call (port "PQport")
    ((conn (* PGconn)))
  :returning ((* :char)))

;; PQtty OBSOLETE

(def-foreign-call (options "PQoptions")
    ((conn (* PGconn)))
  :returning ((* :char)))

(def-foreign-call (status "PQstatus")
    ((conn (* PGconn)))
  :returning ConnStatusType)

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
(def-foreign-call (parameter-status "PQparameterStatus")
    ((conn (* PGconn)) (param-name (* :char)))
  :returning ((* :char))
  :strings-convert t)

;;-7.3
;; 3 for 7.4+
;; 2 for pre-7.4
;; 0  for bad connection
(def-foreign-call (protocol-version "PQprotocolVersion")
    ((conn (* PGconn)))
  :returning :int)

;;-7.4
;; >= 0 valid
;; -1 no server connection
(def-foreign-call (server-version "PQserverVersion")
    ((conn (* PGconn)))
  :returning :int)

(def-foreign-call (error-message "PQerrorMessage")
    ((conn (* PGconn)))
  :returning ((* :char)))

(def-foreign-call (socket "PQsocket")
    ((conn (* PGconn)))
  :returning :int)

(def-foreign-call (backend-PID "PQbackendPID")
    ((conn (* PGconn)))
  :returning :int)

;;PQgetSSL UNUSED

;; COMMAND EXECUTION FUNCTIONS

;; MAIN FUNCTIONS

;; may return null pointer (treat as PGRES_FATAL_ERROR)
;; returns result of last command
(def-foreign-call (exec "PQexec")
    ((conn (* PGconn)) (command (* :char)))
  :returning ((* PGresult))
  :strings-convert t)

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
(def-foreign-call (result-status "PQresultStatus")
    ((res (* PGresult)))
  :returning ExecStatusType)

(def-foreign-call (res-status "PQresStatus")
    ((status ExecStatusType))
  :returning ((* :char)))

(def-foreign-call (result-error-message "PQresultErrorMessage")
    ((res (* PGresult)))
  :returning ((* :char)))

;;-7.3
;; PQresultErrorField UNUSED

;; every PGresult should be cleared when no onger needed
(def-foreign-call (clear "PQclear")
    ((res (* PGresult)))
  :returning :void)

;; PQmakeEmptyPGResult UNUSED

;; RETRIEVING QUERY RESULT INFORMATION
;; extract info from PGresult with status PGRES_TUPLES_OK

(def-foreign-call (ntuples "PQntuples")
    ((res (* PGresult)))
  :returning :int)

(def-foreign-call (nfields "PQnfields")
    ((res (* PGresult)))
  :returning :int)

;; column numbers start at 0
(def-foreign-call (fname "PQfname")
    ((res (* PGresult)) (column-number :int))
  :returning ((* :char)))

;; returns -1 if no match
(def-foreign-call (fnumber "PQfnumber")
    ((res (* PGresult)) (column-name (* :char)))
  :returning :int
  :strings-convert t)

;; InvalidOid returned if 
;;  - column number out of range
;;  - or column not simple reference to table column
;;  - or pre-3.0 protocol
;;-7.3
(def-foreign-call (ftable "PQftable")
    ((res (* PGresult)) (column-number :int))
  :returning Oid)

;;-7.3
(def-foreign-call (ftablecol "PQftablecol")
    ((res (* PGresult)) (column-number :int))
  :returning :int)

;; 0=text
;; 1=binary
;;-7.3
(def-foreign-call (fformat "PQfformat")
    ((res (* PGresult)) (column-number :int))
  :returning :int)

;; (cannot use ftype name)
;; can then query pg_type
(def-foreign-call (pq-ftype "PQftype")
    ((res (* PGresult)) (column-number :int))
  :returning Oid)

(def-foreign-call (fmod "PQfmod")
    ((res (* PGresult)) (column-number :int))
  :returning :int)

;; PQfsize UNUSED

;; 0=text
;; 1=binary
(def-foreign-call (binary-tuples "PQbinaryTuples")
    ((res (* PGresult)))
  :returning :int)

(def-foreign-call (getvalue "PQgetvalue")
    ((res (* PGresult)) (row-number :int) (column-number :int))
  :returning ((* :char)))

;; 0=non-null
;; 1=null
(def-foreign-call (getisnull "PQgetisnull")
    ((res (* PGresult)) (row-number :int) (column-number :int))
  :returning :int)

(def-foreign-call (getlength "PQgetlength")
    ((res (* PGresult)) (row-number :int) (column-number :int))
  :returning :int)

;; PQprint UNUSED

;; RETRIEVING RESULT INFORMATION FOR OTHER COMMANDS
;; eg.for non-SELECT results

(def-foreign-call (cmd-status "PQcmdStatus")
    ((res (* PGresult)))
  :returning ((* :char)))

;; after INSERT, UPDATE, DELETE, MOVE, FETCH
(def-foreign-call (cmd-tuples "PQcmdTuples")
    ((res (* PGresult)))
  :returning ((* :char)))

;; PQoidValue UNUSED
;; PQoidStatus DEPRECATED

;; ESCAPING STRINGS FOR INCLUSION IN SQL COMMANDS

(def-foreign-call (escape-string "PQescapeString")
    ((to (* :void)) (from (* :char)) (length size_t))
  :returning size_t
  :strings-convert nil)

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
(def-foreign-call (put-copy-data "PQputCopyData")
    ((conn (* PGconn)) (buffer (* :char)) (nbytes :int))
  :returning :int
  :strings-convert t)

;; end COPY IN if errormsg NULL
;;  else force fail with errormsg
;; 1=ok
;; 0=blocked
;; -1=error
;;-7.3
(def-foreign-call (put-copy-end "PQputCopyEnd")
    ((conn (* PGconn)) (errormsg (* :char)))
  :returning :int
  :strings-convert t)

;; FUNCTIONS FOR RECEIVING COPY DATA

;; get 1 row
;; *buffer points to allocated memory
;;  (must free with PQfreemem)
;; >1=num bytes in data row
;; 0=in progress
;; -1=copy done (req. async != 0)
;; -2=error
;;-7.3
(def-foreign-call (get-copy-data "PQgetCopyData")
    ((conn (* PGconn)) (buffer (* (* :char))) (async :int))
  :returning :int
  :strings-convert t)

;; OBSOLETE FUNCTIONS FOR COPY
;; (reqd for 7.3)
;; -> EOF = end of input
;; -> 0 = entire line read
;; -> 1 = buffer full
;; "\." = end of COPY
(def-foreign-call (getline "PQgetline")
    ((conn (* PGconn)) (buffer (* :char)) (length :int))
  :returning :int
  :strings-convert t)

(def-foreign-call (getline-async "PQgetlineAsync")
    ((conn (* PGconn)) (buffer (* :char)) (length :int))
  :returning :int
  :strings-convert t)

;; 0=OK
;; EOF=unable to send string
;; "\." to end COPY
(def-foreign-call (putline "PQputline")
    ((conn (* PGconn)) (string (* :char)))
  :returning :int
  :strings-convert t)

(def-foreign-call (putnbytes "PQputnbytes")
    ((conn (* PGconn)) (buffer (* :char)) (nbytes :int))
  :returning :int
  :strings-convert t)

;; 0=OK
(def-foreign-call (endcopy "PQendcopy")
    ((conn (* PGconn)))
  :returning :int)

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

(def-foreign-call (set-client-encoding "PQsetClientEncoding")
    ((conn (* PGconn)) (encoding (* :char)))
  :returning :int
  :strings-convert t)

(def-foreign-call (client-encoding "PQsetClientEncoding")
    ((conn (* PGconn)))
  :returning :int
  :strings-convert t)

(def-foreign-call (pg-encoding-to-char "pg_encoding_to_char")
    ((encoding-id :int))
  :returning ((* :char))
  :strings-convert t)
