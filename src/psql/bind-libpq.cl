;;; -*- Mode: Common-Lisp; Package: PG; -*-

;; This software is Copyright (c) Marina Motion LLC, November 2001.
;; Marina Motion LLC grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :pg)

;;; from postgres_ext.h
(def-foreign-type Oid :unsigned-int)
(defvar NAMEDATALEN 32)

;;; Application-visible enum types

(def-foreign-type ConnStatusType :int)
(defvar CONNECTION_OK 0)
(defvar CONNECTION_BAD 1)
(defvar CONNECTION_STARTED 2) ;Waiting for connection to be made.
(defvar CONNECTION_MADE 3) ; Connection OK; waiting to send.
(defvar	CONNECTION_AWAITING_RESPONSE 4) ; Waiting for a response from the postmaster.
(defvar CONNECTION_AUTH_OK 5)	; Received authentication; waiting for backend startup.
(defvar	CONNECTION_SETENV 6)		; Negotiating environment.

(defun DECODE-CONNECTION-STATUS (status-type)
  (case status-type
    (0 :connection-ok)
    (1 :connection-bad)
    (2 :connection-started)
    (3 :connection-made)
    (4 :connection-awaiting-response)
    (5 :connection-auth-ok)
    (6 :connection-setenv)
    (otherwise :unknown)))

(def-foreign-type PostgresPollingStatusType :int)
(defvar PGRES_POLLING_FAILED 0)
(defvar	PGRES_POLLING_READING 1) ; These two indicate that one may
(defvar PGRES_POLLING_WRITING 2)	; use select before polling again.   
(defvar PGRES_POLLING_OK 3)
(defvar PGRES_POLLING_ACTIVE 4)		; Can call poll function immediately.

(defun DECODE-POSTGRES-POLLING-STATUS (status-type)
  (case status-type
    (0 :pgres-polling-failed)
    (1 :pgres-polling-reading)
    (2 :pgres-polling-writing)
    (3 :pgres-polling-ok)
    (4 :pgres-polling-active)
    (otherwise :unknown)))

(def-foreign-type ExecStatusType :int)
(defvar PGRES_EMPTY_QUERY 0)
(defvar	PGRES_COMMAND_OK 1) ; a query command that doesn't return anything was executed properly by the backend 
(defvar	PGRES_TUPLES_OK 2) ; a query command that returns tuples was executed properly by the backend,
								 ; PGresult contains the result tuples 
(defvar	PGRES_COPY_OUT 3) ; Copy Out data transfer in progress 
(defvar	PGRES_COPY_IN 4)	; Copy In data transfer in progress 
(defvar PGRES_BAD_RESPONSE 5)		; an unexpected response was recv'd from the backend 
(defvar	PGRES_NONFATAL_ERROR 6)
(defvar	PGRES_FATAL_ERROR 7)

(defun DECODE-EXEC-STATUS (status-type)
  (case status-type
    (0 :pgres-empty-query)
    (1 :pgres-command-ok)
    (2 :pgres-tuples-ok)
    (3 :pgres-copy-out)
    (4 :pgres-copy-in)
    (5 :pgres-bad-response)
    (6 :pgres-nonfatal-error)
    (7 :pgres-fatal-error)
    (otherwise :unknown)))

(def-foreign-type PGconn (:struct))
(def-foreign-type PGresult (:struct))

(def-foreign-type PGnotify
    (:struct
     (relname (:array :char 32)) ; NAMEDATALEN=32
     (be_pid  :int)))

(def-foreign-type PQnoticeProcessor
    (:function
     ((* :void)
      (* :char))
     :void))

(def-foreign-type pqbool :char)

(def-foreign-type PQprintOpt
    (:struct
     (header   pqbool)
     (align    pqbool)
     (standard pqbool)
     (html3    pqbool)
     (expanded pqbool)
     (pager    pqbool)
     (fieldSep (* :char))
     (tableOpt (* :char))
     (caption  (* :char))
     (fieldName (* (* :char)))))

(def-foreign-type PQconninfoOption 
    (:struct
     (keyword (* :char))		;The keyword of the option		
     (envvar (* :char))			; Fallback environment variable name
     (compiled (* :char))		; Fallback compiled in default value
     (val (* :char))			; Option's current value, or NULL
     (label (* :char))			; Label for field in connect dialog
     (dispchar (* :char))		; Character to display for this field in
					; a connect dialog. Values are: ""
					; Display entered value as is "*"
					; Password field - hide value "D"	Debug
					; option - don't show by default 
     (dispsize :int)))			; Field size in characters for dialog
 
(def-foreign-type PQArgBlock
    (:struct
     (len   :int)
     (isint :int)
     (u     (:union
	     (ptr (* :int))
	     (integer :int)))))

;;;------------------------------------------------------------------------------------
;;; FUNCTIONS
;;;------------------------------------------------------------------------------------

(def-foreign-call (connect-start "PQconnectStart")
    ((conninfo (* :char)))
  :returning ((* PGconn))
  :strings-convert t)

(def-foreign-call (connect-poll "PQconnectPoll")
    ((conn (* PGconn)))
  :returning PostgresPollingStatusType)

(def-foreign-call (connect-db "PQconnectdb")
    ((conninfo (* :char)))
  :returning ((* PGconn))
  :strings-convert t)

(def-foreign-call (set-db-login "PQsetdbLogin")
    ((pghost (* :char))
     (pgport (* :char))
     (pgoptions (* :char))
     (pgtty (* :char))
     (dbName (* :char))
     (login (* :char))
     (pwd   (* :char)))
  :returning ((* PGconn))
  :strings-convert t)


(def-foreign-call (finish "PQfinish") 
    ((conn (* PGconn)))
  :returning :void)

(def-foreign-call (conndefaults "PQconndefaults") 
    (:void)
  :returning ((* PQconninfoOption)))

(def-foreign-call (conninfo-free "PQconninfoFree")
    ((connOptions (* PQconninfoOption)))
  :returning :void)

(def-foreign-call (reset-start "PQresetStart")
    ((conn (* PGconn)))
  :returning :int)

(def-foreign-call (reset-poll "PQresetPoll")
    ((conn (* PGconn)))
  :returning PostgresPollingStatusType)

(def-foreign-call (reset "PQreset")
    ((conn (* PGconn)))
  :returning :void)

(def-foreign-call (request-cancel "PQrequestCancel")
    ((conn (* PGconn)))
  :returning :int)

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

(def-foreign-call (tty "PQtty")
    ((conn (* PGconn)))
  :returning ((* :char)))

(def-foreign-call (options "PQoptions")
    ((conn (* PGconn)))
  :returning ((* :char)))

(def-foreign-call (status "PQstatus")
    ((conn (* PGconn)))
  :returning ConnStatusType)

(def-foreign-call (error-message "PQerrorMessage")
    ((conn (* PGconn)))
  :returning ((* :char)))

(def-foreign-call (socket "PQsocket")
    ((conn (* PGconn)))
  :returning :int)

(def-foreign-call (backend-pid "PQbackendPID")
    ((conn (* PGconn)))
  :returning :int)

(def-foreign-call (client-encoding "PQclientEncoding")
    ((conn (* PGconn)))
  :returning :int)

(def-foreign-call (set-client-encoding "PQsetClientEncoding")
    ((conn (* PGconn))
     (encoding (* :char)))
  :returning :int
  :strings-convert t)

;;; Besides the fact that (* FILE) doesn't have meaning,
;;;  trace and untrace are reserved common lisp names.
;;;(def-foreign-call (trace "PQtrace")
;;;    ((conn (* PGconn))
;;;     (debug_port (* FILE)))
;;;  :returning :void)

;;;(def-foreign-call (untrace "PQuntrace")
;;;    ((conn (* PGconn)))
;;;  :returning :void)

(def-foreign-call (set-notice-processor "PQsetNoticeProcessor")
    ((conn (* PGconn))
     (proc PQnoticeProcessor)
     (arg  (* :void)))
  :returning PQnoticeProcessor)

(def-foreign-call (exec "PQexec")
    ((conn (* PGconn))
     (query (* :char)))
  :returning ((* PGresult))
  :strings-convert t)

(def-foreign-call (notifies "PQnotifies")
    ((conn (* PGconn)))
  :returning ((* PGnotify)))

(def-foreign-call (send-query "PQsendQuery")
    ((conn (* PGconn))
     (query (* :char)))
  :returning :int
  :strings-convert t)

(def-foreign-call (get-result "PQgetResult")
    ((conn (* PGconn)))
  :returning ((* PGresult)))
     
;;; Routines for managing an asychronous query 

(def-foreign-call (is-busy "PQisBusy")
    ((conn (* PGconn)))
  :returning :int)

(def-foreign-call (consume-input "PQconsumeInput")
    ((conn (* PGconn)))
  :returning :int)

;;; Routines for copy in/out 
(def-foreign-call (getline "PQgetline")
    ((conn (* PGconn))
     (string (* :char))
     (length :int))
  :returning :int
  :strings-convert t)
					;
(def-foreign-call (putline "PQputline")
    ((conn (* PGconn))
     (string (* :char)))
  :returning :int
  :strings-convert t)

(def-foreign-call (getline-async "PQgetlineAsync")
    ((conn (* PGconn))
     (buffer (* :char))
     (bufsize :int))
  :returning :int
  :strings-convert t)

(def-foreign-call (putnbytes "PQputnbytes")
    ((conn (* PGconn))
     (buffer (* :char))
     (nbytes :int))
  :returning :int
  :strings-convert t)

(def-foreign-call (endcopy "PQendcopy")
    ((conn (* PGconn)))
  :returning :int)

;;; Set blocking/nonblocking connection to the backend */
(def-foreign-call (setnonblocking "PQsetnonblocking")
    ((conn (* PGconn))
     (arg :int))				;
  :returning :int)

(def-foreign-call (isnonblocking "PQisnonblocking")
    ((conn (* PGconn)))
  :returning :int)

;;; Force the write buffer to be written (or at least try) */
(def-foreign-call (flush "PQflush")
    ((conn (* PGconn)))
  :returning :int)

;;; "Fast path" interface --- not really recommended for application use
;;;
(def-foreign-call (fn "PQfn")
    ((conn (* PGconn))
     (fnid :int)
     (result-buf (* :int))
     (result-len (* :int))
     (result-is-int :int)
     (args (* PQArgBlock))
     (nargs :int))
  :returning ((* PGresult)))

;;; Accessor functions for PGresult objects
(def-foreign-call (result-status "PQresultStatus")
    ((res (* PGresult)))
  :returning ExecStatusType)

(def-foreign-call (res-status "PQresStatus")
    ((status ExecStatusType))
  :returning ((* :char)))

(def-foreign-call (result-error-message "PQresultErrorMessage")
    ((res (* PGresult)))
  :returning ((* :char)))

(def-foreign-call (ntuples "PQntuples")
    ((res (* PGresult)))
  :returning :int)

(def-foreign-call (nfields "PQnfields")
    ((res (* PGresult)))
  :returning :int)

(def-foreign-call (binary-tuples "PQbinaryTuples")
    ((res (* PGresult)))
  :returning :int)

(def-foreign-call (field-name "PQfname")
    ((res (* PGresult))
     (field-num :int))
  :returning ((* :char)))

(def-foreign-call (field-number "PQfnumber")
    ((res (* PGresult))
     (field-name (* :char)))
  :returning :int
  :strings-convert t)

(def-foreign-call (field-type "PQftype")
    ((res (* PGresult))
     (field-num :int))
  :returning Oid)

(def-foreign-call (field-size "PQfsize")
    ((res (* PGresult))
     (field-num :int))
  :returning :int)

(def-foreign-call (field-mod "PQfmod")
    ((res (* PGresult))
     (field-num :int))
  :returning :int)

(def-foreign-call (cmd-status "PQcmdStatus")
    ((res (* PGresult)))
  :returning ((* :char)))

(def-foreign-call (oid-status "PQoidStatus")
    ((res (* PGresult))) ; old and ugly
  :returning ((* :char)))

(def-foreign-call (oid-value "PQoidValue")
    ((res (* PGresult))) ; new and improved
  :returning Oid)

(def-foreign-call (cmd-tuples "PQcmdTuples")
    ((res (* PGresult)))
  :returning ((* :char)))

(def-foreign-call (getvalue "PQgetvalue")
    ((res (* PGresult))
     (tup-num :int)
     (field-num :int))
  :returning ((* :char)))

(def-foreign-call (getlength "PQgetlength")
    ((res (* PGresult))
     (tup-num :int)
     (field-num :int))
  :returning :int)

(def-foreign-call (getisnull "PQgetisnull")
    ((res (* PGresult))
     (tup-num :int)
     (field-num :int))
  :returning :int)

;;; Delete a PGresult
(def-foreign-call (clear "PQclear")
    ((res (* PGresult)))
  :returning :void)
;;;
;;; Make an empty PGresult with given status (some apps find this
;;; useful). If conn is not NULL and status indicates an error, the
;;; conn's errorMessage is copied.

(def-foreign-call (make-empty-result "PQmakeEmptyPGresult")
    ((conn (* PGconn))
     (status ExecStatusType))
  :returning ((* PGresult)))
  
;;; === in fe-print.c === 

;;;(def-foreign-call (print "PQprint")
;;;    ((fout (* FILE *fout)) ;output stream
;;;     (res (* PGresult))
;;;     (ps (* PQprintOpt))) ; option structure
;;;  :returning :void)


;;; === in fe-lobj.c === 

;;; Large-object access routines 
(def-foreign-call (lo-open "lo_open")
    ((conn (* PGconn))
     (lobj-id Oid)
     (mode :int))
  :returning :int)

(def-foreign-call (lo-close "lo_close")
    ((conn (* PGconn))
     (fd :int))
  :returning :int)

;;;(def-foreign-call (lo-read "lo_read")
;;;    ((conn (* PGconn)) 
;;;     (fd :int) 
;;;     (buf (* :char)) 
;;;     (len size_t))
;;;  :returning :int
;;;  :strings-convert t)

;;;(def-foreign-call (lo-write "lo_write")
;;;    ((conn (* PGconn)) 
;;;     (fd :int) 
;;;     (buf (* :char)) 
;;;     (len size_t))
;;;  :returning :int
;;;  :strings-convert t)

(def-foreign-call (lo-lseek "lo_lseek")
    ((conn (* PGconn)) 
     (fd :int) 
     (offset :int)
     (whence :int))
  :returning :int)

(def-foreign-call (lo-creat "lo_creat")
    ((conn (* PGconn))
     (mode :int))
  :returning Oid)

(def-foreign-call (lo-tell "lo_tell")
    ((conn (* PGconn))
     (fd :int))
  :returning :int)

(def-foreign-call (lo-unlink "lo_unlink")
    ((conn (* PGconn)) 
     (lobjId Oid))			
  :returning :int)

(def-foreign-call (lo-import "lo_import")
    ((conn (* PGconn))
     (filename (* :char)))
  :returning Oid
  :strings-convert t)

(def-foreign-call (lo-export "lo_export")
    ((conn (* PGconn)) 
     (lobjId Oid)
     (filename (* :char)))	
  :returning :int
  :strings-convert t)

;;; === in fe-misc.c === 

;;; Determine length of multibyte encoded char at *s 
(def-foreign-call (mblen "PQmblen")
    ((s (* :unsigned-char))
     (encoding :int))
  :returning :int)

;;; Get encoding id from environment variable PGCLIENTENCODING 
(def-foreign-call (env2encoding "PQenv2encoding")
    (:void)
  :returning :int)
