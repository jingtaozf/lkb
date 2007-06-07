;;; Copyright (c) 2004
;;;   Ben Waldron
;;;   see `licence.txt' for conditions.

(in-package :common-lisp-user)

;;(require :foreign)

(defpackage :pq
  (:use :common-lisp 
	#+:allegro :foreign-functions
	#+:sbcl :sb-alien
	) 
  (:export 
   exec-status-kw-map
   conn-status-kw-map
   connectdb
   finish
   reset
   db
   user
   pass
   host
   port
   options
   status
   parameter-status
   protocol-version
   error-message
   socket
   packend-PID
   exec
   result-status
   result-error-message
   clear
   ntuples
   nfields
   fname
   fnumber
   ftable
   ftablecol
   fformat
   pq-ftype
   fmod
   binary-tuples
   getvalue
   getisnull
   getlength
   cmd-status
   cmd-tuples
   escape-string
   put-copy-data
   put-copy-end
   get-copy-data
   getline
   getline-async
   putline
   putnbytes
   endcopy
   
   sql-error
   
   set-client-encoding
   )) 
