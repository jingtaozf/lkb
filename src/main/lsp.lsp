;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LKB -*-

;;; Copyright (c) 2003--2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: lsp.lsp
;;;      module: LKB server protocol
;;;     version: 0.0 (17-may-03)
;;;  written by: oe, csli stanford
;;; last update: 
;;;  updated by: 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:allegro
(eval-when (:load-toplevel :compile-toplevel :execute)
  (require :process)
  (require :socket))

(in-package :lkb)

(defconstant %lsp-ok% 0)

(defconstant %lsp-invalid-module% 1)

(defconstant %lsp-invalid-command% 2)

(defconstant %lsp-invalid-subcommand% 3)

(defconstant %lsp-invalid-asynchronous-command% 4)

(defconstant %lsp-syntax-error% 5)

(defconstant %lsp-invalid-format% 6)

(defconstant %lsp-invalid-format% 6)

(defconstant %lsp-mysterious-error% 42)

(defparameter *lsp-port* 4712)

(defparameter *lsp-dedbug-p* t)

(defparameter %lsp-socket% nil)

(defparameter %lsp-server% nil)

(defparameter %lsp-clients% nil)

(defstruct client
  id socket stream process display)

(defun lsp-initialize ()
  (lsp-shutdown)
  (setf %lsp-socket%
    (socket:make-socket :connect :passive :local-port *lsp-port*)))

(defun lsp-shutdown ()
  (loop
      for client in %lsp-clients%
      do (lsp-shutdown-client client))
  (when %lsp-socket%
    (when *lsp-dedbug-p*
      (format t "lsp-shutdown(): shutting down server socket.~%"))
    (unless (eq mp:*current-process* %lsp-server%)
      (let ((process %lsp-server%))
        (setf %lsp-server% nil)
        (ignore-errors (mp:process-kill process))))
    (ignore-errors (socket:shutdown %lsp-socket% :output))
    (ignore-errors (socket:shutdown %lsp-socket% :input))
    (ignore-errors (close %lsp-socket%)))
  (setf %lsp-socket% nil))

(defun lsp-shutdown-client (client)
  (cond
   ((numberp client)
    (loop
        for match in %lsp-clients%
        when (= client (client-id match)) do (lsp-shutdown-client match)))
   (t
    (setf %lsp-clients% (delete client %lsp-clients%))
    (let ((process (client-process client)))
      (unless (eq mp:*current-process* process)
        (mp:process-kill process))
      (when *lsp-dedbug-p*
        (format 
         t 
         "lsp-shutdown(): shutting down client # ~a~%"
         (client-id client))
        (ignore-errors (force-output (client-stream client))
        (ignore-errors (close (client-stream client)))))))))

(defun lsp-server (&key wait)
  (if wait
    (unwind-protect
      (loop
          initially (lsp-initialize)
          while %lsp-socket%
          for i from 0
          for stream = (socket:accept-connection %lsp-socket% :wait t)
          when *lsp-dedbug-p* do
            (let* ((address (socket:remote-host stream))
                   (host (socket:ipaddr-to-hostname address))
                   (port (socket:remote-port stream)))
              (format
               t
               "lsp-server(): remote connection from `~a:~a' [~a]~%"
               host port i))
          do 
            (let ((client (make-client 
                           :id i :socket %lsp-socket% :stream stream)))
              (setf (client-process client)
                (mp:run-function 
                 (format nil "LSP Client # ~a" i) #'lsp-loop i stream))
              (push client %lsp-clients%)))
      (lsp-shutdown))
    (setf %lsp-server%
      (mp:run-function "LSP Server" #'lsp-server :wait t))))

(defun lsp-loop (id stream)
  (loop
      with *package* = (find-package :lkb)
      with size = 2048
      with buffer = (make-array size :element-type 'character
                                :adjustable nil :fill-pointer 0)
      while (streamp stream)
      for n = (loop
                  initially (setf (fill-pointer buffer) 0)
                  for n from 1
                  for c = (read-char stream nil nil)
                  when (null c) do 
                    (format
                     t
                     "[~a] lsp-loop(): premature end of file ~
                      (read ~a characters)~%" 
                     id n)
                    (lsp-shutdown-client id)
                    (return)
                  when (= n size) do
                    (incf size size)
                    (setf buffer (adjust-array buffer size))
                  when (char= c #\page) do
                    (return n)
                  while c do (vector-push c buffer))
      when (and (numberp n) (> n 1)) do
        (lsp-process-event id buffer stream)
      else do
        (ignore-errors (close stream))
        (return)))

(defun lsp-process-event (id event stream)
  (let* ((command (lsp-parse-command id event))
         (waitp (eq (first command) 'wait))
         (return %lsp-ok%))

    (when (null command)
      (format stream "~a~c~%" %lsp-syntax-error% #\page)
      (force-output stream)
      (return-from lsp-process-event))

    (when waitp (pop command))

    (when *lsp-dedbug-p*
      (format 
       t 
       "[~a] lsp-process-event(): received: `~(~a~)' command ~@[(wait)~].~%" 
       id (first command) waitp))
    
    (unless waitp
      (format stream "~a~c~%" return #\page)
      (force-output stream))
    (multiple-value-bind (foo condition)
      (ignore-errors
       (case (first command)
         (tsdb 
          (if (find-package :tsdb)
            (let* ((symbol (find-symbol "LSP-PROCESS-EVENT" :tsdb))
                   (function (when (fboundp symbol) 
                               (symbol-function symbol))))
              (if function
                (setf return
                  (funcall function id (rest command) (when waitp stream)))
                (setf return %lsp-invalid-module%)))
            (setf return %lsp-invalid-module%)))
         (mrs 
          (if (find-package :mrs)
            (let* ((symbol (find-symbol "LSP-PROCESS-EVENT" :mrs))
                   (function (when (fboundp symbol) 
                               (symbol-function symbol))))
              (if function
                (setf return 
                  (funcall function id (rest command) (when waitp stream)))
                (setf return %lsp-invalid-module%)))
            (setf return %lsp-invalid-module%)))
         (t
          (setf return %lsp-invalid-command%))))
      (declare (ignore foo))
      (when condition
        (when *lsp-dedbug-p*
          (format
           t
           "[~d] lsp-process-event(): ~a~%" 
           id condition))
        (setf return %lsp-mysterious-error%)))
    (when waitp
      (format stream " ~a~c~%" return #\page)
      (force-output stream))))

(defun lsp-parse-command (id string)
  (loop
      with *package* = (find-package :lkb)
      with *readtable* = (lsp-make-readtable)
      with stream = (make-string-input-stream string)
      for form = (ignore-errors (read stream nil :eof))
      when (null form) do
        (when *lsp-dedbug-p*
          (format 
           t 
           "[~a] lsp-parse-command(): parse error in `~a'~%"
           id string))
        (return)
      while (not (eq form :eof)) collect form))
;;
;; (cl:setf *default-server-path* '(:motif :display "localhost:0"))
;;

(defun lsp-make-readtable ()
  (copy-readtable))

(defun lsp-find-client (id)
  (loop
      for client in %lsp-clients%
      when (equal id (client-id client)) return client))

(defun lsp-retrieve-object (id n)
  (declare (ignore id)))
