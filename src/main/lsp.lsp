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

(defconstant %lsp-mysterious-error% 42)

(defparameter *lsp-port* 4712)

(defparameter *lsp-debug-p* t)

(defparameter %lsp-socket% nil)

(defparameter %lsp-server% nil)

(defparameter %lsp-clients% nil)

(defparameter %lsp-object-counter% 0)

(defparameter %lsp-attic% (make-array 512))

(defstruct client
  id socket stream process
  (display #+:clim clim:*default-server-path* #-:clim nil))

#-:clisp
(defun lsp-initialize ()
  (lsp-shutdown)
  (setf %lsp-socket%
    (socket:make-socket :connect :passive :local-port *lsp-port*))
  (setf %lsp-object-counter% 0)
  (setf %lsp-attic% (make-array 512))
  %lsp-socket%)

#-:clisp
(defun lsp-shutdown ()
  (loop
      for client in %lsp-clients%
      do (lsp-shutdown-client client))
  (when %lsp-socket%
    (when *lsp-debug-p*
      (format t "lsp-shutdown(): shutting down server socket.~%"))
    (unless (eq mp:*current-process* %lsp-server%)
      (let ((process %lsp-server%))
        (setf %lsp-server% nil)
        (ignore-errors (mp:process-kill process))))
    (ignore-errors (socket:shutdown %lsp-socket% :output))
    (ignore-errors (socket:shutdown %lsp-socket% :input))
    (ignore-errors (close %lsp-socket%)))
  (setf %lsp-socket% nil))

#-:clisp
(defun lsp-shutdown-client (client)
  (cond
   ((numberp client)
    (loop
        for match in %lsp-clients%
        when (= client (client-id match)) do (lsp-shutdown-client match)))
   ((client-p client)
    (setf %lsp-clients% (delete client %lsp-clients%))
    (let ((process (client-process client)))
      (unless (eq mp:*current-process* process)
        (mp:process-kill process))
      (when *lsp-debug-p*
        (format 
         t 
         "lsp-shutdown(): shutting down client # ~a~%"
         (client-id client))
        (ignore-errors (force-output (client-stream client))
        (ignore-errors (close (client-stream client)))))))))

#-:clisp 
(defun lsp-server (&key wait)
  (if wait
    (unwind-protect
      (loop
          initially (lsp-initialize)
          while %lsp-socket%
          for i from 0
          for stream = (socket:accept-connection %lsp-socket% :wait t)
          when *lsp-debug-p* do
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
                    #-:clisp
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

(defun lsp-read (id stream)
  (loop
      with *package* = (find-package :lkb)
      with size = 2048
      with buffer = (make-array size :element-type 'character
                                :adjustable nil :fill-pointer 0)
      for n from 1
      for c = (read-char stream nil nil)
      when (null c) do 
        (format
         t
         "[~a] lsp-read(): premature end of file (read ~a characters)~%" 
         id n)
        (return)
      when (= n size) do
        (incf size size)
        (setf buffer (adjust-array buffer size))
      when (char= c #\page) do
        (return buffer)
      while c do (vector-push c buffer)))

(defun lsp-process-event (id event stream)
  #+:lui
  (declare (special %lui-stream% %lui-eoc%))
  (let* ((client  (loop
                      for client in %lsp-clients%
                      when (= (client-id client) id) return client))
         #+:clim
         (display clim:*default-server-path*)
         #+:clim
         (clim:*default-server-path* 
          (if client (client-display client) display))
         (command (lsp-parse-command id event))
         (waitp (eq (first command) 'wait))
         (return %lsp-ok%))

    (when (null command)
      (format stream "~a~c~%" %lsp-syntax-error% #\page)
      (force-output stream)
      (return-from lsp-process-event))

    (when waitp (pop command))

    (when *lsp-debug-p*
      (format 
       t 
       "[~a] lsp-process-event(): received: `~(~a~)' command~@[ (wait)~].~%" 
       id (first command) waitp))
    
    (unless (or waitp (null id))
      (format stream "~a~c~%" return #\page)
      (force-output stream))
    
    (multiple-value-bind (foo condition)
      (ignore-errors
       (case (pop command)
         (tsdb 
          (if (find-package :tsdb)
            (let* ((symbol (find-symbol "LSP-PROCESS-EVENT" :tsdb))
                   (function (when (fboundp symbol) 
                               (symbol-function symbol))))
              (if function
                (setf return
                  (funcall function id command (when waitp stream)))
                (setf return %lsp-invalid-module%)))
            (setf return %lsp-invalid-module%)))
         (mrs 
          (if (find-package :mrs)
            (let* ((symbol (find-symbol "LSP-PROCESS-EVENT" :mrs))
                   (function (when (fboundp symbol) 
                               (symbol-function symbol))))
              (if function
                (setf return 
                  (funcall function id command (when waitp stream)))
                (setf return %lsp-invalid-module%)))
            (setf return %lsp-invalid-module%)))
         (display
          (when client
            (setf (client-display client)
              (if (and (stringp (first command))
                       (> (length (first command)) 0))
                (list (first (client-display client)) :display (first command))
                #+:clim
                display))
            (when *lsp-debug-p*
              (format
               t
               "[~d] lsp-process-event(): new DISPLAY is `~a'.~%"
               id (or (third (client-display client)) "local")))))
         (grammar
          (let ((script (pop command)))
            (when (probe-file script)
              #+:lui
              (unless id (format %lui-stream% "status loading~a~%" %lui-eoc%))
              (read-script-file-aux script)))
          #+:lui
          (unless id (format %lui-stream% "status ready~a~%" %lui-eoc%)))
         (parse
          (let* ((input (pop command))
                 (set (let ((foo (pop command))) 
                        (and foo (intern (string foo) :keyword))))
                 (format (let ((foo (pop command))) 
                           (and foo (intern (string foo) :keyword))))
                 (view (let ((foo (pop command))) 
                         (and foo (intern (string foo) :keyword)))))
            (when (stringp input)
              (setf *sentence* input)
              (unless id 
                #+:lui
                (lui-status (format nil "parsing `~a' ..." input)))
              (parse (split-into-words 
                      (preprocess-sentence-string 
                       (string-trim '(#\space #\tab #\newline) input)))
                     nil)
              (when *parse-record*
                (setf *parse-record* (nreverse *parse-record*))
                (let ((edges (if (eq set :best)
                               (list (first *parse-record*))
                               *parse-record*)))
                  (unless id 
                    #+:lui
                    (lui-status (format 
                                 nil 
                                 " done (~d tree~p; ~,2f seconds)~%"
                                 (length *parse-record*) 
                                 (length *parse-record*)
                                 (/ (first *parse-times*) 100000))))
                  (if (eq view :browse)
                    (lsp-browse id input edges format)
                    (when waitp (lsp-return id stream edges format)))))
              (when waitp
                (format stream " ~a" (length *parse-record*))))))
         (browse
          (pop command)
          (let* ((location (pop command))
                 (format (let ((foo (pop command)))
                           (and foo (intern (string foo) :keyword))))
                 (object (lsp-retrieve-object id location)))
            (when (and (second object) (member format '(:avm :tree)))
              (lsp-browse id (first object) (rest object) format))))
         (t
          (setf return %lsp-invalid-command%))))
      (declare (ignore foo))
      (when condition
        (when *lsp-debug-p*
          (format
           t
           "[~d] lsp-process-event(): ~a~%" 
           id condition))
        (setf return %lsp-mysterious-error%)))
    (when waitp (format stream " ~a~c~%" return #\page))
    (force-output stream)))

(defun lsp-parse-command (id string)
  (loop
      with *package* = (find-package :lkb)
      with *readtable* = (lsp-make-readtable)
      with stream = (make-string-input-stream string)
      for form = (ignore-errors (read stream nil :eof))
      when (null form) do
        (when *lsp-debug-p*
          (format 
           t 
           "[~a] lsp-parse-command(): parse error in `~a'~%"
           id string))
        (return)
      while (not (eq form :eof)) collect form))

(defun lsp-make-readtable ()
  (copy-readtable))

(defun lsp-find-client (id)
  (loop
      for client in %lsp-clients%
      when (equal id (client-id client)) return client))

(defun lsp-store-object (id input object &key globalp)
  (let ((n %lsp-object-counter%))
    (setf (aref %lsp-attic% n) (cons (if globalp -1 id) (list input object)))
    (incf %lsp-object-counter%)
    (when (>= %lsp-object-counter% (array-total-size %lsp-attic%))
      (setf %lsp-attic% (adjust-array %lsp-attic% (* %lsp-object-counter% 2))))
    n))

(defun lsp-retrieve-object (id n)
  (when (and (numberp n) (< n (array-total-size %lsp-attic%)))
    (let ((bucket (aref %lsp-attic% n)))
      (when (or (equal (first bucket) -1) (equal (first bucket) id))
        (rest bucket)))))

(defun lsp-browse (id input edges format &key title)
  (let ((title (or title (format nil "`~a'~@[ [LSP # ~a]~]" input id))))
    (when (eq format :tree)
      (show-parse edges title)
      (return-from lsp-browse))
    (loop
        for edge in edges
        when (null edge) do
          (format
           t
           "[~a] lsp-browse(): null edge for `~a' (~(~a~))~%"
           id input format)
        else do
          (case format
            (:avm
             (display-fs edge title))
            #+:mrs
            ((:mrs :indexed :prolog :scoped :rmrs :dependencies)
             (let ((mrs (mrs::extract-mrs edge)))
               (case format
                 (:mrs
                  (show-mrs-window nil mrs title))
                 (:indexed 
                  (show-mrs-indexed-window nil mrs title))
                 (:prolog 
                  (show-mrs-prolog-window nil mrs title))
                 (:scoped 
                  (show-mrs-scoped-window nil mrs title))
                 (:rmrs 
                  (show-mrs-rmrs-window nil mrs title))
                 (:dependencies 
                  (show-mrs-dependencies-window nil mrs title)))))))))

(defun lsp-return (id stream edges format)

  (loop
      with *package* = (find-package :lkb)
      for edge in edges
      when (null edge) do
        (format
         t
         "[~a] lsp-return(): null edge (~(~a~))~%"
         id format)
      else do
        (case format
          (:avm
           (let* ((dag (tdfs-indef (edge-dag edge)))
                  (string (with-output-to-string (stream)
                            (display-dag1 dag 'compact stream))))
             (format stream " ~s" string)))
          (:tree
           (let* ((*package* (find-package :lkb))
                  (string (with-standard-io-syntax
                            (write-to-string 
                             (parse-tree-structure edge)))))
             (format stream " ~s" string) ))
          #+:mrs
          ((:mrs :indexed :prolog :scoped :rmrs :dependencies)
           (when (eq format :mrs) (setf format :simple))
           (let* ((format (intern (string format) :mrs))
                  (mrs (mrs::extract-mrs edge))
                  (string (with-output-to-string (stream)
                            (mrs::output-mrs1 mrs format stream))))
             (format stream " ~s" string))))))
