;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: TSDB -*-

;;;
;;; [incr tsdb()] --- Competence and Performance Profiling Environment
;;; Copyright (c) 1996 -- 2005 Stephan Oepen (oe@csli.stanford.edu)
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as published by
;;; the Free Software Foundation; either version 2.1 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
;;; License for more details.
;;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file:
;;;      module:
;;;     version:
;;;  written by:
;;; last update:
;;;  updated by:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :tsdb)

(defparameter *pvm-master* nil)

(defparameter *pvm-protocol* 1)

(defun client-idle-p (client)
  (eq (client-status client) :ready))

(defun kill-client (client &key (prefix "") (stream *tsdb-io*))
  (if (client-p client)
    (let ((tid (client-tid client)))
      (pvm_kill tid))
    (format
     stream
     "~&~akill-client(): ignoring invalid client `~a'.~%"
     prefix client)))

(defun initialize-cpus (&key cpus
                             (classes '(:all))
                             (reset t)
                             host task
                             count
                             block
                             wait
                             (file
                              (format nil "/tmp/pvm.debug.~a" (current-user))
                              filep)
                             (prefix "")
                             (stream *tsdb-io*))

  (initialize-tsdb)
  (when reset
    (setf *pvm-clients*
      (cond
        ((eq reset :class)
         (loop
             with classes = (if (consp classes) classes (list classes))
             with allp = (member :all classes :test #'eq)
             for client in *pvm-clients*
             for cpu = (client-cpu client)
             for class = (let ((class (cpu-class cpu)))
                           (if (listp class) class (list class)))
             when (or allp (intersection class classes))
             do (kill-client client)
             else collect client))
        ;;
        ;; _fix_me_
        ;; add :task mode, killing off everything that intersects in its :task
        ;; slot with one of the target cpus identified by .classes.
        ;;                                                      (4-jul-04; oe)
        ((eq reset t)
         (pvm_quit)
         (pvm_start :user (current-user))
         (sleep 2)
         (setf *pvm-master* (pvm_register file *pvm-debug-p*))
         nil))))
  (when (and filep (null reset))
    (format
     stream
     "~&~ainitialize-cpus(): ignoring (protocol) `file' argument (no reset).~%"
     prefix ))
  ;;
  ;; first, create as many clients as we have cpus ...
  ;;
  (loop
      with tag = (gensym)
      with result with encoding
      with classes = (if (consp classes) classes (list classes))
      with allp = (member :all classes :test #'eq)
      for cpu in (or cpus *pvm-cpus*)
      for class = (let ((class (cpu-class cpu)))
                    (if (listp class) class (list class)))
      for tasks = (let ((task (cpu-task cpu)))
                    (if (listp task) task (list task)))
      for node = (if (stringp host) host (cpu-host cpu))
      for delay = (if (numberp wait) wait (cpu-wait cpu))
      do
        (loop
            for i from 1 to (or count 1)
            for tid = (when (and (or allp (intersection class classes))
                                 (or (null task)
                                     (member task tasks :test #'eq)))
                        ;;
                        ;; _fix_me_
                        ;; per-cpu encodings are not really well-defined when
                        ;; running multiple cpus: while poll()ing for messages,
                        ;; in principle, we would have to allow receiving
                        ;; messages with arbitrary encodings, unless we require
                        ;; all active clients to use the same encoding, which
                        ;; seems reasonable in most settings.  to work around
                        ;; this constraint, we would have to provide a list of
                        ;; active clients to the PVM layer (pvm_poll() and
                        ;; friends), for each incoming message find its client
                        ;; definition based on the remote tid, and then invoke
                        ;; native-to-string() accordingly. 
                        ;;                                      (21-sep-07; oe)
                        (when (and (cpu-encoding cpu)
                                   (not (eq (cpu-encoding cpu)
                                            *pvm-encoding*)))
                          (if (null encoding)
                            (format
                             stream
                             "~ainitialize-cpus(): ~
                              changing *pvm-encoding* to ~(~a~).~%"
                             prefix
                             (setf encoding
                               (setf *pvm-encoding* (cpu-encoding cpu))))
                            (format
                             stream
                             "~ainitialize-cpus(): ~
                              ignoring cpu encoding ~(~a~).~%"
                             prefix (cpu-encoding cpu))))
                        (pvm_create
                         (cpu-spawn cpu) (cpu-options cpu)
                         :host node :architecture (cpu-architecture cpu)))
            for ptask = (when (and (integerp tid) (> tid 0)) (tid-status tid))
            when (and tid (null ptask))
            do
              (format
               stream
               "~ainitialize-cpus(): `~a' communication error <~x>.~%"
               prefix node tid)
            when ptask
            do
              (let* ((cpu (let ((cpu (copy-cpu cpu)))
                            (when task (setf (cpu-task cpu) (list task)))
                            cpu))
                     (client (make-client
                              :tid tid :task ptask :cpu cpu :host node
                              :status (list :start tag (get-universal-time)))))
              (push client *pvm-clients*)
              (push client result)
              (when block
                (wait-for-clients
                 :block tid :wait delay :prefix prefix :stream stream))))
      finally
        ;;
        ;; ... then, wait for them to register (start talking) with us.
        ;;
        (unless block
          (wait-for-clients :wait delay :prefix prefix :stream stream))
        ;;
        ;; attempt to shut down clients that we attempted to start but somehow
        ;; failed to bring (fully) on-line.
        ;;
        (setf *pvm-clients*
          (loop
              for client in *pvm-clients*
              for status = (client-status client)
              when (and (consp status)
                        (eq (first status) :start)
                        (eq (second status) tag))
              do (kill-client client)
              else collect client))
        (return (intersection *pvm-clients* result))))

(defun wait-for-clients (&key block wait (prefix "  ") (stream *tsdb-io*))
  (loop
      while (loop
                with now = (get-universal-time)
                for client in *pvm-clients*
                for status = (client-status client)
                thereis (and (consp status)
                             (eq (first status) :start)
                             (or (null wait)
                                 (< (- now (third status)) wait))))
      for message = (pvm_poll (or block -1) -1 1)
      when (message-p message)
      do
        (let* ((tag (message-tag message))
               (remote (message-remote message))
               (content (message-content message))
               (client (find remote *pvm-clients* :key #'client-tid)))
          (cond
           ((eql tag %pvm_task_fail%)
            (let* ((remote (message-corpse message))
                   (client (find remote *pvm-clients* :key #'client-tid)))
              (when (and (client-p client) (cpu-p (client-cpu client)))
                (format
                 stream
                 "~&~await-for-clients(): client exit on `~a' <~x>~%"
                 prefix (client-host client) remote))
              (setf *pvm-clients* (delete client *pvm-clients*))))
           
           ((null client)
            (when *pvm-debug-p*
              (format
               stream
               "~&~await-for-clients(): ~
                ignoring message from alien <~x>:~%~s~%~%"
               prefix remote message)
              (force-output)))
           
           ((eql tag %pvm_lisp_message%)
            (cond
             ((and (eq (first content) :register)
                   (eq (second content) (client-tid client)))
              (multiple-value-bind (minutes seconds)
                  (floor
                   (- (get-universal-time) (third (client-status client)))
                   60)
                (setf (client-status client) :ready)
                (setf (client-protocol client) (third content))
                (format
                 stream
                 "~await-for-clients(): ~
                  `~a' registered as tid <~x> [~2,'0d:~2,'0d].~%"
                 prefix (client-host client) (client-tid client)
                 minutes seconds))
              (when (and block (eql (client-tid client) block))
                (return-from wait-for-clients client)))
             (t
              (when *pvm-debug-p*
                (format
                 stream
                 "~&~await-for-clients(): ~
                  ignoring unexpected message from <~x>:~%~s~%~%"
                 prefix remote message)
                (force-output)))))

           (t
            (when *pvm-debug-p*
              (format
               stream
               "~&~await-for-clients(): ~
                ignoring dubious message from <~x>:~%~s~%~%"
               stream remote message)
              (force-output)))))))

(defun expire-clients (clients)
  (loop
      with now = (get-universal-time)
      for client in clients
      for status = (client-status client)
      for cpu = (client-cpu client)
      when (and (consp status) cpu (numberp (cpu-quantum cpu))
                (> (- now (first status)) (cpu-quantum cpu)))
      do (kill-client client)))

(defun evaluate (form)
  (eval form))

(defun slave (&optional orphan &key self master)

  (initialize-tsdb)
  (let* ((self (or self (pvm_register t *pvm-debug-p*)))
         (master (or master (if orphan nil (pvm_parent))))
         (*package* (find-package "TSDB"))
         (*print-readably* nil))

    (unless (and (not orphan)
                 (or (= master %pvm_no_parent%)
                     (<= master 0)))
      (when master
        (pvm_transmit
         master %pvm_lisp_message%
         (list :register self :lisp)))
      (when *pvm-debug-p*
        (format 
         t 
         "slave(): tid <~x>~@[ (parent <~x>)~] waiting for requests.~%" 
         self master master)
        (force-output))
      (loop 
          for message = (pvm_poll -1 -1 1)
          while (or (null message)
                    (and (not (eq message :error))
                         (not (eql (message-tag message) 
                                   %pvm_task_fail%))))
          when message
          do
            (let* ((tag (message-tag message))
                   (content (message-content message))
                   (action (when (eq tag %pvm_lisp_message%)
                             (first content)))
                   (key (when action (second content)))
                   (form (when action (third content))))
              (when *pvm-debug-p*
                (format
                 t
                 "slave(): received `~a'.~%" message)
                (force-output))
              (cond
               ((= tag %pvm_lisp_message%)
                (case action
                  (:eval
                   (multiple-value-bind (result condition)
                       (ignore-errors (evaluate form))
                     (pvm_transmit
                      (or master (message-remote message)) 
                      %pvm_lisp_message%
                      (if (and (null result) condition)
                        (list :error key (format nil "~a" condition))
                        (list :return key result)))))
                   (:shutdown
                    (pvm_quit)
                    (excl:exit (if (eq tag %pvm_task_fail%) 1 0) 
                               :no-unwind t :quit t))))
               ((= tag %pvm_task_fail%)
                (pvm_quit)
                (excl:exit (if (eq tag %pvm_task_fail%) 1 0) 
                           :no-unwind t :quiet t))
               (t
                (format
                 t
                 "slave(): unexpected message: `~a'~%" message)
                (force-output)))))
      (pvm_quit)
      (sleep 1)
      (excl:exit 1 :no-unwind t :quiet t))))

(defun pvm-process (item &optional (type :parse)
                    &key class
                         (trees-hook :local)
                         (semantix-hook :local)
                         (exhaustive *tsdb-exhaustive-p*)
                         (nanalyses *tsdb-maximal-number-of-analyses*)
                         (nresults 
                          (if *tsdb-write-passive-edges-p*
                            -1
                            *tsdb-maximal-number-of-results*)) 
                         (filter *process-suppress-duplicates*)
                         (i-id 0) (parse-id 0)
                         result-id
                         (wait 5))

  ;;
  ;; zero out :edge or :tree fields, if any, since they are not remote readable
  ;;
  (when (listp item)
    (loop
        for result in (get-field :results item)
        for edge = (assoc :edge result)
        for tree = (assoc :tree result)
        when edge do (setf (rest edge) nil)
        when (and nil tree) do (setf (rest tree) nil)))

  (let* ((item (if (stringp item)
                 (pairlis '(:i-id :parse-id :i-input) 
                          (list i-id parse-id item))
                 item))
         (client (allocate-client item :task type :class class :wait wait))
         (cpu (and client (pvm::client-cpu client)))
         (tid (and client (client-tid client)))
         (protocol (and client (client-protocol client)))
         (tagger (when (cpu-p cpu) (cpu-tagger cpu)))
         (p-input (when (eq type :parse)
                    (let ((input (get-field :i-input item)))
                      (cond
                       ((and (cpu-p cpu) (cpu-preprocessor cpu))
                        (call-hook
                         (cpu-preprocessor cpu) input
                         (when (consp tagger) tagger)))
                       (*tsdb-preprocessing-hook*
                        (call-hook
                         *tsdb-preprocessing-hook* input
                         (when (consp tagger) tagger)))))))
         (item (acons :p-input p-input item))
         (status (if tid 
                   (case protocol
                     (:raw
                      (process_item tid item nanalyses nresults nil))
                     (:lisp
                      (revaluate 
                       tid 
                       `(process-item
                         (quote ,item)
                         :type ,type
                         :result-id ,result-id
                         :exhaustive ,exhaustive
                         :nanalyses ,nanalyses
                         :nresults ,nresults :filter (quote ,filter)
                         :trees-hook ,trees-hook :semantix-hook ,semantix-hook
                         :verbose nil :interactive nil :burst t)
                       nil
                       :key :process-item
                       :verbose nil)))
                   :null))
         (item 
          (case status
            (:ok 
             (let ((status (process-queue nil :client client)))
               (if (rest (assoc :pending status))
                 (pairlis '(:readings :error)
                          (list -1 
                                (format nil "PVM client exit <~x>" tid)))
                 ;;
                 ;; _fix_me_
                 ;; this is how things used to be in the web demo; is it really
                 ;; necessary to put the original item back on?  (3-jul-04; oe)
                 ;;
                 (append (rest (assoc :result status)) 
                         (when (eq type :parse) item)))))
            (:error 
             (setf (client-status client) :error)
             (pairlis '(:readings :error)
                      (list 
                       -1 (format nil "PVM internal error <~x>" tid))))
            (:null
             (pairlis '(:readings :error)
                      (list 
                       -1 
                       (format 
                        nil 
                        "maximum number of active sessions exhausted"))))))
         (results (get-field :results item)))
    ;;
    ;; _fix_me_
    ;; so, why not invoke the full enrich-result() here?        (10-oct-08; oe)
    ;;
    (when results
      (nconc item (acons :unique (length results) nil))
      (setf (get-field :results item) results))
    item))

(defun remote-grammar (task)
  (let ((translator (loop
                        for client in *pvm-clients*
                        for cpu = (client-cpu client)
                        when (and (eq (client-status client) :ready)
                                  (smember :translate (cpu-task cpu)))
                        return client)))
    (if translator 
      (revaluate (client-tid translator) `(remote-grammar ,task))
      (loop
          for client in *pvm-clients*
          for cpu = (client-cpu client)
          when (smember task (cpu-task cpu))
          return (cpu-grammar cpu)))))

(defun client-evaluate (tid form 
                        &optional (block t) 
                        &key (verbose t) (key 0) interrupt)
  (if (keywordp tid)
    (loop
        for client in *pvm-clients*
        for cpu = (client-cpu client)
        when (or (eq tid :all)
                 (null (cpu-task cpu))
                 (smember tid (cpu-task cpu)))
        collect (revaluate
                 (client-tid client) form block
                 :verbose verbose :key key :interrupt interrupt))
    (revaluate
     tid form block
     :verbose verbose :key key :interrupt interrupt)))
