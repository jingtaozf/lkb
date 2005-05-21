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
                             count
                             block
                             (file (format 
                                    nil 
                                    "/tmp/pvm.debug.~a"
                                    (current-user)))
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
  (when (and file (null reset))
    (format
     stream
     "~&~ainitialize-cpus(): ignoring (protocol) `file' argument (no reset).~%"
     prefix ))
  ;;
  ;; first, create as many clients as we have cpus ...
  ;;
  (loop
      with classes = (if (consp classes) classes (list classes))
      with allp = (member :all classes :test #'eq)
      for cpu in (or cpus *pvm-cpus*)
      for class = (let ((class (cpu-class cpu)))
                    (if (listp class) class (list class)))
      do
        (loop
            for i from 1 to (or count 1)
            for tid = (when (or allp (intersection class classes))
                        (pvm_create (cpu-spawn cpu) (cpu-options cpu)
                                    :host (cpu-host cpu) 
                                    :architecture (cpu-architecture cpu)))
            for task = (when (and (integerp tid) (> tid 0)) (tid-status tid))
            when (and tid (null task))
            do
              (format
               stream
               "~ainitialize-cpus(): `~a' communication error [~d].~%"
               prefix (cpu-host cpu) tid)
            when task
            do
              (push (make-client :tid tid :task task :cpu cpu :status :start)
                    *pvm-clients*)
              (when block
                (wait-for-clients :block tid :prefix prefix :stream stream))))
  ;;
  ;; ... then, wait for them to register (start talking) with us.
  ;;
  (wait-for-clients :prefix prefix :stream stream))

(defun wait-for-clients (&key block (prefix "  ") (stream *tsdb-io*))
  (loop
      while (and *pvm-clients* 
                 (find :start *pvm-clients* :key #'client-status))
      for message = (pvm_poll -1 -1 1)
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
                 "~&~await-for-clients(): client exit for `~a' <~a>~%"
                 prefix (cpu-host (client-cpu client)) remote))
              (setf *pvm-clients* (delete client *pvm-clients*))))
           
           ((null client)
            (when *pvm-debug-p*
              (format
               stream
               "~&~await-for-clients(): ~
                ignoring message from alien <~d>:~%~s~%~%"
               prefix remote message)
              (force-output)))
           
           ((eql tag %pvm_lisp_message%)
            (cond
             ((and (eq (first content) :register)
                   (eq (second content) (client-tid client)))
              (setf (client-status client) :ready)
              (setf (client-protocol client) (third content))
              (format
               stream
               "~await-for-clients(): `~a' registered as tid <~x>.~%"
               prefix (cpu-host (client-cpu client)) (client-tid client))
              (when (and block (eql (client-tid client) block))
                (return-from wait-for-clients client)))
             (t
              (when *pvm-debug-p*
                (format
                 stream
                 "~&~await-for-clients(): ~
                  ignoring unexpected message from <~d>:~%~s~%~%"
                 prefix remote message)
                (force-output)))))

           (t
            (when *pvm-debug-p*
              (format
               stream
               "~&~await-for-clients(): ~
                ignoring dubious message from <~d>:~%~s~%~%"
               stream remote message)
              (force-output)))))))

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
         "slave(): tid ~d~@[ (parent ~d)~] waiting for requests.~%" 
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
      (excl:exit 1 :no-unwind t :quiet t))))

(defun pvm-process (item &optional (type :parse)
                    &key (exhaustive *tsdb-exhaustive-p*)
                         (nanalyses *tsdb-maximal-number-of-analyses*)
                         (nresults 
                          (if *tsdb-write-passive-edges-p*
                            -1
                            *tsdb-maximal-number-of-results*)) 
                         (i-id 0) (parse-id 0)
                         result-id
                         rankp
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
         (client (allocate-client item :task type :wait wait))
         (cpu (and client (pvm::client-cpu client)))
         (tid (and client (client-tid client)))
         (protocol (and client (client-protocol client)))
         (p-input (when (eq type :parse)
                    (let ((input (get-field :i-input item)))
                      (cond
                       ((and (cpu-p cpu) (cpu-preprocessor cpu))
                        (call-hook (cpu-preprocessor cpu) input))
                       (*tsdb-preprocessing-hook*
                        (call-hook *tsdb-preprocessing-hook* input))))))
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
                         :nresults ,nresults
                         :trees-hook :local :semantix-hook :local
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
                                (format nil "PVM client exit (tid # ~a)" tid)))
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
                       -1 (format nil "PVM internal error (tid # ~a)" tid))))
            (:null
             (pairlis '(:readings :error)
                      (list 
                       -1 
                       (format 
                        nil 
                        "maximum number of active sessions exhausted"))))))
         (results (get-field :results item))
         (results (if (and rankp (eq type :generate))
                    (let* ((strings
                            (loop
                                for result in results
                                collect (get-field :tree result)))
                           #+:lkb
                           (strings (mt::lm-score-strings strings)))
                      (loop
                          for result in results
                          for foo in strings
                          do (nconc result (acons :score (rest foo) nil)))
                      (stable-sort
                       results
                       #'< :key #'(lambda (foo) 
                                    (or (get-field :score foo) 0))))
                    results)))
    (when results
      (nconc item (acons :unique (length results) nil))
      (setf (get-field :results item) results))
    item))
