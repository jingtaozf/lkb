(in-package "TSDB")

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

#+(version>= 5 0)
(def-foreign-call 
    (_create_run "create_run")
    ((tid :int integer)
     (data (* :char) string)
     (run-id :int integer)
     (comment (* :char) string)
     (interactive :int integer)
     (protocol :int integer)
     (custom (* :char) string))
  :returning :int
  #+(version>= 6 0) :strings-convert #+(version>= 6 0) t)

#-(version>= 5 0)
(defforeign
    '_create_run :entry-point "create_run"
    :arguments '(integer string integer string integer integer string)
    :return-type :integer)

(defun create_run (tid data run-id comment interactive protocol custom)
  (let* ((comment (if (stringp comment) comment ""))
         (interactive (if interactive 1 0))
         (version (and (numberp protocol) (logand protocol 31)))
         (protocol (if (and (numberp protocol) (>= version 1) (<= version 2))
                     protocol
                     1))
         (custom (if (stringp custom) custom ""))
         (status (_create_run 
                  tid data run-id comment interactive protocol custom)))
    (cond
     ((zerop status) :ok)
     (t :error))))

#+(version>= 5 0)
(def-foreign-call 
    (_process_item "process_item")
    ((tid :int integer)
     (id :int integer)
     (input (* :char) string)
     (parse_id :int integer)
     (edges :int integer)
     (nanalyses :int integer)
     (nresults :int integer)
     (interactive :int integer)
     (custom (* :char) string))
  :returning :int
  #+(version>= 6 0) :strings-convert #+(version>= 6 0) t)

#-(version>= 5 0)
(defforeign
    '_process_item :entry-point "process_item"
    :arguments '(integer integer string
                 integer integer integer integer integer)
    :return-type :integer)

(defun process_item (tid item nanalyses nresults interactive custom)
  
  (let* ((i-id (get-field :i-id item))
         (i-input (or (get-field :mrs item)
                      (get-field :p-input item)
                      (get-field :i-input item)))
         (parse-id (get-field :parse-id item))
         (edges (or (get-field :edges item) 0))
         (nanalyses (if (integerp nanalyses) nanalyses 0))
         (interactive (if interactive 1 0))
         (custom (if (stringp custom) custom ""))
         (status 
          (_process_item tid i-id i-input parse-id 
                         edges nanalyses nresults interactive custom)))
    (cond
     ((zerop status) :ok)
     (t :error))))

#+(version>= 5 0)
(def-foreign-call 
    (_complete_run "complete_run")
    ((tid :int integer)
     (run_id :int integer)
     (custom (* :char) string))
  :returning :int
  #+(version>= 6 0) :strings-convert #+(version>= 6 0) t)

#-(version>= 5 0)
(defforeign
    '_complete_run :entry-point "complete_run"
    :arguments '(integer integer string)
    :return-type :integer)

(defun complete_run (tid run-id custom block interrupt)
  (let ((custom (if (stringp custom) custom "")))
    (if (< (_complete_run tid run-id custom) 0)
      :error
      (if block
        (loop
            for message = (pvm_poll tid %pvm_lisp_message% block)
            when (eq message :error)
            return :error
            when (and (message-p message)
                      (eql (message-tag message) %pvm_lisp_message%)
                      (eq (first (message-content message)) :return)
                      (eql (second (message-content message)) 
                           :complete-run))
            return (third (message-content message))
            else when (interrupt-p interrupt)
            do
              (return-from complete_run :interrupt))
        :ok))))

(let ((lock (mp:make-process-lock)))
  (defun allocate-client (item
                          &key protocol (task :parse) class flags (wait 42))
    (loop
        for i from 1 to wait do
          (loop
              for client in *pvm-clients*
              for cpu = (client-cpu client)
              when (and (or (null protocol)
                            (eq (client-protocol client) protocol))
                        (or (null task)
                            (and (eq task :parse) (null (cpu-task cpu)))
                            (eq task (cpu-task cpu))
                            (smember task (cpu-task cpu)))
                        (or (null class)
                            (eq class (cpu-class cpu))
                            (when (consp (cpu-class cpu))
                              (smember class (cpu-class cpu))))
                        (loop
                            for foo = flags then (rest (rest foo))
                            for value = (getf (cpu-flags cpu) (first foo))
                            while foo
                            always (equal value (second foo))))
              do
                (mp:with-process-lock (lock) 
                  (when (eq (client-status client) :ready)
                    (setf (client-status client)
                      (cons (get-universal-time) item))
                    (return-from allocate-client client))))
          (sleep 0.5)))
  (defun free-client (client &optional (status :ready))
    (setf (client-status client) status)))
