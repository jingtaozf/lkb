(in-package "COMMON-LISP-USER")

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

(defpackage "PVM"
  (:use "COMMON-LISP" "FF" "MAKE")
  (:export
   "%PVM_NO_PARENT%" "%PVM_TASK_FAIL%" "%PVM_LISP_MESSAGE%"  "%PVM_C_MESSAGE%"
   "*PVM-ENCODING*" "*PVM-DEBUG-P*" "*PVM-PENDING-EVENTS*" 
   "MAKE-CPU" "CPU-P" "CPU-HOST" "CPU-SPAWN" "CPU-OPTIONS" "CPU-ENCODING"
   "CPU-ARCHITECTURE" "CPU-CLASS" "CPU-TASK" "CPU-THRESHOLD" 
   "CPU-NAME" "CPU-GRAMMAR" "CPU-TEMPLATE" 
   "CPU-PREPROCESSOR" "CPU-TAGGER" "CPU-READER"
   "CPU-CREATE" "CPU-COMPLETE"
   "MAKE-CLIENT" "CLIENT-P" "CLIENT-TID" "CLIENT-CPU" "CLIENT-TASK"
   "CLIENT-PROTOCOL" "CLIENT-FORM" "CLIENT-STATUS" "CLIENT-LOAD" "CLIENT-HOST"
   "CURRENT-USER" "CURRENT-PID" "MAKE-TMP-FILE" "LOAD_AVERAGE"
   "PVM_REGISTER" "PVM_QUIT" "PVM_HALT" "PVM_START" "PVM_KILL" "PVM_PARENT" 
   "PVM_CREATE" 
   "PVM_POLL" "PVM_TRANSMIT" 
   "PVM_TIDTOHOST" "PVM_VM_INFO" "PVM_TASK_INFO" 
   "MESSAGE-P" 
   "MESSAGE-TAG" "MESSAGE-REMOTE" "MESSAGE-LOAD" 
   "MESSAGE-CONTENT" "MESSAGE-CORPSE" 
   "SHUTDOWN" 
   "TID-STATUS" "REVALUATE"))
