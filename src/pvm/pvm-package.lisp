(in-package "COMMON-LISP-USER")

(defpackage "PVM"
  (:use "COMMON-LISP" "FF" "MAKE")
  (:export
   "%PVM_NO_PARENT%" "%PVM_TASK_FAIL%" "%PVM_LISP_MESSAGE%"  "%PVM_C_MESSAGE%"
   "*PVM-DEBUG-P*" "*PVM-PENDING-EVENTS*" 
   "MAKE-CPU" "CPU-P" "CPU-HOST" "CPU-SPAWN" "CPU-OPTIONS"
   "CPU-ARCHITECTURE" "CPU-CLASS" "CPU-THRESHOLD" 
   "CPU-CREATE" "CPU-COMPLETE"
   "MAKE-CLIENT" "CLIENT-P" "CLIENT-TID" "CLIENT-CPU" "CLIENT-TASK"
   "CLIENT-PROTOCOL" "CLIENT-FORM" "CLIENT-STATUS" "CLIENT-LOAD"
   "MAKE-TMP-FILE" "LOAD_AVERAGE"
   "PVM_REGISTER" "PVM_QUIT" "PVM_START" "PVM_KILL" "PVM_PARENT" 
   "PVM_CREATE" 
   "PVM_POLL" "PVM_TRANSMIT" 
   "PVM_TIDTOHOST" "PVM_VM_INFO" "PVM_TASK_INFO" 
   "MESSAGE-P" 
   "MESSAGE-TAG" "MESSAGE-REMOTE" "MESSAGE-LOAD" 
   "MESSAGE-CONTENT" "MESSAGE-CORPSE" 
   "SHUTDOWN" 
   "TID-STATUS" "REVALUATE"))
