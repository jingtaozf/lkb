;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: MAKE -*-

(in-package :common-lisp-user)

(defpackage :mt
  (:use :common-lisp #-:ecl :make #+mcl :ccl) 
  (:export 
   "%TRANSFER-PROPERTIES-ACCUMULATOR%" "*VPMS*" 
   "INITIALIZE-TRANSFER" "ACTIVATE-TRANSFER"
   "READ-TRANSFER-RULES" "READ-TRANSFER-TYPES" "READ-VPM" 
   "TRANSFER-MRS" "MAP-MRS" "PARSE-INTERACTIVELY"
   "FRAGMENTP" "GENERATE-FROM-FRAGMENTED-MRS"
   "*SEMIS*" "*SEMI-TEST*" "SEMI-P"
   "READ-SEMI" "CONSTRUCT-SEMI" "PRINT-SEMI"
   "SEMI-LOOKUP" 
   "SEMI-COMPARE-PREDICATES" "SEMI-COMPATIBLE-PREDICATES"
   "TEST-SEMI-COMPLIANCE"
   "TEST-INTEGRITY"
   "UTOOL-PROCESS" "UTOOL-NET-P")) 

(eval-when #+:ansi-eval-when (:load-toplevel :compile-toplevel :execute)
           #-:ansi-eval-when (load eval compile)

  (loop
      for foo being each external-symbol in :mt
      do
        ;;
        ;; _fix_me_
        ;; avoid Allegro CL `unused variable' compiler warning. (8-oct-03; oe)
        ;;
        (setf foo foo)
        (shadowing-import foo :common-lisp-user)))
