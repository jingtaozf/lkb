;;; Copyright (c) 1998-2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Benjamin Waldron;
;;;   see `licence.txt' for conditions.

(in-package :common-lisp-user)

(defpackage :lkb 
  (:use :common-lisp #-:ecl :make #+mcl :ccl #+:preprocessor :fspp) 
  (:export
  "READ-SCRIPT-FILE-AUX" "RELOAD-SCRIPT-FILE"
  "RELOAD-LEAF-FILES" "RELOAD-LEX-FILES" "RELOAD-GRAMMAR-RULES"
  "RELOAD-LEXICAL-RULES" "RELOAD-TEMPLATE-FILES" "RELOAD-PSORT-FILES"
  "SHOW-TYPE-SPEC-TTY" "SHOW-TYPE-TTY" "SHOW-LEX-TTY" "SHOW-WORDS-TTY"
  "SHOW-GRAMMAR-RULE-TTY" "SHOW-LEX-RULE-TTY" "DO-PARSE-TTY" "SHOW-PARSE"
  "PRINT-CHART" "PARSE-SENTENCES" "APPLY-LEX-TTY" "APPLY-LEX-RULES-TTY"
  "INDEX-FOR-GENERATOR" "DO-GENERATE-TTY" "SHOW-GEN-RESULT" 
  "PRINT-GEN-CHART" "CLEAR-NON-PARENTS"
  #+:lui "LUI-INITIALIZE" #+:lui "LUI-SHUTDOWN")) 

(eval-when #+:ansi-eval-when (:load-toplevel :compile-toplevel :execute)
           #-:ansi-eval-when (load eval compile)
  (loop
      for foo being each external-symbol in :lkb
      do
        (shadowing-import foo :common-lisp-user)))



