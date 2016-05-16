;;; Copyright (c) 1998--2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `LICENSE' for conditions.

(in-package :common-lisp-user)

;;;
;;; because ann used the term `type' the way she does well before it became a
;;; *censored* part of the *censored* common-lisp language |:-}.
;;;
;;; censorship by ann ...
;;;
#+:allegro 
(setf excl:*enable-package-locked-errors* nil)

#+:lispworks
(setf hcl:*packages-for-warn-on-redefinition*
  (loop
     with key = (find-package :common-lisp)
     for name in hcl:*packages-for-warn-on-redefinition*
     for package = (find-package name)
     unless (eq key package) collect name))

#+:clisp
(setf (ext:package-lock "LISP") nil)

(defpackage :mrs 
  (:use #+:lkb :lkb :common-lisp #-:ecl :make)
  (:export
   "*LNKP*" "OUTPUT-LNK"
   "*NORMALIZE-PREDICATES-P*" "NORMALIZE-PREDICATE"
   "PSOA-P" "PSOA-TOP-H" "PSOA-INDEX" "PSOA-LISZT" "PSOA-H-CONS"
   "REL-PRED" "REL-HANDEL" "REL-FLIST" "REL-EXTRA"
   "CHAR-REL-CFROM" "CHAR-REL-CTO"
   "FVPAIR-FEATURE" "FVPAIR-VALUE"
   "VAR-P" "VAR-TYPE" "VAR-EXTRA" "VAR-ID"
   "HCONS-SCARG" "HCONS-OUTSCPD" "HCONS-RELATION"
   "VSYM" "*MRS-PACKAGE*"
   "PATH-VALUE" "IS-VALID-FS" "FS-ARCS" "FS-TYPE"
   "IS-VALID-TYPE" "IS-TOP-TYPE" "EQUAL-OR-SUBTYPE" "COMPATIBLE-TYPES"
   "EDS-P" "ED-P" "EDS-READ" "READ-MRS-OR-EDS-FROM-STRING" "EDS-TO-MRS"
   "EDS-CONVERT-EDGE" "EDS-CONVERT-PSOA" "EDS-EXPLODE"
   "EDS-FRAGMENTED-P" "EDS-CYCLIC-P" "EDS-SUSPICIOUS-P"
   "EDS-OUTPUT-PSOA"
   "*EDS-INCLUDE-QUANTIFIERS-P*" "*EDS-INCLUDE-VACUOUS-RELATIONS-P*"
   "*EDS-PRETTY-PRINT-P*" "*EDS-SHOW-STATUS-P*"
   "*EDS-SHOW-PROPERTIES-P*" "*EDS-SHOW-LNK-P*"))
