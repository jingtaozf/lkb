;;; Copyright (c) 1998--2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

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
   "PSOA-TOP-H" "PSOA-INDEX" "PSOA-LISZT" "PSOA-H-CONS"
   "REL-PRED" "REL-HANDEL" "REL-FLIST" "REL-EXTRA"
   "FVPAIR-FEATURE" "FVPAIR-VALUE"
   "VAR-TYPE" "VAR-EXTRA" "VAR-ID"
   "HCONS-SCARG" "HCONS-OUTSCPD" "HCONS-RELATION"
   "VSYM"))
