;;; Copyright (c) 1998-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions

(in-package :common-lisp-user)

(defpackage :mrs 
  (:use :lkb :common-lisp :make)
  (:export
   "PSOA-TOP-H" "PSOA-INDEX" "PSOA-LISZT" "PSOA-H-CONS"
   "REL-SORT" "REL-HANDEL" "REL-FLIST" "REL-EXTRA"
   "FVPAIR-FEATURE" "FVPAIR-VALUE"
   "VAR-NAME" "VAR-EXTRA" "VAR-ID"
   "HANDLE-VAR-NAME" "HANDLE-VAR-EXTRA" "HANDEL-VAR-ID"
   "HCONS-SCARG" "HCONS-OUTSCPD" "HCONS-RELATION"))
