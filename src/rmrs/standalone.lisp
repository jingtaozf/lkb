(in-package :common-lisp-user)

(defpackage :mrs 
  (:use :common-lisp)
  (:export ))


(in-package :mrs)

;;; defines structures also defined in the main MRS code, so the RMRS
;;; can be run standalone

(defstruct ep
  sort  ; relation name
  flist)


(defstruct (var)
  name
  type
  extra ; useful for e.g. agreement values
  id)
