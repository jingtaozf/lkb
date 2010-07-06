;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LKB -*-

;;; Copyright (c) 2000--2006
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Ben Waldron;
;;;   see `LICENSE' for conditions.

(in-package :fspp)

;;
;; MISC CODE
;;

(defun 2-str (x)
  (cond
   ((stringp x) x)
   ((symbolp x) (symb-2-str x))
   ((numberp x) (num-2-str x))
   ((pathnamep x) (namestring x))
   (t (error "unhandled type"))))

(defun symb-2-str (symb)
  (unless (symbolp symb)
    (error "symbol expected"))
  (cond
   ((null symb) "")
   (t (string-downcase (string symb)))))

(defun num-2-str (num)
  (if (null num)
      (return-from num-2-str))
  (unless (numberp num)
    (error "number expected"))
  (format nil "~a" num))
