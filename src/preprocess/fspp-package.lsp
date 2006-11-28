;;; -*- Mode: LISP; Syntax: Common-Lisp -*-
;;; Copyright (c) 2000--2005
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Ben Waldron;
;;;   see `licence.txt' for conditions.

(in-package :cl-user)

(defpackage #:preprocessor
  (:nicknames #:fspp)
  (:use #:common-lisp)
  (:export #:read-preprocessor
           #:preprocess
           #:clear-preprocessor
           #:preprocessor-initialized-p
	   
           #:x-escape-string
	   #:x-fspp-global

           #:x-read-preprocessor ;; deprecated
           #:x-preprocess ;; deprecated
           #:x-clear-preprocessor ;; deprecated
	   
           #:*local-to-global-point-mapping*
           #:*preprocessor*
	   #:*min-regex-char-code-limit*
	   ))
