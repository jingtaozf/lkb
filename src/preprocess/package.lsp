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
           #:x-read-preprocessor
           #:x-preprocess
           #:x-clear-preprocessor
           #:*local-to-global-point-mapping*
           #:*preprocessor*
	   ))
