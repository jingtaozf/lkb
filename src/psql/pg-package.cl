;; -*- mode: Common-Lisp; package: USER; -*-

;; This software is Copyright (c) Marina Motion LLC, November 2001.
;; Marina Motion LLC grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :user)

(defpackage :pgsql
  (:nicknames :pg)
  (:use :common-lisp :excl :foreign-functions)
  (:export
   "CONNECT-DB" "DECODE-CONNECTION-STATUS" "STATUS" "DB"
   "ERROR-MESSAGE" "SQL" "FINISH" "SQL-ERROR"))

   

