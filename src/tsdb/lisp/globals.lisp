;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: TSDB -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file:
;;;      module:
;;;     version:
;;;  written by:
;;; last update:
;;;  updated by:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "TSDB")

(defparameter *tsdb-name* "[incr tsdb()]")

(defparameter *tsdb-version* "1.5 (21-feb-99)")

(defparameter
  *tsdb-application*
  (format
    nil "exec ~a"
    (namestring (make-pathname :directory (pathname-directory make::bin-dir)
                               :name "tsdb"))))

(defparameter 
  *tsdb-home* 
  (namestring (dir-append (get-sources-dir "tsdb")
                          '(:relative "tsdb" "home"))))

(defparameter 
  *tsdb-skeleton-directory* 
  (namestring (dir-append (get-sources-dir "tsdb")
                          '(:relative "tsdb" "skeletons" "english"))))

(defparameter *tsdb-data* "toy")

(defparameter *tsdb-protocol-file * nil)

(defparameter *tsdb-server-mode-p* nil)

(defparameter *tsdb-io* t)

(defparameter *tsdb-write-run-p* t)

(defparameter *tsdb-write-parse-p* t)

(defparameter *tsdb-write-result-p* t)

(defparameter *tsdb-write-lexicon-chart-p* nil)

(defparameter *tsdb-write-syntax-chart-p* nil)

(defparameter *tsdb-write-output-p* nil)

(defparameter *tsdb-rule-statistics-p* #+:page t #-:page nil)

(defparameter *tsdb-cache-database-writes-p* t)

(defparameter *tsdb-flush-cache-threshold* 5000)

(defparameter *tsdb-verbose-cache-flush-p* nil)

(defparameter *tsdb-trees-hook* nil)

(defparameter *tsdb-semantix-hook* nil)

(defparameter *tsdb-gc-p* nil)

(defparameter *tsdb-minimize-gcs-p* t)

(defparameter *tsdb-tenure-p* nil)

(defparameter *tsdb-gc-verbosity* nil)

(defparameter *tsdb-gc-message-p* t)

(defparameter *tsdb-edge-factor* 2.0)

(defparameter *tsdb-exhaustive-p* t)

(defparameter *tsdb-ignore-output-p* nil)

(defparameter *tsdb-maximal-number-of-edges* 0)

(defparameter *tsdb-lexical-oracle-p* nil)

(defparameter *tsdb-phrasal-oracle-p* nil)

(defparameter *tsdb-default-skeleton* "english")

(defparameter *tsdb-skeleton-index* "Index.lisp")

(defparameter *tsdb-relations-skeleton* "Relations")

(defparameter *tsdb-skeletons* nil)

(defparameter *tsdb-initialized-p* nil)

(defvar *tsdb-phenomena* (make-hash-table :test #'equal))

(defparameter *tsdb-global-gcs* 0)

(defparameter *tsdb-profile-files*
  (list "daughter" "edge" "parse" "result" "rule" "run"))

(defparameter *tsdb-tokens-to-ignore*
  (list "." "(" ")" "!" "?" "-" "'" "[" "]" "`"))

(defparameter *tsdb-slash* #\/)

(defparameter *tsdb-gc-cursor* :pirate)

(defparameter *tenured-bytes* 0)

(defparameter *tsdb-debug-mode-p* nil)

(defparameter %tsdb-lexical-preterminals% nil)
