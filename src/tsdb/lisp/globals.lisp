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

(defparameter *tsdb-version* "1.8 (13-jun-02)")

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
  (namestring (dir-append 
               (get-sources-dir "tsdb")
               '(:relative "tsdb" "skeletons" "english"))))

(defparameter *tsdb-data* "toy")

(defparameter *tsdb-protocol-file * nil)

(defparameter *tsdb-server-mode-p* nil)

(defparameter *tsdb-io* t)

(defparameter *tsdb-write-run-p* t)

(defparameter *tsdb-write-parse-p* t)

(defparameter *tsdb-write-result-p* t)

(defparameter *tsdb-write-passive-edges-p* nil)

(defparameter *tsdb-write-lexicon-chart-p* nil)

(defparameter *tsdb-write-syntax-chart-p* nil)

(defparameter *tsdb-write-output-p* nil)

(defparameter *tsdb-rule-statistics-p* nil)

(defparameter *tsdb-verbose-processing-p* t)

(defparameter *tsdb-cache-database-writes-p* :raw)

(defparameter *tsdb-flush-cache-threshold* 5000)

(defparameter *tsdb-verbose-cache-flush-p* nil)

(defparameter *tsdb-preprocessing-hook* nil)

(defparameter *tsdb-result-hook* "tsdb::result-hook")

(defparameter *tsdb-trees-hook* nil)

(defparameter *tsdb-semantix-hook* nil)

(defparameter *tsdb-gc-p* nil)

(defparameter *tsdb-minimize-gcs-p* t)

(defparameter *tsdb-tenure-p* nil)

(defparameter *tsdb-generation-spread* 20)

(defparameter *tsdb-scavenge-limit* 402653184)

(defparameter *tsdb-tenured-bytes* 0)

(defparameter *tsdb-tenured-bytes-limit* 20971520)

(defparameter *tsdb-gc-verbosity* nil)

(defparameter *tsdb-gc-message-p* t)

(defparameter *tsdb-gc-cursor-p* t)

(defparameter *tsdb-edge-factor* 2.0)

(defparameter *tsdb-exhaustive-p* t)

(defparameter *tsdb-ignore-output-p* nil)

(defparameter *tsdb-maximal-number-of-edges* 0)

(defparameter *tsdb-default-skeleton* "english")

(defparameter *tsdb-skeleton-index* "Index.lisp")

(defparameter *tsdb-relations-skeleton* "Relations")

(defparameter *tsdb-instance-template* "%g/%v/%t/%d/%s")

(defparameter *tsdb-skeletons* nil)

(defparameter *tsdb-initialized-p* nil)

(defvar *tsdb-phenomena* (make-hash-table :test #'equal))

(defparameter *tsdb-gc-statistics* nil)

(defparameter *tsdb-ofs* #\@)

(defparameter *tsdb-efs* #\@)

(defparameter *tsdb-profile-files*
  '("daughter" "edge" "parse" "result" "rule" "run" 
    "tree" "decision" "preference"))

(defparameter *tsdb-id-attributes*
  '(:i-id :p-id :ip-id :s-id :run-id :parse-id :result-id))

(defparameter *tsdb-coded-attributes*
  '(:i-difficulty :i-wf :polarity))

(defparameter *tsdb-tokens-to-ignore*
  '("." "(" ")" "!" "?" "," "+" "-" "'" "[" "]" "`"))

(defparameter *tsdb-slash* #\/)

(defparameter *tsdb-debug-mode-p* nil)

(defparameter %tsdb-lexical-preterminals% nil)
