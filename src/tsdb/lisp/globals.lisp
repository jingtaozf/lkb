;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: TSDB -*-

;;;
;;; [incr tsdb()] --- Competence and Performance Profiling Environment
;;; Copyright (c) 1996 -- 2005 Stephan Oepen (oe@csli.stanford.edu)
;;; Copyright (c) 2006 -- 2012 Stephan Oepen (oe@ifi.uio.no)
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as published by
;;; the Free Software Foundation; either version 2.1 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
;;; License for more details.
;;; 

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

(defparameter *tsdb-version* "2.0 (2-dec-15; beta)")

(defparameter
  *tsdb-application*
  (format
    nil "exec ~a"
    (namestring (make-pathname
                 :directory (pathname-directory make::bin-dir) :name "tsdb"))))

(defparameter 
  *tsdb-home* 
  (namestring (dir-append
               (get-sources-dir "tsdb") '(:relative "tsdb" "home"))))

(defparameter 
  *tsdb-skeleton-directory* 
  (namestring (dir-append 
               (get-sources-dir "tsdb")
               '(:relative "tsdb" "skeletons" "english"))))

(defparameter *tsdb-data* "toy")

(defparameter *tsdb-gold* nil)

(defparameter *tsdb-encoding* #+:logon :utf-8 #-:logon nil)

(defparameter *tsdb-protocol-file* nil)

(defparameter *tsdb-current-granularity* 201208)

(defparameter *tsdb-server-mode-p* nil)

(defparameter *tsdb-io* t)

(defparameter *tsdb-write-run-p* t)

(defparameter *tsdb-write-parse-p* t)

(defparameter *tsdb-write-result-p* t)

(defparameter *tsdb-write-tree-p* nil)

(defparameter *tsdb-write-mrs-p* nil)

(defparameter *tsdb-write-edge-p* t)

(defparameter *tsdb-write-passive-edges-p* nil)

(defparameter *tsdb-write-output-p* nil)

(defparameter *tsdb-rule-statistics-p* nil)

(defparameter *tsdb-verbose-processing-p* t)

(defparameter *tsdb-cache-database-writes-p* :raw)

(defparameter *tsdb-flush-cache-threshold* 5000)

(defparameter *tsdb-verbose-cache-flush-p* nil)

(defparameter *tsdb-preprocessing-hook* nil)

(defparameter *tsdb-tagging-hook* nil)

(defparameter *tsdb-result-hook* "tsdb::result-hook")

(defparameter *tsdb-trees-hook* nil)

(defparameter *tsdb-semantix-hook* nil)

(defparameter *tsdb-gc-p* nil)

(defparameter *tsdb-minimize-gcs-p* t)

(defparameter *tsdb-tenure-p* nil)

(defparameter *tsdb-generation-spread* 10)

(defparameter *tsdb-scavenge-limit* nil)

(defparameter *tsdb-tenured-bytes* 0)

(defparameter *tsdb-tenured-bytes-limit* (* #-:64bit 32 #+:64bit 128 1024 1024))

(defparameter *tsdb-gc-verbosity* nil)

(defparameter *tsdb-gc-message-p* t)

(defparameter *tsdb-gc-cursor-p* t)

(defparameter *tsdb-edge-factor* 2.0)

(defparameter *tsdb-exhaustive-p* t)

(defparameter *tsdb-ignore-output-p* nil)

(defparameter *tsdb-maximal-number-of-edges* 100000)

(defparameter *tsdb-maximal-number-of-analyses* 0)

(defparameter *tsdb-maximal-number-of-results* 1000)

(defparameter *tsdb-default-skeleton* "english")

(defparameter *tsdb-skeleton-index* "Index.lisp")

(defparameter *tsdb-relations-skeleton* "Relations")

(defparameter *tsdb-instance-template* "%g/%v/%t/%d/%s")

(defparameter *tsdb-skeletons* nil)

(defparameter *tsdb-initialized-p* nil)

(defvar *tsdb-phenomena* (make-hash-table :test #'equal))

(defparameter *tsdb-data-hook* nil)

(defparameter *tsdb-gold-hook* nil)

(defparameter *tsdb-gc-statistics* nil)

(defparameter *tsdb-ofs* #\@)

(defparameter *tsdb-efs* #\@)

(defparameter *tsdb-core-files*
  '("item" "analysis" "phenomenon" "parameter" "set" 
    "item-phenomenon" "item-set"))

(defparameter *tsdb-redwoods-files*
  '("tree" "decision" "preference" "update" "fold" "score"))

(defparameter *tsdb-profile-files*
  (append '("daughter" "edge" "parse" "result" "rule" "run")
          *tsdb-redwoods-files*))

(defparameter *tsdb-id-attributes*
  '(:i-id :p-id :ip-id :s-id :run-id :parse-id :result-id))

(defparameter *tsdb-coded-attributes*
  '(:i-difficulty :i-wf :polarity))

(defparameter *tsdb-tokens-to-ignore*
  '("." "(" ")" "!" "?" "," "+" "-" "'" "[" "]" "`"))

(defparameter *tsdb-slash* #\/)

(defparameter *tsdb-debug-mode-p* nil)

(defparameter *pvm-cpus* nil)

(defparameter *pvm-clients* nil)

(defparameter *process-default-task* :parse)

(defparameter *process-custom* nil)

(defparameter *process-suppress-duplicates* '(:mrs))

(defparameter *process-exhaustive-inputs-p* nil)

(defparameter *process-client-retries* 0)

(defparameter *process-scope-generator-input-p* nil)

(defparameter *process-pretty-print-trace-p* t)

(defparameter *process-raw-print-trace-p* nil)

(defparameter *process-sort-profile-p* t)

(defparameter *process-fan-out-log* nil)

(defparameter *process-fan-out-xml* nil)

(defparameter *statistics-select-condition* nil)

(defparameter *redwoods-export-values* 
  '(:derivation :tree :avm :mrs :eds))

(defparameter *redwoods-update-exact-p* nil)

(defparameter *redwoods-update-flag-p* t)

(defparameter *redwoods-agreement-exact-p* t)

(defparameter *redwoods-score-all-p* nil)

(defparameter *redwoods-score-similarities* nil)

(defparameter *redwoods-score-counts* nil)

(defparameter *redwoods-score-microaverage-p* nil)

(defparameter *redwoods-use-item-sets-p* t)

(defparameter *redwoods-thinning-export-p* nil)

(defparameter *redwoods-thinning-normalize-p* nil)

(defparameter *reconstruct-cache* nil)

(defparameter %tsdb-lexical-preterminals% nil)
