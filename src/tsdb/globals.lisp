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

(defparameter *tsdb-version* "1.0 (4-sep-98)")

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
                          '(:relative "tsdb" "skeletons"))))

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

(defparameter *tsdb-cache-database-writes-p* t)

(defparameter *tsdb-flush-cache-threshold* 500)

(defparameter *tsdb-verbose-cache-flush-p* nil)

(defparameter *tsdb-trees-hook* nil)

(defparameter *tsdb-semantix-hook* nil)

(defparameter *tsdb-gc-p* nil)

(defparameter *tsdb-minimize-gcs-p* t)

(defparameter *tsdb-tenure-p* nil)

(defparameter *tsdb-gc-message-p* t)

(defparameter *tsdb-item-factor* 2.0)

(defparameter *tsdb-exhaustive-p* t)

(defparameter *tsdb-ignore-output-p* nil)

(defparameter *tsdb-maximal-number-of-tasks* 0)

(defparameter *tsdb-lexical-oracle-p* nil)

(defparameter *tsdb-phrasal-oracle-p* nil)

(defparameter *tsdb-default-skeleton* "english")

(defparameter *tsdb-skeleton-index* "Index.lisp")

(defparameter *tsdb-skeletons* nil)

(defparameter *tsdb-initialized-p* nil)

(defvar *tsdb-phenomena* (make-hash-table :test #'equal))

(defparameter *tsdb-global-gcs* 0)

(defparameter *tsdb-profile-files*
  (list "daughter" "edge" "parse" "result" "run"))

(defparameter *tsdb-tokens-to-ignore*
  (list "." "(" ")" "!" "?" "-" "'" "[" "]" "`"))

(defparameter *tsdb-slash* #\/)

(defparameter *tsdb-gc-cursor* :pirate)

(defparameter *tenured-bytes* 0)

(defparameter *tsdb-debug-mode-p* nil)

(defparameter %tsdb-lexical-preterminals% nil)

#+allegro
(eval-when (:load-toplevel :execute)
  (let ((default-gc-after-hook excl:*gc-after-hook*)
        global-gc-p)
    (setf excl:*gc-after-hook*
      #'(lambda (global scavenged tenured foo bar)
          (when (null global-gc-p)
            (unless global
              (incf *tenured-bytes* tenured)
              (when (> *tenured-bytes* excl:*tenured-bytes-limit*)
                (excl:without-interrupts
                  (setf global-gc-p t)
                  (busy :cursor *tsdb-gc-cursor*)
                  (when *tsdb-gc-message-p*
                    (format 
                     *terminal-io*
                     "~&gc-after-hook(): ~d bytes were tenured; ~
                      triggering global gc().~%"
                     *tenured-bytes*))
                  (excl:gc t)
                  (setf global-gc-p nil)
                  (setf *tenured-bytes* 0)
                  (incf *tsdb-global-gcs*)
                  (busy :action :restore)))))
          (when default-gc-after-hook
            (funcall default-gc-after-hook 
                     global scavenged tenured foo bar)))))
  (setf excl:*global-gc-behavior* nil))





