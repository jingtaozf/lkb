;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: MAIN -*-

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

(in-package "MAIN")

(define-command
  '(:tsdb
    &nec command &optional argument &key condition run skeleton load
    &doc 
    "Interact with tsdb(1) and profiling module; `tsdb :help' for details.")
  #'tsdb:tsdb)

#+:obsolete
(define-command
  '(:retrieve
    &optional condition language
    &doc "Retrieve TSDB test items (optionally matching CONDITION).")
  #'tsdb:retrieve)

#+:obsolete
(define-command
  '(:retrieve-and-process
    &optional condition run language comment
    &key verbose gc
    &doc 
    "Retrieve TSDB test items (optionally matching CONDITION), feed them into 
  the parser and store results in the `parse' relation (as RUN with COMMENT).")
  #'tsdb:retrieve-and-process)

#+:obsolete
(define-command
  '(:vocabulary
    &optional condition language load verbose
    &doc "Retrieve TSDB test suite vocabulary (optionally matching CONDITION);
  if LOAD is not null(), the necessary vocabulary is dynamically loaded;
  VERBOSE controls the tracing level (one of `:full', `:fair', or `:none').")
  #'tsdb:vocabulary)

