;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: TSDB -*-

;;; Copyright (c) 2003--2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: lsp.lisp
;;;      module: [incr tsdb()]-specific handlers for LKB Server Protocol (LSP)
;;;     version: 0.0 (17-may-03)
;;;  written by: oe, csli stanford
;;; last update: 
;;;  updated by: 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :tsdb)

(defun lsp-process-event (id command stream)

  (let ((action (intern (string (pop command)) :tsdb))
        (return lkb::%lsp-ok%))
    (case action
      (item
       (let* ((data (pop command))
              (i-id (pop command))
              (set (intern (string (pop command)) :keyword))
              (format (intern (string (pop command)) :keyword))
              (view (when command (intern (string (pop command)) :keyword))))
         (if (and (streamp stream) (stringp data) (integerp i-id) set format)
           (let* ((condition (format nil "i-id == ~a" i-id))
                  (items (analyze data :thorough '(:derivation) 
                                  :condition condition))
                  (item (when (and items (null (rest items))) (first items)))
                  (parse-id (get-field :parse-id item))
                  (trees (when parse-id 
                           (select '("t-active" "t-version") 
                                   '(:integer :integer) 
                                   "tree" 
                                   (format nil "parse-id == ~a" parse-id) 
                                   data)))
                  (version (when trees
                             (loop
                                 for tree in trees
                                 maximize (get-field :t-version tree))))
                  (active (when version
                            (let ((foo (select '("result-id") '(:integer) 
                                               "preference" 
                                               (format 
                                                nil 
                                                "parse-id == ~a ~
                                                 && t-version == ~d" 
                                                parse-id version) 
                                               data)))
                              (loop 
                                  for bar in foo 
                                  collect (get-field :result-id bar)))))
                  (*redwoods-export-values* (list format)))
             (when active
               (if (eq view :browse)
                 (loop
                     with *package* = (find-package lkb::*lkb-package*)
                     with lkb::*deleted-daughter-features* = nil
                     with i-input = (get-field :i-input item)
                     with results = (get-field :results item)
                     for i from 1
                     for result in results
                     for result-id = (get-field :result-id result)
                     for derivation = (get-field :derivation result)
                     for edge = (and derivation (reconstruct derivation))
                     for tdfs = (and edge (lkb::edge-dag edge))
                     for mrs = (and edge (mrs::extract-mrs edge))
                     for title = (format
                                  nil
                                  "(~a:~a) `~a' [LSP # ~a]" 
                                  i-id result-id i-input id)
                     when (or (eq set :all)
                              (member result-id active :test #'eql))
                     do
                       (case format
                         (:avm
                          (lkb::display-fs tdfs title))
                         (:tree
                          (lkb::draw-new-parse-tree
                           (lkb::make-new-parse-tree edge 1) title nil))
                         (:simple 
                          (lkb::show-mrs-window nil mrs title))
                         (:indexed 
                          (lkb::show-mrs-indexed-window nil mrs title))
                         (:prolog 
                          (lkb::show-mrs-prolog-window nil mrs title))
                         (:scoped 
                          (lkb::show-mrs-scoped-window nil mrs title))
                         (:rmrs 
                          (lkb::show-mrs-rmrs-window nil mrs title))
                         (:dependencies 
                          (lkb::show-mrs-dependencies-window nil mrs title))
                         (t
                          (setf return %lsp-invalid-format%))))
                 (let ((string (with-output-to-string (stream)
                                 (export-tree item active :lspp t 
                                              :stream stream))))
                   (format stream "~s" string)
                   (when (eq set :all)
                     (let ((string (with-output-to-string (stream)
                                     (export-tree item active :lspp t
                                                  :complementp t
                                                  :stream stream))))
                       (format stream "~s" string)))))))
           (setf return lkb::%lsp-syntax-error%))))
      (t (setf return lkb::%lsp-invalid-subcommand%)))
    return))
