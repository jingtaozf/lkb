;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: TSDB -*-

;;;
;;; [incr tsdb()] --- Competence and Performance Profiling Environment
;;; Copyright (c) 1996 -- 2005 Stephan Oepen (oe@csli.stanford.edu)
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
           (let* ((*package* (find-package :lkb))
                  (condition (format nil "i-id == ~a" i-id))
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
                                  collect (get-field :result-id bar))))))
             (when active
               (loop
                   with lkb::*deleted-daughter-features* = nil
                   with i-input = (get-field :i-input item)
                   with results = (get-field :results item)
                   for i from 1
                   for result in results
                   for result-id = (get-field :result-id result)
                   for derivation = (get-field :derivation result)
                   for edge = (and derivation (reconstruct derivation))
                   for title = (format
                                nil
                                "(~a:~a) `~a' [LSP # ~a]" 
                                i-id result-id i-input id)
                   when (or (eq set :all)
                            (member result-id active :test #'eql))
                   do
                     (if (member format '(:avm :tree :mrs :indexed :prolog
                                          :scoped :dependencies))
                       (if (eq view :browse)
                         (lkb::lsp-browse 
                          id i-input (list edge) format :title title)
                         (lkb::lsp-return id stream (list edge) format))
                       (setf return lkb::%lsp-invalid-format%)))))
           (setf return lkb::%lsp-syntax-error%))))
      (t (setf return lkb::%lsp-invalid-subcommand%)))
    return))
