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
  (declare (ignore id))
  (let ((action (intern (string (pop command)) :tsdb))
        (return lkb::%lsp-ok%))
    (case action
      (item
       (let* ((data (pop command))
              (i-id (pop command))
              (set (intern (string (pop command)) :tsdb))
              (view (intern (string (pop command)) :keyword)))
         (if (and (streamp stream) (stringp data) (integerp i-id) set view)
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
                  (*redwoods-export-values* (list view)))
             (when active
               (let ((string (with-output-to-string (stream)
                               (export-tree item active :lspp t 
                                             :stream stream))))
                 (format stream "~s" string))
               (when (eq set 'all)
                 (let ((string (with-output-to-string (stream)
                                 (export-tree item active :lspp t
                                              :complementp t
                                              :stream stream))))
                   (format stream "~s" string)))))
           (setf return lkb::%lsp-syntax-error%))))
      (t (setf return lkb::%lsp-invalid-subcommand%)))
    return))
