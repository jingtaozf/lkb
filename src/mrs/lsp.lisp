;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: MRS -*-

;;; Copyright (c) 2003--2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: lsp.lisp
;;;      module: MRS-specific handlers for LKB Server Protocol (LSP)
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

(in-package :mrs)

(defun lsp-process-event (id command stream)
  (let* ((psoa (pop command))
         (psoa (cond 
                ((psoa-p psoa) psoa)
                ((stringp psoa) (read-mrs-from-string psoa))
                ((numberp psoa) (lkb::lsp-retrieve-object id psoa))))
         (action (intern (string (pop command)) :mrs))
         (return lkb::%lsp-ok%))
    (case action
      (browse
       (let* ((format (if (first command)
                        (intern (string (pop command)) :mrs)
                        'simple))
              (title (or (pop command) 
                         (format nil "~@(~a~) MRS [LSP # ~a]" format id))))
         (case format
           (simple (lkb::show-mrs-window nil psoa title))
           (indexed (lkb::show-mrs-indexed-window nil psoa title))
           (prolog (lkb::show-mrs-prolog-window nil psoa title))
           (scoped (lkb::show-mrs-scoped-window nil psoa title))
           (rmrs (lkb::show-mrs-rmrs-window nil psoa title))
           (dependencies (lkb::show-mrs-dependencies-window nil psoa title))
           (t (setf return lkb::%lsp-invalid-format%)))))
      (convert
       (if (streamp stream)
         (let* ((format (if (first command)
                          (intern (string (pop command)) :mrs)
                          'simple)))
           (cond
            ((member format '(simple indexed prolog scoped rmrs))
             (let ((string (with-output-to-string (stream)
                             (output-mrs1 psoa format stream))))
               (format stream "~s" string)))
            ((eq format 'dependencies)
             (let ((string (with-output-to-string (stream)
                             (ed-output-psoa psoa :stream stream))))
               (format stream "~s" string)))
            (t
             (setf return lkb::%lsp-invalid-format%))))
         (setf return lkb::%lsp-invalid-asynchronous-command%)))
      (t (setf return lkb::%lsp-invalid-subcommand%)))
    return))
