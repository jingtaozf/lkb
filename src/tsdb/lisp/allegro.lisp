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

(defparameter *scavenge-limit* 402653184)

(eval-when (:load-toplevel :execute)
  ;;
  ;; establish gc() hook that toggles podium(1) cursor for global gc()s;
  ;; unfortunately, there is no (pre-5.0 and all-lisp) way to get gc() cursors
  ;; for scavenges too.
  ;;
  (let ((default-gc-after-hook excl:*gc-after-hook*)
        global-gc-p)
    (setf excl:*gc-after-hook*
      #'(lambda (global scavenged tenured foo bar)
          #+:lkb
          (when (null global-gc-p)
            (when (or (>= scavenged *scavenge-limit*) (< scavenged 0))
              (top-level::zoom-command
               :from-read-eval-print-loop nil :all t :brief t)
              (error
               #+:lkb
               "gc-after-hook(): scavenge limit exceeded [~d] (~d edges)."
               #-:lkb
               "gc-after-hook(): scavenge limit exceeded [~d]." 
               scavenged #+:lkb common-lisp-user::*edge-id*))
            (unless global
              (incf *tenured-bytes* tenured)
              (when (> *tenured-bytes* excl:*tenured-bytes-limit*)
                (excl:without-interrupts
                  (setf global-gc-p t)
                  #-(version>= 5 0)
                  (busy :gc :start)
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
                  #-(version>= 5 0)
                  (busy :gc :end)))))
          (when default-gc-after-hook
            (funcall default-gc-after-hook 
                     global scavenged tenured foo bar)))))
  (setf excl:*global-gc-behavior* nil)
  ;;
  ;; ensure that podium(1) process (talking to wish(1)) terminates gracefully;
  ;; apparently, the EOF that wish(1) should see once the lisp stream is gone,
  ;; is insufficient to make wish(1) exit.
  ;;
  (push '(ignore-errors (shutdown-podium)) sys:*exit-cleanup-forms*)
  (excl:advise mp:process-kill :before nil nil 
               (when (and *tsdb-wish-process*
                          (eq (first excl:arglist) *tsdb-wish-process*))
                 (ignore-errors (shutdown-podium)))))
