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

(eval-when (:load-toplevel :execute)
  ;;
  ;; establish gc() hook that toggles podium(1) cursor for global gc()s;
  ;; unfortunately, there is no (pre-5.0 and all-lisp) way to get gc() cursors
  ;; for scavenges too.
  ;;
  (let ((default-gc-after-hook excl:*gc-after-hook*)
        global-gc-p)
    (setf excl:*gc-after-hook*
      #'(lambda (global new old efficiency pending)
          (when *tsdb-gc-statistics*
            (incf (gc-statistics (if global :global :scavenge)))
            (incf (gc-statistics :new) new)
            (incf (gc-statistics :old) old)
            (when (and (integerp efficiency) (not (zerop efficiency)))
              (push efficiency (gc-statistics :efficiency))))
          (when (null global-gc-p)
            (when (or (>= new *tsdb-scavenge-limit*) (< new 0))
              (top-level::zoom-command
               :from-read-eval-print-loop nil :all t :brief t)
              (error
               #+:lkb
               "gc-after-hook(): scavenge limit exceeded [~d] (~d edges)."
               #-:lkb
               "gc-after-hook(): scavenge limit exceeded [~d]." 
               new #+:lkb common-lisp-user::*edge-id*))
            (unless global
              (incf *tsdb-tenured-bytes* old)
              (when (> *tsdb-tenured-bytes* *tsdb-tenured-bytes-limit*)
                (excl:without-interrupts
                  (setf global-gc-p t)
                  #-(version>= 5 0)
                  (busy :gc :start)
                  (when *tsdb-gc-message-p*
                    (format 
                     *terminal-io*
                     "~&gc-after-hook(): ~d bytes were tenured; ~
                      triggering global gc().~%"
                     *tsdb-tenured-bytes*))
                  (excl:gc t)
                  (setf global-gc-p nil)
                  (setf *tsdb-tenured-bytes* 0)
                  #-(version>= 5 0)
                  (busy :gc :end)))))
          (when default-gc-after-hook
            (funcall default-gc-after-hook 
                     global new old efficiency pending)))))
  (setf excl:*global-gc-behavior* nil)
  ;;
  ;; ensure that podium(1) process (talking to wish(1)) terminates gracefully;
  ;; apparently, the EOF that wish(1) should see once the lisp stream is gone,
  ;; is insufficient to make wish(1) exit.
  ;;
  (push '(ignore-errors (shutdown-podium)) sys:*exit-cleanup-forms*)
  (when (find :compiler *features*)
    (excl:advise mp:process-kill :before nil nil 
                 (when (and *tsdb-wish-process*
                            (eq (first excl:arglist) *tsdb-wish-process*))
                   (ignore-errors (shutdown-podium))))))
