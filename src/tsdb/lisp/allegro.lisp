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
  (let* ((default-gc-after-hook excl:*gc-after-hook*)
         (user (current-user))
         (pid (current-pid))
         (stream (if (output-stream-p excl:*initial-terminal-io*)
                   excl:*initial-terminal-io*
                   (ignore-errors
                    (let ((file (format nil "/tmp/acl.debug.~a.~a" user pid)))
                      (when (probe-file file)
                        (delete-file file))
                      (open file :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)))))
        global-gc-p)
    (setf excl:*gc-after-hook*
      #'(lambda (global new old efficiency pending)
          #+:oe
          (let ((*print-readably* nil)
                (*print-miser-width* 40)
                (*print-pretty* t)
                (tpl:*zoom-print-circle* t)
                (tpl:*zoom-print-level* nil)
                (tpl:*zoom-print-length* nil)
                (*terminal-io* stream)
                (*standard-output* stream))
            (format
             t
             "~&gc-after-hook(): ~:[local~;global~]~@[ (recursive)~*~]; ~
              new: ~a; old: ~a; pending: ~a~@[~*; efficiency: ~d~].~%" 
             global global-gc-p new old pending 
             (integerp efficiency) (round efficiency))
            (top-level::zoom-command
             :from-read-eval-print-loop nil :all t :brief t))
          (when *tsdb-gc-statistics*
            (incf (gc-statistics (if global :global :scavenge)))
            (incf (gc-statistics :new) new)
            (incf (gc-statistics :old) old)
            (when (and (not global) (integerp efficiency))
              (push efficiency (gc-statistics :efficiency))))
          (when (null global-gc-p)
            #+(version>= 5 0)
            (when (or (>= new *tsdb-scavenge-limit*) (< new 0))
              (ignore-errors
               (let ((*print-readably* nil)
                     (*print-miser-width* 40)
                     (*print-pretty* t)
                     (tpl:*zoom-print-circle* t)
                     (tpl:*zoom-print-level* nil)
                     (tpl:*zoom-print-length* nil)
                     (*terminal-io* stream)
                     (*standard-output* stream))
                 (top-level::zoom-command
                  :from-read-eval-print-loop nil :all t :brief t)))
              (error
               #+:lkb
               "gc-after-hook(): scavenge limit exceeded [~d] (~d edges)."
               #-:lkb
               "gc-after-hook(): scavenge limit exceeded [~d]." 
               new #+:lkb common-lisp-user::*edge-id*))
            (unless global
              (incf *tsdb-tenured-bytes* old)
              (when (and *tsdb-tenured-bytes-limit*
                         (> *tsdb-tenured-bytes* *tsdb-tenured-bytes-limit*))
                (excl:without-interrupts
                  (setf global-gc-p t)
                  #-(version>= 5 0)
                  (busy :gc :start)
                  (when (and #-:oe *tsdb-gc-message-p* (output-stream-p stream))
                    (format 
                     stream
                     "~&gc-after-hook(): ~d bytes were tenured; ~
                      triggering global gc().~%"
                     *tsdb-tenured-bytes*))
                  (let ((*terminal-io* stream)
                        (*standard-output* stream))
                    #+(and :oe :gcdebug)
                    (room)
                    (excl:gc t)
                    #+(and :oe :gcdebug)
                    (room))
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
  ;; is insufficient to make it exit.
  ;;
  (push '(ignore-errors (shutdown-podium)) sys:*exit-cleanup-forms*)
  (when (find :compiler *features*)
    (excl:advise mp:process-kill :before nil nil 
                 (when (and *tsdb-wish-process*
                            (eq (first excl:arglist) *tsdb-wish-process*))
                   (ignore-errors (shutdown-podium))))))
