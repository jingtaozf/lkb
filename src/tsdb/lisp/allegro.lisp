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
;;;        file: allegro.lisp
;;;      module: gc() after hook for Allegro CL
;;;     version: 0.0 (30-jul-98)
;;;  written by: oe, csli stanford
;;; last update: 26-mar-00
;;;  updated by: oe, csli stanford
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
  ;; use gc() hook to toggle [incr tsdb()] podium cursor for global gc()s;
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
          (when (and *tsdb-gc-message-p* (output-stream-p stream))
            (format
             stream
             "~&gc-after-hook(): ~:[local~;global~]~@[ (r)~*~]; ~
              new: ~a; old: ~a; pending: ~a~@[~*; efficiency: ~d~].~%" 
             global global-gc-p new old pending 
             (integerp efficiency) efficiency)
            #+:gcdebug
            (let ((*print-readably* nil)
                  (*print-miser-width* 40)
                  (*print-pretty* t)
                  (tpl:*zoom-print-circle* t)
                  (tpl:*zoom-print-level* nil)
                  (tpl:*zoom-print-length* nil)
                  (*terminal-io* stream)
                  (*standard-output* stream))
              (tpl::zoom-command
               :from-read-eval-print-loop nil :all t :brief t)))
          (when *tsdb-gc-statistics*
            (incf (gc-statistics (if global :global :scavenge)))
            (incf (gc-statistics :new) new)
            (incf (gc-statistics :old) old)
            (when (and (not global) (integerp efficiency))
              (push efficiency (gc-statistics :efficiency))))
          (when (null global-gc-p)
            ;;
            ;; unfortunately, this breaks because of yet another bug in the
            ;; Allegro memory management: when newspace is expanded during a
            ;; scavenge, the `copy new' value reported by the gc() statistics
            ;; is much bigger than would be appropriate.  this causes our .new.
            ;; parameter to overflow the fixnum range.  email to `bugs@franz'
            ;; sent today.                                   (22-jan-00  -  oe)
            ;;
            #+:null
            (when (and (integerp *tsdb-scavenge-limit*)
                       (>= new *tsdb-scavenge-limit*))
              (let ((*print-readably* nil)
                    (*print-miser-width* 40)
                    (*print-pretty* t)
                    (tpl:*zoom-print-circle* t)
                    (tpl:*zoom-print-level* nil)
                    (tpl:*zoom-print-length* nil)
                    (*terminal-io* stream)
                    (*standard-output* stream))
                (tpl::zoom-command
                 :from-read-eval-print-loop nil :all t :brief t))
              (error "gc-after-hook(): scavenge limit exceeded [~d]" new))
            (unless global
              (incf *tsdb-tenured-bytes* old)
              (when (and *tsdb-tenured-bytes-limit*
                         (> *tsdb-tenured-bytes* *tsdb-tenured-bytes-limit*))
                (excl:without-interrupts
                  (setf global-gc-p t)
                  #-(version>= 5 0)
                  (busy :gc :start)
                  (when (and *tsdb-gc-message-p* (output-stream-p stream))
                    (format 
                     stream
                     "~&gc-after-hook(): ~d bytes were tenured; ~
                      triggering immediate global gc().~%"
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
  ;; increase buffer size used with stream io; reduce write(2) system calls.
  ;;
  (setf excl::stream-buffer-size 8192)
  ;;
  ;; exercise Allegro scheduler at least once per minute; we hope this may work
  ;; around Allegro bug [spr19158]; ultimately, of course, we expect Franz to
  ;; come up with an actual fix.                        (21-may-00  -  oe)
  ;;
  (flet ((exercise-scheduler () (loop (sleep 60))))
    (mp:process-run-function '(:name "scheduler work-out [spr19158]")
                             #'exercise-scheduler))
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


