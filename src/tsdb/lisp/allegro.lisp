;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: TSDB -*-

;;;
;;; [incr tsdb()] --- Competence and Performance Profiling Environment
;;; Copyright (c) 1996 -- 2005 Stephan Oepen (oe@csli.stanford.edu)
;;; Copyright (c) 2006 -- 2009 Stephan Oepen (oe@ifi.uio.no)
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

(in-package :tsdb)

(defparameter *tsdb-gc-debug* nil)

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
                    (let ((file (format
                                 nil
                                 "~a/acl.debug.~a.~a"
                                 (tmp :acl) user pid)))
                      (when (probe-file file) (delete-file file))
                      (open file :direction :output
                            :if-exists :supersede 
                            :if-does-not-exist :create)))))
         (scavenges 0)
         (gcs 0)
        global-gc-p)
    (setf excl:*gc-after-hook*
      #'(lambda (global new old efficiency pending)
          (if global (incf gcs) (incf scavenges))
          (when (and (or *tsdb-gc-message-p* *tsdb-gc-debug*)
                     (output-stream-p stream))
            (let* ((statm (when (probe-file "/proc/self/statm")
                            (with-open-file (stream "/proc/self/statm")
                              (loop
                                  for foo = (read stream nil nil)
                                  while foo collect foo))))
                   (size (when (first statm) (* (first statm) 4096)))
                   (resident (when (second statm) (* (second statm) 4096))))
              (format
               stream
               "~&[~a] gc-after-hook(): {~:[L~;G~]~@[R~*~]~
                #~a N=~a O=~a~@[ E=~d%~]}~@[~* [S=~a R=~a]~].~%" 
               (current-time :long :short) global global-gc-p 
               (if global-gc-p gcs scavenges)
               (pprint-potentially-large-integer new)
               (pprint-potentially-large-integer old)
               (and (integerp efficiency) efficiency)
               (and (numberp size) (numberp resident))
               (pprint-potentially-large-integer size)
               (pprint-potentially-large-integer resident))))
          (when *tsdb-gc-statistics*
            (incf (gc-statistics (if global :global :scavenge)))
            (incf (gc-statistics :new) new)
            (incf (gc-statistics :old) old)
            (when (and (not global) (integerp efficiency))
              (push efficiency (gc-statistics :efficiency))))
          (when (and *tsdb-gc-debug* (output-stream-p stream))
            (let ((*print-readably* nil)
                  (*print-pretty* t)
                  (*terminal-io* stream)
                  (*standard-output* stream)
                  (n (if global gcs scavenges))
                  (actions
                   (if global (rest *tsdb-gc-debug*) (first *tsdb-gc-debug*))))
              (when actions
                (loop
                    for action in actions
                    for count = (first action)
                    when (and (numberp count) (zerop (mod n count))) do
                      (loop
                          for action in (rest action)
                          do
                            (format
                             stream
                             "~&[~a] gc-after-hook(): executing `~(~a~)' ~
                              for ~:[L~;G~]~@[R~*~]#~a:~%"
                             (current-time :long :short) action
                             global global-gc-p
                             (if global-gc-p gcs scavenges))
                            (case action
                              ((:new :pan :notpan :old :malloc :holes)
                               (excl:print-type-counts action))
                              (:count
                               (excl:print-type-counts t))
                              (:room
                               (room))
                              (:zoom
                               (let ((*print-miser-width* 40)
                                     (tpl:*zoom-print-circle* t)
                                     (tpl:*zoom-print-level* nil)
                                     (tpl:*zoom-print-length* nil))
                                 (tpl::zoom-command
                                  :from-read-eval-print-loop nil
                                  :all t :brief t :count t)))))))))
          (unless global-gc-p
            ;;
            ;; unfortunately, this breaks because of yet another issue in the
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
                  (when (and (or *tsdb-gc-message-p* *tsdb-gc-debug*)
                             (output-stream-p stream))
                    (format 
                     stream
                     "~&[~a] gc-after-hook(): ~a tenured; ~
                      forcing global gc().~%"
                     (current-time :long :short) 
                     (pprint-potentially-large-integer *tsdb-tenured-bytes*)))
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
  ;; come up with an actual fix.                           (21-may-00  -  oe)
  ;;
  ;; --- see whether we still need this.                      (21-apr-05; oe)
  ;;
  #+:null
  (flet ((exercise-scheduler () (loop (sleep 60))))
    (mp:process-run-function '(:name "scheduler work-out [spr19158]")
                             #'exercise-scheduler))
  ;;
  ;; ensure that podium(1) process (talking to wish(1)) terminates gracefully;
  ;; apparently, the EOF that wish(1) should see once the lisp stream is gone,
  ;; is insufficient to make it exit.
  ;;
  (push '(ignore-errors (shutdown-podium)) sys:*exit-cleanup-forms*)
  ;;
  ;; with the latest set of CLIM patches, it appears this latter bit results in
  ;; an `operation on closed stream' error, when shutting down the Lisp.
  ;;                                                             (8-feb-08; oe)
  (when (find :compiler *features*)
    (excl:advise mp:process-kill :before nil nil 
                 (when (and *tsdb-wish-process*
                            (eq (first excl:arglist) *tsdb-wish-process*))
                   (let* ((stream excl:*initial-terminal-io*)
                          (*standard-output* stream)
                          (*debug-io* stream)
                          (*terminal-io* stream)
                          (*standard-input* stream)
                          (*error-output* stream)
                          (*query-io* stream)
                          (*trace-output* stream))
                     (ignore-errors (shutdown-podium)))))))


