;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LKB -*-

;;; Copyright (c) 2003--2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: lui.lsp
;;;      module: Linguistic User Interface protocol
;;;     version: 0.0 (1-jun-03)
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

(in-package :lkb)

(defparameter *lui-application*
  "exec /home/oe/lbin/nc localhost 4712"
  #+:null
  (format
   nil 
   "exec ~a"
   (namestring (make-pathname :directory (pathname-directory make::bin-dir)
                              :name "lui"))))

(defparameter *lui-debug-p* t)

(defparameter %lui-stream% nil)

(defparameter %lui-pid% nil)

(defparameter %lui-process% nil)

(defparameter %lui-pending-events% nil)

(defun lui-initialize ()
  (lui-shutdown)
  (let (foo)
    (multiple-value-setq (%lui-stream% foo %lui-pid%)
      (run-process *lui-application*
                   :wait nil
                   :output :stream :input :stream :error-output nil))
    (when foo (setf foo foo))
    (setf %lui-process%
      (mp:run-function '(:name "LUI") #'lsp-loop nil %lui-stream%))))

(defun lui-shutdown ()

  (when %lui-stream%
    (ignore-errors
     (format %lui-stream% "~%~%exit~%~a" #\page)
     (force-output %lui-stream%)
     (sleep 2)
     (close %lui-stream%)
     (setf %lui-stream% nil)))
  (when %lui-pid%
    (ignore-errors
     (run-process "kill -HUP ~d" %lui-pid% 
                  :wait t :output "/dev/null" :error-output "/dev/null")
     (run-process "kill -TERM ~d" %lui-pid% 
                  :wait t :output "/dev/null" :error-output "/dev/null")
     (run-process "kill -QUIT ~d" %lui-pid% 
                  :wait t :output "/dev/null" :error-output "/dev/null"))
    #+:allegro
    (sys:os-wait nil %lui-pid%)
    (setf %lui-pid% nil))
  (when %lui-process%
    (let ((process %lui-process%))
      (setf %lui-process% nil)
      (ignore-errors
       (mp:process-kill process)))))

(defun send-to-lui (string &key (wait nil) recursive)
  (unless recursive
    #+:debug
    (when (and %lui-process% (not (eq mp:*current-process* %lui-process%)))
      (mp:process-add-arrest-reason %lui-process% :send-to-lui))
    (when *lui-debug-p*
      (format t "~&send-to-lui(): [send] `~a'.~%" string))
    (format %lui-stream% "~@[wait ~*~]~a~%~a" wait string #\page)
    (force-output %lui-stream%))
  (unwind-protect 
      (when (or wait recursive)
        (let ((*package* (find-package :lkb))
              (form (when (streamp %lui-stream%) 
                      (lsp-parse-command nil (lsp-read nil %lui-stream%)))))
          (when *lui-debug-p*
            (format t "~&send-to-lui(): [return] `~s'.~%" form))
          (cond
           ((eq (first form) 'event)
            (when *lui-debug-p*
              (format
               t 
               "~&send-to-lui(): queueing intervening event `~s'.~%" 
               form))
            (setf %lui-pending-events% 
              (append %lui-pending-events% (list form)))
            (send-to-lui nil :recursive t))
           (t
            form))))
    #+:debug
    (when (and %lui-process% (not (eq mp:*current-process* %lui-process%)))
      (mp:process-revoke-arrest-reason %lui-process% :send-to-lui))))

(defun process-pending-events ()
  #+:debug
  (when (and %lui-process% (not (eq mp:*current-process* %lui-process%)))
    (mp:process-add-arrest-reason %lui-process% :pending-events))
  (loop
      while (and (streamp %lui-stream%) %lui-pending-events%)
      do (lsp-process-event nil (pop %lui-pending-events%) %lui-stream%))
  #+:debug
  (when (and %lui-process% (not (eq mp:*current-process* %lui-process%)))
    (mp:process-revoke-arrest-reason %lui-process% :pending-events)))

(defun lui-show-parses (edges &optional (input *sentence*))
  (loop
      for i from 1
      for title = (format nil "`~a' Parse Tree # ~d" input i)
      for edge in edges 
      for top = (make-new-parse-tree edge 1)
      for id = (lsp-store-object nil input top)
      do
        (format %lui-stream% "tree ~d " id)
        (lui-show-tree top input)
        (format %lui-stream% "~s~a~%" title #\page)))

(defun lui-show-tree (top &optional (input *sentence*))
  (let* ((edge (get top 'edge-record))
         (daughters (get top 'daughters))
         (form (when (and daughters (null (rest daughters))
                          (null (get (first daughters) 'edge-record)))
                 (get-string-for-edge (first daughters))))
         (id (lsp-store-object nil input edge))
         (label (get-string-for-edge top))
         (n (edge-id edge))
         (rule (if (rule-p (edge-rule edge))
                 (string (rule-id (edge-rule edge)))
                 (string-downcase (string (first (edge-lex-ids edge)))))))
    (format %lui-stream% "#T[~a ~s ~s ~a ~s " id label form n rule)
    (loop
        for daughter in (if form (get (first daughters) 'daughters) daughters)
        do (lui-show-tree daughter))
    (format %lui-stream% "]")))

(defun lui-display-fs (tdfs input id)
  (declare (ignore id))
  (let* ((id (lsp-store-object nil input tdfs))
         (title (format nil "`~a' AVM" input))
         (dag (tdfs-indef tdfs)))
    (format %lui-stream% "avm ~d" id)
    (display-dag1 dag 'linear %lui-stream%)
    (format %lui-stream% " ~s~a~%" title #\page)))
