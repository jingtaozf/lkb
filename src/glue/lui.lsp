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
  (format
   nil 
   "exec ~a"
   (namestring (make-pathname :directory (pathname-directory make::bin-dir)
                              :name "yzlui"))))

(defparameter *lui-hidden-features*
  '(ARG ARG0 ARG1 ARG2 ARG3 ARG4
    MARG L-INDEX R-INDEX L-HNDL R-HNDL L-HANDEL R-HANDEL 
    MAIN SUBORD ROLE HINST NHINST))

(defparameter *lui-debug-p* t)

(defparameter %lui-stream% nil)

(defparameter %lui-pid% nil)

(defparameter %lui-process% nil)

(defparameter %lui-pending-events% nil)

(defparameter %lui-eoc% (format nil " ~a" #\page))

(defun lui-initialize ()
  (lui-shutdown)
  (let (foo)
    (multiple-value-setq (%lui-stream% foo %lui-pid%)
      #-(or :openmcl :clisp)
      (run-process *lui-application*
                   :wait nil
                   :output :stream :input :stream 
                   #-:clisp :error-output #-:clisp nil)
      #+:clisp
      (ext:make-pipe-io-stream *lui-application* :buffered nil)
      #+:openmcl
      (let ((process (run-program "yzlui" nil :wait nil :input :stream
                                  :output :stream)))
        (values (make-two-way-stream 
                 (external-process-input-stream process)
                 (external-process-input-stream process)))))
                    
    (when foo (setf foo foo))
    (format
     %lui-stream%
     "parameter list-type ~a~a~%~
      parameter non-empty-list-type ~a~a~%~
      parameter empty-list-type ~a~a~%~
      parameter list-head ~a~a~%~
      parameter list-tail ~a~a~%~
      status ready~a~%"
     *list-type* %lui-eoc%
     *non-empty-list-type* %lui-eoc%
     *empty-list-type* %lui-eoc% 
     (first *list-head*) %lui-eoc%
     (first *list-tail*) %lui-eoc%
     %lui-eoc%)
    (force-output %lui-stream%)
    #-:clisp
    (setf %lui-process%
      (mp:run-function '(:name "LUI") #'lsp-loop nil %lui-stream%))
    #+:clisp
    (lsp-loop nil %lui-stream%)
    #+:clisp
    (lui-shutdown)))

(defun lui-shutdown ()

  (when %lui-stream%
    (ignore-errors
     (format %lui-stream% "~%~%quit~a" %lui-eoc%)
     (force-output %lui-stream%))
    (sleep 2)
    (ignore-errors (close %lui-stream%))
    (setf %lui-stream% nil))
  #-:clisp
  (when %lui-pid%
    (ignore-errors
     (run-process "kill -HUP ~d" %lui-pid% 
                  :wait t :output "/dev/null" 
                  #-:clisp :error-output #-:clisp "/dev/null")
     (run-process "kill -TERM ~d" %lui-pid% 
                  :wait t :output "/dev/null" 
                  #-:clisp :error-output #-:clisp "/dev/null")
     (run-process "kill -QUIT ~d" %lui-pid% 
                  :wait t :output "/dev/null" 
                  #-:clisp :error-output #-:clisp "/dev/null"))
    #+:allegro
    (sys:os-wait nil %lui-pid%)
    (setf %lui-pid% nil))
  #-:clisp
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
    (format %lui-stream% "~@[wait ~*~]~a~a" wait string %lui-eoc%)
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

(defun lui-status (string)
  (format %lui-stream% "message ~s~a~%" string %lui-eoc%))
  
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
        (format %lui-stream% " ~s~a~%" title %lui-eoc%))
  (force-output %lui-stream%))

(defun lui-show-tree (top &optional (input *sentence*))
  (let* ((edge (get top 'edge-record))
         (tdfs (get top 'edge-fs))
         (daughters (get top 'daughters))
         (form (when (and daughters (null (rest daughters))
                          (null (get (first daughters) 'edge-record)))
                 (get-string-for-edge (first daughters))))
         (id (lsp-store-object nil input tdfs))
         (label (get-string-for-edge top))
         (n (edge-id edge))
         (rule (if (rule-p (edge-rule edge))
                 (rule-id (edge-rule edge))
                 (string-downcase (string (first (edge-lex-ids edge)))))))
    (format %lui-stream% "#T[~a ~s ~s ~a ~a " id label form n rule)
    (loop
        for daughter in (if form (get (first daughters) 'daughters) daughters)
        do (lui-show-tree daughter))
    (format %lui-stream% "]")))

(defun lui-display-fs (tdfs title id)
  (declare (ignore id))
  (let* ((id (lsp-store-object nil nil tdfs))
         (dag (tdfs-indef tdfs)))
    (let ((string (with-output-to-string (stream)
                    (format stream "avm ~d " id)
                    (display-dag1 dag 'linear stream))))
      (format %lui-stream% string))
    (format %lui-stream% " ~s~a~%" title %lui-eoc%))
  (force-output %lui-stream%))

(defun lui-display-mrs (mrs)
  (let* ((id (lsp-store-object nil nil mrs))
         (dag (mrs::psoa-to-dag mrs))
         (title "Simple MRS Display"))
    (let ((string (with-output-to-string (stream)
                    (when *lui-hidden-features*
                      (format 
                       stream 
                       "parameter+ hidden-features ~a~a~%"
                       (first *lui-hidden-features*) %lui-eoc%)
                      (loop
                          for foo in (rest *lui-hidden-features*)
                          do
                            (format 
                             stream 
                             "parameter+ hidden-features ~a~a~%"
                             foo %lui-eoc%)))
                    (format stream "avm ~d " id)
                    (display-dag1 dag 'linear stream))))
      (format %lui-stream% string))
    (format %lui-stream% " ~s~a~%" title %lui-eoc%))
  (force-output %lui-stream%))

(defun lui-status-p (key)
  (case key
    #+:null
    (:tree (streamp %lui-stream%))
    #-:null
    (:avm (streamp %lui-stream%))
    #-:null
    (:mrs (streamp %lui-stream%))))
