;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LKB -*-

;;; Copyright (c) 2003--2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: glue.lsp
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

(defparameter *glue-application*
  (format
   nil 
   "exec ~a"
   (namestring (make-pathname :directory (pathname-directory make::bin-dir)
                              :name "yzglue"))))

(defparameter *glue-hidden-features*
  '(ARG ARG0 ARG1 ARG2 ARG3 ARG4
    MARG L-INDEX R-INDEX L-HNDL R-HNDL L-HANDEL R-HANDEL 
    MAIN SUBORD ROLE HINST NHINST))

(defparameter *glue-debug-p* t)

(defparameter %glue-stream% nil)

(defparameter %glue-pid% nil)

(defparameter %glue-process% nil)

(defparameter %glue-pending-events% nil)

(defparameter %glue-eoc% (format nil " ~a" #\page))

(defun glue-initialize ()
  (glue-shutdown)
  (let (foo)
    (multiple-value-setq (%glue-stream% foo %glue-pid%)
      #-(or :openmcl :clisp)
      (run-process *glue-application*
                   :wait nil
                   :output :stream :input :stream 
                   #-:clisp :error-output #-:clisp nil)
      #+:clisp
      (ext:make-pipe-io-stream *glue-application* :buffered nil)
      #+:openmcl
      (let ((process (run-program "yzglue" nil :wait nil :input :stream
                                  :output :stream)))
        (values (make-two-way-stream 
                 (external-process-input-stream process)
                 (external-process-input-stream process)))))
                    
    (when foo (setf foo foo))
    (format
     %glue-stream%
     "parameter list-type ~a~a~%~
      parameter non-empty-list-type ~a~a~%~
      parameter empty-list-type ~a~a~%~
      parameter list-head ~a~a~%~
      parameter list-tail ~a~a~%~
      status ready~a~%"
     *list-type* %glue-eoc%
     *non-empty-list-type* %glue-eoc%
     *empty-list-type* %glue-eoc% 
     (first *list-head*) %glue-eoc%
     (first *list-tail*) %glue-eoc%
     %glue-eoc%)
    (force-output %glue-stream%)
    #-:clisp
    (setf %glue-process%
      (mp:run-function '(:name "GLUE") #'lsp-loop nil %glue-stream%))
    #+:clisp
    (lsp-loop nil %glue-stream%)
    #+:clisp
    (glue-shutdown)))

(defun glue-shutdown ()

  (when %glue-stream%
    (ignore-errors
     (format %glue-stream% "~%~%quit~a" %glue-eoc%)
     (force-output %glue-stream%))
    (sleep 2)
    (ignore-errors (close %glue-stream%))
    (setf %glue-stream% nil))
  #-:clisp
  (when %glue-pid%
    (ignore-errors
     (run-process "kill -HUP ~d" %glue-pid% 
                  :wait t :output "/dev/null" 
                  #-:clisp :error-output #-:clisp "/dev/null")
     (run-process "kill -TERM ~d" %glue-pid% 
                  :wait t :output "/dev/null" 
                  #-:clisp :error-output #-:clisp "/dev/null")
     (run-process "kill -QUIT ~d" %glue-pid% 
                  :wait t :output "/dev/null" 
                  #-:clisp :error-output #-:clisp "/dev/null"))
    #+:allegro
    (sys:os-wait nil %glue-pid%)
    (setf %glue-pid% nil))
  #-:clisp
  (when %glue-process%
    (let ((process %glue-process%))
      (setf %glue-process% nil)
      (ignore-errors
       (mp:process-kill process)))))

(defun send-to-glue (string &key (wait nil) recursive)
  (unless recursive
    #+:debug
    (when (and %glue-process% (not (eq mp:*current-process* %glue-process%)))
      (mp:process-add-arrest-reason %glue-process% :send-to-glue))
    (when *glue-debug-p*
      (format t "~&send-to-glue(): [send] `~a'.~%" string))
    (format %glue-stream% "~@[wait ~*~]~a~a" wait string %glue-eoc%)
    (force-output %glue-stream%))
  (unwind-protect 
      (when (or wait recursive)
        (let ((*package* (find-package :lkb))
              (form (when (streamp %glue-stream%) 
                      (lsp-parse-command nil (lsp-read nil %glue-stream%)))))
          (when *glue-debug-p*
            (format t "~&send-to-glue(): [return] `~s'.~%" form))
          (cond
           ((eq (first form) 'event)
            (when *glue-debug-p*
              (format
               t 
               "~&send-to-glue(): queueing intervening event `~s'.~%" 
               form))
            (setf %glue-pending-events% 
              (append %glue-pending-events% (list form)))
            (send-to-glue nil :recursive t))
           (t
            form))))
    #+:debug
    (when (and %glue-process% (not (eq mp:*current-process* %glue-process%)))
      (mp:process-revoke-arrest-reason %glue-process% :send-to-glue))))

(defun process-pending-events ()
  #+:debug
  (when (and %glue-process% (not (eq mp:*current-process* %glue-process%)))
    (mp:process-add-arrest-reason %glue-process% :pending-events))
  (loop
      while (and (streamp %glue-stream%) %glue-pending-events%)
      do (lsp-process-event nil (pop %glue-pending-events%) %glue-stream%))
  #+:debug
  (when (and %glue-process% (not (eq mp:*current-process* %glue-process%)))
    (mp:process-revoke-arrest-reason %glue-process% :pending-events)))

(defun glue-status (string)
  (format %glue-stream% "message ~s~a~%" string %glue-eoc%))
  
(defun glue-show-parses (edges &optional (input *sentence*))
  (loop
      for i from 1
      for title = (format nil "`~a' Parse Tree # ~d" input i)
      for edge in edges 
      for top = (make-new-parse-tree edge 1)
      for id = (lsp-store-object nil input top)
      do
        (format %glue-stream% "tree ~d " id)
        (glue-show-tree top input)
        (format %glue-stream% " ~s~a~%" title %glue-eoc%))
  (force-output %glue-stream%))

(defun glue-show-tree (top &optional (input *sentence*))
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
    (format %glue-stream% "#T[~a ~s ~s ~a ~a " id label form n rule)
    (loop
        for daughter in (if form (get (first daughters) 'daughters) daughters)
        do (glue-show-tree daughter))
    (format %glue-stream% "]")))

(defun glue-display-fs (tdfs title id)
  (declare (ignore id))
  (let* ((id (lsp-store-object nil nil tdfs))
         (dag (tdfs-indef tdfs)))
    (let ((string (with-output-to-string (stream)
                    (format stream "avm ~d " id)
                    (display-dag1 dag 'linear stream))))
      (format %glue-stream% string))
    (format %glue-stream% " ~s~a~%" title %glue-eoc%))
  (force-output %glue-stream%))

(defun glue-display-mrs (mrs)
  (let* ((id (lsp-store-object nil nil mrs))
         (dag (mrs::psoa-to-dag mrs))
         (title "Simple MRS Display"))
    (let ((string (with-output-to-string (stream)
                    (when *glue-hidden-features*
                      (format 
                       stream 
                       "parameter+ hidden-features ~a~a~%"
                       (first *glue-hidden-features*) %glue-eoc%)
                      (loop
                          for foo in (rest *glue-hidden-features*)
                          do
                            (format 
                             stream 
                             "parameter+ hidden-features ~a~a~%"
                             foo %glue-eoc%)))
                    (format stream "avm ~d " id)
                    (display-dag1 dag 'linear stream))))
      (format %glue-stream% string))
    (format %glue-stream% " ~s~a~%" title %glue-eoc%))
  (force-output %glue-stream%))

(defun glue-status-p (key)
  (case key
    #+:null
    (:tree (streamp %glue-stream%))
    #-:null
    (:avm (streamp %glue-stream%))
    #-:null
    (:mrs (streamp %glue-stream%))))
