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

(defparameter *pvm-master* nil)

(defparameter *pvm-protocol* 1)

(defun client-idle-p (client)
  (eq (client-status client) :ready))

(defun initialize-cpus (&key cpus
                             (classes '(:all))
                             (reset t)
                             block
                             (file (format 
                                    nil 
                                    "/tmp/pvm.debug.~a"
                                    (current-user)))
                             (prefix "")
                             (stream *tsdb-io*))

  (initialize-tsdb)
  (when reset                           
    (pvm_quit)
    (pvm_start :user (current-user))
    (sleep 2)
    (setf *pvm-master* (pvm_register file *pvm-debug-p*))
    (setf *pvm-clients* nil))
  (when (and file (null reset))
    (format
     stream
     "~&~ainitialize-cpus(): ignoring (protocol) `file' argument (no reset).~%"
     prefix ))
  ;;
  ;; first, create as many clients as we have cpus ...
  ;;
  (loop
      with classes = (if (consp classes) classes (list classes))
      with allp = (member :all classes :test #'eq)
      for cpu in (or cpus *pvm-cpus*)
      for class = (let ((class (cpu-class cpu)))
                    (if (listp class) class (list class)))
      for tid = (when (or allp (intersection class classes))
                  (pvm_create (cpu-spawn cpu) (cpu-options cpu)
                              :host (cpu-host cpu) 
                              :architecture (cpu-architecture cpu)))
      for task = (when (and (integerp tid) (> tid 0)) (tid-status tid))
      when (and tid (null task))
      do
        (format
         stream
         "~ainitialize-cpus(): `~a' communication error [~d].~%"
         prefix (cpu-host cpu) tid)
      when task
      do
        (push (make-client :tid tid :task task :cpu cpu :status :start)
              *pvm-clients*)
        (when block
          (wait-for-clients :block tid :prefix prefix :stream stream)))
  ;;
  ;; ... then, wait for them to register (start talking) with us.
  ;;
  (wait-for-clients :prefix prefix :stream stream))

(defun wait-for-clients (&key block (prefix "  ") (stream *tsdb-io*))
  (loop
      while (and *pvm-clients* 
                 (find :start *pvm-clients* :key #'client-status))
      for message = (pvm_poll -1 -1 1)
      when (message-p message)
      do
        (let* ((tag (message-tag message))
               (remote (message-remote message))
               (content (message-content message))
               (client (find remote *pvm-clients* :key #'client-tid)))
          (cond
           ((eql tag %pvm_task_fail%)
            (let* ((remote (message-corpse message))
                   (client (find remote *pvm-clients* :key #'client-tid)))
              (when (and (client-p client) (cpu-p (client-cpu client)))
                (format
                 stream
                 "~&~await-for-clients(): client exit for `~a' <~a>~%"
                 prefix (cpu-host (client-cpu client)) remote))
              (setf *pvm-clients* (delete client *pvm-clients*))))
           
           ((null client)
            (when *pvm-debug-p*
              (format
               stream
               "~&~await-for-clients(): ~
                ignoring message from alien <~d>:~%~s~%~%"
               prefix remote message)
              (force-output)))
           
           ((eql tag %pvm_lisp_message%)
            (cond
             ((and (eq (first content) :register)
                   (eq (second content) (client-tid client)))
              (setf (client-status client) :ready)
              (setf (client-protocol client) (third content))
              (format
               stream
               "~await-for-clients(): `~a' registered as tid <~d>.~%"
               prefix (cpu-host (client-cpu client)) (client-tid client))
              (when (and block (eql (client-tid client) block))
                (return-from wait-for-clients client)))
             (t
              (when *pvm-debug-p*
                (format
                 stream
                 "~&~await-for-clients(): ~
                  ignoring unexpected message from <~d>:~%~s~%~%"
                 prefix remote message)
                (force-output)))))

           (t
            (when *pvm-debug-p*
              (format
               stream
               "~&~await-for-clients(): ~
                ignoring dubious message from <~d>:~%~s~%~%"
               stream remote message)
              (force-output)))))))

(defun evaluate (form)
  (eval form))

(defun slave (&optional orphan)

  (let* ((self (pvm_register t *pvm-debug-p*))
         (master (if orphan nil (pvm_parent)))
         (*package* (find-package "TSDB"))
         (*print-readably* nil))
  
    (unless (and (not orphan)
                 (or (= master %pvm_no_parent%)
                     (<= master 0)))
      (when master
       (pvm_transmit
        master %pvm_lisp_message%
        (list :register self :lisp)))
      (when *pvm-debug-p*
        (format 
         t 
         "slave(): tid ~d~@[ (parent ~d)~] waiting for requests.~%" 
         self master master)
        (force-output))
      (loop 
          for message = (pvm_poll -1 -1 1)
          while (or (null message)
                    (and (not (eq message :error))
                         (not (eql (message-tag message) 
                                   %pvm_task_fail%))))
          when message
          do
            (let* ((tag (message-tag message))
                   (content (message-content message))
                   (action (when (eq tag %pvm_lisp_message%)
                             (first content)))
                   (key (when action (second content)))
                   (form (when action (third content))))
              (when *pvm-debug-p*
                (format
                 t
                 "slave(): received `~a'.~%" message)
                (force-output))
              (cond
               ((= tag %pvm_lisp_message%)
                (case action
                  (:eval
                   (multiple-value-bind (result condition)
                       (ignore-errors (evaluate form))
                     (pvm_transmit
                      (or master (message-remote message)) 
                      %pvm_lisp_message%
                      (if (and (null result) condition)
                        (list :error key (format nil "~a" condition))
                        (list :return key result)))))
                   (:shutdown
                    (pvm_quit)
                    (excl:exit (if (eq tag %pvm_task_fail%) 1 0) 
                               :no-unwind t :quit t))))
               ((= tag %pvm_task_fail%)
                (pvm_quit)
                (excl:exit (if (eq tag %pvm_task_fail%) 1 0) 
                           :no-unwind t :quiet t))
               (t
                (format
                 t
                 "slave(): unexpected message: `~a'~%" message)
                (force-output)))))
      (pvm_quit)
      (excl:exit 1 :no-unwind t :quiet t))))
