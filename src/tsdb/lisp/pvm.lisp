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

(defparameter *pvm-cpus*
  (list
   (make-cpu 
    :host "eo.Stanford.EDU"
    :spawn "/eo/e4/malouf/acl50/base"
    :options '("-I" "/user/oe/tmp/lkb.dxl" "-L" "/user/oe/lpvm.lisp")
    :class :csli :threshold 2)
   (make-cpu 
    :host "eoan"
    :spawn "/eo/e4/malouf/acl50/base"
    :options '("-I" "/user/oe/tmp/lkb.dxl" "-L" "/user/oe/lpvm.lisp")
    :class :csli :threshold nil)
   (make-cpu 
    :host "top.coli.uni-sb.de"
    :spawn "/proj/perform/nacl/home/lisp"
    :options '("-I" "/proj/perform/images/lkb.dxl" "-qq" 
               "-L" "/proj/perform/lpvm.lisp")
    :class '(:external :coli) :threshold 3.5)
   (make-cpu 
    :host "top.coli.uni-sb.de"
    :spawn "/proj/perform/nacl/home/lisp"
    :options '("-I" "/proj/perform/images/lkb.dxl" "-qq" 
               "-L" "/proj/perform/lpvm.lisp")
    :class '(:external :coli) :threshold 2.5)
   (make-cpu 
    :host "limit.dfki.uni-sb.de"
    :spawn "/proj/perform/nacl/home/lisp"
    :options '("-I" "/proj/perform/images/lkb.dxl" "-qq" 
               "-L" "/proj/perform/lpvm.lisp")
    :class '(:external :dfki) :threshold 4)
   (make-cpu 
    :host "limit.dfki.uni-sb.de"
    :spawn "/proj/perform/nacl/home/lisp"
    :options '("-I" "/proj/perform/images/lkb.dxl" "-qq" 
               "-L" "/proj/perform/lpvm.lisp")
    :class '(:external :dfki) :threshold 2.5)
   (make-cpu 
    :host "let.dfki.uni-sb.de"
    :spawn "/proj/perform/nacl/home/lisp"
    :options '("-I" "/proj/perform/images/lkb.dxl" "-qq" 
               "-L" "/proj/perform/lpvm.lisp")
    :class '(:external :dfki) :threshold 8)
   (make-cpu 
    :host "let.dfki.uni-sb.de"
    :spawn "/proj/perform/nacl/home/lisp"
    :options '("-I" "/proj/perform/images/lkb.dxl" "-qq" 
               "-L" "/proj/perform/lpvm.lisp")
    :class '(:external :dfki) :threshold 6)
   (make-cpu 
    :host "let.dfki.uni-sb.de"
    :spawn "/proj/perform/nacl/home/lisp"
    :options '("-I" "/proj/perform/images/lkb.dxl" "-qq" 
               "-L" "/proj/perform/lpvm.lisp")
    :class '(:external :dfki) :threshold 4)
   (make-cpu 
    :host "let.dfki.uni-sb.de"
    :spawn "/proj/perform/nacl/home/lisp"
    :options '("-I" "/proj/perform/images/lkb.dxl" "-qq" 
               "-L" "/proj/perform/lpvm.lisp")
    :class '(:external :dfki) :threshold 4)))

(defparameter *pvm-tasks* nil)

(defparameter *pvm-master* nil)

(defun run-status (run)
  (let ((task (get-field :task run)))
    (and task (task-status task))))

(defun task-idle-p (task)
  (eq (task-status task) :ready))

(defun initialize-cpus (&key (cpus *pvm-cpus*)
                             (classes '(:csli))
                             (file (format 
                                    nil 
                                    "/tmp/pvm.debug.~a"
                                    (current-user))))
  (pvm_start :user (current-user))
  ;;
  ;; first, create as many tasks as we have cpus ...
  ;;
  (setf *pvm-master* (pvm_register file))
  (setf *pvm-tasks* nil)
  (loop
      with classes = (if (consp classes) classes (list classes))
      with allp = (member :all classes :test #'eq)
      for cpu in cpus
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
         *tsdb-io*
         "initialize-cpus(): `~a' communication error [~d].~%"
         (cpu-host cpu) tid)
      when task
      do
        (push (make-task :tid tid :task task :cpu cpu :status :start)
              *pvm-tasks*))
  ;;
  ;; ... then, wait for them to register (start talking) with us.
  ;;
  (loop
      while (and *pvm-tasks* 
                 (find :start *pvm-tasks* :key #'task-status))
      for message = (pvm_poll -1 -1 5)
      when (message-p message)
      do
        (let* ((tag (message-tag message))
               (remote (message-remote message))
               (content (message-content message))
               (task (find remote *pvm-tasks* :key #'task-tid)))
          (cond
           ((eql tag %pvm_task_fail%)
            (let* ((remote (message-corpse message))
                   (task (find remote *pvm-tasks* :key #'task-tid)))
              (setf *pvm-tasks* (delete task *pvm-tasks*))))

           ((null task)
            (when *pvm-debug-p*
              (format
               *tsdb-io*
               "~&initialize-cpus(): ~
                ignoring message from alien <~d>:~%~s~%~%"
               remote message)
              (force-output)))
           
           ((eql tag %pvm_lisp_message%)
            (cond
             ((and (eq (first content) :register)
                   (eq (second content) (task-tid task)))
              (setf (task-status task) :ready)
              (format
               *tsdb-io*
               "initialize-cpus(): `~a' registered (tid ~d).~%"
               (cpu-host (task-cpu task)) (task-tid task)))
             (t
              (when *pvm-debug-p*
                (format
                 *tsdb-io*
                 "~&initialize-cpus(): ~
                  ignoring unexpected message from <~d>:~%~s~%~%"
                 remote message)
                (force-output)))))

           (t
            (when *pvm-debug-p*
              (format
               *tsdb-io*
               "~&initialize-cpus(): ~
                ignoring dubious message from <~d>:~%~s~%~%"
               remote message)
              (force-output)))))))

(defun evaluate (form)
  (eval form))

(defun slave (&optional orphan)

  (let* ((self (pvm_register t))
         (master (if orphan nil (pvm_parent)))
         (*package* (find-package "TSDB"))
         (*print-readably* nil))
  
    (unless (and (not orphan)
                 (or (= master %pvm_no_parent%)
                     (<= master 0)))
      (when master
       (pvm_transmit
        master %pvm_lisp_message%
        (list :register self)))
      (when *pvm-debug-p*
        (format 
         t 
         "slave(): tid ~d~@[ (parent ~d)~] waiting for requests.~%" 
         self master master)
        (force-output))
      (loop 
          for message = (pvm_poll (or master -1) -1 5)
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
