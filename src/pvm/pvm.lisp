(in-package "PVM")

(eval-when (:load-toplevel :compile-toplevel :execute)
  (require :foreign))

(defconstant %pvm_no_parent% -23)
(defconstant %pvm_task_fail% 42)
(defconstant %pvm_lisp_message% 50)

(defparameter 
  *pvm*
  (format
   nil "exec ~a"
   (namestring (make-pathname :directory (pathname-directory make::bin-dir)
                              :name "pvm"))))
(defparameter 
  *pvmd*
  (format
   nil "exec ~a"
   (namestring (make-pathname :directory (pathname-directory make::bin-dir)
                              :name "pvmd3"))))

(defparameter *pvm-debug-p* nil)
(defparameter *pvm-pending-events* nil)

(defstruct cpu host spawn options initialize architecture class threshold)
(defstruct task tid task cpu form status load)

(defun make-tmp-file (prefix)
  (let ((file (format 
               nil 
               "/tmp/~a.~a.~a.~a" 
               prefix (system:getenv "USER") (pvm_mytid) (gensym ""))))
    (when (probe-file file) (delete-file file))
    file))

(def-foreign-call 
    (_pvm_register "pvm_register")
    ((file (* :char) string))
  :returning :int)

(defun pvm_register (&optional (file t))
  (let ((file (cond
               ((or (eq file t) (equal file "")) "")
               ((eq file nil) "/dev/null")
               ((stringp file) file)
               (t ""))))
    (_pvm_register file)))

(def-foreign-call 
    pvm_mytid
    (:void)
  :returning :int)

(def-foreign-call 
    pvm_quit
    (:void)
  :returning :int)

(def-foreign-call 
    (_pvm_halt "pvm_halt")
    (:void)
  :returning :int)
(def-foreign-call 
    pvmendtask
    (:void)
  :returning :int)

(defun pvm_halt ()
  (_pvm_halt)
  (pvm_quit))

(def-foreign-call 
    pvm_kill
    ((tid :int integer))
  :returning :int)

(def-foreign-call 
    pvm_parent 
    (:void)
  :returning :int)

(def-foreign-call 
    (_pvm_create "pvm_create")
    ((task (* :char) string)
     (argv (* (* :char)) (simple-array simple-string (*)))
     (host (* :char) string)
     (architecture (* :char) string))
  :returning :int)

(defun pvm_create (task arguments &key host architecture)
  (let ((arguments (if (and arguments 
                            (consp arguments)
                            (every #'(lambda (foo) 
                                       (or (stringp foo) (symbolp foo)))
                                   arguments))
                     (make-array (+ (length arguments) 1)
                                 :initial-contents (append arguments '(0)))
                     (make-array 1 :initial-contents '(0))))
        (host (if host (string host) ""))
        (architecture 
         (if (or host (null architecture)) "" (string architecture))))
  (_pvm_create task arguments host architecture)))

(def-foreign-call  
    pvm_flush 
    (:void) 
  :returning :int)

(def-foreign-call 
    (_pvm_poll "pvm_poll")
    ((tid :int integer)
     (tag :int integer)
     (block :int integer)
     (file (* :char) string))
  :returning :int)

(defun pvm_poll (tid tag block)
  (let* ((file (make-tmp-file ".pvm.io"))
         (block (cond ((null block) 0)
                      ((numberp block) (round block))
                      (t -1)))
         (status 
          (_pvm_poll tid tag block file)))
    (cond
     ((zerop status) nil)
     ((or (< status 0) (not (probe-file file))) :error)
     (t
      (let* ((buffer (make-string status))
             (length (with-open-file (stream file :direction :input)
                       (read-sequence buffer stream))))
        (unless *pvm-debug-p* (delete-file file))
        (when (eq status length)
          (multiple-value-bind (result condition)
              (ignore-errors (read-from-string buffer))
            (when (and (null result) condition *pvm-debug-p*)
              (format
               t
               "~&pvm_poll(): error `~a'.~%" condition))
            (or result :error))))))))
              

(def-foreign-call 
    (_pvm_transmit "pvm_transmit")
    ((tid :int integer)
     (tag :int integer)
     (file (* :char) string))
  :returning :int)

(defun pvm_transmit (tid tag form)
  (let ((file (make-tmp-file ".pvm.io"))
        (*print-readably* nil))
    (with-open-file (stream file
                     :direction :output
                     :if-does-no-exist :create :if-exists :supersede)
      (format stream "~s" form))
    (when (probe-file file)
      (let ((status (_pvm_transmit tid tag file)))
        (unless *pvm-debug-p* (delete-file file))
        status))))

(def-foreign-call 
    (pvm_tidtohost "pvm_tidtohost")
    ((tid :int integer))
  :returning :int)

(def-foreign-call 
    (_pvm_vm_info "pvm_vm_info")
    ((file (* :char) string))
  :returning :int)

(defun pvm_vm_info ()
  (let ((file (make-tmp-file ".pvm.io")))
    (let ((status (_pvm_vm_info file)))
      (cond
       ((zerop status) nil)
       ((or (< status 0) (not (probe-file file))) :error)
       (t
        (let* ((buffer (make-string status))
               (length (with-open-file (stream file :direction :input)
                         (read-sequence buffer stream))))
          (unless *pvm-debug-p* (delete-file file))
          (when (eq status length)
            (multiple-value-bind (result condition)
                (ignore-errors (read-from-string buffer))
              (when (and (null result) condition *pvm-debug-p*)
                (format
                 t
                 "~&pvm_vm_info(): error `~a'.~%" condition))
              (or result :error)))))))))

(def-foreign-call 
    (_pvm_task_info "pvm_task_info")
    ((tid :int integer)
     (file (* :char) string))
  :returning :int)

(defun pvm_task_info (&optional (tid 0))
  (let ((file (make-tmp-file ".pvm.io" )))
    (let ((status (_pvm_task_info tid file)))
      (cond
       ((zerop status) nil)
       ((or (< status 0) (not (probe-file file))) :error)
       (t
        (let* ((buffer (make-string status))
               (length (with-open-file (stream file :direction :input)
                         (read-sequence buffer stream))))
          (unless *pvm-debug-p* (delete-file file))
          (when (eq status length)
            (multiple-value-bind (result condition)
                (ignore-errors (read-from-string buffer))
              (when (and (null result) condition *pvm-debug-p*)
                (format
                 t
                 "~&pvm_task_info(): error `~a'.~%" condition))
              (or result :error)))))))))

(defun pvm_start (&key reset (user (system:getenv "USER")))
  (when reset
    (pvm_quit)
    (let ((file (format nil "/tmp/.pvm.halt.~a.~a" user (gensym ""))))
      (with-open-file (stream file :direction :output :if-exists :supersede)
        (format stream "halt~%"))
      (run-process 
       *pvm* :wait t 
       :output "/dev/null" :if-output-exists :append
       :error-output "/dev/null" :if-error-output-exists :append
       :input file)
      (when (probe-file file) (delete-file file))))
  (let ((status (run-process 
                 *pvmd* :wait t 
                 :output "/dev/null" :if-output-exists :append
                 :error-output "/dev/null" :if-error-output-exists :append)))
    (if (zerop status) :ok :error)))

(defmacro message-p (message)
  `(and (consp ,message)
        (find :tag ,message :key #'first)
        (find :remote ,message :key #'first)))

(defmacro message-tag (message)
  `(rest (assoc :tag ,message)))
(defmacro message-load (message)
  `(rest (assoc :load ,message)))
(defmacro message-remote (message)
  `(rest (assoc :remote ,message)))
(defmacro message-content (message)
  `(rest (assoc :content ,message)))
(defmacro message-corpse (message)
  `(rest (assoc :corpse ,message)))

(defun shutdown (tid)
  (pvm_transmit tid %pvm_lisp_message% '(:shutdown)))

(defun tid-status (tid)
  (find tid (rest (assoc :tasks (pvm_task_info tid))) 
        :key #'(lambda (task) (rest (assoc :tid task)))))
         
(defun revaluate (tid form &optional (block t) &key (verbose t) (key 0))
  (when (and block (null (tid-status tid)))
    (when verbose
      (format
       t
       "revaluate(): invalid task id ~d.~%" tid))
    (return-from revaluate :error))
  (if (< (pvm_transmit tid %pvm_lisp_message% (list :eval key form)) 0)
    :error
    (if block
      (loop
          for message = (pvm_poll tid %pvm_lisp_message% block)
          when (eq message :error)
          return nil
          when (and (message-p message)
                    (eql (message-tag message) %pvm_lisp_message%)
                    (eq (first (message-content message)) :return)
                    (eql (second (message-content message)) key))
          return (third (message-content message))
          else
          do (when message (push message *pvm-pending-events*)))
      :ok)))