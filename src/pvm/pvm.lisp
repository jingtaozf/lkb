(in-package "PVM")

(eval-when (:load-toplevel :compile-toplevel :execute)
  (require :foreign))

(defconstant %pvm_no_parent% -23)
(defconstant %pvm_task_fail% 42)
(defconstant %pvm_lisp_message% 50)
(defconstant %pvm_c_message% 51)
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

(defstruct cpu 
  host spawn options 
  architecture class threshold name template
  preprocessor reader
  create complete)

(defstruct client tid cpu task protocol form status load)

(defun current-user ()
  (or #+(and :allegro-version>= (version>= 5 0)) 
      (sys:user-name)
      #+(and :allegro (not (and :allegro-version>= (version>= 5 0))))
      (system:getenv "USER")
      #+(and :mcl :powerpc) 
      (ccl:process-name ccl:*current-process*)
      #+:lucid 
      (lcl:environment-variable "USER")
      "nobody"))

#+(and :allegro-version>= (version>= 5 0))
(def-foreign-call 
    (current-pid "getpid")
    (:void)
  :returning :int)

#+(and :allegro-version>= (not (version>= 5 0)))
(defforeign 
    'current-pid
    :entry-point "getpid"
    :arguments nil
    :return-type :integer)

#-:allegro-version>=
(defun getpid () (random (expt 2 15)))

#+(version>= 5 0)
(def-foreign-call pvm_mytid (:void) :returning :int)

#-(version>= 5 0)
(defforeign 'pvm_mytid :arguments nil :return-type :integer)

(let ((user (current-user))
      tid)
  (defun make-tmp-file (prefix)
    (unless tid (setf tid (pvm_mytid)))
    (let ((file (format 
                 nil 
                 "/tmp/~a.~a.~a.~a" 
                 prefix user tid (gensym ""))))
    (when (probe-file file) (delete-file file))
    file)))

#+(version>= 5 0)
(def-foreign-call load_average (:void) :returning :double)

#-(version>= 5 0)
(defforeign 'load_average :arguments nil :return-type :double-float)

#+(version>= 5 0)
(def-foreign-call 
    (_pvm_register "pvm_register")
    ((file (* :char) string)
     (debugp :int integer))
  :returning :int
  #+(version>= 6 0) :strings-convert #+(version>= 6 0) t)

#-(version>= 5 0)
(defforeign
    '_pvm_register :entry-point "pvm_register"
    :arguments '(string integer)
    :return-type :integer)

(defun pvm_register (&optional (file t) debugp)
  (let ((file (cond
               ((or (eq file t) (equal file "")) "")
               ((eq file nil) "/dev/null")
               ((stringp file) file)
               (t "")))
        (debugp (if debugp 1 0)))
    (_pvm_register file debugp)))

#+(version>= 5 0)
(def-foreign-call pvm_quit (:void) :returning :int)

#-(version>= 5 0)
(defforeign 'pvm_quit :arguments nil :return-type :integer)

#+(version>= 5 0)
(def-foreign-call pvmendtask (:void) :returning :int)

#-(version>= 5 0)
(defforeign 'pvmendtask :arguments nil :return-type :integer)

#+(version>= 5 0)
(def-foreign-call pvm_kill ((tid :int integer)) :returning :int)

#-(version>= 5 0)
(defforeign 'pvm_kill :arguments '(integer) :return-type :integer)

#+(version>= 5 0)
(def-foreign-call pvm_parent (:void) :returning :int)

#-(version>= 5 0)
(defforeign 'pvm_parent :arguments nil :return-type :integer)


#+(version>= 5 0)
(def-foreign-call 
    (_pvm_create "pvm_create")
    ((task (* :char) string)
     (argv (* (* :char)) 
           #+(not (version>= 6 0)) (simple-array simple-string (*)))
     (host (* :char) string)
     (architecture (* :char) string))
  :returning :int
  #+(version>= 6 0) :strings-convert #+(version>= 6 0) t)

#-(version>= 5 0)
(defforeign 
    '_pvm_create :entry-point "pvm_create"
    :arguments '(string (simple-array simple-string (*)) string string)
    :return-type :integer)

(defun pvm_create (task arguments &key host architecture)
  (let* ((arguments
          (when (consp arguments)
            (loop
                for argument in arguments
                for string = (if (stringp argument) 
                               argument
                               (format nil "~(~s~)" argument))
                for native = #+:ics (excl:string-to-native string)
                             #-:ics string
                collect native)))
         (argv
          #+:ics
          (let* ((size (+ (length arguments) 1))
                 (vector (allocate-fobject (list :array '(* :char) size))))
            (loop
                for argument in arguments
                for i from 0
                do
                  (setf (fslot-value-typed '(:array (* :char)) nil vector i)
                    argument)
                finally 
                  (incf i)
                  (setf (fslot-value-typed '(:array (* :char)) nil vector i)
                    0))
            vector)
          #-:ics
          (make-array (+ (length arguments) 1)
                                :initial-contents (append arguments '(0))))
         (host (if host (string host) ""))
         (architecture 
          (if (or host (null architecture)) "" (string architecture)))
         (result (_pvm_create task argv host architecture)))
    #+:ics
    (loop
        for argument in arguments
        when (integerp argument) do (excl:aclfree argument))
    result))

#+(version>= 5 0)
(def-foreign-call pvm_flush (:void) :returning :int)

#-(version>= 5 0)
(defforeign 'pvm_flush :arguments nil :return-type :integer)

#+(version>= 5 0)
(def-foreign-call 
    (_pvm_poll "pvm_poll")
    ((tid :int integer)
     (tag :int integer)
     (block :int integer)
     #-:ics
     (output (* :char) string)
     #+:ics
     (output :int integer)
     (size :int integer))
  :returning :int
  #+(version>= 6 0) :strings-convert #+(version>= 6 0) nil)

#-(version>= 5 0)
(defforeign
    '_pvm_poll :entry-point "pvm_poll"
    :arguments '(integer integer integer string integer)
    :return-type :integer)

#+(version>= 5 0)
(def-foreign-call 
    pvm_collect 
    (#-:ics
     (output (* :char) string)
     #+:ics
     (output :int integer)
     (size :int integer))
  :returning :int
  #+(version>= 6 0) :strings-convert #+(version>= 6 0) nil)

#-(version>= 5 0)
(defforeign
    'pvm_collect
    :arguments '(string integer)
    :return-type :integer)

(let* ((size 4096)
       (output (make-array 
                (+ size 1) :element-type 'character :allocation :static))
       #+:ics
       (output (excl:lispval-other-to-address output)))
  (defun pvm_poll (tid tag block)
    (let* ((block (cond ((null block) 0)
                        ((numberp block) (round block))
                        (t -1)))
           (status (_pvm_poll tid tag block output size)))
      (cond
       ((zerop status) nil)
       ((< status 0) :error)
       ((> status size)
        (setf size (+ status 4096))
        ;;
        ;; _fix_me_
        ;; we would want to free the old .output. array here; apparently, the
        ;; way recommended in the 5.0.1 documentation --- viz. aclfree() plus 
        ;; lispval-other-to-address() --- is unavailable because the latter
        ;; function does not exist :-{.                     (16-feb-00  -  oe)
        ;;
        ;; it seems, ACL 6.0 has lispval-other-to-address().   (13-sep-01; oe)
        ;;
        (when (integerp output) (excl:aclfree output))
        (let ((new (make-array 
                    (+ size 1) :element-type 'character :allocation :static)))
          (setf output #-:ics new #+:ics (excl:lispval-other-to-address new)))
        (let ((status (pvm_collect output size)))
          (if (< status 0)
            :error
            (multiple-value-bind (result condition)
                (ignore-errors 
                 (let ((string 
                        #-:ics output #+:ics (excl:native-to-string output)))
                   (when *pvm-debug-p*
                     (format t "pvm_poll(): read `~a'.~%" string))
                   (read-from-string string t nil :end status)))
              (when (and (null result) condition)
                (format
                 t
                 "~&pvm_poll(): error `~a'.~%" condition))
              (or result :error)))))
       (t
        (multiple-value-bind (result condition)
            (ignore-errors 
             (let ((string 
                    #-:ics output #+:ics (excl:native-to-string output)))
               (when *pvm-debug-p*
                 (format t "pvm_poll(): read `~a'.~%" string))
               (read-from-string string t nil :end status)))
          (when (and (null result) condition)
            (format
             t
             "~&pvm_poll(): error `~a'.~%" condition))
          (or result :error)))))))

#+(version>= 5 0)
(def-foreign-call 
    (_pvm_transmit "pvm_transmit")
    ((tid :int integer)
     (tag :int integer)
     (file (* :char) string))
  :returning :int
  #+(version>= 6 0) :strings-convert #+(version>= 6 0) t)

#-(version>= 5 0)
(defforeign
    '_pvm_transmit :entry-point "pvm_transmit"
    :arguments '(integer integer string)
    :return-type :integer)

(defun pvm_transmit (tid tag form)
  (let* ((*print-readably* nil)
         (string (format nil "~s" form))
         (status (_pvm_transmit tid tag string)))
    status))

#+(version>= 5 0)
(def-foreign-call pvm_tidtohost ((tid :int integer)) :returning :int)

#-(version>= 5 0)
(defforeign 'pvm_tidtohost :arguments '(integer) :return-type :integer)

#+(version>= 5 0)
(def-foreign-call 
    (_pvm_vm_info "pvm_vm_info")
    ((file (* :char) string))
  :returning :int
  #+(version>= 6 0) :strings-convert #+(version>= 6 0) t)

#-(version>= 5 0)
(defforeign 
    '_pvm_vm_info :entry-point "pvm_vm_info"
    :arguments '(string)
    :return-type :integer)

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

#+(version>= 5 0)
(def-foreign-call 
    (pvm_announce "pvm_announce")
    ((class (* :char) string)
     (version (* :char) string)
     (user (* :char) string))
  :returning :int
  #+(version>= 6 0) :strings-convert #+(version>= 6 0) t)

#-(version>= 5 0)
(defforeign
    'pvm_announce :entry-point "pvm_announce"
    :arguments '(string string string)
    :return-type :integer)

#+(version>= 5 0)
(def-foreign-call 
    (pvm_delinfo "pvm_delinfo")
    ((class (* :char) string)
     (index :int integer)
     (flags :int integer))
  :returning :int
  #+(version>= 6 0) :strings-convert #+(version>= 6 0) t)

#-(version>= 5 0)
(defforeign
    'pvm_delinfo :entry-point "pvm_delinfo"
    :arguments '(string integer integer)
    :return-type :integer)

(defun pvm_retract (class)
  (pvm_delinfo class 0 0))

#+(version>= 5 0)
(def-foreign-call 
    (_pvm_task_info "pvm_task_info")
    ((tid :int integer)
     (file (* :char) string))
  :returning :int
  #+(version>= 6 0) :strings-convert #+(version>= 6 0) t)

#-(version>= 5 0)
(defforeign
    '_pvm_task_info :entry-point "pvm_task_info"
    :arguments '(integer string)
    :return-type :integer)

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

(defun pvm_halt (&key (user (system:getenv "USER")))
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

(defun pvm_start (&key reset (user (system:getenv "USER")))
  (pvm_quit)
  (when reset (pvm_halt :user user))
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
         
(defun revaluate (tid form 
                  &optional (block t) 
                  &key (verbose t) (key 0) interrupt)
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
          return :error
          when (and (message-p message)
                    (eql (message-tag message) %pvm_lisp_message%)
                    (eq (first (message-content message)) :return)
                    (eql (second (message-content message)) key))
          return (third (message-content message))
          else when (interrupt-p interrupt)
          do
            (return-from revaluate :interrupt))
      :ok)))

(defun interrupt-p (interrupt)
  (when (and interrupt (probe-file interrupt))
    (delete-file interrupt)
    t))
