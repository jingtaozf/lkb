(in-package :mt)

(defparameter *utool-binary*
  #+:logon
  (let* ((root (system:getenv "LOGONROOT"))
         (root (and root (namestring (parse-namestring root)))))
    (when root
      (format nil "exec ~a/bin/utool" root))))

(defparameter *utool-options*
  '((:solve . "solve -I mrs-prolog -O plugging-lkb")
    (:classify . "classify -I mrs-prolog")))

(let ((lock (mp:make-process-lock)))

  (defun utool-process (mrs &key (action :classify))
    (mp:with-process-lock (lock)
      (let* ((in (format 
                  nil 
                  "/tmp/.utool.io.~a.~a.in" 
                  (lkb::current-user) (lkb::current-pid)))
             (out (format 
                   nil 
                   "/tmp/.utool.io.~a.~a.out" 
                   (lkb::current-user) (lkb::current-pid)))
             (options (rest (assoc action *utool-options*)))
             (command (when options
                        (format nil "~a ~a" *utool-binary* options))))
        (when command
          (with-open-file (stream in
                           :direction :output :if-exists :supersede)
            (mrs::output-mrs1 mrs 'mrs::prolog stream))
          (when (probe-file out) (delete-file out))
          (let ((status
                 (run-process 
                  command
                  :wait t
                  :output out :if-output-exists :supersede :input in 
                  :error-output "/dev/null" :if-error-output-exists :append))
                bindings)
            (case action
              (:solve
               (when (probe-file out)
                 (with-open-file (stream out :direction :input)
                   (setf bindings (ignore-errors (read stream)))))))
            #+:null
            (ignore-errors (delete-file in))
            (ignore-errors (delete-file out))
            (case action
              (:solve bindings)
              (:classify status))))))))
  

