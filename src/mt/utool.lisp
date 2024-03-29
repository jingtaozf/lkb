(in-package :mt)

;;;
;;; Copyright (c) 2004 -- 2006 Stephan Oepen (oe@csli.stanford.edu)
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

(defparameter *utool-binary*
  #+:logon
  (let* ((root (system:getenv "LOGONROOT"))
         (root (and root (namestring (parse-namestring root)))))
    (when root
      (format nil "exec ~a/bin/utool" root)))
  #-:logon
  nil)

(defparameter *utool-options*
  '((:solve . "solve -I mrs-prolog -O plugging-lkb")
    (:classify . "classify -I mrs-prolog")))

(let ((lock (mp:make-process-lock)))

  (defun utool-process (mrs &key (action :classify))
    (unless *utool-binary* (return-from utool-process))
    
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
                        (format
                         nil
                         "~a ~a '~a'"
                         *utool-binary* options in)))
             (mrs::*output-ignored-roles* mrs::*scoping-ignored-roles*))
        (when command
          (with-open-file (stream in
                           :direction :output :if-exists :supersede)
            (mrs::output-mrs1 mrs 'mrs::prolog stream))
          (when (probe-file out) (delete-file out))
          (let ((status
                 (run-process 
                  command
                  :wait t
                  :output out :if-output-exists :supersede :input "/dev/null" 
                  :error-output "/dev/null" :if-error-output-exists :append))
                bindings)
            (case action
              (:solve
               (when (probe-file out)
                 (with-open-file (stream out :direction :input)
                   (setf bindings (ignore-errors (read stream)))))))
            (ignore-errors (delete-file in))
            (ignore-errors (delete-file out))
            (case action
              (:solve bindings)
              (:classify status))))))))

(defun utool-net-p (mrs)
  (let ((status (utool-process mrs)))
    (when (or (not (integerp status)) (zerop (logand status 16)))
      (error "~%MRS is not hyper-normally connected"))
    status))
