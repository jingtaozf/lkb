(in-package :mt)

;;;
;;; Copyright (c) 2007 -- 2007 Stephan Oepen (oe@csli.stanford.edu)
;;; Copyright (c) 2007 -- 2007 Erik Velldal (erikve@ifi.uio.no)
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

(defparameter *smt-directory*
  #+:logon
  (namestring (dir-append (get-sources-dir "mt") '(:relative "mt")))
  #-:logon
  nil)

(defparameter *smt* nil)

(defstruct smt
  lfn lnf mlfn mlnf)

(defstruct wp
  in out weight)

(defun initialize-smt ()
  #+:logon
  (setf *smt-directory*
    (namestring (dir-append (get-sources-dir "mt") '(:relative "mt")))))

(defun read-smt (&key file hash)
  (if file
    (multiple-value-bind (input foo pid)
        (run-process
         (format nil "gzip -d -c '~a'" (namestring file))
         :wait nil
         :output :stream :input "/dev/null"
         :error-output "/dev/null" :if-error-output-exists :append)
      (declare (ignore foo))
      (let* ((file (pathname file))
             (name (format 
                    nil 
                    "~a~@[.~a~]"
                    (pathname-name file) (pathname-type file))))
        (format t "read-smt(): reading file `~a' ... " name)
        (loop
            for line = (read-line input nil nil)
            while line do
              (multiple-value-bind (foo matches)
                  (cl-ppcre:scan-to-strings "^([^ ]+) ([^ ]+) ([^ ]+)$" line)
                (declare (ignore foo))
                (when (= (array-total-size matches) 3)
                  ;;
                  ;; _fix_me_
                  ;; given the file names i received from erik, it looks like 
                  ;; the order on each line is reversed, i.e. out <-- in?
                  ;;                                            (5-jan-07; oe)
                  (let* ((in (aref matches 1))
                         (in (unless (string= in "NULL") in))
                         (out (aref matches 0))
                         (out (unless (string= out "NULL") out))
                         (weight
                          (ignore-errors (read-from-string (aref matches 2)))))
                    (push
                     (make-wp :in in :out out :weight weight)
                     (gethash in hash))))))
        (let ((n (hash-table-count hash)))
          (format t "(~a pair~p).~%" n n ))
        (loop
            for key being each hash-key using (hash-value value) in hash
            do (setf (gethash key hash) (sort value #'> :key #'wp-weight)))
        #+:allegro
        (sys:os-wait nil pid)
        hash))
    (setf *smt*
      (when *smt-directory*
        (let ((lfn
               (make-pathname
                :directory *smt-directory* :name "lex.f2n" :type "gz"))
              (lnf
               (make-pathname
                :directory *smt-directory* :name "lex.n2f" :type "gz"))
              (smt (make-smt)))
          (when (probe-file lfn)
            (setf (smt-lfn smt)
              (read-smt :file lfn :hash (make-hash-table :test #'equal)))
            (when (probe-file lnf)
              (setf (smt-lnf smt)
                (read-smt :file lnf :hash (make-hash-table :test #'equal)))
              (setf (smt-mlfn smt)
                (loop
                    for wps being each hash-value in (smt-lfn smt)
                    minimize (loop
                                 for wp in wps minimize (wp-weight wp))))
              (setf (smt-mlnf smt)
                (loop
                    for wps being each hash-value in (smt-lnf smt)
                    minimize (loop
                                 for wp in wps minimize (wp-weight wp))))
              smt)))))))

(defun smt-score-strings (source targets &key (smt *smt*) weight)
  (unless (smt-p smt)
    (return-from smt-score-strings
      (loop
          repeat (length targets)
          collect (if (numberp weight) 0 (cons 0 0)))))
  (labels ((smt-score-pair (source target &key (direction :forward))
             (when (or (null source) (null target))
               (return-from smt-score-pair 0))
             (when (eq direction :backward)
               (let ((foo source))
                 (setf source target)
                 (setf target foo)))
             (loop
                 with alignments = (make-array (length target)) with deletions
                 for stoken in source
                 for wps
                 = (gethash
                    stoken
                    (if (eq direction :forward) (smt-lfn smt) (smt-lnf smt)))
                 do
                   (loop
                       for wp in wps
                       for out = (wp-out wp)
                       for match
                       = (and out (position out target :test #'string=))
                       when (null out)
                       return (push (wp-weight wp) deletions)
                       when match
                       return (push (wp-weight wp) (aref alignments match))
                       finally
                         (if (eq direction :forward)
                           (push (smt-mlfn smt) deletions)
                           (push (smt-mlnf smt) deletions)))
                 finally
                   (return
                     (let ((product (apply #'* deletions)))
                       (loop
                           for alignment across alignments
                           for sum = (apply #'+ alignment)
                           when alignment
                           do (setf product
                                (* product (/ sum (length alignment)))))
                       (if (zerop product)
                         0
                         (let ((n (+ (length deletions)
                                     (length (remove nil alignments)))))
                           (/ (log product) n))))))))
    (let* ((strings (scrub-strings (cons source targets)))
           (source (ppcre:split "[ \\t]+" (first strings)))
           (targets (loop
                        for target in (rest strings)
                        collect (ppcre:split "[ \\t]+" target))))
      (loop
          for target in targets
          for forward = (smt-score-pair source target)
          for backward = (smt-score-pair source target :direction :backward)
          collect (if (numberp weight)
                    (+ (* forward weight) (* backward (- 1 weight)))
                    (cons forward backward))))))

(defparameter *smt-binary*
  #+:logon
  (let* ((root (getenv "LOGONROOT"))
         (root (and root (parse-namestring root))))
    (when root (format nil "~a/bin/pharaoh" root)))
  #-:logon
  nil)

(defparameter *smt-configuration*
  #+:logon
  (namestring (make-pathname 
               :directory (namestring
                           (dir-append 
                            (get-sources-dir "mt") '(:relative "mt")))
               :name "pharaoh.ini"))
  #-:logon
  nil)

(defparameter *smt-input* nil)

(defparameter *smt-output* 
  (format nil "/tmp/.smt.~a.~a.out" (lkb::current-user) (lkb::current-pid)))

(defparameter *smt-log* 
  (format nil "/tmp/.smt.~a.~a.log" (lkb::current-user) (lkb::current-pid)))

(defparameter *smt-pid* nil)
      
(let ((lock (mp:make-process-lock)))

  (defun smt-initialize (&optional (configuration *smt-configuration*))
    (when (and *smt-binary* configuration)
      (mp:with-process-lock (lock)
        (with-open-file (stream *smt-output*
                         :direction :output :if-exists :supersede))
        (with-open-file (stream *smt-log*
                         :direction :output :if-exists :supersede))
        (let* ((directory (pathname-directory configuration))
               (directory (namestring (make-pathname :directory directory)))
               (command (format
                        nil
                        "cd '~a' && ~a -f '~a'"
                        directory *smt-binary* configuration))
              foo)
          (multiple-value-setq (*smt-input* foo *smt-pid*)
            (run-process 
             command
             :wait nil
             :input :stream
             :output *smt-output* :if-output-exists :supersede
             :error-output *smt-log* :if-error-output-exists :supersede))
          (setf foo foo))))
    (sleep 0.5))
  
  (defun smt-translate-strings (strings &key (wait 30))
    (if *smt-binary*
      (mp:with-process-lock (lock)
        (when (null *smt-input*) (smt-initialize))
        (loop
            for string in (scrub-strings strings)
            do
              (format *smt-input* "<s> ~a </s>~%" (lm-normalize-string string))
            finally
              (close *smt-input*)
              (setf *smt-input* nil)
              #+:allegro
              (sys:os-wait nil *smt-pid*))
        (when (and (numberp wait) (> wait 0))
          (with-open-file (stream *smt-output*
                           :direction :input :if-does-not-exist nil)
            (loop
                while (and (zerop (file-length stream)) (> wait 0))
                do (sleep 1))))
        (let ((results
               (with-open-file (stream *smt-output* :direction :input)
                 (loop
                     for line = (read-line stream nil nil)
                     while line collect line))))
          (smt-initialize)
          results))
      strings)))
