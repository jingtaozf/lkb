;;; -*- Mode: COMMON-LISP; Syntax: Common-Lisp; Package: TSDB -*-

;;;
;;; [incr tsdb()] --- Competence and Performance Profiling Environment
;;; Copyright (c) 1996 -- 2006 Stephan Oepen (oe@csli.stanford.edu)
;;; Copyright (c) 2005 -- 2006 Erik Velldal (erikve@ifi.uio.no)
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

(in-package :tsdb)

(defparameter *bdb-cache-size* #-:64bit 200 #+:64bit 1024)

(def-foreign-call db_open
    ((file (* :char) string)
     (createp :int integer)
     (cache :int integer))
  :returning :int
  :strings-convert t)

(defun open-fc (file &optional mode
                &key createp (verbose t)
                     (cache *bdb-cache-size*))
  (declare (ignore mode))

  (when (and (pathnamep file) (null (pathname-type file)))
    (setf file (merge-pathnames file (make-pathname :type "bdb"))))
  
  (let* ((cache (if (numberp cache) (* cache 1024 1024) (* 128 1024 1024)))
         (fc (make-fc :file file :cache cache)))
    (setf (fc-db fc)
      (cond
       (createp
        (when verbose
          (let* ((file (pathname file))
                 (name (format 
                        nil 
                        "~a~@[.~a~]"
                        (pathname-name file) (pathname-type file))))
            (format
             t
             "~&[~a] open-fc(): new BTree `~a'.~%"
             (current-time :long :short) name)))
        (when (probe-file file) (delete-file file))
        (db_open (namestring file) 1 cache))
       (t
        (db_open (namestring file) 0 cache))))
    (unless (>= (fc-db fc) 0)
      (error
       "open-fc(): error ~a for `~a'."
       (fc-db fc) (namestring file)))
    fc))

(def-foreign-call db_close
    ((handle :int integer))
  :returning :void)

(defun close-fc (fc &key gcp)
  (when (numberp (fc-db fc))
    (db_close (fc-db fc)))
  (setf (fc-db fc) nil)
  (when gcp (excl:gc)))

(def-foreign-call db_cursor_close
    ((chandle :int integer))
  :returning :void)

(def-foreign-call db_write_feature_int
    ((handle :int integer)
     (iid :int integer) (rid :int integer) (tid :int integer)
     (parameters :int integer) (nparameters :int integer)
     (code :int integer) (count :int integer))
  :returning :int)

(def-foreign-call db_write_feature_float
    ((handle :int integer)
     (iid :int integer) (rid :int integer) (tid :int integer)
     (parameters :int integer) (nparameters :int integer)
     (code :int integer) (count :float single-float))
  :returning :int)

(defun db_write_feature (db iid rid tid
                         parameters nparameters code count)
  (typecase count
    (integer
     (db_write_feature_int
      db iid rid tid parameters nparameters code count))
    (float
     (db_write_feature_float
      db iid rid tid parameters nparameters code count))
    (t
     (error "db_write-feature(): invalid count `~(~a~)'." count))))

(let ((foo (allocate-fobject '(:array :int 2) :c)))

  (defun store-feature (fc iid rid feature)
    (unless (numberp (fc-db fc))
      (error "store-feature(): invalid feature cache handle."))
    (when (> (length (feature-parameters feature)) 2)
      (error
       "store-feature(): excessive parameter list (~a); see `bdb.lisp'."
       (length (feature-parameters feature))))
    (loop
        for i from 0 to (- (length (feature-parameters feature)) 1)
        do (setf (fslot-value-typed '(:array :int 2) :c foo i)
             (nth i (feature-parameters feature))))
    (let ((status
           (db_write_feature
            (fc-db fc) iid rid (feature-tid feature)
            foo (length (feature-parameters feature))
            (feature-code feature) (feature-count feature))))
      (unless (zerop status)
        (error
         "store-feature(): error writing (~a) [~a (~{~a~^ ~})] for ~a@~a."
         (feature-code feature) (feature-tid feature)
         (feature-parameters feature) iid rid))
      (when (zerop (mod (incf (fc-strikes fc)) 5000))
        #+:null
        (db_flush (fc-db fc)))
      status)))

(def-foreign-call db_read_feature_int
    ((handle :int integer) (chandle :int integer)
     (iid :int integer) (rid :int integer) (tid :int integer)
     (parameters :int integer) (nparameters :int integer)
     (code :int integer) (count :int integer))
  :returning :int)

(def-foreign-call db_read_feature_float
    ((handle :int integer) (chandle :int integer)
     (iid :int integer) (rid :int integer) (tid :int integer)
     (parameters :int integer) (nparameters :int integer)
     (code :int integer) (count :int integer))
  :returning :int)

(defun db_read_feature (db cursor iid rid tid
                        parameters nparameters code count)
  (declare (special *feature-float-valued-tids*))
  (if (member tid *feature-float-valued-tids* :test #'=)
    (db_read_feature_float
     db cursor iid rid tid parameters nparameters code count)
    (db_read_feature_int
     db cursor iid rid tid parameters nparameters code count)))

(let* ((code (allocate-fobject :int :c))
       (icount (allocate-fobject :int :c))
       (fcount (allocate-fobject :float :c))
       (foo (allocate-fobject '(:array :int 2) :c)))

  (defun retrieve-features (fc iid rid tid parameters)
    (declare (special *feature-float-valued-tids*))
    (unless (numberp (fc-db fc))
      (error "retrieve-features(): invalid feature cache handle."))
    (when (> (length parameters) 2)
      (error
       "retrieve-features(): excessive parameter list (~a); see `bdb.lisp'."
       (length parameters)))
    (loop
        for i from 0 to (- (length parameters) 1)
        do (setf (fslot-value-typed '(:array :int 2) :c foo i)
             (nth i parameters)))
    (let* ((count (if (member tid *feature-float-valued-tids* :test #'=)
                    fcount
                    icount))
           (type (if (member tid *feature-float-valued-tids* :test #'=)
                   :float
                   :int))
           (cursor
            (db_read_feature
             (fc-db fc) -1 iid rid tid foo (length parameters) code count))
           features)
      (when (>= cursor 0)
        (push
         (make-feature
          :tid tid :parameters parameters
          :code (fslot-value-typed :int :c code)
          :count (fslot-value-typed type :c count))
         features)
        (loop
            for status
            = (db_read_feature
               -1 cursor iid rid tid foo (length parameters) code count)
            while (>= status 0)
            do
              (push
               (make-feature
                :tid tid :parameters parameters
                :code (fslot-value-typed :int :c code)
                :count (fslot-value-typed type :c count))
               features))
        (db_cursor_close cursor))
      features)))
