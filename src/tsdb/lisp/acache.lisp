;;; -*- Mode: COMMON-LISP; Syntax: Common-Lisp; Package: TSDB -*-

;;;
;;; [incr tsdb()] --- Competence and Performance Profiling Environment
;;; Copyright (c) 1996 -- 2005 Stephan Oepen (oe@csli.stanford.edu)
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

(defparameter *acache-cache-size* #-:64bit 200 #+:64bit 1024)

(defparameter *acache-strikes* #-:64bit 100000 #+:64bit nil)

(defstruct encoder
  (max 4096)
  (buffer (make-array
           4096 :element-type '(unsigned-byte 8) :initial-element 0))
  (size 0))

(defmethod print-object ((object encoder) stream)
  (format 
   stream 
   "#[E ~a #(" (encoder-size object))
  (loop
      for i from 0 to (- (encoder-size object) 1)
      do
        (format stream "~@[ ~*~]~a" (> i 0) (aref (encoder-buffer object) i)))
  (format stream ")]"))

(defparameter *encoder-pool* (loop repeat 7 collect (make-encoder)))

(let ((lock (mp:make-process-lock)))
  (defun allocate-encoder ()
    (mp:with-process-lock (lock)
      (or (pop *encoder-pool*) (make-encoder))))

  (defun free-encoder (encoder)
    (reset-encoder encoder)
    (mp:with-process-lock (lock)
      (push encoder *encoder-pool*))))

(defun reset-encoder (encoder &key fillp)
  (setf (encoder-size encoder) 0)
  (when fillp
    (loop
        with buffer = (encoder-buffer encoder)
        for i from 0 to (- (encoder-max encoder) 1)
        do (setf (aref buffer i) 0))))

(defmacro with-encoder ((encoder) &body body)
  `(let ((,encoder (allocate-encoder)))
     (unwind-protect (progn ,@body) (free-encoder ,encoder))))

(defun byte-encoding-size (n)
  (let* ((n (abs n))
         (size (cond
                ((zerop n) 0)
                (t
                 (loop
                     for foo = n then (ash foo -8)
                     until (zerop foo) count 1)))))
    (when (> size 127)
      (error "integer encoding overflow [~a]; see `acache.lisp'" n))
    (+ size 1)))

(defun byte-encode-integer (encoder n)
  (loop
      with size = (byte-encoding-size n)
      with vector = (encoder-buffer encoder)
      for i from (+ (encoder-size encoder) 1)
      to (+ (encoder-size encoder) (- size 1))
      for j = (abs n) then (ash j -8)
      do (setf (aref vector i) (logand j 255))
      finally
        (setf (aref vector (+ (encoder-size encoder)))
          (logior (- size 1) (if (minusp n) 128 0)))
        (incf (encoder-size encoder) size)
        (return size)))

(defun byte-decode-integer (vector &optional (offset 0))
  (loop
      with size = (aref vector offset)
      with sign = (logbitp 7 size)
      for i from (+ offset 1) to (+ offset (logand size 127))
      sum (ash (aref vector i) (* 8 (- i 1 offset))) into result
      finally (return (values (if sign (- result) result)
                              (+ offset (logand size 127) 1)))))

(defun byte-encode-float (encoder d)
  (let ((start (encoder-size encoder)))
    (typecase d
      (single-float
       (setf (aref (encoder-buffer encoder) start) 2)
       (incf (encoder-size encoder))
       (multiple-value-bind (one two) (excl:single-float-to-shorts d)
         (byte-encode-integer encoder one)
         (byte-encode-integer encoder two)))
      (double-float
       (setf (aref (encoder-buffer encoder) start) 4)
       (incf (encoder-size encoder))
       (multiple-value-bind (one two three four)
           (excl:double-float-to-shorts d)
         (byte-encode-integer encoder one)
         (byte-encode-integer encoder two)
         (byte-encode-integer encoder three)
         (byte-encode-integer encoder four)))
      (number
       (byte-encode-float encoder (coerce d 'single-float))))
    (- (encoder-size encoder) start)))

(defun byte-decode-float (vector &optional (offset 0))
  (let ((size (aref vector offset))
        (start offset))
    (cond
     ((= size 2)
      (multiple-value-bind (one offset)
          (byte-decode-integer vector (+ offset 1))
        (multiple-value-bind (two offset) (byte-decode-integer vector offset)
          (values (excl:shorts-to-single-float one two) (- offset start)))))
     ((= size 4)
      (multiple-value-bind (one offset)
          (byte-decode-integer vector (+ offset 1))
        (multiple-value-bind (two offset) (byte-decode-integer vector offset)
          (multiple-value-bind (three offset)
              (byte-decode-integer vector offset)
            (multiple-value-bind (four offset)
                (byte-decode-integer vector offset)
              (values (excl:shorts-to-double-float one two three four)
                      (- offset start))))))))))

(defun byte-encode-feature (encoder feature)
  (+ (byte-encode-integer encoder (feature-code feature))
     (typecase (feature-count feature)
       (integer (byte-encode-integer encoder (feature-count feature)))
       (float (byte-encode-float encoder (feature-count feature))))))

(defun byte-decode-feature (vector &optional (offset 0) &key tid parameters)
  (declare (special *feature-float-valued-tids*))
  (multiple-value-bind (code offset) (byte-decode-integer vector offset)
    (make-feature
     :code code :count (if (member tid *feature-float-valued-tids* :test #'=) 
                           (byte-decode-float vector offset)
                         (byte-decode-integer vector offset))
     :tid tid :parameters parameters)))

(declaim (inline byte=))
         
(defun byte= (vector1 vector2
              &key (start1 0) (end1 (- (array-total-size vector1) 1))
                   (start2 0) (end2 (- (array-total-size vector2) 1)))
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0))
           (type simple-array vector1 vector2)
           (type fixnum start1 end1 start2 end2))
  
  (and (= (- end1 start1) (- end2 start2))
       (loop
           for i from start1 to end1
           for j from start2 to end2
           always (= (aref vector1 i) (aref vector2 j)))))

(defun open-fc (file &optional mode
                &key createp (verbose t)
                     (cache *acache-cache-size*))
  (declare (ignore mode))
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
             "~&[~a] open-db(): new BTree `~a'.~%"
             (current-time :long :short) name)))
        (db.btree:create-btree
         file :if-exists :supersede :if-does-not-exist
         :create :unique-keys nil :cache-size cache))
       (t
        (db.btree:open-btree file :cache-size cache))))
    fc))

(defun close-fc (fc &key gcp)
  (db.btree:close-btree (fc-db fc))
  (setf (fc-db fc) nil)
  (when gcp (excl:gc)))

(defun feature-make-key (encoder iid rid tid parameters)
  (+ (byte-encode-integer encoder iid)
     (byte-encode-integer encoder rid)
     (byte-encode-integer encoder tid)
     (loop
         for key in parameters
         sum (byte-encode-integer encoder key))))

(defun store-feature (fc iid rid feature)
  (let ((db (fc-db fc)))
    (with-encoder (kencoder)
      (with-encoder (vencoder)
        (feature-make-key
         kencoder
         iid rid (feature-tid feature)
         (feature-parameters feature))
        (byte-encode-feature vencoder feature)
        (db.btree:set-btree-ext
         db (encoder-buffer kencoder) 0 (encoder-size kencoder)
         (encoder-buffer vencoder) 0 (encoder-size vencoder))))
    ;;
    ;; in the hope of reducing memory use, flush the BTree to disk every 5000
    ;; times we have added a feature.
    ;;
    (when (zerop (mod (incf (fc-strikes fc)) 5000))
      (db.btree:sync-btree db))))

(defun retrieve-features (fc iid rid tid parameters)
  (let ((db (fc-db fc)))
    (with-encoder (encoder)
      (feature-make-key encoder iid rid tid parameters)
      (let ((cursor (db.btree:create-cursor db)))
        (when (db.btree:position-cursor-ext
               cursor (encoder-buffer encoder) 0 (encoder-size encoder))
          (let ((features
                 (multiple-value-bind (key kstart kend value vstart vend)
                     (db.btree:cursor-get-ext cursor :key nil)
                   (declare (ignore key kstart kend vend))
                   (list (byte-decode-feature
                          value vstart :tid tid :parameters parameters)))))
            (loop
                with match = t
                while match do
                  (multiple-value-bind (key kstart kend value vstart vend)
                      (db.btree:cursor-next-ext cursor)
                    (declare (ignore vend))
                    (if (and key
                             (byte=
                              key (encoder-buffer encoder)
                              :start1 kstart :end1 (- kend 1)
                              :start2 0 :end2 (- (encoder-size encoder) 1)))
                      (push (byte-decode-feature
                             value vstart :tid tid :parameters parameters)
                            features)
                      (setf match nil))))
            (db.btree:position-cursor cursor nil :kind :unbind)
            (when (and (numberp *acache-strikes*)
                       (zerop (mod (incf (fc-strikes fc)) *acache-strikes*)))
              ;;
              ;; in an attempt to work around what looks like a BTree-internal
              ;; problem, viz. the BTree holding on to copies of our keys and
              ;; not releasing them for the lifetime of the DB object, throw
              ;; away the complete cache every now and again.
              ;;
              (format
               t
               "~&[~a] retrieve-features(): ~
                flushing BTree [~a strikes]~%"
               (current-time :long :short) (fc-strikes fc))
              #+:debug
              (excl:print-type-counts :new)
              (close-fc fc)
              #+:debug
              (excl:print-type-counts :new)
              (excl:gc)
              (setf (fc-db fc)
                (db.btree:open-btree (fc-file fc) :cache-size (fc-cache fc)))
              (setf (fc-strikes fc) 0))
            features))))))

#+:debug
(defun test (items)
  (setf (sys:gsgc-switch :print) t)
  (setf (sys:gsgc-switch :stats) t)
  (setf (sys:gsgc-switch :verbose) t)
  (setf (sys:gsgc-parameter :auto-step) nil)
  (excl:gc) (excl:gc t) (excl:gc :tenure) (excl:gc)
  (format t "~%============~%~%")
  (with-open-file (stream items :direction :input)
    (loop
        with db = (db.btree:open-btree "fc.abt" :cache-size (* 100 1024 1024))
        with cursor = (db.btree:create-cursor db)
        for iid = (read stream nil nil)
        while iid
        when (zerop (mod iid 50))
        do
          (excl:gc) (excl:print-type-counts :new)
          (pprint (db.btree:analyze-btree db :safe t))
        do
          (loop
              for rid from 0 to (read stream nil nil)
              do
                (loop
                    for i from 0 to 4
                    do
                      (loop
                          for j from 1 to 4
                          do (btree-retrieve cursor iid rid j (list i))))
                (loop
                    for i from 1 to 4
                    do
                      (btree-retrieve cursor iid rid 10 (list i 1))
                      (btree-retrieve cursor iid rid 11 (list i 1))))
        finally (excl:gc) (excl:print-type-counts :new)))
  (format t "~%============~%~%")
  (excl:gc) (excl:gc t) (excl:gc))

#+:debug
(let ((encoder (make-encoder)))
  (defun btree-retrieve (cursor iid rid tid parameters)
    (feature-make-key encoder iid rid tid parameters)
    (when (db.btree:position-cursor-ext
           cursor (encoder-buffer encoder) 0 (encoder-size encoder))
      (db.btree:cursor-get-ext cursor :key nil)
      (loop
          with match = t
          while match do
            (multiple-value-bind (key kstart kend value vstart vend)
                (db.btree:cursor-next-ext cursor :value nil)
              (declare (ignore value vstart vend))
              (unless (and key (byte=
                                key (encoder-buffer encoder)
                                :start1 kstart :end1 (- kend 1)
                                :start2 0 :end2 (- (encoder-size encoder) 1)))
                (setf match nil)))))
    (reset-encoder encoder)
    (db.btree:position-cursor cursor nil :kind :unbind)))
