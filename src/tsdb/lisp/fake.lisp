;;; -*- Mode: COMMON-LISP; Syntax: Common-Lisp; Package: TSDB -*-

;;;
;;; [incr tsdb()] --- Competence and Performance Profiling Environment
;;; Copyright (c) 2006 -- 2007 Stephan Oepen (oe@csli.stanford.edu)
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

(defparameter *fakes* nil)

(defstruct fake
  class task ids inputs)

(defun initialize-fake (profiles &key class (task :parse) fake (reset t)
                                      (verbose t) (stream *tsdb-io*))
  (when reset (setf *fakes* nil))
  (let ((fake (or fake (make-fake :class class :task task))))
    (setf (fake-ids fake) (make-hash-table :test #'eql))
    (setf (fake-inputs fake) (make-hash-table :test #'equal))
    (loop
        for profile in (if (consp profiles) profiles (list profiles))
        for items = (retrieve nil profile :output nil)
        do
          (when verbose
            (format
             stream
             "initialize-fake(): reading `~a'.~%" profile))
          (loop
              for foo in items
              for item = (acons :source profile foo)
              for id = (get-field :i-id item)
              for input = (get-field :i-input item)
              do
                (setf (gethash id (fake-ids fake)) item)
                (setf (gethash input (fake-inputs fake)) item)))
    (push fake *fakes*)))

(defun search-fake (input &key class (task :parse) fake)
  (if fake
    (let* ((item (gethash input (fake-inputs fake)))
           (result
            (when item
              (analyze
               (get-field :source item)
               :condition (format nil "i-id == ~a" (get-field :i-id item))
               :thorough '(:derivation :tree :mrs :surface :flags)
               :readerp '(:flags)))))
      (first result))
    (loop
        for fake in *fakes*
        thereis (and (or (null class) (null (fake-class fake))
                         (eq (fake-class fake) class))
                     (or (null task) (null (fake-task fake))
                         (eq (fake-task fake) task))
                     (search-fake input :fake fake)))))

    
                 