(in-package :tsdb)

;;;
;;; [incr tsdb()] --- Competence and Performance Profiling Environment
;;; Copyright (c) 1996 -- 2005 Stephan Oepen (oe@csli.stanford.edu)
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

(defparameter *html-directory*
  (pathname-directory 
   (dir-append (get-sources-dir "tsdb") '(:relative "tsdb" "html"))))

(defun html-output (file &key (stream *tsdb-io*) values)
  (let ((file (make-pathname :directory *html-directory* :name file)))
    (when (probe-file file)
      (if values
        (loop
            with size = (file-size file)
            with buffer = (make-array (* size 2)
                                      :element-type 'character
                                      :adjustable nil :fill-pointer 0)
            with in = (open file :direction :input)
            for c = (read-char in nil nil)
            while c do (vector-push c buffer)
            finally
              (close in)
              (apply #'format stream buffer values))
        (with-open-file (in file :direction :input)
          (loop
              for c = (read-char in nil nil)
              while c do (write-char c stream)))))))

    