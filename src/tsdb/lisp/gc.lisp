(in-package "TSDB")

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

#+(version>= 5 0)
(def-foreign-call enable_gc_cursor ((type :int integer)) :returning :int)

#-(version>= 5 0)
(defforeign 'enable_gc_cursor :arguments '(integer) :return-type :integer)

#+(version>= 5 0)
(def-foreign-call gc_start ((type :int integer)) :returning :int)

#-(version>= 5 0)
(defforeign 'gc_start :arguments '(integer) :return-type :integer)

#+(version>= 5 0)
(def-foreign-call gc_end ((type :int integer)) :returning :int)

#-(version>= 5 0)
(defforeign 'gc_end :arguments '(integer) :return-type :integer)

(defun enable-gc-cursor (pid)
  (let ((gc-start (get-entry-point "gc_start"))
        (gc-end (get-entry-point "gc_end")))
    (when (and gc-start gc-end (integerp pid))
      (enable_gc_cursor pid)
      #+(version>= 5 0)
      (push (make-array 
             1 :element-type '(unsigned-byte #-:64bit 32 #+:64bit 64) 
             :initial-element gc-start)
            (excl:gc-before-c-hooks))
      #+(version>= 5 0)
      (push (make-array 
             1 :element-type '(unsigned-byte #-:64bit 32 #+:64bit 64)
             :initial-element gc-end)
            (excl:gc-after-c-hooks)))))

            