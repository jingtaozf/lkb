;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: TSDB -*-

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: macros.lisp
;;;      module: 
;;;     version: 0.0 (experimental)
;;;  written by: oe, coli saarbruecken
;;; last update: 31-aug-99
;;;  updated by: oe, coli saarbruecken
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :tsdb)

(defmacro sassoc (element list)
  `(loop
       for foo in (the list ,list)
       when (eq (first (the cons foo)) ,element) return foo))

(defmacro smember (element list)
  `(loop for foo in (the list ,list) thereis (eq ,element foo)))

(defmacro get-field (field alist)
  `(rest (assoc ,field ,alist)))

(defmacro get-field+ (field alist &optional default)
  `(or (rest (assoc ,field ,alist)) ,default))

(defmacro find-tsdb-directory (language &key test)
  `(let* ((home (make-pathname :directory *tsdb-home*))
          (suffix (pathname-directory (make-pathname :directory ,language)))
          (path (append (pathname-directory home) (rest suffix)))
          (data (make-pathname :directory path)))
     (if ,test
       (when (probe-file data) (namestring data))
       (namestring data))))
;;;
;;; _fix_me_
;;; for some weird reason, this was creating garbage strings when called from
;;; inside tsdb-do-create() but not when called interactively; that makes it a
;;; little hard to debug, but we did not need the generality of dir-append(),
;;; anyway ...                                                  (28-feb-04; oe)
;;;
#+:mystery
(defmacro find-tsdb-directory (language)
  `(let* ((data (dir-append (make-pathname :directory *tsdb-home*)
                            (list :relative ,language))))
     (namestring data)))

(defmacro find-skeleton (name)
  `(let* ((name (if (keywordp ,name) (string ,name) ,name)))
     (find name *tsdb-skeletons* 
           :key #'(lambda (foo) (get-field :path foo))
           :test #'equal)))

(defmacro find-skeleton-directory (skeleton)
  `(let* ((path (dir-append (namestring *tsdb-skeleton-directory*)
                            (list :relative (get-field :path ,skeleton)))))
     (namestring path)))

(defmacro divide (numerator denominator)
  `(if (zerop ,denominator) 0 (/ ,numerator ,denominator)))

(defmacro average (values)
  `(loop
       for value in ,values
       for i from 1
       sum value into total
       finally (return (divide total i))))

(defmacro sum (values)
  `(apply #'+ ,values))

(defmacro minus-one-p (integer)
  `(and (integerp ,integer) (= ,integer -1)))

(defmacro gc-statistics (key)
  `(aref *tsdb-gc-statistics*
         (case ,key
           (:global 0)
           (:scavenge 1)
           (:new 2)
           (:old 3)
           (:efficiency 4))))
  
(defmacro convert-time (time granularity)
  `(if (= ,time -1)
     -1
     (/ ,time (cond
               ((zerop ,granularity) 10)
               ((= ,granularity 9808) 100)
               ((>= ,granularity 9902) 1000)))))

(defmacro make-meter (start end)
  `(pairlis (list :start :end) (list ,start ,end)))

(defmacro mduration (meter)
  `(when ,meter (- (get-field :end ,meter) (get-field :start ,meter))))

(defmacro madjust (action meter value)
  `(when ,meter
     (let* ((start (get-field :start ,meter))
            (end (get-field :end ,meter))
            (duration (- end start)))
       (case ',action
         (* (setf end (+ start (* duration ,value))))
         (/ (setf end (+ start (/ duration ,value))))
         (+ (setf start (+ start ,value))
            (setf end (+ end ,value))))
       (make-meter start end))))

(defun time-a-funcall (timed-function report-function)
   #+(and :allegro-version>= (not (version>= 6 1)))
   (excl::time-a-funcall timed-function report-function)
   #+(and :allegro-version>= (version>= 6 1))
   (excl::time-a-funcall report-function timed-function)
   #-:allegro
   (let* ((treal (get-internal-real-time))
          (tcpu (get-internal-run-time))
          #+:mcl (tgc (ccl:gctime))
          #+:mcl (others (ccl::total-bytes-allocated)))
      (multiple-value-prog1
         (funcall timed-function)
         (let (#+:mcl (others (- (ccl::total-bytes-allocated) others)))
            (funcall report-function
               ;; tgcu tgcs tu ts tr scons ssym sother
               #+:mcl (round (* (- (ccl:gctime) tgc) 1000)
                             internal-time-units-per-second)
               #-:mcl 0
               0
               (round (* (- (get-internal-run-time) tcpu) 1000)
                      internal-time-units-per-second)
               0
               (round (* (- (get-internal-real-time) treal) 1000)
                      internal-time-units-per-second)
               0 0
               #+:mcl others #-:mcl -1)))))

(defmacro make-counts (&key (absolute 0) (contexts 0)
                            (events 0) (relevant 0))
  `(list ,absolute ,contexts ,events ,relevant))

(defmacro counts-absolute (counts)
  `(first ,counts))

(defmacro counts-contexts (counts)
  `(second ,counts))

(defmacro counts-events (counts)
  `(third ,counts))

(defmacro counts-relevant (counts)
  `(fourth ,counts))

(defmacro counts>= (counts1 counts2)
  `(and (>= (counts-absolute ,counts1) (counts-absolute ,counts2))
        (>= (counts-contexts ,counts1) (counts-contexts ,counts2))
        (>= (counts-events ,counts1) (counts-events ,counts2))
        (>= (counts-relevant ,counts1) (counts-relevant ,counts2))))
