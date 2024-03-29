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

(defmacro test-field (field alist)
  `(consp (assoc ,field ,alist)))

(defmacro get-field (field alist)
  `(rest (assoc ,field ,alist)))

(defmacro get-field+ (field alist &optional default)
  `(or (rest (assoc ,field ,alist)) ,default))

(defmacro get-field- (field alist)
  `(let ((foo (rest (assoc ,field ,alist))))
     (unless (and (stringp foo) (string= foo "")) foo)))

(defmacro set-field (field value alist)
  `(loop
       for list = ,alist then tail
       for tail = (rest list)
       when (eq ,field (first (first list)))
       do (setf (rest (first list)) ,value) and return t
       else when (null tail)
       do (setf (rest list) (acons ,field ,value nil)) and return nil))

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

(defmacro divide (numerator denominator)
  ;;
  ;; _fix_me_
  ;; the test for equal numerators and denominators now makes 0/0 come out as 1,
  ;; which may impact calculations of precision in corner cases.  i could see us
  ;; come down on either side of this question.                  (28-jan-12; oe)
  ;;
  `(if (and (numberp ,numerator) (numberp ,denominator))
     (cond
      ((zerop ,numerator) 0)
      ((= ,numerator ,denominator) 1)
      ((zerop ,denominator) 0)
      (t (/ ,numerator ,denominator)))
     0))

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
  `(when ,time
     (if (= ,time -1)
       -1
       (/ ,time (cond
                 ((zerop ,granularity) 10)
                 ((= ,granularity 199808) 100)
                 ((>= ,granularity 199902) 1000))))))

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
  ;;
  ;; mimicry of an /old/ version of time-a-funcall() in Allegro CL.  the report
  ;; function takes eight arguments: user and system gc() time, user and system
  ;; non-gc() time, wall-clock time, and allocation counts for cons() cells,
  ;; symbols, and other bytes.
  ;;
   #+(and :allegro-version>= (not (version>= 6 1)))
   (excl::time-a-funcall timed-function report-function)
   #+(and :allegro-version>= (version>= 6 1) (not (version>= 8 2)))
   (excl::time-a-funcall report-function timed-function)
   ;;
   ;; _fix_me_
   ;; as of Allegro CL 8.2, timing is now in microseconds (surely a good thing,
   ;; in principle), and arguments to excl::time-a-funcall() have changed.
   ;;                                                          (17-aug-11; oe)
   #+(and :allegro-version>= (version>= 8 2))
   (excl::time-a-funcall 
    #'(lambda (stream tgcu tgcs tu ts tr scons sother static &rest ignore)
        (declare (ignore stream ignore))
        (funcall 
         report-function 
         (round tgcu 1000) (round tgcs 1000) (round tu 1000) (round ts 1000)
         (round tr 1000) scons 0 (+ sother static)))
    *standard-output*
    timed-function)
   #-:allegro
   (let* ((treal (get-internal-real-time))
          (tcpu (get-internal-run-time))
          #+:mcl (tgc (ccl:gctime))
          #+:mcl (others (ccl::total-bytes-allocated)))
      (multiple-value-prog1
         (funcall timed-function)
         (let (#+:mcl (others (- (ccl::total-bytes-allocated) others)))
            (funcall report-function
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
