;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: TSDB -*-

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

(defmacro get-field (field alist)
  `(rest (assoc ,field ,alist)))

(defmacro get-field+ (field alist &optional default)
  `(or (rest (assoc ,field ,alist)) ,default))

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
