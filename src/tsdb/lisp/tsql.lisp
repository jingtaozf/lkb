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
;;;        file:
;;;      module:
;;;     version:
;;;  written by:
;;; last update:
;;;  updated by:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "TSDB")

(defun compute-report-string (types)
  (let* ((types (substitute "\"%s\"" :string types))
         (types (substitute "\"%s\"" :date types))
         (types (substitute "%d" :integer types)))
    (concatenate 'string 
      "("
      (reduce #'(lambda (x y) (concatenate 'string x " " y)) types)
      ")")))

(defun select (attributes types relations condition
               &optional (language *tsdb-data*) 
               &key absolute unique
                    quiet ro meter status file (format :lisp) (readerp t) sort
                    (efs *tsdb-efs*) sourcep)
  (declare (special *statistics-tcl-formats*))

  (let ((virtual (virtual-profile-p language)))
    (when virtual
      (return-from select
        (select-virtual
         virtual
         attributes types relations condition
         :absolute absolute :unique unique
         :quiet quiet :ro ro :meter meter :status status :file file
         :format format :readerp readerp :sort sort
         :efs efs :sourcep sourcep))))
  
  (when meter 
    (meter :value (get-field :start meter)))
  (when status
    (status :text (format nil "retrieving `~a' data ..." language)))
  
  (let* ((condition (if (equal condition "") nil condition))
         (attributes (if (listp attributes) attributes (list attributes)))
         (types (if (null types)
                  (let* ((schema 
                          (read-database-schema language :absolute absolute))
                         (fields (map 'list #'rest schema))
                         (fields (reduce #'(lambda (foo bar)
                                             (union foo bar 
                                                    :key #'first 
                                                    :test #'string=))
                                         fields))
                         types unknown)
                    (dolist (attribute attributes)
                      (let ((field (find attribute fields 
                                         :key #'first :test #'string=)))
                        (cond
                         (field (push (second field) types))
                         (t (unless quiet
                              (format
                               *tsdb-io*
                               "~&select(): ignoring unknown attribute `~a'.~%"
                               attribute))
                            (push attribute unknown)))))
                    (dolist (attribute unknown)
                      (setf attributes 
                        (delete attribute attributes :test #'equal)))
                    (nreverse types))
                  (if (consp types) types (list types))))
         (relations (if (listp relations) relations (list relations)))
         (sattribute 
          (when sort
            (if (eq sort t)
              (intern (string-upcase (first attributes)) :keyword)
              (typecase sort
                (string (intern (string-upcase sort :keyword)))
                (keyword sort)
                (symbol (intern sort :keyword))
                (t (intern (string-upcase (first attributes)) :keyword))))))
         (stype (when sattribute
                  (position sattribute attributes :test #'string-equal)))
         (stype (when stype (nth stype types))))
                  
    (if (not (= (length attributes) (length types)))
      (format
       *tsdb-io*
       "~&select(): `attributes' vs. `types' mismatch (~d vs. ~d).~%"
       (length attributes) (length types))
      (let* ((rmeter (madjust * meter 0.5))
             (dmeter (madjust + (madjust * meter 0.5) (mduration rmeter)))
             (keys (map 
                     'list 
                     #'(lambda (foo)
                         (intern (string-upcase foo) :keyword))
                     attributes))
             (sattributes (format nil "~{~a ~}" attributes))
             (relations (when relations (format nil "~{~a ~}" relations)))
             (report (compute-report-string types))
             (query 
              (format 
               nil
               "select ~a~@[from ~a~]~@[where ~a ~]report ~s"
               sattributes relations condition report))
             (result (call-tsdb query language :absolute absolute
                                :format :lisp :unique unique
                                :quiet quiet :ro ro))
             data)
        (when rmeter (meter :value (get-field :end rmeter)))
        (loop
            for line in result
            for item = (pairlis keys line)
            when sourcep do
              (nconc item (acons :source language nil))
            do
              (push item data))

        (when (and readerp (find-attribute-reader :i-input))
          (loop
              with reader = (find-attribute-reader :i-input)
              for tuple in data
              for value = (get-field :i-input tuple)
              for new = (and value (funcall reader value))
              when (and new (stringp new) (not (string= new ""))) do 
                (setf (rest tuple) (acons :o-input new (rest tuple)))))

        (when dmeter (meter :value (get-field :end dmeter)))
        (when status
          (status :text (format nil "retrieving `~a' data ... done" language)
                  :duration 10))

        (when sort
          (setf data 
            (sort data (if (eq stype :integer) #'< #'string<) 
                  :key #'(lambda (foo) (get-field sattribute foo)))))
        (case format
          (:lisp
           data)
          ((:ntcl :ascii)
           (let* ((stream (if file
                            (create-output-stream file nil)
                            *tsdb-io*))
                  (width (length attributes))
                  (length (length data))
                  (*print-circle* nil)
                  (totals (make-array (length attributes) :initial-element 0)))
             (when (eq format :ntcl)
               (format
                stream
                "viewer fast~%~
                 noofrows ~d~%noofcols ~d~%~
                 titlerows 1~%tilecols 1~%"
                (+ length 2) width)
               (loop
                   for attribute in attributes
                   do
                     (format stream "{~a}~%" attribute)))
             (loop
                 for item in data
                 do
                   (loop
                       for attribute in attributes
                       for key = (intern (string-upcase attribute) :keyword)
                       for field = (get-field key item)
                       for i from 0 by 1
                       when (and (integerp field) (not (= field -1)))
                       do 
                         (incf (aref totals i) field)
                       when (eq format :ntcl) do
                         (format stream "{~a}~%" field)))
             (loop
                 for attribute in attributes
                 for key = (intern (string-upcase attribute) :keyword)
                 for type in types
                 for i from 0 by 1
                 for total = (aref totals i)
                 when (member key *tsdb-id-attributes* :test #'eq)
                 do
                   (format stream "{~a}~%" length)                  
                 else when (and (eq type :integer)
                                (not (member key 
                                             *tsdb-coded-attributes* 
                                             :test #'eq)))
                 do
                   (format stream "{~a}~%" total)
                 else do
                   (format stream "{-}~%"))
             (force-output stream)
             (when file (close stream))
             length))
          (:tcl
           (let* ((stream (if file
                            (create-output-stream file nil)
                            *tsdb-io*))
                  (width (length attributes))
                  (length (length data))
                  (*print-circle* nil)
                  (totals (make-array (length attributes) :initial-element 0)))
             (when *statistics-tcl-formats* 
               (format stream *statistics-tcl-formats*))
             (format
              stream
              "flags 0~%~
               layout col def -m1 5 -r 1 -m2 5 -c black -j left~%~
               layout row def -m1 5 -r 0 -m2 5 -c black -j center~%~
               layout col 0 -m1 5 -r 2 -m2 5 -c black -j left~%~
               layout row 0 -m1 5 -r 2 -m2 5 -c black -j center~%~
               layout row 1 -m1 5 -r 2 -m2 5 -c black -j center~%~
               layout col ~d -m1 5 -r 2 -m2 5 -c black -j left~%~
               layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%~
               layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%"
              width (+ length 1) (+ length 2))
             (do ((types types (rest types))
                  (i 1 (+ i 1)))
                 ((null types))
               (when (member (first types) (list :integer :data))
                 (format
                  stream
                  "layout col ~d -m1 5 -r 1 -m2 5 -c black -j right~%~%"
                  i)))
             (do ((attributes attributes (rest attributes))
                  (i 1 (+ i 1)))
                 ((null attributes))
               (format
                stream
                "cell 1 ~d -contents ~s -format title~%~
                 region 1 ~d 1 ~d -contents ~s -format title ~
                   -hor_justify center~%"
                i (first attributes)
                i i (first attributes)))
             (loop
                 for item in data
                 for i from 2 by 1
                 do
                   (loop
                       for attribute in attributes
                       for key = (intern (string-upcase attribute) :keyword)
                       for field = (get-field key item)
                       for i-id = (get-field :i-id item)
                       for j from 1 by 1
                       when (and (integerp field) (not (= field -1))) do 
                         (incf (aref totals (- j 1)) field)
                       do
                         (if (and i-id (eq key :i-input))
                           (let ((o-input (get-field :o-input item))
                                 (tag (intern (gensym "") :keyword)))
                             (when o-input
                               (setf (get :source tag) language)
                               (setf (get :i-id tag) i-id)
                               (setf (get :i-input tag) o-input)
                               (setf (get :field tag) :o-input)
                               (setf (get :value tag) field))
                             (format
                              stream
                              "cell ~d ~d -contents {~a} ~
                               -format data -key ~d -source {~a}~
                               ~:[~*~; -action browse -stag ~a~]~%"
                              i j (tcl-escape-braces (or o-input field))
                              i-id language
                              field tag))
                           (format
                            stream
                            "cell ~d ~d -contents ~:[{~a}~;~:d~] ~
                             -format data~%"
                            i j (integerp field) (tcl-escape-braces field)))
                       finally
                         (when (zerop (mod (- i 1) 10))
                           (format
                            stream
                            "layout row ~d -m1 5 -r 2 -m2 5 ~
                             -c black -j center~%"
                            i))))
             (loop
                 for attribute in attributes
                 for key = (intern (string-upcase attribute) :keyword)
                 for type in types
                 for i from 0 by 1
                 for total = (aref totals i)
                 when (member key *tsdb-id-attributes* :test #'eq)
                 do
                   (format
                    stream
                    "cell ~d ~d -contents ~:d -format total~%"
                    (+ (length data) 2) (+ i 1) (length data))
                 else when (and (eq type :integer)
                                (not (member key 
                                             *tsdb-coded-attributes* 
                                             :test #'eq)))
                 do
                   (format
                    stream
                    "cell ~d ~d -contents ~:d -format total~%"
                    (+ (length data) 2) (+ i 1) total)
                 else
                 do
                   (format
                    stream
                    "cell ~d ~d -contents \"-\" -format total~%"
                    (+ (length data) 2) (+ i 1)))
             (force-output stream)
             (when file (close stream))
             length)))))))

(defun select-virtual (data attributes types relations condition
                       &key absolute unique
                            quiet ro meter status file (format :lisp)
                            (readerp t) sort (efs *tsdb-efs*) sourcep)
  
  (declare (ignore meter status))
  
  ;;
  ;; _fix_me_
  ;; in order to correctly get :tcl (or other formatted output) and enforce
  ;; sorting of tuples, we will have to separate tuple assembly and generation
  ;; of output one day.                                  (6-dec-05; erik & oe)
  ;;
  (when (or file (not (eq format :lisp)))
    (error "select-virtual(): functionality temporarily unavailable; ~
            see `tsql.lisp'."))
  
  (when (probe-file data)
    (with-open-file (stream data :direction :input)
      (let* ((sattribute 
              (when sort
                (if (eq sort t)
                  (intern (string-upcase (first attributes)) :keyword)
                  (typecase sort
                    (string (intern (string-upcase sort :keyword)))
                    (keyword sort)
                    (symbol (intern sort :keyword))
                    (t (intern
                        (string-upcase (first attributes)) :keyword))))))
             (stype (when sattribute
                      (position sattribute attributes :test #'string-equal)))
             (stype (when stype (nth stype types)))
             (result
              (loop
                  for name = (read stream nil nil)
                  while name
                  append 
                    (select
                     attributes types relations condition name
                     :absolute absolute :unique unique
                     :quiet quiet :ro ro :format :lisp :readerp readerp
                     :sort nil :efs efs :sourcep sourcep))))
        (if sort
          (sort result (if (eq stype :integer) #'< #'string<) 
                :key #'(lambda (foo) (get-field sattribute foo)))
          result)))))

(defun tcount (data relation &key absolute quiet)
  
  (let* ((query (format nil "count ~a" relation))
         (result (call-tsdb query data :absolute absolute :ro t :quiet quiet))
         (colon (and result (position #\: result)))
         (result (and colon (subseq result (+ colon 2))))
         (count (and result (read-from-string result))))
    (and (numberp count) count)))

(defun insert (data relation tuples &key absolute meter (normalize t))
  
  (when meter (meter :value (get-field :start meter)))
  (let* ((path (if absolute (namestring data) (find-tsdb-directory data)))
         (status (verify-tsdb-directory path :absolute t))
         (schema (when status (read-database-schema path :absolute t)))
         (relation (if (stringp relation) relation (string relation)))
         (attributes 
          (rest (find relation schema :test #'string= :key #'first)))
         (ntuples (length tuples))
         (increment (when (and meter (> ntuples 0))
                      (/ (mduration meter) ntuples))))
                      
    (cond
     ((null status)
      (format
       *tsdb-io*
       "insert(): invalid database `~a'.~%"
       data))
     ((eq (get-field :status status) :ro)
      (format
       *tsdb-io*
       "insert(): read-only database `~a'.~%"
       data))
     ((null attributes)
      (format
       *tsdb-io*
       "insert(): invalid relation `~a' for `~a'.~%"
       relation data))
     (t
      (with-open-file (stream (make-pathname :directory path :name relation)
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :append)
        #+:allegro
        (when *tsdb-encoding*
          (setf (stream-external-format stream) *tsdb-encoding*))
        (loop
            for tuple in tuples
            for i from 1 by 1
            do
              (loop
                  for (attribute type) in attributes
                  for start = t then nil
                  for key = (intern (string-upcase attribute) :keyword)
                  do
                    (let* ((value (or (get-field key tuple)
                                      (if (eq type :integer) -1 "")))
                           (value (if (eq type :string)
                                    (normalize-string
                                     value :escape t :normalize normalize)
                                    value)))
                      (unless start (write-char *tsdb-ofs* stream))
                      (if (eq type :integer)
                        (write value :stream stream)
                        (write-string value stream)))
                  finally (format stream "~%"))
              (when increment
                (meter-advance increment))))
      (when meter (meter :value (get-field :end meter)))
      tuples))))


;;;
;;; (overtly naive) functions borrowed from introductory Common-Lisp course 
;;; by janal@dfki.de and \me; i always used to say this code can be made 
;;; useful.                                        (22-may-97  -  oe@coli) 
;;;

;;;
;;; iterative select() function: loop through .relation., extract current value
;;; for .field., funcall() .test., accumulate .result. destructively (push()).
;;;
(defun iselect (relation field value &key (test #'equal))
  (do* ((relation relation (rest relation))
        (record (first relation) (first relation))
        (result nil result))
      ((null relation) result)
    (let ((comparison (get-field field record)))
      (when (and comparison (funcall test comparison value))
        (push record result)))))

;;;
;;; lazy combine(): fails to check for value compatibility in common fields;
;;; compact coding, though :-).
;;;
(defun lcombine (record-1 record-2)
  (union record-1 record-2 :key #'first))

;;;
;;; iterative join() function: traverse .relation-1., extract .value. for
;;; .key., select() correspoding (equal .key. value) records from
;;; .relation-2., compute .record. x .records. cross product, accumulate
;;; .result. destructively; not the most efficient algorithm, though.
;;;
(defun join (relation-1 relation-2 key &key meter)
  (when meter (meter :value (get-field :start meter)))
  (do* ((relation-1 relation-1 (rest relation-1))
        (record (first relation-1) (first relation-1))
        (value (get-field key record) 
               (get-field key record))
        (records (iselect relation-2 key value)
                 (iselect relation-2 key value))
        (result nil result))
      ((null relation-1) 
       (when meter (meter :value (get-field :end meter)))
       result)
    (dolist (foo records)
      (push (lcombine record foo) result))))

;;;
;;; once the database access has been optimized, the join() function suddenly
;;; shows up in the time profile; here is an alternate implementation (that we
;;; probably will not teach in the introductory course).
;;;
;;; _note_ njoin() assumes .relation-1. and .relation-2. to be sort()ed on 
;;; .key.
;;;
(defun njoin (relation-1 relation-2 key &key meter)

  (when meter (meter :value (get-field :start meter)))
  (let ((test= (if (numberp (get-field key (first relation-1))) #'= #'string=))
        (test< (if (numberp (get-field key (first relation-1))) #'< #'string<))
        result)
    (loop
        while (and relation-1 relation-2)
        for record-1 = (first relation-1)
        and record-2 = (first relation-2)
        for key-1 = (get-field key record-1)
        and key-2 = (get-field key record-2)
        do
          (cond
           ((funcall test= key-1 key-2)
            (push (append record-1 record-2) result)
            (loop
                for record-2 in (rest relation-2)
                for key-2 = (get-field key record-2)
                while (funcall test= key-1 key-2)
                do (push (append record-1 record-2) result))
            (pop relation-1))
           ((funcall test< key-1 key-2) (pop relation-1))
           (t (pop relation-2))))
    (nreverse result)))
