;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: TSDB -*-

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
                    quiet ro meter status file (format :lisp) sort)
  (declare (special *statistics-tcl-formats*))

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
                         (t (format
                             *tsdb-io*
                             "~&select(): ignoring unknown attribute `~a'.~%"
                             attribute)
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
                                :redirection :output :unique unique
                                :quiet quiet :ro ro))
             data)
        (when rmeter (meter :value (get-field :end rmeter)))
        (when (and (stringp result) (probe-file result))
          (with-open-file (stream result)
            (do ((line (read stream nil) (read stream nil)))
                ((null line))
              (push (pairlis keys line) data)))
          (unless *tsdb-debug-mode-p* (delete-file result)))
        (when dmeter (meter :value (get-field :end dmeter)))
        (when status
          (status :text (format nil "retrieving `~a' data ... done" language)
                  :duration 10))

        (case format
          (:lisp
           (if sort 
             (sort data (if (eq stype :integer) #'< #'string<) 
                   :key #'(lambda (foo) (get-field sattribute foo)))
             data))
          (:tcl
           (let* ((data 
                   (if sort
                     (sort (copy-list data)
                           (if (eq stype :integer) #'< #'string<) 
                           :key #'(lambda (foo) (get-field sattribute foo)))
                     data))
                  (stream (if file
                            (create-output-stream file nil)
                            *tsdb-io*))
                  (width (length attributes))
                  (length (length data))
                  (*print-circle* nil))
             (when *statistics-tcl-formats* 
               (format stream *statistics-tcl-formats*))
             (format
              stream
              "layout col def -m1 5 -r 1 -m2 5 -c black -j left~%~
               layout row def -m1 5 -r 0 -m2 5 -c black -j center~%~
               layout col 0 -m1 5 -r 2 -m2 5 -c black -j left~%~
               layout row 0 -m1 5 -r 2 -m2 5 -c black -j center~%~
               layout row 1 -m1 5 -r 2 -m2 5 -c black -j center~%~
               layout col ~d -m1 5 -r 2 -m2 5 -c black -j left~%~
               layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%"
              width (+ length 1))
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
             (do* ((data data (rest data))
                   (item (first data) (first data))
                   (i 2 (+ i 1)))
                 ((null data))
               (do* ((attributes attributes (rest attributes))
                     (attribute (string-upcase (first attributes))
                                (string-upcase (first attributes)))
                     (attribute (intern attribute :keyword)
                                (intern attribute :keyword))
                     (field (get-field attribute item)
                            (get-field attribute item))
                     (key (get-field :i-id item) (get-field :i-id item))
                     (j 1 (+ j 1)))
                   ((null attributes))
                 (if (and key (eq attribute :i-input))
                   (format
                    stream
                    "cell ~d ~d -contents ~s ~
                     -format data -key ~d -source {~a}~%"
                    i j field key language)
                   (format
                    stream
                    "cell ~d ~d -contents ~:[~s~;~:d~] -format data~%"
                    i j (integerp field) field)))
               (when (zerop (mod (- i 1) 10))
                 (format
                  stream
                  "layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%"
                  i)))
             (force-output stream)
             (when file (close stream)))
           (length data)))))))

(defun tcount (data relation &key absolute quiet)
  
  (let* ((query (format nil "count ~a" relation))
         (result (call-tsdb query data :absolute absolute :ro t :quiet quiet))
         (colon (and result (position #\: result)))
         (result (and colon (subseq result (+ colon 2))))
         (count (and result (read-from-string result))))
    (and (numberp count) count)))

(defun insert (data relation tuples &key absolute meter)
  
  (when meter (meter :value (get-field :start meter)))
  (let* ((path (if absolute (namestring data) (find-tsdb-directory data)))
         (status (verify-tsdb-directory path :absolute t))
         (schema (when status (read-database-schema path :absolute t)))
         (relation (if (stringp relation) relation (string relation)))
         (attributes 
          (rest (find relation schema :test #'string= :key #'first)))
         (ntuples (length tuples))
         (interval (cond 
                    ((< ntuples 200) 10)
                    ((< ntuples 1000) 50)
                    ((< ntuples 2000) 100)
                    ((< ntuples 10000) 500)
                    (t 1000)))
         (increment (when meter 
                      (/ (mduration meter) (floor ntuples interval)))))
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
                                    (normalize-string value)
                                    value)))
                      (format stream "~:[@~;~]~a" start value))
                  finally (format stream "~%"))
              (when (and increment (zerop (mod i interval)))
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
