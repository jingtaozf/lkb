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
         (types (substitute "%d" :integer types)))
    (concatenate 'string 
      "("
      (reduce #'(lambda (x y) (concatenate 'string x " " y)) types)
      ")")))

(defun select (attributes types relations condition
               &optional (language *tsdb-data*) 
               &key redirection absolute unique
                    quiet ro meter status file (format :lisp) sort)

  (when meter 
    (meter :value (get-field :start meter)))
  (when status
    (status :text (format nil "retrieving `~a' data ..." language)))
  
  (let ((condition (if (equal condition "") nil condition))
        (attributes (if (listp attributes) attributes (list attributes)))
        (types (if (listp types) types (list types)))
        (relations (if (listp relations) relations (list relations))))
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
                                :redirection redirection :unique unique
                                :quiet quiet :ro ro))
             data)
        (when rmeter (meter :value (get-field :end rmeter)))
        (with-input-from-string (stream result)
          (do ((line (read stream nil) (read stream nil)))
              ((null line))
            (push (pairlis keys line) data)))
        (when dmeter (meter :value (get-field :end dmeter)))
        (when status
          (status :text (format nil "retrieving `~a' data ... done" language)))

        (case format
          (:lisp
           data)
          (:tcl
           (let* ((data 
                   (if sort
                     (let ((key (intern (string-upcase (first attributes))
                                        :keyword)))
                       (sort (copy-list data)
                             (if (eq (first types) :integer) #'< #'string<) 
                             :key #'(lambda (foo)
                                      (get-field key foo))))
                     data))
                  (stream (if file
                            (create-output-stream file nil)
                            *tsdb-io*))
                  (width (length attributes))
                  (length (length data)))
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
                     (j 1 (+ j 1)))
                   ((null attributes))
                 (format
                  stream
                  "cell ~d ~d -contents ~s -format data~%"
                  i j field))
               (when (zerop (mod (- i 1) 10))
                 (format
                  stream
                  "layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%"
                  i)))
             (force-output stream)
             (when file (close stream)))
           (length data)))))))

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




