;;; Copyright (c) 2003-2004
;;;   Ben Waldron, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :mrs)

 ;;; this section derived from oe's mt/semi.lsip

(defvar *semi* nil)

(defconstant *semi-u-type* "u")

(defconstant *semi-h-type* "h")

(defconstant *semi-i-type* "i")

(defconstant *semi-e-type* "e")

(defconstant *semi-x-type* "x")

(defstruct semi
  signature
  (roles (make-hash-table))
  (properties (make-hash-table))
  (predicates (make-hash-table :test #'equal)))

(unless *semi* (setf *semi* (make-semi)))

(defmethod print-object ((object semi) stream)
;;  (if %transfer-raw-output-p%
;;      (call-next-method)
    (let ((roles (hash-table-count (semi-roles object)))
          (predicates (hash-table-count (semi-predicates object)))
          (properties (hash-table-count (semi-properties object))))
      (format
       stream
       "#[SEM-I: ~a role~p; ~a predicate~p; ~a propert~a]"
       roles roles predicates predicates properties (if (= properties 1) "y" "ies"))))
;;)

(defmethod close-semi ((semi semi))
  (with-slots (signature roles predicates properties) semi
    (setf signature nil)
    (clrhash properties)
    (clrhash roles)
    (clrhash predicates)))

;;; build semi

(defmethod populate-semi ((semi semi))
  (close-semi semi)
  (let ((semantic-table *semantic-table*))
    (maphash 
     #'(lambda (key val)
	 (declare (ignore key))
	 (extend-semi semi val :mode :batch))
     semantic-table))
  (populate-semi-roles *semi*)
  semi)

(defmethod extend-semi ((semi semi) (record semantics-record) &key (mode :dynamic))
  (mapcar
   #'(lambda (rel)
       (record-ep rel semi :mode mode))
   (semantics-record-relations record)))

(defun record-mrs (mrs semi &key (mode :dynamic))
  (when (mrs::psoa-p mrs)
    (loop
        for ep in (mrs:psoa-liszt mrs)
        do (record-ep ep semi :mode mode))))

(defun record-ep (ep semi &key (mode :dynamic))
  (loop
      with roles = (rel-flist ep)
      with pred = (mrs::rel-pred ep)
      for role in roles
      for feature = (mrs:fvpair-feature role)
      for value = (let ((value (fvpair-value role)))
                    (if (var-p value)
                      (loop
                          with type = (let ((type (var-type value)))
                                        (vsym 
                                         (or type *semi-u-type*)))
			  with extra-list = (var-extra value)
                          for extra in extra-list
                          do 
                            (record-property type extra semi)
                          finally 
			    (return (make-var-base :type type :extra extra-list)))
		      value))
      do
	(when (eq mode :dynamic)
	  (record-role feature value semi))
      finally
        (record-predicate pred roles semi)))

(defmethod populate-semi-roles ((semi semi))
  (with-slots (roles predicates) semi
    (typecase roles
      (hash-table (clrhash roles))
      (null (setf roles (make-hash-table)))
      (t (error "hash-table or null expected")))
    (loop
	for frames being each hash-value in predicates
	do
	  (loop
	      for frame in frames
	      do
		(loop
		    for role in frame
		    for feature = (fvpair-feature role)
		    for value = (fvpair-value role)
		    do
		      (record-role feature value semi)
		      )))))

(defun record-role (feature value semi)
  (pushnew value (gethash feature (semi-roles semi)) :test #'eq))

(defun record-predicate (pred roles semi)
  (pushnew roles (gethash pred (semi-predicates semi)) :test #'equal))

(defun record-property (type fvpair semi)
  (with-slots (feature value) fvpair
    (pushnew 
     (cons type value)
     (gethash feature (semi-properties semi)) 
     :test #'equal)))

(defun print-semi (semi &key (generalizep t) (stream t) (format :plain))
  (case format
    (:plain
     (print-semi-plain semi 
		       :generalizep generalizep
		       :stream stream))
    (:full
     (print-semi-full semi 
		      :generalizep generalizep
		      :stream stream))
    (:db
     (print-semi-db semi))))

;;; format :plain

(defun print-semi-plain (semi &key (generalizep t) (stream t))
  ;;
  ;; _fix_me_
  ;; with larger SEM-Is, we should probably cache the sorted lists in the
  ;; SEM-I itself, using timestamps on the actual data fields to make sure we
  ;; can dynamically augment the SEM-I, still.              (13-jan-04; oe)
  ;;
  (let* ((properties (summarize-properties semi))
	 (roles (loop
                    for role being each hash-key in (semi-roles semi)
                    for values being each hash-value in (semi-roles semi)
                    collect (cons role values)))
         (roles (sort roles #'string-lessp :key #'first))
         (predicates (summarize-predicates semi))
         (predicates
          (loop
              with symbols 
              with strings
              for predicate in predicates
              when (stringp (first predicate)) do (push predicate strings)
              else do (push predicate symbols)
              finally (return (cons symbols strings))))
         (predicates (cons
                      (sort (first predicates) #'string-lessp :key #'first)
                      (sort (rest predicates) #'string-lessp :key #'first))))
  (loop
      initially (format stream "properties:~%~%")
      for (feature . values) in properties
      do 
	(format 
	 stream 
	 "  ~a [~{~(~a~)~^ ~}] : ~{~(~a~)~^ ~}~%" 
	 feature (first values) (rest values))
      finally (format stream "~%"))
  (loop
      initially (format stream "roles:~%~%")
      for (role . full-values) in roles
      for values = (to-types (collapse-values full-values))
      do 
	(format 
	 stream 
	 "  ~a : ~{~(~a~)~^ ~}~%" 
	 role (sort
	       (if generalizep (generalize-values values) values)
	       #'string-lessp))
      finally (format stream "~%"))
  (loop
      initially (format stream "predicates:~%~%")
      for predicate in (first predicates)
      do (print-predicate-plain 
	  predicate :generalizep generalizep :stream stream))
  (loop
      for predicate in (rest predicates)
      do (print-predicate-plain
	  predicate :generalizep generalizep :stream stream))))

(defun print-predicate-plain (predicate &key (generalizep t) (stream t))
  (loop
      with *package* = (find-package :lkb)
      initially (format stream "  ~(~s~) :" (first predicate))
      for (role . foo) in (rest predicate)
      for optionalp = (member nil foo)
      for values = (to-types (remove nil foo))
      do
        (format 
         stream
         "~:[,~;~] ~@[[~* ~]~a ~{~@[~(~a~)~]~^ ~}~@[~* ]~]"
         (eq role (first (first (rest predicate))))
         optionalp 
         role (if generalizep (generalize-values values) values) 
         optionalp)
      finally (format stream ".~%")))

(defun summarize-predicates (semi)
  (loop
      with predicates
      for predicate being each hash-key in (semi-predicates semi)
      for frames being each hash-value in (semi-predicates semi)
      for roles = (loop
                      with roles
                      for frame in frames
                      do
                        (loop 
                            for foo in frame 
                            do
                              (pushnew 
                               ;(cons (first foo) nil) ;;eg. (ARG0) 
                               (cons (fvpair-feature foo) nil) ;;eg. (ARG0) 
                               roles :key #'first)) ;;eg. ((ARG0) (ARG1))
                      finally (return roles))
      do
        (loop
            for frame in frames
            do 
              (loop
                  for role in roles
                  for value = (loop
                                  ;for (feature . value) in frame
                                  for fvpair in frame
				  for feature = (fvpair-feature fvpair)
				  for value = (fvpair-value fvpair)
                                  when (eq feature (first role))
                                  return (collapse-value value))
                  do
                    (pushnew value (rest role) :test #'equal)))
        (push 
         (cons predicate (sort roles #'string-lessp :key #'first))
         predicates)
      finally (return predicates)))

(defun summarize-properties (semi)
  (let* ((buckets
          (loop
              for feature being each hash-key in (semi-properties semi)
              for bucket being each hash-value in (semi-properties semi)
              collect (cons feature bucket)))
         (buckets (sort buckets #'string-lessp :key #'first)))
    (loop
        for bucket in buckets
        do
          (loop
              with types with values
              for (type . value) in (rest bucket)
              do
                (pushnew type types)
                (pushnew value values)
              finally
                (setf (rest bucket)
                  (cons (generalize-values types) 
                        (sort values #'string-lessp)))))
    buckets))

(defun generalize-values (values)
  ;;
  ;; _fix_me_
  ;; first of all, this should not be hard-wiring the signature in code, and
  ;; second, there should be a less naive way of generalizing (14-jan-04; oe)
  ;;
  (when (and (member (mrs::vsym *semi-e-type*) values)
             (member (mrs::vsym *semi-x-type*) values))
    (pushnew (mrs::vsym *semi-i-type*) values))
  (when (and (member (mrs::vsym *semi-h-type*) values)
             (or (member (mrs::vsym *semi-i-type*) values)
                 (member (mrs::vsym *semi-e-type*) values)
                 (member (mrs::vsym *semi-x-type*) values)))
    (pushnew (mrs::vsym *semi-u-type*) values))
  (cond
   ((member (mrs::vsym *semi-u-type*) values)
    (loop
        for value in values 
        unless (member 
                value 
                (list (mrs::vsym *semi-h-type*) (mrs::vsym *semi-i-type*) 
                      (mrs::vsym *semi-e-type*) (mrs::vsym *semi-x-type*)))
        collect value))
   ((member (mrs::vsym *semi-i-type*) values)
    (loop
        for value in values 
        unless (member 
                value 
                (list (mrs::vsym *semi-e-type*) (mrs::vsym  *semi-x-type*)))
        collect value))
   (t
    values)))

;;; format :full

(defun print-semi-full (semi &key generalizep (stream t))
  (let* ((properties (summarize-properties semi))
	 (roles (loop
                    for role being each hash-key in (semi-roles semi)
                    for values being each hash-value in (semi-roles semi)
                    collect (cons role values)))
         (roles (sort roles #'string-lessp :key #'first))
	 (predicates (sort
		      (loop
			  for pred being each hash-key in (semi-predicates semi)
			  for frames being each hash-value in (semi-predicates semi)
			  collect (cons pred frames))
		      #'pred-order
		      :key #'car)))
  (loop
      initially (format stream "properties:~%~%")
      for (feature . values) in properties
      do 
	(format 
	 stream 
	 "  ~a [~{~(~a~)~^ ~}] : ~{~(~a~)~^ ~}~%" 
	 feature (first values) (rest values))
      finally (format stream "~%"))
  (loop
      initially (format stream "roles:~%~%")
      for (role . full-values) in roles
      for values = (to-types full-values)
      do 
	(format 
	 stream 
	 "  ~a : ~{~(~a~)~^ ~}~%" 
	 role (sort values #'string-lessp))
      finally (format stream "~%"))
  (loop
      initially (format stream "predicates:~%~%")
      for predicate in predicates
      do (print-predicate-full
	  predicate  :generalizep generalizep :stream stream))))

(defun print-predicate-full (predicate &key generalizep (stream t))
  (declare (ignore generalizep))
  (loop
      with disp = (make-instance 'simple :stream stream)
      with (pred . frames) = predicate
      for frame in frames
      for rel = (make-rel-base :pred pred :flist frame)
      do  
	(setf *already-seen-vars* nil)
	(print-rel rel :display-to disp)))

;;; format = :db

(defstruct sdbt
  (rows (make-hash-table))
  last)

(defstruct sdb
  (pred (make-sdbt))
  (frame (make-sdbt :last 0))
  (role (make-sdbt :last 0))
  (slot-val (make-sdbt :last 0))
  (var (make-sdbt :last 0))
  (extra (make-sdbt :last 0)))

(defun next-counter (sdbt)
  (with-slots (last) sdbt
    (setf last (1+ last))))
  
(defun sdbt-hash (row sdbt)
  (setf (gethash (first row) sdbt) row))

(defun print-sdb (sdb)
  (let ((temp-dir (make-pathname :directory (pathname-directory (lkb::lkb-tmp-dir)))))
    (with-open-file 
	(stream
	 (format nil "~a/semi.obj.pred" temp-dir)
	 :direction :output :if-exists :supersede)
      (format t "~%writing table pred...")
      (print-sdbt (sdb-pred sdb) :stream stream))
    (with-open-file 
	(stream
	 (format nil "~a/semi.obj.frame" temp-dir)
	 :direction :output :if-exists :supersede)
      (format t "~%writing table frame...")
      (print-sdbt (sdb-frame sdb) :stream stream))
    (with-open-file 
	(stream
	 (format nil "~a/semi.obj.role" temp-dir)
	 :direction :output :if-exists :supersede)
      (format t "~%writing table role...")
      (print-sdbt (sdb-role sdb) :stream stream))
    (with-open-file 
	(stream
	 (format nil "~a/semi.obj.slot-val" temp-dir)
	 :direction :output :if-exists :supersede)
      (format t "~%writing table slot-val...")
      (print-sdbt (sdb-slot-val sdb) :stream stream))
    (with-open-file 
	(stream
	 (format nil "~a/semi.obj.var" temp-dir)
	 :direction :output :if-exists :supersede)
      (format t "~%writing table var...")
      (print-sdbt (sdb-var sdb) :stream stream))
    (with-open-file 
	(stream
	 (format nil "~a/semi.obj.extra" temp-dir)
	 :direction :output :if-exists :supersede)
      (format t "~%writing table extra...")
      (print-sdbt (sdb-extra sdb) :stream stream))))

(defun print-sdbt (sdbt &key (stream t))
  (format t " ~a" (sdbt-rows sdbt))
  (maphash #'(lambda (key row)
	       (declare (ignore key))
	       (format stream "~a~%" (tsv-line row)))
	   (sdbt-rows sdbt)))

(defun print-semi-db (semi)
  (let* ((predicates (sort
		      (loop
			  for pred being each hash-key in (semi-predicates semi)
			  for frames being each hash-value in (semi-predicates semi)
			  collect (cons pred frames))
		      #'pred-order
		      :key #'car)))
    (loop
	with sdb = (make-sdb)
	initially (format t "~%preparing semi-db tables...~%")
	for predicate in predicates
	do (process-predicate-db predicate sdb)
	finally
	  (print-sdb sdb))))

(defun process-predicate-db (predicate sdb)
  (loop
      with (pred . frames) = predicate
      with pred-str = pred
      for frame in frames
      for frame-id = (next-counter (sdb-frame sdb))
      for pred-row = (list pred-str frame-id)
      do
	(sdbt-hash pred-row (sdbt-rows (sdb-pred sdb)))
	(loop
	    for role in frame
	    for role-id = (next-counter (sdb-role sdb))
	    for frame-row = (list frame-id role-id)
	    for slot = (fvpair-feature role)
	    for slot-val = (fvpair-value role)
	    for value-id = (next-counter (sdb-slot-val sdb))
	    for role-row = (list role-id slot value-id)
	    with value-row
	    do
	      (sdbt-hash frame-row (sdbt-rows (sdb-frame sdb)))
	      (sdbt-hash role-row (sdbt-rows (sdb-role sdb)))
	      (typecase slot-val
		(string
		 (setf value-row (list value-id slot-val nil nil))
		 (sdbt-hash value-row (sdbt-rows (sdb-slot-val sdb))))
		(symbol
		 (setf value-row (list value-id nil slot-val nil))
		 (sdbt-hash value-row (sdbt-rows (sdb-slot-val sdb))))
		(var-base
		 (let* ((var slot-val)
		       (var-id (next-counter (sdb-var sdb)))
		       (type (var-base-type var)))
		   (setf value-row (list value-id nil nil var-id))
		   (sdbt-hash  value-row (sdbt-rows (sdb-slot-val sdb)))
		   (loop
		       with extra-list = (var-base-extra var)
		       for extra in extra-list
		       for extra-id = (next-counter (sdb-var sdb))
		       for var-row = (list var-id type extra-id)
		       for extra-feature = (extrapair-feature extra)
		       for extra-value = (extrapair-value extra)
		       for extra-row = (list extra-id extra-feature extra-value)
		       do
		   (sdbt-hash  var-row (sdbt-rows (sdb-var sdb)))
		   (sdbt-hash  extra-row (sdbt-rows (sdb-extra sdb))))))))))
		   
;;; aux fns

(defun pred-order (pred1 pred2)
  (typecase pred1
    (symbol
     (typecase pred2
       (symbol (string-lessp pred1 pred2))
       (string t)
       (t (error "unhandled type"))))
    (string
     (typecase pred2
       (symbol nil)
       (string (string-lessp pred1 pred2))
       (t (error "unhandled type"))))
    (t (error "unhandled type"))))

(defun encode-as-str (val)
  (cond
   ((null val)
    "")
   ((symbolp val)
    (let ((val-str (string val)))
      (if (and (> (length val-str) 0)
	       (eq (aref val-str 0) #\"))
	  (format nil "\\~a" val-str)
	val-str)))
   ((stringp val)
    (format nil "\"~a\"" val))
   (t
    (error "unhandled type: ~a" val))))

(defun to-types (predicate-values)
  (remove-duplicates (mapcar #'to-type predicate-values)))

(defun to-type (predicate-value)
  (if (var-base-p predicate-value)
      (var-type predicate-value)
    predicate-value))

(defun collapse-values (values)
  (mapcar #'collapse-value values))

(defun collapse-value (value)
  (if (not (var-base-p value))
      (typecase value
	(null :null)
	(string :string)
	(symbol :symbol)
	(number :number)
	(t :constant))
    value))


(defun print-semantic-table ()
  (let ((disp (make-instance 'simple :stream t)))
    (maphash 
     #'(lambda (id record)
	 (declare (ignore id))
	 (print (semantics-record-id record))
	 (setf *already-seen-vars* nil)
	 (mapc 
	  #'(lambda (x)
	     (print-rel x :display-to disp))
	  (semantics-record-relations record)))
     *semantic-table*)))

;;;
;;; tsv text format
;;;

(defun tsv-line (row)
  (str-list-2-str
   (mapcar #'2-tsv-str row)
   (format nil "~a" #\tab)))

(defun tsv-escape (str &optional (sep-char #\tab))
  (let ((l))
    (do ((i (1- (length str)) (1- i)))
	((< i 0))
      (push (aref str i) l)
      (if (eq (aref str i) sep-char)
	  (push #\\ l)))
    (concatenate 'string l)))

(defun 2-tsv-str (val)
  (tsv-escape
   (typecase val
     (null
      "\\N")
     (symbol
      (let ((val-str (string val)))
	(if (and (> (length val-str) 0)
		 (eq (aref val-str 0) #\"))
	    (format nil "\\~a" val-str)
	  val-str)))
     (string
      (format nil "\"~a\"" val))
     (number
      (format nil "~a" val))
     (t
      (error "unhandled type: ~a" val)))))

(defun str-list-2-str (str-list &optional (separator " "))
  (unless (listp str-list)
    (error "list expected"))
  (cond
   ((null str-list) "")
   (t (apply 'concatenate
	     (cons
	      'string
	      (cons
	       (pop str-list)
	       (mapcan #'(lambda (x) (list separator x)) str-list)))))))
  
;(defun encode-as-str (val)
;  (typecase val
;   (null 
;    "")
;   ((symbolp val)
;    (let ((val-str (string val)))
;      (if (and (> (length val-str) 0)
;	       (eq (aref val-str 0) #\"))
;	  (format nil "\\~a" val-str)
;	val-str)))
;   ((stringp val)
;    (format nil "\"~a\"" val))
;   (t
;    (error "unhandled type: ~a" val))))

