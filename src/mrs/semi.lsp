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
  (predicates (make-hash-table :test #'equal))
  (lexicon (make-hash-table)))

(unless *semi* (setf *semi* (make-semi)))

(defmethod print-object ((object semi) stream)
  (let ((properties (hash-table-count (semi-properties object)))
	(roles (hash-table-count (semi-roles object)))
	(predicates (hash-table-count (semi-predicates object)))
	(lexicon (print (hash-table-count (semi-lexicon object)))))
      (format
       stream
       "#[SEM-I: ~a role~p; ~a predicate~p; ~a propert~a; ~a lexical item~p]"
       roles roles 
       predicates predicates 
       properties (if (= properties 1) "y" "ies")
       lexicon lexicon
       )))

(defmethod close-semi ((semi semi))
  (with-slots (signature roles predicates properties) semi
    (setf signature nil)
    (clrhash properties)
    (clrhash roles)
    (clrhash predicates)))

;;; build semi

(defmethod populate-semi ((semi semi))
  (with-slots (lexicon) semi
    (setf lexicon *semantic-table*)
    (close-semi semi)
    (setf lexicon *semantic-table*)
    (maphash 
     #'(lambda (key val)
	 (declare (ignore key))
	 (extend-semi semi val :mode :batch))
     lexicon)
    (populate-semi-roles *semi*)
    semi))

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
  ;;
  ;; _fix_me_
  ;; probably simpler and faster to use independent routines
  ;; to extend each has table (predicates, roles, properties)
  ;; or even: generate roles/properties on demand
  ;;   eg. populate-semi-roles
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
        (record-predicate pred ep semi)))

(defmethod populate-semi-roles ((semi semi))
  (with-slots (roles predicates) semi
    (typecase roles
      (hash-table (clrhash roles))
      (null (setf roles (make-hash-table)))
      (t (error "hash-table or null expected")))
    (loop
	for rels being each hash-value in predicates
	do
	  (loop
	      for rel in rels
	      do
		(loop
		    for role in (rel-base-flist rel)
		    for feature = (fvpair-feature role)
		    for value = (fvpair-value role)
		    do
		      (record-role feature value semi)
		      )))))

(defun record-role (feature value semi)
  (pushnew value (gethash feature (semi-roles semi)) :test #'eq))

(defun record-predicate (pred rel semi)
  (pushnew rel (gethash pred (semi-predicates semi)) :test #'eq))

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
      for rels being each hash-value in (semi-predicates semi)
      for roles = (loop
                      with roles
                      for rel in rels
		      for frame = (rel-base-flist rel)
                      do
                        (loop 
                            for foo in frame 
                            do
                              (pushnew 
                                (cons (fvpair-feature foo) nil) ;;eg. (ARG0) 
                               roles :key #'first)) ;;eg. ((ARG0) (ARG1))
                      finally (return roles))
      do
        (loop
            for rel in rels
	    for frame = (rel-base-flist rel)
            do 
              (loop
                  for role in roles
                  for value = (loop
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
			  for rels being each hash-value in (semi-predicates semi)
			  collect (cons pred rels))
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
      for (pred . rels) in predicates
      do 
	(setf pred pred) ;; to avoid compiler warning
	(print-predicate-full
	  rels  :generalizep generalizep :stream stream))))

(defun print-predicate-full (rels &key generalizep (stream t))
  (declare (ignore generalizep))
  (loop
      with disp = (make-instance 'simple :stream stream)
      for rel in rels
      do  
	(setf *already-seen-vars* nil)
	(print-rel rel :display-to disp)))

;;; format = :db

(defstruct sdbt
  name
  (rows (make-hash-table))
  last)

(defmethod clear ((sdbt sdbt))
  (with-slots (rows last) sdbt
    (clrhash rows)
    (setf last nil))
  sdbt)

(defmethod print-object ((object sdbt) stream)
  (let (
	(keys-c (hash-table-count (sdbt-rows object)))
	)
      (format
       stream
       "#[SDBT ~a: ~a key~p]"
       (sdbt-name object)
       keys-c keys-c
       )))

(defstruct sdb
  (tables (list
	   (make-sdbt :name 'pred)
	   (make-sdbt :name 'frame :last 0)
	   (make-sdbt :name 'var :last 0)
	   (make-sdbt :name 'extra :last 0)
	   )))

(defmethod clear ((sdb sdb))
  (with-slots (tables) sdb
    (mapc #'clear tables)
    sdb))

(defun sdb-table (sdb table-name)
  (or
   (car (member table-name (sdb-tables sdb) :test #'eq :key #'sdbt-name))
   (error "unknown table name: ~a" table-name)))

(defun update-table (sdb sdbt)
  (with-slots (name) sdbt
  (let ((foo
	 (member name
		 (sdb-tables sdb) 
		 :test #'eq
		 :key #'sdbt-name)))
    (if foo
	(setf (car foo) sdbt)
      (error "no table ~a in ~a" name sdb)))))

(defun next-counter (sdbt)
  (with-slots (last) sdbt
    (setf last (1+ last))))

(defun sdbt-rows-hash (row sdbt)
  (push row (gethash (car row) sdbt))
  sdbt)

(defun print-sdb (sdb)
  (let ((temp-dir (make-pathname :directory (pathname-directory (lkb::lkb-tmp-dir)))))
    (mapc
     #'(lambda (x)
	 (with-open-file 
	     (stream
	      (format nil "~a/semi.obj.~(~a~)" temp-dir (sdbt-name x))
	      :direction :output 
	      :if-exists :supersede)
	   (format t "~%writing table ~a..." (sdbt-name x))
	   (print-sdbt x :stream stream)))
     (sdb-tables sdb))))
    
(defun load-sdb (sdb dbname)
  (mapcar #'(lambda (x)
	      (load-sdbt x dbname))
	  (sdb-tables sdb)))

(defun load-sdbt (sdbt dbname)
  (clear sdbt)
  (format t "~%loading table ~a from ~a..." (sdbt-name sdbt) dbname)
  (let ((sql-query (lkb::fn-get-raw-records 
		    dbname 
		    ''lkb::test 
		    (format nil "SELECT * FROM semi_~a"
			    (sdbt-name sdbt)))))
    (mapc #'(lambda (row) (sdbt-rows-hash 
			   (mapcar #'str-to-mixed2 row)
			   (sdbt-rows sdbt)))
	  (lkb::records sql-query))
    (setf (sdbt-last sdbt) nil)))

(defun print-sdbt (sdbt &key (stream t))
  (format t " ~a" (sdbt-rows sdbt))
  (maphash #'(lambda (key rows)
	       (declare (ignore key))
	       (mapc #'(lambda (row)
			 (format stream "~a~%" (tsv-line row)))
		     rows))
	   (sdbt-rows sdbt)))

;; for now...
(defvar *sdb* (make-sdb))

(defun print-semi-db (semi)
  (loop
      with sdb = (make-sdb)
      initially (format t "~%preparing semi-db tables...~%")
      for record being each hash-value in (semi-lexicon semi)
      do (process-record-db record sdb)
      finally
	(setf *sdb* sdb)
	(print-sdb sdb)))

(defmethod populate-semi-from-sdb ((semi semi) (sdb sdb))
  (with-slots (lexicon) semi
    (close-semi semi)
    (let* ((pred-t (sdb-table sdb 'pred))
	   (pred-r (sdbt-rows pred-t)))
      (loop
	  for lex-id being each hash-key in pred-r
	  do
	    (setf (gethash lex-id lexicon)
	      (load-lex-id-db lex-id sdb))
	    ))))

;;; -> lex-id
;;; semantics_record.id = lex-id
;;;                 .relations = frame-list
(defun load-lex-id-db (lex-id sdb)
  (let* ((pred-t (sdb-table sdb 'pred))
	 (pred-r (sdbt-rows pred-t))
	 (rows (gethash lex-id pred-r)))
    (make-semantics-record :id lex-id
			   :relations (load-relations-db rows sdb)))) 

;;; -> (lex-id pred frame-id)*
;;; rel-base*.pred = pred
;;;          .flist = role-list
(defun load-relations-db (rows sdb)
  (loop
      for row in rows
      for pred = (second row)
      for frame-id = (third row)
      collect
	(make-rel :pred pred
		  :flist (load-fvpairs-db frame-id sdb))))

;;; -> frame-id
;;; (frame-id slot str symb var-id)
;;; fvpair*.feature = slot
;;;        .value   = slot-val
(defun load-fvpairs-db (frame-id sdb)
  (loop
      with frame-t = (sdb-table sdb 'frame)
      with frame-r = (sdbt-rows frame-t)
      with rows = (gethash frame-id frame-r)
      for row in rows
      for slot = (second row)
      for str = (third row)
      for symb = (fourth row)
      for var-id = (fifth row)
      for type = (sixth row)
      for slot-val = (cond
		      ((and str (null symb) (null var-id))
		       str)
		      ((and (null str) symb (null var-id))
		       symb)
		      ((and (null str) (null symb) var-id)
		       (make-var :type type
				      :extra (load-extra-list-db var-id sdb)))
		      (t
		       (error "(str,symb,var-id)=(~a,~a,~a)"
			      str symb var-id)))
      collect
	(make-fvpair :feature slot
		     :value slot-val)))

;;; -> var-id
;;; (var-id extra-id)
;;; extrapair*.feature = feature
;;;           .value = value  
(defun load-extra-list-db (var-id sdb)
  (loop 
      with var-t = (sdb-table sdb 'var)
      with var-r = (sdbt-rows var-t)
      with rows = (gethash var-id var-r)
      for row in rows
      for extra-id = (second row)
      collect
	(load-extra-db extra-id sdb)))

;;; -> extra-id
;;; (extra-id feature value)
;;; extrapair.feature
;;;          .value
(defun load-extra-db (extra-id sdb)
  (let* ((extra-t (sdb-table sdb 'extra))
	 (extra-r (sdbt-rows extra-t))
	 (rows (gethash extra-id extra-r))
	 (row (car rows))
	 (feature (second row))
	 (value (third row)))
    (unless (= 1 (length rows))
      (error "~a rows for extra-id=~a" 
	     (length rows) extra-id))
    (make-extrapair :feature feature
		    :value value)))


(defun process-record-db (record sdb)
  (let* ((pred-t (sdb-table sdb 'pred))
	(frame-t (sdb-table sdb 'frame))
	(var-t (sdb-table sdb 'var))
	(extra-t (sdb-table sdb 'extra))
	
	(pred-r (sdbt-rows pred-t))
	(frame-r (sdbt-rows frame-t))
	(var-r (sdbt-rows var-t))
	(extra-r (sdbt-rows extra-t)))
    (loop
	with lex-id = (semantics-record-id record)
	with rels = (semantics-record-relations record)
	for rel in rels
	for pred = (rel-base-pred rel)
	for frame = (rel-base-flist rel)
	for frame-id = (next-counter frame-t)
	for pred-row = (list lex-id pred frame-id)
	do
	  (sdbt-rows-hash pred-row pred-r)
	  (loop
	      for role in frame
	      for slot = (fvpair-feature role)
	      for slot-val = (fvpair-value role)
	      with frame-row
	      do
		(typecase slot-val
		  (string
		   (setf frame-row (list frame-id slot slot-val nil nil nil))
		   (sdbt-rows-hash frame-row frame-r))
		  (symbol
		   (setf frame-row (list frame-id slot nil slot-val nil nil))
		   (sdbt-rows-hash frame-row frame-r))
		  (var-base
		   (let* ((var slot-val)
			  (var-id (next-counter var-t))
			  (type (var-base-type var)))
		     (setf frame-row (list frame-id slot nil nil var-id type))
		     (sdbt-rows-hash  frame-row frame-r)
		     (loop
			 with extra-list = (var-base-extra var)
			 for extra in extra-list
			 for extra-id = (next-counter extra-t)
			 for var-row = (list var-id extra-id)
			 for extra-feature = (extrapair-feature extra)
			 for extra-value = (extrapair-value extra)
			 for extra-row = (list extra-id extra-feature extra-value)
			 do
			   (sdbt-rows-hash  var-row var-r)
			   (sdbt-rows-hash  extra-row extra-r)
			   ))))))))
		   
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
      (let ((val-str (string-downcase (string val))))
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
  
(defun str-to-mixed2 (val-str)
  (let ((len (length val-str)))
    (cond 
     ((= (length val-str) 0)
      nil)
     ((eq (aref val-str 0) #\")
      (unless (eq (aref val-str (1- len)) #\")
	(error "STRING val must be of form \\\"STR\\\""))
      (subseq val-str 1 (1- len)))
     ((and (eq (aref val-str 0) #\\)
	  (eq (aref val-str 1) #\"))
      (lkb::str-2-symb (format nil "\"~a" (subseq val-str 2 len))))
     (t
      (lkb::str-2-symb val-str)))))
