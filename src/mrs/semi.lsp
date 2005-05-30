;;; Copyright (c) 2003-2004
;;;   Ben Waldron, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :mrs)

 ;;; this section derived from oe's mt/semi.lsip

(defvar *semi* nil)

(defparameter *semi-u-type* "u")
(defparameter *semi-h-type* "h")
(defparameter *semi-i-type* "i")
(defparameter *semi-e-type* "e")
(defparameter *semi-x-type* "x")

(defstruct semi
  signature
  (roles (make-hash-table))
  (properties (make-hash-table))
  (predicates (make-hash-table :test #'equal))
  (lexicon (make-hash-table))
  (pred-names (make-hash-table :test #'equal))
  (lex-preds (make-hash-table :test #'equal))
  (pos-preds (make-hash-table :test #'equal)))

(defclass pred-name ()
  ((key :accessor key :initarg :key)
   (name :accessor name :initarg :name)
   (string-p :accessor string-p :initarg :string-p)
   (lex :accessor lex :initarg :lex)
   (pos :accessor pos :initarg :pos)
   (id :accessor id :initarg :id)))
  
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
  (with-slots (signature roles predicates properties
	       lexicon pred-names lex-preds pos-preds)
      semi
    (setf signature nil)
    (clrhash properties)
    (clrhash roles)
    (clrhash predicates)
    (setf lexicon (make-hash-table)) ;; don't want to clear *semantic-table*
    (clrhash pred-names)
    (clrhash lex-preds)
    (clrhash pos-preds)
    ))

;;; build semi

(defmethod populated-p ((semi semi))
  (with-slots (signature) semi
    (if signature t)))
  
(defmethod populate-semi ((semi semi))
  (with-slots (lexicon signature) semi
    (close-semi semi)
    (setf lexicon *semantic-table*)
    (maphash 
     #'(lambda (key val)
	 (declare (ignore key))
	 (extend-semi semi val :mode :batch))
     lexicon)
    (populate-semi-roles semi)
    (setf signature (get-universal-time))
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
  (pushnew rel (gethash pred (semi-predicates semi)) :test #'eq)
  (unless (gethash pred (semi-pred-names semi))
    (let ((pred-name (get-pred-name pred)))
      (setf (gethash pred (semi-pred-names semi)) pred-name)
      (pushnew pred-name 
	       (gethash (lex pred-name) (semi-lex-preds semi))
	       :test #'eq)
      (pushnew pred-name 
	       (gethash (pos pred-name) (semi-pos-preds semi))
	       :test #'eq))))

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
	(setf pred pred) ;; prevent compiler warning
	(print-predicate-full
	 rels  :generalizep generalizep :stream stream))))

(defun lookup-semi (id)
  (print-predicate-full 
   (slot-value (gethash id (slot-value *semi* 'lexicon)) 'relations))
  (lkb::with-package (:lkb) (print-comps-info (extract-comps-info-by-id id)))
  nil)

(defun print-comps-info (comps-info)
  (mapcar #'print-comps-info2 comps-info))
  
(defun print-comps-info2 (x)
  (format t "~%~%COMPS ==  ~a (opt ~a) ~a" 
	  (cdr (assoc :rel x))
	  (cdr (assoc :opt x))
	  (print-coslot (cdr (assoc :coslot x)))))

(defun print-coslot (x)
  (mapcan #'(lambda (y) (list (format nil "~a: ~a " (cdr y) (car y))))
	      x))

(defun print-predicate-full (rels &key generalizep (stream t))
  (declare (ignore generalizep))
  (loop
      with disp = (make-instance 'simple :stream stream)
      for rel in rels
      do  
	(setf *already-seen-vars* nil)
	(print-semi-rel rel :display-to disp)))

;;; format = :db

(defstruct sdbt
  name
  (rows (make-hash-table))
  last
  (id-struc (make-hash-table :test #'equalp))
  )

(defmethod clear ((sdbt sdbt))
  (with-slots (rows last id-struc) sdbt
    (clrhash rows)
    (setf last nil))
    (clrhash (sdbt-id-struc sdbt))
  sdbt)

(defmethod print-object ((object sdbt) stream)
  (let (
	(keys-c (hash-table-count (sdbt-rows object)))
	(hashed-c (hash-table-count (sdbt-id-struc object)))
	)
      (format
       stream
       "#[SDBT ~a: ~a key~p ~a hashed]"
       (sdbt-name object) 
       keys-c keys-c
       hashed-c
       )))

(defun next-counter (sdbt)
  (with-slots (last) sdbt
    (setf last (1+ last))))

(defun sdbt-rows-hash (row sdbt)
  (push row (gethash (car row) sdbt))
  sdbt)

(defun print-sdbt (sdbt &key (stream t))
  (format t "~%~a" (sdbt-rows sdbt))
  (maphash #'(lambda (key rows)
	       (declare (ignore key))
	       (mapc #'(lambda (row)
			 (format stream "~a~%" (tsv-line row)))
		     rows))
	   (sdbt-rows sdbt)))

(defstruct sdb
  (tables (list
	   (make-sdbt :name 'pred)
	   (make-sdbt :name 'frame :last 0)
	   (make-sdbt :name 'var :last 0)
	   (make-sdbt :name 'extra :last 0)))
  (leaf-hash (make-hash-table :test #'equal)))

(defmethod clear ((sdb sdb))
  (with-slots (tables leaf-hash) sdb
    (mapc #'clear tables)
    (clrhash leaf-hash)
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

(defun print-sdb (sdb)
  (let ((temp-dir (make-pathname :directory (namestring (lkb::lkb-tmp-dir)))))
    (mapc
     #'(lambda (x)
	 (with-open-file 
	     (stream
	      (format nil "~asemi.obj.~(~a~)" temp-dir (sdbt-name x))
	      :direction :output 
	      :if-exists :supersede)
	   ;;(format t "~%writing table ~a..." (sdbt-name x))
	   (print-sdbt x :stream stream)))
     (sdb-tables sdb))))
    
(defvar *sdb* nil)

(defun print-semi-db (semi)
  (loop
      with sdb = (setf *sdb* (make-sdb))
      initially (format t "~%preparing semi-db tables...~%")
      for record being each hash-value in (semi-lexicon semi)
      do (process-record-db record sdb)
      finally
	(setf *sdb* sdb)
	(print-sdb sdb)))

(defun process-record-db (record sdb)
  (let* ((pred-t (sdb-table sdb 'pred))
	 (frame-t (sdb-table sdb 'frame))
	 (var-t (sdb-table sdb 'var))
	 (extra-t (sdb-table sdb 'extra))
	 
	 (pred-r (sdbt-rows pred-t))
	 (frame-r (sdbt-rows frame-t))
	 (var-r (sdbt-rows var-t))
	 (extra-r (sdbt-rows extra-t))
	 
	 ;(pred-h (sdbt-id-struc pred-t))
	 (frame-h (sdbt-id-struc frame-t))
	 (var-h (sdbt-id-struc var-t))
	 (extra-h (sdbt-id-struc extra-t))
	 )
    (loop
	with lex-id = (semantics-record-id record)
	with rels = (semantics-record-relations record)
	for rel in rels
	for pred = (rel-base-pred rel)
	for frame = (rel-base-flist rel)
	for frame-hashed = (gethash frame frame-h)
	for frame-id = (or frame-hashed (next-counter frame-t))
		       ;bmw
	for pred-row = (list lex-id 
			     pred 
			     frame-id 
			     (2-symb pred) 
			     (if (stringp pred) 'T 'F)
;;			     "NOW"
			     )
	do
	  (sdbt-rows-hash pred-row pred-r)
	unless frame-hashed
	do
	  (setf (gethash frame frame-h) frame-id)
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
			  (var-hashed (gethash var var-h))
			  (var-id (or var-hashed (next-counter var-t)))
			  (type (var-base-type var)))
		     (setf frame-row (list frame-id slot nil nil var-id type))
		     (sdbt-rows-hash  frame-row frame-r)
		     (unless var-hashed
		       (setf (gethash var var-h) var-id)
		       (loop
			   with extra-list = (var-base-extra var)
			   for extra in extra-list
			   for extra-hashed = (gethash extra extra-h)
			   for extra-id = (or extra-hashed (next-counter extra-t))
			   for var-row = (list var-id extra-id)
			   for extra-feature = (extrapair-feature extra)
			   for extra-value = (extrapair-value extra)
			   for extra-row = (list extra-id extra-feature extra-value)
			   do
			     (sdbt-rows-hash  var-row var-r)
			   unless extra-hashed
			   do
			     (setf (gethash extra extra-h) extra-id)
			     (sdbt-rows-hash  extra-row extra-r)
			     ))))))))
    sdb)
		   
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
	     (print-semi-rel x :display-to disp))
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
   (2-db-str val)))
   
(defun 2-db-str (val)
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
     (error "unhandled type: ~a" val))))

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

(defun str-2-symb (str)
  (unless (stringp str)
    (error "string exected"))
  (intern (string-upcase str)))

(defun 2-symb (x)
  (typecase x
    (string
     (str-2-symb x))
    (symbol
     x)
    (t
     (error "unhandled type"))))

(defun get-lex-pred-fields (pred-str)
  (when (eq (aref pred-str 0) 
	    #\_)
    (let* ((split-pred (split-on-char pred-str #\_))
	   (len (length split-pred)))
      (cond
       ((< len 4)
	(format t "~%malformed pred: ~a" pred-str)
	nil)
       ((= len 4)
	(subseq split-pred 1 3))
       (t
	(subseq split-pred 1 4))))))

(defun split-on-char (string &optional (char #\Space))
  (loop for i = 0 then (1+ j)
      as j = (position char string :start i)
      collect (subseq string i j)
      while j))

(defun get-pred-name (pred)
  (let* ((pred-str (if (stringp pred) pred (string-downcase (string pred))))
	 (pred-fields (get-lex-pred-fields pred-str)))
    (make-instance 'pred-name
      :key pred
      :name pred-str
      :string-p (stringp pred)
      :lex (nth 0 pred-fields)
      :pos (nth 1 pred-fields)
      :id (nth 2 pred-fields))))

(defun lookup-preds (lex &key pos id (semi *semi*))
  (mapcar #'key
	  (loop
	      for pred-name in (gethash lex (semi-lex-preds semi))
	      if 
		(and
		 (or (null pos)
		     (string= (pos pred-name)
			      pos))
		 (or (null id)
		     (string= (id pred-name)
			      id)))
	      collect pred-name)))

(defun info-from-semi-by-pred (pred &key (semi *semi*))
   "depreciated: see more-info-from-semi-by-pred"
  (let* ((pred-name (gethash pred (semi-pred-names semi)))
	 (predicate (gethash pred (semi-predicates semi)))
	 
	 (args (mapcar
		#'rel-base-flist
		predicate)))
    (cons (string-p pred-name)
	  args)))

(defun get-info-from-semi (lex &key pos id (semi *semi*))
  "depreciated: see more-info-from-semi"
  (mapcar #'info-from-semi-by-pred (lookup-preds lex :pos pos :id id :semi semi)))

(defun more-info-from-semi-by-pred (pred &key (semi *semi*))
  "return stringness and arg-structure"
  (let* ((pred-name (gethash pred (semi-pred-names semi)))
	 (predicate (gethash pred (semi-predicates semi)))
	 
	 (args (mapcar
		#'rel-base-flist
		predicate)))
    (list
     (cons :stringness (string-p pred-name))
     (cons :args args))))

(defun get-more-info-from-semi (lex &key pos id (semi *semi*))
  "for each pred matching lex pos? id? return info on pred"
  (mapcar #'(lambda (x) 
	      (list
	       (cons :pred x)
	       (cons :info (more-info-from-semi-by-pred x))))
	  (lookup-preds lex :pos pos :id id :semi semi)))

;;;
;;; extract info from comps list
;;;

(defparameter *comps-path* 
  `(,(vsym "SYNSEM") ,(vsym "LOCAL") ,(vsym "CAT") ,(vsym "VAL") ,(vsym "COMPS"))
  "Following this path into a sign gets you to the COMPS list")

(defparameter *comps-elt-key-path* 
  `(,(vsym "LOCAL") ,(vsym "CAT") ,(vsym "HEAD") ,(vsym "KEYS") ,(vsym "KEY"))
  "Following this path into an element on the COMPS list gets you the KEY")

(defparameter *comps-elt-index-path* 
  `(,(vsym "LOCAL") ,(vsym "CONT") ,(vsym "HOOK") ,(vsym "INDEX"))
  "Following this path into an element on the COMPS list gets you the INDEX")

(defparameter *comps-elt-opt-path* 
  `(,(vsym "OPT"))
  "Following this path into an element on the COMPS list gets you the OPT value")

(defparameter *semantics-to-rels-path* 
  `(,(vsym "RELS"))
  "Following this path from the start of the MRS structure gets you to the RELS list")

(defun get-dag-by-id (lex-id)
  (let* ((entry (lkb::get-lex-entry-from-id lex-id))
	 (dag (and
	       entry
	       (tdfs-indef (lex-entry-full-fs entry)))))
    dag))

(defun get-comps-list-by-id (lex-id)
  (get-comps-list (get-dag-by-id lex-id)))

(defun get-comps-list (dag)
  (let* ((comps-path *comps-path*)
	 (comps-dag (path-value dag 
				comps-path)))
    (lkb::dag-list-2-list comps-dag)))

(defun get-rels-list-by-id (lex-id)
  (get-rels-list (get-dag-by-id lex-id)))

(defun get-rels-list (dag)
  (let* ((rels-path 
	  (append *initial-semantics-path* 
		  *semantics-to-rels-path*))
	 (rels-dag (path-value dag 
			       rels-path)))
    (lkb::dag-diff-list-2-list rels-dag)))

(defun extract-comps-info (dag)
  (let* ((comps-list (get-comps-list dag))
	 (rels-list (get-rels-list dag)))
    (mapcar
     #'(lambda (x) (extract-comps-elt-info x rels-list))
     comps-list)))

(defun extract-comps-info-by-id (lex-id)
  (extract-comps-info (get-dag-by-id lex-id)))

(defun extract-comps-elt-info (comps-elt rels-list)
  (let* ((comp-rel (lkb::dag-type 
	       (path-value comps-elt
			   *comps-elt-key-path*)))
	 (index (path-value comps-elt
			    *comps-elt-index-path*))
	 (opt (lkb::dag-type (path-value comps-elt
					 *comps-elt-opt-path*)))
	 (coslot
	  (loop
	      for rel in rels-list
	      for rel-pred = (lkb::dag-type
			      (path-value rel *rel-name-path*))
	      append
		(loop
		    for arc in (dag-arcs rel)
		    for feat = (car arc)
		    for val = (cdr arc)
		    unless (ignored-sem-arc feat)
		    if (eq val index)
		    collect
		      (cons rel-pred feat)
		      ))))
    (list
     (cons :rel comp-rel)
     (cons :coslot coslot)
     (cons :opt opt))))
    
(defun ignored-sem-arc (feature)
    (or (member feature *ignored-sem-features*)
     (eql feature (car *rel-handel-path*))
     (eql feature (car *rel-name-path*))))

;;
;; 
;;

(defun rc (&optional (file "mrs/semi"))
	   (lkb::recomp file))

