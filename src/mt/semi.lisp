(in-package :mt)

(defparameter *semis* nil)

(defstruct semi
  signature
  (roles (make-hash-table))
  (predicates (make-hash-table :test #'equal))
  (properties (make-hash-table)))

(defmethod print-object ((object semi) stream)
  (if %transfer-raw-output-p%
    (call-next-method)
    (let ((roles (hash-table-count (semi-roles object)))
          (predicates (hash-table-count (semi-predicates object)))
          (properties (hash-table-count (semi-properties object))))
      (format
       stream
       "#[SEM-I: ~a role~p; ~a predicate~p; ~a properties]"
       roles roles predicates predicates properties properties))))

(defun print-semi (semi &key (generalizep t) (stream t) (format :plain))
  ;;
  ;; _fix_me_
  ;; with larger SEM-Is, we should probably cache the sorted lists in the
  ;; SEM-I itself, using timestamps on the actual data fields to make sure we
  ;; can dynamically augment the SEM-I, still.              (13-jan-04; oe)
  ;;
  (let* ((roles (loop
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
                      (sort (rest predicates) #'string-lessp :key #'first)))
         (properties (summarize-properties semi)))
    (case format
      (:plain
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
           for (role . values) in roles
           do 
             (format 
              stream 
              "  ~a : ~{~(~a~)~^ ~}~%" 
              role (if generalizep (generalize-values values) values))
           finally (format stream "~%"))
       (loop
           initially (format stream "predicates:~%~%")
           for predicate in (first predicates)
           do (print-predicate 
               predicate :generalizep generalizep :stream stream))
       (loop
           for predicate in (rest predicates)
           do (print-predicate
               predicate :generalizep generalizep :stream stream))))))

(defun print-predicate (predicate &key (generalizep t) (stream t))
  (loop
      with *package* = (find-package :lkb)
      initially (format stream "  ~(~s~) :" (first predicate))
      for (role . foo) in (rest predicate)
      for optionalp = (member nil foo)
      for values = (remove nil foo)
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
                               (cons (first foo) nil) 
                               roles :key #'first))
                      finally (return roles))
      do
        (loop
            for frame in frames
            do 
              (loop
                  for role in roles
                  for value = (loop
                                  for (feature . value) in frame
                                  when (eq feature (first role))
                                  return value)
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
      
     
(defun record-mrs (mrs semi)
  (when (mrs::psoa-p mrs)
    (loop
        for ep in (mrs:psoa-liszt mrs)
        do (record-ep ep semi))))

(defun record-ep (ep semi)
  (loop
      with roles
      with pred = (mrs::rel-pred ep)
      for role in (mrs:rel-flist ep)
      for feature = (mrs:fvpair-feature role)
      for value = (let ((value (mrs:fvpair-value role)))
                    (if (mrs::var-p value)
                      (loop
                          with type = (let ((type (mrs:var-type value)))
                                        (mrs::vsym 
                                         (or type *semi-u-type*)))
                          for extra in (mrs:var-extra value)
                          do 
                            (record-property
                             type
                             (mrs::extrapair-feature extra) 
                             (mrs::extrapair-value extra)
                             semi)
                          finally (return type))
                      (typecase value
                        (null :null)
                        (string :string)
                        (symbol :symbol)
                        (number :number)
                        (t :constant))))
      do
        (record-role feature value semi)
        (push (cons feature value) roles)
      finally
        (record-predicate pred roles semi)))

(defun record-role (feature value semi)
  (pushnew value (gethash feature (semi-roles semi)) :test #'equal))

(defun record-predicate (pred roles semi)
  (pushnew roles (gethash pred (semi-predicates semi)) :test #'equal))

(defun record-property (type feature value semi)
  (pushnew 
   (cons type value)
   (gethash feature (semi-properties semi)) 
   :test #'equal))
