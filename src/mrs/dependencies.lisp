(in-package :mrs)

(defparameter *dependencies-pretty-print-p* t)

(defparameter *dependencies-include-quantifiers-p* nil)

(defparameter *dependencies-discourse-relation* (vsym "DISCOURSE_REL"))

(defparameter *dependencies-message-relation* (vsym "MESSAGE"))

(defparameter %dependencies-eds% nil)

(defparameter %dependencies-variable-counter% 0)

(defparameter %dependencies-symbol-table% nil)

(defparameter %dependencies-equivalences% nil)

(defparameter %dependencies-relevant-features% 
  '("ARG" "ARG1" "ARG2" "ARG3" "ARG4" "BV" "SOA" "RSTR" "BODY"
    "MARG" "L-INDEX" "R-INDEX" "L-HANDEL" "R-HANDEL" "MAIN" "SUBORD" "ROLE"
    "HINST" "NHINST"))

(defstruct eds
  top relations hcons)

(defmethod print-object ((object eds) stream)
  (if *dependencies-pretty-print-p*
    (loop
        initially
          (format 
           stream 
           "{~@[~a:~]~@[ (cyclic)~]~@[~*~%~]" 
           (eds-top object) (ed-cyclic-p object) (eds-relations object))
        for ed in (eds-relations object)
        unless (or (ed-bleached-p ed) (null (ed-arguments ed))) do
          (format stream " ~a~%" ed)
        finally
          (format stream "}~%"))
    (call-next-method)))

(defstruct ed
  handle id type
  predicate arguments carg
  raw foo)

(defmethod print-object ((object ed) stream)
  (if *dependencies-pretty-print-p*
    (loop
        initially
          (format
           stream 
           "~(~a~):~(~a~)[" 
           (ed-id object) (ed-predicate object))
        for (role . value) in (ed-arguments object)
        do
          (format 
           stream 
           "~:[, ~;~]~:@(~a~) ~@[~(~a~)~]:~(~a~)~@[(~(~a)~)~]"
           (eq role (first (first (ed-arguments object))))
           role (if (ed-p value) (ed-id value) "")
           (if (ed-p value) (ed-predicate value) value)
           (when (ed-p value) (ed-carg value)))
        finally
          (format stream "]"))
    (call-next-method)))

(defun mrs-output-psoa (psoa &key (stream t))
  (if (psoa-p psoa)
    (format stream "~a~%" (ed-convert-psoa psoa))
    (format stream "{}~%")))

(defun ed-convert-psoa (psoa)
  (when (psoa-p psoa)
    (loop
        with index = (psoa-index psoa)
        with name = (when (var-p index) (var-name index))
        with hcons = (psoa-h-cons psoa)
        with %dependencies-eds% = (make-eds :top name :hcons hcons)
        with %dependencies-symbol-table% = (make-hash-table)
        with %dependencies-equivalences% = (make-hash-table :test #'equal)
        with %dependencies-variable-counter% = 0
        for relation in (psoa-liszt psoa)
        for ed = (ed-convert-relation relation)
        when ed do (push ed (eds-relations %dependencies-eds%))
        finally
          (setf (eds-relations %dependencies-eds%)
            (nreverse (eds-relations %dependencies-eds%)))
          (ed-bleach-eds %dependencies-eds%)
          (ed-augment-eds %dependencies-eds%)
          (return %dependencies-eds%))))

(defun ed-convert-relation (relation)
  (let* ((handle (let ((handle (rel-handel relation)))
                   (when (handle-var-p handle) (handle-var-name handle))))
         (id (ed-find-identifier relation))
         (predicate (rel-reltype relation))
         (flist (rel-flist relation))
         (carg (loop
                   with carg = (list (vsym "CARG") 
                                     (vsym "NAMED")
                                     (vsym "CONST_VALUE"))
                   for fvpair in flist
                   when (member (fvpair-feature fvpair) carg :test #'eq)
                   return (fvpair-value fvpair))))
    (make-ed :handle handle :id id 
             :predicate predicate :carg carg :raw relation)))

(defun ed-bleach-eds (eds)
  (loop
      for ed in (eds-relations eds)
      when (ed-message-p ed) do
        (let* ((handle (ed-handle ed))
               (relation (ed-raw ed))
               (flist (rel-flist relation))
               (soa (loop
                        with soa = (list (vsym "SOA") (vsym "MARG"))
                        for fvpair in flist
                        thereis 
                          (when (member (fvpair-feature fvpair) soa :test #'eq)
                            (fvpair-value fvpair)))))
          (when (handle-var-p soa)
            (setf (gethash handle %dependencies-equivalences%)
              (handle-var-name soa))
            (setf (ed-type ed) :message)))
      when (ed-quantifier-p ed) do (setf (ed-type ed) :quantifier)
      when (ed-discourse-p ed) do (setf (ed-type ed) :discourse)))

(defun ed-augment-eds (eds)
  (loop
      for ed in (eds-relations eds)
      unless (ed-bleached-p ed) do
        (loop
            with relation = (ed-raw ed)
            with flist = (rel-flist relation)
            for feature in %dependencies-relevant-features%
            for key = (vsym feature)
            for value = (loop
                            for fvpair in flist
                            thereis 
                              (when (eq (fvpair-feature fvpair) key)
                                (fvpair-value fvpair)))
            for representative = (and value (ed-find-representative value))
            when representative do
              (push (cons key representative) (ed-arguments ed)))
        (setf (ed-arguments ed) (nreverse (ed-arguments ed)))))

(defun ed-find-identifier (relation)
  (or (gethash relation %dependencies-symbol-table%)
    (let* ((flist (and (rel-p relation) (rel-flist relation)))
           instance event)
      (loop
          for fvpair in flist
          for feature = (fvpair-feature fvpair)
          when (eq feature (vsym "EVENT"))
          do (setf event (fvpair-value fvpair))
          when (or (eq feature (vsym "INST")) 
                   (eq feature (vsym "ARG0"))
                   (eq feature (vsym "C-ARG")))
          do (setf instance (fvpair-value fvpair)))
      (let* ((name (or
                    (and instance (var-p instance) (var-name instance))
                    (and event (var-p event) (var-name event))
                    (format nil "_~d" (incf %dependencies-variable-counter%))))
             #+:null
             (name (or (gethash name %dependencies-equivalences%) name)))
        (setf (gethash relation %dependencies-symbol-table%) name)))))

(defun ed-find-representative (variable)
  (cond
   ((handle-var-p variable)
    (loop
        with foo = (handle-var-name variable)
        with name = (or (gethash foo %dependencies-equivalences%) foo)
        with qeq = (or (ed-hcons-qeq name) (ed-hcons-qeq foo))
        for ed in (eds-relations %dependencies-eds%)
        for handle = (ed-handle ed)
        when (and (not (ed-bleached-p ed))
                  (or (equal name handle) (equal qeq handle)))
        collect ed
        into candidates
        finally 
          (return
            (if (null (rest candidates))
              (first candidates)
              (ed-select-representative candidates)))))
   ((var-p variable)
    (loop
        with foo = (var-name variable)
        with name = (or (gethash foo %dependencies-equivalences%) foo)
        for ed in (eds-relations %dependencies-eds%)
        for id = (unless (ed-quantifier-p ed) (ed-id ed))
        thereis (when (equal name id) ed)))
   ((stringp variable) variable)))

(defun ed-select-representative (eds)
  ;;
  ;; given a set of candidate representatives (typically corresponding to EPs
  ;; within one LF conjunct, i.e. sharing the same handle), heuristically find
  ;; one to stand in for the conjunction: give preference to EPs with a `real'
  ;; intrinsic variable (i.e. ones introducing an event or instance variable) 
  ;; or look at the dependency topology among the candidate EPs and choose one
  ;; that occurs as an argument to the other(s).
  ;;
  (or 
   (loop
       for ed in eds
       unless (char= (char (ed-id ed) 0) #\_) collect ed into candidates
       finally (when (null (rest candidates)) (return (first candidates))))
   (loop
       for ed in eds
       for id = (ed-id ed)
       for referrers = (loop
                           for referrer in eds
                           for flist = (unless (eq ed referrer)
                                         (rel-flist (ed-raw referrer)))
                           when (loop
                                    for fvpair in flist
                                    for value = (fvpair-value fvpair)
                                    thereis (and (var-p value)
                                                 (equal (var-name value) id)))
                           collect referrer)
       when referrers collect (cons (length referrers) ed) into candidates
       finally
         (return (rest (first (sort candidates #'> :key #'first)))))
   (first eds)))

(defun ed-hcons-qeq (handle)
  (loop
      with name = (if (stringp handle) handle (when (handle-var-p handle)
                                                (handle-var-name handle)))
      for hcons in (eds-hcons %dependencies-eds%)
      for scarg = (when (eq (hcons-relation hcons) (vsym "QEQ"))
                    (handle-var-name (hcons-scarg hcons)))
      thereis 
        (when (equal name scarg) 
          (handle-var-name (hcons-outscpd hcons)))))

(defun ed-quantifier-p (ed)
  (let ((flist (rel-flist (ed-raw ed))))
    (find *scope-feat* flist :key #'fvpair-feature)))

(defun ed-message-p (ed)
  (when *dependencies-message-relation*
    (let ((type (ed-predicate ed)))
      (or (eq type *dependencies-message-relation*)
          (subtype-p type *dependencies-message-relation*)))))

(defun ed-discourse-p (ed)
  (when *dependencies-discourse-relation*
    (let ((type (ed-predicate ed)))
      (or (eq type *dependencies-discourse-relation*)
          (subtype-p type *dependencies-discourse-relation*)))))

(defun ed-bleached-p (ed)
  (or 
   (member (ed-type ed) '(:message :discourse) :test #'eq)
   (and (null *dependencies-include-quantifiers-p*)
        (eq (ed-type ed) :quantifier))))

(defun ed-wellformed-p (eds)
  (not (ed-cyclic-p eds)))

(defun ed-cyclic-p (eds)
  (loop
      for ed in (eds-relations eds)
      unless (or (ed-bleached-p ed) (null (ed-arguments ed))) do
        (unless (ed-mark ed) (return ed))))

(defun ed-mark (ed &optional (mark (gensym)))
  (unless (eq (ed-foo ed) mark)
    (setf (ed-foo ed) mark)
    (loop
        for (role . value) in (ed-arguments ed)
        when (and (ed-p value) (not (ed-bleached-p value))) do
          (setf role role)
          (unless (ed-mark value mark) (return nil))
        finally (return t))))
