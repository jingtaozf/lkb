;;; Copyright (c) 1998--2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.


(in-package :mrs)

;;;
;;; ToDo
;;;
;;; - identify more wellformedness conditions: e.g. mark all connected pieces 
;;;   of the graph and check for remaining fragments, or stipulate that certain
;;;   relations minimally require certain roles (with certain types, say).
;;;

(defparameter *eds-pretty-print-p* t)

(defparameter *eds-include-quantifiers-p* nil)

(defparameter *eds-include-vacuous-relations-p* nil)

(defparameter *eds-message-relation* (vsym "MESSAGE"))

(defparameter *eds-fragment-relation* (vsym "UNKNOWN_REL"))

(defparameter *eds-bleached-relations*
  (list (vsym "SELECTED_REL") (vsym "DEG_REL")))

(defparameter %eds-eds% nil)

(defparameter %eds-variable-counter% 0)

(defparameter %eds-symbol-table% nil)

(defparameter %eds-representatives-table% nil)

(defparameter %eds-equivalences% nil)

(defparameter %eds-relevant-features% 
  '("ARG" "ARG1" "ARG2" "ARG3" "ARG4" "BV" "SOA"
    "CONST_VALUE" "CARG"
    "MARG" "L-INDEX" "R-INDEX" 
    "L-HNDL" "R-HNDL"  "L-HANDEL" "R-HANDEL" "MAIN" "SUBORD" "ROLE"
    "HINST" "NHINST"))

(defstruct eds
  top relations hcons status)

(defmethod print-object ((object eds) stream)
  (if *eds-pretty-print-p*
    (let ((cyclicp (ed-cyclic-p object))
          (fragmentedp (ed-fragmented-p object)))
      (loop
          initially
            (format 
             stream 
             "{~@[~(~a~):~]~
              ~:[~3*~; (~@[cyclic~*~]~@[ ~*~]~@[fragmented~*~])~]~@[~%~]" 
             (eds-top object) 
             (or cyclicp fragmentedp) 
             cyclicp (and cyclicp fragmentedp) fragmentedp
             (eds-relations object))
          for ed in (eds-relations object)
          unless (and (null (ed-status ed))
                      (or (ed-bleached-p ed) (ed-vacuous-p ed))) do
            (format 
             stream 
             "~c~a~%" 
             (cond
              ((member :cyclic (ed-status ed)) #\|)
              ((member :fragmented (ed-status ed)) #\|)
              (t #\Space))
             ed)
          finally
            (format stream "}~%")))
    (call-next-method)))

(defstruct ed
  handle id type
  predicate arguments carg
  raw status mark)

(defmethod print-object ((object ed) stream)
  (if *eds-pretty-print-p*
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

(defun ed-output-psoa (psoa &key (stream t))
  (if (psoa-p psoa)
    (format stream "~a~%" (ed-convert-psoa psoa))
    (format stream "{}~%")))

(defun ed-convert-psoa (psoa)
  (when (psoa-p psoa)
    (loop
        with index = (psoa-index psoa)
        with name = (when (var-p index) (var-name index))
        with hcons = (psoa-h-cons psoa)
        with %eds-eds% = (make-eds :top name :hcons hcons)
        with %eds-symbol-table% = (make-hash-table)
        with %eds-representatives-table% = (make-hash-table)
        with %eds-equivalences% = (make-hash-table :test #'equal)
        with %eds-variable-counter% = 0
        for relation in (psoa-liszt psoa)
        for ed = (ed-convert-relation relation)
        when ed do (push ed (eds-relations %eds-eds%))
        finally
          (setf (eds-relations %eds-eds%)
            (nreverse (eds-relations %eds-eds%)))
          (ed-bleach-eds %eds-eds%)
          (ed-augment-eds %eds-eds%)
          (return %eds-eds%))))

(defun ed-convert-relation (relation)
  (let* ((handle (let ((handle (rel-handel relation)))
                   (when (handle-var-p handle) (handle-var-name handle))))
         (id (ed-find-identifier relation))
         (predicate (or (rel-sort relation) (rel-reltype relation)))
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
            (setf (gethash handle %eds-equivalences%) soa))
          (setf (ed-type ed) :message))
      when (ed-quantifier-p ed) do (setf (ed-type ed) :quantifier)
      when (ed-fragment-p ed) do (setf (ed-type ed) :fragment)))

(defun ed-augment-eds (eds)
  (loop
      for ed in (eds-relations eds)
      unless (ed-bleached-p ed) do
        (loop
            with old = (vsym "CONST_VALUE")
            with relation = (ed-raw ed)
            with flist = (rel-flist relation)
            for feature in %eds-relevant-features%
            for key = (vsym feature)
            for value = (loop
                            for fvpair in flist
                            thereis 
                              (when (eq (fvpair-feature fvpair) key)
                                (fvpair-value fvpair)))
            for representative = (and value (ed-find-representative value))
            when representative do
              (push (cons (if (eq key old) (vsym "CARG") key) representative)
                    (ed-arguments ed)))
        (setf (ed-arguments ed) (nreverse (ed-arguments ed)))))

(defun ed-find-identifier (relation)
  (or (gethash relation %eds-symbol-table%)
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
                    (format nil "_~d" (incf %eds-variable-counter%)))))
        (setf (gethash relation %eds-symbol-table%) name)))))

(defun ed-find-representative (variable &optional (selectp t))
  (or (gethash variable %eds-representatives-table%)
      (setf (gethash variable %eds-representatives-table%)
        (cond
         ((handle-var-p variable)
          (loop
              with alternate = (ed-variable-equivalence variable)
              with name = (handle-var-name (or alternate variable))
              with qeq = (ed-hcons-qeq name)
              for ed in (eds-relations %eds-eds%)
              for handle = (ed-handle ed)
              when (and (not (ed-bleached-p ed))
                        (or (equal name handle) 
                            (and (handle-var-p qeq) 
                                 (equal (handle-var-name qeq) handle))))
              collect ed
              into candidates
              finally 
                (return
                  (if selectp
                    (if (and candidates (null (rest candidates)))
                      (first candidates)
                      (ed-select-representative
                       (append
                        (when (handle-var-p qeq)
                          (ed-find-representative qeq nil))
                        (when (var-p alternate)
                          (ed-find-representative alternate nil))
                        candidates)))
                    candidates))))
         ((var-p variable)
          (loop
              with foo = (var-name variable)
              with name = (let ((bar (ed-variable-equivalence foo)))
                            (if  bar (handle-var-name bar) foo))
              for ed in (eds-relations %eds-eds%)
              for id = (unless (or (ed-bleached-p ed)
                                   (ed-quantifier-p ed))
                         (ed-id ed))
              thereis (when (equal name id) ed)))
         ((stringp variable) variable)))))

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
       finally (when (and candidates (null (rest candidates)))
                 (return (first candidates))))
   (loop
       for ed in eds
       unless (eq (ed-type ed) :quantifier) collect ed into candidates
       finally (when (and candidates (null (rest candidates)))
                 (return (first candidates))))
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
      for hcons in (eds-hcons %eds-eds%)
      for scarg = (when (eq (hcons-relation hcons) (vsym "QEQ"))
                    (handle-var-name (hcons-scarg hcons)))
      thereis 
        (when (equal name scarg) (hcons-outscpd hcons))))

(defun ed-variable-equivalence (variable)
  (if (var-p variable)
    (gethash (var-name variable) %eds-equivalences%)
    (gethash variable %eds-equivalences%)))

(defun ed-quantifier-p (ed)
  (let ((flist (rel-flist (ed-raw ed))))
    (find *scope-feat* flist :key #'fvpair-feature)))

(defun ed-message-p (ed)
  (when *eds-message-relation*
    (let ((type (ed-predicate ed)))
      (or (eq type *eds-message-relation*)
          (ignore-errors (subtype-p type *eds-message-relation*))))))

(defun ed-fragment-p (ed)
  (when *eds-fragment-relation*
    (let ((type (ed-predicate ed)))
      (or (eq type *eds-fragment-relation*)
          (ignore-errors (subtype-p type *eds-fragment-relation*))))))

(defun ed-bleached-p (ed)
  (or 
   (eq (ed-type ed) :message)
   (and (null *eds-include-quantifiers-p*) (eq (ed-type ed) :quantifier))
   (when *eds-bleached-relations*
     (loop
         with predicate = (ed-predicate ed)
         for foo in *eds-bleached-relations*
         for type = (if (stringp foo) (vsym foo) foo)
         thereis (or (eq predicate type) 
                     (ignore-errors (subtype-p predicate type)))))))

(defun ed-vacuous-p (ed)
  (unless *eds-include-vacuous-relations-p*
    (or (null (ed-arguments ed))
        (and (null (rest (ed-arguments ed)))
             (eq (first (first (ed-arguments ed))) (vsym "CARG"))))))

(defun ed-suspicious-p (eds)
  (append (when (ed-cyclic-p eds) '(:cyclic))
          (when (ed-fragmented-p eds) '(:fragmented))))

(defun ed-cyclic-p (eds)
  (loop
      with return = nil
      for ed in (eds-relations eds)
      unless (or (ed-bleached-p ed) (null (ed-arguments ed))) do
        (unless (ed-walk ed)
          (push :cyclic (ed-status ed))
          (setf return t))
      finally 
        (when return (push :cyclic (eds-status eds)))
        (return return)))

(defun ed-walk (ed &optional (start (list (ed-id ed)) startp))
  ;;
  ;; _fix_me_
  ;; on certain platforms (notably Linux), Allegro CL 6.2 will fail to detect a
  ;; stack overflow in (certain) recursive functions; this was pointed out to
  ;; Franz as [spr27625] in apr-03, and soon my favourite tech person, Lois 
  ;; Wolf, suggested to make those function not-inline.        (19-may-03; oe)
  ;;
  (declare (notinline foo))
  (unless (and startp (member (ed-id ed) start))
    (loop
        for (role . value) in (unless (ed-bleached-p ed) (ed-arguments ed))
        when (ed-p value) do
          (setf role role)
          (unless (ed-walk value (adjoin (ed-id ed) start)) (return nil))
        finally (return t))))

(defun ed-fragmented-p (eds)
  (let ((mark (gensym))
        (agenda (loop
                    with top = (eds-top eds)
                    for ed in (eds-relations eds)
                    when (equal (ed-id ed) top) collect ed)))
    (loop
        for ed = (pop agenda)
        for id = (ed-id ed)
        while ed do
          (unless (or (eq (ed-mark ed) mark) (ed-bleached-p ed))
            (setf (ed-mark ed) mark)
            (loop
                for argument in (ed-arguments ed)
                when (ed-p (rest argument)) do (push (rest argument) agenda))
            (loop
                for ed in (eds-relations eds)
                unless (or (eq (ed-mark ed) mark) (ed-bleached-p ed)) do
                  (loop
                      for argument in (ed-arguments ed)
                      when (and (ed-p (rest argument))
                                (equal (ed-id (rest argument)) id)) do
                        (push ed agenda)))))
    (loop
        with return = nil
        for ed in (eds-relations eds)
        when (and (not (ed-bleached-p ed)) (not (eq (ed-mark ed) mark)))
        do 
          (push :fragmented (ed-status ed))
          (setf return t)
        finally 
          (loop
              for ed in (eds-relations eds)
              when (eq (ed-type ed) :fragment) do (setf return nil))
          (when return (push :fragmented (eds-status eds)))
          (return return))))


