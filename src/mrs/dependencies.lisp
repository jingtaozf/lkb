;;; Copyright (c) 1998--2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `LICENSE' for conditions.


(in-package :mrs)

;;;
;;; ToDo
;;;
;;; - identify more wellformedness conditions: e.g. mark all connected pieces 
;;;   of the graph and check for remaining fragments, or stipulate that certain
;;;   relations minimally require certain roles (with certain types, say).
;;;

(defparameter *eds-debug-p* nil)

(defparameter *eds-pretty-print-p* t)

(defparameter *eds-include-messages-p* nil)

(defparameter *eds-include-quantifiers-p* t)

(defparameter *eds-include-vacuous-relations-p* nil)

(defparameter *eds-message-relation* (vsym "message_m_rel"))

(defparameter *eds-fragment-relation* (vsym "unknown_rel"))

(defparameter *eds-bleached-relations*
  (list (vsym "selected_rel") (vsym "deg_rel")))

(defparameter %eds-variable-counter% 0)

(defparameter %eds-symbol-table% (make-hash-table))

(defparameter %eds-representatives-table% (make-hash-table :test #'equal))

(defparameter %eds-equivalences% (make-hash-table :test #'equal))

(defparameter %eds-relevant-features% 
  '("ARG" "ARG1" "ARG2" "ARG3" "ARG4" "BV" "SOA"
    "CONST_VALUE" "CARG" "TERM1" "TERM2" "FACTOR1" "FACTOR2"
    "MARG" "L-INDEX" "R-INDEX" "L-HNDL" "R-HNDL"  "L-HANDEL" "R-HANDEL"
    "MAIN" "SUBORD" "ROLE" "HINST" "NHINST"))

(defstruct eds
  top relations hcons raw status)

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
  handle id type variable
  predicate arguments carg
  lnk raw status mark abstraction)

(defmethod print-object ((object ed) stream)
  (if *eds-pretty-print-p*
    (loop
        initially
          (format
           stream 
           "~(~a~):~(~a~)[" 
           (ed-id object) (ed-linked-predicate object))
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

(defun ed-linked-predicate (ed &key (lnkp t))
  (let ((predicate (ed-predicate ed))
        (lnk (ed-lnk ed)))
    (case (and lnkp (first (ed-lnk ed)))
      (:characters
       (format nil "~a<~a:~a>" predicate (second lnk) (third lnk)))
      (:vertices
       (format nil "~a<~a-~a>" predicate (second lnk) (third lnk)))
      (:tokens
       (format nil "~a<~{~a~^,~}>" predicate (rest lnk)))
      (t 
       (format nil "~a" (ed-predicate ed))))))

(defun ed-linked-abstraction (ed &key (lnkp t))
  (let ((abstraction (ed-abstraction ed))
        (lnk (ed-lnk ed)))
    (when abstraction
      (case (and lnkp (first (ed-lnk ed)))
        (:characters
         (format nil "_~(~a~)_<~a:~a>" abstraction (second lnk) (third lnk)))
        (:vertices
         (format nil "_~(~a~)_<~a-~a>" abstraction (second lnk) (third lnk)))
        (:tokens
         (format nil "_~(~a~)_<~{~a~^,~}>" abstraction (rest lnk)))
        (t 
         (format nil "_~(~a~)_" abstraction))))))

(defun ed-output-psoa (psoa &key (stream t) (format :ascii)
                                 cargp markp lnkp collocationp abstractp)
  (if (psoa-p psoa)
    (case format
      (:ascii
       (format stream "~a~%" (ed-convert-psoa psoa)))
      (:triples
       (let* ((eds (ed-convert-psoa psoa))
              (triples (ed-explode
                        eds
                        :lnkp lnkp :cargp cargp
                        :collocationp collocationp :abstractp abstractp)))
         (loop
             with *package* = (find-package :lkb)
             initially (unless markp (format stream "{~%"))
             for triple in triples
             do
               (format
                stream
                "~:[  ~;<s> ~]~{~a~^ ~}~:[  ~; </s>~]~%"
                markp triple markp)
             finally (unless markp (format stream "}~%~%")))
         (length triples)))
      (:lui
       (let ((attic (make-hash-table :test #'equal))
             (id 0))
         (labels ((record (object)
                    (or (gethash object attic)
                        (let ((n id))
                          (setf (gethash object attic) n)
                          (incf id)
                          n))))
           (let ((eds (ed-convert-psoa psoa)))
             (format 
              stream
              "#X[~a \"{~(~a~)\" \":\" newline~%\"    \" #X["
              (record (eds-top eds)) (eds-top eds))
             (loop
                 with firstp = t
                 for ed in (eds-relations eds)
                 do
                   (format
                    stream
                    "~@[newline~*~] ~a \"~(~a~)\" \":~(~a~)[\" #X["
                    (not firstp)
                    (record (ed-id ed)) (ed-id ed) (ed-linked-predicate ed))
                   (setf firstp nil)
                   (loop
                       with firstp = t
                       for (role . value) in (ed-arguments ed)
                       do
                         (format
                          stream
                          "~@[\", \" wrap ~*~]\"~a \" ~
                           ~@[~a ~]~@[\"~(~a~)\" ~]\":~(~a~)~@[(~(~a~))~]\""
                          (not firstp) role
                          (and (ed-p value) (record (ed-id value)))
                          (and (ed-p value) (ed-id value))
                          (if (ed-p value) (ed-predicate value) value)
                          (and (ed-p value) (ed-carg value)))
                         (setf firstp nil)
                       finally (format stream "] \"]\"~%"))
                 finally 
                   (format 
                    stream
                    "] newline \"}\"]~%~
                     #M[]")))))))
    (format stream "{}~%")))

#+:lkb
(defun ed-convert-edge (edge)
  (when (lkb::edge-p edge)
    (ed-convert-psoa (or (lkb::edge-mrs edge) (extract-mrs edge)))))

(defun ed-convert-psoa (psoa)
  (when (psoa-p psoa)
    (loop
        with index = (psoa-index psoa)
        with name = (when (var-p index) (var-string index))
        with hcons = (psoa-h-cons psoa)
        with eds = (make-eds :top name :hcons hcons :raw psoa)
        initially (ed-reset)
        for relation in (psoa-liszt psoa)
        for ed = (ed-convert-relation relation)
        when ed do (push ed (eds-relations eds))
        finally
          (setf (eds-relations eds)
            (nreverse (eds-relations eds)))
          (ed-bleach-eds eds)
          (ed-augment-eds eds)
          (ed-reset)
          (return eds))))

(defun ed-convert-relation (relation)
  (let* ((handle (let ((handle (rel-handel relation)))
                   (when (ed-handle-p handle) (var-string handle))))
         (id (ed-find-identifier relation))
         (predicate (string (rel-pred relation)))
         (abstraction
          #+:ppcre
          (cond
           ((ppcre:scan "_[aA](?:_[^_]+)?_[rR][eE][lL]" predicate) :a)
           ((ppcre:scan "_[nN](?:_[^_]+)?_[rR][eE][lL]" predicate) :n)
           ((ppcre:scan "_[pP](?:_[^_]+)?_[rR][eE][lL]" predicate) :p)
           ((ppcre:scan "_[qQ](?:_[^_]+)?_[rR][eE][lL]" predicate) :q)
           ((ppcre:scan "_[vV](?:_[^_]+)?_[rR][eE][lL]" predicate) :v)
           (t :x)))
         (predicate (remove-right-sequence 
                     *sem-relation-suffix* (string-downcase predicate)))
         (flist (rel-flist relation))
         (carg (loop
                   with carg = (list (vsym "CARG") 
                                     (vsym "NAMED")
                                     (vsym "CONST_VALUE"))
                   for fvpair in flist
                   when (member (fvpair-feature fvpair) carg :test #'eq)
                   return (fvpair-value fvpair)))
         (lnk (let* ((lnk (rel-lnk relation))
                     (from (rel-cfrom relation))
                     (to (rel-cto relation)))
                (or lnk
                    (and (numberp from) (numberp to) (>= from 0) (>= to 0)
                         (list :characters from to))))))
    (make-ed :handle handle :id id :lnk lnk
             :predicate predicate :carg carg :abstraction abstraction
             :raw relation)))

(defun ed-bleach-eds (eds)
  (loop
      for ed in (eds-relations eds)
      when (ed-message-p ed) do
        (let* ((handle (ed-handle ed))
               (relation (ed-raw ed))
               (soa (loop
                        with soa = (list (vsym "SOA") (vsym "MARG"))
                        for fvpair in (rel-flist relation)
                        thereis 
                          (when (member (fvpair-feature fvpair) soa :test #'eq)
                            (fvpair-value fvpair)))))
          (when (ed-handle-p soa)
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
            for representative = (when value 
                                   (ed-find-representative eds value))
            when representative do
              #+:null
              (when (ed-p representative)
                (setf (ed-variable representative) value))
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
          when (unless (ed-message-p relation)
                 (or (eq feature (vsym "INST")) 
                     (eq feature (vsym "ARG0"))
                     (eq feature (vsym "C-ARG"))))
          do (setf instance (fvpair-value fvpair)))
      (let* ((name (or
                    (and instance (var-p instance) (var-string instance))
                    (and event (var-p event) (var-string event))
                    (format nil "_~d" (incf %eds-variable-counter%)))))
        (setf (gethash relation %eds-symbol-table%) name)))))

(defun ed-find-representative (eds variable &optional (selectp t))
  (or (gethash (cons variable selectp) %eds-representatives-table%)
      (setf (gethash (cons variable selectp) %eds-representatives-table%)
        (cond
         ((ed-handle-p variable)
          (loop
              with alternate = (ed-variable-equivalence variable)
              with name = (var-string (or alternate variable))
              with qeq = (ed-hcons-qeq eds name)
              for ed in (eds-relations eds)
              for handle = (ed-handle ed)
              when (and (not (ed-bleached-p ed))
                        (or (equal name handle) 
                            (and (ed-handle-p qeq) 
                                 (equal (var-string qeq) handle))))
              collect ed into candidates
              finally 
                (return
                  (if selectp
                    (if (and candidates (null (rest candidates)))
                      (first candidates)
                      (ed-select-representative
                       (append
                        (when (ed-handle-p qeq)
                          (ed-find-representative qeq nil))
                        (when (var-p alternate)
                          (ed-find-representative alternate nil))
                        candidates)))
                    candidates))))
         ((var-p variable)
          (loop
              with foo = (var-string variable)
              with name = (let ((bar (ed-variable-equivalence foo)))
                            (if  bar (var-string bar) foo))
              for ed in (eds-relations eds)
              for id = (unless (or (ed-bleached-p ed)
                                   (ed-quantifier-p ed))
                         (ed-id ed))
              when (equal name id) collect ed into candidates
              finally 
                (return
                  (if selectp
                    (if (and candidates (null (rest candidates)))
                      (first candidates)
                      (ed-select-representative candidates))
                    candidates))))
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
       unless (or (ed-message-p ed)
                  (char= (char (ed-id ed) 0) #\_)) collect ed into candidates
       finally (when (and candidates (null (rest candidates)))
                 (return (first candidates))))
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
                                    thereis (when (var-p value)
                                              (equal (var-string value) id)))
                           collect referrer)
       when referrers collect (cons (length referrers) ed) into candidates
       finally
         (return (rest (first (sort candidates #'> :key #'first)))))
   (first eds)))

(defun ed-handle-p (variable)
  (when (var-p variable)
    (is-handel-var variable)))

(defun ed-hcons-qeq (eds handle)
  (loop
      with name = (if (stringp handle) handle (when (ed-handle-p handle)
                                                (var-string handle)))
      for hcons in (eds-hcons eds)
      for scarg = (when (string-equal (hcons-relation hcons) "qeq")
                    (var-string (hcons-scarg hcons)))
      thereis 
        (when (equal name scarg) (hcons-outscpd hcons))))

(defun ed-variable-equivalence (variable)
  (if (var-p variable)
    (gethash (var-string variable) %eds-equivalences%)
    (gethash variable %eds-equivalences%)))

(defun ed-quantifier-p (ed)
  (or
   (eq (ed-type ed) :quantifier)
   (let ((flist (rel-flist (ed-raw ed))))
     (find *scope-feat* flist :key #'fvpair-feature))
   (let ((pred (ed-predicate ed)))
     (and (stringp pred) (string= (subseq pred (- (length pred) 2)) "_q")))))

(defun ed-message-p (thing)
  (when *eds-message-relation*
    (typecase thing
      (ed
       (let ((type (ed-predicate thing)))
         (or (eq (ed-type thing) :message)
             (and (ed-raw thing) (ed-message-p (ed-raw thing)))
             (and (stringp type)
                  (string= (subseq type (- (length type) 2)) "_m")))))
      (rel
       (let ((type (rel-pred thing)))
         (or (eq type *eds-message-relation*)
             (when (stringp type) (search "_m_rel" type))
             (ignore-errors
              (equal-or-subtype type *eds-message-relation*))))))))

(defun ed-fragment-p (ed)
  (when *eds-fragment-relation*
    (let ((pred (and (rel-p (ed-raw ed)) (rel-pred (ed-raw ed)))))
      (or (eq (ed-type ed) :fragment)
          (eq pred *eds-fragment-relation*)
          (ignore-errors (equal-or-subtype pred *eds-fragment-relation*))))))

(defun ed-bleached-p (ed)
  (or 
   (and (null *eds-include-messages-p*) (eq (ed-type ed) :message))
   (and (null *eds-include-quantifiers-p*) (eq (ed-type ed) :quantifier))
   (when *eds-bleached-relations*
     (loop
         with predicate = (ed-predicate ed)
         for foo in *eds-bleached-relations*
         for type = (if (stringp foo) (vsym foo) foo)
         thereis (or (eq predicate type) 
                     (ignore-errors (equal-or-subtype predicate type)))))))

(defun ed-vacuous-p (ed)
  (unless *eds-include-vacuous-relations-p*
    (unless (and *eds-include-quantifiers-p* (eq (ed-type ed) :quantifier))
      (or (null (ed-arguments ed))
          (and (null (rest (ed-arguments ed)))
               (eq (first (first (ed-arguments ed))) (vsym "CARG")))))))

(defun ed-suspicious-p (eds)
  (append (when (ed-cyclic-p eds) '(:cyclic))
          (when (ed-fragmented-p eds) '(:fragmented))))

(defun ed-cyclic-p (eds)
  (loop
      with return = nil
      for ed in (eds-relations eds)
      unless (or (ed-bleached-p ed) (null (ed-arguments ed))) do
        (unless (ed-walk ed)
          (pushnew :cyclic (ed-status ed))
          (setf return t))
      finally 
        (when return (pushnew :cyclic (eds-status eds)))
        (return return)))

(defun ed-walk (ed &optional (start (list (ed-id ed)) startp))
  ;;
  ;; _fix_me_
  ;; on certain platforms (notably Linux), Allegro CL 6.2 will fail to detect a
  ;; stack overflow in (certain) recursive functions; this was pointed out to
  ;; Franz as [spr27625] in apr-03, and soon my favourite tech person, Lois 
  ;; Wolf, suggested to make those functions not-inline.       (19-may-03; oe)
  ;;
  #+:null
  (declare (notinline ed-walk))
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
    ;;
    ;; put .mark. on all EDs that are `reachable' from the top variable
    ;;
    (loop
        for ed = (pop agenda)
        for id = (and ed (ed-id ed))
        while ed do
          (unless (or (eq (ed-mark ed) mark) (ed-bleached-p ed))
            (setf (ed-mark ed) mark)
            ;;
            ;; put all arguments of current ED on agenda (for future marking)
            ;;
            (loop
                for argument in (ed-arguments ed)
                when (ed-p (rest argument)) do (push (rest argument) agenda))
            ;;
            ;; also, add all EDs for which the current one is an argument, i.e.
            ;; the inverse link.
            ;;
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
        when (and (not (ed-bleached-p ed)) 
                  (not (eq (ed-mark ed) mark))
                  (not (and (ed-quantifier-p ed)
                            (loop
                                with id = (ed-id ed)
                                for ed in (eds-relations eds)
                                thereis (equal (ed-id ed) id)))))
        do 
          (pushnew :fragmented (ed-status ed))
          (setf return t)
        finally 
          (loop
              for ed in (eds-relations eds)
              when (eq (ed-type ed) :fragment) do (setf return nil))
          (when return (pushnew :fragmented (eds-status eds)))
          (return return))))

(defun ed-explode (eds &key (lnkp t) (cargp t) (propertyp t) collocationp
                            tagp abstractp)
  
  ;;
  ;; _fix_me_
  ;; to be more informative, particularly when predicates occur more than once
  ;; in an MRS, this would have to include characterization.   (23-jan-04; oe)
  ;; --- or another way of linking to surface positions, e.g. the new LNK set
  ;; of surface token identifiers (in the VM and YY spirit :-).
  ;;
  
  ;;
  ;; _fix_me_
  ;; not sure what the `variable' slot was intended for, but it appears to be
  ;; exclusively used in ed-explode(); make sure all EDs have a correct value.
  ;;                                                           (26-nov-04; oe)
  (loop
      with key = (vsym "ARG0")
      for ed in (eds-relations eds)
      for raw = (ed-raw ed)
      for roles = (and (rel-p raw) (rel-flist raw))
      for arg0 = (loop
                     for role in roles
                     when (eq (fvpair-feature role) key)
                     return (fvpair-value role))
      do (setf (ed-variable ed) arg0))

  (nconc
   (loop
       for ed in (eds-relations eds)
       for functor = (format
                      nil
                      "~a~@[(~a)~]"
                      (ed-linked-predicate ed :lnkp lnkp)
                      (and cargp (ed-carg ed)))
       unless (and (null (ed-status ed)) 
                   (or (ed-bleached-p ed) (ed-vacuous-p ed)))
       nconc
         (nconc
          ;;
          ;; _fix_me_
          ;; this is getting somewhat baroque: we want a way of including
          ;; quantifiers in this list, jointly with the EP introducing the
          ;; variable bound by the quantifier.                 (13-aug-04; oe)
          ;;
          (when (ed-quantifier-p ed)
            (let* ((target (loop
                               with id = (ed-id ed)
                               for ed in (eds-relations eds)
                               when (and (equal (ed-id ed) id)
                                         (not (ed-quantifier-p ed)))
                               return ed))
                   (result (when target
                             (list functor (vsym "ARG0")
                                   (format
                                    nil
                                    "~a~@[(~a)~]"
                                    (ed-linked-predicate target :lnkp lnkp)
                                    (and cargp (ed-carg target))))))
                   (abstraction (when (and target abstractp)
                                  (ed-linked-abstraction target :lnkp lnkp)))
                   (abstraction (when abstraction
                                  (list functor (vsym "ARG0") abstraction))))
              (nconc
               (when result (list (append (and tagp '(:ep)) result)))
               (when abstraction
                 (list (append (and tagp '(:ap)) abstraction))))))
          (loop
              for (role . value) in (ed-arguments ed)
              when (and (null tagp) (ed-p value)) collect
                (let ((argument (format
                                 nil
                                 "~a~@[(~a)~]"
                                 (ed-linked-predicate value :lnkp lnkp)
                                 (and cargp (ed-carg value)))))
                  (list functor role argument))
              into eps
              when (and abstractp (null tagp)
                        (ed-p value) (ed-abstraction value)) collect
                (let ((argument (format
                                 nil
                                 "~a~@[(~a)~]"
                                 (ed-linked-abstraction value :lnkp lnkp)
                                 (and cargp (ed-carg value)))))
                  (list functor role argument))
              into abstractions
              when (and tagp (ed-p value)) nconc
                (let ((argument (format
                                 nil
                                 "~a~@[(~a)~]"
                                 (ed-linked-predicate value :lnkp lnkp)
                                 (and cargp (ed-carg value)))))
                  (list role argument))
              into eps
              when (and abstractp tagp
                        (ed-p value) (ed-abstraction value)) nconc
                (let ((argument (format
                                 nil
                                 "~a~@[(~a)~]"
                                 (ed-linked-abstraction value :lnkp lnkp)
                                 (and cargp (ed-carg value)))))
                  (list role argument))
              into abstractions
              finally (return
                        (nconc
                         (if tagp
                           (and eps (list (append (list :ep functor) eps)))
                           eps)
                         (if tagp
                           (and abstractions
                                (list (append (list :ap functor) abstractions)))
                           abstractions))))))
                         
   
   (when propertyp
     (loop
         for ed in (eds-relations eds)
         unless (or (and (null (ed-status ed)) (ed-bleached-p ed))
                    (ed-quantifier-p ed)
                    (ed-message-p ed)
                    (not (var-p (ed-variable ed))))
         nconc
           (loop
               with id = (format
                          nil
                          "~a~@[(~a)~]"
                          (ed-linked-predicate ed :lnkp lnkp)
                          (and cargp (ed-carg ed)))
               with ad = (ed-linked-abstraction ed :lnkp lnkp)
               for extra in (var-extra (ed-variable ed))
               for value = (format nil "~(~a~)" (extrapair-value extra))
               when (and (null tagp) value) collect 
                 (list id (extrapair-feature extra) value)
               into indices
               when (and abstractp ad (null tagp) value) collect 
                 (list ad (extrapair-feature extra) value)
               into abstractions
               when (and tagp value) nconc
                 (list (extrapair-feature extra) value)
               into indices
               when (and abstractp ad tagp value) nconc
                 (list (extrapair-feature extra) value)
               into abstractions
               finally
                 (return
                   (nconc
                    (if tagp
                      (and indices (list (append (list :index id) indices)))
                      indices)
                    (if tagp
                      (and abstractions
                           (list (append (list :andex ad) abstractions)))
                      abstractions))))))
                    
   (when collocationp
     (loop
         with functors
         = (loop
               for ed in (eds-relations eds)
               for functor = (format
                              nil
                              "~a~@[(~a)~]"
                              (ed-linked-predicate ed :lnkp lnkp)
                              (and cargp (ed-carg ed)))
               for abstraction = (ed-linked-abstraction ed :lnkp lnkp)
               unless (and (null (ed-status ed)) (ed-bleached-p ed))
               collect (cons functor abstraction) into functors
               finally
                 (return (sort functors #'string< :key #'first)))
         for functor in functors
         for i from 1
         nconc
           (loop
               for match in (nthcdr i functors)
               collect (list (first functor) (first match))
               when (and abstractp (rest functor))
               collect (list (rest functor) (first match))
               when (and abstractp (rest match))
               collect (list (first functor) (rest match))
               when (and abstractp (rest functor) (rest match))
               collect (list (rest functor) (rest match)))))))

(defun ed-reset ()
  (setf %eds-variable-counter% 0)
  (unless *eds-debug-p*
    (clrhash %eds-symbol-table%)
    (clrhash %eds-representatives-table%)
    (clrhash %eds-equivalences%)))
