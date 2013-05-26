;;; Copyright (c) 2001--2012 Stephan Oepen (oe@ifi.uio.no);
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

(defparameter *eds-include-vacuous-relations-p* t)

(defparameter *eds-message-relation* (vsym "message_m_rel"))

(defparameter *eds-fragment-relation* (vsym "unknown_rel"))

(defparameter *eds-bleached-relations* (list (vsym "selected_rel")))

(defparameter *eds-quantifier-argument* (vsym "BV"))

(defparameter *eds-non-representatives*
  (list (vsym "appos_rel")))

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
  handle id properties type variable
  predicate arguments carg
  lnk raw status mark abstraction)

(defmethod print-object ((object ed) stream)
  (if *eds-pretty-print-p*
    (loop
        with carg = (vsym "CARG")
        initially
          (format
           stream 
           "~(~a~):~(~a~)~@[(~s)~][" 
           (ed-id object) (ed-linked-predicate object) (ed-carg object))
        for (role . value) in (ed-arguments object)
        unless (eq role carg) do
          (format 
           stream 
           "~:[, ~;~]~:@(~a~) ~@[~(~a~)~]~@[:~(~a~)~]~@[(~s)~]"
           (eq role (first (first (ed-arguments object))))
           role (if (ed-p value) (ed-id value) "")
           (unless *eds-include-vacuous-relations-p*
             (if (ed-p value) (ed-predicate value) value))
           (unless *eds-include-vacuous-relations-p*
             (when (ed-p value) (ed-carg value))))
        finally
          (format stream "]"))
    (call-next-method)))

(defun ed-linked-predicate (ed &key (lnkp t))
  (let ((predicate (or (ed-predicate ed) "_"))
        (lnk (ed-lnk ed)))
    (case (and lnkp (first (ed-lnk ed)))
      (:id
       (format nil "~a<@~a>" predicate (second lnk)))
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

(defun ed-output-psoa (psoa &key (stream t) (format :ascii) (propertyp t)
                                 cargp markp lnkp collocationp abstractp
                                 sortp dmrsp (n 0))
  (case format
    (:ascii
     (if (psoa-p psoa)
       (format stream "~a~%" (ed-convert-psoa psoa))
       (format stream "{}~%")))
    (:triples
     (let* ((eds (ed-convert-psoa psoa))
            (triples
             (if dmrsp
               (dmrs-explode (rmrs-to-dmrs (mrs-to-rmrs psoa)))
               (ed-explode
                eds
                :lnkp lnkp :cargp cargp :propertyp propertyp
                :collocationp collocationp :abstractp abstractp))))
       (when sortp
         (setf triples
           (sort
            triples
            #'(lambda (foo bar)
                (or (string< (first foo) (first bar))
                    (and (string= (first foo) (first bar))
                         (string< (second foo) (second bar))))))))
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
               with carg = (vsym "CARG")
               with firstp = t
               for ed in (eds-relations eds)
               do
                 (format
                  stream
                  "~@[newline~*~] ~a \"~(~a~)\" \":~(~a~)~@[(~a)~][\" #X["
                  (not firstp)
                  (record (ed-id ed)) (ed-id ed)
                  (ed-linked-predicate ed) (ed-carg ed))
                 (setf firstp nil)
                 (loop
                     with firstp = t
                     for (role . value) in (ed-arguments ed)
                     unless (eq role carg) do
                       (format
                        stream
                        "~@[\", \" ~*~]~@[wrap ~*~]\"~a \" ~
                         ~@[~a ~]~@[\"~(~a~)\" ~]~
                         ~@[\":~(~a~)\"~]~@[\"(~(~a~))\"~]"
                        (not firstp)
                        (and (not firstp)
                             (null *eds-include-vacuous-relations-p*))
                        role
                        (and (ed-p value) (record (ed-id value)))
                        (and (ed-p value) (ed-id value))
                        (unless *eds-include-vacuous-relations-p*
                          (if (ed-p value) (ed-predicate value) value))
                        (unless *eds-include-vacuous-relations-p*
                          (and (ed-p value) (ed-carg value))))
                       (setf firstp nil)
                     finally (format stream "] \"]\"~%"))
               finally 
                 (format 
                  stream
                  "] newline \"}\"]~%~
                   #M[]"))))))
    (:html
     (let ((eds (ed-convert-psoa psoa)))
       (format stream "<table class=mrsEds>~%")
       (format stream "<tr><td>")
       (let ((properties (when propertyp
                           (loop
                               with top = (eds-top eds)
                               for ed in (eds-relations eds)
                               when (equal (ed-id ed) top)
                               return (ed-properties ed)))))
         (declare (ignore properties))
         (mrs-variable-html (eds-top eds) nil n nil stream))
       (format stream ":</td></tr>~%")
       (loop
           for ed in (eds-relations eds)
           for lnk = (ed-lnk ed)
           unless (or (ed-bleached-p ed) (ed-vacuous-p ed))
           do
             (format 
              stream 
              "<tr><td nowrap~:[~*~*~; onMouseOver=\"highlight(~a, ~a)\" ~
                     onMouseOut=\"highlight()\"~]>&nbsp;"
              (and lnk (eq (first lnk) :characters)) (second lnk) (third lnk))
             (mrs-variable-html (ed-id ed) nil n nil stream)
             (if (and propertyp (ed-properties ed))
               (let* ((string (make-string-output-stream)))
                 (format string "<table class=mrsProperties>")
                 (loop
                     for property in (ed-properties ed)
                     do
                       (format 
                        string 
                        "<tr><td class=mrsPropertyFeature>~a~
                           <td class=mrsPropertyValue>~(~a~)</td></tr>"
                        (extrapair-feature property)
                        (extrapair-value property)))
                 (format string "</table>")
                 (format
                  stream 
                  ":<span onMouseOver=\"writetxt('~a')\" ~
                      onMouseOut=\"writetxt(0)\">~(~a~)</span>"
                  (get-output-stream-string string) (ed-predicate ed)))
               (format stream ":~(~a~)" (ed-predicate ed)))
             (output-lnk (ed-lnk ed) :stream stream :format :html)
             (format stream "~@[(~s)~][" (ed-carg ed))
             (loop
                 with firstp = t
                 for (role . value) in (ed-arguments ed)
                 when (ed-p value)
                 do
                   (format stream "~:[,&nbsp;~;~]~a&nbsp;" firstp role)
                   (mrs-variable-html (ed-id value) nil n nil stream)
                   (setf firstp nil))
             (format stream "]</td></tr>~%"))
       (format stream "</table>~%")))
    (:latex
     (labels ((variable (id &optional properties)
                (let ((type (latex-escape-string (subseq id 0 1)))
                      (index (parse-integer id :start 1 :junk-allowed t)))
                  (format
                   nil "\\svar{~a}{~a}{~@[~a~]}"
                   type index properties)))
              #+:null
              (properties (ed)
                ""))
       (loop
           with eds = (ed-convert-psoa psoa)
           initially
             (format stream "\\eds{~a}{~%" (variable (eds-top eds)))
           for relations on (eds-relations eds)
           for ed = (first relations)
           for predicate = (latex-escape-string (ed-predicate ed))
           for carg = (and (ed-carg ed) (latex-escape-string (ed-carg ed)))
           for lnk = (when (ed-lnk ed)
                       (output-lnk (ed-lnk ed) :format :latex :stream nil))
           unless (ed-bleached-p ed) do
             (format
              stream "  \\sep{~a}{\\spred{~a~@[~a~]~@[(~a)~]}}{"
              (variable (ed-id ed)) predicate lnk carg)
             (loop
                 for (role . value) in (ed-arguments ed)
                 for firstp = t then nil 
                 when (ed-p value) do
                   (format
                    stream "~:[, ~;~%    ~]\\srole{~a}{~a}"
                    firstp role (variable (ed-id value))))
             (format stream "}~@[~*\\\\~%~]" (rest relations))
           finally (format stream "}~%"))))))

#+:lkb
(defun ed-convert-edge (edge)
  (when (lkb::edge-p edge)
    (ed-convert-psoa (or (lkb::edge-mrs edge) (extract-mrs edge)))))

(defun ed-convert-psoa (psoa)
  (when (psoa-p psoa)
    (loop
        with eds = (make-eds :hcons (psoa-h-cons psoa) :raw psoa)
        initially (ed-reset)
        ;;
        ;; in a first pass through the EPs of the input MRS, create EDS graph
        ;; nodes, one per EP.  these will have their key key properties set
        ;; (predicate, handle, distinguished variable, CARG, LNK, et al.) but
        ;; not yet contain any outgoing arcs.
        ;;
        for relation in (psoa-liszt psoa)
        for ed = (ed-convert-relation relation)
        when ed do (push ed (eds-relations eds))
        finally
          (setf (eds-relations eds) (nreverse (eds-relations eds)))
          ;;
          ;; next, post-process the EDS graph: first, classify `special' EDs,
          ;; i.e. messages, quantifiers, and so-called fragment EDs (which are
          ;; pseudo relations, much like underspecified conjunctions, used in
          ;; the LOGON NoEn MT system, to glue together XLE fragment analyses).
          ;; in the case of messages, there is optional support to equate the
          ;; label of the message with its SOA or MARG argument, i.e. `bleach'
          ;; the message, in the sense of making it gratuitous for the graph.
          ;;
          (ed-bleach-eds eds)
          ;;
          ;; next, actually fill in argument arcs: for each role in each ED,
          ;; find the ED that is assumed to be the `representative' for the
          ;; value (i.e. MRS variable) of this role; in doing so, consider =q
          ;; handle constraints as if the top and bottom handle were equated.
          ;; in the case of multiple candidate representative EDs, various 
          ;; disambiguation heuristics apply, see ed-select-representative().
          ;;
          (ed-augment-eds eds)
          ;;
          ;; finally, we need to make sure that all EDs end up with unique
          ;; identifiers, which from here on only serve to uniquely name the
          ;; nodes of the EDS dependency graph.  seeing that, in the initial
          ;; ED creation phase, ED identifiers were locally determined by the
          ;; distinguished variable, there is no protection against multiple
          ;; EDs sharing one distinguished variable (even though, with current
          ;; versions of the ERG at least, that seems likely only in illformed
          ;; input MRSs). 
          ;;
          (ed-uniq-ids eds)
          ;;
          ;; finally, determine what should be the root node of the dependency
          ;; graph: until early 2012, we always used to grab the INDEX, but in
          ;; a recent moment of clarity, we realized that the root node should
          ;; be parallel to the structure we create for clause subordination,
          ;; e.g. pairs like: `[Kim knows that] Abrams probably arrived.'
          ;;
          (let* ((ltop (ed-find-representative eds (psoa-top-h psoa)))
                 (index (ed-find-representative eds (psoa-index psoa))))
            (setf (eds-top eds)
              (or (and (ed-p ltop) (ed-id ltop))
                  (and (ed-p index) (ed-id index))
                  (and (var-p (psoa-index psoa))
                       (var-string (psoa-index psoa))))))
          (ed-reset)
          (return eds))))

(defun ed-convert-relation (relation)
  (when (and *eds-quantifier-argument* (is-quant-rel relation))
    ;;
    ;; to simplify the downstream treatment of quantifiers, make sure the label
    ;; of the bound variable is not ARG0.  but avoid destructive changes to our
    ;; original input structure; this is potentially hazardous, as the hash of
    ;; relations to variables (%eds-symbol-table%) now uses a local copy; for
    ;; all i can tell just now, access to the hash table is within the scope
    ;; of ed-convert-relation(), however.                      (15-jun-12; oe).
    ;;
    (setf relation (copy-rel relation))
    (setf (rel-flist relation)
      (loop
          with inherent
          = (list (vsym "INST") (vsym "ARG0"))
          for fvpair in (rel-flist relation)
          when (member (fvpair-feature fvpair) inherent :test #'eq)
          collect (make-fvpair
                   :feature *eds-quantifier-argument*
                   :value (fvpair-value fvpair))
          else collect fvpair)))
  (let* ((*package* (find-package :lkb))
         (handle (let ((handle (rel-handel relation)))
                   (when (ed-handle-p handle) (var-string handle))))
         (identifier (ed-find-identifier relation))
         (predicate (when (rel-pred relation)
                      (string-downcase (string (rel-pred relation)))))
         (abstraction
          #+:ppcre
          (when predicate
            (cond
             ((ppcre:scan "_a(?:_[^_]+)?_rel" predicate) :a)
             ((ppcre:scan "_n(?:_[^_]+)?_rel" predicate) :n)
             ((ppcre:scan "_p(?:_[^_]+)?_rel" predicate) :p)
             ((ppcre:scan "_q(?:_[^_]+)?_rel" predicate) :q)
             ((ppcre:scan "_v(?:_[^_]+)?_rel" predicate) :v)
             (t :x))))
         (predicate (if (and predicate (stringp *sem-relation-suffix*))
                      (remove-right-sequence 
                       (string-downcase *sem-relation-suffix*) predicate)
                      predicate))
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
    (make-ed
     :handle handle
     :id (first identifier) :properties (rest identifier)
     :predicate predicate :lnk lnk :carg carg :abstraction abstraction
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
              (when (eq key old) (setf key (vsym "CARG")))
              #+:null
              (when (ed-p representative)
                (setf (ed-variable representative) value))
              (push (cons key representative) (ed-arguments ed)))
        (setf (ed-arguments ed) (nreverse (ed-arguments ed)))))

(defun ed-uniq-ids (eds)
  (loop
      for ed in (eds-relations eds)
      for id = (ed-id ed)
      for collisions
      = (loop
            for ed in (eds-relations eds)
            when (equal (ed-id ed) id) collect ed)
      when (rest collisions) do
        (let ((representative
               (loop
                   for foo being each hash-key in %eds-representatives-table%
                   when (and (consp foo) (var-p (first foo))
                             (string= (var-string (first foo)) id)
                             (rest foo))
                   return (gethash foo %eds-representatives-table%))))
          (when representative
            (setf collisions (delete representative collisions)))
          (setf collisions
            (sort
             collisions #'>
             :key #'(lambda (ed) (length (ed-arguments ed)))))
          (loop
              for ed in (if representative collisions (rest collisions))
              do
                (setf (ed-id ed)
                  (format nil "_~d" (incf %eds-variable-counter%)))))))

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
      (let* ((variable (or (and (var-p instance) instance)
                           (and (var-p event) event)))
             (name (if variable
                     (var-string variable)
                     (format nil "_~d" (incf %eds-variable-counter%))))
             (properties (and variable (var-extra variable))))
        (setf (gethash relation %eds-symbol-table%) (cons name properties))))))

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
                       eds
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
                      (ed-select-representative eds candidates))
                    candidates))))
         ((stringp variable) variable)))))

(defun ed-select-representative (eds candidates)
  ;;
  ;; given a set of candidate representatives (typically corresponding to EPs
  ;; within one LF conjunct, i.e. sharing the same handle), heuristically find
  ;; one to stand in for the conjunction: give preference to EPs with a `real'
  ;; intrinsic variable (i.e. ones introducing an event or instance variable) 
  ;; or look at the dependency topology among the candidate EPs and choose one
  ;; that occurs as an argument to the other(s).
  ;;
  (or
   (when *eds-non-representatives*
     (let ((candidates (loop
                           for ed in candidates
                           unless (ed-non-representative-p ed)
                           collect ed))
           (*eds-non-representatives* nil))
       (ed-select-representative eds candidates)))
   (loop
       for ed in candidates
       unless (or (ed-message-p ed)
                  (char= (char (ed-id ed) 0) #\_)) collect ed into candidates
       finally (when (and candidates (null (rest candidates)))
                 (return (first candidates))))
   (loop
       for ed in candidates
       unless (char= (char (ed-id ed) 0) #\_) collect ed into candidates
       finally (when (and candidates (null (rest candidates)))
                 (return (first candidates))))
   (loop
       for ed in candidates
       unless (eq (ed-type ed) :quantifier) collect ed into candidates
       finally (when (and candidates (null (rest candidates)))
                 (return (first candidates))))
   ;;
   ;; the most common cause of one-to-many correspondences between a variable
   ;; and a set of EPs are handles shared with (intersective) modifiers, e.g.
   ;; in a structure like [believe that] `she arrived very quickly.'  here, the
   ;; degree specifier is an intersective modifier on the adverb, which in turn
   ;; is an intersective modifier on the arriving event; thus, all three share
   ;; one label, and `arrive' is the ARG1 of `quickly', which is the ARG1 of
   ;; `very'.  to pick out `arrive' in this scenario, count recursive referrals 
   ;; (aka argumenthood), such that `arrive' receives an incoming count of 2.
   ;;
   (labels ((referrers (target candidates)
              (loop
                  with id = (ed-id target)
                  for ed in candidates
                  for flist = (unless (eq ed target) (rel-flist (ed-raw ed)))
                  when (loop
                           for fvpair in flist
                           for value = (fvpair-value fvpair)
                           thereis (when (var-p value) 
                                     (equal (var-string value) id)))
                  append
                    (cons ed (referrers ed (remove target candidates)))))
            (incoming (candidates &optional (eds candidates))
              (let ((referrers
                     (loop
                         for candidate in candidates
                         for referrers = (referrers candidate eds)
                         when referrers
                         collect (cons (length referrers) candidate))))
                (setf referrers (sort referrers #'> :key #'first))
                (loop
                    with n = (first (first referrers))
                    for referrer in referrers
                    while (= (first (first referrers)) n)
                    collect (rest referrer))))
            (outgoing (candidates)
              (let ((references
                     (loop
                         for candidate in candidates
                         for n = (length (ed-arguments candidate))
                         collect (cons n candidate))))
                (setf references (sort references #'> :key #'first))
                (loop
                    with n = (first (first references))
                    for reference in references
                    while (= (first reference) n) collect (rest reference)))))
     (or
      (let* ((local (incoming candidates))
             (global (incoming local (eds-relations eds)))
             (outgoing (outgoing global)))
        ;;
        ;; use incoming links among .candidates. as the first criterion; then
        ;; incoming links against the structure at large (see below); finally,
        ;; count outgoing links to break ties, if need be.  note that, given a
        ;; non-empty list, outgoing() always returns a non-empty result, hence
        ;; we only need two sub-clauses in the or() below.
        ;;
        (or (first outgoing) (first local)))
      ;;
      ;; from here on, we are grasping at straws (and likely looking at input
      ;; structures that are not perfectly well-formed).  if there still is a
      ;; need for disambiguation at this point, give preference to nodes that
      ;; have more incoming links, i.e. are more connected to the structure at
      ;; large (as arguments).
      ;;
      (let* ((global (incoming candidates (eds-relations eds)))
             (outgoing (outgoing global)))
        (or (first outgoing) (first global)))
      ;;
      ;; finally, give preferences to nodes that have more outgoing links.
      ;;
      (first (outgoing candidates))))))

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
         with predicate = (and (rel-p (ed-raw ed)) (rel-pred (ed-raw ed)))
         for foo in *eds-bleached-relations*
         for type = (if (stringp foo) (vsym foo) foo)
         thereis (or (eq predicate type) 
                     (ignore-errors (equal-or-subtype predicate type)))))))

(defun ed-non-representative-p (ed)
  (when *eds-non-representatives*
    (loop
        with pred = (and (rel-p (ed-raw ed)) (rel-pred (ed-raw ed)))
        for foo in *eds-non-representatives*
        for type = (if (stringp foo) (vsym foo) foo)
        thereis (ignore-errors (equal-or-subtype pred type)))))

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
  ;;
  ;; _fix_me_
  ;; why not use the actual ED structures, to protect against cycles?
  ;;                                                           (25-feb-12; oe)
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
                  (not (eq (ed-mark ed) mark)))
        do
          (pushnew :fragmented (ed-status ed))
          (setf return t)
        else when (eq (ed-mark ed) mark)
        do (setf (ed-status ed) (delete :fragmented (ed-status ed)))
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

(defun ed-flag (eds &optional ed 
                &key (up t) (down t) n top bottom (mark (gensym "")))
  (loop for ed in (eds-relations eds) do (setf (ed-mark ed) nil))
  (labels ((prefixp (prefix list &key (test #'eql))
             (loop
                 for foo in prefix
                 for bar in list
                 always (funcall test foo bar))))
    (let* ((ed (or ed (find (eds-top eds) (eds-relations eds) :key #'ed-id)))
           (agenda (and ed (list (list ed)))))
      ;;
      ;; put .mark. on all EDs that are `reachable' from the top-level .ed.
      ;;
      (loop
          with *package* = (find-package :lkb)
          for bucket = (pop agenda)
          for current = (first bucket)
          for path = (rest bucket)
          while bucket
          unless (or (ed-bleached-p current)
                     (when (eq (first (ed-mark current)) mark)
                       (member path (rest (ed-mark current)) :test #'prefixp)))
          do
            #+:null
            (format
             t "~a [~{~a~^ ~}] <-- ~a~%"
             (ed-predicate current) (rest (ed-mark current)) path)
            (if (ed-mark current)
              (push path (rest (ed-mark current)))
              (setf (ed-mark current) (list mark path)))
          when (and down
                    (or (null n) (<= (length path) n))
                    (not (and bottom (funcall bottom eds current)))) do
            ;;
            ;; put all arguments of current ED on agenda (for future marking)
            ;;
            (loop
                for (role . value) in (ed-arguments current)
                when (and (ed-p value)
                          (not (and (consp (first path))
                                    (eq role (first (first path))))))
                do (push (cons value (cons role path)) agenda))
          when (and up
                    (or (null n) (<= (length path) n))
                    (not (and top (funcall top eds current)))) do
            ;;
            ;; also, add all EDs for which the current one is an argument, i.e.
            ;; the inverse link.
            ;;
            (loop
                for ed in (eds-relations eds)
                unless (or (ed-bleached-p ed) (eq (first (ed-mark ed)) mark))
                do
                  (loop
                      for (role . value) in (ed-arguments ed)
                      when (eq value current)
                      do (push (cons ed (cons (list role) path)) agenda)))))
    (loop
        for ed in (eds-relations eds)
        when (eq (first (ed-mark ed)) mark)
        do
          (setf (rest (ed-mark ed))
            (loop
                for path in (rest (ed-mark ed))
                collect (reverse path))))
    eds))

(defun ed-heads (eds ed &optional (type :direct))
  (let ((mark (gensym "")))
    (ed-flag
     eds ed
     :up t :down nil :n (and (eq type :direct) 1) :mark mark)
    (loop
        for ed in (eds-relations eds)
        when (and (consp (ed-mark ed)) (eq (first (ed-mark ed)) mark))
        collect ed)))

(defun ed-dependents (eds ed &optional (type :direct))
  (let ((mark (gensym "")))
    (ed-flag
     eds ed
     :up nil :down t :n (and (eq type :direct) 1) :mark mark)
    (loop
        for ed in (eds-relations eds)
        when (and (consp (ed-mark ed))
                  (eq (first (ed-mark ed)) mark))
        collect ed)))

(defun dmrs-explode (dmrs)
  (labels ((label (id)
             (loop
                 for node in (dmrs-nodes dmrs)
                 when (= (dmrs-node-id node) id)
                 return
                   (let* ((pred (dmrs-node-pred node))
                          (pred (if (realpred-p pred)
                                  (format
                                   nil "_~a_~a~@[_~a~]"
                                   (realpred-lemma pred)
                                   (realpred-pos pred)
                                   (realpred-sense pred))
                                  (ppcre::regex-replace "_rel$" pred "")))
                          (from (dmrs-node-cfrom node))
                          (to (dmrs-node-cto node))
                          (carg (dmrs-node-carg node)))
                     (format
                      nil "~a~:[~*~*~;<~a:~a>~]~@[(~a)~]"
                      pred (and from to) from to carg)))))
    (loop
        with rstr = (vsym "RSTR") with arg0 = (vsym "ARG0")
        for link in (dmrs-links dmrs)
        for from = (label (dmrs-link-from link))
        for to = (label (dmrs-link-to link))
        for role = (let* ((role (dmrs-link-pre link))
                          (role 
                           (cond
                            ((null role)
                             (vsym (format nil "/~a" (dmrs-link-post link))))
                            ((stringp role)
                             (vsym role))
                            (t role))))
                     (if (eq role rstr) arg0 role))
        when role
        collect (list from role to))))
