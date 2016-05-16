;;; Copyright (c) 2001--2014 Stephan Oepen (oe@ifi.uio.no);
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

(defparameter *eds-predicate-filter* nil)

(defparameter *eds-quantifier-argument* (vsym "BV"))

(defparameter *eds-untensed* (list (cons (vsym "TENSE") (vsym "untensed"))))

(defparameter *eds-non-representatives*
  (list (vsym "appos_rel") (vsym "focus_d_rel") (vsym "parg_d_rel")))

(defparameter *eds-predicate-modifiers*
  (list (ppcre:create-scanner "_x_deg_rel$")))

(defparameter *eds-show-lnk-p* t)

(defparameter *eds-show-properties-p* t)

(defparameter *eds-show-status-p* nil)

(defparameter %eds-variable-counter% 0)

(defparameter %eds-symbol-table% (make-hash-table))

(defparameter %eds-representatives-table% (make-hash-table :test #'equal))

(defparameter %eds-equivalences% (make-hash-table :test #'equal))

(defparameter %eds-filter% nil)

(defparameter %eds-relevant-features% 
  '("ARG" "ARG1" "ARG2" "ARG3" "ARG4" "BV" 
    "L-INDEX" "R-INDEX" "L-HNDL" "R-HNDL" "CARG"
    "SOA" "CONST_VALUE" "TERM1" "TERM2" "FACTOR1" "FACTOR2"
    "MARG" "L-HANDEL" "R-HANDEL" "MAIN" "SUBORD" "ROLE" "HINST" "NHINST"))

(defstruct eds
  top relations hcons raw status)

(defmethod print-object ((object eds) stream)
  (if *eds-pretty-print-p*
    (let ((cyclicp (eds-cyclic-p object))
          (fragmentedp (eds-fragmented-p object)))
      (loop
          initially
            (format 
             stream 
             "{~@[~(~a~):~]~
              ~:[~3*~; (~@[cyclic~*~]~@[ ~*~]~@[fragmented~*~])~]~@[~%~]" 
             (eds-top object) 
             (and *eds-show-status-p* (or cyclicp fragmentedp) )
             cyclicp (and cyclicp fragmentedp) fragmentedp
             (eds-relations object))
          for ed in (eds-relations object)
          unless (or (and (null (ed-status ed))
                          (or (ed-bleached-p ed) (ed-vacuous-p ed)))
                     (member ed %eds-filter% :test #'eq))
          do
            (format 
             stream 
             "~c~a~%"
             (cond
              ((null *eds-show-status-p*) #\Space)
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
  lnk inverse raw status mark abstraction)

(defmethod print-object ((object ed) stream)
  (if *eds-pretty-print-p*
    (loop
        with carg = (vsym "CARG")
        initially
          (format
           stream 
           "~(~a~):~(~a~)~@[(~s)~]" 
           (ed-id object) (ed-linked-predicate object) (ed-carg object))
          (when (and *eds-show-properties-p* (ed-properties object))
            (loop
                with *package* = (find-package *mrs-package*)
                with type = (first (ed-properties object))
                with properties = (if (extrapair-p type)
                                    (ed-properties object)
                                    (rest (ed-properties object)))
                initially
                  (format 
                   stream "{~@[~(~a~)~]~@[ ~]"
                   (unless (extrapair-p type) type) properties)
                finally (format stream "}")
                for property in properties
                do
                  (format
                   stream "~:[~;, ~]~a ~(~a~)"
                   (not (eq property (first properties)))
                   (extrapair-feature property)
                   (extrapair-value property))))
          (format stream "[")
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

(defun ed-linked-predicate (ed &key (lnkp *eds-show-lnk-p*))
  (let ((predicate (or (ed-predicate ed) "_"))
        (lnk (ed-lnk ed)))
    ;;
    ;; _fix_me_
    ;; why not use output-lnk(), to avoid code duplication?    (27-feb-16; oe)
    ;;
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

(defun eds-output-psoa (psoa &key (stream t) (format :ascii)
                                  (lnkp *eds-show-lnk-p*)
                                  (propertiesp *eds-show-properties-p*)
                                  cargp collocationp abstractp sentinelp
                                  sortp dmrsp (n 0) 
                                  (filter *eds-predicate-filter*)
                                  input id debug
                                  (prefix "") (columns *print-right-margin*))

  (when (null stream)
    (return-from eds-output-psoa
      (with-output-to-string (stream)
        (eds-output-psoa
         psoa :stream stream :format format :lnkp lnkp :propertiesp propertiesp
         :cargp cargp :collocationp collocationp :abstractp abstractp
         :sentinelp sentinelp :sortp sortp :dmrsp dmrsp :n n :filter filter
         :input input :id id :debug debug :prefix prefix :columns columns))))
  
  (let ((eds (if (eds-p psoa) psoa (eds-convert-psoa psoa)))
        (%eds-filter% nil)
        (*eds-show-lnk-p* lnkp)
        (*eds-show-properties-p* propertiesp))
    (when filter
      (when (stringp filter) (setf filter (ppcre::create-scanner filter)))
      (loop
          for ed in (eds-relations eds)
          when (and (ppcre:scan filter (ed-predicate ed))
                    (null (ed-inverse ed))
                    (not (string= (ed-id ed) (eds-top eds))))
          do (push ed %eds-filter%)))

    (case format
      ((:ascii :native)
       (cond
        ((eds-p psoa)
         (format stream "~a~%" psoa))
        ((eds-p eds)
         (format stream "~a~%" eds))
        (t
         (format stream "{}~%"))))
      (:triples
       (let* ((triples
               (if dmrsp
                 (dmrs-explode (rmrs-to-dmrs (mrs-to-rmrs psoa)))
                 (eds-explode
                  eds
                  :cargp cargp
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
             initially (unless sentinelp (format stream "{~%"))
             for triple in triples
             do
               (format
                stream
                "~:[  ~;<s> ~]~{~a~^ ~}~:[  ~; </s>~]~%"
                sentinelp triple sentinelp)
             finally (unless sentinelp (format stream "}~%~%")))
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
                     #M[]")))))
      (:html
       (format stream "<table class=mrsEds>~%")
       (format stream "<tr><td>")
       (mrs-variable-html (eds-top eds) nil n nil stream)
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
             (if (and propertiesp (ed-properties ed))
               (let* ((string (make-string-output-stream)))
                 (format string "<table class=mrsProperties>")
                 (loop
                     with type = (first (ed-properties ed))
                     with properties
                     = (if (extrapair-p type) 
                         (ed-properties ed)
                         (rest (ed-properties ed)))
                     initially
                       (unless (extrapair-p type)
                         (format
                          string
                          "<tr><td class=mrsPropertyFeature>~(~a~)
                           <td class=mrsPropertyValue>&nbsp;</td></tr>"
                          type))
                     for property in properties
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
       (format stream "</table>~%"))
      (:latex
       (labels ((variable (id &optional properties)
                  (let ((type (latex-escape-string (subseq id 0 1)))
                        (index (parse-integer id :start 1 :junk-allowed t)))
                    (format
                     nil "\\svar{~a}{~a}{~@[~a~]}"
                     type index properties))))
         (loop
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
             finally (format stream "}~%"))))
      (:amr
       (when id (format stream "~&# ::id ~a~%" id))
       (when input (format stream "~&# ::snt ~a~%" input))
       (let ((attic (make-hash-table :test #'eq))
             (*package* (find-package :lkb))
             (*print-right-margin* columns)
             (*standard-output* stream))
         (labels ((output (ed &optional ignore)
                    (let ((match (gethash ed attic)))
                      (if match
                        (write-string match)
                        (let* ((id (ed-id ed)))
                          (setf (gethash ed attic) id)
                          (write-char #\()
                          (pprint-logical-block (nil nil)
                            (write-string id)
                            (write-string " / ")
                            (if (position #\/ (ed-predicate ed))
                              (write (ed-predicate ed))
                              (write-string (ed-predicate ed)))
                            (when (and lnkp (ed-lnk ed))
                              (write-string " :lnk ")
                              (write (output-lnk (ed-lnk ed) :stream nil)))
                            (when (ed-carg ed)
                              (write-string " :carg ")
                              (write (ed-carg ed)))
                            (loop
                                for (role . value) in (ed-arguments ed)
                                for match = (find role ignore :key #'first)
                                unless (or (not (ed-p value))
                                           (member value %eds-filter%)
                                           (eq value (rest match)))
                                do 
                                  (write-char #\space)
                                  (pprint-newline :linear)
                                  (write-char #\:)
                                  (write role)
                                  (write-char #\space)
                                  (output value (cons (cons role ed) ignore)))
                            (loop
                                for (role . value) in (ed-inverse ed)
                                for match = (find role ignore :key #'first)
                                unless (or (not (ed-p value))
                                           (member value %eds-filter%)
                                           (eq value (rest match)))
                                do 
                                  (write-char #\space)
                                  (pprint-newline :linear)
                                  (write-char #\:)
                                  (write role)
                                  (write-string "-of ")
                                  (output value (cons (cons role ed) ignore)))
                            (write-char #\))))))))
           (let* ((top (find 
                        (eds-top eds) (eds-relations eds)
                        :key #'ed-id :test #'string=)))
             (when top (output top) (terpri stream) (terpri stream)))
           (when (and debug
                      (< (hash-table-count attic) (length (eds-relations eds))))
             (format 
              debug "eds-output-psoa(): lost in AMR syntax ~@[[~a]~]~%" 
              id))
             (loop
                 for ed in (eds-relations eds)
                 unless (gethash ed attic)
                 do (format debug "  ~a~%" ed)))))
      (:json
       (loop
           initially (format 
                      stream 
                      "~a{~:[~2*~;\"id\": ~a,~%~a~]~
                       ~:[~3*~;~:[~; ~]\"input\": ~s,~%~a~]~
                       ~:[~; ~]\"top\": ~s,~%~a \"nodes\": {~%"
                      prefix (numberp id) id prefix
                      (stringp input) (numberp id) input prefix
                      (or (numberp id) (stringp input)) (eds-top eds) prefix)
           with last = (first (last (eds-relations eds)))
           for ed in (eds-relations eds)
           for predicate = (ed-predicate ed)
           for carg = (ed-carg ed)
           for lnk = (when (ed-lnk ed) 
                       (output-lnk (ed-lnk ed) :format :json :stream nil))
           unless (or (ed-bleached-p ed)
                      (member ed %eds-filter%))
           do
             (format 
              stream 
              "~a   ~s: {\"label\": ~s~@[, \"lnk\": ~a~]~@[, \"carg\": ~s~]"
              prefix (ed-id ed) predicate lnk carg)
             (when (and *eds-show-properties-p* (ed-properties ed))
               (loop
                   with type = (first (ed-properties ed))
                   with properties = (if (extrapair-p type)
                                       (ed-properties ed)
                                       (rest (ed-properties ed)))
                   initially
                     (format
                      stream ", \"properties\": {~@[\"type\": ~s~]"
                      (unless (extrapair-p type) type))
                   finally (format stream "}")
                   for property in properties
                   do
                     (format
                      stream "~:[, ~;~]\"~a\": \"~(~a~)\""
                      (and (extrapair-p type) (eq property (first properties)))
                      (extrapair-feature property)
                      (extrapair-value property))))
             (loop
                 initially (format stream ", \"edges\": {")
                 for (role . value) in (ed-arguments ed)
                 for firstp = t then nil 
                 when (and (ed-p value)
                           (not (member value %eds-filter% :test #'eq)))
                 do
                   (format
                    stream "~:[, ~;~]~s: ~s"
                    firstp (string-upcase role) (ed-id value))
                 finally (format stream "}}"))
             (unless (eq ed last) (format stream ",~%"))
           finally (format stream "}}"))))))

#+:lkb
(defun eds-convert-edge (edge)
  (when (lkb::edge-p edge)
    (eds-convert-psoa (or (lkb::edge-mrs edge) (extract-mrs edge)))))

(defun eds-convert-psoa (psoa)
  (when (psoa-p psoa)
    (loop
        with eds = (make-eds :hcons (psoa-h-cons psoa) :raw psoa)
        initially (ed-reset)
        ;;
        ;; in a first pass through the EPs of the input MRS, create EDS graph
        ;; nodes, one per EP.  these will have their key properties set
        ;; (predicate, handle, distinguished variable, CARG, LNK, et al.) but
        ;; not yet contain any outgoing arcs.
        ;;
        for relation in (psoa-liszt psoa)
        for ed = (ed-convert-relation relation psoa)
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
          (eds-bleach-eds eds)
          ;;
          ;; next, actually fill in argument arcs: for each role in each ED,
          ;; find the ED that is assumed to be the `representative' for the
          ;; value (i.e. MRS variable) of this role; in doing so, consider =q
          ;; handle constraints as if the top and bottom handle were equated.
          ;; in the case of multiple candidate representative EDs, various 
          ;; disambiguation heuristics apply, see ed-select-representative().
          ;;
          (eds-augment-eds eds)
          ;;
          ;; as a matter of convenience, for example to output in AMR syntax,
          ;; cache inverse argument relations in each node.
          ;;
          (eds-inverse eds)
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
          (eds-uniq-ids eds)
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

(defun ed-convert-relation (relation mrs)
  
  (let (type)
    (when (and *eds-quantifier-argument* (is-quant-rel relation))
      ;;
      ;; to simplify downstream treatment of quantifiers, make sure the role 
      ;; name label of the bound variable is not ARG0.  but avoid destructive 
      ;; changes to our original input; this is potentially hazardous, as the
      ;; hash of relations to variables (%eds-symbol-table%) now uses a local
      ;; copy; for all i can tell just now, access to the hash table is within
      ;; the scope of ed-convert-relation(), however.          (15-jun-12; oe)
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
            else collect fvpair))
      (setf type :quantifier))
    ;;
    ;; arguably hacky: in the ERG analysis of degree specifiers on quantifiers,
    ;; say ‘nearly all’, there is no connection other than label identification
    ;; between the degree specifier and the quantifier.  this means there is no
    ;; connection in terms of an argument relation or actual logical variables,
    ;; and by default degree specifiers come out unconnected to the dependency
    ;; graph.  our somewhat hand-wavy interpretation of this analysis for some
    ;; ten years now has been in term of ‘predicate modification’, i.e. roughly
    ;; like ‘nearly(all)[x, h1, h2]’.  one may think the degree specifier should
    ;; take the (label of the) quantifier as its ARG1, but that structure cannot
    ;; be scope-resolved within current assumptions.  hence, mimic that argument
    ;; relation on the degree specifier in EDS conversion.       (8-feb-14; oe)
    ;;
    (when *eds-predicate-modifiers*
      (let* ((predicate (when (rel-pred relation)
                          (string-downcase (string (rel-pred relation)))))
             (arg1 (loop
                       with arg1 = (vsym "ARG1")
                       for argument in (rel-flist relation)
                       when (eq (fvpair-feature argument) arg1)
                       return argument)))
        (when (and predicate
                   (loop
                       for pattern in *eds-predicate-modifiers*
                       thereis
                         (if (functionp pattern)
                           (ppcre:scan pattern predicate)
                           (ed-compare-predicates
                            predicate pattern :type :subsumption)))
                   (and arg1 (var-p (fvpair-value arg1)) 
                        (string-equal (var-type (fvpair-value arg1)) "u")))
          (let* ((label (rel-handel relation)))
            (when (loop
                      for relation in (psoa-liszt mrs)
                      thereis (eq (rel-handel relation) label))
              (setf relation (copy-rel relation))
              (setf (rel-flist relation)
                (loop
                    with arg1 = (vsym "ARG1")
                    for argument in (rel-flist relation)
                    when (eq (fvpair-feature argument) arg1)
                    collect (make-fvpair
                             :feature (vsym "ARG1")
                             :value label)
                    else collect argument))
              (setf type :specifier))))))

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
       :type type :raw relation))))

(defun eds-bleach-eds (eds)
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

(defun eds-augment-eds (eds)
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

(defun eds-inverse (eds)
  (loop
      for ed in (eds-relations eds)
      do
        (loop
            for (role . value) in (ed-arguments ed)
            when (ed-p value)
            do (push (cons role ed) (ed-inverse value)))))

(defun eds-uniq-ids (eds)
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
             (type (and variable (var-type variable)))
             (properties (and variable (cons type (var-extra variable)))))
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
                                 (equal (var-string qeq) handle)))
                        (not (eq (ed-type ed) :specifier)))
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
  (when *eds-non-representatives*
    (return-from ed-select-representative
      (let ((candidates (loop
                            for ed in candidates
                            unless (ed-non-representative-p ed)
                            collect ed))
            (*eds-non-representatives* nil))
        (ed-select-representative eds candidates))))
   (when (or (null candidates) (null (rest candidates)))
     (return-from ed-select-representative (first candidates)))
   (or
    ;;
    ;; the following two disambiguation attempts are only historically relevant,
    ;; dis-preferring messages and nodes whose identifier was synthesized (i.e.
    ;; where there was no distinguished variable available, or it was shared
    ;; with another node, who got to own it).
    ;;
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
                   unless (ed-bleached-p ed)
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
                     while (= (first referrer) n)
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
         ;; we only need two sub-clauses in the or() below.  actually, as long
         ;; as .candidates. is a sub-set of the full list of relations (which in
         ;; the context of converting one MRS will always be the case), it would
         ;; seem impossible for .outgoing. to become empty, provided .local. is
         ;; non-empty.
         ;;
         (or (first outgoing) (first local)))
       ;;
       ;; in 1214 at least, mrs/991 (‘there were cats in the garden’) has ‘in_p’
       ;; share its label with the existential be, but the external argument of
       ;; the preposition actually is the instance variable of ‘cats’.  not sure
       ;; this really is the intended (or correct) analysis, as it fails to give
       ;; a parallel structure for ‘in the garden, there were cats’.  but either
       ;; way, to make sure we pick the existential ‘be’ over the preposition,
       ;; we need to dis-prefer untensed events.
       ;;
       (let ((tensed (loop
                         for ed in candidates
                         unless (ed-untensed-p ed) collect ed)))
         (when tensed
           (setf candidates tensed)
           (when (null (rest tensed)) (first tensed))))
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

(defun ed-compare-predicates (predicate1 predicate2 &key (type :equivalence))
  (if *normalize-predicates-p*
    (let ((predicate1 (normalize-predicate predicate1))
          (predicate2 (normalize-predicate predicate2)))
      (or (string= predicate1 predicate2)
          (when (eq type :subsumption)
            (mt:semi-compare-predicates predicate1 predicate2 :type type))))
     (or (eq predicate1 predicate2)
         (when (and (stringp predicate1) (stringp predicate2))
           (string-equal predicate1 predicate2))
         (when (eq type :subsumption)
           (let ((type1 (vsym predicate1))
                 (type2 (vsym predicate2)))
             (or (eq type1 type2)
                 (when (and (is-valid-type type1) (is-valid-type type2))
                   (ignore-errors (mrs:equal-or-subtype type1 type2)))))))))

(defun ed-message-p (thing)
  (when *eds-message-relation*
    (typecase thing
      (ed
       (let ((predicate (ed-predicate thing)))
         (or (eq (ed-type thing) :message)
             (ed-compare-predicates
              predicate *eds-message-relation* :type :subsumption)
             (and (ed-raw thing) (ed-message-p (ed-raw thing)))
             (string= (subseq predicate (- (length predicate) 2)) "_m"))))
      (rel
       (let ((predicate (rel-pred thing)))
         (or (eq predicate *eds-message-relation*)
             (when (stringp predicate) (search "_m_rel" predicate))
             (ignore-errors
              (equal-or-subtype predicate *eds-message-relation*))))))))

(defun ed-untensed-p (properties)
  (if (ed-p properties)
    (ed-untensed-p (ed-properties properties))
    (loop
        for pair in properties
        for property = (and (extrapair-p pair) (extrapair-feature pair))
        for test = (and property (rest (assoc property *eds-untensed*)))
        when test return (eq (extrapair-value pair) test)
        ;;
        ;; also, consider variables untensed when there is no TENSE property
        ;;
        finally (return t))))

(defun ed-fragment-p (ed)
  (when *eds-fragment-relation*
    (or (eq (ed-type ed) :fragment)
        (ed-compare-predicates
         (ed-predicate ed) *eds-fragment-relation* :type :subsumption)
        (let ((pred (and (rel-p (ed-raw ed)) (rel-pred (ed-raw ed)))))
          (when pred
            (ignore-errors 
             (equal-or-subtype pred (vsym *eds-fragment-relation*))))))))

(defun ed-bleached-p (ed)
  (or 
   (and (null *eds-include-messages-p*) (eq (ed-type ed) :message))
   (and (null *eds-include-quantifiers-p*) (eq (ed-type ed) :quantifier))
   (when *eds-bleached-relations*
     (loop
         with predicate = (ed-predicate ed)
         with pred = (and (rel-p (ed-raw ed)) (rel-pred (ed-raw ed)))
         for relation in *eds-bleached-relations*
         thereis (or (ed-compare-predicates
                      predicate relation :type :subsumption)
                     (ignore-errors
                      (equal-or-subtype pred (vsym relation))))))))

(defun ed-non-representative-p (ed)
  (when *eds-non-representatives*
    (loop
        with predicate = (ed-predicate ed)
        with pred = (and (rel-p (ed-raw ed)) (rel-pred (ed-raw ed)))
        for relation in *eds-non-representatives*
        thereis (or (ed-compare-predicates
                     predicate relation :type :subsumption)
                    (ignore-errors 
                     (equal-or-subtype pred (vsym relation)))))))

(defun ed-vacuous-p (ed)
  (unless *eds-include-vacuous-relations-p*
    (unless (and *eds-include-quantifiers-p* (eq (ed-type ed) :quantifier))
      (or (null (ed-arguments ed))
          (and (null (rest (ed-arguments ed)))
               (eq (first (first (ed-arguments ed))) (vsym "CARG")))))))

(defun eds-suspicious-p (eds)
  (append (when (eds-cyclic-p eds) '(:cyclic))
          (when (eds-fragmented-p eds) '(:fragmented))))

(defun eds-cyclic-p (eds)
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

(defun eds-fragmented-p (eds)
  (let ((mark (gensym))
        (agenda (loop
                    with top = (eds-top eds)
                    for ed in (eds-relations eds)
                    when (string= (ed-id ed) top) collect ed)))
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

(defun eds-explode (eds &key (lnkp *eds-show-lnk-p*)
                             (propertiesp *eds-show-properties-p*)
                             (cargp t) collocationp
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
  ;; exclusively used in eds-explode(); make sure all EDs have a correct value.
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
                         
   
   (when propertiesp
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
    (let* ((ed (or ed (find
                       (eds-top eds) (eds-relations eds)
                       :key #'ed-id :test #'string=)))
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
            #-:null
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

(defun eds-to-mrs (eds &key semi (errorp t))
  (declare (special mt:*semis*))
  (unless (mt:semi-p semi) (setf semi (first mt:*semis*)))
  (unless (mt:semi-p semi)
    (mt:construct-semi)
    (unless (mt:semi-p (setf semi (first mt:*semis*)))
      (if errorp
        (error "eds-to-mrs(): unable to locate or construct a SEM-I.")
        (return-from eds-to-mrs))))
  (let ((mrs (make-psoa))
        (variables (make-hash-table :test #'eq))
        (relations (make-hash-table :test #'eq))
        (generator (create-variable-generator))
        (bv (vsym "BV"))
        (arg0 (vsym "ARG0")))
    (labels ((quantifierp (ed)
               (member (vsym "BV") (ed-arguments ed) :key #'first))
             (canonical-role (label)
               (cond
                ((eq label bv) arg0)
                (t label)))
             (variable (&optional (type "u") properties)
               (make-var :type type :id (funcall generator) :extra properties)))
      (setf (psoa-top-h mrs) (variable "h"))
      (loop
          for node in (eds-relations eds)
          for predicate = (ed-predicate node)
          for sps
          = (mt:semi-lookup :semi semi :predicate predicate :alias predicate)
          for lnk = (ed-lnk node)
          for properties = (ed-properties node)
          for arguments = (ed-arguments node)
          when sps do
            (let* ((synopses (mt::sps-synopses sps))
                   (synopsis (first synopses))
                   (pred (and synopses (mt::ep-pred synopsis)))
                   (variable
                    (or (gethash node variables)
                        (setf (gethash node variables)
                          (let* ((type (first properties))
                                 (type (unless (extrapair-p type) type)))
                            (variable
                             (or type "i")
                             (if type (rest properties) properties))))))
                   (roles
                    (unless (quantifierp node)
                      (list
                       (make-fvpair :feature (vsym "ARG0") :value variable))))
                   (relation
                    (make-rel
                     :handel (variable "h") :pred pred :lnk lnk :flist roles)))
              (when (ed-carg node)
                (push
                 (make-fvpair :feature (vsym "CARG") :value (ed-carg node))
                 (rel-flist relation)))
              (loop
                  for (dependency . value) in arguments
                  when (ed-p value) 
                  do
                    (let* ((variable
                            (or (gethash value variables)
                                (setf (gethash value variables)
                                  (let* ((properties (ed-properties value))
                                         (type (first properties))
                                         (type 
                                          (unless (extrapair-p type) type)))
                                    (variable
                                     (or type "i")
                                     (if type (rest properties) properties))))))
                           (role
                            (make-fvpair
                             :feature (canonical-role dependency)
                             :value (or variable value))))
                      (push role (rel-flist relation))))
              (setf (gethash node relations) relation)
              (setf (rel-flist relation)
                (sort
                 (rel-flist relation)
                 #'(lambda (foo bar)
                     (let ((foo (position foo *feat-priority-list*))
                           (bar (position bar *feat-priority-list*)))
                       (if foo
                         (if bar (< foo bar) t)
                         bar)))
                 :key #'fvpair-feature))
              (push relation (psoa-liszt mrs)))
          else do
            (if errorp
              (error "eds-to-mrs(): invalid predicate ‘~a’." predicate)
              (return-from eds-to-mrs)))
      (let* ((top (find 
                   (eds-top eds) (eds-relations eds)
                   :key #'ed-id :test #'string=))
             (index (or (gethash top variables) (variable "i"))))
        (when top
          (let* ((larg (rel-handel (gethash top relations)))
                 (qeq (make-hcons
                       :relation "QEQ" :scarg (psoa-top-h mrs) :outscpd larg)))
            (push qeq (psoa-h-cons mrs))))
        (setf (psoa-index mrs) index))
      mrs)))

(defun eds-read (file &key decoder)
  (cond
   (decoder
    (multiple-value-bind (stream foo pid)
        (run-process 
         decoder :wait nil 
         :input file :output :stream :error-output nil)
      (declare (ignore foo))
      (let ((eds (eds-read stream)))
        (close stream)
        #+:allegro
        (sys:os-wait nil pid)
        eds)))
   ((streamp file)
    (labels ((|{|-reader (stream char)
                 (declare (ignore char))
                 (read-delimited-list #\} stream t))
             (|[|-reader (stream char)
                 (declare (ignore char))
                 (read-delimited-list #\] stream t))
             (read-ed (stream)
               (let ((c (peek-char t stream nil nil)))
                 (unless (and c (char= c #\}))
                   (let ((ed (make-ed)))
                     (setf (ed-id ed) (read stream nil nil))
                     (setf (ed-predicate ed) (read stream nil nil))
                     (setf (ed-lnk ed) (read-lnk stream))
                     (let ((c (peek-char t stream nil nil)))
                       (when (and c (char= c #\())
                         (read-char stream nil nil)
                         (setf (ed-carg ed) (read stream nil nil))
                         (read-char stream nil nil)))
                     (let ((c (peek-char t stream nil nil)))
                       (when (and c (char= c #\{))
                         (setf (ed-properties ed) (read stream nil nil))))
                     (let ((c (peek-char t stream nil nil)))
                       (when (and c (char= c #\[))
                         (setf (ed-arguments ed) (read stream nil nil))))
                     (when (and (ed-id ed) (ed-predicate ed))
                       (setf (ed-id ed) (string (ed-id ed)))
                       (setf (ed-predicate ed) (string (ed-predicate ed)))
                       (let ((type (when (oddp (length (ed-properties ed)))
                                     (first (ed-properties ed)))))
                         (setf (ed-properties ed)
                           (loop
                               with properties
                               = (if type 
                                   (rest (ed-properties ed))
                                   (ed-properties ed))
                               while (rest properties)
                               collect
                                 (let* ((feature (pop properties))
                                        (value (pop properties))
                                        (value 
                                         (if (or (symbolp value)
                                                 (stringp value))
                                           value
                                           (format nil "~a" value))))
                                   (make-extrapair 
                                    :feature (vsym feature)
                                    :value (vsym value)))))
                         (when type (push type (ed-properties ed))))
                       (setf (ed-arguments ed)
                         (loop
                             with arguments = (ed-arguments ed)
                             while (rest arguments)
                             collect (cons 
                                      (vsym (pop arguments))
                                      (pop arguments))))
                       ed))))))
               
      (loop
          with *package* = (find-package *mrs-package*)
          with *readtable* = (copy-readtable)
          with eds = (make-eds)
          initially
            (setf (readtable-case *readtable*) :preserve)
            (set-macro-character #\{ #'|{|-reader nil)
            (set-macro-character #\} (get-macro-character #\) nil))
            (set-macro-character #\[ #'|[|-reader nil)
            (set-macro-character #\] (get-macro-character #\) nil))
            (set-syntax-from-char #\: #\space)
            (set-syntax-from-char #\< #\")
            (set-syntax-from-char #\> #\")
            (set-syntax-from-char #\, #\space)
            (unless (char=
                     #\{
                     (loop
                         for c = (read-char file nil nil)
                         while (and c (not (char= c #\{)))
                         finally (return c)))
              (error "eds-read(): missing or invalid preamble."))
            (unless (setf (eds-top eds) (read file nil nil))
              (error "eds-read(): missing or invalid top node."))
          for ed = (read-ed file)
          while ed 
          do (push ed (eds-relations eds))
          finally
            (setf (eds-top eds) (string (eds-top eds)))
            (setf (eds-relations eds) (nreverse (eds-relations eds)))
            (loop
                with nodes = (eds-relations eds)
                for node in nodes
                do
                  (setf (ed-arguments node)
                    (loop
                        for (role . id) in (ed-arguments node)
                        for value = (when id 
                                      (find 
                                       (string id) nodes 
                                       :key #'ed-id :test #'string=))
                        when value collect (cons role value))))
            (eds-inverse eds)
            (return eds))))
   ((and (stringp file)
         (let ((c (with-input-from-string (stream file)
                    (peek-char t  stream nil nil))))
           (and c (char= c #\{))))
    (with-input-from-string (stream file)
      (eds-read stream)))
   ((and (or (stringp file) (pathnamep file)) (probe-file file))
    (with-open-file (stream file :direction :input)
      (eds-read stream)))
   (t
    (error "eds-read(): invalid input source ‘~a’." file))))

(defun eds (edge)
  (with-output-to-string (stream)
    (let ((*package* (find-package *mrs-package*))
          (*eds-show-properties-p* t))
      (write (eds-convert-edge edge) :stream stream))))

#+:null
(labels ((iid (item)
           (tsdb:get-field :i-id item))
         (output (profile stream
                  &optional (format :amr) active)
           (loop
               with items 
               = (tsdb::analyze
                  profile :condition "readings > 0 && t-active > 0"
                  :thorough '(:mrs))
               for item in items
               for id = (tsdb:get-field :i-id item)
               for results = (tsdb:get-field :results item)
               for mrs = (tsdb:get-field :mrs (first results))
               when (and (mrs::psoa-p mrs)
                         (or (null active)
                             (member id active :key #'iid)))
               do
                 (if (eq format :ascii)
                   (let ((file
                          (format
                           nil "~~/lib/sdp/release/2015/eds/~a.eds" id)))
                     (with-open-file (stream file :direction :output
                                      :if-exists :supersede)
                       (mrs:eds-output-psoa
                        mrs :format format :stream stream
                        :lnkp t :propertiesp nil
                        :filter "^[^_].*_q$|^focus_d$|^parg_d$"
                        :id id :input (tsdb:get-field :i-input item))))
                   (mrs:eds-output-psoa
                    mrs :format format :stream stream
                    :lnkp t :propertiesp nil
                    :filter "^[^_].*_q$|^focus_d$|^parg_d$"
                    :id id :input (tsdb:get-field :i-input item))))
           (tsdb::purge-profile-cache profile)))
  (let* ((format :ascii)
         (dm (when (eq format :ascii)
               (tsdb::read-items-from-conll-file
                "~/lib/sdp/train/en.dm.sdp" :type :sdp+ :cycle t :rawp t))))
    (with-open-file (stream "/tmp/train.amr" :direction :output
                     :if-exists :supersede)
      (loop
          with dm
          = (tsdb::read-items-from-conll-file
             "~/lib/sdp/train/en.dm.sdp" :type :sdp+ :cycle t :rawp t)
          for segment
          in '("wsj00a" "wsj00b" "wsj00c" "wsj00d" 
               "wsj01a" "wsj01b" "wsj01c" "wsj01d"
               "wsj02a" "wsj02b" "wsj02c" "wsj02d" 
               "wsj03a" "wsj03b" "wsj03c" 
               "wsj04a" "wsj04b" "wsj04c" "wsj04d" "wsj04e" 
               "wsj05a" "wsj05b" "wsj05c" "wsj05d" "wsj05e" 
               "wsj06a" "wsj06b" "wsj06c" "wsj06d"
               "wsj07a" "wsj07b" "wsj07c" "wsj07d" "wsj07e"
               "wsj08a" 
               "wsj09a" "wsj09b" "wsj09c" "wsj09d"
               "wsj10a" "wsj10b" "wsj10c" "wsj10d"
               "wsj11a" "wsj11b" "wsj11c" "wsj11d" "wsj11e"
               "wsj12a" "wsj12b" "wsj12c" "wsj12d"
               "wsj13a" "wsj13b" "wsj13c" "wsj13d" "wsj13e"
               "wsj14a" "wsj14b" "wsj14c" "wsj14d" "wsj14e"
               "wsj15a" "wsj15b" "wsj15c" "wsj15d" "wsj15e"
               "wsj16a" "wsj16b" "wsj16c" "wsj16d" "wsj16e" "wsj16f"
               "wsj17a" "wsj17b" "wsj17c" "wsj17d"
               "wsj18a" "wsj18b" "wsj18c" "wsj18d" "wsj18e"
               "wsj19a" "wsj19b" "wsj19c" "wsj19d"
               "wsj20a" "wsj20b" "wsj20c" "wsj20d")
          do
            (output (format nil "gold/erg/~a" segment) stream format dm)))
    (with-open-file (stream "/tmp/test.amr" :direction :output
                     :if-exists :supersede)
      (loop
          for segment in '("wsj21a" "wsj21b" "wsj21c" "wsj21d")
          do
            (output (format nil "gold/erg/~a" segment) stream format dm)))))
#+:null
(let ((train (tsdb::read-items-from-conll-file
              "~/lib/sdp/train/en.dm.sdp" :type :sdp+ :cycle t :rawp t))
      (test (tsdb::read-items-from-conll-file
             "~/lib/sdp/test/en.id.dm.sdp" :type :sdp+ :cycle t :rawp t))
      (path "~/lib/sdp/release/2015/eds/")
      (decoder "gzip -d -c"))
  (labels ((output (items stream format)
             (loop
                 for item in items
                 for id = (tsdb:get-field :i-id item)
                 for input 
                 = (let ((file (format nil "~a~a.txt.gz" path id)))
                     (multiple-value-bind (stream foo pid)
                         (run-process 
                          decoder :wait nil
                          :input file :output :stream :error-output nil)
                       (declare (ignore foo))
                       (let ((line (read-line stream nil nil)))
                         (close stream)
                         #+:allegro
                         (sys:os-wait nil pid)
                         line)))
                 for mrs 
                 = (let ((file (format nil "~a~a.mrs.gz" path id)))
                     (read-mrs-from-file file :decoder decoder))
                 when stream do 
                   (eds-output-psoa
                    mrs :format format :stream stream
                    :lnkp t :propertiesp (eq format :json)
                    :filter "^[^_].*_q$|^focus_d$|^parg_d$"
                    :id id :input input :columns 79)
                   (when (eq format :json) (terpri stream))
                 else do
                   (with-open-file (stream (format nil "~a~a.eds" path id)
                                    :direction :output :if-exists :supersede)
                     (eds-output-psoa
                      mrs :format format :stream stream
                      :lnkp t :propertiesp t
                      :filter "^[^_].*_q$|^focus_d$|^parg_d$"
                      :id id :input input)))))
    (output train nil :ascii)
    (output test nil :ascii)
    (with-open-file (stream "~/lib/sdp/release/2015/eds/train.amr"
                     :direction :output :if-exists :supersede)
      (output train stream :amr))
    (with-open-file (stream "~/lib/sdp/release/2015/eds/test.amr"
                     :direction :output :if-exists :supersede)
      (output test stream :amr))
    (with-open-file (stream "~/lib/sdp/release/2015/eds/train.json"
                     :direction :output :if-exists :supersede)
      (output train stream :json))
    (with-open-file (stream "~/lib/sdp/release/2015/eds/test.json"
                     :direction :output :if-exists :supersede)
      (output test stream :json))))
