(in-package :mt)

;;;
;;; ToDO
;;;
;;; + detect `null' MRS resulting from making the input TDFS well-formed;
;;; - work out what exactly to do about ambiguity: do we risk spurious parallel
;;;   analyses in cases where the same rule can be unified onto an input MRS in
;;;   more than one way; what about analyses that differ only in the order of
;;;   rule applications (if that was allowed) ... some kind of normal form?
;;; + add `Scope' and `Generate' buttons to transfer MRS browser.
;;; - provide `negation' switch on result filter.
;;; - investiage `filter' processing further: is it actually correct?
;;; - think of a nice, declarative way to delete properties from indices.
;;;

(defparameter *transfer-edge-limit* 20000)

(defparameter *transfer-debug-stream* 
  (or #+:allegro excl:*initial-terminal-io* t))

(defparameter *transfer-postprocess-p* t)

(defparameter %transfer-generation% 0)

(defparameter %transfer-edge-id% 100)

(defparameter %transfer-rule-id% 0)

(defparameter %transfer-optional-rules% 0)

(defparameter %transfer-variable-features% nil)

(defparameter %transfer-variable-id% 100)

(defparameter %transfer-raw-output-p% nil)

(defparameter %transfer-recursive-output-p% t)

(defparameter %transfer-edges% nil)

;;;
;;; _fix_me_
;;; the following are stop-gap solutions and, presumably, should be replaced
;;; with a more general solution, possibly a separate set of transfer rules
;;; manipulating variable properties destructively and taking advantage of
;;; match operators like :null and :exact to test for specific values.
;;;                                                           (9-jan-03; oe)
(defparameter %transfer-properties-filter%
  (list
   (cons (mrs::vsym "TENSE") (mrs::vsym "E.TENSE"))
   #+:null
   (cons (mrs::vsym "PROG") (mrs::vsym "E.ASPECT.PROGR"))
   #+:null
   (cons (mrs::vsym "PERF") (mrs::vsym "E.ASPECT.PERF"))
   (cons (mrs::vsym "NATGEND") (mrs::vsym "PNG.GEN"))
   (cons (mrs::vsym "ASPECT-PROTRACTED") nil)
   (cons (mrs::vsym "ASPECT-STATIVE") nil)
   (cons (mrs::vsym "ASPECT-TELIC") nil)
   (cons (mrs::vsym "ASPECT-BOUNDED") nil)
   (cons (mrs::vsym "ASPECT-INCHOATIVE") nil)))

(defparameter %transfer-properties-accumulator%
  (list
   (cons 
    (mrs::vsym "x")
    (list
     (list (mrs::vsym "PERS") (mrs::vsym "NUM") (mrs::vsym "PNG.PN"))
     (list (mrs::vsym "1") (mrs::vsym "sg") (mrs::vsym "1sg"))
     (list (mrs::vsym "1") (mrs::vsym "pl") (mrs::vsym "1pl"))
     (list (mrs::vsym "1") nil (mrs::vsym "1per"))
     (list (mrs::vsym "2") (mrs::vsym "sg") (mrs::vsym "2sg"))
     (list (mrs::vsym "2") (mrs::vsym "pl") (mrs::vsym "2pl"))
     (list (mrs::vsym "2") nil (mrs::vsym "2per"))
     (list (mrs::vsym "3") (mrs::vsym "sg") (mrs::vsym "3sg"))
     (list (mrs::vsym "3") (mrs::vsym "pl") (mrs::vsym "3pl"))
     (list (mrs::vsym "3") nil (mrs::vsym "3per"))
     (list nil (mrs::vsym "sg") nil (mrs::vsym "pernum"))
     (list nil (mrs::vsym "pl") nil (mrs::vsym "pernum"))))
   (cons
    (mrs::vsym "e")
    (list
     (list (mrs::vsym "PROG") (mrs::vsym "E.ASPECT.PROGR"))
     (list (mrs::vsym "+") (mrs::vsym "+"))
     (list (mrs::vsym "-") (mrs::vsym "-"))
     (list nil (mrs::vsym "-"))))
   (cons
    (mrs::vsym "e")
    (list
     (list (mrs::vsym "PERF") (mrs::vsym "E.ASPECT.PERF"))
     (list (mrs::vsym "+") (mrs::vsym "+"))
     (list (mrs::vsym "-") (mrs::vsym "-"))
     (list nil (mrs::vsym "-"))))))

(defparameter %transfer-properties-defaults% 
  (list
   (list (mrs::vsym "e") 
         (cons (mrs::vsym "E.MOOD") (mrs::vsym "indicative")))))

(defparameter %transfer-values-filter%
  (list
   (cons (mrs::vsym "pres") (mrs::vsym "present"))
   (cons (mrs::vsym "m") (mrs::vsym "masc"))
   (cons (mrs::vsym "f") (mrs::vsym "fem"))
   (cons (mrs::vsym "n") (mrs::vsym "neut"))))

(defstruct mtrs
  id mtrs flags)

(defstruct mtr
  id
  context filter
  input output defaults
  variables vector flags)

(defmethod print-object ((object mtr) stream)
  (if %transfer-raw-output-p%
    (call-next-method)
    (format
     stream
     "~@[!~a ~]~@[@~a ~]~@[~a ~]-> ~a~@[ / ~a~]"
     (mtr-filter object) (mtr-context object)
     (mtr-input object) (mtr-output object) (mtr-defaults object))))

(defstruct (edge (:constructor make-edge-x))
  (id (let ((n %transfer-edge-id%)) (incf %transfer-edge-id%) n))
  rule mrs daughter solution n (vector 0)
  (source 0))

(defun make-edge (&rest rest)
  (let* ((edge (apply #'make-edge-x rest))
         (daughter (edge-daughter edge)))
    (setf (edge-vector edge)
      (cond
       ((and (edge-p daughter) (mtr-p (edge-rule edge)))
        (logior (edge-vector daughter) (mtr-vector (edge-rule edge))))
       ((edge-p daughter) (edge-vector daughter))
       ((mtr-p (edge-rule edge)) (mtr-vector (edge-rule edge)))
       (t (edge-vector edge))))
    (push edge %transfer-edges%)
    (cond
     ((> (edge-id edge) *transfer-edge-limit*)
      (print-edges)
      (error 
       "make-edge(): transfer edge limit exhausted (~a)."
       *transfer-edge-limit*))
     (t
      edge))))
    
(defmethod print-object ((object edge) stream)
  (if %transfer-raw-output-p%
    (call-next-method)
    (let ((recursep (or (not (numberp %transfer-recursive-output-p%))
                        (> %transfer-recursive-output-p% 0)))
          (daughterp (and (mtr-p (edge-rule object))
                          (edge-p (edge-daughter object)))))
      (when (numberp %transfer-recursive-output-p%)
        (decf %transfer-recursive-output-p%))
      (if daughterp
        (format
         stream
         "~<#D[~;~a~@[ ~(~a~)~]~:[~* ...~; ~_~a~]~;]~:>"
         (list
          (edge-id object) 
          (mtr-id (edge-rule object))
          recursep
          (edge-daughter object)))
        (format stream "~<#D[~;~a]~:>" (list (edge-id object)))))))

(defun print-edges ()
  (loop
      for edge in %transfer-edges%
      for %transfer-recursive-output-p% = 10
      do (format *transfer-debug-stream* "~a~%" edge)))

(defstruct (solution (:copier x-copy-solution))
  variables eps hconss)

(defun copy-solution (solution)
  (when solution
    (let ((result (x-copy-solution solution)))
      (setf (solution-variables result)
        (copy-list (solution-variables solution)))
      (setf (solution-eps result)
        (copy-list (solution-eps solution)))
      result)))

(defmacro scratch (scratch)
  `(let ((generation (first ,scratch)))
     (when (and (integerp generation) (= generation %transfer-generation%))
       (rest ,scratch))))

(defsetf scratch (scratch) (value)
  `(let ((generation (first ,scratch)))
     (unless (and (integerp generation) (= generation %transfer-generation%))
       (setf (first ,scratch) %transfer-generation%))
     (setf (rest ,scratch) ,value)))

(defun forward-variable (old new solution)
  ;;
  ;; _fix_me_
  ;; most likely, it will never happen that we attempt to forward a variable
  ;; that is itself forwarded, since all code below should have derefenced it,
  ;; first thing in the morning.  confirm!                     (22-jan-04; oe)
  ;;
  (let ((foo (getf (solution-variables solution) old)))
    (if foo
      (forward-variable foo new solution)
      (setf (getf (solution-variables solution) old) new))))

(defun retrieve-variable (variable solution)
  ;;
  ;; _fix_me_
  ;; transfer/24 appears to be caused by variable lookup failure, even though
  ;; h8 == h53 (for item # 2 from the `mrs' test suite) appears to be in the
  ;; set of variable equations, but somehow they are not eq()!  for now, try a
  ;; more robust comparison, not assuming that variables be eq() --- it is not
  ;; that much more expensive, after all.                       (9-jan-04; oe)
  ;;
  ;; we believe that moving postprocess-mrs() (which produces copies) out until
  ;; after all unifcation and `expansion' is complete probably has resolved our
  ;; non-eq()ness issues.                                      (19-jan-04; oe)
  ;;
  (when (mrs::var-p variable)
    (let ((new (getf (solution-variables solution) variable)))
      (if new (retrieve-variable new solution) variable))))

(defun align-eps (old new solution)
  (push (cons old new) (solution-eps solution)))

(defun retrieve-ep (ep solution)
  (loop
      for (old . new) in (solution-eps solution)
      when (eq ep old) return new
      when (eq ep new) return old))

(defun align-hconss (old new solution)
  (push (cons old new) (solution-hconss solution)))

(defun retrieve-hcons (hcons solution)
  (loop
      for (old . new) in (solution-hconss solution)
      when (eq hcons old) return new
      when (eq hcons new) return old))

;;;
;;; construction of transfer rules from MTR descriptions in TDL
;;;
(defun read-transfer-types (files &optional settings)
  (let ((*readtable* (copy-readtable)))
    (lkb::read-tdl-type-files-aux files settings)))

(defun read-transfer-rules (files &optional name &key flags)

  ;;
  ;; _fix_me_
  ;; make top variable type (`u') customizable, one day.       (22-jan-04; oe)
  ;;
  (when (lkb::is-valid-type (mrs::vsym "u"))
    (loop
        for type in (lkb::retrieve-descendants (mrs::vsym "u"))
        for name = (lkb::type-name type)
        do
          (loop
              for feature in (lkb::appropriate-features-of name)
              do (pushnew feature %transfer-variable-features%))))

  (let* ((files (if (listp files) files (list files)))
         (id (if (stringp name) 
               name 
               (let ((file (pathname (first files))))
                 (format 
                  nil 
                  "~a~@[.~a~]"
                  (pathname-name file) (pathname-type file))))))

    (loop
        with rules = nil
        for foo in files
        for file = (pathname foo)
        unless (probe-file file) do
          (format
           t
           "read-transfer-rules(): ignoring invalid file `~a~@[.~a~]'.~%"
           (pathname-name file) (pathname-type file))
        else do
          (format 
           t 
           "read-transfer-rules(): reading file `~a~@[.~a~]'.~%"
           (pathname-name file) (pathname-type file))
          (with-open-file (stream file :direction :input)
            (loop
                with *readtable* = (lkb::make-tdl-break-table)
                for c = (peek-char t stream nil nil)
                while c do
                  (cond
                   ((char= c #\;) (read-line stream))
                   ;;
                   ;; _fix_me_
                   ;; this should do full detection of `#|' comments instead.
                   ;;                                         (8-oct-03; oe)
                   ((char= c #\#) (lkb::read-tdl-comment stream))
                   (t 
                    (let* ((id (read stream nil nil))
                           (c (peek-char t stream nil nil))
                           (c (when (and c (char= c #\:))
                                (read-char stream)
                                (peek-char t stream nil nil))))
                      (unless (or (null c) (char= c #\=))
                        (error 
                         "read-transfer-rules(): ~
                          syntax error following rule id `~(~a~)'."
                         id))
                      (read-char stream)
                      (let ((unifications 
                             (lkb::read-expanded-avm-def stream id))
                            lhs rhs)
                        (when (lkb::check-for #\. stream id)
                          (multiple-value-setq (lhs rhs)
                            (discriminate-mtr-unifications unifications))
                          (setf lhs (lkb::process-unifications lhs))
                          (unless lhs
                            (error 
                             "read-transfer-rules(): ~
                              `~(~a~)' is incoherent.~%"
                             id))
                          (setf lhs (lkb::create-wffs lhs))
                          (unless lhs
                            (error 
                             "read-transfer-rules(): ~
                              `~(~a~)' is not wellformed.~%"
                             id))
                          (when rhs
                            (setf rhs (lkb::process-unifications rhs))
                            (unless rhs
                              (error 
                               "read-transfer-rules(): ~
                              `~(~a~)' is incoherent.~%"
                               id))
                            (setf rhs (lkb::create-wffs rhs))
                            (unless rhs
                              (error 
                               "read-transfer-rules(): ~
                              `~(~a~)' is not wellformed.~%"
                               id)))
                          (let ((rule (convert-dag-to-mtr lhs rhs id)))
                            ;;
                            ;; _fix_me_
                            ;; maybe inherit flags from MTRS defaults and do a
                            ;; normalization of :obligatory ([ OPTIONAL - ]).
                            ;;                                 (25-jan-04; oe)
                            (when rule (push rule rules))))))))))
        finally 
          (setf *transfer-rule-sets*
            (append 
             *transfer-rule-sets*
             (list (make-mtrs :id id :mtrs (nreverse rules) :flags flags)))))))

(defun discriminate-mtr-unifications (unifications)
  (loop
      with lhss with rhss
      with n = (length *mtr-output-path*)
      for unification in unifications
      for lhs = (lkb::unification-lhs unification)
      for path = (and (lkb::path-p lhs) (lkb::path-typed-feature-list lhs))
      for outputp = (null (mismatch *mtr-output-path* path :end2 n))
      for rhs = (lkb::unification-rhs unification)
      when (and outputp (lkb::u-value-p rhs))
      do 
        #+:debug
        (format t "rhs: ~a~%" unification)
        (push unification rhss)
        (push 
         (lkb::make-unification :lhs lhs :rhs lhs)
         lhss)
      else do 
        #+:debug
        (format t "lhs: ~a~%" unification)
        (push unification lhss)
        (let ((rhsp (and (lkb::path-p rhs)
                         (null
                          (mismatch
                           *mtr-output-path*
                           (lkb::path-typed-feature-list rhs)
                           :end2 n)))))
          (cond
           ((and outputp rhsp)
            (push unification rhss))
           (rhsp
            (push (lkb::make-unification :lhs rhs :rhs rhs) rhss))
           (outputp 
            (push (lkb::make-unification :lhs lhs :rhs lhs) rhss))))
      finally 
        #+:debug
        (setf %lhss% lhss %rhss% rhss)
        (return (values lhss rhss))))

(defun convert-dag-to-mtr (lhs rhs id)
  ;;
  ;; _fix_me_
  ;; give some thought to variable types: authors of transfer rules might well
  ;; expect that variable names contribute type information in the MRS spirit.
  ;; we would have to go through .dag. at some point (soon enough to still have
  ;; access to the original tag names) and add :type information to each node.
  ;;
  ;; _fix_me_
  ;; i finally worked out, why ann introduced the `^c42' notation: for PRED and
  ;; roles in *value-feats*, construct-mrs() will always create a constant,
  ;; even where variables were called for (by means of coreferences, say).  we
  ;; will need to specialize construct-mrs() relatively soon, it seems, also to
  ;; treat special operators like :exact.                      (24-jan-04; oe)
  ;; 
  #+:debug
  (setf %lhs% lhs %rhs% rhs)
  (let* ((generator (let ((n 0)) #'(lambda () (decf n))))
         (mrs::*named-nodes* nil)
         (filter (mrs::path-value lhs *mtr-filter-path*))
         (filter (and filter 
                      (not (vacuous-constraint-p *mtr-filter-path* filter))
                      (mrs::construct-mrs filter generator)))
         (context (mrs::path-value lhs *mtr-context-path*))
         (context (and context 
                       (not (vacuous-constraint-p *mtr-context-path* context))
                       (mrs::construct-mrs context generator)))
         (input (mrs::path-value lhs *mtr-input-path*))
         (input (and input
                     (not (vacuous-constraint-p *mtr-input-path* input))
                     (mrs::construct-mrs input generator)))
         (output (mrs::path-value lhs *mtr-output-path*))
         (output (and output
                      (not (vacuous-constraint-p *mtr-output-path* output))
                      (mrs::construct-mrs output generator)))
         (defaults (and rhs (mrs::path-value rhs *mtr-output-path*)))
         (defaults (and defaults
                        (not (vacuous-constraint-p *mtr-output-path* defaults))
                        (mrs::construct-mrs defaults generator)))
         (flags (and lhs (mrs::path-value lhs *mtr-flags-path*)))
         (flags (and flags
                     (not (vacuous-constraint-p *mtr-flags-path* flags))
                     (convert-dag-to-flags flags))))
         
    (unless (or output rhs)
      ;;
      ;; warn: rule with no output specification
      ;;
      (format
       *transfer-debug-stream*
       "convert-dag-to-mtr(): `~(~a~)' has an empty output specification.~%"
       id))
    ;;
    ;; _fix_me_
    ;; since our current treatment of FILTER components is wrong anyway, give a
    ;; warning and ignore all rules that try to use filters.     (8-jan-04; oe)
    ;; 
    (if filter
      (format
       *transfer-debug-stream*
       "convert-dag-to-mtr(): ~
        ignoring `~(~a~)' because it contains a FILTER.~%"
       id)
      (let* ((vector (ash 1 %transfer-rule-id%))
             (mtr (make-mtr :id id :filter filter :context context
                            :input input :output output :defaults defaults
                            :variables (nreverse mrs::*named-nodes*)
                            :flags flags :vector vector)))
        (incf %transfer-rule-id%)
        (when (member :optional (mtr-flags mtr))
          (setf vector (logior vector %transfer-optional-rules%)))
        mtr))))

(defun convert-dag-to-flags (dag)
  (let* ((optional (mrs::path-value dag *mtr-optional-path*))
         flags)
    (when (lkb::bool-value-true optional)
      (pushnew :optional flags))
    (when (lkb::bool-value-false optional)
      (pushnew :obligatory flags))
    flags))

(defun vacuous-constraint-p (path dag)
  (let* ((feature (if (consp path) (first (last path)) path))
         (dag (if (lkb::dag-p dag) dag (lkb::create-typed-dag dag)))
         (type (lkb::minimal-type-for feature))
         (constraint (and type (lkb::constraint-of type))))
    (lkb::dag-subsumes-p dag constraint)))

;;;
;;; top-level drivers for application of transfer rules; use `edge' structure
;;; to keep track of something reminiscent of a derivation tree.
;;;
(defun initialize-transfer ()
  (setf *transfer-rule-sets* nil)
  (setf %transfer-rule-id% 0)
  (setf %transfer-generation% 0)
  (setf %transfer-variable-features% nil))

(defun transfer-mrs (mrs &key (filterp t) 
                              (postprocessp *transfer-postprocess-p*))
  #+:debug
  (setf %mrs% mrs)
  (setf %transfer-edges% nil)
  (setf %transfer-edge-id% 0)
  (multiple-value-bind (result condition)
      (#-:debug ignore-errors #+:debug progn
       (transfer-mrs2 
        (list (make-edge :mrs mrs :n 1000)) 
        *transfer-rule-sets*
        :filterp filterp))
    (when condition
      #+:clim
      (clim:beep)
      (format
       *transfer-debug-stream*
       "transfer-mrs(): `~a'~%" condition))
    (if postprocessp
      (loop
          for edge in result
          for clone = (copy-edge edge)
          do (setf (edge-mrs clone) (postprocess-mrs (edge-mrs edge)))
          collect clone)
      result)))

(defun transfer-mrs2 (edges mtrss &key (filterp nil))
  (if (null mtrss)
    edges
    (transfer-mrs2 
     (loop
         for edge in edges
         append (apply-mtrs edge (first mtrss) :filterp filterp))
     (rest mtrss)
     :filterp filterp)))

(defun adjoin-edge (new edges)
  (labels ((subsumesp (edge1 edge2)
             ;;
             ;; true if .edge1. (by MTR vector) subsumes .edge2.
             ;;
             (let* ((mask (lognot %transfer-optional-rules%))
                    (vector1 (logand (edge-vector edge1) mask))
                    (vector2 (logand (edge-vector edge2) mask)))
               (= (logior vector2 vector1) vector2))))
    (loop
        with result
        for old in edges
        when (subsumesp old new) return edges
        unless (subsumesp new old) do (push old result)
        finally (return (cons new result)))))

(defun apply-mtrs (edge mtrs &key (filterp t))

  (when (or (null edge) (null mtrs)) (return-from apply-mtrs))
  
  (loop
      with result
      with agenda = (list edge)
      with all = (mtrs-mtrs mtrs)
      for task = (pop agenda)
      for rule = (and task (edge-rule task))
      for mtrs = (if (or (eq task edge) (null rule))
                   ;;
                   ;; when we advance from one MTRS to another, the top .edge.
                   ;; may have a non-empty rule, but taken from the other set.
                   ;;
                   all
                   (member rule all))
      while task do
        #+:debug
        (format *transfer-debug-stream* "apply-mtrs(): << ~a~%" task)
        ;;
        ;; each rule in the current set can be obligatory (the new default) or
        ;; optional; if one of the non-optional rules fires, then the current
        ;; derivation (i.e. .task.) is consumed and does not become a result.
        ;;
        (loop
            with resultp = t
            for mtr in mtrs
            for optionalp = (member :optional (mtr-flags mtr))
            for edges = (apply-mtr task mtr)
            while resultp
            when (and edges (not optionalp)) do (setf resultp nil)
            do (loop 
                   for edge in edges 
                   do 
                     #+:debug
                     (format
                      *transfer-debug-stream*
                      "apply-mtrs(): >> ~a~%"
                      edge)
                     (push edge agenda))
            finally 
              (when resultp 
                #+:debug
                (format
                 *transfer-debug-stream*
                 "apply-mtrs(): == ~a~%"
                 task)
                (push task result)))
      finally 
        ;;
        ;; sort resulting derivations by `edit distance' to the original MRS,
        ;; measured in terms of the size of overlap of predicates names; also
        ;; see comment on filtering below.
        ;;
        (let ((eps (mrs:psoa-liszt (edge-mrs edge))))
          (loop
              for edge in result
              do (setf (edge-source edge)
                   (length 
                    (intersection 
                     (mrs:psoa-liszt (edge-mrs edge)) eps 
                     :key #'mrs::rel-pred :test #'equal))))
          (setf result (stable-sort result #'< :key #'edge-source)))
        (if filterp
          ;;
          ;; when filtering is requested, suppress all derivations that have a
          ;; non-emtpy overlap (in terms of predicate names) with the input MRS
          ;; ... for now, this assumes that the SL and TL predicate sets are
          ;; fully disjoint, but mid-term we should use `color coding' instead.
          ;;
          (return
            (loop
                with eps = (mrs:psoa-liszt (edge-mrs edge))
                for edge in result
                for mrs = (edge-mrs edge)
                when (null (intersection 
                            (mrs:psoa-liszt mrs) eps 
                            :key #'mrs::rel-pred :test #'equal))
                collect edge))
          (return result))))

(defun apply-mtr (edge mtr)
  (loop
      with %transfer-variable-id% = (edge-n edge)
      for solution in (unify-mtr (edge-mrs edge) mtr)
      for mrs = (expand-solution (edge-mrs edge) mtr solution)
      collect (make-edge 
               :rule mtr :mrs mrs :daughter edge 
               :solution solution :n %transfer-variable-id%)))

(defparameter %transfer-trace-p% nil)

(defparameter %transfer-trace-context% nil)

(defparameter %transfer-trace-failure% nil)

(defmacro transfer-trace (type value)
  `(when %transfer-trace-p% 
     (setf (getf %transfer-trace-context% ,type) ,value)))

;;;
;;; for debugging purposes, record failure condition during unification; there
;;; could be a number of distinct failure types:
;;;
;;; - :top or :index, variable type or extra;
;;; - unifying two EPs, 
;;;   PRED or role, variable vs. constant; variable type or extra; and
;;; - HCONS, it seems impossible to fail on HCONS, right now.
;;;
;;; _fix_me_
;;;; complete tracing { and | or } failure accumulation.        (22-jan-04; oe)

;;;
;;; to simplify the treatment of constants, where the rule may use a variable
;;; to bind a constant, all of the following assume that the second parameter
;;; to each unification corresponds to the rule, i.e. can be a variable even if
;;; the first is constant.
;;;
;;; _fix_me_
;;; not sure that this is still relevant, at least not in interesting ways, as
;;; we are reasonably close to a notion of unification now     (24-oct-03; oe)
;;;
(defun unify-mtr (mrs mtr)
  #+:debug
  (format
   *transfer-debug-stream*
   "unify-mtr(): `~(~a~)' @ ~a~%" (mtr-id mtr) mrs)
  (transfer-trace :mtr mtr)
  (incf %transfer-generation%)
  (let* ((filter (mtr-filter mtr))
         (context (mtr-context mtr))
         (input (mtr-input mtr))
         solutions)
    (transfer-trace :component :filter)
    ;;
    ;; _fix_me_
    ;; move filter test towards the end.                       (22-jan-04; oe)
    ;;
    (when (and filter (unify-mtr-component mrs filter))
      ;;
      ;; trace
      ;;
      (return-from unify-mtr))
    (transfer-trace :component :context)
    (setf solutions (unify-mtr-component mrs context))
    (unless solutions
      ;;
      ;; trace
      ;;
      (return-from unify-mtr))
    #+:debug
    (format 
     t 
     "unify-mtr(): ~a solution~p for CONTEXT component.~%"
     (length solutions) (length solutions))
    (when input
      (transfer-trace :component :input)
      (setf solutions
        (loop
            for solution in solutions
            append (unify-mtr-component mrs input solution)))
      #+:debug
      (format 
       t 
       "unify-mtr(): ~a solution~p for INPUT component.~%"
       (length solutions) (length solutions))
      (unless solutions
        ;;
        ;; trace
        ;;
        (return-from unify-mtr)))
    (when context
      (setf solutions
        (loop
            with hcons1 = (mrs:psoa-h-cons mrs)
            with hcons2 = (mrs:psoa-h-cons context)
            for solution in solutions
            nconc (unify-hconss hcons1 hcons2 solution)))
      #+:debug
      (format 
       t 
       "unify-mtr(): ~a solution~p for CONTEXT component HCONS.~%"
       (length solutions) (length solutions))
      (unless solutions
        ;;
        ;; trace
        ;;
        (return-from unify-mtr)))
    (when input
      (setf solutions
        (loop
            with hcons1 = (mrs:psoa-h-cons mrs)
            with hcons2 = (mrs:psoa-h-cons input)
            for solution in solutions
            nconc (unify-hconss hcons1 hcons2 solution)))
      #+:debug
      (format 
       t 
       "unify-mtr(): ~a solution~p for INPUT component HCONS.~%"
       (length solutions) (length solutions))
      (unless solutions
        ;;
        ;; trace
        ;;
        (return-from unify-mtr)))
    solutions))

(defun unify-mtr-component (mrs1 mrs2 &optional solution)
  (if (null mrs2)
    (list solution)
    (let* ((solution (if solution (copy-solution solution) (make-solution)))
           (top1 (mrs:psoa-top-h mrs1))
           (top2 (mrs:psoa-top-h mrs2)))
      (transfer-trace :part :top)
      (unless (unify-values top1 top2 solution)
        ;;
        ;; trace
        ;;
        (return-from unify-mtr-component))
      (let* ((index1 (mrs:psoa-index mrs1))
             (index2 (mrs:psoa-index mrs2)))
        (transfer-trace :part :index)
        (unless (unify-values index1 index2 solution)
          ;;
          ;; trace
          ;;
          (return-from unify-mtr-component)))
      (let* ((eps1 (mrs:psoa-liszt mrs1))
             (eps2 (mrs:psoa-liszt mrs2))
             (solutions (unify-epss eps1 eps2 solution)))
        (unless solutions
          ;;
          ;; trace
          ;;
          (return-from unify-mtr-component))
        ;;
        ;; re-order computation for better efficiency (and while there is no
        ;; good way of rejecting false results based on a cycle check).
        ;;
        solutions
        #+:null
        (let* ((hcons1 (mrs:psoa-h-cons mrs1))
               (hcons2 (mrs:psoa-h-cons mrs2))
               (solutions
                (loop
                    for solution in solutions
                    append (unify-hconss hcons1 hcons2 solution))))
          (unless solutions
            ;;
            ;; trace
            ;;
            (return-from unify-mtr-component))
          solutions)))))

(defun unify-epss (eps1 eps2 solution)
  ;;
  ;; we must not destructively modify .solution. --- assume that unify-eps()
  ;; will always copy its input parameter first.  require that all elements of
  ;; .eps2. get bound successfully, i.e. only return solutions that account for
  ;; all of the CONTEXT or INPUT elements from an MTR.
  ;;
  (when eps1
    (if (null eps2)
      (list solution)
      (let* ((ep2 (first eps2))
             (solutions
              (loop
                  for ep1 in eps1
                  for result = (unify-eps ep1 ep2 solution)
                  when result collect result)))
        (when solutions
          (loop
              for solution in solutions
              nconc (unify-epss eps1 (rest eps2) solution)))))))

;;;
;;; in unifying two sets of EPs, i believe we are assuming that no EP can be
;;; unified with more than one counterpart (which intuitively seems to make a
;;; lot of sense); it means there cannot be `overlap' between the CONTEXT and
;;; INPUT parts of a rule, which is fine since bindings are accumulated from
;;; both, so there should be no need to have a duplicate EP (targetting just
;;; one EP in the input MRS) spread out over those two.       (23-oct-03; oe)
;;;
(defun unify-eps (ep1 ep2 solution)
  (transfer-trace :part (cons ep1 ep2))
  (unless (or (retrieve-ep ep1 solution) (retrieve-ep ep2 solution))
    (let* ((solution (copy-solution solution))
           (pred (unify-preds 
                  (mrs::rel-pred ep1) (mrs::rel-pred ep2) solution))
           (label (when pred
                    (unify-values 
                     (mrs:rel-handel ep1) (mrs:rel-handel ep2) solution))))
      (when label
        (loop
            with flist1 = (mrs:rel-flist ep1)
            with flist2 = (mrs:rel-flist ep2)
            for role in (intersect flist1 flist2 :key #'mrs:fvpair-feature)
            for feature = (mrs:fvpair-feature role)
            for role1 = (find feature flist1 :key #'mrs:fvpair-feature)
            for role2 = (find feature flist2 :key #'mrs:fvpair-feature)
            unless (unify-values 
                    (mrs:fvpair-value role1) (mrs:fvpair-value role2) solution)
            do (return-from unify-eps))
        
        (align-eps ep1 ep2 solution)
        solution))))

(defun unify-hconss (hconss1 hconss2 solution)
  ;;
  ;; we must not destructively modify .solution. --- assume that unify-hcons()
  ;; will always copy its input parameter first.
  ;;
  ;; _fix_me_
  ;; what exactly does it mean to unify on handle constraints?  say we computed
  ;; all possible alignments of .hcons1. and .hcons2., that would potentially
  ;; blow up the number of solutions if the CONTEXT or FILTER parts of an MTR
  ;; contain handle constraints that are unconnected to the rest of the MTR, 
  ;; i.e. do not share any of its variables.  assuming that we rejected that in
  ;; constructing the transfer rules, then we may be left with an interesting
  ;; ordering problem: doing regular unification on a CONTEXT part when some
  ;; of the variable bindings are only established in the INPUT part, may still
  ;; give rise to large combinatorics of solutions.  ... this obviously needs
  ;; a little more thinking.                                  (24-oct-03; oe)
  ;;
  ;; _fix_me_
  ;; consider moving HCONS unification out of unify-mtr-component() and defer
  ;; it until after the CONTEXT and INPUT components have been unified; that
  ;; should potentially reduce combinatorics quite a bit.     (21-jan-04; oe)
  ;;
  (if hconss1
    (if (null hconss2)
      (list solution)
      (let* ((hcons2 (first hconss2))
             (solutions
              (loop
                  for hcons1 in hconss1
                  for result = (unify-hcons hcons1 hcons2 solution)
                  when result collect result)))
        (when solutions
          (loop
              for solution in solutions
              nconc (unify-hconss hconss2 (rest hconss2) solution)))))
    (unless hconss2 (list solution))))

(defun unify-hcons (hcons1 hcons2 solution)
  ;;
  ;; okay, here is the current (22-jan-04) rationale about HCONS unification;
  ;; all handles in HCONS must occur somewhere else in the input MRS (this is
  ;; enforced by the MRS reader0; in the MTR, HCONS elements that do not link
  ;; up with other parts of the rule, could only be used to unconditionally
  ;; ditch all HCONS elements, maybe of a certain type, from the input MRS.
  ;; when unifying HCONSs, we disallow the creation of additional equations for
  ;; handles that are bound already, i.e. :strictp to unify-values() will cause
  ;; failure where a variable that is forwarded already would end up being
  ;; equated with yet another variable (while, ordinarily, we do allow chains
  ;; of transitive equalities).                                 (22-jan-04; oe)
  ;;
  (let* ((solution (copy-solution solution))
         (harg (unify-values
                (mrs:hcons-scarg hcons1) (mrs:hcons-scarg hcons2) 
                solution :strictp t))
         (larg (when harg
                 (unify-values
                  (mrs:hcons-outscpd hcons1) (mrs:hcons-outscpd hcons2)
                  solution :strictp t))))
    (when larg 
      (align-hconss hcons1 hcons2 solution)
      solution)))

(defun unify-values (variable1 variable2 solution &key strictp)
  (let* ((value1 (if (mrs::var-p variable1) 
                   (retrieve-variable variable1 solution)
                   variable1))
         (forwardp1 (not (eq variable1 value1)))
         (value2 (if (mrs::var-p variable2)
                   (retrieve-variable variable2 solution)
                   variable2))
         (forwardp2 (not (eq variable2 value2))))
    (cond
     ((or (eq value1 value2) (null value2))
      value1)
     ((and (numberp value1) (numberp value2))
      (= value1 value2))
     ;;
     ;; _fix_me_
     ;; why this special case?  should be subsumed by unify-types(), i suppose.
     ;;                                                        (24-jan-04; oe)
     ((and (stringp value1) (stringp value2))
      (string-equal value1 value2))
     ((and (or (stringp value1) (symbolp value1))
           (or (stringp value2) (symbolp value2)))
      (unify-types value1 value2))
     ((null value1) 
      value2)
     ((and (mrs::var-p value1) (not (mrs::var-p value2)))
      (forward-variable value1 value2 solution)
      value2)
     ((and strictp (or forwardp1 forwardp2)) nil)
     (t
      (let* ((type (unify-types 
                    (mrs::var-type value1) (mrs::var-type value2) :internp t))
             (extras (unify-extras
                      (mrs:var-extra value1) (mrs:var-extra value2)))
             (new (when (and type (listp extras)) 
                    (new-variable type extras))))
        ;;
        ;; _fix_me_
        ;; maybe we could re-use one of the input variables if they contained
        ;; all the information of .new. already; that would be reminiscent of
        ;; what we call `subgraph sharing' in unification?    (24-oct-03; oe)
        ;;
        (when new
          #+:debug
          (format 
           t 
           "unify-values(): ~a [~a] & ~a [~a] == ~a.~%" 
           variable1 value1 variable2 value2 new)
          (forward-variable value1 new solution)
          (forward-variable value2 new solution)
          new))))))

(defun unify-types (type1 type2 &key internp)
  (let* ((type1 (if internp (intern (string-upcase type1) :lkb) type1))
         (type2 (if internp (intern (string-upcase type2) :lkb) type2))
         (glb (and (lkb::is-valid-type type1) (lkb::is-valid-type type2)
                   (lkb::greatest-common-subtype type1 type2))))
    (when glb (if internp (string-downcase (string glb)) glb))))

(defun unify-extras (extras1 extras2)
  (let* ((common (intersect 
                  extras1 extras2 :key #'mrs::extrapair-feature :test #'eq))
         (result
          (loop
              for extra1 in common
              for feature = (mrs::extrapair-feature extra1)
              for value1 = (mrs::extrapair-value extra1)
              for extra2 = (find feature extras2 :key #'mrs::extrapair-feature)
              for value2 = (mrs::extrapair-value extra2)
              for value = (if (and (stringp value1) (stringp value2)
                                   (string-equal value1 value2))
                            value1
                            (unify-types value1 value2))
              unless value do (return-from unify-extras :fail)
              else collect (mrs::make-extrapair 
                            :feature feature :value value))))
    (loop
        for extra in extras1
        for feature = (mrs::extrapair-feature extra)
        unless (member feature common :key #'mrs::extrapair-feature :test #'eq)
        do (push extra result))
    (loop
        for extra in extras2
        for feature = (mrs::extrapair-feature extra)
        unless (member feature common :key #'mrs::extrapair-feature :test #'eq)
        do (push extra result))
    result))

(defun unify-preds (pred1 pred2 solution)
  ;;
  ;; _fix_me_
  ;; what about PREDs that stand in a subsumption relation?  presumably, we
  ;; also need to record the result somewhere, or return it?    (8-jan-04; oe)
  ;;
  (let ((pred2 (if (mrs::var-p pred2) 
                 (retrieve-variable pred2 solution)
                 pred2)))
    (cond
     ((mrs::var-p pred2)
      (forward-variable pred2 pred1 solution)
      pred1)
     ((and (stringp pred1) (stringp pred2))
      (string-equal pred1 pred2))
     ((or (eq pred1 pred2) (null pred2))))))

(defun expand-solution (mrs mtr solution)
  ;;
  ;; go through EPs from .mrs., ditching those that were aligned with one from
  ;; .mtr. during unification; then, through in EPs from .mtr. OUTPUT part and
  ;; unify in all applicable information from .solution.  eventually, do more
  ;; or less the same for HCONS.
  ;;
  (let* ((top (when (mrs:psoa-top-h mrs) 
                (expand-value (mrs:psoa-top-h mrs) solution)))
         (index (when (mrs:psoa-index mrs) 
                  (expand-value (mrs:psoa-index mrs) solution)))
         (result (mrs::make-psoa :top-h top :index index)))
    (setf (mrs:psoa-liszt result)
      (nconc
       ;;
       ;; the loop() will make sure that we have a full copy of :liszt in the
       ;; `output' MRS .result., hence will not modify the input destructively.
       ;;
       (loop
           with input = (and (mtr-input mtr) (mrs:psoa-liszt (mtr-input mtr)))
           for ep1 in (mrs:psoa-liszt mrs)
           for ep2 = (retrieve-ep ep1 solution)
           unless (and ep2 (find ep2 input)) 
           collect (expand-ep ep1 solution))
       (loop
           with defaults = (when (mrs::psoa-p (mtr-defaults mtr))
                             (mrs:psoa-liszt (mtr-defaults mtr)))
           for ep in (and (mtr-output mtr) (mrs:psoa-liszt (mtr-output mtr)))
           for default = (pop defaults)
           collect (merge-eps (expand-ep ep solution) default))))
    (setf (mrs:psoa-h-cons result)
      (nconc
       ;;
       ;; do the same thing, give or take, for the handle contraints
       ;;
       (loop
           with input = (and (mtr-input mtr) (mrs:psoa-h-cons (mtr-input mtr)))
           for hcons1 in (mrs:psoa-h-cons mrs)
           for hcons2 = (retrieve-hcons hcons1 solution)
           unless (and hcons2 (find hcons2 input)) 
           collect 
             (mrs::make-hcons 
              :relation (mrs:hcons-relation hcons1)
              :scarg (retrieve-variable (mrs:hcons-scarg hcons1) solution)
              :outscpd (retrieve-variable 
                        (mrs:hcons-outscpd hcons1) solution)))
       (loop
           for hcons in (and (mtr-output mtr) 
                             (mrs:psoa-h-cons (mtr-output mtr)))
           collect 
             (mrs::make-hcons 
              :relation (mrs:hcons-relation hcons)
              :scarg (expand-value (mrs:hcons-scarg hcons) solution)
              :outscpd (expand-value
                        (mrs:hcons-outscpd hcons) solution)))))
    result))

(defun expand-ep (ep solution)
  (let* ((label (expand-value (mrs:rel-handel ep) solution))
         (pred (let ((pred (mrs:rel-pred ep)))
                  (if (mrs::var-p pred)
                    (retrieve-variable pred solution)
                    pred)))
         (result (mrs::make-rel :handel label :pred pred)))
    (setf (mrs:rel-flist result)
      (loop
          for role in (mrs:rel-flist ep)
          for feature = (mrs:fvpair-feature role)
          for value = (expand-value (mrs:fvpair-value role) solution)
          collect (mrs::make-fvpair :feature feature :value value)))
    result))

(defun expand-value (value solution)
  (if (mrs::var-p value)
    (let ((foo (retrieve-variable value solution)))
      (if (and (mrs::var-p foo) (< (mrs::var-id foo) 0))
        (let ((new (new-variable (mrs::var-type foo) (mrs::var-extra foo))))
          (forward-variable foo new solution)
          new)
        foo))
    value))

(defun merge-eps (ep default)
  (unless default (return-from merge-eps ep))
  (when (mrs::rel-pred default)
    (setf (mrs::rel-pred ep) (mrs::rel-pred default)))
  (loop
      with defaults = (mrs:rel-flist default)
      for role in (mrs:rel-flist ep)
      for feature = (mrs:fvpair-feature role)
      for value = (mrs:fvpair-value role)
      for default = (find feature defaults :key #'mrs:fvpair-feature)
      when (and value default) do 
        ;;
        ;; _fix_me_
        ;; this is not quite right: as long as we well-type the DEFAULTS part,
        ;; there could be non-variable defaults that are more general than the
        ;; actual value, for CARGs, say.                        (8-jan-04; oe)
        ;;
        (let ((default (mrs:fvpair-value default)))
          (if (and (mrs::var-p value) (mrs::var-p default))
            (merge-values value default)
            (setf (mrs:fvpair-value role) default))) 
      else when default do
        (push (mrs::copy-fvpair default) (mrs:rel-flist ep)))
  ep)

(defun merge-values (variable default)
  ;;
  ;; _fix_me_
  ;; apparently, we assume we can frob the input .value., presumably we know
  ;; it to be a copy at this point.  better confirm this ...    (8-jan-04; oe)
  ;;
  ;;
  ;; merge in properties from .default. but keep .variable. identity.  also,
  ;; take care to not overwrite using `vacuous' constraints, i.e. ones that
  ;; were introduced by well-typing only.
  ;;
  ;; _fix_me_
  ;; a little unsure about variable types too: is it possible for the default
  ;; part to have a less specific type than the variable already?
  ;;                                                           (22-jan-04; oe)
  (when (null default) (return-from merge-values variable))
  (when (and (mrs::var-type default)
             (or (null (mrs::var-type variable))
                 (not (member (mrs::var-type default) '
                              ("u" "i") :test #'string-equal))))
    (setf (mrs::var-type variable) (mrs::var-type default)))
  (setf (mrs:var-extra variable)
    (if (null (mrs:var-extra variable))
      (loop
          for extra in (mrs:var-extra default)
          for feature = (mrs::extrapair-feature extra)
          for value = (mrs::extrapair-value extra)
          collect (mrs::make-extrapair :feature feature :value value))
      (loop
          with defaults = (mrs:var-extra default)
          for extra in (mrs:var-extra variable)
          for feature = (mrs::extrapair-feature extra)
          for default = (let ((default (find feature defaults 
                                             :key #'mrs::extrapair-feature)))
                          (and default (mrs::extrapair-value default)))
          when (or (null default) (vacuous-constraint-p feature default))
          collect (mrs::make-extrapair 
                   :feature feature :value (mrs::extrapair-value extra))
          else collect (mrs::make-extrapair :feature feature :value default))))
  variable)

(defun new-variable (type extras)
  (let ((id %transfer-variable-id%))
    (incf %transfer-variable-id%)
    (mrs::make-var :id id :type type :extra extras)))

(defun constant-role-p (role)
  (member role mrs::*value-feats* :test #'eq))

;;;
;;; _fix_me_
;;; as it stands, the unification ends up sharing variables with the original
;;; input MRS, hence postprocess-mrs() is required to create copies at this
;;; stage, to avoid frobbing the original.                    (3-nov-03; oe)
;;;
(defun accumulate-properties (type properties 
                              &optional
                              (accumulator %transfer-properties-accumulator%))
  (when properties
    (loop
        with type = (if (stringp type) (mrs::vsym (string-upcase type)) type)
        with output with obsolete
        for (key match . rules) in accumulator
        for features = (butlast match)
        for extras = (loop
                         for feature in features
                         collect (find
                                  feature properties
                                  :key #'mrs::extrapair-feature))
        when (and (or (null type) (null key) (eq type key))
                  (= (length features) (length extras))) do
          (loop
              with values = (loop
                                for extra in extras
                                collect (when (mrs::extrapair-p extra)
                                          (mrs::extrapair-value extra)))
              for rule in rules
              when (search values rule :test #'eq) do
                (push 
                 (mrs::make-extrapair 
                  :feature (first (last match)) 
                  :value (first (last rule)))
                 output)
                (loop 
                    for extra in extras 
                    when (mrs::extrapair-p extra) do (pushnew extra obsolete)))
        finally
          (return
            (nconc
             output
             (loop 
                 for property in properties
                 unless (member property obsolete) 
                 collect property))))))

(defun postprocess-mrs (mrs 
                        &optional 
                        (variables %transfer-properties-filter%)
                        (accumulator %transfer-properties-accumulator%) 
                        (values %transfer-values-filter%))
  (let ((mrs (mrs::copy-psoa mrs))
        copies)
    (labels ((postprocess-variable (variable)
               (or (rest (assoc variable copies))
                   (let ((copy (mrs::copy-var variable)))
                     (setf (mrs:var-extra copy)
                       (accumulate-properties 
                        (mrs:var-type copy)
                        (mrs:var-extra copy) accumulator))
                     (setf (mrs:var-extra copy)
                       (loop
                           for extra in (mrs:var-extra copy)
                           for feature = (mrs::extrapair-feature extra)
                           for value = (mrs::extrapair-value extra)
                           for fmatch = (find feature variables :key #'first)
                           for vmatch = (find value values :key #'first)
                           unless (or (and fmatch (null (rest fmatch)))
                                      (and vmatch (null (rest vmatch))))
                           collect
                             (if (or fmatch vmatch)
                               (mrs::make-extrapair 
                                :feature (or (rest fmatch) feature)
                                :value (or (rest vmatch) value))
                               extra)))
                     (push (cons variable copy) copies)
                     copy))))
      (setf (mrs:psoa-top-h mrs) (postprocess-variable (mrs:psoa-top-h mrs)))
      (setf (mrs:psoa-index mrs) (postprocess-variable (mrs:psoa-index mrs)))
      (setf (mrs:psoa-liszt mrs)
        (loop
            for ep in (mrs:psoa-liszt mrs)
            for copy = (mrs::copy-rel ep)
            do
              (setf (mrs::rel-flist copy)
                (loop
                    for role in (mrs:rel-flist ep)
                    for value = (mrs:fvpair-value role)
                    collect 
                      (mrs::make-fvpair 
                       :feature (mrs:fvpair-feature role)
                       :value (if (mrs::var-p value) 
                                (postprocess-variable value)
                                value))))
            collect copy))
      (mrs::fill-mrs mrs %transfer-properties-defaults%))))

(defun merge-and-copy-mrss (mrs1 mrs2)
  ;;
  ;; _fix_me_
  ;; this function is a little overly eager in making copies, thus breaking 
  ;; eq-ness() of variables in the result; for now, we only use it for display
  ;; purposes (to overlay the OUTPUT and DEFAULT parts of MTRs), hence no need
  ;; to loose sleep yet.                                       (9-jan-04; oe)
  ;;
  (let* ((top (when (mrs:psoa-top-h mrs1)
                (merge-values 
                 (mrs::copy-var (mrs:psoa-top-h mrs1)) 
                 (and mrs2 (mrs:psoa-top-h mrs2)))))
         (index (when (mrs:psoa-index mrs1) 
                  (merge-values 
                   (mrs::copy-var (mrs:psoa-index mrs1))
                   (and mrs2 (mrs:psoa-index mrs2)))))
         (result (mrs::make-psoa :top-h top :index index)))
    (setf (mrs:psoa-liszt result)
      (loop
          with defaults = (and mrs2 (mrs:psoa-liszt mrs2))
          for ep in (mrs:psoa-liszt mrs1)
          for default = (pop defaults)
          collect (merge-eps ep default)))
    (setf (mrs:psoa-h-cons result)
      (loop
          for hcons in (mrs:psoa-h-cons mrs1)
          collect 
            (mrs::make-hcons 
             :relation (mrs:hcons-relation hcons)
             :scarg (mrs::copy-var (mrs:hcons-scarg hcons))
             :outscpd (mrs::copy-var (mrs:hcons-outscpd hcons)))))
    result))

(defun intersect (set1 set2 &key (key #'identity) (test #'eql))
  ;;
  ;; much like intersection(), except guarantee that all elements returned are
  ;; taken from .set1.
  ;;
  ;; _fix_me_
  ;; provide an actual implementation one day :-}.             (23-oct-03; oe)
  ;;
  (loop
      with intersection = (intersection set1 set2 :key key :test test)
      for foo in set1
      when (member (funcall key foo) intersection :key key :test test)
      collect foo))

(defun browse (object)
  (typecase object
    (edge (mrs::browse-mrs (edge-mrs object)))
    (mrs::psoa (mrs::browse-mrs object))))
