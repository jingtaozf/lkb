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

(defparameter %transfer-edge-id% 100)

(defparameter %transfer-variable-id% 100)

(defparameter %transfer-raw-output-p% nil)

(defparameter %transfer-edges% nil)

(defparameter *transfer-edge-limit* 20000)

(defparameter *transfer-debug-stream* 
  (or #+:allegro excl:*initial-terminal-io* t))

(defparameter %transfer-properties-filter%
  (list
   (cons (mrs::vsym "TENSE") (mrs::vsym "E.TENSE"))
   (cons (mrs::vsym "MOOD") (mrs::vsym "E.MOOD"))
   (cons (mrs::vsym "PERS") nil)
   (cons (mrs::vsym "NUM") nil)
   (cons (mrs::vsym "ASPECT-PROTRACTED") nil)
   (cons (mrs::vsym "ASPECT-STATIVE") nil)
   (cons (mrs::vsym "ASPECT-TELIC") nil)
   (cons (mrs::vsym "ASPECT-BOUNDED") nil)
   (cons (mrs::vsym "ASPECT-INCHOATIVE") nil)))

(defparameter %transfer-values-filter%
  (list
   #+:null
   (cons (mrs::vsym "pres") (mrs::vsym "present"))))

(defstruct mtrs
  id mtrs)

(defstruct mtr
  id
  context filter
  input output defaults
  variables)

(defmethod print-object ((object mtr) stream)
  (if %transfer-raw-output-p%
    (call-next-method)
    (format
     stream
     "~@[!~a ~]~@[@~a ~]~@[~a -> ~]~a"
     (mtr-filter object) (mtr-context object)
     (mtr-input object) (mtr-output object))))

(defstruct (edge (:constructor make-edge-x))
  (id (let ((n %transfer-edge-id%)) (incf %transfer-edge-id%) n))
  rule mrs daughter n 
  (source 0))

(defun make-edge (&rest rest)
  (let ((edge (apply #'make-edge-x rest)))
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
    (format
     stream
     "~<#D[~;~a~:[~2*~; ~(~a~) ~_~a~]~;]~:>"
     (list
      (edge-id object) 
      (and (mtr-p (edge-rule object)) (edge-p (edge-daughter object)))
      (mtr-id (edge-rule object)) (edge-daughter object)))))

(defun print-edges ()
  (loop
      for edge in %transfer-edges%
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

(defun forward-variable (old new solution)
  (setf (getf (solution-variables solution) old) new))

(defun retrieve-variable (variable solution)
  (let ((new (getf (solution-variables solution) variable)))
    (if new (retrieve-variable new solution) variable)))

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

(defun read-transfer-rules (files &optional name)
  
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
                          (loop
                              with n = (length *mtr-output-path*)
                              for unification in unifications
                              for foo = (lkb::unification-lhs unification)
                              for bar = (lkb::unification-rhs unification)
                              when (and (lkb::path-p foo)
                                        (null
                                         (mismatch
                                          *mtr-output-path*
                                          (lkb::path-typed-feature-list foo)
                                          :end2 n))
                                        (lkb::u-value-p bar))
                              do 
                                (push unification rhs)
                                (push 
                                 (lkb::make-unification :lhs foo :rhs foo)
                                 lhs)
                              else do 
                                (push unification lhs)
                                (let ((lhsp
                                       (when (lkb::path-p foo)
                                         (null
                                          (mismatch
                                           *mtr-output-path*
                                           (lkb::path-typed-feature-list foo)
                                           :end2 n))))
                                      (rhsp
                                       (when (lkb::path-p bar)
                                         (null
                                          (mismatch
                                           *mtr-output-path*
                                           (lkb::path-typed-feature-list bar)
                                           :end2 n)))))
                                  (cond
                                   ((and lhsp rhsp)
                                    (push unification rhs))
                                   (lhsp
                                    (push
                                     (lkb::make-unification :lhs foo :rhs foo)
                                     rhs))
                                   (rhsp
                                    (push
                                     (lkb::make-unification :lhs bar :rhs bar)
                                     rhs)))))
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
                            (when rule (push rule rules))))))))))
        finally (push 
                 (make-mtrs :id id :mtrs (nreverse rules))
                 *transfer-rule-sets*))))

(defun convert-dag-to-mtr (lhs rhs id)
  ;;
  ;; _fix_me_
  ;; give some thought to variable types: authors of transfer rules might well
  ;; expect that variable names contribute type information in the MRS spirit.
  ;; we would have to go through .dag. at some point (soon enough to still have
  ;; access to the original tag names) and add :type information to each node.
  ;;
  #+:debug
  (setf %lhs% lhs)
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
                        (mrs::construct-mrs defaults generator))))
    (unless (or output rhs)
      ;;
      ;; warn: rule with no output specification
      ;;
      (format
       *transfer-debug-stream*
       "convert-dag-to-mtr(): `~(~a~)' has an empty output specification.~%"
       id))
    (make-mtr :id id :filter filter :context context
              :input input :output output :defaults defaults
              :variables (nreverse mrs::*named-nodes*))))

(defun vacuous-constraint-p (path dag)
  (let* ((type (lkb::minimal-type-for (first (last path))))
         (constraint (and type (lkb::constraint-of type))))
    (lkb::dag-subsumes-p dag constraint)))

;;;
;;; top-level drivers for application of transfer rules; use `edge' structure
;;; to keep track of something reminiscent of a derivation tree.
;;;
(defun initialize-transfer ()
  (setf *transfer-rule-sets* nil))

(defun transfer-mrs (mrs &key (filterp t))
  #+:debug
  (setf %mrs% mrs)
  (setf %transfer-edges% nil)
  (setf %transfer-edge-id% 0)
  (multiple-value-bind (result condition)
      (ignore-errors 
       (transfer-mrs2 
        (list (make-edge :mrs mrs :n 42)) 
        *transfer-rule-sets*
        :filterp filterp))
    (when condition
      #+:clim
      (clim:beep)
      (format
       *transfer-debug-stream*
       "transfer-mrs(): `~a'~%" condition))
    result))

(defun transfer-mrs2 (edges mtrss &key (filterp nil))
  (if (null mtrss)
    edges
    (transfer-mrs2 
     (loop
         for edge in edges
         append (apply-mtrs edge (first mtrss) :filterp filterp))
     (rest mtrss)
     :filterp filterp)))

(defun apply-mtrs (edge mtrs &key (filterp t))

  (when (or (null edge) (null mtrs)) (return-from apply-mtrs))
  
  (loop
      with result
      with agenda = (list edge)
      with all = (mtrs-mtrs mtrs)
      for task = (pop agenda)
      for rule = (and task (edge-rule task))
      for mtrs = (if (null rule) 
                   all
                   (member rule all))
      while task do
        (push task result)
        (loop
            for mtr in mtrs
            for edges = (apply-mtr task mtr)
            do
              (loop
                  for task in edges 
                  do (push task agenda)))
      finally 
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
               :rule mtr :mrs (postprocess-mrs mrs) :daughter edge 
               :n %transfer-variable-id%)))

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
  (let* ((filter (mtr-filter mtr))
         (context (mtr-context mtr))
         (input (mtr-input mtr))
         solutions)
    (when (and filter (unify-mtr-component mrs filter))
      ;;
      ;; trace
      ;;
      (return-from unify-mtr))
    (setf solutions (unify-mtr-component mrs context))
    (unless solutions
      ;;
      ;; trace
      ;;
      (return-from unify-mtr))
    (when input
      (setf solutions
        (loop
            for solution in solutions
            append (unify-mtr-component mrs input solution)))
      (unless solutions
        ;;
        ;; trace
        ;;
        (return-from unify-mtr))
      solutions)))

(defun unify-mtr-component (mrs1 mrs2 &optional solution)
  (if (null mrs2)
    (list solution)
    (let* ((solution (if solution (copy-solution solution) (make-solution)))
           (top1 (mrs:psoa-top-h mrs1))
           (top2 (mrs:psoa-top-h mrs2)))
      (unless (unify-values top1 top2 solution)
        ;;
        ;; trace
        ;;
        (return-from unify-mtr-component))
      (let* ((index1 (mrs:psoa-index mrs1))
             (index2 (mrs:psoa-index mrs2)))
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
  ;; will always copy its input parameter first.
  ;;
  (when eps1
    (if (null eps2)
      (list solution)
      (nconc
       (loop
           with ep1 = (first eps1)
           for ep2 in eps2
           for result = (unify-eps ep1 ep2 solution)
           when result collect result)
       (unify-epss (rest eps1) eps2 solution)))))

;;;
;;; in unifying two sets of EPs, i believe we are assuming that no EP can be
;;; unified with more than one counterpart (which intuitively seems to make a
;;; lot of sense); it means there cannot be `overlap' between the CONTEXT and
;;; INPUT parts of a rule, which is fine since bindings are accumulated from
;;; both, so there should be no need to have a duplicate EP (targetting just
;;; one EP in the input MRS) spread out over those two.       (23-oct-03; oe)
;;;
(defun unify-eps (ep1 ep2 solution)
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
                    (mrs:fvpair-value role1) (mrs:fvpair-value role2)
                    solution)
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
  (if hconss1
    (if (null hconss2)
      (list solution)
      (nconc
       (loop
           with hcons1 = (first hconss1)
           for hcons2 in hconss2
           for result = (unify-hcons hcons1 hcons2 solution)
           when result collect result)
       (unify-hconss (rest hconss1) hconss2 solution)))))

(defun unify-hcons (hcons1 hcons2 solution)
  (let* ((solution (copy-solution solution))
         (harg (unify-values
                (mrs:hcons-scarg hcons1) (mrs:hcons-scarg hcons2) solution))
         (larg (when harg
                 (unify-values
                  (mrs:hcons-outscpd hcons1) (mrs:hcons-outscpd hcons2)
                  solution))))
    (when larg 
      (align-hconss hcons1 hcons2 solution)
      solution)))

(defun unify-values (value1 value2 solution)
  (let* ((value1 (if (mrs::var-p value1)
                   (retrieve-variable value1 solution) 
                   value1))
         (value2 (if (mrs::var-p value2)
                   (retrieve-variable value2 solution) 
                   value2)))
    (cond
     ((or (eq value1 value2) (null value2))
      value1)
     ((and (numberp value1) (numberp value2))
      (= value1 value2))
     ((and (stringp value1) (stringp value2))
      (string-equal value1 value2))
     ((null value1) 
      value2)
     ((and (mrs::var-p value1) (not (mrs::var-p value2)))
      (forward-variable value1 value2 solution)
      value2)
     (t
      (let* ((type (unify-types 
                    (mrs::var-type value1) (mrs::var-type value2) :internp t))
             (extras (unify-extras
                      (mrs:var-extra value1) (mrs:var-extra value2)))
             (new (when (and type (listp extras)) (new-variable type extras))))
        ;;
        ;; _fix_me_
        ;; maybe we could re-use one of the input variables if they contained
        ;; all the information of .new. already; that would reminiscent of what
        ;; we call `subgraph sharing' in unification.  issues?  (24-ocr-03; oe)
        ;;
        (when new
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
           with input = (let ((mrs (mtr-input mtr)))
                          (and mrs (mrs:psoa-liszt mrs)))
           for ep1 in (mrs:psoa-liszt mrs)
           for ep2 = (retrieve-ep ep1 solution)
           unless (and ep2 (find ep2 input)) 
           collect (expand-ep ep1 solution))
       (loop
           with defaults = (when (mrs::psoa-p (mtr-defaults mtr))
                             (mrs:psoa-liszt (mtr-defaults mtr)))
           for ep in (mrs:psoa-liszt (mtr-output mtr))
           for default = (pop defaults)
           collect (merge-eps (expand-ep ep solution) default))))
    (setf (mrs:psoa-h-cons result)
      (nconc
       ;;
       ;; do the same thing, give or take, for the handle contraints
       ;;
       (loop
           with input = (let ((mrs (mtr-input mtr)))
                          (and mrs (mrs:psoa-h-cons mrs)))
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
           for hcons in (mrs:psoa-h-cons (mtr-output mtr))
           collect 
             (mrs::make-hcons 
              :relation (mrs:hcons-relation hcons)
              :scarg (retrieve-variable (mrs:hcons-scarg hcons) solution)
              :outscpd (retrieve-variable 
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
    (retrieve-variable value solution)
    value))

(defun merge-eps (ep default)
  (unless default
    (return-from merge-eps ep))
  (when (mrs::rel-pred default)
    (setf (mrs::rel-pred ep) (mrs::rel-pred default)))
  (loop
      with defaults = (mrs:rel-flist default)
      for role in (mrs:rel-flist ep)
      for feature = (mrs:fvpair-feature role)
      for value = (mrs:fvpair-value role)
      for default = (find feature defaults :key #'mrs:fvpair-feature)
      when (and value default) do 
        (let ((default (mrs:fvpair-value default)))
          (if (and (mrs::var-p value) (mrs::var-p default))
            (merge-values value default)
            (setf (mrs:fvpair-value role) default))) 
      else when default do
        (push (mrs::copy-fvpair default) (mrs:rel-flist ep)))
  ep)

(defun merge-values (value default)
  (loop
      with defaults = (mrs:var-extra default)
      for extra in (mrs:var-extra value)
      for feature = (mrs::extrapair-feature extra)
      for value = (mrs::extrapair-value extra)
      for default = (find feature defaults :key #'mrs::extrapair-feature)
      when (and value default) do 
        (setf (mrs::extrapair-value extra) (mrs::extrapair-value default))
      else when default do
        (push (mrs::copy-extrapair default) (mrs:var-extra value)))
  value)

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
(defun postprocess-mrs (mrs &optional (variables %transfer-properties-filter%)
                                      (values %transfer-values-filter%))
  (let (copies)
    (labels ((postprocess-variable (variable)
               (or (rest (assoc variable copies))
                   (let ((copy (mrs::copy-var variable)))
                     (setf (mrs:var-extra copy)
                       (loop
                           for extra in (mrs:var-extra variable)
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
      (setf (mrs:psoa-index mrs) (postprocess-variable (mrs:psoa-index mrs)))
      (loop
          for ep in (mrs:psoa-liszt mrs)
          do
            (loop
                for role in (mrs:rel-flist ep)
                for value = (mrs:fvpair-value role)
                when (mrs::var-p value) 
                do 
                (setf (mrs:fvpair-value role) (postprocess-variable value)))))
    mrs))

(defun intersect (set1 set2 &key (key #'identity) (test #'eql))
  ;;
  ;; much like intersection(), except guarantee that all elements returned are
  ;; taken from .set1.
  ;;
  ;; _fix_me_
  ;; provide native implementation one day :-}.              (23-oct-03; oe)
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
