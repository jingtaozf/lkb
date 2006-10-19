;;;
;;; Copyright (c) 2004 -- 2006 Stephan Oepen (oe@csli.stanford.edu)
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as published by
;;; the Free Software Foundation; either version 2.1 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
;;; License for more details.
;;; 

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

(defparameter *transfer-edge-limit* 800)

(defparameter *transfer-debug-p* nil)

(defparameter *transfer-debug-stream* 
  (or #+:allegro excl:*initial-terminal-io* t))

(defparameter *transfer-filter-p* t)

(defparameter *transfer-interlingua-predicates* nil)

(defparameter *transfer-preemptive-filter-p* nil)

(defparameter *transfer-tm* nil)

(defparameter *transfer-postprocess-p* t)

(defparameter *transfer-skolemize-p* nil)

(defparameter *transfer-maximum-rank* nil)

(defparameter %transfer-generation% 0)

(defparameter %transfer-edge-id% 100)

(defparameter %transfer-rule-id% 0)

(defparameter %transfer-copy-eps% nil)

(defparameter %transfer-special-elements% nil)

(defparameter %transfer-optional-rules% 0)

(defparameter %transfer-variable-features% nil)

(defparameter %transfer-variable-id% 100)

(defparameter %transfer-raw-output-p% nil)

(defparameter %transfer-recursive-output-p% t)

(defparameter %transfer-edges% nil)

(defparameter %transfer-chart% nil)

(defparameter %transfer-original-variables% nil)

(defparameter %transfer-solutions% nil)

(defstruct mtrs
  id mtrs before after in out flags)

(defstruct mtr
  id
  context filter
  input output defaults
  variables vector flags special rank)

(defmethod print-object ((object mtr) stream)
  (if %transfer-raw-output-p%
    (call-next-method)
    (format
     stream
     "~@[~a : ~]~@[~a ~]~@[! ~a ~]-> ~a~@[ / ~a~]"
     (mtr-context object) (mtr-input object) (mtr-filter object) 
     (mtr-output object) (mtr-defaults object))))

(defmacro mtrs-filter-p (mtrs)
  `(getf (mtrs-flags ,mtrs) :filter))

(defmacro mtrs-recurse-p (mtrs)
  `(getf (mtrs-flags ,mtrs) :recurse))

(defmacro mtrs-subsume-p (mtrs)
  `(getf (mtrs-flags ,mtrs) :subsume))

(defmacro mtrs-task (mtrs)
  `(getf (mtrs-flags ,mtrs) :task))

(defmacro mtrs-edges (mtrs)
  `(getf (mtrs-flags ,mtrs) :edges))

(defmacro mtr-optional-p (mtr)
  `(member :optional (mtr-flags ,mtr) :test #'eq))

(defmacro mtr-fail-p (mtr)
  `(member :fail (mtr-flags ,mtr) :test #'eq))

(defmacro mtr-exhaustive-p (mtr)
  `(member :exhaustive (mtr-flags ,mtr) :test #'eq))

(defun mtr-trigger (mtr)
  (loop
      for (value . key) in (mtr-special mtr)
      when (eq key :trigger) return value))

(defun interlingua-predicate-p (pred)
  (member pred *transfer-interlingua-predicates* :test #'equal))

(defun transfer-rule-sets (&optional task)
  (loop
      for mtrs in *transfer-rule-sets*
      for foo = (mtrs-task mtrs)
      when (or (null task) (null foo) (eq task foo))
      collect mtrs))

(defstruct (edge (:constructor make-edge-x))
  (id (let ((n %transfer-edge-id%)) (incf %transfer-edge-id%) n))
  rule mrs daughter solution n (vector 0)
  source (depth 0) score)

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
    (setf (edge-depth edge) 
      (if (edge-p daughter)
        (+ 1 (edge-depth daughter))
        1))
    (when (edge-p daughter) (setf (edge-source edge) (edge-source daughter)))
    (push edge %transfer-edges%)
    (cond
     ((and (numberp *transfer-edge-limit*)
           (> (edge-id edge) *transfer-edge-limit*))
      (when (member :chart *transfer-debug-p* :test #'eq)
        (print-edges))
      (error 
       "make-edge(): transfer edge limit exhausted (~a)"
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
         "~<#D[~;~a {~,2f}~@[ ~(~a~)~]~:[~* ...~; ~_~a~]~;]~:>"
         (list
          (edge-id object)
          (if (numberp (edge-score object)) (edge-score object) 0)
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

(defun copy-solution (&optional solution)
  (if solution
    (let ((result (x-copy-solution solution)))
      (setf (solution-variables result)
        (copy-list (solution-variables solution)))
      (setf (solution-eps result)
        (copy-list (solution-eps solution)))
      (when (member :solutions *transfer-debug-p* :test #'eq)
        (push result %transfer-solutions%))
      result)
    (let ((new (make-solution)))
      (when (member :solutions *transfer-debug-p* :test #'eq)
        (push new %transfer-solutions%))
      new)))

(defun solution<= (solution1 solution2)
  (let ((eps1 (length (solution-eps solution1)))
        (eps2 (length (solution-eps solution2))))
    (or (< eps1 eps2)
        (when (= eps1 eps2)
          (let ((variables1 (length (solution-variables solution1)))
                (variables2 (length (solution-variables solution2))))
            (or (< variables1 variables2)
                (when (= variables1 variables2)
                  (< (length (solution-hconss solution1))
                     (length (solution-hconss solution2))))))))))

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
  (if (mrs::var-p variable)
    (let ((new (getf (solution-variables solution) variable)))
      (if (and new (not (eq new variable)))
        (retrieve-variable new solution)
        variable))
    variable))

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

(defun read-transfer-rules (files &optional name 
                            &key (filter nil filterp)
                                 before after in out
                                 reset task optional (recurse t) subsume edges)

  ;;
  ;; _fix_me_
  ;; make top variable type (`u') customizable, one day.       (22-jan-04; oe)
  ;;
  (when (lkb::is-valid-type (mrs::vsym *semi-u-type*))
    (loop
        for type in (lkb::retrieve-descendants (mrs::vsym *semi-u-type*))
        for name = (lkb::ltype-name type)
        do
          (loop
              for feature in (lkb::appropriate-features-of name)
              do (pushnew feature %transfer-variable-features%))))

  (when reset (initialize-transfer))
  
  ;;
  ;; protect general MRS code from ending up using our variable generator, as
  ;; construct-mrs() makes a global assignment :-(.            (27-jan-04; oe)
  ;;
  (let* ((mrs::*variable-generator* mrs::*variable-generator*)
         (mrs::%mrs-roles-filter% nil)
         (mrs::*mrs-record-all-nodes-p* t)
         (files (if (listp files) files (list files)))
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
           "~&read-transfer-rules(): ignoring invalid file `~a~@[.~a~]'.~%"
           (pathname-name file) (pathname-type file))
        else do
          (format 
           t 
           "~&read-transfer-rules(): reading file `~a~@[.~a~]'.~%"
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
                            lhs constants copies rhs)
                        (when (lkb::check-for #\. stream id)
                          (multiple-value-setq (lhs constants 
                                                copies rhs)
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
                          (let ((rule (convert-dag-to-mtr 
                                       lhs constants copies 
                                       rhs id
                                       :optional optional
                                       :task task)))
                            (when rule 
                              (record-mtr (compile-mtr rule))
                              (push rule rules))))))))))
        finally 
          (let* ((*package* (find-package :mt))
                 (before
                  (ignore-errors
                   (typecase before
                     (null nil)
                     (string (symbol-function (read-from-string before)))
                     (symbol (symbol-function before))
                     (function before))))
                 (after
                  (ignore-errors
                   (typecase after
                     (null nil)
                     (string (symbol-function (read-from-string after)))
                     (symbol (symbol-function after))
                     (function after))))
                 (mtrs (make-mtrs
                        :id id :mtrs (nreverse rules)
                        :before before :after after :in in :out out)))
            (setf (getf (mtrs-flags mtrs) :recurse) recurse)
            (setf (getf (mtrs-flags mtrs) :filter)
              (if filterp filter t))
            (when task (setf (mtrs-task mtrs) task))
            (when edges (setf (mtrs-edges mtrs) edges))
            (when subsume (setf (getf (mtrs-flags mtrs) :subsume) subsume))
            (setf *transfer-rule-sets* 
              (append *transfer-rule-sets* (list mtrs)))))))

(defun discriminate-mtr-unifications (unifications)
  ;;
  ;; _fix_me_
  ;; give a little more thought to built-in assumptions about surface order,
  ;; e.g. confirm that .outputp. below would also work if an MTR had OUTPUT as
  ;; its first component.                                      (27-jan-04; oe)
  ;;
  #+:debug
  (setf %unifications unifications)
  (labels ((prefixp (prefix list n)
             (and (>= (length list) n) (null (mismatch prefix list :end2 n)))))
    (loop
        with lhss with constants with copies with rhss
        with n = (length *mtr-output-path*)
        for unification in unifications
        for lhs = (lkb::unification-lhs unification)
        for path = (and (lkb::path-p lhs) (lkb::path-typed-feature-list lhs))
        for outputp = (prefixp *mtr-output-path* path n)
        for rhs = (lkb::unification-rhs unification)
        when (and path (lkb::u-value-p rhs)
                  (eq (lkb::u-value-type rhs) *mtr-copy-operator*))
        do 
          (push path copies)
          (push 
           (lkb::make-unification :lhs lhs :rhs lhs)
           lhss)
        else when (and outputp (lkb::u-value-p rhs))
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
          ;;
          ;; for coreferences on constant MRS elements (PRED, CARG, et al., and
          ;; properties inside of variables, circumvent construct-mrs() default
          ;; behavior; maybe we should not use construct-mrs(), at some point.
          ;;
          (when (and path (lkb::path-p rhs)
                     (let ((feature (first (last path))))
                       (or (eq feature (first (last mrs::*rel-name-path*)))
                           (member feature mrs::*value-feats*)
                           (member feature %transfer-variable-features%))))
            (push unification constants))
          (let ((rhsp (and (lkb::path-p rhs)
                           (prefixp *mtr-output-path*
                             (lkb::path-typed-feature-list rhs)
                             n))))
            (cond
             ((and outputp rhsp)
              (push unification rhss))
             (rhsp
              (push (lkb::make-unification :lhs rhs :rhs rhs) rhss))
             (outputp 
              (push (lkb::make-unification :lhs lhs :rhs lhs) rhss))))
        finally 
          #+:debug
          (setf %lhss lhss %rhss rhss)
          (return (values lhss constants copies rhss)))))

(defun convert-dag-to-mtr (lhs constants copies rhs id 
                           &key optional task)
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
  ;; _fix_me_
  ;; the method implemented above (finding .constants. among the unifications
  ;; of the MTR specification will only work for coreferences in the instance;
  ;; to generally deal with inherited coreferences too, we will need to walk
  ;; through the dag and identify coreferences ... not that hard, but we will
  ;; have to do a little more work (also worry about coreferences on constant
  ;; nodes more, e.g. handle an additional type appropriately, since constants
  ;; cannot have sub-structures, at least :-).                 (27-jan-04; oe)
  ;;
  ;; _fix_me_
  ;; i now more or less believe we should stop using construct-mrs(), and build
  ;; our own MRS-like data structure instead ... later.        (15-feb-04; oe)
  ;;
  #+:debug
  (setf %lhs lhs %constants constants %copies copies %rhs rhs)
  (let ((generator (let ((n 0)) #'(lambda () (decf n))))
        (mrs::*named-nodes* nil)
        (mrs::*all-nodes* nil))
    ;;
    ;; to allow use of `standard' MRS variables as (meta-level) variables on
    ;; `constant' MRS elements (like PRED, CARG, and extras), work around the
    ;; default construct-mrs() behaviour; maybe we should move to doing all MRS
    ;; construction ourselves, or maybe not use MRS variables for meta-level
    ;; variables.
    ;;
    (when constants (convert-constants-to-variables lhs constants generator))
    ;;
    ;; _fix_me_
    ;; for now, construct MRS components in transfer rules _without_ using the
    ;; SEM-I variable property mapping; however, we surely could, and it would
    ;; probably make transfer rules look cleaner, but (for example) disconnect 
    ;; the generator-internal trigger rules further from the type hierarchy.
    ;; think this over more asap.                              (23-jun-06; oe)
    ;;
    (let* ((*vpms* nil)
           (filter (mrs::path-value lhs *mtr-filter-path*))
           (filter (and filter 
                        (not (vacuous-constraint-p *mtr-filter-path* filter))
                        (mrs::construct-mrs filter generator)))
           (context (mrs::path-value lhs *mtr-context-path*))
           (context 
            (and context 
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
           (defaults 
               (and defaults
                    (not (vacuous-constraint-p *mtr-output-path* defaults))
                    (mrs::construct-mrs defaults generator)))
           (dag (and lhs (mrs::path-value lhs *mtr-flags-path*)))
           (flags (and dag
                       (not (vacuous-constraint-p *mtr-flags-path* dag))
                       (convert-dag-to-flags dag)))
           (flags (if (and optional (not (member :obligatory flags)))
                    (adjoin :optional flags)
                    (remove :obligatory flags)))
           (rank (convert-dag-to-rank dag))
           (special (and dag 
                         (not (vacuous-constraint-p *mtr-flags-path* dag))
                         (convert-dag-to-special dag :id id))))

      ;;
      ;; for later postprocessing, look up relations that were requested for
      ;; OUTPUT copy, and record them in their MTR.
      ;;
      (loop
          for path in copies
          for dag = (mrs::path-value lhs path)
          for ep = (rest (assoc dag mrs::*all-nodes* :test #'eq))
          unless ep do
            (format
             t
             "~&convert-dag-to-mtr(): no EP for +copy+ path `~{~(~a~)~^ ~}'.~%"
             path)
          else do (push ep %transfer-copy-eps%))
      
      (unless (or output rhs (eq task :trigger))
        ;;
        ;; warn: rule with no output specification
        ;;
        (format
         t
         "~&convert-dag-to-mtr(): ~
          `~(~a~)' has empty output specification.~%"
         id))
      ;;
      ;; _fix_me_
      ;; since our current treatment of FILTER components is wrong anyway, give
      ;; a warning and ignore rules that try to use filters.    (8-jan-04; oe)
      ;; 
      (if (and nil filter)
        (format
         t
         "~&convert-dag-to-mtr(): ~
          ignoring `~(~a~)' because it contains a FILTER.~%"
         id)
        (let* ((vector (ash 1 %transfer-rule-id%))
               (mtr (make-mtr :id id :filter filter :context context
                              :input input :output output :defaults defaults
                              :variables (nreverse mrs::*named-nodes*)
                              :flags flags :special special :rank rank
                              :vector vector)))
          (incf %transfer-rule-id%)
          (when (member :optional (mtr-flags mtr))
            (setf vector (logior vector %transfer-optional-rules%)))
          mtr)))))

(defun convert-constants-to-variables (dag constants generator)
  (loop
      for constant in constants
      for path = (lkb::path-typed-feature-list (lkb::unification-lhs constant))
      for node = (mrs::path-value dag path)
      when node do (mrs::create-variable node generator)))

(defun convert-dag-to-flags (dag)
  #+:debug
  (setf %dag dag)
  (let* ((optional (mrs::path-value dag *mtr-optional-path*))
         (fail (mrs::path-value dag *mtr-fail-path*))
         (exhaustive (mrs::path-value dag *mtr-exhaustive-path*))
         flags)
    (when (lkb::bool-value-true optional)
      (pushnew :optional flags))
    (when (lkb::bool-value-false optional)
      (pushnew :obligatory flags))
    (when (lkb::bool-value-true fail)
      (pushnew :fail flags))
    (when (lkb::bool-value-true exhaustive)
      (pushnew :exhaustive flags))
    flags))

(defun convert-dag-to-rank (dag)
  #+:debug
  (setf %dag dag)
  (when dag
    (let ((rank (mrs::path-value dag *mtr-rank-path*)))
      (when rank
        (ignore-errors (mrs::extract-integer-from-fs-type rank))))))

(defun convert-dag-to-special (dag &key id)
  (let ((equal (lkb::existing-dag-at-end-of dag *mtr-equal-path*))
        (subsume (lkb::existing-dag-at-end-of dag *mtr-subsume-path*))
        (trigger (mrs::path-value dag *mtr-trigger-path*))
        special)
    (when (lkb::dag-p equal)
      (loop
          for i from 0
          for list = equal 
          then (lkb::existing-dag-at-end-of list lkb::*list-tail*)
          for first = (when list
                        (lkb::existing-dag-at-end-of list lkb::*list-head*))
          while (and first (lkb::dag-p first)) do
            (let ((match (rest (assoc first mrs::*all-nodes* :test #'eq))))
              (if match
                (push (cons match :equal) special)
                (format
                 t
                 "~&convert-dag-to-special(): ~
                  no match for EQUAL element # ~a.~%"
                 i)))))
    (when (lkb::dag-p subsume)
      (loop
          for i from 0
          for list = subsume
          then (lkb::existing-dag-at-end-of list lkb::*list-tail*)
          for first = (when list
                        (lkb::existing-dag-at-end-of list lkb::*list-head*))
          while (and first (lkb::dag-p first)) do
            (let ((match (rest (assoc first mrs::*all-nodes* :test #'eq))))
              (if match
                (push (cons match :subsume) special)
                (format
                 t
                 "~&convert-dag-to-special(): ~
                  no match for SUBSUME element # ~a.~%"
                 i)))))
    (when (mrs::is-valid-fs trigger)
      (let ((le (mrs::vsym (mrs::fs-type trigger))))
        (push id (gethash le *transfer-triggers*))
        (push (cons le :trigger) special)))
    special))

(defun vacuous-constraint-p (path dag)
  (let* ((feature (if (consp path) (first (last path)) path))
         (dag (if (lkb::dag-p dag) dag (lkb::create-typed-dag dag)))
         (type (lkb::minimal-type-for feature))
         (constraint (and type (lkb::constraint-of type))))
    (lkb::dag-subsumes-p dag constraint)))

(defun compile-mtr (mtr)
  #-:ppcre
  mrs
  #+:ppcre
  (labels ((compile (mrs)
             (when mrs
               (loop
                   for ep in (mrs:psoa-liszt mrs)
                   for pred = (mrs:rel-pred ep)
                   when (and (stringp pred) (eq (char pred 0) #\~)) do 
                     (setf (mrs:rel-pred ep)
                       (ppcre::create-scanner (subseq pred 1)))
                   do
                     (loop
                         for role in (mrs:rel-flist ep)
                         for value = (mrs:fvpair-value role)
                         when (and (stringp value) (eq (char value 0) #\~)) do
                           (setf (mrs:fvpair-value role)
                             (ppcre::create-scanner (subseq value 1)))
                         when (mrs::var-p value) do
                           (loop
                               for extra in (mrs:var-extra value)
                               for value = (mrs::extrapair-value extra)
                               when (and (stringp value)
                                         (eq (char value 0) #\~))
                               do
                                 (setf (mrs::extrapair-value extra)
                                   (ppcre::create-scanner
                                    (subseq value 1)))))))
             mrs)
           (unfill (mrs)
             (when mrs
               (loop
                   for ep in (mrs:psoa-liszt mrs)
                   for pred = (mrs:rel-pred ep)
                   when (eq pred mrs::*top-semantics-type*)
                   do (setf (mrs:rel-pred ep) nil)))
             mrs))
    (compile (mtr-filter mtr))
    (compile (mtr-input mtr))
    (compile (mtr-context mtr))
    (unfill (mtr-defaults mtr))
    mtr))

(defun record-mtr (mtr)
  (when (and *transfer-lexicon* (mrs::psoa-p (mtr-input mtr)))
    (loop
        with top = (lkb::minimal-type-for (first (last mrs::*rel-name-path*)))
        with source = (first *transfer-lexicon*)
        for ep in (mrs:psoa-liszt (mtr-input mtr))
        for pred = (mrs:rel-pred ep)
        when (and (or (stringp pred) (symbolp pred))
                  (not (lkb::subtype-or-equal top pred)))
        do (push mtr (gethash pred source)))
    (loop
        with top = (lkb::minimal-type-for (first (last mrs::*rel-name-path*)))
        with target = (rest *transfer-lexicon*)
        for oep in (and (mtr-output mtr) (mrs:psoa-liszt (mtr-output mtr)))
        for dep in (and (mtr-defaults mtr) (mrs:psoa-liszt (mtr-defaults mtr)))
        for pred 
        = (or (let ((pred (and dep (mrs:rel-pred dep))))
                (and (or (stringp pred) (symbolp pred))
                     (not (lkb::subtype-or-equal top pred))
                     pred))
              (mrs:rel-pred oep))
        when (and (or (stringp pred) (symbolp pred))
                  (not (lkb::subtype-or-equal top pred)))
        do (push mtr (gethash pred target)))))

;;;
;;; top-level drivers for application of transfer rules; use `edge' structure
;;; to keep track of something reminiscent of a derivation tree.
;;;
(defun initialize-transfer ()
  (setf *vpms* nil)
  (setf *transfer-rule-sets* nil)
  (setf *transfer-triggers* (make-hash-table))
  ;;
  ;; _fix_me_
  ;; make the transfer lexicon a structure, actually.          (19-jul-04; oe)
  ;;
  (setf *transfer-lexicon* 
    (cons (make-hash-table :test #'equal) (make-hash-table :test #'equal)))
  (setf %transfer-rule-id% 0)
  (setf %transfer-generation% 0)
  (setf %transfer-copy-eps% nil)
  (setf %transfer-special-elements% nil)
  (setf %transfer-variable-features% nil))

(defun transfer-mrs (mrs &key (filter *transfer-filter-p*) 
                              (preemptive *transfer-preemptive-filter-p*)
                              (debug '(:chart))
                              task (tm *transfer-tm*))
  #+:debug
  (setf %mrs% mrs)
  (setf %transfer-edges% nil)
  (setf %transfer-chart% nil)
  (let* ((*transfer-filter-p* filter)
         (*transfer-preemptive-filter-p* preemptive)
         (*transfer-debug-p* (when (consp debug) debug))
         (%transfer-edge-id% 0)
         (%mrs-copy-cache% nil)
         (%transfer-original-variables% nil)
         (mrs (clone-mrs mrs :skolemizep t))
         (n (loop
                for variable in %mrs-copy-cache%
                maximize (or (mrs:var-id (first variable)) 0)
                do (push (rest variable) %transfer-original-variables%))))
    (multiple-value-bind (result condition)
        (#-:debug ignore-errors #+:debug progn
         (transfer-mrs2 
          (list (make-edge :mrs mrs :n n)) 
          (loop
              for mtrs in *transfer-rule-sets*
              for foo = (mtrs-task mtrs)
              when (or (null task) (null foo) (eq task foo))
              collect mtrs)))
      (when condition
        (unless debug (error condition))
        #+:clim
        (clim:beep)
        (format
         *transfer-debug-stream*
         "~&transfer-mrs(): `~a'~%" condition))

      #+:lm
      (when tm
        (let* ((mrss (loop for edge in result collect (edge-mrs edge)))
               (scores (ignore-errors (tm-score-mrss mrss :model tm))))
          (loop
              for edge in result
              for score in scores
              when (numberp (rest score))
              do (setf (edge-score edge) (- (rest score))))
          (setf result (sort result #'> :key #'edge-score))))

      (when *transfer-postprocess-p*
        (setf result
          (loop
              for edge in result
              for clone = (copy-edge edge)
              do (setf (edge-mrs clone) (postprocess-mrs (edge-mrs edge)))
              collect clone)))

      result)))

(defun transfer-mrs2 (edges mtrss)
  (if (null mtrss)
    edges
    (transfer-mrs2 
     (let* ((mtrs (first mtrss))
            (edges (loop for edge in edges append (apply-mtrs edge mtrs))))
       ;;
       ;; as we advance from one MTR set to the next, apply the output VPM and
       ;; after hook, if any; ditch intermediate solutions for which there are
       ;; problems in VPM- or post-processing.
       ;;
       (when (mtrs-out mtrs)
         (loop
             for i from 0
             for edge in edges
             do
               (handler-case
                   (setf (edge-mrs edge)
                     (map-mrs
                      (edge-mrs edge) (mtrs-out mtrs)
                      :forward :skolemizep t))
                 (condition (condition)
                   (format
                    *transfer-debug-stream*
                    "~&transfer-mrs2(): [~a] `~a'.~%"
                    i condition)
                   (setf (edge-mrs edge) nil)))))
       (when (mtrs-after mtrs)
         (loop
             for i from 0
             for edge in edges
             when (edge-mrs edge) do
               (handler-case
                   (setf (edge-mrs edge)
                     (funcall (mtrs-after mtrs) (edge-mrs edge)))
                 (condition (condition)
                   (format
                    *transfer-debug-stream*
                    "~&transfer-mrs2(): [~a] `~a'.~%"
                    i condition)
                   (setf (edge-mrs edge) nil)))))
       (loop for edge in edges when (edge-mrs edge) collect edge))
     (rest mtrss))))

(defun equivalentp (edge1 edge2)
  (let* ((mask (lognot %transfer-optional-rules%))
         (vector1 (logand (edge-vector edge1) mask))
         (vector2 (logand (edge-vector edge2) mask))
         (mrs1 (edge-mrs edge1))
         (mrs2 (edge-mrs edge2)))
    (and (= (edge-depth edge1) (edge-depth edge2))
         (= (logior vector2 vector1) vector2)
         (mrs= mrs1 mrs2 :properties (list *mtr-skolem-property*)))))

(defun packed-edge-p (edge)
  (loop
      for old in %transfer-chart%
      thereis (unless (eq edge old) (equivalentp edge old))))

(defun apply-mtrs (edge mtrs &key filter)

  (when (or (null edge) (null mtrs)) (return-from apply-mtrs))
  
  (push edge %transfer-chart%)
  (when (mtrs-before mtrs)
    (handler-case
        (setf (edge-mrs edge) (funcall (mtrs-before mtrs) (edge-mrs edge)))
      (condition (condition)
        (format *transfer-debug-stream* "~&apply-mtrs(): `~a'.~%" condition)
        (return-from apply-mtrs))))
  (when (mtrs-in mtrs)
    (handler-case
        (setf (edge-mrs edge)
          (map-mrs (edge-mrs edge) (mtrs-in mtrs) :forward :skolemizep t))
      (condition (condition)
        (format *transfer-debug-stream* "~&apply-mtrs(): `~a'.~%" condition)
        (return-from apply-mtrs))))
  
  (loop
      with result
      with *transfer-edge-limit* = (or (mtrs-edges mtrs) *transfer-edge-limit*)
      with filter = (or filter (mtrs-filter-p mtrs))
      with recurse = (mtrs-recurse-p mtrs)
      with subsume = (mtrs-subsume-p mtrs)
      with agenda = (list edge)
      with all = (loop
                     for mtr in (mtrs-mtrs mtrs)
                     for rank = (mtr-rank mtr)
                     when (or (null *transfer-maximum-rank*)
                              (null rank)
                              (not (member :optional (mtr-flags mtr)))
                              (<= rank *transfer-maximum-rank*))
                     collect mtr)
      initially
        (when (and filter *transfer-preemptive-filter-p*)
          (loop
              with unknown
              with source = (first *transfer-lexicon*)
              for ep in (mrs:psoa-liszt (edge-mrs edge))
              for pred = (mrs:rel-pred ep)
              unless (or (gethash pred source)
                         (interlingua-predicate-p pred))
              do (push ep unknown)
              finally
                (when unknown
                  (error
                   "unknown transfer predicates: ~{|~a|~^, ~}"
                   (loop
                       for ep in unknown
                       collect (mrs::ep-shorthand ep))))))
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
            while resultp
            for edges = (apply-mtr task mtr :subsumesp subsume)
            when (and edges (not optionalp) recurse) do (setf resultp nil)
            do (loop 
                   for edge in edges 
                   unless (and recurse (packed-edge-p edge))
                   do 
                     #+:debug
                     (format
                      *transfer-debug-stream*
                      "apply-mtrs(): >> ~a~%"
                      edge)
                     (push edge %transfer-chart%)
                     (if recurse
                       (push edge agenda)
                       (push edge result)))
            finally 
              (when (and resultp recurse)
                #+:debug
                (format
                 *transfer-debug-stream*
                 "apply-mtrs(): == ~a~%"
                 task)
                (push task result)))
      finally 
        (if filter
          (return
            (stable-sort 
             (loop
                 with eps = (loop
                                for ep in (mrs:psoa-liszt (edge-mrs edge))
                                unless (interlingua-predicate-p
                                        (mrs::rel-pred ep))
                                collect ep)
                 for edge in result
                 for source = (intersection 
                               (mrs:psoa-liszt (edge-mrs edge)) eps 
                               :key #'mrs::rel-pred :test #'equal)
                 do
                   (loop
                       for foo in source
                       do (pushnew foo (edge-source edge)))
                 unless (and source *transfer-filter-p*) collect edge)
             #'(lambda (foo bar)
                 (if (or (and (null (edge-source foo)) 
                              (null (edge-source bar)))
                         (and (edge-source foo) (edge-source bar)))
                   (> (edge-depth foo) (edge-depth bar))
                   (null (edge-source foo))))))
          ;;
          ;; sort resulting derivations by `edit distance' to the original MRS,
          ;; measured in terms of the number of MTRs that we have applied.
          ;;
          (return (stable-sort result #'> :key #'edge-depth)))))

(defun apply-mtr (edge mtr &key subsumesp)
  (loop
      with %transfer-variable-id% = (edge-n edge)
      for solution in (unify-mtr (edge-mrs edge) mtr :subsumesp subsumesp)
      for mrs = (expand-solution (edge-mrs edge) mtr solution)
      when mrs
      collect (make-edge 
               :rule mtr :mrs mrs :daughter edge 
               :solution solution :n %transfer-variable-id%)))

(defparameter %transfer-trace-p% nil)

(defparameter %transfer-trace-context% nil)

(defparameter %transfer-trace-failure% nil)

(defmacro transfer-trace (type value)
  `(setf (getf %transfer-trace-context% ,type) ,value))

(defmacro current-mtr ()
  `(getf %transfer-trace-context% :mtr))

(defmacro current-component ()
  `(getf %transfer-trace-context% :component))

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
;;; complete tracing { and | or } failure accumulation.        (22-jan-04; oe)
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
(defun unify-mtr (mrs mtr &key subsumesp)
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
    (transfer-trace :component :context)
    (setf solutions (unify-mtr-component mrs context nil :subsumesp subsumesp))
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
            append (unify-mtr-component
                    mrs input solution :subsumesp subsumesp)))
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
            nconc (unify-hconss hcons1 hcons2 solution :subsumesp subsumesp)))
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
            nconc (unify-hconss hcons1 hcons2 solution :subsumesp subsumesp)))
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
    (when filter
      (transfer-trace :component :filter)
      (setf solutions
        (loop
            for solution in solutions
            unless (unify-mtr-component
                    mrs filter solution :subsumesp subsumesp)
            collect solution))
      #+:debug
      (format 
       t 
       "unify-mtr(): ~a solution~p for FILTER component HCONS.~%"
       (length solutions) (length solutions)))
    solutions))

(defun unify-mtr-component (mrs1 mrs2 &optional solution 
                            &key (disjointp t) subsumesp)
  (if (null mrs2)
    (list solution)
    (let* ((solution (copy-solution solution))
           (top1 (mrs:psoa-top-h mrs1))
           (top2 (mrs:psoa-top-h mrs2)))
      (transfer-trace :part :top)
      (unless (or (null mrs::*rel-handel-path*)
                  (unify-values top1 top2 solution :subsumesp subsumesp))
        ;;
        ;; trace
        ;;
        (return-from unify-mtr-component))
      (let* ((index1 (mrs:psoa-index mrs1))
             (index2 (mrs:psoa-index mrs2)))
        (transfer-trace :part :index)
        (unless (unify-values index1 index2 solution :subsumesp subsumesp)
          ;;
          ;; trace
          ;;
          (return-from unify-mtr-component)))
      (let* ((eps1 (mrs:psoa-liszt mrs1))
             (eps2 (mrs:psoa-liszt mrs2))
             (solutions (unify-epss
                         eps1 eps2 solution
                         :disjointp disjointp :subsumesp subsumesp)))
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
                    append (unify-hconss
                            hcons1 hcons2 solution
                            :subsumesp subsumesp))))
          (unless solutions
            ;;
            ;; trace
            ;;
            (return-from unify-mtr-component))
          solutions)))))

(defun unify-epss (eps1 eps2 solution &key (disjointp t) subsumesp)
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
                  for result = (unify-eps 
                                ep1 ep2 solution
                                :disjointp disjointp :subsumesp subsumesp)
                  when result collect result)))
        (when solutions
          (loop
              for solution in solutions
              nconc (unify-epss 
                     eps1 (rest eps2) solution
                     :disjointp disjointp :subsumesp subsumesp)))))))

;;;
;;; in unifying two sets of EPs, i believe we are assuming that no EP can be
;;; unified with more than one counterpart (which intuitively seems to make a
;;; lot of sense); it means there cannot be `overlap' between the CONTEXT and
;;; INPUT parts of a rule, which is fine since bindings are accumulated from
;;; both, so there should be no need to have a duplicate EP (targetting just
;;; one EP in the input MRS) spread out over those two.       (23-oct-03; oe)
;;;
(defun unify-eps (ep1 ep2 solution &key (disjointp t) subsumesp)

  (transfer-trace :part (cons ep1 ep2))
  (unless (when disjointp 
            (or (retrieve-ep ep1 solution) (retrieve-ep ep2 solution)))
    (let* ((solution (copy-solution solution))
           (pred (unify-preds 
                  (mrs::rel-pred ep1) (mrs::rel-pred ep2) solution
                  :subsumesp subsumesp))
           (label (when pred
                    (unify-values 
                     (mrs:rel-handel ep1) (mrs:rel-handel ep2) solution
                     :subsumesp subsumesp))))
      (when (and pred (or (null mrs::*rel-handel-path*) label))
        (loop
            with flist1 = (mrs:rel-flist ep1)
            with flist2 = (mrs:rel-flist ep2)
            with intersection 
            = (let ((foo (intersect flist1 flist2 :key #'mrs:fvpair-feature)))
                ;;
                ;; in subsumption mode, make sure all roles from the second EP
                ;; (originally part of an MTR) are present in the target EP.
                ;;
                (when subsumesp
                  (loop
                      for role in flist2
                      for feature = (mrs:fvpair-feature role)
                      unless (find feature flist1 :key #'mrs:fvpair-feature)
                      do (return-from unify-eps)))
                foo)
            for role in intersection
            for feature = (mrs:fvpair-feature role)
            for role1 = (find feature flist1 :key #'mrs:fvpair-feature)
            for role2 = (find feature flist2 :key #'mrs:fvpair-feature)
            unless (unify-values 
                    (mrs:fvpair-value role1) (mrs:fvpair-value role2) solution
                    :subsumesp subsumesp)
            do (return-from unify-eps))
        
        ;;
        ;; now also check to make sure all MTR variables that require `special'
        ;; treatment (matching against a specific value from the input MRS) got
        ;; bound, as in the above we only processed the intersection of roles.
        ;;
        (loop
            with special = (mtr-special (current-mtr))
            for role in (mrs:rel-flist ep2)
            for variable = (mrs:fvpair-value role)
            when (and (mrs::var-p variable)
                      (loop
                          for (value . key) in special
                          when (and (eq variable value)
                                    (member key '(:equal :subsume)))
                          return key)
                      (eq (retrieve-variable variable solution) variable))
            return nil
            finally
              (align-eps ep1 ep2 solution)
              (return solution))))))

(defun unify-hconss (hconss1 hconss2 solution &key subsumesp)
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
                  for result = (unify-hcons
                                hcons1 hcons2 solution :subsumesp subsumesp)
                  when result collect result)))
        (when solutions
          (loop
              for solution in solutions
              nconc (unify-hconss
                     hconss1 (rest hconss2) solution :subsumesp subsumesp)))))
    (unless hconss2 (list solution))))

(defun unify-hcons (hcons1 hcons2 solution &key subsumesp)

  ;;
  ;; okay, here is the current (22-jan-04) rationale about HCONS unification;
  ;; all handles in HCONS must occur somewhere else in the input MRS (this is
  ;; enforced by the MRS reader); in the MTR, HCONS elements that do not link
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
                solution :strictp t :subsumesp subsumesp))
         (larg (when harg
                 (unify-values
                  (mrs:hcons-outscpd hcons1) (mrs:hcons-outscpd hcons2)
                  solution :strictp t :subsumesp subsumesp))))
    (when larg 
      (align-hconss hcons1 hcons2 solution)
      solution)))

(defun unify-values (variable1 variable2 solution &key strictp subsumesp)

  (let* ((value1 (if (mrs::var-p variable1) 
                   (retrieve-variable variable1 solution)
                   variable1))
         (forwardp1 (not (eq variable1 value1)))
         (value2 (if (mrs::var-p variable2)
                   (retrieve-variable variable2 solution)
                   variable2))
         (forwardp2 (not (eq variable2 value2)))
         (special (loop
                      for special in (mtr-special (current-mtr))
                      when (eq variable2 (first special))
                      return (rest special))))
    (cond
     ((or (eq value1 value2) (null value2))
      value1)
     ((and (numberp value1) (numberp value2))
      (= value1 value2))
     #+:ppcre
     ((functionp variable2)
      (ppcre::scan 
       variable2
       (if (symbolp value1) (format nil "~(~a~)" value1) value1)))
     
     ;;
     ;; _fix_me_
     ;; why this special case?  should be subsumed by unify-types(), i suppose.
     ;;                                                        (24-jan-04; oe)
     ((and (stringp value1) (stringp value2))
      (string-equal value1 value2))
     ((and (or (stringp value1) (symbolp value1))
           (or (stringp value2) (symbolp value2)))
      (unify-types value1 value2 :special special :subsumesp subsumesp))
     ((and (null value1)
           (not subsumesp) (member special '(:equal :subsume) :test #'eq))
      value2)
     ((and (not (mrs::var-p value1)) (mrs::var-p value2))
      (forward-variable value2 value1 solution)
      value1)
     ((and strictp (or forwardp1 forwardp2)) nil)
     (t
      (let* ((type (unify-types 
                    (mrs::var-type value1) (mrs::var-type value2) 
                    :internp t :special special :subsumesp subsumesp))
             (extras (unify-extras
                      (mrs:var-extra value1) (mrs:var-extra value2)
                      :subsumesp subsumesp))
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

(defun unify-types (type1 type2 &key internp special subsumesp)

  (let* ((type1 (if internp (intern (string-upcase type1) :lkb) type1))
         (type2 (if internp (intern (string-upcase type2) :lkb) type2))
         (glb (cond
                ((eq special :equal) (and (eq type1 type2) type1))
                ((or (eq special :subsume) subsumesp)
                 (and (lkb::subtype-or-equal type1 type2) type1))
                (t               
                 (and (lkb::is-valid-type type1) (lkb::is-valid-type type2)
                      (lkb::greatest-common-subtype type1 type2))))))
    (when glb (if internp (string-downcase (string glb)) glb))))

(defun unify-extras (extras1 extras2 &key subsumesp)

  (when subsumesp
    (loop
        for extra in extras2
        for feature = (mrs::extrapair-feature extra)
        unless (find feature extras1 :key #'mrs::extrapair-feature :test #'eq)
        do (return-from unify-extras :fail)))
  
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
                            (unify-types value1 value2 :subsumesp subsumesp))
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

(defun unify-preds (pred1 pred2 solution &key subsumesp)
  (declare (ignore subsumesp))
  
  ;;
  ;; _fix_me_
  ;; what about PREDs that stand in a subsumption relation?  presumably, we
  ;; also need to record the result somewhere, or return it?    (8-jan-04; oe)
  ;;
  (let ((pred2 (if (mrs::var-p pred2) 
                 (retrieve-variable pred2 solution)
                 pred2)))

    (cond
     #+:ppcre
     ((functionp pred2)
      (ppcre::scan 
       pred2
       (if (symbolp pred1) (format nil "~(~a~)" pred1) pred1)))
     ((mrs::var-p pred2)
      (forward-variable pred2 pred1 solution)
      pred1)
     ((and (stringp pred1) (stringp pred2))
      (string-equal pred1 pred2))
     ((or (eq pred1 pred2) 
          ;;
          ;; _fix_me_
          ;; half of the solution to the concern expressed above, picked up
          ;; from dan; make sure the caller records the new value.
          ;;                                                    (4-apr-04; oe)
          (unify-types pred1 pred2)
          (null pred2))))))

(defun expand-solution (mrs mtr solution)
  ;;
  ;; go through EPs from .mrs., ditching those that were aligned with one from
  ;; .mtr. during unification; then, through in EPs from .mtr. OUTPUT part and
  ;; unify in all applicable information from .solution.  eventually, do more
  ;; or less the same for HCONS.
  ;;
  (when (mtr-exhaustive-p mtr)
    (loop
        for ep in (mrs:psoa-liszt mrs)
        unless (retrieve-ep ep solution)
        do (return-from expand-solution)))
  
  (let* ((output (mtr-output mtr))
         ;;
         ;; _fix_me_
         ;; LTOP and INDEX we used to take from the input MRS, which seems just
         ;; wrong; being a tad nervuous about this as a last-minute change for
         ;; the 31-aug integration, keep the original code around for now.
         ;;                                                     (31-aug-04; oe)
         (top (or (and output (mrs:psoa-top-h output)
                       (expand-value (mrs:psoa-top-h output) solution))
                  (when (mrs:psoa-top-h mrs) 
                    (expand-value (mrs:psoa-top-h mrs) solution)))
              #+:null
              (when (mrs:psoa-top-h mrs) 
                (expand-value (mrs:psoa-top-h mrs) solution)))
         (index (or (and output (mrs:psoa-index output) 
                         (expand-value (mrs:psoa-index output) solution))
                   (when (mrs:psoa-index mrs) 
                     (expand-value (mrs:psoa-index mrs) solution)) )
                #+:null
                (when (mrs:psoa-index mrs) 
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
           with input = (and (mtr-input mtr) (mrs:psoa-liszt (mtr-input mtr)))
           with defaults = (when (mrs::psoa-p (mtr-defaults mtr))
                             (mrs:psoa-liszt (mtr-defaults mtr)))
           for ep in (when (mtr-output mtr) 
                       (mrs:psoa-liszt (mtr-output mtr)))
           for i from 0
           for copyp = (member ep %transfer-copy-eps% :test #'eq)
           for original = (when copyp
                            (let ((ep1 (nth i input)))
                              (and ep1 (retrieve-ep ep1 solution))))
           for default = (pop defaults)
           when (and copyp (null original))
           do
             (format
              t
              "expand-solution(): unmatched +copy+ EP # ~a in `~(~a~)'.~%"
              i (mtr-id mtr))
           else when copyp
           collect
             ;;
             ;; _fix_me_
             ;; for the time being, we disallow variable bindings in +copy+
             ;; OUTPUT EPs, so that we can assert that all information is in
             ;; the DEFAULTS component.                        (15-feb-04; oe)
             ;;
             (merge-eps (expand-ep original solution) default)
           else collect (merge-eps (expand-ep ep solution) default))))
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
    (eliminate-scratch result)))

(defun expand-ep (ep solution)
  ;;
  ;; at this stage, we expand variable bindings and should make sure to _not_
  ;; share any structure with either the input MRS or MTR used.
  ;;
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
          when value 
          collect (mrs::make-fvpair :feature feature :value value)))
    result))

(defun expand-value (value solution)
  (if (mrs::var-p value)
    (let ((foo (retrieve-variable value solution)))
      (unless (variable-delete-p foo)
        (if (and (mrs::var-p foo) 
                 (or (member foo %transfer-original-variables% :test #'eq)
                     (< (mrs::var-id foo) 0)))
          (let ((new (new-variable (mrs::var-type foo) (mrs::var-extra foo))))
            (forward-variable foo new solution)
            new)
          foo)))
    value))

(defun merge-eps (ep default)
  ;;
  ;; apparently, we assume we can destructively modify .ep. at this point.  we
  ;; better make sure it not share any variables with the original ...
  ;;
  #+:debug
  (setf %ep ep %default default)
  (unless default (return-from merge-eps ep))
  (when (mrs::rel-pred default)
    (setf (mrs::rel-pred ep) (mrs::rel-pred default)))
  (loop
      with defaults = (mrs:rel-flist default)
      with used
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
        (push default used)
        (let ((default (mrs:fvpair-value default)))
          (if (and (mrs::var-p value) (mrs::var-p default))
            (merge-values value default)
            (setf (mrs:fvpair-value role) 
              (if (mtr-operator-p default) 
                (merge-values value default)
                (if (or (eq default lkb:*toptype*)
                        (eq default lkb::*string-type*))
                  value
                  default)))))
      else when default do
        ;;
        ;; _fix_me_
        ;; this seems wrong: just copy in the actual `value' part from the MTR;
        ;; i suspect we should (a) copy and (b) ensure variable uniqueness.
        ;;                                                     (19-feb-04; oe)
        (push default used)
        (push (mrs::copy-fvpair default) (mrs:rel-flist ep))
      finally
        (loop
            for default in defaults
            for value = (let ((foo (mrs:fvpair-value default)))
                          (if (mrs::var-p foo)
                            (new-variable
                             (mrs::var-type foo) (mrs::var-extra foo))
                            foo))
            unless (member default used :test #'eq) do
              (push 
               (mrs::make-fvpair 
                :feature (mrs:fvpair-feature default) :value value)
               (mrs:rel-flist ep))))
  ep)

(defun mtr-operator-p (value)
  (or (eq value *mtr-upcase-operator*) (eq value *mtr-downcase-operator*)))

(defun role-delete-p (role value)
  (declare (ignore value))
  (member role mrs::*ignored-sem-features* :test #'eq))

(defun variable-delete-p (variable)
  (when (mrs::var-p variable)
    (let ((properties (mrs:var-extra variable)))
      (loop
          for property in properties
          thereis (and (eq (mrs::extrapair-feature property)
                           *mtr-ditch-property*)
                       (eq (mrs::extrapair-value property) 
                           *mtr-true-type*))))))

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
  (when (mtr-operator-p default)
    (return-from merge-values
      (cond
       ((and (stringp variable) (eq default *mtr-upcase-operator*))
        (string-upcase variable))
       ((and (stringp variable) (eq default *mtr-downcase-operator*))
        (string-downcase variable))
       (t variable))))
  (when (and (mrs::var-type default)
             (or (null (mrs::var-type variable))
                 (not (member 
                       (mrs::var-type default)
                       (list *semi-u-type* *semi-i-type*)
                       :test #'string-equal))))
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
          with used
          for extra in (mrs:var-extra variable)
          for feature = (mrs::extrapair-feature extra)
          for default = (let ((default (find feature defaults 
                                             :key #'mrs::extrapair-feature)))
                          (when default 
                            (push default used)
                            (mrs::extrapair-value default)))
          when (or (null default) (vacuous-constraint-p feature default))
          collect (mrs::make-extrapair 
                   :feature feature :value (mrs::extrapair-value extra))
          into result
          else 
          collect (mrs::make-extrapair :feature feature :value default)
          into result
          finally
            (loop
                for default in defaults
                unless (member default used :test #'eq)
                do (push (mrs::copy-extrapair default) result))
            (return result))))
  variable)

(defun new-variable (type &optional extras)
  (let* ((extras (remove
                  *mtr-skolem-property* extras
                  :key #'mrs::extrapair-feature))
         (variable (mrs::make-var 
                    :id %transfer-variable-id% :type type :extra extras)))
    (incf %transfer-variable-id%)
    ;;
    ;; _fix_me_
    ;; work out why *transfer-skolemize-p* is not on (by default) during core
    ;; transfer; preferably move to re-skolemization after each MTR expansion.
    ;;                                                          (30-jun-04; oe)
    (when #+:null (null *transfer-skolemize-p*) #-:null t
      (push 
       (mrs::make-extrapair 
        :feature *mtr-skolem-property*
        :value (mrs::var-string variable))
       (mrs:var-extra variable)))
    variable))

(defun constant-role-p (role)
  (member role mrs::*value-feats* :test #'eq))

(defun eliminate-scratch (mrs)
  (labels ((process-variable (variable)
             (when (mrs::var-p variable)
               (setf (mrs:var-extra variable)
                 (loop
                     for extra in (mrs:var-extra variable)
                     for feature = (mrs::extrapair-feature extra)
                     unless (eq feature *mtr-scratch-property*)
                     collect extra)))
             variable))
    (process-variable (mrs:psoa-top-h mrs))
    (process-variable (mrs:psoa-index mrs))
    (loop
        for ep in (mrs:psoa-liszt mrs)
        do
          (process-variable (mrs:rel-handel ep))
          (loop
              for role in (mrs:rel-flist ep)
              for value = (mrs:fvpair-value role)
              do (process-variable value)))
    mrs))

;;;
;;; _fix_me_
;;; as it stands, the unification ends up sharing variables with the original
;;; input MRS, hence postprocess-mrs() is required to create copies at this
;;; stage, to avoid frobbing the original.                    (3-nov-03; oe)
;;;
(defun postprocess-mrs (mrs)
  (let ((mrs (mrs::copy-psoa mrs))
        copies)
    (labels ((postprocess-variable (variable)
               (when variable
                 (or (rest (assoc variable copies))
                     (let ((copy (mrs::copy-var variable)))
                       (setf (mrs:var-extra copy)
                         (loop
                             for extra in (mrs:var-extra variable)
                             for feature = (mrs::extrapair-feature extra)
                             unless (or (eq feature *mtr-skolem-property*)
                                        (eq feature *mtr-mark-property*)
                                        (eq feature *mtr-scratch-property*)
                                        (eq feature *mtr-ditch-property*))
                             collect extra))
                       (push (cons variable copy) copies)
                       copy)))))
      (setf (mrs:psoa-top-h mrs) (postprocess-variable (mrs:psoa-top-h mrs)))
      (setf (mrs:psoa-index mrs) (postprocess-variable (mrs:psoa-index mrs)))
      (setf (mrs:psoa-liszt mrs)
        (loop
            for ep in (mrs:psoa-liszt mrs)
            for copy = (mrs::copy-rel ep)
            do
              (setf (mrs:rel-handel copy) 
                (postprocess-variable (mrs:rel-handel ep)))
              (setf (mrs::rel-flist copy)
                (loop
                    for role in (mrs:rel-flist ep)
                    for value = (mrs:fvpair-value role)
                    unless (or (role-delete-p (mrs:fvpair-feature role) value)
                               (and (mrs::var-p value)
                                    (variable-delete-p value)))
                    collect 
                      (mrs::make-fvpair 
                       :feature (mrs:fvpair-feature role)
                       :value (if (mrs::var-p value) 
                                (postprocess-variable value)
                                value))))
            collect copy))
      (setf (mrs:psoa-h-cons mrs)
        (loop
            for hcons in (mrs:psoa-h-cons mrs)
            for copy = (mrs::make-hcons :relation (mrs:hcons-relation hcons))
            do
              (setf (mrs:hcons-scarg copy)
                (postprocess-variable (mrs:hcons-scarg hcons)))
              (setf (mrs:hcons-outscpd copy)
                (postprocess-variable (mrs:hcons-outscpd hcons)))
            collect copy))
      mrs)))

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

(defun read-derivation-from-string (string)
  (let ((*readtable* (copy-readtable))
        (*package* (find-package :lkb)))
    (set-syntax-from-char #\] #\space *readtable*)
    (set-syntax-from-char #\} #\space *readtable*)
    (with-input-from-string (stream string)
      (read-derivation stream))))

(defun read-derivation (stream)
  (let* ((c (peek-char t stream nil nil))
         (c (when (and c (char= c #\#))
              (read-char stream)
              (peek-char t stream nil nil)))
         (c (when (and c (char= c #\D))
              (read-char stream)
              (peek-char t stream nil nil)))
         (id (when (and c (char= c #\[))
               (read-char stream)
               (read stream nil nil)))
         (score (let ((c (peek-char t stream nil nil)))
                  (when (and c (char= c #\{))
                    (read-char stream)
                    (read stream nil nil))))
         (mtr (when (numberp id)
                (read stream nil nil)))
         (daughter (when (and mtr (symbolp mtr))
                     (read-derivation stream))))
    (when id (make-edge :id id :score score :rule mtr :daughter daughter))))

(defun browse (object)
  (typecase object
    (edge (mrs::browse-mrs (edge-mrs object)))
    (mrs::psoa (mrs::browse-mrs object))))
