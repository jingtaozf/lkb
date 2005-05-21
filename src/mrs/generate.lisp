;;; Copyright (c) 1998--2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :lkb)


;;; Control parameters

(defparameter *gen-packing-p* t)
(defparameter *gen-filtering-p* t)
(defparameter *bypass-equality-check* nil)
(defparameter *gen-equate-qeqs-p* nil)

(defparameter *gen-scoring-hook* nil)
(defparameter *gen-filtering-debug* nil)
(defparameter *gen-adjunction-debug* nil)
(defparameter *gen-equality-debug* nil)


(defvar *gen-chart* nil)
(defvar *gen-record* nil)
(defvar *gen-rel-indexes* nil)
(defvar *lexemes-allowed-orderings* nil)
(defvar *generator-input* nil)
(defvar *non-intersective-rules* nil)

(defparameter %generator-lexical-items% nil)
(defparameter %generator-unknown-eps% nil)
(defparameter %generator-statistics% nil)

;;;
;;; given all the new parameters we have just introduced, attempt to make sure
;;; things are set up coherently.
;;;
(defun check-generator-environment ()
  (when mrs::*instloc-path*
    (let* ((feature (first (last mrs::*instloc-path*)))
           (type (minimal-type-for feature)))
      (unless (or (equal type *string-type*)
                  (subtype-p *string-type* type))
        (format
         t
         "~%Error: the constraint of feature `~a' must be equal to, or be a supertype of, ~
            type `~(~a~)' (the *string-type* parameter) ."
         feature *string-type*)
        (force-output t)
        (return-from check-generator-environment :error))))
  (when (and *intersective-rule-names* *gen-filtering-p*)
    (format 
     t
     "~%Warning: intersective rules and variable accessibility filtering ~
        are incompatible - the generator will ignore intersective rules.")
    (setf *intersective-rule-names* nil))
  (when (and (null *intersective-rule-names*) (null *gen-filtering-p*))
    (format t "~%Warning: filtering off and no intersective rules specified"))
  (force-output t))


;;; Utility functions for initialising and building daughters and leaves
;;; fields in active chart edges

(defun gen-make-list-and-insert (len item index)
  ;; make list of length len and insert item at index (1-based)
  (let ((lst (make-list len)))
     (setf (nth (1- index) lst) item)
     lst))

(defun gen-copy-list-and-insert (lst item index)
  ;; non-destructively replace index-th element of lst by item
  (if (eql index 0)
     (cons item (cdr lst))
     (cons (car lst) (gen-copy-list-and-insert (cdr lst) item (1- index)))))


;;; Functions on sets (of MRS relations) represented as integers

(defmacro gen-chart-set-disjoint-p (r1 r2)
   `(not (logtest ,r1 ,r2)))

(defmacro gen-chart-set-equal-p (r1 r2)
   `(eql ,r1 ,r2))

(defun gen-chart-subset-p (r1 r2)
   ;; is r1 a strict subset or equal to r2
   (eql (logior r1 r2) r2))

(defun gen-chart-set-difference (r1 r2)
   (logand r1 (lognot r2)))

(defun gen-chart-set-union (r1 r2)
   (if r1 (logior r1 r2) r2))

(defun gen-chart-set-non-empty-p (r)
   (not (zerop r)))


(defun gen-chart-ordering-allowed-p (left right)
  ;; ordering will already have been checked within both left and right
  (flet ((lexeme-set-intersection-p (lst1 lst2)
           (dolist (x1 lst1 nil) 
             (when (member x1 lst2 :test #'eq) (return t)))))
     (dolist (ordering *lexemes-allowed-orderings* t)
        (do ((rtail (cdr ordering) (cdr rtail)))
           ((null rtail))
           (when (lexeme-set-intersection-p left (car rtail))
              (do ((ltail ordering (cdr ltail)))
                 ((eq ltail rtail))
                 (when (lexeme-set-intersection-p right (car ltail))
                    ;; (print (list (mapcar #'mrs::found-lex-lex-id left) 
                    ;;              (mapcar #'mrs::found-lex-lex-id right)))
                    (return-from gen-chart-ordering-allowed-p nil)))
              ;; this ordering constraint can't contain another set of
              ;; lexemes matching left, so go on and try next constraint
              (return-from nil))))))


;;; Find semantic variables in a dag that are externally visible. For
;;; reentrant dags this does unnecessary work, but this doesn't show
;;; in profiling, so there's no need to complicate things.
;;; No point in collecting variables if *gen-filtering-p* is false.

(defun collect-semantic-variables-in-fs (tdfs)
   ;; look at the end of every instloc for an instantiated string and extract
   ;; its number as an integer
   (labels
      ((sem-vars-in-dag (dag vars)
          (let ((d (existing-dag-at-end-of dag mrs::*instloc-path*)))
             (if (and d (stringp (dag-type d)))
                (let* ((type-name (dag-type d))
                       (s (position-if #'digit-char-p type-name))
                       (e (and s (position-if-not #'digit-char-p type-name :start s))))
                      (when s
                         (pushnew
                            (parse-integer type-name
                               :start s :end (or e (length type-name)))
                            vars)))
               (dolist (arc (dag-arcs dag))
                 ;;
                 ;; when we are not packing, all variables would seem to remain
                 ;; accessible in the FS reconstruction of the MRS, i.e. below
                 ;; RELS and HCONS; thus, simulate FS restriction here.
                 ;;                                              (9-jan-05; oe)
                 ;; _fix_me_
                 ;; think more about this: an erroneously set restrictor could
                 ;; cause people surprising results; if they were not packing
                 ;; anyway, why should they care about the restrictor?
                 ;;                                             (20-mar-05; oe)
                 (unless (or *gen-packing-p*
                             (smember 
                              (dag-arc-attribute arc) *packing-restrictor*))
                   (setq vars (sem-vars-in-dag (dag-arc-value arc) vars)))))
             vars)))
      (if *gen-filtering-p*
         (sem-vars-in-dag (tdfs-indef tdfs) nil)
         nil)))

#|
(defun collect-semantic-variables-in-fs (tdfs)
   ;; only look for variables in expected places - this puts constraints on
   ;; the grammar which if not satisfied can lead to strings not being
   ;; generated
   (flet
      ((sem-vars-at-end-of (dag path vars)
          (when path
             (let ((d (existing-dag-at-end-of dag (append path '(instloc))))) ; ***
                (when (and d (stringp (dag-type d)))
                   (let* ((type-name (dag-type d))
                          (s (position-if #'digit-char-p type-name))
                          (e (and s (position-if-not #'digit-char-p type-name :start s))))
                      (when s
                         (pushnew
                            (parse-integer type-name
                               :start s :end (or e (length type-name)))
                            vars))))))
          vars))
      (if *gen-filtering-p*
         (let ((vars nil))
            (when mrs::*initial-semantics-path*
               (let ((d (existing-dag-at-end-of
                            (tdfs-indef tdfs) mrs::*initial-semantics-path*)))
                  (when d
                     (setq vars
                        (sem-vars-at-end-of d mrs::*psoa-top-h-path*
                           (sem-vars-at-end-of d mrs::*psoa-index-path*
                              (sem-vars-at-end-of d mrs::*psoa-xarg-path* vars)))))))
            (when mrs::*slash-semantics-path*
               (let ((d (existing-dag-at-end-of
                            (tdfs-indef tdfs) mrs::*slash-semantics-path*)))
                  (when d
                     (setq vars
                        (sem-vars-at-end-of d mrs::*psoa-top-h-path*
                           (sem-vars-at-end-of d mrs::*psoa-index-path*
                              (sem-vars-at-end-of d mrs::*psoa-xarg-path* vars)))))))
            vars)
         nil)))
|#


(defun merge-new-inaccessible (new-accessible old-accessible old-inaccessible)
   ;; compute new inaccessible list to include variables that used to be
   ;; accessible but are no longer in the accessible set
   (if *gen-filtering-p*
      (union
         (set-difference old-accessible new-accessible)
         old-inaccessible)
      nil))


;;;
;;; see whether we can put the condition facility to use.  it has the advantage
;;; of centralizing information about the various types of (expected) errors,
;;; their internal structure (if we were to pass any of that to an application
;;; system), printing regime, et al.                           (25-apr-04; oe)
;;;
(define-condition generator-uninitialized (error) 
  () 
  (:report (lambda (condition stream) 
             (declare (ignore condition))
             (format stream "the generator indices are uninitialized"))))

(define-condition unknown-predicates (error) 
  ((eps :initarg :eps :initform nil)) 
  (:report (lambda (condition stream) 
             (with-slots (eps) condition
               (format
                stream
                "unknown predicates: ~{|~(~a~)|~^, ~}"
                (loop
                    with result
                    for ep in eps
                    do (pushnew (mrs::ep-shorthand ep) result :test #'equal)
                    finally (when result
                              (return (sort result #'string-lessp)))))))))


;;; Interface to generator - take an input MRS, ask for instantiated lexical
;;; items, instantiated and uninstantiated rules that might be applicable,
;;; and partial ordering on lexical items, and then call generator
;;; proper. Clear chart and analyses record before entry in case we don't make
;;; it into generation proper. Do it also in chart-generate since that is also
;;; an entry point
(defun generate-from-mrs (mrs &key signal nanalyses)
  (if (mrs::fragmentp mrs)
    (mt::generate-from-fragmented-mrs mrs :signal signal)
    (handler-case (generate-from-mrs-internal mrs :nanalyses nanalyses)
      (condition (condition)
        (if signal
          (error condition)
          (warn (format nil "~a" condition)))))))

(defun generate-from-mrs-internal (input-sem &key nanalyses)

  ;; (ERB 2003-10-08) For aligned generation -- if we're in first only
  ;; mode, break up the tree in *parse-record* for reference by
  ;; ag-gen-lex-priority and ag-gen-rule-priority.  Store in *found-configs*.
  #+:arboretum
  (populate-found-configs)
  
  (setf input-sem (mrs::fill-mrs (mrs::unfill-mrs input-sem)))
  (setf *generator-input* input-sem)
  (when *gen-equate-qeqs-p*
    (setf input-sem (mrs::equate-all-qeqs input-sem)))
  (with-package (:lkb)
    (clear-gen-chart)
    (setf *cached-category-abbs* nil)

    ;;
    ;; no need to even try generating when there is no relation index
    ;;
    (unless (and (hash-table-p mrs::*relation-index*)
                 (> (hash-table-count mrs::*relation-index*) 0))
      (error 'generator-uninitialized))
    
    (let ((*gen-packing-p* (if *gen-first-only-p* nil *gen-packing-p*))
          lex-results lex-items grules lex-orderings 
          tgc tcpu conses symbols others)
      (time-a-funcall
       #'(lambda () 
           (multiple-value-setq (lex-results grules lex-orderings)
             (mrs::collect-lex-entries-from-mrs input-sem))
           (multiple-value-setq (lex-items grules lex-orderings)
             (filter-generator-lexical-items 
              (apply #'append lex-results) grules lex-orderings)))
       #'(lambda (tgcu tgcs tu ts tr scons ssym sother &rest ignore)
           (declare (ignore tr ignore))
           (setf tgc (+ tgcu tgcs) tcpu (+ tu ts)
                 conses (* scons 8) symbols (* ssym 24) others sother)))
      (setq %generator-statistics%
        (pairlis '(:ltgc :ltcpu :lconses :lsymbols :lothers)
                 (list tgc tcpu conses symbols others)))
      
      (when *debugging* (print-generator-lookup-summary lex-items grules))
      
      (let ((rel-indexes nil) (rel-indexes-n -1) (input-rels 0))
        (dolist (lex lex-items)
          (loop
              with eps = (mrs::found-lex-main-rels lex)
              initially (setf (mrs::found-lex-main-rels lex) 0)
              for ep in eps
              for index = (ash 1 (or (getf rel-indexes ep)
                                     (setf (getf rel-indexes ep)
                                           (incf rel-indexes-n))))
              do 
                (setf (mrs::found-lex-main-rels lex)
                  (logior (mrs::found-lex-main-rels lex) index))))
        (dolist (grule grules)
          (when (mrs::found-rule-p grule)
            (loop
                with eps = (mrs::found-rule-main-rels grule)
                initially (setf (mrs::found-rule-main-rels grule) 0)
                for ep in eps
                for index = (ash 1 (or (getf rel-indexes ep)
                                       (setf (getf rel-indexes ep)
                                             (incf rel-indexes-n))))
                do
                  (setf (mrs::found-rule-main-rels grule)
                    (logior (mrs::found-rule-main-rels grule) index)))))
        (setf %generator-unknown-eps% nil)
        (loop
            for ep in (mrs::psoa-liszt input-sem)
            do
              (if (getf rel-indexes ep)
                (setq input-rels
                   (logior input-rels (ash 1 (getf rel-indexes ep))))
                (push ep %generator-unknown-eps%)))
        (when %generator-unknown-eps%
           (error 'unknown-predicates :eps %generator-unknown-eps%))

        #+:debug
        (setf %input-sem input-sem
              %rel-indexes rel-indexes %input-rels input-rels)
        
        (chart-generate
         input-sem input-rels lex-items grules lex-orderings rel-indexes
         *gen-first-only-p* :nanalyses nanalyses)))))


(defun filter-generator-lexical-items (lex-items grules lex-orderings)
  (values
   (remove-if
    #'(lambda (x) (member x *duplicate-lex-ids* :test #'eq))
    lex-items :key #'mrs::found-lex-lex-id)
   grules lex-orderings))


;;; generate from an input MRS and a set of lexical entry FSs. Each entry
;;; covers zero or more of the relations in the MRS
;;;
;;; constraints on generation:
;;;
;;; when completing an active edge ensure that all relations containing
;;; indices internal to the rule have been realised (ensures only maximal
;;; alternative phrase containing modifiers is available for incorporation
;;; into larger phrases)
;;;
;;; extending an active edge with an inactive must not result in the
;;; duplication of any relation (prevents e.g. same modifier being added
;;; repeatedly)
;;;
;;; final analysis must not have any semantics missing (e.g. makes sure all
;;; relevant modifiers have been realised)
;;;
;;; returns values: realisations, number of argument instantiations filtered
;;; out, number of argument instantiation attempts, number of argument
;;; instantiations succeeding, number of unifications, number of copies, number
;;; of active and inactive edges

(defun chart-generate (input-sem input-rels found-lex-items possible-grules
                       *lexemes-allowed-orderings* *gen-rel-indexes*
                       &optional (*gen-first-only-p* *gen-first-only-p*)
                       &key nanalyses)

  (setq %generator-lexical-items% found-lex-items)
 
  (let ((*intersective-rule-names* 
         ;;
         ;; disable two-phase set-up when index accessibility filtering is on
         ;;
         (unless *gen-filtering-p* *intersective-rule-names*))
        (*safe-not-to-copy-p* t)
        (*filtered-tasks* 0) (*executed-tasks* 0) (*successful-tasks* 0) 
        (*unifications* 0) (*copies* 0) 
        (*non-intersective-rules*
         ;; this is all rules for best-first mode
         (remove-if
          #'(lambda (r)
              (or (and (not *gen-first-only-p*)
                       (some
                        #'(lambda (p)
                            (cond ((atom p) (eq p (rule-id r)))
                                  (t (eq (car p) (rule-id r)))))
                        *intersective-rule-names*))
                  (spelling-change-rule-p r)))
          possible-grules))
        tgc tcpu conses symbols others (consistent nil) (partial nil))
    
   (with-parser-lock ()
      (clear-gen-chart)
      (setf *cached-category-abbs* nil)
      (when *gen-packing-p* (reset-packings))
      (flush-heap *agenda*)

      (time-a-funcall
       #'(lambda () 
           (catch 'first
             ;; Add lexical edges
             (dolist (found found-lex-items)
               (let* ((lex-entry-fs (mrs::found-lex-inst-fs found))
                      (word (extract-orth-from-fs lex-entry-fs))
                      (edge
                       (make-g-edge
                        :id (next-edge) 
                        :category (indef-type-of-tdfs lex-entry-fs)
                        :rule word
                        :dag lex-entry-fs
                        :needed nil
                        :rels-covered (mrs::found-lex-main-rels found)
                        :children nil
                        :leaves (list word)
                        :lex-ids (list (mrs::found-lex-lex-id found))
                        :lexemes (list found))))
                 (when *gen-packing-p*
                   (setf (g-edge-odag edge) lex-entry-fs)
                   (setf (g-edge-dag edge) (copy-tdfs-partially lex-entry-fs)))
                 (setf (g-edge-dag-restricted edge)
                   (restrict-fs (tdfs-indef (g-edge-dag edge))))
                 (setf (g-edge-accessible edge)
                   (collect-semantic-variables-in-fs (g-edge-dag edge)))
                 (incf *copies*)        ; each lex entry will be a copy
                 (with-agenda (when *gen-first-only-p* 
                                (if *gen-scoring-hook*
                                  (funcall 
                                   *gen-scoring-hook*
                                   (list :lexicon edge))
                                  (gen-lex-priority lex-entry-fs)))
                   (gen-chart-add-inactive edge input-sem input-rels))))
             ;; Process tasks
             (loop 
                 until (empty-heap *agenda*)
                 do (funcall (heap-extract-max *agenda*)))
             ;; Look for results
             (unless *gen-first-only-p*
               (multiple-value-setq (consistent partial)
                 (gen-chart-find-covering-edges
                  (gen-chart-retrieve-with-index *toptype* 'inactive)
                  input-rels)))))
       #'(lambda (tgcu tgcs tu ts tr scons ssym sother &rest ignore)
           (declare (ignore tr ignore))
           (setq tgc (+ tgcu tgcs) tcpu (+ tu ts)
                 conses (* scons 8) symbols (* ssym 24) others sother)))
      (setq %generator-statistics%
        (nconc %generator-statistics%
               (pairlis '(:gtgc :gtcpu :gconses :gsymbols :gothers)
                        (list tgc tcpu conses symbols others))))

      (time-a-funcall
       #'(lambda () 
           ;; Perform adjunction phase and unpack
           (unless *gen-first-only-p*
             (setq *gen-record*
               (let* ((candidates
                       (nconc consistent
                              (when (and *intersective-rule-names* partial)
                                (gen-chart-adjoin-modifiers 
                                 partial input-rels possible-grules))))
                      (complete
                       (cond
                        ((and *intersective-rule-names* partial)
                         (loop
                             for edge in candidates
                             nconc
                               (loop
                                   for edge in (unpack-edge! edge)
                                   when (gen-chart-check-covering
                                         edge input-rels)
                                   collect edge)))
                        (*gen-packing-p*
                         ;;
                         ;; in packing mode, we need to include the final test
                         ;; for semantic equality in unpacking, since otherwise
                         ;; the n-best counting may end up wrong.
                         ;;
                         ;; _fix_me_
                         ;; this all badly interacts with the `filter' mode for
                         ;; using the post-generation semantics test, since it 
                         ;; is not really clear which notion of n-best to use
                         ;; then.  for now, risk calling the test twice on each
                         ;; result in filter mode (see result selection below).
                         ;;                                     (27-jan-05; oe)
                         (selectively-unpack-edges
                          (loop
                              for edge in candidates
                              when (gen-chart-check-covering edge input-rels)
                              collect edge)
                          nanalyses
                          :test (unless *bypass-equality-check*
                                  #'(lambda (edge)
                                      (gen-chart-check-compatible 
                                       edge input-sem)))))
                        (t
                         (loop
                             for edge in candidates
                             when (gen-chart-check-covering edge input-rels)
                             collect edge))))
                      (consistent
                       (loop
                           for edge in complete
                           when (or (and *gen-packing-p*
                                         (null *bypass-equality-check*))
                                    (gen-chart-check-compatible 
                                     edge input-sem))
                           collect edge)))
                 (if (null *bypass-equality-check*)
                   consistent
                   (if (eq *bypass-equality-check* :filter)
                     (or consistent complete)
                     complete))))))
       #'(lambda (tgcu tgcs tu ts tr scons ssym sother &rest ignore)
           (declare (ignore tr ignore))
           (setq tgc (+ tgcu tgcs) tcpu (+ tu ts)
                 conses (* scons 8) symbols (* ssym 24) others sother)))
      (setq %generator-statistics%
        (nconc %generator-statistics%
               (pairlis '(:atgc :atcpu :aconses :asymbols :aothers)
                        (list tgc tcpu conses symbols others))))
      ;;
      ;; _fix_me_
      ;; the edge accounting fails to include packed edges and those created
      ;; during unpacking.                                      (1-jan-05; oe)
      ;;
      (values
       (extract-strings-from-gen-record) ; also spelling e.g. "a" -> "an"
       *filtered-tasks* *executed-tasks* *successful-tasks*
       *unifications* *copies*
       (length (gen-chart-retrieve-with-index *toptype* 'active))
       (length (gen-chart-retrieve-with-index *toptype* 'inactive))))))


(defun extract-strings-from-gen-record nil
  (loop 
      for edge in *gen-record*
      for string = (fix-spelling (g-edge-leaves edge)) ;; in spell.lsp
      do (setf (edge-string edge) string)
      collect string))


(defun clear-gen-chart nil
   (setq *edge-id* 0)
   (setq %edge-allowance% 0)
   (setq *gen-chart* nil)
   (setq *gen-record* nil))


;;; Find edges that are potential results that cover all or part of input
;;; relations

(defun gen-chart-find-covering-edges (candidate-edges input-rels)
  (let ((covering nil)
        (partial nil)
        (start-symbols
         (if (listp *start-symbol*) *start-symbol* (list *start-symbol*))))
    (dolist (new
               (if *substantive-roots-p*
                  (gen-chart-root-edges candidate-edges start-symbols)
                  (gen-filter-root-edges candidate-edges start-symbols)))
        ;; process has so far ensured that we have not generated any edge
        ;; containing a relation name that is not in input semantics, and that
        ;; no edge contains duplicates of exactly the same relation - now check
        ;; if we have generated all relations
        (if (gen-chart-check-covering new input-rels)
           (push new covering)
           (push new partial)))
    (when *gen-adjunction-debug*
       (format t "~&Covering edges: ~:A~%Partial edges: ~:A"
          (mapcar #'g-edge-id covering) (mapcar #'g-edge-id partial)))
    (values covering partial)))


(defun gen-chart-check-covering (edge input-rels)
   ;; check that we've got all the relations that we wanted
   (gen-chart-subset-p
      input-rels
      (g-edge-rels-covered edge)))


(defun gen-chart-check-compatible (edge input-sem)
   ;; construct the MRS for edge
   ;; We test for 'compatibility' rather than equality - in
   ;; particular, semantics of generated string might be more specific than
   ;; input MRS wrt things like scope - so we pass 3nd arg of nil to mrs-equalp
   ;; Semantics are already guaranteed to be compatible wrt relation arguments since
   ;; these were skolemised in the input MRS
   (or (and *bypass-equality-check* (not (eq *bypass-equality-check* :filter)))
      (let* ((mrs (mrs::extract-mrs edge))
             (mrs (mrs::fill-mrs (mrs::unfill-mrs mrs))))
        (setf (edge-mrs edge) mrs)
;;            (when *sem-debugging*
;;              (mrs::output-mrs input-sem 'mrs::simple)
;;              (mrs::output-mrs mrs 'mrs::simple))  
        #+:null
        (ignore-errors
         (mt::unify-mtr-component
          (if *gen-equate-qeqs-p* (mrs::equate-all-qeqs mrs) mrs)
          input-sem))
        #-:null
        (mrs::mrs-equalp 
         (if *gen-equate-qeqs-p* (mrs::equate-all-qeqs mrs) mrs)
         input-sem nil *debugging* 
         (not (eq *bypass-equality-check* :filter))))))


(defun gen-chart-root-edges (edges start-symbols)
   ;; c.f. create-new-root-edges in parse.lsp
   (loop for start-symbol in start-symbols        
       nconc
       (let ((tdfs (get-tdfs-given-id start-symbol)))
         (if tdfs
            (loop for edge in edges
               nconc
               (let ((unif 
                       (yadu tdfs (g-edge-dag edge))))
                  (if unif
                      (let*
                         ((accessible (collect-semantic-variables-in-fs unif))
                          (new-edge
                             (make-g-edge :id (next-edge)
                                          :category (indef-type-of-tdfs unif)
                                          :rule 'root
                                          :dag unif
                                          :needed nil
                                          :dag-restricted
                                          (restrict-fs (tdfs-indef unif))
                                          :rels-covered (g-edge-rels-covered edge)
                                          :children (list edge)
                                          :leaves (g-edge-leaves edge)
                                          :lex-ids (g-edge-lex-ids edge)
                                          :lexemes (g-edge-lexemes edge)
                                          :accessible accessible
                                          :inaccessible
                                          (merge-new-inaccessible accessible
                                             (g-edge-accessible edge)
                                             (g-edge-inaccessible edge)))))
                        (gen-chart-add-with-index new-edge)
                        (list new-edge)))))))))


(defun gen-filter-root-edges (edges &optional (start-symbols *start-symbol*))

  (unless (listp start-symbols) (setf start-symbols (list start-symbols)))
  (loop
      with roots = (loop
                       for start-symbol in start-symbols
                       for root = (get-tdfs-given-id start-symbol)
                       when root collect root)
      for edge in edges
      for match = (loop
                      for root in roots
                      thereis (unifiable-wffs-p
                               (tdfs-indef root)
                               (tdfs-indef (g-edge-dag edge))))
      when match collect edge)
  ;; c.f. filter-root-edges in parse.lsp
  ;;
  ;; _fix_me_
  ;; this version gives us duplicate results with multiple start symbols.
  ;;                                                          (23-apr-04; oe)
  #+:null
   (loop for start-symbol in start-symbols
      nconc
      (let ((root-spec (get-tdfs-given-id start-symbol)))
         (if root-spec
            (loop for edge in edges
               nconc
               (if (unifiable-wffs-p
                      (tdfs-indef root-spec) (tdfs-indef (g-edge-dag edge)))
                  (list edge)))))))


;;; Chart indexing - on *semantics-index-path* values. May be full types or
;;; instance types. Seem to be mostly disjoint if not eq, so don't bother using
;;; a tree-like representation
;;;
;;; Apart from these fns, print-gen-chart, find-gen-edge-given-id and
;;; create-gen-chart-pointers are the only functions that need to know the
;;; internal representation of chart

#+:gen-index
(defun gen-chart-dag-index (index-dag edge-id)
  (declare (ignore edge-id))
   (if index-dag
      (unify-get-type index-dag) ; may be called inside unif context
      (progn
         ;;(cerror (format nil "use type ~A" *toptype*)
         ;;   "unexpectedly missing index for edge ~A: ~S" edge-id dag)
         ;; (warn "unexpectedly missing index for edge ~A - using ~A" edge-id *toptype*)
         *toptype*)))
#-:gen-index
(defun gen-chart-dag-index (index-dag edge-id)
   (declare (ignore index-dag edge-id))
   *toptype*)


(defun gen-chart-add-with-index (edge &optional chart-index)
  #-:gen-index (declare (ignore chart-index))
  (let ((index
           #+:gen-index
           (or chart-index
              (gen-chart-dag-index
               (or 
                (existing-dag-at-end-of
                  (tdfs-indef (g-edge-dag edge)) *semantics-index-path*)
                (if *alt-semantics-index-path*
                    (existing-dag-at-end-of
                     (tdfs-indef (g-edge-dag edge)) 
                     *alt-semantics-index-path*)))
                 (g-edge-id edge)))
           #-:gen-index *toptype*))
      (setf (g-edge-index edge) index)
      (let ((entry (assoc index *gen-chart* :test #'equal))) ; may be a cons
         (unless entry
            (push (setq entry (list* index nil nil)) *gen-chart*))
         ;; active edges are stored in the cadr of chart entries, inactive
         ;; in the cddr
         (if (g-edge-needed edge)
            (progn
               (push edge (cadr entry))
               index)
            (if
               (and *gen-packing-p*
                  (dolist (inact (cddr entry) nil)
                     (when 
                        (gen-chart-set-equal-p
                           (g-edge-rels-covered edge) (g-edge-rels-covered inact))
                        (multiple-value-bind (forwardp backwardp)
                               (dag-subsumes-p (tdfs-indef (g-edge-dag inact)) (tdfs-indef (g-edge-dag edge)))
                           (when *debugging*
                              (format t "~&Trying subsumption between edges ~A and ~A"
                                      (g-edge-id inact) (g-edge-id edge)))
                           (when forwardp
                              ;; (print (list forwardp backwardp (g-edge-id inact) (g-edge-id edge)))
                              (if backwardp
                                 (progn
                                    (incf (packings-equivalent *packings*))
                                    (push edge (g-edge-equivalent inact)))
                                 (progn
                                    (incf (packings-proactive *packings*))
                                    (push edge (g-edge-packed inact))))
                              (return t))))))
               nil
               (progn
                  (push edge (cddr entry))
                  index))))))


(defun gen-chart-retrieve-with-index (index mode)
  ;; return all active/inactive edges in chart keyed by a type compatible with index
  #-:gen-index (declare (ignore index))
  (let ((res nil))
    (dolist (entry *gen-chart* res)
      (when #+:gen-index (greatest-common-subtype (car entry) index)
            #-:gen-index t
         (let ((edges (if (eq mode 'active) (cadr entry) (cddr entry))))
            (setq res
               (if res (append edges res) edges)))))))


;;; Daughter paths in a rule, in the order that they should be instantiated. Second
;;; value is the zero-based index of the head daughter

(defun gen-chart-rule-ordered-daughters (rule)
   (values (rule-daughters-apply-order rule)
      (position (car (rule-daughters-apply-order rule)) (cdr (rule-order rule))
         :test #'eq)))


;;; Core control functions. Processing inactive and active edges

(defun gen-chart-add-inactive (edge input-sem input-rels)
  ;; assume that all relations supplied in lexical entries and rules
  ;; are relevant to input-sem, i.e. that if we've generated an edge
  ;; it won't contain any relations that aren't in input-sem
  (unless edge (return-from gen-chart-add-inactive nil))
  (let ((index (gen-chart-add-with-index edge)))
    ;; no index, so not entered into the chart
    (unless index (return-from gen-chart-add-inactive nil))
    ;; did we just find a result?
    (when *gen-first-only-p*
      (let* ((complete
              (delete-if-not
               #'(lambda (u)
                   (and (gen-chart-check-covering u input-rels)
                        (gen-chart-check-compatible u input-sem)))
               (if *gen-packing-p* (unpack-edge! edge) (list edge))))
             (sentential (gen-filter-root-edges complete)))
        (when sentential
           (setq *gen-record* sentential)
           (throw 'first t))))
    ;; see if this new inactive edge can extend any existing active edges
    (mapc
       #'(lambda (act)
           (with-agenda (when *gen-first-only-p* 
                          (if *gen-scoring-hook*
                            (funcall 
                             *gen-scoring-hook*
                             (list :active act edge))
                            (gen-rule-priority (edge-rule act))))
             (gen-chart-test-active edge act input-sem input-rels)))
       (gen-chart-retrieve-with-index index 'active))
    ;; see if we can create new active edges by instantiating the head
    ;; daughter of a rule
    (mapc #'(lambda (rule) 
              (when *debugging*
                (format t "~&Trying to create new active edge from rule ~A ~
                     and inactive edge ~A"
                        (rule-id rule) (g-edge-id edge)))
              (with-agenda (when *gen-first-only-p* 
                             (if *gen-scoring-hook*
                               (funcall 
                                *gen-scoring-hook*
                                (list :rule rule edge))
                               (gen-rule-priority rule)))
                (multiple-value-bind (gen-daughter-order head-index) ; zero-based on daughters
                    (gen-chart-rule-ordered-daughters rule)
                  (multiple-value-bind (act chart-index)
                       (gen-chart-create-active
                          rule edge gen-daughter-order head-index)
                    (when act
                      ;; and try and fill in next needed daughter of active edge
                      (gen-chart-extend-active act input-sem input-rels chart-index))))))
          *non-intersective-rules*)))


(defun gen-chart-create-active (rule edge gen-daughter-order head-index)
   (multiple-value-bind (unified-dag restricted chart-index)
          (gen-chart-try-unification rule
             (if *gen-packing-p* (rule-rtdfs rule) (rule-full-fs rule))
             (first gen-daughter-order) ; head daughter path
             (nth head-index (rule-daughters-restricted rule)) ; head restrictor
             head-index edge
             (rest gen-daughter-order)
             (first (rule-order rule)))
      (when unified-dag
         (let ((ndaughters (length gen-daughter-order)))
            (values
               (make-g-edge
                  :id (next-edge) :rule rule
                  ;; category slot not filled in since not a complete constituent
                  :dag unified-dag
                  :res rule
                  :needed (rest gen-daughter-order)
                  :dag-restricted restricted
                  :rels-covered
                  (gen-chart-set-union
                     (if (mrs::found-rule-p rule)
                        (mrs::found-rule-main-rels rule)) 
                     (g-edge-rels-covered edge))
                  :children
                  (gen-make-list-and-insert ndaughters edge (1+ head-index))
                  :lex-ids
                  (gen-make-list-and-insert
                     ndaughters (g-edge-lex-ids edge) (1+ head-index))
                  :leaves
                  (gen-make-list-and-insert
                     ndaughters (g-edge-leaves edge) (1+ head-index))
                  :lexemes (g-edge-lexemes edge)
                  :accessible (g-edge-accessible edge)
                  :inaccessible (g-edge-inaccessible edge))
               chart-index)))))


(defun gen-chart-test-active (inact act input-sem input-rels &optional one-off-p)
  ;; can extend active edge with inactive? First check to make sure new edge
  ;; would not use any relation from initial lexical items more than once.
  (when (and
           (or one-off-p ; intersection guaranteed to be OK if one-off-p
               (gen-chart-set-disjoint-p
                  (g-edge-rels-covered act) (g-edge-rels-covered inact)))
           (if (car (g-edge-children act)) ; looking for inact on right (left done)
              (gen-chart-ordering-allowed-p
                 (g-edge-lexemes act) (g-edge-lexemes inact))
              (gen-chart-ordering-allowed-p
                 (g-edge-lexemes inact) (g-edge-lexemes act))))
       (when *debugging*
         (format t "~&Trying to extend active edge ~A with inactive edge ~A"
                 (g-edge-id act) (g-edge-id inact)))
       (let ((next-index
               (position (first (g-edge-needed act))
                         (rest (rule-order (g-edge-res act))) :test #'eq)))
         (multiple-value-bind (unified-dag restricted index-dag)
              (gen-chart-try-unification (g-edge-rule act)
                (g-edge-dag act)
                (first (g-edge-needed act))
                (g-edge-dag-restricted act)
                next-index inact
                (rest (g-edge-needed act))
                (first (rule-order (g-edge-res act)))
                act)
           (when unified-dag
             ;; remaining non-head daughters in active edge are filled in
             ;; left-to-right order
             (let ((new-act
                    (make-g-edge
                     :id (next-edge) 
                     ;; category slot not filled in since not (yet) a complete constituent
                     :rule (g-edge-rule act)
                     :dag unified-dag
                     :res (g-edge-res act)
                     :needed (rest (g-edge-needed act))
                     :dag-restricted restricted
                     :rels-covered
                     (gen-chart-set-union
                        (g-edge-rels-covered act) (g-edge-rels-covered inact))
                     :children
                     (gen-copy-list-and-insert
                       (g-edge-children act) inact next-index)
                     :lex-ids 
                     (gen-copy-list-and-insert
                       (g-edge-lex-ids act) (g-edge-lex-ids inact) next-index)
                     :leaves
                     (gen-copy-list-and-insert
                       (g-edge-leaves act) (g-edge-leaves inact) next-index)
                     :lexemes
                     (append (g-edge-lexemes act) (g-edge-lexemes inact))
                     :mod-index (g-edge-mod-index act)
                     :accessible (union (g-edge-accessible act) (g-edge-accessible inact))
                     :inaccessible (union (g-edge-inaccessible act) (g-edge-inaccessible inact)))))
               (if one-off-p new-act
                 ;; (ERB 2003-10-22) There originally wasn't any reference to
                 ;; the agenda here, but I'm not getting my hands on all of the
                 ;; edges.
                 (with-agenda (when *gen-first-only-p* 
                                (if *gen-scoring-hook*
                                  (funcall 
                                   *gen-scoring-hook*
                                   (list :active act inact))
                                  (gen-rule-priority inact)))
                   (gen-chart-extend-active 
                    new-act input-sem input-rels index-dag)))))))))


(defun gen-chart-extend-active (act input-sem input-rels act-chart-index)
   (if (g-edge-needed act)
      ;; add newly extended active edge to chart, then look for any existing
      ;; inactive edges which can extend it
      (let ((index (gen-chart-add-with-index act act-chart-index)))
         (dolist (e (gen-chart-retrieve-with-index index 'inactive))
            (gen-chart-test-active e act input-sem input-rels)))
      ;; have ended up completing an active edge - forming a complete constituent
      (gen-chart-add-inactive
         (gen-chart-finish-active act input-sem) input-sem input-rels)))


(defun gen-chart-finish-active (e input-sem)
   ;; turn active into an inactive edge
   (setf (g-edge-category e) (indef-type-of-tdfs (g-edge-dag e)))
   (setf (g-edge-res e) nil)
   (setf (g-edge-lex-ids e)
      (apply #'append (g-edge-lex-ids e)))
   (setf (g-edge-leaves e)
      (apply #'append (g-edge-leaves e)))
   (let ((old-accessible (g-edge-accessible e)))
      (setf (g-edge-accessible e)
         (collect-semantic-variables-in-fs (g-edge-dag e)))
      (setf (g-edge-inaccessible e)
         (merge-new-inaccessible
            (g-edge-accessible e) old-accessible (g-edge-inaccessible e))))
   (when *gen-filtering-p*
      (dolist (rel (mrs::psoa-liszt input-sem))
         (unless (logbitp (getf *gen-rel-indexes* rel) (g-edge-rels-covered e))
            ;; rel in input semantics not covered by this edge - now check the
            ;; rel's handle and its other non-ignorable variables against the edge's
            ;; inaccessible set
           (when (and mrs::*rel-handel-path*
                      (member (mrs::var-id (mrs::rel-handel rel))
                              (g-edge-inaccessible e)))
               (when *gen-filtering-debug*
                  (format t
"~&Filtering edge ~A on non-covered rel ~A needing inaccessible variable ~A~%"
                     (g-edge-id e) (mrs::rel-pred rel) (mrs::var-id (mrs::rel-handel rel)))
                  (print-gen-chart-edge e t nil))
               (return-from gen-chart-finish-active nil))
            (dolist (fp (mrs::rel-flist rel))
               (when (and (mrs::var-p (mrs::fvpair-value fp))
                          (not (member (mrs::fvpair-feature fp) mrs::*scoping-ignored-roles*))
                          (member (mrs::var-id (mrs::fvpair-value fp)) (g-edge-inaccessible e)))
                  (when *gen-filtering-debug*
                     (format t
"~&Filtering edge ~A on non-covered rel ~A needing inaccessible variable ~A~%"
                        (g-edge-id e) (mrs::rel-pred rel) (mrs::var-id (mrs::fvpair-value fp)))
                     (print-gen-chart-edge e t nil))
                  (return-from gen-chart-finish-active nil))))))
   e)


;;; Unification routines, entered only through gen-chart-try-unification

(defun gen-chart-try-unification (rule rule-tdfs daughter-path
                                  daughter-restricted daughter-index edge 
                                  needed mother-path &optional act)
   ;; try unification corresponding to applying a grammar rule or incorporating
   ;; inactive into an active edge
   (if (and (check-rule-filter rule (g-edge-rule edge) daughter-index)
            (restrictors-compatible-p daughter-restricted 
                                      (g-edge-dag-restricted edge)))
      (gen-chart-evaluate-unification
         rule-tdfs daughter-path (g-edge-dag edge) needed mother-path act)
      (progn (incf *filtered-tasks*) nil)))


(defun gen-chart-evaluate-unification (rule-tdfs daughter-path fs needed
                                       mother-path &optional act)
   ;; c.f. evaluate-unifications in parse.lsp
   ;; 
   ;; No orthography done here - it was done during the production of the
   ;; initial set of candidate lexical entries
   ;;
   ;; unify path <daughter-path> of rule-tdfs with fs, then if needed is
   ;; false return mother portion of rule-tdfs. Return as second value
   ;; the quick-check restrictor for the result, and if needed is true
   ;; return third value of semantic index for next needed daughter
   (unless rule-tdfs
      ;; a previous delayed copy stuffed back into edge failed
      (return-from gen-chart-evaluate-unification nil))
   (when (functionp rule-tdfs)
      ;; redo the unification and delayed copy, stuffing result it back into
      ;; the active edge. Copy might fail due to circularity
      (setf (g-edge-dag act) (setq rule-tdfs (funcall rule-tdfs)))
      (unless rule-tdfs
         (decf *successful-tasks*)
         (return-from gen-chart-evaluate-unification nil)))
   (with-unification-context (ignore)
     (incf *executed-tasks*)
     (unless
        (setq rule-tdfs
           (yadu rule-tdfs
              (create-temp-parsing-tdfs fs daughter-path)))
        ;(print (list (dag-type (tdfs-indef rule-tdfs)) daughter-path (dag-type (tdfs-indef fs))))
        (return-from gen-chart-evaluate-unification nil))
     (incf *successful-tasks*)
     (if needed
        #+:gen-immediate-copy
        (let ((dag (copy-tdfs-elements rule-tdfs))) ; immediately copy active edge
           (when dag
              (values dag
                 (restrict-fs
                    (existing-dag-at-end-of (tdfs-indef dag) (first needed)))
                 #+:gen-index
                 (gen-chart-dag-index
                    (existing-dag-at-end-of
                       (tdfs-indef dag) (append (first needed) *semantics-index-path*))
                    nil)
                 #-:gen-index *toptype*)))
        #-:gen-immediate-copy
        (values
           ;; return a closure which when funcalled will replay the unification and
           ;; perform copy - don't do copy yet since no guarantee we'll ever use it
           #'(lambda ()
               (with-unification-context (ignore)
                  (copy-tdfs-elements ; does (incf *copies*) itself
                     (yadu rule-tdfs
                        (create-temp-parsing-tdfs fs daughter-path)))))
           (x-restrict-fs
              (x-existing-dag-at-end-of (tdfs-indef rule-tdfs) (first needed)))
           #+:gen-index
           (gen-chart-dag-index
              (x-existing-dag-at-end-of (tdfs-indef rule-tdfs)
                 (append (first needed) *semantics-index-path*))
              nil)
           #-:gen-index *toptype*)
        (let ((dag (gen-chart-restrict-and-copy
                      (tdfs-at-end-of mother-path rule-tdfs))))
           (when dag (values dag (restrict-fs (tdfs-indef dag))))))))


(defun gen-chart-restrict-and-copy (dag)
   ;; delete arcs just holding constituents' feature structures -
   ;; before copying otherwise their copies would be thrown away
   ;; immediately we have to check whether any of the deleted dags
   ;; contain a cycle - if so then the whole rule application should
   ;; fail. C.f. active parser function restrict-and-copy-tdfs
   (let* ((real-dag (deref-dag (tdfs-indef dag)))
          (new (clone-dag real-dag))
          (arcs-to-check nil))
     (flet ((member-with-cyclic-check (arc)
              (when (member (dag-arc-attribute arc) 
                            *deleted-daughter-features* :test #'eq)
                (push arc arcs-to-check)
                t)))
       (setf (dag-arcs new)
         (remove-if #'member-with-cyclic-check (dag-arcs new)))
       (setf (dag-comp-arcs new)
         (remove-if #'member-with-cyclic-check (dag-comp-arcs new)))
       ;; take advantage of the fact that removed arcs might share
       ;; structure by checking them all at once
       (let ((res
               (and
                 (not (cyclic-dag-p
                        (make-dag :type *toptype* 
                                  :arcs arcs-to-check)))
                 (setf (dag-forward real-dag) new)
                 (copy-tdfs-elements dag)))) ; does (incf *copies*) itself
         (or res
           ;; charge copy failure to last successful unification
           (progn (decf *successful-tasks*) nil))))))


;;; Second phase, where intersective modifiers are introduced

(defun gen-chart-adjoin-modifiers (partial-edges input-rels possible-grules)
   (declare (ignore input-sem))
   (let
      ((intersective-edges (gen-chart-intersective-inactive-edges))
       (intersective-rules-and-daughters
         (mapcar
            #'(lambda (p)
               (if (consp p) p
                  (let ((rule (get-grammar-rule-entry p)))
                     (cons p
                        (if rule
                           (loop for i from 1 to (1- (length (rule-order rule)))
                                 collect i))))))
            *intersective-rule-names*))
       (partial-extendable nil))
      (when *gen-adjunction-debug*
         (format t "~%Intersective inactive edges: ~:A" 
            (mapcar #'g-edge-id intersective-edges)))
      (let*
         ((mod-candidate-edges
            (gen-chart-active-mod-candidate-edges
               intersective-edges possible-grules intersective-rules-and-daughters)))
         ;; perform all possible adjunctions of modifiers into others e.g. to get nested PPs -- note that
         ;; after doing this some of the modifiers may have supersets of rels-covered
         ;; unpackable from them
         (when *gen-adjunction-debug* (format t "~&Adjoining into adjuncts"))
         (dolist (int mod-candidate-edges)
            (gen-chart-insert-adjunction int
               (remove-if-not
                  #'(lambda (mod) (gen-chart-set-disjoint-p mod (g-edge-rels-covered int)))
                  mod-candidate-edges :key #'g-edge-rels-covered)
               nil))
         ;; adjoin into partial analyses, considering only modifiers that do not have
         ;; any overlap, and rejecting any partial analysis that couldn't be completed
         ;; by any subset of modifiers available
         (dolist (partial partial-edges)
            (when *gen-adjunction-debug*
               (format t "~&---~%Partial edge [~A] spanning ~:A" (g-edge-id partial)
                  (g-edge-leaves partial)))
            ;; (print (gen-chart-set-rel-preds (g-edge-rels-covered partial)))
            (let
               ((missing-rels
                   (gen-chart-set-difference input-rels (g-edge-rels-covered partial)))
                (non-overlapping
                   (remove-if-not
                      #'(lambda (mod)
                           (gen-chart-set-disjoint-p mod (g-edge-rels-covered partial)))
                      mod-candidate-edges :key #'g-edge-rels-covered)))
               ;; (print (list missing-rels (gen-chart-set-rel-preds missing-rels)))
               (when (and non-overlapping
                        (gen-chart-set-equal-p missing-rels
                           (reduce #'gen-chart-set-union
                              non-overlapping :key #'g-edge-rels-covered)))
                  (when *gen-adjunction-debug*
                     (format t "~&Checking adjunction into partial edge"))
                  (let ((adjoined
                          (gen-chart-insert-adjunction partial non-overlapping nil)))
                     (when adjoined
                        (when *gen-adjunction-debug*
                           (format t "~&Successful modifiers ~A" (mapcar #'g-edge-id adjoined)))
                        (push partial partial-extendable))))))
         partial-extendable)))


(defun gen-chart-set-rel-preds (rels)
   (flet ((rgetf (plist val)
            (do ((tail plist (cddr tail)))
                ((null tail) nil)
                (when (eql (cadr tail) val) (return (car tail))))))
      (loop for n from 0 to (1- (integer-length rels))
         for rel = (and (logbitp n rels) (rgetf *gen-rel-indexes* n))
         when rel collect (mrs::rel-pred rel))))


(defun gen-chart-intersective-inactive-edges nil
  ;; return a list of all inactive edges in chart which are able to function
  ;; as intersective modifiers
  (remove-if-not
     #'(lambda (e)
          (and
             ;; words like 'had' on their own with no semantics cannot be
             ;; intersective modifiers
             (gen-chart-set-non-empty-p (g-edge-rels-covered e))
             (intersective-modifier-dag-p (tdfs-indef (g-edge-dag e)))))
     (gen-chart-retrieve-with-index *toptype* 'inactive)))


;;; Make active edges from inactive intersective modifier edges

(defun gen-chart-active-mod-candidate-edges (intersective-edges possible-grules
      intersective-rules-and-daughters)
   (mapcan
      #'(lambda (inact)
         (mapcan
            #'(lambda (rule)
               (let ((entry
                       (assoc (rule-id rule) intersective-rules-and-daughters :test #'eq)))
                  (when entry
                     (mapcan
                        #'(lambda (index) ; index is 1 for 1st daughter, etc
                           (let ((path (nth index (rule-order rule))))
                              (unless path
                                 (error "No daughter ~A in rule ~A" index (rule-id rule)))
                              (multiple-value-bind (act index-dag)
                                    (gen-chart-create-active 
                                       rule inact
                                       (cons path
                                          (remove path (cdr (rule-order rule)) :test #'eq))
                                       (position path (cdr (rule-order rule)) :test #'eq))
                                 (when act
                                    (unless (eql (length (g-edge-needed act)) 1)
                                       (error "Intersective modification rule ~A is not ~
                                          binary branching" (rule-id rule)))
                                    (setf (g-edge-mod-index act)
                                       (position (first (g-edge-needed act)) (cdr (rule-order rule))
                                          :test #'eq))
                                    (when (functionp (g-edge-dag act))
                                       (setf (g-edge-dag act) (funcall (g-edge-dag act))))
                                    (gen-chart-add-with-index act index-dag)
                                    (when *gen-adjunction-debug*
                                       (format t "~&Inactive [~A] -> active [~A]"
                                          (g-edge-id inact) (g-edge-id act)))
                                    (list act)))))
                         (cdr entry)))))
            possible-grules))
       ;; (mapcan #'(lambda (e) (unpack-edge! e)) intersective-edges)
       intersective-edges))


;;; Attempt to adjoin all possible of acts into forest with top node edge, recording
;;; in list adjoined which of acts succeeded in adjoining at some point.

(defun gen-chart-insert-adjunction (edge acts adjoined)
   (unless (member nil (g-edge-children edge))
      ;; don't try to adjoin into the top of an active edge
      (dolist (act acts)
         (when (gen-chart-try-adjunction act edge)
            (pushnew act adjoined)
            (pushnew act (g-edge-adjuncts edge)))))
   (dolist (c (g-edge-children edge))
      (when c
         ;; don't try to adjoin into the needed daughter of an active edge
         (setq adjoined (gen-chart-insert-adjunction c acts adjoined))))
   (dolist (p (g-edge-equivalent edge))
      (setq adjoined (gen-chart-insert-adjunction p acts adjoined)))
   (dolist (p (g-edge-packed edge))
      (setq adjoined (gen-chart-insert-adjunction p acts adjoined)))
   adjoined)
         
         
(defun gen-chart-try-adjunction (act edge)
   (let ((rule (g-edge-rule act))
         (rule-tdfs (g-edge-dag act))
         (daughter-path (first (g-edge-needed act)))
         (daughter-restricted (g-edge-dag-restricted act))
         (daughter-index (g-edge-mod-index act))
         (fs (g-edge-dag edge)))
      (if (and (check-rule-filter
                  rule (g-edge-rule edge) daughter-index)
               (restrictors-compatible-p
                  daughter-restricted (g-edge-dag-restricted edge)))
         (with-unification-context (ignore)
            (incf *executed-tasks*)
            (if
               (yadu rule-tdfs
                  (create-temp-parsing-tdfs fs daughter-path))
               (progn
                  (incf *successful-tasks*)
                  (when *gen-adjunction-debug*
                     (format t
"~&Adjoining active edge ~A covering ~A~%   into inactive edge ~A covering ~A" 
                        (g-edge-id act) (g-edge-leaves act) (g-edge-id edge)
                        (g-edge-leaves edge)))
                  t)
               nil))
         (progn (incf *filtered-tasks*) nil))))


;;; Print out summary of strings in generator chart - (print-gen-summary)

(defun print-gen-summary (&key (stream t))
   (format stream "~&------~%")
   (let ((leaves nil))
      (dolist (entry *gen-chart*)
         (dolist (e (cddr entry))
            (pushnew (g-edge-leaves e) leaves :test #'equal)
            (dolist (p (g-edge-equivalent e))
               (pushnew (g-edge-leaves p) leaves :test #'equal)
            (dolist (p (g-edge-packed e))
               (pushnew (g-edge-leaves p) leaves :test #'equal)))))
      (dolist (s
                (sort leaves
                   ;; on length, and then lexicographically case-insensitive
                   #'(lambda (x y)
                       (if (eql (length x) (length y))
                          (loop for a in x for b in y do
                             (unless (string-equal a b) (return (string-lessp a b))))
                          (< (length x) (length y))))))
         (format stream "~A~%" s))
      (format stream "~%")))


;;; Print out contents of generator chart (tty output) - (print-gen-chart)

(defun print-gen-chart (&key concise (stream t))
   (format stream "~&------~%")
   (dolist (entry (reverse *gen-chart*)) ; order in which originally created
      (format stream "~%Vertex ~(~A~):~%" (car entry))
      (dolist (e (sort (append (cadr entry) (copy-list (cddr entry))) #'<
                       :key #'edge-id))
         (print-gen-chart-edge e stream concise)
         (dolist (p (g-edge-equivalent e))
            (format stream " = packed ")
            (print-gen-chart-edge p stream concise)
         (dolist (p (g-edge-packed e))
            (format stream " > packed ")
            (print-gen-chart-edge p stream concise)))))
   (format stream "~%"))

(defun print-gen-chart-edge (e stream concise)
   (format stream "[~A] ~A~A ~35,5T=> (~{~:A~^ ~}) ~A [~{~A~^ ~}]~%"
      (g-edge-id e)
      (if (rule-p (g-edge-rule e)) (rule-id (g-edge-rule e))
         (if concise (first (g-edge-lex-ids e)) (g-edge-rule e)))
      (if (g-edge-needed e)
         (format nil " / ~{~A~^ ~}" (g-edge-needed e))
         "")
      (g-edge-leaves e)
      (if (and *gen-filtering-p* *gen-filtering-debug*)
         (format nil " a~:A-i~:A "
            (sort (copy-list (g-edge-accessible e)) #'<)
            (sort (copy-list (g-edge-inaccessible e)) #'<))
         "")
      (mapcan
         #'(lambda (x) (if x (list (g-edge-id x))))
         (g-edge-children e))))


(defun print-generator-lookup-summary (lex-items grules)
  ;;
  ;; code formerly commented out in body of generate-from-mrs(); move here for
  ;; better readability of the main function.                  (19-apr-04; oe)
  ;;
  (dolist (lex lex-items)
    (format t "~%Id ~A, Index ~A, Lexical rules ~:A, Main rel sorts ~:A"
            (mrs::found-lex-lex-id lex)
            #+:gen-index
            (gen-chart-dag-index
             (existing-dag-at-end-of
              (tdfs-indef (mrs::found-lex-inst-fs lex)) *semantics-index-path*)
             nil)
            #-:gen-index *toptype*
            (mrs::found-lex-rule-list lex)
            (mapcar #'mrs::rel-pred (mrs::found-lex-main-rels lex))))
  (print
   (sort (remove-duplicates (mapcar #'mrs::found-lex-lex-id lex-items))
         #'string-lessp))
  (finish-output)
  (dolist (grule grules)
    (when (mrs::found-rule-p grule)
      (format t "~%Id ~A, Index ~A, Main rel sorts ~:A"
              (mrs::found-rule-id grule)
              #+:gen-index
              (gen-chart-dag-index
               (existing-dag-at-end-of
                (tdfs-indef (mrs::found-rule-full-fs grule)) 
                *semantics-index-path*)
               nil)
              #-:gen-index *toptype*
              (mapcar #'mrs::rel-pred (mrs::found-rule-main-rels grule))))))


;;; End of file
