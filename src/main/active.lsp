;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: active.lsp
;;;      module: experimental key-driven ((hyper-)active) parser for the LKB
;;;     version: 0.0 (16-jun-99)
;;;  written by: oe, coli saarbruecken
;;; last update: 18-aug-99
;;;  updated by: oe, coli saarbruecken
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :common-lisp-user)

;;;
;;; experimental active parser; tries to minimize changes to existing LKB code
;;; --- will need some reorganization, should it prove feasible.
;;;
;;; among other things, task priority computation is naive as it stands: since
;;; lexical processing is done separately (using the existing LKB procedures),
;;; priority computation in the active (key-driven) parser currently ignores
;;; whatever lexical priorities were assigned to words.  i guess one would have
;;; to add a slot to the edge structure to pass up priority values (and then
;;; call a user-supplied function to compute the task priority from the 
;;; arguments, i.e. rule or active plus passive item).
;;;

;;;
;;; while playing with various strategies, some of the extra functionality
;;; over vanilla active parsing is conditionalized with features, so that we
;;; can obtain precise profiles without interference.  the conditionals make
;;; the code awkword to read, though, and should ultimately disappear.
;;;
(eval-when #+:ansi-eval-when (:load-toplevel :compile-toplevel :execute)
           #-:ansi-eval-when (load eval compile)
  (pushnew :agenda *features*)
  (pushnew :packing *features*)
  (pushnew :hyper-activity *features*))

(defparameter *hyper-activity-p* t)

#+:packing
(defparameter *chart-packing-p* nil)

(defstruct (active-chart-configuration (:include chart-configuration))
  open
  forwardp
  done
  #+:adebug
  (executions 0)
  #+:adebug
  (successes 0))

(defvar *achart* (make-array (list *chart-limit* 2)))

#+:agenda
(defparameter *aagenda* (make-heap))

(defmacro yadu! (tdfs1 tdfs2 path)
  `(yadu ,tdfs1 (create-temp-parsing-tdfs ,tdfs2 ,path)))

(defmacro passives-by-start (position)
  `(let ((entry (aref *chart* ,position 1)))
     (and entry (chart-entry-configurations entry))))

(defmacro passives-by-end (position)
  `(let ((entry (aref *chart* ,position 0)))
     (and entry (chart-entry-configurations entry))))

(defmacro actives-by-start (position)
  `(aref *achart* ,position 1))

(defmacro actives-by-end (position)
  `(aref *achart* ,position 0))

(defmacro passives-delete (configuration)
  `(let* ((start (chart-configuration-begin ,configuration))
          (end (chart-configuration-end ,configuration))
          (by-start (aref *chart* start 1))
          (by-end (aref *chart* end 0)))
     (when by-start
       (setf (chart-entry-configurations by-start)
         (delete ,configuration 
                 (chart-entry-configurations by-start)
                 :test #'eq :count 1)))
     (when by-end
       (setf (chart-entry-configurations by-end)
         (delete ,configuration 
                 (chart-entry-configurations by-end)
                 :test #'eq :count 1)))))

(defun complete-chart (begin end)

  (let ((*active-edge-id* 0)
        (*minimal-vertex* begin)
        (*maximal-vertex* end))
    (declare (special *active-edge-id* *minimal-vertex* *maximal-vertex*))
    ;;
    ;; initialize *aagenda* through postulation of rules over all passive
    ;; items obtained from regular LKB lexical processing (i.e. words);
    ;; the active parser will share the LKB chart but use its own agenda
    ;; (storing tasks).  additionally, we have to initialize the second
    ;; *chart* dimension indexed by start positions, but not duplicate
    ;; the entry in the first dimension; therefore the second argument to
    ;; fundamental4passive(). 
    ;;
    (loop 
        for i from 0 to (1- *chart-limit*) do 
          (setf (actives-by-start i) nil (actives-by-end i) nil))
    #+:agenda
    (flush-heap *aagenda*)
    (loop
        for i from 0 to end do
          (loop
              for passive in (passives-by-end i) do
                (fundamental4passive passive t)))
    ;;
    ;; now run the main parser loop: until we empty the agenda (or hell
    ;; freezes over) apply the fundamental rule of chart parsing.
    ;;
    #+:agenda
    (loop
        until (empty-heap *aagenda*)
        for task = (heap-extract-max *aagenda*)
        when (rule-p (first task)) do
          (process-rule-and-passive task)
        else do
          (process-active-and-passive task))))

(defvar *active-edge-id* 0)
(declaim (type fixnum *active-edge-id*))

(defun next-active-edge ()
  (when (> (abs *active-edge-id*) *maximum-number-of-active-edges*)
     (error "next-active-edge(): ~
             edge limit exhausted (see *maximum-number-of-active-edges*)"))
  (decf *active-edge-id*))

(defun postulate (passive)
  (declare (special *minimal-vertex* *maximal-vertex*))

  #+:adebug
  (print-trace :postulate passive)
  
  ;;
  ;; create parsing tasks for each rule that could fire with .passive. in its
  ;; key daughter.
  ;;
  (loop
      with edge = (chart-configuration-edge passive)
      with begin = (chart-configuration-begin passive)
      with end = (chart-configuration-end passive)
      for rule in *parser-rules*
      for rhs = (rule-rhs rule)
      for open = (rest rhs)
      for key = (first rhs)
      unless (and open 
                  (if (< (first open) key)
                    (= begin *minimal-vertex*) 
                    (= end *maximal-vertex*)))
      do
        (if (and (check-rule-filter rule (edge-rule edge) key)
                 (restrictors-compatible-p
                  (nth key (rule-daughters-restricted rule))
                  (edge-dag-restricted edge)))
          #+:agenda
          (heap-insert *aagenda* (rule-priority rule) (cons rule passive))
          #-:agenda
          (process-rule-and-passive rule passive)
          (incf *filtered-tasks*))))

(defun fundamental4active (active)

  #+:adebug
  (print-trace :fundamental4active active)
  
  (let ((begin (chart-configuration-begin active))
        (end (chart-configuration-end active))
        #+:hyper-activity 
        (done (active-chart-configuration-done active)))
    ;;
    ;; add .active. to active chart (indexed by start and end vertex)
    ;;
    (push active (actives-by-start begin))
    (push active (actives-by-end end))
    ;;
    ;; try to combine with adjacent passive edges (on the side that we want to
    ;; fill next) and create new tasks for combinations that pass the filter.
    ;;
    (loop
        with aedge = (chart-configuration-edge active)
        with arule = (edge-rule aedge)
        with key = (first (active-chart-configuration-open active))
        with avector = (edge-dag-restricted aedge)
        for passive in (if (active-chart-configuration-forwardp active)
                         (passives-by-start end)
                         (passives-by-end begin))
        for pedge = (chart-configuration-edge passive)
        unless (or #+:hyper-activity (eq passive done)
                   #+:packing (edge-frozen pedge)) do
          (if (and (check-rule-filter arule (edge-rule pedge) key)
                   (restrictors-compatible-p
                    avector
                    (edge-dag-restricted pedge)))
            #+:agenda
            (heap-insert *aagenda* (rule-priority arule) (cons active passive))
            #-:agenda
            (process-active-and-passive active passive)
            (incf *filtered-tasks*)))))

(defun fundamental4passive (passive &optional oldp)
  (declare (special *minimal-vertex* *maximal-vertex*))

  #+:adebug
  (print-trace :fundamental4passive passive)
  
  (let* ((pedge (chart-configuration-edge passive))
         (prule (edge-rule pedge))
         (pvector (edge-dag-restricted pedge))
         (begin (chart-configuration-begin passive))
         (end (chart-configuration-end passive))
         (preceding (actives-by-end begin))
         (following (actives-by-start end)))
    ;;
    ;; add .passive. to chart (indexed by start and end vertex); .oldp is only
    ;; used in initialization to prevent duplication of passive edges obtained
    ;; from (regular) LKB lexical processing (array handling is awkward here).
    ;;
    (unless oldp
      (if (aref *chart* end 0)
        (push passive (chart-entry-configurations (aref *chart* end 0)))
        (setf (aref *chart* end 0)
          (make-chart-entry :configurations (list passive)))))
    (if (aref *chart* begin 1)
      (push passive (chart-entry-configurations (aref *chart* begin 1)))
      (setf (aref *chart* begin 1)
        (make-chart-entry :configurations (list passive))))
    ;;
    ;; check to see whether .passive. is a complete parsing result; trigger
    ;; non-local exit when the maximal number of readings (to compute) has
    ;; been reached.
    ;;
    (when (and *maximal-number-of-readings*
               (= begin *minimal-vertex*) (= end *maximal-vertex*))
      (let ((result (find-spanning-edge passive begin end)))
        (when result
          (push (get-internal-run-time) *parse-times*)
          (setf *parse-record* (nconc result *parse-record*))
          (when (zerop (decf *maximal-number-of-readings*))
            (throw :best-first t)))))
    ;;
    ;; create new tasks through postulation of rules over .passive.
    ;;
    (postulate passive)
    ;;
    ;; try to combine .passive. with left- and right-adjacent active edges that
    ;; want to extend in the right direction and pass the filter(s).
    ;;
    (loop
        for active in preceding
        when (active-chart-configuration-forwardp active) do
          (let* ((aedge (chart-configuration-edge active))
                 (key (first (active-chart-configuration-open active))))
            (if (and (check-rule-filter (edge-rule aedge) prule key)
                     (restrictors-compatible-p
                      (edge-dag-restricted aedge)
                      pvector))
              #+:agenda
              (heap-insert *aagenda* 
               (rule-priority prule) (cons active passive))
              #-:agenda
              (process-active-and-passive active passive)
              (incf *filtered-tasks*))))
    (loop
        for active in following
        unless (active-chart-configuration-forwardp active) do
          (let* ((aedge (chart-configuration-edge active))
                 (key (first (active-chart-configuration-open active))))
            (if (and (check-rule-filter (edge-rule aedge) prule key)
                     (restrictors-compatible-p
                      (edge-dag-restricted aedge)
                      pvector))
              #+:agenda
              (heap-insert *aagenda* 
               (rule-priority prule) (cons active passive))
              #-:agenda
              (process-active-and-passive active passive)
              (incf *filtered-tasks*))))))

#+:packing
(defun packed-edge-p (start end edge)
  (loop
      with dag = (tdfs-indef (edge-dag edge))
      for configuration in (passives-by-start start)
      when (= (chart-configuration-end configuration) end) do
        (let* ((oedge (chart-configuration-edge configuration))
               (odag  (tdfs-indef (edge-dag oedge))))
          (multiple-value-bind (forwardp backwardp)
              (dag-subsumes-p odag dag)
            (when forwardp
              #+:pdebug
              (format 
               t 
               "~&packed-edge-p(): ~
                packing <~d> into <~d>.~%"
               (edge-id edge) (edge-id oedge))
              (push edge (edge-packed oedge))
              (incf *packings*)
              (return configuration))
            (when backwardp
              #+:pdebug
              (format 
               t 
               "~&packed-edge-p(): ~
                ~:[~;(re)~]packing <~d> into <~d> (backwards).~%"
               (edge-frozen oedge) (edge-id oedge) (edge-id edge))
              (setf (edge-packed edge) (edge-packed oedge))
              (passives-delete configuration)
              (unless (edge-frozen oedge)
                (push oedge (edge-packed edge))
                (labels ((freeze (edge)
                           (unless (edge-frozen edge)
                             (setf (edge-frozen edge) t)
                             (loop 
                                 for parent in (edge-parents edge) do
                                   (freeze parent)))))
                  (freeze oedge))))))
      finally (return nil)))

(defun process-rule-and-passive #+:agenda (task)
                                #-:agenda (rule passive)

  #+:adebug
  (print-trace :process-rule-and-passive 
               #+:agenda (first task) #-:agenda rule)
  #+:adebug
  (print-trace :process-rule-and-passive 
               #+:agenda  (rest task) #-:agenda passive)
  #+:packing
  (when (edge-frozen (chart-configuration-edge #+:agenda (rest task)
                                               #-:agenda passive))
    (return-from process-rule-and-passive nil))
  
  (let* (#+:agenda (rule (first task))
         (rtdfs (rule-full-fs rule))
         (rhs (rule-rhs rule)) (open (rest rhs)) (key (first rhs))
         (daughters (rest (rule-order rule))) (path (nth key daughters))
         #+:agenda (passive (rest task))
         (edge (chart-configuration-edge passive)) (ptdfs (edge-dag edge))
         #+:hyper-activity (generation *unify-generation*)
         #+:hyper-activity atdfs
         nedge)
    (with-unification-context (ignore)
      (incf *executed-tasks*)
      (let* ((tdfs (yadu! rtdfs ptdfs path)))
        (when tdfs
          (let* ((root (tdfs-at-end-of (first (rule-order rule)) tdfs))
                 (vector (if open
                           (tdfs-qc-vector
                            tdfs (nth (first open) daughters))
                           (tdfs-qc-vector root)))
                 (copy (if open
                         (if *hyper-activity-p*
                           t
                           (copy-tdfs-elements tdfs))
                         (restrict-and-copy-tdfs root))))
            (when copy
              #+:hyper-activity
              (setf atdfs tdfs)
              (setf nedge
                (make-edge :id (if open (next-active-edge) (next-edge))
                           :category (indef-type-of-tdfs
                                      (if (eq copy t) tdfs copy))
                           :rule rule :children (list edge)
                           :dag copy :dag-restricted vector
                           :lex-ids (copy-list (edge-lex-ids edge))
                           :leaves (copy-list (edge-leaves edge)))))))))
    (when nedge
      (incf *successful-tasks*)
      (let ((begin (chart-configuration-begin passive))
            (end (chart-configuration-end passive)))
        (cond
         (open
          (let* ((forwardp (< key (first open)))
                 (passive #+:hyper-activity
                          (when *hyper-activity-p*
                            (loop
                                with key = (first open)
                                for passive in (if forwardp 
                                                 (passives-by-start end)
                                                 (passives-by-end begin))
                                for pedge = (chart-configuration-edge passive)
                                when (and #+:packing 
                                          (null (edge-frozen pedge))
                                          (check-rule-filter 
                                           rule (edge-rule pedge) key)
                                          (restrictors-compatible-p
                                           (edge-dag-restricted nedge)
                                           (edge-dag-restricted pedge)))
                                return passive)
                            #-:hyper-activity
                            nil))
                 (active (make-active-chart-configuration 
                          :begin begin :end end :edge nedge
                          :open open :forwardp forwardp
                          :done passive)))
            ;;
            ;; this is _truly_ experimental: to take full advantage of that
            ;; hyper-active strategy, we want to extend a new active edge
            ;; right after it was built.  hence, reset the generation counter
            ;; to what was in effect when .nedge. was derived.
            ;;
            #+:hyper-activity
            (when passive
              #+:adebug
              (print-trace :extend passive)
              (let ((*unify-generation* generation))
                #+:agenda
                (process-active-and-passive (cons active passive) atdfs)
                #-:agenda
                (process-active-and-passive active passive atdfs)))
            (fundamental4active active)))
         (t
          (unless #+:packing
                  (and *chart-packing-p* (packed-edge-p begin end nedge))
                  #-:packing
                  nil
            (push nedge (edge-parents edge))
            (fundamental4passive
             (make-chart-configuration
              :begin begin :end end :edge nedge)))))))))

(defun process-active-and-passive #+:agenda (task &optional atdfs)
                                  #-:agenda (active passive &optional atdfs)
  
  #+:adebug
  (print-trace :process-active-and-passive 
               #+:agenda (first task) #-:agenda active)
  #+:adebug
  (print-trace :process-active-and-passive 
               #+:agenda (rest task) #-:agenda passive)

  #+:packing
  (when (edge-frozen (chart-configuration-edge #+:agenda (rest task)
                                               #-:agenda passive))
    (return-from process-active-and-passive nil))

  (let* (#+:agenda (active (first task))
         (key (first (active-chart-configuration-open active)))
         (open (rest (active-chart-configuration-open active)))
         (forwardp (active-chart-configuration-forwardp active))
         (aedge (chart-configuration-edge active))
         (achildren (edge-children aedge))
         (atdfs (or atdfs (edge-dag aedge))) (arule (edge-rule aedge))
         (daughters (rest (rule-order arule))) (path (nth key daughters))
         #+:agenda (passive (rest task))
         (pedge (chart-configuration-edge passive)) (ptdfs (edge-dag pedge))
         (nedge
          (with-unification-context (ignore)
            (incf *executed-tasks*)
            (when (eq atdfs t)
              #+:adebug
              (print-trace :reconstruct active)
              (loop
                  initially (setf atdfs (rule-full-fs arule))
                  for edge in achildren
                  for tdfs = (edge-dag edge)
                  for i in (rule-rhs arule)
                  for path = (nth i daughters)
                  do
                    (incf *executed-tasks*)
                    (setf atdfs 
                      (yadu! atdfs tdfs path))
                    (incf *successful-tasks*)))
            #+:adebug
            (incf (active-chart-configuration-executions active))
            (let* ((tdfs (yadu! atdfs ptdfs path)))
              (when tdfs
                (let* ((root (tdfs-at-end-of (first (rule-order arule)) tdfs))
                       (vector (if open
                                 (tdfs-qc-vector 
                                  tdfs (nth (first open) daughters))
                                 (tdfs-qc-vector root)))
                       (copy (if open
                               (if *hyper-activity-p*
                                 t
                                 (copy-tdfs-elements tdfs))
                               (restrict-and-copy-tdfs root))))
                  (when copy
                    (let* ((category (indef-type-of-tdfs 
                                      (if (eq copy t) tdfs copy)))
                           (children (if forwardp
                                       (append achildren (list pedge))
                                       (cons pedge achildren)))
                           (ids (loop 
                                    for child in children 
                                    append (edge-lex-ids child)))
                           (leaves (loop 
                                       for child in children 
                                       append (edge-leaves child))))
                      (make-edge :id (if open (next-active-edge) (next-edge))
                                 :category category :rule arule
                                 :children children
                                 :dag copy :dag-restricted vector
                                 :lex-ids ids :leaves leaves)))))))))
    (when nedge
      (incf *successful-tasks*)
      #+:adebug
      (incf (active-chart-configuration-successes active))
      (let* ((begin (if forwardp
                      (chart-configuration-begin active)
                      (chart-configuration-begin passive)))
             (end (if forwardp
                    (chart-configuration-end passive)
                    (chart-configuration-end active))))
        (cond
         (open
          (fundamental4active 
           (make-active-chart-configuration 
            :begin begin :end end :edge nedge
            :open open :forwardp (< key (first open)))))
         (t
          (unless #+:packing
                  (and *chart-packing-p* (packed-edge-p begin end nedge))
                  #-:packing
                  nil
            (push nedge (edge-parents pedge))
            (fundamental4passive
             (make-chart-configuration
              :begin begin :end end :edge nedge)))))))))

(defun restrict-and-copy-tdfs (tdfs)
  (let* ((dag (deref-dag (tdfs-indef tdfs)))
         (new (clone-dag dag))
         (arcs nil))
    (flet ((member-with-cyclic-check (arc)
             (when (member (dag-arc-attribute arc) 
                           *deleted-daughter-features* :test #'eq)
               (push arc arcs))))
      (setf (dag-arcs new)
        (remove-if #'member-with-cyclic-check (dag-arcs new)))
      (setf (dag-comp-arcs new)
        (remove-if #'member-with-cyclic-check (dag-comp-arcs new)))
      (let ((result
             (unless (cyclic-dag-p (make-dag :type *toptype* :arcs arcs))
               (let ((copy (copy-dag new)))
                 (and copy 
                      (make-tdfs :indef copy 
                                 :tail (copy-list (tdfs-tail tdfs))))))))
        (unless result (decf *successful-tasks*))
        result))))

(defun tdfs-qc-vector (tdfs &optional path)
  (let* ((dag (x-existing-dag-at-end-of (deref-dag (tdfs-indef tdfs)) path))
         (vector (x-restrict-fs dag)))
    #+:adebug
    (when *adebugp*
      (format t "tdfs-qc-vector(): ~s;~%" vector))
    vector))

#+:adebug
(defparameter *adebugp* t)

#+:adebug
(defun debug-label (object)
  (cond
   ((chart-configuration-p object)
    (debug-label (chart-configuration-edge object)))
   ((edge-p object)
    (format 
     nil 
     "~(~a~)" 
     (if (rule-p (edge-rule object))
       (rule-id (edge-rule object)) 
       (edge-rule object))))
   ((stringp object) object)
   ((rule-p object) (rule-id object))
   (t "unknown")))

#+:adebug
(defun print-trace (context object)
  (when *adebugp*
    (if (rule-p object)
      (let* ((label (debug-label object))
             (open (rule-rhs object)))
        (format
         t
         "~(~a~)(): `~(~a~)' [open: ~{~a~^ ~}];~%"
         context label open))
      (let* ((begin (chart-configuration-begin object))
             (end (chart-configuration-end object))
             (label (debug-label object))
             (edge (chart-configuration-edge object))
             (id (edge-id edge))
             (children (loop 
                           for child in (edge-children edge)
                           collect (edge-id child)))
             (open (when (active-chart-configuration-p object)
                     (active-chart-configuration-open object)))
             (forwardp (when (active-chart-configuration-p object)
                         (active-chart-configuration-forwardp object))))
        (format 
         t
         "~(~a~)(): ~d-~d: ~a (~a < ~{~a~^ ~})~
          ~@[ [open: ~{~a~^ ~} - ~:[backwards~;forward~]]~];~%"
         context begin end label id children open forwardp)))))

#+:cray
(defun unpack-edge (edge &optional insidep)
  (let ((children (edge-children edge)))
    (cond
     ((and (edge-packed edge) (null insidep))
      (nconc (unpack-edge edge t)
             (loop
                 for edge in (edge-packed edge)
                 nconc (unpack-edge edge))))
     ((null children)
      (list (edge-dag edge)))
     (t
      (loop
          with daughters = (loop
                               for edge in children
                               collect (unpack-edge edge))
          with decompositions = (cross-product daughters)
          with rule = (edge-rule edge)
          with rtdfs = (rule-full-fs rule)
          with paths = (rest (rule-order rule))
          for decomposition in decompositions
          for instantiation = (with-unification-context (ignore)
                                (loop
                                    with result = rtdfs
                                    for path in paths
                                    for tdfs in decomposition 
                                    while result do
                                      (setf result (yadu! result tdfs path))
                                    finally 
                                      (return (when result
                                                (restrict-and-copy-tdfs 
                                                 result)))))
          when instantiation collect instantiation)))))

;;;
;;; the unfolding of packed edges still takes substantially more time than we
;;; would like it to.  this version behaves exactly like the existing code for
;;; tree drawing (modulo the matching of nodes against tree node labels).
;;;
;;; once thing we should try is to maintain a reference counter on rules (per
;;; derivation); then, only rules that were used more than just once, had to be
;;; copied out.  however, it is not obvious how (and whether) the counting
;;; could be done in just one pass.
;;;

(defun unpack-edge (edge)
  ;;
  ;; unfold tree structure below .edge.  we have to do two passes to be able
  ;; to instantiate each resulting derivation within just one (big) unification
  ;; context; since the edges may share children, each recursive level would
  ;; have to return copies otherwise.
  ;;
  (labels ((unpack-edge! (edge &optional insidep)
             (let ((children (edge-children edge)))
               (cond
                ((and (edge-packed edge) (null insidep))
                 (nconc (unpack-edge! edge t)
                        (loop
                            for edge in (edge-packed edge)
                            nconc (unpack-edge! edge))))
                ((null children)
                 (list edge))
                (t
                 (loop
                     with daughters = (loop
                                          for edge in children
                                          collect (unpack-edge! edge))
                     with decompositions = (cross-product daughters)
                     for decomposition in decompositions
                     collect (cons edge decomposition))))))
           (instantiate (derivation)
             (if (edge-p derivation)
               ;;
               ;; a leaf node: for now ignore spelling change (`morph-history')
               ;; and just copy the corresponding feature structure; if lexical
               ;; rules delete daughter features (which they probably do), then
               ;; this should be refined some day.           (19-jul-99  -  oe)
               ;;
               (copy-tdfs-completely (edge-dag derivation))
               ;;
               ;; descend recursively into all children; then reapply the rule
               ;; originally used to build this edge to all resulting feature
               ;; structures; needs to copy rule because it may be used more
               ;; than just once in this derivation.
               ;;
               (let ((edge (first derivation))
                     (children (rest derivation)))
                 (loop
                     with rule = (edge-rule edge)
                     with paths = (rest (rule-order rule))
                     with result = (copy-tdfs-completely (rule-full-fs rule))
                     for child in children
                     for tdfs = (instantiate child)
                     for path in paths
                     unless tdfs return nil
                     while result do
                       (setf result (yadu! result tdfs path))
                     finally (return result))))))
    ;;
    ;; first unfold all packed nodes into a list of derivations; a derivation
    ;; is represented as a list of edges here, where the first() is the root
    ;; node at each level
    ;;
    (let ((derivations (unpack-edge! edge)))
      (loop
          for derivation in derivations 
          for instantiation = (with-unification-context (ignore)
                                (let ((result (instantiate derivation)))
                                  (when result
                                    (copy-tdfs-elements result))))
          when instantiation collect instantiation))))

(defstruct extension
  id
  position
  path
  edge)

#+:fixme
(defun validate-edge (edge)
  (labels ((embed (extensions path)
             (cond
              ((extension-p extensions)
               (setf (extension-path extensions)
                 (append path (extension-path extensions))))
              ((consp extensions)
               (embed (first extensions) path)
               (embed (first extensions) path))))
           (instantiate (edge)
             (if (edge-children edge)
               (let* ((rule (edge-rule edge))
                      (paths (rest (rule-order rule)))
                      (result (copy-tdfs-completely (rule-full-fs rule)))
                      extensions)
                 (loop
                     for child in (edge-children edge)
                     for path in paths
                     while result do
                       (multiple-value-bind (tdfs overlays)
                           (instantiate child)
                         (setf result (yadu! result tdfs path))
                         (when overlays
                           (embed overlays path)
                           (push overlays extensions))))
                 (let* ((id (edge-id edge))
                        (overlays
                         (loop
                             for edge in (edge-packed edge)
                             for i from 0
                             for overlay = (make-extension
                                            :id id :position i 
                                            :path nil :edge edge)
                             collect (list overlay))))
                   (unless result
                     (error 
                      "instantiate(): ~
                       reconstruction of edge # ~d failed."
                      id))
                   (values result 
                           (cross-product (cons overlays extensions)))))
               (copy-tdfs-completely (edge-dag edge)))))
    (let (skeleton extensions)
      (with-unification-context (ignore)
        (multiple-value-bind (result overlays)
            (instantiate edge)
          (setf skeleton (copy-tdfs-elements result))
          (setf extensions overlays)))
      (values skeleton extensions))))

(defun cross-product (lists)
  ;;
  ;; ((a b) (1 2)) --> ((A 1) (A 2) (B 1) (B 2))
  ;;
  (if (null (rest lists))
    (loop
        for foo in (first lists) collect (list foo))
    (loop
        with rests = (cross-product (rest lists))
        for foo in (first lists)
        nconc (loop
                  for list in rests
                  collect (cons foo list)))))
