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
  (pushnew :excursion *features*)
  (pushnew :hyper-activity *features*))

(defparameter *hyper-activity-p* t)

#+:packing
(defparameter *chart-packing-p* nil)

;;;
;;; chart packing under (partial) subsumption is experimental; known problems
;;;
;;;   - because packing is only attempted within the active parser, it cannot 
;;;     (yet) apply to lexical processing; thus, similar lexical items may well
;;;     proliferate and make life harder than necessary.
;;;
;;;   - when computing the rule filter and quick check vectors, the paths that
;;;     are ignored in packing mode (i.e. in subsumption and unification) need
;;;     to be excluded.

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

(defmacro rule-and-passive-task (rule passive)
  #+:agenda
  `(if *maximal-number-of-readings*
     (let ((priority (rule-priority ,rule)))
       (heap-insert *aagenda* priority (cons ,rule ,passive)))
     (process-rule-and-passive (cons ,rule ,passive)))
  #-:agenda
  `(process-rule-and-passive ,rule ,passive))

(defmacro active-and-passive-task (active passive arule)
  #+:agenda
  `(if *maximal-number-of-readings*
     (let ((priority (rule-priority ,arule)))
       (heap-insert *aagenda* priority (cons ,active ,passive)))
     (process-active-and-passive (cons ,active ,passive)))
  #-:agenda
  `(process-active-and-passive ,active ,passive))
  

(defun complete-chart (begin end &optional (packingp *chart-packing-p*))

  (let ((*active-edge-id* 0)
        (*minimal-vertex* begin)
        (*maximal-vertex* end)
        (*partial-dag-interpretation* (and packingp '(cont))))
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
    ;; the agenda is only used in exhaustive mode, though.  for more complex
    ;; input it can increase run-time measureably.      (27-sep-99  --  oe)
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
    (when *maximal-number-of-readings*
      (loop
          until (empty-heap *aagenda*)
          for task = (heap-extract-max *aagenda*)
          when (rule-p (first task)) do
            (process-rule-and-passive task)
          else do
            (process-active-and-passive task)))))

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
          (rule-and-passive-task rule passive)
          (incf *filtered-tasks*))))

(defun fundamental4active (active)

  #+:adebug
  (print-trace :fundamental4active active)
  
  (let ((begin (chart-configuration-begin active))
        (end (chart-configuration-end active))
        #+(and :hyper-activity :excursion)
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
        unless (or #+(and :hyper-activity :excursion) (eq passive done)
                   #+:packing (edge-frozen pedge)) do
          (if (and (check-rule-filter arule (edge-rule pedge) key)
                   (restrictors-compatible-p
                    avector
                    (edge-dag-restricted pedge)))
            (active-and-passive-task active passive arule)
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
              (active-and-passive-task active passive (edge-rule aedge))
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
              (active-and-passive-task active passive (edge-rule aedge))
              (incf *filtered-tasks*))))))

;;;
;;; packing is substantially more complex in retroactive mode: when a new edge
;;; (.edge.) is found to subsume an existing edge (.oedge.), we need to
;;;
;;;   - pack .oedge. into .edge. (unless .oedge. is frozen, see below);
;;;   - raise all packings from .oedge. into .edge.;
;;;   - delete .oedge. from the chart;
;;;   - make sure .oedge. will not be processed further (including pending 
;;;     tasks on the agenda that involve .oedge.);
;;;   - block further processing for all edges that were already derived from
;;;     .oedge. (since .edge. is guaranteed to derive host edges for them).
;;;
;;; given the `release' (jul-99) grammar and the input `then, i guess that is
;;; settled' we get the following situation:
;;;
;;;   [62] { [158 < 157; 150 < 149] and [157] { [149 < 74 148]
;;;
;;; because [150] is packed already when [149] gets packed retroactively.  this
;;; situation requires that we further complicate our notion of edge freezing.
;;;
;;; to block processing of edges and their derivatives when they are packed
;;; retroactively, the `frozen' slot in the edge structure is used.  any edge
;;; that was frozen is ignored by the parser.  however, in the unpacking phase
;;; [149] still represents a valid reading, while [150] is a spurious duplicate
;;; of the tree [158 < 149].  hence, the following distinction is made:
;;;
;;;   - edges that were frozen to block further processing because they were
;;;     packed retroactively;
;;;   - edges that were globally invalidated because one of their childres was
;;;     packed retroactively.
;;;
;;; the second class is marked with a negative `frozen' value and ignored in
;;; the unpacking phase.  this was hard to debug.          (17-sep-99  -  oe)
;;;
#+:packing
(defun packed-edge-p (start end edge)
  (labels ((freeze (edge id &optional recursivep)
             (when (or (null (edge-frozen edge))
                       (plusp (edge-frozen edge))
                       (minusp id))
               #+:pdebug
               (format
                t
                "~&freeze(): freezing <~d> for <~d>.~%"
                (edge-id edge) id)
               (setf (edge-frozen edge) id))
             (loop 
                 with id = (if recursivep id (- id))
                 for parent in (edge-parents edge) do
                   (freeze parent id t))))
    (loop
        with dag = (tdfs-indef (edge-dag edge))
        for configuration in (passives-by-start start)
        when (= (chart-configuration-end configuration) end) do
          (let* ((oedge (chart-configuration-edge configuration))
                 (odag  (tdfs-indef (edge-dag oedge))))
            (multiple-value-bind (forwardp backwardp)
                (dag-subsumes-p odag dag)
              (when (and forwardp (null (edge-frozen oedge)))
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
                (setf (edge-packed edge) 
                  (nconc (edge-packed edge) (edge-packed oedge)))
                (setf (edge-packed oedge) nil)
                (passives-delete configuration)
                (unless (edge-frozen oedge)
                  (push oedge (edge-packed edge)))
                (freeze oedge (edge-id edge)))))
        finally (return nil))))

(defun process-rule-and-passive #+:agenda (task)
                                #-:agenda (rule passive)

  #+:adebug
  (print-trace :process-rule-and-passive 
               #+:agenda (first task) #-:agenda rule
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
         #+(and :hyper-activity :excursion) (generation *unify-generation*)
         #+(and :hyper-activity :excursion) atdfs
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
              #+(and :hyper-activity :excursion)
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
                 (passive #+(and :hyper-activity :excursion)
                          (when  *hyper-activity-p*
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
                            #-(and :hyper-activity :excursion)
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
            #+(and :hyper-activity :excursion)
            (when passive
              #+:adebug
              (print-trace :extend active passive)
              (let ((*unify-generation* generation))
                #+:agenda
                (process-active-and-passive (cons active passive) atdfs)
                #-:agenda
                (process-active-and-passive active passive atdfs)))
            (fundamental4active active)))
         (t
          #+:packing
          (when *chart-packing-p*
            (loop
                for edge in (edge-children nedge) do
                  (push nedge (edge-parents edge))))
          (unless #+:packing
                  (and *chart-packing-p* (packed-edge-p begin end nedge))
                  #-:packing
                  nil
            (fundamental4passive
             (make-chart-configuration
              :begin begin :end end :edge nedge)))))))))

(defun process-active-and-passive #+:agenda (task &optional atdfs)
                                  #-:agenda (active passive &optional atdfs)
  
  #+:adebug
  (print-trace :process-active-and-passive 
               #+:agenda (first task) #-:agenda active
               #+:agenda (rest task) #-:agenda passive)

  #+:packing
  (when (or (edge-frozen (chart-configuration-edge 
                          #+:agenda (rest task) #-:agenda passive))
            (loop
                for edge in (edge-children 
                             (chart-configuration-edge 
                              #+:agenda (first task) #-:agenda active))
                thereis (edge-frozen edge)))
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
                    (setf atdfs 
                      (yadu! atdfs tdfs path))))
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
          #+:packing
          (when *chart-packing-p*
            (loop
                for edge in (edge-children nedge) do
                  (push nedge (edge-parents edge))))
          (unless #+:packing
                  (and *chart-packing-p* (packed-edge-p begin end nedge))
                  #-:packing
                  nil
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
    #+:qcdebug
    (format t "tdfs-qc-vector(): ~s;~%" vector)
    vector))

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
(defun print-trace (context object &optional argument)
  (if (rule-p object)
    (let* ((label (debug-label object))
           (open (rule-rhs object)))
      (format
       t
       "~@[~(~a~)():~] `~(~a~)' [open: ~{~a~^ ~}]~:[;~%~; +~]"
       context label open argument))
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
       "~@[~(~a~)():~] ~d:~d ~a (~a < ~{~a~^ ~})~
        ~@[ [open: ~{~a~^ ~} - ~:[backwards~;forward~]]~]~:[;~%~; +~]"
       context begin end label id children open forwardp argument)))
  (when argument (print-trace nil argument)))

(defun cross-product (lists)
  (if (null (rest lists))
    (loop
        for foo in (first lists) collect (list foo))
    (loop
        with rests = (cross-product (rest lists))
        for foo in (first lists)
        nconc (loop
                  for bar in rests
                  collect (cons foo bar)))))

(defun unpack-edge! (edge &optional insidep)
  (labels ((instantiate (edge children i n)
             #+:udebug
             (format
              t
              "instantiate(): (~d < ~{~d~^ ~}~%"
              (edge-id edge)
              (loop
                  for tdfs in children
                  collect (dag-type (tdfs-indef tdfs))))
             (unless (vectorp (edge-dag-restricted edge))
               (setf (edge-dag-restricted edge) (make-array n)))
             (let* ((cache (edge-dag-restricted edge))
                    (entry (aref cache i)))
               (cond
                ((eq entry :bottom) nil)
                ((tdfs-p entry) entry)
                (t
                 (with-unification-context (ignore)
                   (loop
                       with rule = (edge-rule edge)
                       with paths = (rest (rule-order rule))
                       with result = (rule-full-fs rule)
                       for path in paths
                       for tdfs in children 
                       while result do
                         (setf result (yadu! result tdfs path))
                       finally
                         (return
                           (cond
                            (result
                             (setf (aref cache i)
                               (restrict-and-copy-tdfs result)))
                            (t
                             (setf (aref cache i) :bottom)
                             nil))))))))))

    (let ((children (edge-children edge))
          (morphology (edge-morph-history edge)))
      (cond
       ((and (edge-frozen edge) (minusp (edge-frozen edge)))
        #+:udebug
        (format
         t
         "~&unpack-edge!(): ignoring <~d> (frozen for <~d>)~%"
         (edge-id  edge) (edge-frozen edge))
        nil)
       ((and (edge-packed edge) (null insidep))
        (nconc (unpack-edge! edge t)
               (loop
                   for edge in (edge-packed edge)
                   nconc (unpack-edge! edge))))
       (morphology
        (loop
            with decompositions = (unpack-edge! morphology)
            with n = (length decompositions)
            for decomposition in decompositions
            for i from 0
            for instantiation = (instantiate edge (list decomposition) i n)
            when instantiation collect instantiation))
       (children
        (loop
            with daughters = (loop
                                 for edge in children
                                 collect (unpack-edge! edge))
            with decompositions = (cross-product daughters)
            with n = (length decompositions)
            for decomposition in decompositions
            for i from 0
            for instantiation = (instantiate edge decomposition i n)
            when instantiation collect instantiation))
       (t
        (list (edge-dag edge)))))))

;;;
;;; the unfolding of packed edges still takes substantially more time than we
;;; would like it to.  this version behaves exactly like the existing code for
;;; tree drawing (modulo the matching of nodes against tree node labels).
;;;
;;; one thing we should try is to maintain a reference counter on rules (per
;;; derivation); then, only rules that were used more than just once, had to be
;;; copied out.  however, it is not obvious how (and whether) the counting
;;; could be done in just one pass.
;;;
#+:cray
(defun unpack-edge (edge)
  ;;
  ;; unfold tree structure below .edge.  we have to do two passes to be able
  ;; to instantiate each resulting derivation within just one (big) unification
  ;; context; since the edges may share children, each recursive level would
  ;; have to return copies otherwise.
  ;;
  (let* ((*chart-packing-p* nil)
         (nrules 
          (+ (hash-table-count *lexical-rules*) (hash-table-count *rules*)))
         (counter (make-array nrules :element-type 'fixnum)))
    (labels ((unpack! (edge &optional insidep)
               (let ((children (edge-children edge))
                     (morphology (edge-morph-history edge)))
                 (cond
                  ((and (edge-frozen edge) (minusp (edge-frozen edge))) nil)
                  ((and (edge-packed edge) (null insidep))
                   (nconc (unpack! edge t)
                          (loop
                              for edge in (edge-packed edge)
                              nconc (unpack! edge))))
                  (children
                   (loop
                       with daughters = (loop
                                            for edge in children
                                            collect (unpack! edge))
                       with decompositions = (cross-product daughters)
                       for decomposition in decompositions
                       collect (cons edge decomposition)))
                  (morphology
                   (loop
                       with decompositions = (unpack! morphology)
                       for decomposition in decompositions
                       collect (list edge decomposition)))                
                  ((null children)
                   (list edge))
                  (t
                   (error 
                    "unpack!(): implausible edge # ~d." 
                    (edge-id edge))))))

             (count (action &optional rule)
               (if (eq action :reset)
                 (loop
                     for i from 0 to (- nrules 1) do
                       (setf (aref counter i) 0))
                 (let ((index (and (rule-p rule) (rule-apply-index rule))))
                   (cond
                    ((null index) 0)
                    ((eq action :add)
                     (incf (aref counter index)))
                    (t 
                     (aref counter index))))))

             (edge-label (edge)
               (if (rule-p (edge-rule edge))
                 (rule-id (edge-rule edge)) 
                 (first (edge-lex-ids edge))))

             (recode (tree &optional reset)
               (when reset (count :reset))
               (if (edge-p tree)
                 (list (edge-label tree))
                 (let ((edge (first tree))
                       (children (rest tree)))
                   (count :add (edge-rule edge))
                   (cons (edge-label edge)
                         (loop 
                             for edge in children
                             collect (recode edge))))))
             
             (instantiate (tree)
               (if (edge-p tree)
                 ;;
                 ;; a leaf node: for the time being ignore spelling change
                 ;; (`morph-history') and just copy the corresponding feature
                 ;; structure; if inflectional rules delete daughter features
                 ;; (which they probably do), then this should be refined some
                 ;; day soon.
                 ;;                                         (19-jul-99  -  oe)
                 ;;
                 (copy-tdfs-completely (edge-dag tree))
                 ;;
                 ;; descend recursively into all children; then reapply the
                 ;; rule originally used to build this edge to all resulting
                 ;; feature structures; needs to copy rule because it may be
                 ;; used more than just once in this derivation.
                 ;;
                 (let ((edge (first tree))
                       (children (rest tree)))
                   (loop
                       with rule = (edge-rule edge)
                       with fs = (rule-full-fs rule)
                       with paths = (rest (rule-order rule))
                       with count = (count :get rule)
                       with result = (if (> count 1)
                                       (copy-tdfs-completely fs)
                                       fs)
                       for child in children
                       for tdfs = (instantiate child)
                       unless tdfs return nil
                       for path in paths
                       while result do
                         (setf result (yadu! result tdfs path))
                       finally (return result)))))

             (rootp (tdfs)
               (loop 
                   for root in (if (listp *start-symbol*)
                                 *start-symbol*
                                 (list *start-symbol*))
                   for rtdfs = (get-tdfs-given-id root)
                   thereis (and rtdfs (yaduablep tdfs rtdfs)))))
      ;;
      ;; first unfold all packed nodes into a list of derivations; a derivation
      ;; is represented as a list of edges here, where the first() is the root
      ;; node at each level
      ;;
      (let ((trees (unpack! edge)))
        (loop
            for tree in trees for i from 0
            for derivation = (recode tree :reset)
            for instantiation = (with-unification-context (ignore)
                                  (let ((result (instantiate tree)))
                                    (when (and result
                                               (rootp result))
                                      result
                                      #+:foo
                                      (copy-tdfs-elements result))))
            when instantiation collect derivation)))))

#+:test
(let ((*active-parsing-p* t)
      (*show-parse-p* nil)
      (*first-only-p* nil)
      (*chart-packing-p* t)
      contemplated filtered executed successful packings)
  (time (multiple-value-setq (contemplated filtered
                              executed successful
                              packings)
          (do-parse-tty "so we will have an evening there to go over things. or relax.")))
  (format
   t
   "~&~d trees; ~d packings; ~d readings; ~d edges~%"
   (length *parse-record*)
   packings
   (if *chart-packing-p*
     (time
      (loop
          for edge in *parse-record*
          sum (length (unpack-edge edge))))
     (length *parse-record*))
   (tsdb::get-field :pedges (summarize-chart))))
