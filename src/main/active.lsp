;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LKB -*-


;;; Copyright (c) 2000--2002
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: active.lsp
;;;      module: experimental key-driven ((hyper-)active) parser for the LKB
;;;     version: 0.0 (16-jun-99)
;;;  written by: oe, coli saarbruecken
;;; last update: 20-jan-00
;;;  updated by: oe, coli saarbruecken
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :lkb)

;;;
;;; experimental active parser; tries to minimize changes to existing LKB code
;;; --- will need some reorganization, should it prove feasible.
;;;

;;;
;;; while playing with various strategies, some of the extra functionality
;;; over vanilla active parsing is conditionalized with features, so that we
;;; can obtain precise profiles without interference.  the conditionals make
;;; the code awkward to read, though, and should ultimately disappear.
;;;
(eval-when #+:ansi-eval-when (:load-toplevel :compile-toplevel :execute)
           #-:ansi-eval-when (load eval compile)
  (pushnew :agenda *features*)
  (pushnew :hyper-activity *features*)
  (pushnew :excursion *features*)
  #+:packing (pushnew :retroactivity *features*)
  #+:packing (pushnew :restrict *features*)
  #+(and :agenda :packing :naacl) (pushnew :priority *features*))

(defparameter *hyper-activity-p* t)

(defparameter *scoring-hook* nil)

;;;
;;; chart packing under (partial) subsumption is experimental; known problems
;;;
;;;   - because packing is only attempted within the active parser, it cannot 
;;;     (yet) apply to lexical processing; thus, similar lexical items may well
;;;     proliferate and make life harder than necessary.
;;;
;;;   - when computing the rule filter and quick check vectors, the paths that
;;;     are ignored in packing mode (i.e. in subsumption and unification) need
;;;     to be excluded (solved --- 23-oct-99 -- oe).
;;;

#+:packing
(defparameter *chart-packing-p* nil)

#+:packing
(defstruct packings
  (equivalent 0 :type fixnum)
  (proactive 0 :type fixnum)
  (retroactive 0 :type fixnum)
  (frozen 0 :type fixnum)
  (failures 0 :type fixnum))

#+:packing
(defparameter *packings* (make-packings))

#+:packing
(defun reset-packings (&optional (packings *packings*))
  (setf (packings-equivalent packings) 0)
  (setf (packings-proactive packings) 0)
  (setf (packings-retroactive packings) 0)
  (setf (packings-frozen packings) 0)
  (setf (packings-failures packings) 0))

(defstruct (active-chart-configuration (:include chart-configuration))
  open
  forwardp
  done
  #+:adebug
  (executions 0)
  #+:adebug
  (successes 0))

(defvar *active-edge-id* 0)
(defvar *maximum-number-of-active-edges* nil)
(declaim (type fixnum *active-edge-id*))

(defun next-active-edge ()
  (when (> (abs *active-edge-id*) 
           (or *maximum-number-of-active-edges* *maximum-number-of-edges*))
     (error "next-active-edge(): ~
             edge limit exhausted (see `~a')"
            (if *maximum-number-of-active-edges*
              "*maximum-number-of-active-edges*"
              "*maximum-number-of-edges*")))
  (decf *active-edge-id*))

(defvar *achart* (make-array (list *chart-limit* 2)))

(defun clear-achart (&optional (chart *achart*))
  (loop 
      for i from 0 to (1- *chart-limit*) do 
        (setf (aref chart i 0) nil (aref chart i 1) nil))
  (setf *active-edge-id* 0))

;;;
;;; add `chart reduction' support analoguous to PET (and first implemented in
;;; DFKI PAGE, following a proposal by Dr. Mueller).  given a set of pairs of
;;; paths into signs <source , target>+, after chart initialization (lexical
;;; look-up), check that for each lexical entry with a valid type at one of the
;;; source paths there is an item in the chart whose type at one of the target
;;; paths is compatible (i.e. unifiable) with the source type.  in other words,
;;; for all selectional restrictions, as found in a source path, some lexical
;;; entry must supply the appropriate value.  the `rely on' lexical entry will
;;; only survive if an `_on_rel_s' is provided somewhere in the chart.
;;;
(defparameter *chart-dependencies* nil)

(defun chart-dependencies-provided (tasks)
  (loop
      with paths = (loop 
                       for i from 0
                       for path in *chart-dependencies*
                       unless (evenp i) collect path into paths
                       finally (return paths))
      with results = (make-array (length paths) :initial-element nil)
      for task in tasks
      for configuration = (rest task)
      for edge = (chart-configuration-edge configuration)
      for tdfs = (edge-dag edge)
      for dag = (tdfs-indef tdfs)
      do
        (loop
            for path in paths
            for i from 0
            for key = (existing-dag-at-end-of dag path)
            for type = (and key (type-of-fs key))
            when type do (pushnew type (aref results i) :test #'eq))
      finally (return results)))

(defun chart-dependencies-p (edge paths provided)
  (loop
      with tdfs = (edge-dag edge)
      with dag = (tdfs-indef tdfs)
      for path in paths
      for i from 0
      for node = (existing-dag-at-end-of dag path)
      always (or (null node) 
                 (loop
                     with type = (type-of-fs node)
                     for foo in (aref provided i)
                     thereis (greatest-common-subtype foo type)))))
  
(defun reduce-chart ()
  
  (labels ((freeze (edge id)
             #+:rdebug
             (format t "~&freeze(): ~a~%" edge)
             (setf (edge-frozen edge) id)
             (loop 
                 for parent in (edge-parents edge) do (freeze parent id))))

    (loop
        with tasks = (loop
                         until (empty-heap *agenda*) 
                         for task = (heap-extract-max-full *agenda*)
                         when (chart-configuration-p (rest task) )
                         collect task
                         else do
                           (error 
                            "reduce-chart(): ~
                             invalid non-lexical task on agenda upon entry."))
        with paths = (loop 
                         for i from 0
                         for path in *chart-dependencies*
                         unless (or (oddp i) (member path paths :test #'equal))
                         collect path into paths
                         finally (return paths))
        with provided = (chart-dependencies-provided tasks)
        for task in tasks
        for configuration = (rest task)
        for edge = (chart-configuration-edge configuration)
        for happyp = (chart-dependencies-p edge paths provided)
        unless happyp do (freeze edge -1)
        else collect task into survivors
        finally
          (setf *chart-dependencies* nil)
          (loop
              for survivor in survivors
              do (lexical-task (first survivor) (rest survivor))))))

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

(defmacro inapplicablep (rule arule position)
  `(unless (check-rule-filter ,rule ,arule ,position)
     (incf *filtered-tasks*)))

(defmacro incompatiblep (vector avector)
  `(unless (restrictors-compatible-p ,vector ,avector)
     (incf *filtered-tasks*)))

(defun lexical-task (priority passive)
  #-:agenda
  (declare (ignore priority))
  
  #+:packing
  (when *chart-packing-p*
    (let ((edge (chart-configuration-edge passive)))
      (setf (edge-odag edge) (edge-dag edge))
      #+:restrict
      (setf (edge-dag edge) (copy-tdfs-partially (edge-dag edge)))
      (setf (edge-dag-restricted edge)
        (restrict-fs (tdfs-indef (edge-dag edge))))))
  #+:agenda
  (cond 
   ((or *first-only-p* *chart-dependencies*)
    (heap-insert *agenda* 
                 (if *scoring-hook* (funcall *scoring-hook* passive) priority)
                 passive))
   (t
    (fundamental4passive passive)))
  #-:agenda
  (fundamental4passive passive))

(defmacro rule-and-passive-task (rule passive)
  #+:agenda
  `(cond
    (*first-only-p*
     (let* ((task (cons ,rule ,passive))
            (priority (if *scoring-hook*
                        (funcall *scoring-hook* task)
                        (rule-priority ,rule))))
       (heap-insert *agenda* priority task)))
    #+:priority
    (*chart-packing-p*
     (let* ((end (chart-configuration-end ,passive))
            (start (chart-configuration-begin ,passive))
            (priority (- end (/ start *maximal-vertex*))))
       (heap-insert 
        *agenda* priority (cons ,rule ,passive))))
    (t
     (process-rule-and-passive (cons ,rule ,passive))))
  #-:agenda
  `(process-rule-and-passive ,rule ,passive))

(defmacro active-and-passive-task (active passive arule)
  #-:agenda
  (declare (ignore arule))
  #+:agenda
  `(cond
    (*first-only-p*
     (let* ((task (cons ,active ,passive))
            (priority (if *scoring-hook*
                        (funcall *scoring-hook* task)
                        (rule-priority ,arule))))
       (heap-insert *agenda* priority task)))
    #+:priority
    (*chart-packing-p*
     (let* ((forwardp (active-chart-configuration-forwardp ,active))
            (end (chart-configuration-end (if forwardp ,passive ,active)))
            (start (chart-configuration-begin (if forwardp ,active ,passive)))
            (priority (- end (/ start *maximal-vertex*))))
       (heap-insert *agenda* priority (cons ,active ,passive))))
    (t
     (process-active-and-passive (cons ,active ,passive))))
  #-:agenda
  `(process-active-and-passive ,active ,passive))

(defun complete-chart ()

  ;;
  ;; shadow global value of *chart-dependencies*, such that reduce-chart() can
  ;; `destructively' reset it to nil and cause subsequent task execution to go
  ;; back to non-agenda mode, unless required otherwise (in best-first mode).
  ;;
  (let ((*chart-dependencies* *chart-dependencies*))
    ;;
    ;; the chart reduction after lexical look-up only works when we are using
    ;; an agenda, such that the lexical look-up phase is completed before we 
    ;; start actual parsing.  i should probably eliminate many of these 
    ;; conditionals and just make the agenda-driven mode the default.
    ;;                                                        (24-oct-02 ; oe)
    #+:agenda
    (when *chart-dependencies* (reduce-chart))

    ;;
    ;; now run the main parser loop: until we empty the agenda (or hell
    ;; freezes over) apply the fundamental rule of chart parsing.
    ;;
    #+:agenda
    (when (or *first-only-p* #+:priority *chart-packing-p*)
      (loop
          until (empty-heap *agenda*)
          for task = (heap-extract-max *agenda*)
          when (chart-configuration-p task) do
            (fundamental4passive task)
          else when (rule-p (first task)) do
            (process-rule-and-passive task)
          else do
            (process-active-and-passive task)))))

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
  (declare (special *maximal-vertex*))
  
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

(defun fundamental4passive (passive)
  (declare (special *minimal-vertex* *maximal-vertex*))

  #+:adebug
  (print-trace :fundamental4passive passive)
  
  #+:null
  (when *scoring-hook*
    (setf (edge-score passive) (funcall *scoring-hook* passive)))
  
  (let* ((pedge (chart-configuration-edge passive))
         (prule (edge-rule pedge))
         (pvector (edge-dag-restricted pedge))
         (begin (chart-configuration-begin passive))
         (end (chart-configuration-end passive))
         (preceding (actives-by-end begin))
         (following (actives-by-start end)))
    ;;
    ;; add .passive. to chart (indexed by start and end vertex); array handling
    ;; is somewhat awkward here.
    ;;
    (if (aref *chart* end 0)
      (push passive (chart-entry-configurations (aref *chart* end 0)))
      (setf (aref *chart* end 0)
        (make-chart-entry :configurations (list passive))))
    (if (aref *chart* begin 1)
      (push passive (chart-entry-configurations (aref *chart* begin 1)))
      (setf (aref *chart* begin 1)
        (make-chart-entry :configurations (list passive))))
    ;;
    ;; check to see whether .passive. is a complete parsing result; trigger
    ;; non-local exit when the maximal number of readings (to compute) has
    ;; been reached.
    ;;
    (when (and *first-only-p*
               (= begin *minimal-vertex*) (= end *maximal-vertex*))
      (let ((result (find-spanning-edge passive begin end)))
        (when result
          (push (get-internal-run-time) *parse-times*)
          (setf *parse-record* (nconc result *parse-record*))
          (when (zerop (decf *first-only-p*))
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
;;;   - edges that were globally invalidated because one of their children was
;;;     packed retroactively.
;;;
;;; the second class is marked with a negative `frozen' value and ignored in
;;; the unpacking phase.  this was hard to debug.          (17-sep-99  -  oe)
;;;

#+:packing
(defun packed-edge-p (start end edge)
  (labels (#+:pdebug
           (edge-label (edge)
             (format 
              nil 
              "<~(~a~) ~d>" 
              (if (rule-p (edge-rule edge))
                (rule-id (edge-rule edge)) 
                (edge-rule edge))
              (edge-id edge)))
           (freeze (edge id &optional recursivep)
             (when (or (null (edge-frozen edge))
                       (plusp (edge-frozen edge))
                       (minusp id))
               #+:pdebug
               (format
                t
                "~&freeze(): freezing ~a for <~d>.~%"
                (edge-label edge) id)
               (setf (edge-frozen edge) id)
               ;;
               ;; _fix_me_
               ;; there is reason to suspect we may end up counting duplicate
               ;; freezings here.                           (29-jan-03; oe)
               ;;
               (when (minusp id) (incf (packings-frozen *packings*))))
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
                  [~d:~d] packing ~a ~:[-->~;==~] ~a.~%"
                 start end (edge-label edge) backwardp (edge-label oedge))
                (cond 
                 (backwardp
                  (push edge (edge-equivalent oedge))
                  (incf (packings-equivalent *packings*)))
                 (t
                  (push edge (edge-packed oedge))
                  (incf (packings-proactive *packings*))))
                (return configuration))
              #+:retroactivity
              (when backwardp
                #+:pdebug
                (format 
                 t 
                 "~&packed-edge-p(): ~
                  [~d:~d] ~:[~;(re)~]packing ~a <-- ~a.~%"
                 start end (edge-frozen oedge) 
                 (edge-label edge) (edge-label oedge))
                ;;
                ;; use nconc() here since .edge. can collect packings from more
                ;; than one existing .oedge. in this loop()
                ;;
                (setf (edge-packed edge) 
                  (nconc (edge-packed edge) (edge-packed oedge)))
                (setf (edge-equivalent edge) 
                  (nconc (edge-equivalent edge) (edge-equivalent oedge)))
                (setf (edge-packed oedge) nil)
                (setf (edge-equivalent oedge) nil)
                (passives-delete configuration)
                (unless (edge-frozen oedge)
                  (push oedge (edge-packed edge))
                  (incf (packings-retroactive *packings*)))
                (freeze oedge (edge-id edge)))))
        finally (return nil))))

(defun process-rule-and-passive #+:agenda (task)
                                #-:agenda (rule passive)

  #+:adebug
  (print-trace :process-rule-and-passive 
               #+:agenda (first task) #-:agenda rule
               #+:agenda (rest task) #-:agenda passive)
  #+:packing
  (when (edge-frozen (chart-configuration-edge #+:agenda (rest task)
                                               #-:agenda passive))
    (return-from process-rule-and-passive nil))
  
  (let* (#+:agenda (rule (first task))
         (rtdfs (if #+:restrict *chart-packing-p* #-:restrict nil
                  (rule-rtdfs rule)
                  (rule-full-fs rule)))
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
                         #+:hyper-activity
                         (if *hyper-activity-p*
                           t
                           (copy-tdfs-elements tdfs))
                         #-:hyper-activity
                         (copy-tdfs-elements tdfs)
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
            ;; _mystery_
            ;; it seems active edges are not recorded in the parent relation, so
            ;; will never be be frosted.  right now, uc and i fail to explain
            ;; why that should be unnecessary.                   (27-may-03; oe)
            ;;
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
                  (push nedge (edge-parents edge)))
            (when (packed-edge-p begin end nedge)
              (return-from process-rule-and-passive nil)))
          (fundamental4passive
           (make-chart-configuration :begin begin :end end :edge nedge))))))))

(defun process-active-and-passive #+:agenda (task &optional atdfs)
                                  #-:agenda (active passive &optional atdfs)

  #+:adebug
  (print-trace :process-active-and-passive 
               #+:agenda (first task) #-:agenda active
               #+:agenda (rest task) #-:agenda passive)

  #+:packing
  (when (and *chart-packing-p*
             (or (edge-frozen (chart-configuration-edge 
                               #+:agenda (rest task) #-:agenda passive))
                 (loop
                     for edge in (edge-children 
                                  (chart-configuration-edge 
                                   #+:agenda (first task) #-:agenda active))
                     thereis (edge-frozen edge))))
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
                  for edge in achildren
                  for tdfs = (edge-dag edge)
                  for i in (rule-rhs arule)
                  for path = (nth i daughters)
                  initially (setf atdfs (if #+:restrict *chart-packing-p* 
                                            #-:restrict nil
                                          (rule-rtdfs arule)
                                          (rule-full-fs arule)))
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
                               #+:hyper-activity
                               (if *hyper-activity-p*
                                 t
                                 (copy-tdfs-elements tdfs))
                               #-:hyper-activity
                               (copy-tdfs-elements tdfs)
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
                  (push nedge (edge-parents edge)))
            (when (packed-edge-p begin end nedge)
              (return-from process-active-and-passive nil)))
          (fundamental4passive
           (make-chart-configuration :begin begin :end end :edge nedge))))))))

(defun restrict-and-copy-tdfs (tdfs)
  (let* ((dag (deref-dag (tdfs-indef tdfs)))
         (new (clone-dag dag))
         restricted)
    (incf *copies*)
    (flet ((remove-restricted-arcs (arcs &aux rest)
             (dolist (arc arcs (nreverse rest))
               (if (smember (dag-arc-attribute arc) 
                            *deleted-daughter-features*)
                 (push arc restricted)
                 (push arc rest)))))
      (setf (dag-arcs new)
        (remove-restricted-arcs (dag-arcs new)))
      (setf (dag-comp-arcs new)
        (remove-restricted-arcs (dag-comp-arcs new)))
      (let ((result
             (unless (cyclic-dag-p (make-dag :type *toptype* :arcs restricted))
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

(defun count-nodes (edge &key (mark (gensym)) ignorep packingp chartp)
  (let* ((current (cond
                   (ignorep
                    (setf (edge-dag-restricted edge) mark)
                    0)
                   ((eq (edge-dag-restricted edge) mark) 0)
                   (t
                    (setf (edge-dag-restricted edge) mark)
                    (if chartp
                      (if (find-edge-given-id (edge-id edge)) 1 0)
                      1))))
         (children (loop
                       for edge in (edge-children edge)
                       sum (count-nodes edge :mark mark :chartp chartp
                                        :packingp packingp)))
         (packings (if packingp
                     #+:packing
                     (+ (loop
                            for edge in (edge-packed edge)
                            sum (count-nodes edge :mark mark 
                                             :packingp packingp
                                             :ignorep t :chartp chartp))
                        (loop
                            for edge in (edge-equivalent edge)
                            sum (count-nodes edge :mark mark 
                                             :packingp packingp
                                             :ignorep t :chartp chartp)))
                     #-:packing
                     0
                     0)))
    (+ current children packings)))

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

#+(and :packing :fdebug)
(defparameter *unpacking-failure-paths* (make-hash-table :test #'equal))

#+:packing
(defun unpack-edge! (id edge &optional insidep)
  #+:fdebug
  (clrhash *unpacking-failure-paths*)
  (labels ((instantiate (edge children i n)
             #+:idebug
             (format
              t
              "instantiate(): (~d < ~{~d~^ ~}~%"
              (edge-id edge)
              (loop
                  for child in children
                  collect (edge-id child)))
             ;;
             ;; re-use `foo' slot to keep local cache of how this edge can be
             ;; unfolded record failure, where appropriate, too. 
             ;;
             ;; _fix_me_
             ;; the caching mechanism is unnecessarily complex: it would seem
             ;; more transparent to make unpack-edge!() as a whole cache the
             ;; set of edges corresponding to the input .edge. instead.
             ;;                                                (11-dec-03; oe)
             (unless (vectorp (edge-foo edge))
               (setf (edge-foo edge) 
                 (make-array n :initial-element nil)))
             (let* ((cache (edge-foo edge))
                    (entry (aref cache i))
                    #+:fdebug
                    (*unify-debug* :return))
               (cond
                ((eq entry :bottom) (incf (packings-failures *packings*)) nil)
                ((edge-p entry) entry)
                (t
                 (with-unification-context (ignore)
                   (loop
                       with rule = (edge-rule edge)
                       with paths = (rest (rule-order rule))
                       with result = (rule-full-fs rule)
                       with leaves = nil
                       with lex-ids = nil
                       with rels = nil
                       with lexemes = nil
                       for path in paths
                       for child in children 
                       for tdfs = (or (edge-odag child) (edge-dag child))
                       when (g-edge-p child) do
                         (setf rels (append rels (g-edge-rels-covered child)))
                         (setf lexemes (append lexemes (g-edge-lexemes child)))
                       while result do
                         (setf leaves (append lex-ids (edge-leaves child)))
                         (setf lex-ids (append lex-ids (edge-lex-ids child)))
                         (setf result (yadu! result tdfs path))
                       finally
                         (return
                           (cond
                            (result
                             (let ((copy (restrict-and-copy-tdfs result)))
                               (setf (aref cache i)
                                 (if (g-edge-p edge)
                                   (make-g-edge
                                    :id (next-edge) :rule rule :dag copy
                                    :category (indef-type-of-tdfs copy)
                                    :from (edge-from edge) :to (edge-to edge)
                                    :children children 
                                    :leaves leaves :lex-ids lex-ids
                                    :rels-covered rels :lexemes lexemes)
                                   (make-edge
                                    :id (next-edge) :rule rule :dag copy
                                    :category (indef-type-of-tdfs copy)
                                    :from (edge-from edge) :to (edge-to edge)
                                    :children children 
                                    :leaves leaves :lex-ids lex-ids)))))
                            (t
                             #+:fdebug
                             (when id
                               (let ((type (first %failure%))
                                     (path (second %failure%))
                                     (nature (rest (rest %failure%))))
                                 (push (list id  type nature)
                                   (gethash path *unpacking-failure-paths*))))
                             (setf (aref cache i) :bottom)
                             (incf (packings-failures *packings*))
                             nil))))))))))

    (let ((children (edge-children edge))
          (morphology (edge-morph-history edge)))
      (cond
       ;;
       ;; ignore genuinely frozen edges; now that we are into the unpacking
       ;; phase, frosted edges represent valid alternatives again.
       ;; 
       ((and (edge-frozen edge) (minusp (edge-frozen edge)))
        #+:udebug
        (format
         t
         "~&unpack-edge!(): ignoring <~d> (frozen for <~d>)~%"
         (edge-id edge) (edge-frozen edge))
        nil)
       ;;
       ;; unless we are inside of a recursive call on this edge already, make
       ;; sure we recurse on all packings node and accumulate results.
       ;;
       ((and (null insidep) (or (edge-packed edge) (edge-equivalent edge)))
        (nconc (unpack-edge! id edge t)
               (loop
                   for edge in (edge-packed edge)
                   nconc (unpack-edge! id edge))
               (loop
                   for edge in (edge-equivalent edge)
                   nconc (unpack-edge! id edge))))
       ;;
       ;; given the (idiosyncratic) LKB representation of rule applications
       ;; that affect the surface form, this is just a variant of the general
       ;; case where we have children.
       ;;
       (morphology
        (loop
            with decompositions = (unpack-edge! id morphology)
            with n = (length decompositions)
            for decomposition in decompositions
            for i from 0
            for instantiation = (instantiate edge (list decomposition) i n)
            when instantiation collect instantiation))
       ;;
       ;; the (default) recursive case: for each daughter, unfold it and build
       ;; list of unfolding results, one per daughter.  then compute all ways
       ;; in which this edge can be unfolded (`decomposed') and instantiate
       ;; each one in turn; feed total number of decompositions and index into
       ;; instantiate() to support cache maintenance.
       ;;
       (children
        (loop
            with daughters = (loop
                                 for edge in children
                                 collect (unpack-edge! id edge))
            with decompositions = (cross-product daughters)
            with n = (length decompositions)
            for decomposition in decompositions
            for i from 0
            for instantiation = (instantiate edge decomposition i n)
            when instantiation collect instantiation))
       ;;
       ;; at the leafs of the tree, terminate the recursion.
       ;;
       (t (list edge))))))

#+:test
(let ((*active-parsing-p* t)
      (*show-parse-p* nil)
      (*first-only-p* nil)
      (*chart-packing-p* t)
      contemplated filtered executed successful)
  (reset-packings)
  (time (multiple-value-setq (contemplated filtered
                              executed successful)
          (do-parse-tty "so we will have an evening there to go over things or relax.")))
  (format
   t
   "~&~d trees; (=~d, >~d, <~d) packings; ~d readings; ~d [~d] edges~%"
   (length *parse-record*)
   (packings-equivalent *packings*)
   (packings-proactive *packings*)
   (packings-retroactive *packings*)
   (if *chart-packing-p*
     (time
      (loop
          for edge in *parse-record*
          sum (length (unpack-edge! nil edge))))
     (length *parse-record*))
   (tsdb::get-field :pedges (summarize-chart))
   (unless (null *parse-record*)
     (count-nodes (first *parse-record*) 
                  :packingp *chart-packing-p* :chartp t))))
