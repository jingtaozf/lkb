;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LKB -*-


;;; Copyright (c) 2000--2002
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `LICENSE' for conditions.


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
  (pushnew :retroactivity *features*))

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

(defparameter *chart-packing-p* nil)

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
  (incf (statistics-aedges *statistics*))
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
(defvar *chart-dependencies* nil)

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
     entry))

(defmacro passives-by-end (position)
  `(let ((entry (aref *chart* ,position 0)))
     entry))

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
       (setf (aref *chart* start 1)
         (delete ,configuration 
                 by-start
                 :test #'eq :count 1)))
     (when by-end
       (setf (aref *chart* end 0)
         (delete ,configuration 
                 by-end
                 :test #'eq :count 1)))))

(defmacro inapplicablep (rule arule position)
  `(unless (check-rule-filter ,rule ,arule ,position)
     (incf (statistics-ftasks *statistics*))))

(defmacro incompatiblep (vector avector)
  `(unless (restrictors-compatible-p ,vector ,avector)
     (incf (statistics-ftasks *statistics*))))

(defun lexical-task (priority passive)
  
  #+:adebug
  (print-trace :lexical-task passive)

  (when *chart-packing-p*
    (let* ((edge (chart-configuration-edge passive)))
      ;;
      ;; this is slightly more complicated than it ought to, as some of these
      ;; `lexical' tasks actually are instantiations of chains of one or more
      ;; lexical rules (the way the LKB lexical retrieval works).  unpacking 
      ;; expects `odag' to be set on the bottom daughter in each branch; so we
      ;; need to descend down to the leave.  furthermore, chart dependencies
      ;; make lexical tasks come through the agenda twice, thus we use `odag'
      ;; on the top edge to indicate whether it has been processed for packing
      ;; already.
      ;;
      (unless (edge-odag edge)
        (setf (edge-odag edge) (edge-dag edge))
        (loop
            for edge = (chart-configuration-edge passive) then child
            for child
            = (if (edge-children edge)
                (first (edge-children edge)))
            while child
            finally 
              (unless (edge-odag edge) 
                (setf (edge-odag edge) (edge-dag edge))))
        (setf (edge-dag edge) (copy-tdfs-partially (edge-dag edge)))
        (setf (edge-dag-restricted edge)
          (restrict-fs (tdfs-indef (edge-dag edge)))))))
  (cond 
   ((or *first-only-p* *chart-dependencies*)
    (heap-insert 
     *agenda* 
     (if *scoring-hook* (funcall *scoring-hook* passive) priority)
     passive))
   (t
    (fundamental4passive passive))))

(defmacro rule-and-passive-task (rule passive)
  `(cond
    (*first-only-p*
     (let* ((task (cons ,rule ,passive))
            (priority (if *scoring-hook*
                        (funcall *scoring-hook* task)
                        (rule-priority ,rule))))
       (heap-insert *agenda* priority task)))
    (*chart-packing-p*
     (let* ((end (chart-configuration-end ,passive))
            (start (chart-configuration-begin ,passive))
            (priority (- end (/ start *maximal-vertex*))))
       (heap-insert 
        *agenda* priority (cons ,rule ,passive))))
    (t
     (process-rule-and-passive (cons ,rule ,passive)))))

(defmacro active-and-passive-task (active passive arule)
  `(cond
    (*first-only-p*
     (let* ((task (cons ,active ,passive))
            (priority (if *scoring-hook*
                        (funcall *scoring-hook* task)
                        (rule-priority ,arule))))
       (heap-insert *agenda* priority task)))
    (*chart-packing-p*
     (let* ((forwardp (active-chart-configuration-forwardp ,active))
            (end (chart-configuration-end (if forwardp ,passive ,active)))
            (start (chart-configuration-begin (if forwardp ,active ,passive)))
            (priority (- end (/ start *maximal-vertex*))))
       (heap-insert *agenda* priority (cons ,active ,passive))))
    (t
     (process-active-and-passive (cons ,active ,passive)))))

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
    (when *chart-dependencies* (reduce-chart))

    ;;
    ;; now run the main parser loop: until we empty the agenda (or hell
    ;; freezes over) apply the fundamental rule of chart parsing.
    ;;
    (when (or *first-only-p* *chart-packing-p*)
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
      with orthographemics = (edge-partial-tree edge)
      for rule in (if orthographemics
                    (let* ((next (pt-node-rule (first orthographemics)))
                           (rule (get-lex-rule-entry next)))
                      (unless rule
                        (error
                         "postulate(): ~a requires unknown rule `~(~a~)'.~%"
                         passive next))
                      (cons rule
                            (loop
                                for foo in *parser-lexical-rules*
                                when (check-sp-lr-feeding rule foo)
                                collect foo)))
                    *parser-rules*)
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
          (incf (statistics-ftasks *statistics*)))))

(defun fundamental4active (active)
  (declare (special *maximal-vertex*))
  
  #+:adebug
  (print-trace :fundamental4active active)
  
  (let ((begin (chart-configuration-begin active))
        (end (chart-configuration-end active)))
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
        unless (edge-frozen pedge) do
          (if (and (not (edge-partial-tree pedge))
		   (not (edge-partial-tree aedge))
		   (check-rule-filter arule (edge-rule pedge) key)
                   (restrictors-compatible-p
                    avector
                    (edge-dag-restricted pedge)))
            (active-and-passive-task active passive arule)
            (incf (statistics-ftasks *statistics*))))))

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
      (push passive (aref *chart* end 0))
      (setf (aref *chart* end 0) (list passive)))
    (if (aref *chart* begin 1)
      (push passive (aref *chart* begin 1))
      (setf (aref *chart* begin 1) (list passive)))
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
            (if (and (not (edge-partial-tree pedge))
		     (not (edge-partial-tree aedge))
		     (check-rule-filter (edge-rule aedge) prule key)
                     (restrictors-compatible-p
                      (edge-dag-restricted aedge)
                      pvector))
              (active-and-passive-task active passive (edge-rule aedge))
              (incf (statistics-ftasks *statistics*)))))
    (loop
        for active in following
        unless (active-chart-configuration-forwardp active) do
          (let* ((aedge (chart-configuration-edge active))
                 (key (first (active-chart-configuration-open active))))
            (if (and (not (edge-partial-tree pedge))
		     (not (edge-partial-tree aedge))
		     (check-rule-filter (edge-rule aedge) prule key)
                     (restrictors-compatible-p
                      (edge-dag-restricted aedge)
                      pvector))
              (active-and-passive-task active passive (edge-rule aedge))
              (incf (statistics-ftasks *statistics*)))))))

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
               (when (minusp id) (incf (statistics-frozen *statistics*))))
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
                  (incf (statistics-equivalent *statistics*)))
                 (t
                  (push edge (edge-packed oedge))
                  (incf (statistics-proactive *statistics*))))
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
                  (incf (statistics-retroactive *statistics*)))
                (freeze oedge (edge-id edge)))))
        finally (return nil))))

(defun process-rule-and-passive (task)

  #+:adebug
  (print-trace :process-rule-and-passive (first task) (rest task))
  (when (edge-frozen (chart-configuration-edge (rest task)))
    (return-from process-rule-and-passive nil))
  
  (let* ((rule (first task))
         (rtdfs (if *chart-packing-p* (rule-rtdfs rule) (rule-full-fs rule)))
         (rhs (rule-rhs rule)) (open (rest rhs)) (key (first rhs))
         (daughters (rest (rule-order rule))) (path (nth key daughters))
         (passive (rest task))
         (edge (chart-configuration-edge passive)) (ptdfs (edge-dag edge))
         (orthographemics (edge-partial-tree edge))
         (otdfs (when (and (rule-orthographemicp rule)
                           (stringp (second (first orthographemics))))
                  (make-orth-tdfs (second (first orthographemics)))))
         nedge)
    (with-unification-context (ignore)
      (incf (statistics-etasks *statistics*))
      (let* ((tdfs (yadu! rtdfs ptdfs path)))
        (when tdfs
          (let* ((root (tdfs-at-end-of (first (rule-order rule)) tdfs))
                 (root (if otdfs (yadu root otdfs) root))
                 (vector (if open
                           (tdfs-qc-vector
                            tdfs (nth (first open) daughters))
                           (tdfs-qc-vector root)))
                 (copy (if open
                         (if *hyper-activity-p*
			     t
			   (progn
			     (if *characterize-p*
				 (set-characterization-indef-within-unification-context 
				  (tdfs-indef tdfs) (edge-cfrom edge) (edge-cto edge)))
			     (copy-tdfs-elements tdfs))) ;; performs copy-dag
			 (restrict-and-copy-tdfs  ;; performs clone-dag
			  root :cfrom (edge-cfrom edge) :cto (edge-cto edge)))))
            (when copy
              (setf nedge
                (make-edge :id (if open (next-active-edge) (next-edge))
                           :category (indef-type-of-tdfs
                                      (if (eq copy t) tdfs copy))
                           :rule rule :children (list edge)
                           :dag copy :dag-restricted vector
                           :lex-ids (copy-list (edge-lex-ids edge))
                           :leaves (copy-list (edge-leaves edge))
                           :orth-tdfs otdfs
                           :partial-tree (if (rule-orthographemicp rule)
                                           (rest orthographemics)
                                           orthographemics))))))))
    (when nedge
      (incf (statistics-stasks *statistics*))
      (let ((begin (chart-configuration-begin passive))
            (end (chart-configuration-end passive)))
        (cond
         (open
          (let* ((forwardp (< key (first open)))
                 (active (make-active-chart-configuration 
                          :begin begin :end end :edge nedge
                          :open open :forwardp forwardp)))
            ;;
            ;; _fix_me_
            ;; it seems active edges are not recorded in the parent relation, 
            ;; so will never be frosted.  right now, uc and i fail to explain
            ;; why that should be unnecessary.                 (27-may-03; oe)
            ;;
            (fundamental4active active)))
         (t
          (when *chart-packing-p*
            (loop
                for edge in (edge-children nedge) do
                  (push nedge (edge-parents edge)))
            (when (packed-edge-p begin end nedge)
              (return-from process-rule-and-passive nil)))
          (fundamental4passive
           (make-chart-configuration :begin begin :end end :edge nedge))))))))

(defun process-active-and-passive (task)

  #+:adebug
  (print-trace :process-active-and-passive(first task) (rest task))

  (when (and *chart-packing-p*
             (or (edge-frozen (chart-configuration-edge (rest task)))
                 (loop
                     for edge in (edge-children 
                                  (chart-configuration-edge (first task)))
                     thereis (edge-frozen edge))))
    (return-from process-active-and-passive nil))

  (let* ((active (first task))
         (key (first (active-chart-configuration-open active)))
         (open (rest (active-chart-configuration-open active)))
         (forwardp (active-chart-configuration-forwardp active))
         (aedge (chart-configuration-edge active))
         (achildren (edge-children aedge))
         (atdfs (edge-dag aedge)) (arule (edge-rule aedge))
         (daughters (rest (rule-order arule))) (path (nth key daughters))
         (passive (rest task))
         (pedge (chart-configuration-edge passive)) (ptdfs (edge-dag pedge))
	 (children (if forwardp
		       (append achildren (list pedge))
		     (cons pedge achildren)))
        (nedge
          (with-unification-context (ignore)
            (incf (statistics-etasks *statistics*))
            (when (eq atdfs t)
              #+:adebug
              (print-trace :reconstruct active)
              (loop
                  for edge in achildren
                  for tdfs = (edge-dag edge)
                  for i in (rule-rhs arule)
                  for path = (nth i daughters)
                  initially (setf atdfs (if *chart-packing-p* 
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
		       (cfrom (loop for edge in children
				  minimize (or (edge-cfrom edge) -1)))
		       (cto (loop for edge in children
				maximize (or (edge-cto edge) -1)))
                       (copy (if open
                               (if *hyper-activity-p*
                                 t
                                 (progn
				   (if *characterize-p*
				       (set-characterization-indef-within-unification-context
					(tdfs-indef tdfs) cfrom cto))
				   (copy-tdfs-elements tdfs)))
			       (restrict-and-copy-tdfs 
				root :cfrom cfrom :cto cto))))
                  (when copy
                    (let* ((category (indef-type-of-tdfs 
                                      (if (eq copy t) tdfs copy)))
;                           (children (if forwardp
;                                       (append achildren (list pedge))
;                                       (cons pedge achildren)))
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
      (incf (statistics-stasks *statistics*))
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
          (when *chart-packing-p*
            (loop
                for edge in (edge-children nedge) do
                  (push nedge (edge-parents edge)))
            (when (packed-edge-p begin end nedge)
              (return-from process-active-and-passive nil)))
          (fundamental4passive
           (make-chart-configuration :begin begin :end end :edge nedge))))))))

(defun restrict-and-copy-tdfs (tdfs &key cfrom cto)
  (let* ((dag (deref-dag (tdfs-indef tdfs)))
         (new (clone-dag dag))
         restricted)
    (incf (statistics-copies *statistics*))
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
	       (if *characterize-p*
		   (set-characterization-indef-within-unification-context new cfrom cto))
               (let ((copy (copy-dag new)))
                 (and copy 
                      (make-tdfs :indef copy 
                                 :tail (copy-list (tdfs-tail tdfs))))))))
        (unless result (incf (statistics-stasks *statistics*)))
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
;	   (partial-tree (edge-partial-tree edge))
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
                     0)))
    (+ current children packings)))
