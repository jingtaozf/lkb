;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: active.lsp
;;;      module: experimental key-driven (active) parser for the LKB
;;;     version: 0.0 (16-jun-99)
;;;  written by: oe, coli saarbruecken
;;; last update: 21-jun-99
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

(eval-when #+:ansi-eval-when (:load-toplevel :compile-toplevel :execute)
           #-:ansi-eval-when (load eval compile)
  (pushnew :agenda *features*))

(defparameter *hyper-activity* nil)

(defstruct (active-chart-configuration (:include chart-configuration))
  open
  forwardp
  #+:adebug (executions 0)
  #+:adebug (successes 0))

(defvar *achart* (make-array (list *chart-limit* 2)))

(defstruct (arule (:include rule))
  rhs)

#+:agenda
(defparameter *aagenda* (make-heap))
(defparameter *arules* nil)

(defun initialize-arules ()
  (setf *arules* nil)
  (loop
      for rule in (get-matching-rules nil nil)
      for tdfs = (rule-full-fs rule)
      for dag = (tdfs-indef tdfs)
      for daughters = (rest (rule-order rule))
      for arity = (length daughters)
      for key = (or
                 (loop
                     for path in daughters
                     for i from 0
                     for daughter = (existing-dag-at-end-of dag path)
                     when (key-daughter-p daughter)
                     return i)
                 0)
      for rhs = (cons
                 key
                 (nconc
                  (loop for i from 0 to (- key 1) collect i)
                  (loop for i from (+ key 1) to (- arity 1) collect i)))
      for arule = (make-arule 
                   :orth (rule-orth rule) :infl-pos (rule-infl-pos rule)
                   :sense-id (rule-sense-id rule) :id (rule-id rule)
                   :language (rule-language rule) 
                   :unifs (rule-unifs rule) :def-unifs (rule-def-unifs rule)
                   :mother-p (rule-mother-p rule) :full-fs (rule-full-fs rule)
                   :daughters-restricted (rule-daughters-restricted rule)
                   :daughters-restricted-reversed 
                   (rule-daughters-restricted-reversed rule)
                   :daughters-apply-order (rule-daughters-apply-order rule)
                   :order (rule-order rule)
                   :daughters-order-reversed 
                   (rule-daughters-order-reversed rule)
                   :apply-filter (rule-apply-filter  rule)
                   :apply-index (rule-apply-index rule)
                   :rhs rhs)
      do
        (push arule *arules*)))

(defun complete-chart (begin end)

  (when (null *arules*) (initialize-arules))
  (let ((*minimal-vertex* begin)
        (*maximal-vertex* end))
    (declare (special *minimal-vertex* *maximal-vertex*))
    ;;
    ;; initialize *aagenda* through postulation of rules over all passive items
    ;; obtained from regular LKB lexical processing (i.e. words); the active
    ;; parser will share the LKB chart but use its own agenda (storing tasks).
    ;; additionally, we have to initialize the second *chart* dimension indexed
    ;; by start positions, but not duplicate the entry in the first dimension;
    ;; therefore the second argument to fundamental4passive().
    ;;
    (loop 
      for i from 0 to (1- *chart-limit*)
      do 
        (setf (aref *achart* i 0) nil (aref *achart* i 1) nil))
    #+:agenda
    (flush-heap *aagenda*)
    (loop
        for i from 0 to (- *chart-limit* 1)
        for entry = (aref *chart* i 0)
        for configurations = (and entry (chart-entry-configurations entry))
        do
          (loop
              for passive in configurations
              for begin = (chart-configuration-begin passive)
              do
                (fundamental4passive passive :old)))
    ;;
    ;; now run the main parser loop: until we empty the agenda (or hell freezes
    ;; over) apply the fundamental rule of chart parsing.
    ;;
    #+:agenda
    (loop
        until (empty-heap *aagenda*)
        for task = (heap-extract-max *aagenda*)
        when (rule-p (first task)) do
          (process-rule-and-passive task)
        else do
          (process-active-and-passive task))))

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
      for rule in *arules*
      for rhs = (arule-rhs rule)
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
          (process-rule-and-passive (cons rule passive))
          (incf *filtered-tasks*))))

(defun fundamental4active (active)

  #+:adebug
  (print-trace :fundamental4active active)
  
  (let ((begin (chart-configuration-begin active))
        (end (chart-configuration-end active)))
    ;;
    ;; add .active. to active chart (indexed by start and end vertex)
    ;;
    (push active (aref *achart* end 0))
    (push active (aref *achart* begin 1))
    ;;
    ;; try to combine with adjacent passive edges (on the side that we want to
    ;; fill next) and create new tasks for combinations that pass the filter.
    ;;
    (loop
        with aedge = (chart-configuration-edge active)
        with arule = (edge-rule aedge)
        with key = (first (active-chart-configuration-open active))
        with avector = (edge-dag-restricted aedge)
        with entry = (if (active-chart-configuration-forwardp active)
                       (aref *chart* end 1)
                       (aref *chart* begin 0))
        for passive in (and entry (chart-entry-configurations entry))
        for pedge = (chart-configuration-edge passive)
        when (and (check-rule-filter arule (edge-rule pedge) key)
                  (restrictors-compatible-p
                   avector
                   (edge-dag-restricted pedge)))
        do
          #+:agenda
          (heap-insert *aagenda* (rule-priority arule) (cons active passive))
          #-:agenda
          (process-active-and-passive (cons active passive))
        else do
          (incf *filtered-tasks*))))

(defun fundamental4passive (passive &optional oldp)
  (declare (special *minimal-vertex* *maximal-vertex*))

  #+:adebug
  (print-trace :fundamental4passive passive)
  
  (let* ((pedge (chart-configuration-edge passive))
         (prule (edge-rule pedge))
         (pvector (edge-dag-restricted pedge))
         (begin (chart-configuration-begin passive))
         (end (chart-configuration-end passive))
         (preceding (aref *achart* begin 0))
         (following (aref *achart* end 1)))
    ;;
    ;; add .passive. to chart (indexed by start and end vertex); .oldp is only
    ;; used in initialization to prevent duplication of passive edges obtained
    ;; from (regular) LKB lexical processing.
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
    (when (and (= begin *minimal-vertex*) (= end *maximal-vertex*))
      (let ((result (find-spanning-edge passive begin end)))
        (when result
          (push (get-internal-run-time) *parse-times*)
          (setf *parse-record* (nconc result *parse-record*))
          (when *maximal-number-of-readings*
            (when (zerop (decf *maximal-number-of-readings*))
              (throw :best-first t))))))
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
              (process-active-and-passive (cons active passive))
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
              (process-active-and-passive (cons active passive))
              (incf *filtered-tasks*))))))

(defun process-rule-and-passive (task)

  #+:adebug
  (print-trace :process-rule-and-passive (first task))
  #+:adebug
  (print-trace :process-rule-and-passive (rest task))
  
  (let* ((rule (first task))
         (rtdfs (rule-full-fs rule))
         (rhs (arule-rhs rule)) (open (rest rhs)) (key (first rhs))
         (daughters (rest (rule-order rule))) (path (nth key daughters))
         (passive (rest task))
         (edge (chart-configuration-edge passive)) (ptdfs (edge-dag edge))
         (nedge
          (with-unification-context (ignore)
            (incf *executed-tasks*)
            (let* ((tdfs (yadu rtdfs (create-temp-parsing-tdfs ptdfs path))))
              (when tdfs
                (let* ((root (tdfs-at-end-of (first (rule-order rule)) tdfs))
                       (vector (if open
                                 (tdfs-qc-vector
                                  tdfs (nth (first open) daughters))
                                 (tdfs-qc-vector root)))
                       (copy (if open
                               (if *hyper-activity*
                                 t
                                 (copy-tdfs-elements tdfs))
                               (restrict-and-copy-tdfs root))))
                  (when copy
                    (make-edge :id (next-edge)
                               :category (indef-type-of-tdfs
                                          (if (eq copy t) tdfs copy))
                               :rule rule :children (list edge)
                               :dag copy :dag-restricted vector
                               :lex-ids (copy-list (edge-lex-ids edge))
                               :leaves (copy-list (edge-leaves edge))))))))))
    (when nedge
      (incf *successful-tasks*)
      (let ((begin (chart-configuration-begin passive))
            (end (chart-configuration-end passive)))
        (if open
          (fundamental4active
           (make-active-chart-configuration 
            :begin begin :end end :edge nedge
            :open open :forwardp (< key (first open))))
          (fundamental4passive
           (make-chart-configuration
            :begin begin :end end :edge nedge)))))))

(defun process-active-and-passive (task)
  
  #+:adebug
  (print-trace :process-active-and-passive (first task))
  #+:adebug
  (print-trace :process-active-and-passive (rest task))

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
                  for i in (arule-rhs arule)
                  for path = (nth i daughters)
                  do
                    (setf atdfs 
                      (yadu atdfs (create-temp-parsing-tdfs tdfs path)))
                    (incf *executed-tasks*) (incf *successful-tasks*)))
            #+:adebug
            (incf (active-chart-configuration-executions active))
            (let* ((tdfs (yadu atdfs (create-temp-parsing-tdfs ptdfs path))))
              (when tdfs
                (let* ((root (tdfs-at-end-of (first (rule-order arule)) tdfs))
                       (vector (if open
                                 (tdfs-qc-vector 
                                  tdfs (nth (first open) daughters))
                                 (tdfs-qc-vector root)))
                       (copy (if open
                               (if *hyper-activity*
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
                      (make-edge :id (next-edge)
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
        (if open
          (fundamental4active
           (make-active-chart-configuration 
            :begin begin :end end :edge nedge
            :open open :forwardp (< key (first open))))
          (fundamental4passive
           (make-chart-configuration
            :begin begin :end end :edge nedge)))))))

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
               (setf (dag-forward dag) new)
               (copy-tdfs-elements tdfs))))
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
    (if (arule-p object)
      (let* ((label (debug-label object))
             (open (arule-rhs object)))
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

