;;; Copyright John Carroll 1998 All Rights Reserved.
;;; No use or redistribution without permission.
;;;
;;; CSLI, Stanford University, USA

(in-package :cl-user)

;;;

(defstruct (dotted-edge (:include edge))
   res ; feature name of mother in rule dag in active edges
   needed ; ordered list of names of daughter features still to be found
   )

(defstruct (g-edge (:include dotted-edge)
                   (:constructor make-g-edge
                    (&key id category rule dag needed
                          (dag-restricted
                             ;; restricted field in inactive edges is dag, for
                             ;; active edges it's the next needed daughter
                             (restrict-fs
                                (if needed
                                   (existing-dag-at-end-of (tdfs-indef dag)
                                      (if (listp (first needed))
                                         (first needed)
                                         (list (first needed))))
                                   (tdfs-indef dag))))
                          leaves lex-ids children morph-history 
                          spelling-change res rels-covered)))
   ;; category: not used
   ;; id, rule, leaves: filled in, not used in generation algorithm itself, but
   ;; needed for chart display etc
   rels-covered ; set of relations generated so far
   index)


(defvar *gen-chart* nil)
(defvar *gen-record* nil)
(defvar *gen-chart-unifs* 0)
(defvar *gen-chart-fails* 0)
(defvar *non-intersective-rules* nil)

(defparameter *gen-filter-debug* nil)
(defparameter *gen-adjunction-debug* nil)
(defparameter *gen-equality-debug* nil)

(eval-when
 (compile load eval)    
(proclaim
   '(special *edge-id* *safe-not-to-copy-p* *orth-path* *toptype*
       *start-symbol* *debugging* *substantive-roots-p*
       mrs::*initial-semantics-path* mrs::*psoa-liszt-path*
       mrs::*liszt-first-path* mrs::*liszt-rest-path* mrs::*rel-name-path*
       mrs::*null-semantics-found-items* mrs::*possible-grules*))
)


;;; Utility functions for initialising and building daughters and leaves
;;; fields in active chart edges

(defmacro gen-make-list-and-insert (len item index)
   ;; make list of length len and insert item at index (1-based)
  `(let ((lst (make-list ,len)))
     (setf (nth (1- ,index) lst) ,item) lst))

(defmacro gen-copy-list-and-insert (lst item)
  ;; replace first nil with item in top-level copy of lst
  `(substitute ,item nil ,lst :count 1))


;;; Functions on sets (of MRS relations)

(defun gen-chart-set-intersection-p (lst1 lst2)
  (loop for x in lst1
      thereis (member x lst2 :test #'eq)))

(defun gen-chart-set-equal-p (lst1 lst2)
   (and
      (do* ((t1 lst1 (cdr t1)) (t2 lst2 (cdr t2))) ; same length?
           ((or (null t1) (null t2)) (eq t1 t2)))
      (gen-chart-subset-p lst1 lst2)))

(defun gen-chart-subset-p (lst1 lst2)
   (dolist (x lst1 t) (unless (member x lst2 :test #'eq) (return nil))))

(defun gen-chart-set-difference (lst1 lst2)
   (remove-if #'(lambda (x) (member x lst2 :test #'eq)) lst1))


;;; Interface to generator - take an input MRS, ask for instantiated lexical
;;; items and other items that might be applicable, and call generator
;;; proper. Clear chart and analyses record before entry in case we don't make
;;; it into generation proper. Do it also in chart-generate since that is also
;;; an entry point

(defun generate-from-mrs (input-sem)
  (clear-gen-chart)
  (setf *cached-category-abbs* nil)
  (let*
      ((found-lex-list
	(apply #'append (mrs::collect-lex-entries-from-mrs input-sem)))
       (filtered
        (remove-if #'(lambda (x) (member x '(AN) :test #'eq))
					; *** e.g. a -> an
		   found-lex-list
		   :key #'mrs::found-lex-lex-id))
       (empty
	(mrs::possibly-applicable-null-semantics input-sem)
	;; this must be called after mrs::collect-lex-entries-from-mrs since
	;; the latter sets up mrs::*null-semantics-found-items* for it
	))
    #|
    (for lex in filtered
    do
    (format t "~%Id ~A, Rules ~A" (mrs::found-lex-lex-id lex)
    (mrs::found-lex-rule-list lex)))
    |#
    (if filtered
	(chart-generate input-sem (append filtered empty) 
			mrs::*possible-grules*)
      (progn
	(format t "~%Some lexical entries could not be found from MRS ~
                     relations - has function index-for-generator been run ~
                     yet?")
	nil))))


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
;;; returns values: realisations, time taken (msecs), number of unification
;;; attempts, number of unification failures, number of active edges, number
;;; of inactive, total number of chart edges

(defun chart-generate (input-sem found-lex-items possible-grules
		       &optional (first-only-p *gen-first-only-p*))
  ;; also an entry point so ensure chart is clear
  (clear-gen-chart) 
  (flush-heap *agenda*)
  ;;(when (> (length found-lex-items) 80)
  ;;   (format t "~%More than 80 initial lexical items - skipping")
  ;;   (return-from chart-generate nil))
  (let ((*safe-not-to-copy-p* t)
	(*gen-chart-unifs* 0) (*gen-chart-fails* 0)
	(*gen-first-only-p* first-only-p)
	(*non-intersective-rules*
	 (remove-if
	  #'(lambda (r)
	      (or (member (rule-id r) *intersective-rule-names* :test #'eq)
		  (spelling-change-rule-p r)))
	  possible-grules)))
    #+(and mcl powerpc) (setq aa 0 bb 0 cc 0 dd 0 ee 0 ff 0 gg 0 hh 0 ii 0 jj 0)
    (unless *intersective-rule-names*
      (format t "~%Warning: no intersective rules defined"))
    (unless (catch 'first
	      ;; Add lexical edges
	      (mapc #'(lambda (found) 
			(let* ((lex-entry-fs (mrs::found-lex-inst-fs found))
			       (word (extract-orth-from-fs lex-entry-fs)))
			  (with-agenda (when *gen-first-only-p* 
					 (gen-lex-priority lex-entry-fs))
			    (gen-chart-add-inactive 
			     (make-g-edge
			      :id (next-edge) 
			      :rule word
			      :dag lex-entry-fs
			      :needed nil
			      :rels-covered (mrs::found-lex-main-rels found)
			      :children nil :leaves (list word) 
			      :lex-ids (list 
					(mrs::found-lex-lex-id found)))
			     input-sem))))
		    found-lex-items)
	      ;; Process tasks
	      (loop 
		  until (empty-heap *agenda*)
		  do (funcall (heap-extract-max *agenda*))))
      (unless first-only-p
	;;
	;; we have done this already (incrementally) in the generate loop
	;;
	(multiple-value-bind (complete partial)
	    (gen-chart-find-complete-edges
	     (gen-chart-retrieve-inactive-with-index *toptype*) input-sem)
	  (setq *gen-record*
	    (nconc complete
		   (when *intersective-rule-names*
		     (gen-chart-adjoin-modifiers partial input-sem)))))))
    (let* ((inact-tot
	    (length (gen-chart-retrieve-inactive-with-index *toptype*)))
	   (act-tot
	    (length (gen-chart-retrieve-active-with-index *toptype*))))
      (values
       ;; extract-string-from-gen-record 
       ;; looks after changes like
       ;; e.g. "a apple" -> "an apple"
       (extract-strings-from-gen-record)
       *gen-chart-unifs* *gen-chart-fails*
       act-tot inact-tot (+ act-tot inact-tot)))))

(defun extract-strings-from-gen-record nil
  (for edge in *gen-record*
       filter
       (fix-spelling
        ;; in spell.lsp
        (g-edge-leaves edge))))

(defun clear-gen-chart nil
   (setq *edge-id* 0)
   (setq *gen-chart* nil)
   (setq *gen-record* nil))


;;; Finish off syntactically complete analyses, and return those that cover
;;; all of input semantics

(defun gen-chart-find-complete-edges (candidate-edges input-sem)
  (let ((complete nil) (incompatible nil)
	(partial nil)
	(start-symbols
	 (if (listp *start-symbol*) *start-symbol* (list *start-symbol*))))
    (dolist (e candidate-edges)
      ;; process has so far ensured that we have not generated any edge
      ;; containing a relation name that is not in input semantics, and that
      ;; no edge contains duplicates of exactly the same relation - now check
      ;; that we have generated all relations
      (dolist
	  (new
	      (if *substantive-roots-p*
                  (gen-chart-root-edges e start-symbols)
		(gen-filter-root-edges e start-symbols)))
	(if (gen-chart-check-complete new input-sem)
	    (if (gen-chart-check-compatible new input-sem)
		(push new complete)
	      (push new incompatible))
	  (push new partial))))
    (when *gen-adjunction-debug*
      (format t "~&Complete compatible edges: ~:A~%Complete incompatible edges: ~:A~
                    ~%Partial edges: ~:A"
	      (mapcar #'g-edge-id complete) (mapcar #'g-edge-id incompatible)
	      (mapcar #'g-edge-id partial)))
    (when *gen-equality-debug* 
      (format t "~%Complete incompatible edges: ~:A" 
	      (mapcar #'g-edge-id incompatible)))
    (values complete partial)))


(defun gen-chart-check-complete (edge input-sem)
   ;; check that semantics of edge is subsumed by input-sem, i.e. that we've
   ;; got all the relations that we wanted
   #+ignore (print (list (edge-id edge) (mapcar #'mrs::rel-sort (g-edge-rels-covered edge))))
   (gen-chart-subset-p
    (mrs::psoa-liszt input-sem)
    (g-edge-rels-covered edge)))


(defun gen-chart-check-compatible (edge input-sem)
   ;; construct the MRS for edge
   ;; We test for 'compatibility' rather than equality - in
   ;; particular, semantics of generated string might be more specific than
   ;; input MRS wrt things like scope - so we pass 3nd arg of nil to mrs-equalp
   ;; Semantics are already guaranteed to be compatible wrt relation arguments since
   ;; these were skolemised in the input MRS
   (let ((sem-fs
            (existing-dag-at-end-of (tdfs-indef (g-edge-dag edge))
                mrs::*initial-semantics-path*)))
     (when (and sem-fs (dag-p sem-fs))
       (let ((mrs (mrs::construct-mrs sem-fs nil t)))
;;         (when *debugging*
;;           (display-fs sem-fs "semstructure"))
;;        (when *sem-debugging*
;;           (mrs::output-mrs input-sem 'mrs::simple)
;;           (mrs::output-mrs mrs 'mrs::simple))  
         (mrs::mrs-equalp mrs input-sem nil *debugging*)))))

#|
   ;; make sure any errors in mrs-equalp don't stop the show
   (handler-case 
         (mrs::mrs-equalp mrs input-sem nil *debugging*)
      (error (condition)
         (warn "Error '~A' occurred in ~A - returning true"
            condition 'mrs::mrs-equalp)
         t)))
|#



(defun gen-chart-root-edges (edge start-symbols)
  ;; c.f. create-new-root-edges in parse.lsp
   (for start-symbol in start-symbols        
       filter
       (let ((tdfs (get-tdfs-given-id 
                    start-symbol)))
         (if tdfs
             (let ((unif 
                    (yadu tdfs (g-edge-dag edge))))
               (if unif
                   (let ((new-edge
                          (make-g-edge :dag unif
                                     :id (next-edge)
                                     :rule 'root
                                     :children (list edge)
                                     :leaves (g-edge-leaves edge)
                                     :lex-ids (g-edge-lex-ids edge))))
                      (gen-chart-add-with-index new-edge
                         (gen-chart-dag-index (tdfs-indef (g-edge-dag new-edge))
                            (g-edge-id new-edge)))
                      new-edge)))))))

(defun gen-filter-root-edges (edge start-symbols)
   ;; c.f. filter-root-edges in parse.lsp
   (dolist (start-symbol start-symbols)
      (let ((root-spec (get-tdfs-given-id start-symbol)))
         (when root-spec
            (when (yadu root-spec (g-edge-dag edge))
               (return (list edge)))))))


;;; Chart indexing - on *semantics-index-path* values. May be full types or
;;; instance types. Seem to be mostly disjoint if not eq, so don't bother using
;;; a tree-like representation
;;; Apart from these fns, show-gen-chart (below), and find-gen-edge-given-id
;;; (tree-nodes.lsp) are the only functions that need to know the internal
;;; representation of chart

(defun gen-chart-dag-index (dag edge-id)
   (let ((index-dag (existing-dag-at-end-of dag *semantics-index-path*)))
      (if index-dag
         (let ((index (type-of-fs index-dag)))
            (when (and (consp index) (null (cdr index)))
               ;; simplify single-item disjunction - doesn't need to be stored as
               ;; an atomic type and will speed up subsequent type lookup
               (setq index (car index)))
            index)
         (progn
            ;;(cerror (format nil "use type ~A" *toptype*)
            ;;   "unexpectedly missing index for edge ~A: ~S" edge-id dag)
            (warn "unexpectedly missing index for edge ~A - using ~A" edge-id *toptype*)
            *toptype*))))


(defun gen-chart-add-with-index (edge index)
  (when (g-edge-index edge)
    (print (list (g-edge-index edge) index)))
  (setf (g-edge-index edge) index)
  (let ((entry (assoc index *gen-chart* :test #'equal)))
    (unless entry
      (push (setq entry (cons index nil)) *gen-chart*))
    (push edge (cdr entry))))


(defun gen-chart-retrieve-active-with-index (index)
  ;; return all active edges in chart keyed by a type compatible with index
  (let ((res nil))
    (dolist (entry *gen-chart* res)
      (when (greatest-common-subtype (car entry) index)
	(dolist (e (cdr entry))
	  (when (g-edge-needed e)
	    (push e res)))))))

(defun gen-chart-retrieve-inactive-with-index (index)
  ;; return all inactive edges in chart keyed by a type compatible with index
  (let ((res nil))
    (dolist (entry *gen-chart* res)
      (when (greatest-common-subtype (car entry) index)
	(dolist (e (cdr entry))
	  (when (null (g-edge-needed e))
	    (push e res)))))))


;;; Daughter paths in a rule, in the order that they should be instantiated. Second
;;; value is the zero-based index of the head daughter

(defun gen-chart-rule-ordered-daughters (rule)
   (values (rule-daughters-apply-order rule)
      (position (car (rule-daughters-apply-order rule)) (cdr (rule-order rule)))))


;;; Core control functions. Processing inactive and active edges

(defun gen-chart-add-inactive (edge input-sem)
  ;; check that we haven't generated any relation that is not in input
  ;; semantics assume that once a relation has been put in liszt it won't be
  ;; removed. This filter is needed as well as less expensive one that lexical
  ;; items are used only once since some rules can introduce relations.
  (unless (gen-chart-subset-p (g-edge-rels-covered edge) 
			      (mrs::psoa-liszt input-sem))
    (when *gen-filter-debug*
      (format t "~&Filtered out candidate inactive edge covering ~A ~
                   with lexical relations ~(~A~) that are not a subset of ~
                   input semantics"
	      (g-edge-leaves edge) 
	      (mapcar #'mrs::rel-sort (g-edge-rels-covered edge))))
    (return-from gen-chart-add-inactive nil))
  (let ((index (gen-chart-dag-index (tdfs-indef (g-edge-dag edge)) 
				    (g-edge-id edge))))
    (gen-chart-add-with-index edge index)
    ;; Did we just find a result?
    (when *gen-first-only-p*
      (multiple-value-bind (complete partial)
	  (gen-chart-find-complete-edges (list edge) input-sem)
	(setq *gen-record*
	  (nconc complete
		 (when *intersective-rule-names*
		   (gen-chart-adjoin-modifiers partial input-sem))))
	(when *gen-record*
	  (throw 'first t))))
    ;; see if this new inactive edge can extend any existing active edges
    (mapc #'(lambda (e) 
	      (with-agenda (when *gen-first-only-p* 
			     (gen-rule-priority (edge-rule e)))
		(gen-chart-test-active edge e input-sem)))
	  (gen-chart-retrieve-active-with-index index))
    ;; see if we can create new active edges by instantiating the head
    ;; daughter of a rule
    (mapc #'(lambda (rule) 
	      (when *debugging*
		(format t "~&Trying to create new active edge from rule ~A ~
                     and inactive edge ~A"
			(rule-id rule) (g-edge-id edge)))
	      (with-agenda (when *gen-first-only-p* (gen-rule-priority rule))
		(multiple-value-bind (gen-daughter-order head-index) ; zero-based on daughters
		    (gen-chart-rule-ordered-daughters rule)
		  (let ((act (gen-chart-create-active rule edge 
						      gen-daughter-order 
						      head-index)))
		    (when act
		      (gen-chart-extend-active act input-sem))))))
	  *non-intersective-rules*)))


(defun gen-chart-create-active (rule edge gen-daughter-order head-index)
   (let
      ((unified-dag
          (gen-chart-try-unification rule
             (rule-full-fs rule)
             (first gen-daughter-order) ; head daughter path
             (nth head-index (rule-daughters-restricted rule)) ; head restrictor
             head-index edge
             (not (rest gen-daughter-order))
             (first (rule-order rule)))))
      (when unified-dag
         (make-g-edge
            :id (next-edge) :rule rule
            :dag unified-dag
            :res rule
            :needed (rest gen-daughter-order)
            :rels-covered
            (append
               (if (mrs::found-rule-p rule)
                  (mrs::found-rule-main-rels rule)) 
               (g-edge-rels-covered edge))
            :children
            (gen-make-list-and-insert (length gen-daughter-order)
                                      edge (1+ head-index))
            :lex-ids 
            (gen-make-list-and-insert (length gen-daughter-order)
               (g-edge-lex-ids edge) (1+ head-index))
            :leaves
            (gen-make-list-and-insert (length gen-daughter-order)
               (g-edge-leaves edge) (1+ head-index))))))


(defun gen-chart-test-active (inact act input-sem &optional one-off-p)
  ;; can extend active edge with inactive? First check to make sure new edge
  ;; would not use any relation from initial lexical items more than once.
  ;; Assumption here that rels are eq between alternative lexical items
  (when (not (gen-chart-set-intersection-p (g-edge-rels-covered act)
					   (g-edge-rels-covered inact)))
    (when *debugging*
      (format t "~&Trying to extend active edge ~A with inactive edge ~A"
	      (g-edge-id act) (g-edge-id inact)))
    (let ((unified-dag
	   (gen-chart-try-unification 
	    (g-edge-rule act)
	    (g-edge-dag act)
	    (first (g-edge-needed act))
	    (g-edge-dag-restricted act)
	    (position (first (g-edge-needed act))
		      (rest (rule-order (g-edge-res act))) :test #'eq)
	    inact
	    (not (rest (g-edge-needed act)))
	    (first (rule-order (g-edge-res act))))))
      (when unified-dag
	;; remaining non-head daughters in active edge are filled in
	;; left-to-right order
	(let ((new-act
	       (make-g-edge
		:id (next-edge) 
		:rule (g-edge-rule act)
		:dag unified-dag
		:res (g-edge-res act)
		:needed (rest (g-edge-needed act))
		:rels-covered
		(append (g-edge-rels-covered act)
			(g-edge-rels-covered inact))
		:children
		(gen-copy-list-and-insert (g-edge-children act)
					  inact)
		:lex-ids 
		(gen-copy-list-and-insert (g-edge-lex-ids act)
					  (g-edge-lex-ids inact))
		:leaves
		(gen-copy-list-and-insert (g-edge-leaves act)
					  (g-edge-leaves inact)))))
	  (if one-off-p new-act
	    (gen-chart-extend-active new-act input-sem)))))))


(defun gen-chart-extend-active (act input-sem)
   ;; (show-gen-chart)
   (if (g-edge-needed act)
      ;; add newly extended active edge to chart, then
      ;; look for any existing inactive edges which can extend it
      (let
         ((index
             (gen-chart-dag-index
                (existing-dag-at-end-of (tdfs-indef (g-edge-dag act))
                   (if (listp (first (g-edge-needed act)))
                      (first (g-edge-needed act))
                      (list (first (g-edge-needed act)))))
                (g-edge-id act))))
         (gen-chart-add-with-index act index)
         (dolist (e (gen-chart-retrieve-inactive-with-index index))
            (gen-chart-test-active e act input-sem)))
      ;; have ended up completing an active edge - forming a complete constituent
      (progn
         (gen-chart-finish-active act)
         (gen-chart-add-inactive act input-sem))))


(defun gen-chart-finish-active (act)
   ;; turn active into an inactive edge
   (setf (g-edge-res act) nil)
   (setf (g-edge-leaves act)
      (apply #'append (g-edge-leaves act)))
   act)


;;; Unification routines, entered only through gen-chart-try-unification

(defun gen-chart-try-unification (rule rule-tdfs daughter-path
				  daughter-restricted daughter-index edge 
				  completedp mother-path)
  ;; attempt to apply a grammar rule - c.f. apply-immediate-grammar-rule in
  ;; parse.lsp
  (when (and (check-rule-filter rule (g-edge-rule edge) daughter-index)
	     (restrictors-compatible-p daughter-restricted 
				       (g-edge-dag-restricted edge)))
    (gen-chart-evaluate-unification rule-tdfs daughter-path (g-edge-dag edge) 
				    completedp mother-path)))


(defun gen-chart-evaluate-unification (rule-tdfs daughter-index fs completedp
				       mother-path)
  ;; c.f. evaluate-unifications in parse.lsp
  ;; 
  ;; No orthography done here - it was done during the production of the
  ;; initial set of candidate lexical entries
  ;;
  ;; unify path <daughter-index> of rule-tdfs with fs, then if completedp is
  ;; true return mother portion of rule-tdfs
  (with-unification-context (ignore)
    (incf *gen-chart-unifs*)
    (unless
	(setq rule-tdfs
	  (yadu rule-tdfs
		(create-temp-parsing-tdfs
		 fs daughter-index)))
      (incf *gen-chart-fails*)
      (return-from gen-chart-evaluate-unification nil))
    (if completedp
	;; delete arcs just holding constituents' feature structures - before
	;; copying otherwise their copies would be thrown away immediately we
	;; have to check whether any of the deleted dags contain a cycle - if
	;; so then the whole rule application should fail
	(let*
            ((result (tdfs-at-end-of mother-path rule-tdfs))
             (real-dag (deref-dag (tdfs-indef result)))
             (new (clone-dag real-dag))
             (arcs-to-check nil))
	  (flet ((member-with-cyclic-check (arc)
		   (when (member (dag-arc-attribute arc) 
				 *deleted-daughter-features*)
		     (push arc arcs-to-check)
		     t)))
	    (setf (dag-arcs new)
	      (remove-if #'member-with-cyclic-check (dag-arcs new)))
	    (setf (dag-comp-arcs new)
	      (remove-if #'member-with-cyclic-check (dag-comp-arcs new)))
	    ;; take advantage of the fact that removed arcs might share
	    ;; structure by checking them all at once
	    (unless (cyclic-dag-p (make-dag :type *toptype* 
					    :arcs arcs-to-check))
	      ;; (setf (dag-copy new) 'copy)
	      (setf (dag-forward real-dag) new)
	      (copy-tdfs-elements result))))
      (copy-tdfs-elements rule-tdfs))))



;;; Second phase, where intersective modifiers are introduced
;;;
;;; if we could find just one preferred partial edge at this point then we
;;; would potentially save a lot of work of inserting modifiers into all the
;;; other analyses
;;;
;;; Could share some computation in reconstruction of parses after modifiers
;;; inserted - put into chart and re-use for other alternatives where same
;;; modifier is inserted in same place
;;;
;;; find missing rels and look for sets of potential modifier edges that cover
;;; them, then try inserting modifier edges into partial edge and recompute
;;; nodes higher up in tree
;;;
;;; all edges created from now on have index *toptype* since we're not trying
;;; to retrieve them from the chart

(defun gen-chart-adjoin-modifiers (partial-edges input-sem)
  (let ((*active-modifier-edges* nil)
	(mod-candidate-edges (gen-chart-intersective-inactive-edges))
	;; (cached-rels-edges (make-hash-table :test #'equal))
	(res nil))
    (declare (special *active-modifier-edges*))
    (when *gen-adjunction-debug*
      (format t "~%Candidate modifier edges ~:A" 
	      (mapcar #'edge-id mod-candidate-edges)))
    (dolist (partial partial-edges)
      (when *gen-adjunction-debug*
	(format t "~&---~%Partial edge [~A] spanning ~:A" (g-edge-id partial)
		(g-edge-leaves partial)))
      (let ((missing-rels
	     (gen-chart-set-difference
	      (mrs::psoa-liszt input-sem) 
	      (g-edge-rels-covered partial))))
	(when *gen-adjunction-debug*
	  (format t "~&Missing relations ~(~:A~)" 
		  (mapcar #'mrs::rel-sort missing-rels)))
	(dolist (mod-alt
		    (gen-chart-mod-edge-partitions missing-rels 
						   mod-candidate-edges))
	  (when *gen-adjunction-debug*
	    (format t "~&Relation partitions ~(~:A~)"
		    (mapcar
		     #'(lambda (rels-and-edges)
			 (cons (mapcar #'mrs::rel-sort (car rels-and-edges))
			       (mapcar #'g-edge-id (cdr rels-and-edges))))
		     mod-alt)))
	  (setq mod-alt (gen-chart-create-active-mod-edges mod-alt))
	  (let* ((e-ms-list
		  (gen-chart-try-modifiers (list* partial nil (mapcar #'cdr mod-alt))))
		 (complete
		  (gen-chart-find-complete-edges
		   (mapcan
		    #'(lambda (e-ms)
			(if (find-if #'consp (cddr e-ms)) nil (list (car e-ms))))
		    e-ms-list)
		   input-sem)))
	    (setq res (nconc complete res))))))
    res))


(defun gen-chart-intersective-inactive-edges nil
  ;; return a list of all inactive edges in chart which are able to function
  ;; as intersective modifiers
  (let ((res nil))
    (dolist (entry *gen-chart*)
      (dolist (e (cdr entry))
	(when
	    (and (not (g-edge-needed e))
		 ;; words like 'had' on their own with no semantics cannot be
		 ;; intersective modifiers
		 (g-edge-rels-covered e)
		 (intersective-modifier-dag-p (tdfs-indef (g-edge-dag e))))
	  (push e res))))
    res))

;; compute sets of partitions of input relations with each partition
;; containing the set of modifier edges that cover those relations: e.g.  (
;; (((r1 r2) e1 e2) ((r3 r4) e3)) (((r1) e4 e5 e6) ((r2 r3 r4) e5)) )

(defun gen-chart-mod-edge-partitions (rels edges)
  (let ((cache nil))
    ;; Find subsets of rels that are covered by some edge in edges
    (dolist (e edges)
      (let ((rels-covered (sort (copy-list (g-edge-rels-covered e))
				#'string<
				:key #'mrs::rel-sort)))
	(when (gen-chart-subset-p rels-covered rels)
	  (let ((entry (assoc rels-covered cache :test #'equal)))
	    (if entry 
		(pushnew e (cdr entry))
	      (push (cons rels-covered (list e)) cache))))))
    ;; Collect all the subsets, and compute the partitions of rels that can be
    ;; constructed out of them
    (mapcar #'(lambda (p)
		(mapcar #'(lambda (s)
			    (append (list s)
				    (cdr (assoc s cache :test #'equal))))
			p))
	    (make-partitions nil (mapcar #'car cache) rels))))

(defun make-partitions (partition rest set)
  (if (is-partition-p partition set)
      (list partition)
    (loop for tail on rest
	nconc (when (disjoint-p (car tail) partition)
		(make-partitions (cons (car tail) partition) 
				 (cdr tail) 
				 set)))))

(defun is-partition-p (subsets set)
  (let ((members (apply #'append subsets)))
    (dolist (x members)
      (unless (member x set :test #'eq)
	(return-from is-partition-p nil)))
    (dolist (x set t)
      (unless (member x members :test #'eq)
	(return-from is-partition-p nil)))))

(defun disjoint-p (set subsets)
  (dolist (s subsets t)
    (dolist (x s)
      (when (member x set :test #'eq)
	(return-from disjoint-p nil)))))

  
#+ignore
(defun gen-chart-mod-edge-partitions (rels next rest mod-candidate-edges 
				      cached-rels-edges)
  ;; compute sets of partitions of input relations with each partition
  ;; containing the set of modifier edges that cover those relations: e.g.  (
  ;; (((r1 r2) e1 e2) ((r3 r4) e3)) (((r1) e4 e5 e6) ((r2 r3 r4) e5)) ) this
  ;; gets expensive if there are many relations
  (let ((rels-edges (gethash rels cached-rels-edges t)))
    (when (eq rels-edges t)
      (setq rels-edges nil)
      (dolist (e mod-candidate-edges)
	(let ((rels-covered (g-edge-rels-covered e)))
	  (when (gen-chart-set-equal-p rels-covered rels)
	    (push e rels-edges))))
      (setf (gethash rels cached-rels-edges) rels-edges))
    (if rest
	(nconc
	 (when rels-edges
	   (mapcar
	    #'(lambda (alt) (cons (cons rels rels-edges) alt))
	    (gen-chart-mod-edge-partitions
	     (list (car rest)) (cdr rest) (cdr rest) mod-candidate-edges
	     cached-rels-edges)))
	 (mapcon
	  #'(lambda (tail)
	      (gen-chart-mod-edge-partitions (cons (car tail) rels)
					     (cdr tail) (remove (car tail) rest :test #'eq)
					     mod-candidate-edges cached-rels-edges))
	  next))
      (when rels-edges (list (list (cons rels rels-edges)))))))



(defun gen-chart-create-active-mod-edges (mod-alt)
  (flet
      ((active-from-inactive (inact)
         (mapcan
	  #'(lambda (rule-name)
	      (let ((rule (get-grammar-rule-entry rule-name)))
		(mapcan
		 #'(lambda (path)
		     (let ((act
			    (gen-chart-create-active 
			     rule inact
			     (cons path (remove path (cdr (rule-order rule))))
			     (position path (cdr (rule-order rule))))))
		       (when act
			 (unless (eql (length (g-edge-needed act)) 1)
			   (error "Intersective modification rule ~A is not ~
                                   binary branching" (rule-id rule)))
			 (gen-chart-add-with-index act *toptype*)
			 (when *gen-adjunction-debug*
			   (format t "~&Inactive [~A] -> active [~A]"
				   (g-edge-id inact) (g-edge-id act)))
			 (list act))))
		 (cdr (rule-order rule)))))
	  *intersective-rule-names*)))
    (declare (special *active-modifier-edges*))
    (mapcar
     #'(lambda (rels-and-edges)
	 (cons (car rels-and-edges)
	       (mapcan
		#'(lambda (inact)
		    (let ((mods (getf *active-modifier-edges* inact t)))
		      (copy-list
		       (if (eq mods t)
			   (setf (getf *active-modifier-edges* inact)
			     (active-from-inactive inact))
			 mods))))
		(cdr rels-and-edges))))
     mod-alt)))


;;; Take set of possible modifier edges and try to apply each at every node in
;;; partial tree. Only need to redo unifications above node replacements

(defun gen-chart-try-modifiers (edge-and-mods)
   (if (member (car edge-and-mods) (cadr edge-and-mods) :test #'eq)
      (list edge-and-mods) ; we've already processed this node
      (progn
         (setq edge-and-mods
            (list* (car edge-and-mods) (cons (car edge-and-mods) (cadr edge-and-mods))
               (cddr edge-and-mods)))
         (let ((daughters-alts (list (cons nil (cdr edge-and-mods)))))
            (dolist (d (gen-chart-try-modifiers-children (car edge-and-mods)))
               (setq daughters-alts
                  (gen-chart-try-modifiers-daughter d daughters-alts)))
            (gen-chart-apply-modifiers
               (cons edge-and-mods
                  (if (cdr daughters-alts)
                     (mapcan
                        #'(lambda (mods-and-daughters)
                            (gen-chart-reapply-rule (car edge-and-mods)
                               (reverse (car mods-and-daughters)) (cdr mods-and-daughters)))
                        (cdr daughters-alts)))))))))

(defun gen-chart-try-modifiers-children (e)
   (mapcan
      #'(lambda (c)
          (when c
             (if (g-edge-needed c)
                (gen-chart-try-modifiers-children c)
                (list c))))
      (g-edge-children e)))

(defun gen-chart-try-modifiers-daughter (d daughters-alts)
   (mapcan
      #'(lambda (alt)
          (mapcar
             #'(lambda (d-and-mods)
                 (cons (cons (car d-and-mods) (car alt)) (cdr d-and-mods)))
             (gen-chart-try-modifiers (cons d (cdr alt)))))
      daughters-alts))


(defun gen-chart-apply-modifiers (edge-and-mods-list)
  ;; try and apply to edge one modifying active edge from an alternative
  (append 
   edge-and-mods-list
   (mapcan
    #'(lambda (edge-and-mods)
	(let ((edge (car edge-and-mods))
	      (res nil))
	  (dolist (act-set (cddr edge-and-mods))
	    (dolist (act act-set)
	      ;; (print (list (g-edge-id edge) (g-edge-id act)))
	      (let ((new-edge (gen-chart-test-active edge act nil t)))
		(when new-edge
		  (gen-chart-finish-active new-edge) 
		  ;;(when *gen-adjunction-debug*
		  ;;   (print
		  ;;      (list (g-edge-leaves new-edge)
		  ;;         (g-edge-id act) (g-edge-id new-edge))))
		  (gen-chart-add-with-index new-edge (g-edge-index edge))
		  (setq res
		    (append
		     (gen-chart-try-modifiers
		      (list* new-edge (cadr edge-and-mods)
			     (mapcar #'(lambda (set) 
					 (unless (eq set act-set) set))
				     (cddr edge-and-mods))))
		     res))))))
	  res))
    edge-and-mods-list)))


(defun gen-chart-reapply-rule (edge daughters mods) ; -> list of edge-and-mods
   (let*
      ((rule (edge-rule edge))
       (unified-dag (rule-full-fs rule)))
      (with-unification-context (ignore)
         (loop 
	    while unified-dag
            for path in (cdr (rule-order rule))
            for dtr in daughters
	    as dtr-fs = (g-edge-dag dtr)
	    do (setq unified-dag
	         (yadu unified-dag (create-temp-parsing-tdfs dtr-fs path))))
         (setq unified-dag
            (and unified-dag
               (copy-tdfs-elements
                  (tdfs-at-end-of (car (rule-order rule)) unified-dag))))
         (when unified-dag 
            (let
               ((new-edge
                  (make-g-edge
                     :id (next-edge) :rule rule
                     :dag unified-dag
                     :needed nil
                     :rels-covered
                     (apply #'append
                        (if (mrs::found-rule-p rule)
                           (mrs::found-rule-main-rels rule))
                        (mapcar #'g-edge-rels-covered daughters))
                     :children daughters
                     :lex-ids (apply #'append
                                     (mapcar #'g-edge-lex-ids daughters))
                     :leaves (mapcar #'g-edge-leaves daughters))))
               (gen-chart-finish-active new-edge)
               (gen-chart-add-with-index new-edge (g-edge-index edge))
               (list (cons new-edge mods)))))))


;;; Print out contents of generator chart (tty output) - (print-gen-chart)

(defun print-gen-chart ()
   (format t "~&------~%")
   (dolist (entry *gen-chart*)
      (format t "Vertex ~(~A~):~%" (car entry))
      (dolist (e (reverse (cdr entry))) ; to get in chronological order
         (format t "[~A] ~A~A ~30,5T=> (~{~:A~^ ~})  [~{~A~^ ~}]~%"
            (g-edge-id e)
            (if (rule-p (g-edge-rule e)) (rule-id (g-edge-rule e))
                (g-edge-rule e))
            (if (g-edge-needed e)
               (format nil " / ~{~A~^ ~}" (g-edge-needed e))
               "")
            (g-edge-leaves e)
            (mapcan
               #'(lambda (x) (if x (list (g-edge-id x))))
               (g-edge-children e)))))
   (format t "~%"))


#|
;;; for debugging collect orth / mrs of entries in lex sets

(mapcar
   #'(lambda (lex-entry-alt)
        (mapcar #'extract-orth-from-fs lex-entry-alt))
   lex-entry-alts)

(mapcar
   #'(lambda (lex-entry-alt)
        (mapcar #'(lambda (lex-entry-fs)
           (existing-dag-at-end-of (tdfs-indef lex-entry-fs)
              mrs::*initial-semantics-path*))
        lex-entry-alt))
   lex-entry-alts)
|#


;;; End of file
