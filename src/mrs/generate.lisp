;;; Copyright John Carroll 1998 All Rights Reserved.
;;; No use or redistribution without permission.
;;;
;;; CSLI, Stanford University, USA

(in-package :lkb)


;;; moved structures to parse.lsp since also used in interface

(defvar *gen-chart* nil)
(defvar *gen-record* nil)
(defvar *non-intersective-rules* nil)
(defvar *lexemes-allowed-orderings* nil)

(defparameter *gen-adjunction-debug* nil)
(defparameter *gen-equality-debug* nil)

(defvar *bypass-equality-check* nil)

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


;;; Functions on sets (of MRS relations)

(defun gen-chart-set-intersection-p (lst1 lst2)
   (dolist (x lst1) (when (member x lst2 :test #'eq) (return t))))

(defun gen-chart-subset-p (lst1 lst2)
   (dolist (x lst1 t) (unless (member x lst2 :test #'eq) (return nil))))

(defun gen-chart-set-difference (lst1 lst2)
   (remove-if #'(lambda (x) (member x lst2 :test #'eq)) lst1))


(defun gen-chart-ordering-allowed-p (left right)
  ;; ordering will already have been checked within both left and right
  (dolist (ordering *lexemes-allowed-orderings* t)
     (do ((rtail (cdr ordering) (cdr rtail)))
        ((null rtail))
        (when (gen-chart-set-intersection-p left (car rtail))
           (do ((ltail ordering (cdr ltail)))
              ((eq ltail rtail))
              (when (gen-chart-set-intersection-p right (car ltail))
                 ;; (print (list (mapcar #'mrs::found-lex-lex-id left) 
                 ;;              (mapcar #'mrs::found-lex-lex-id right)))
                 (return-from gen-chart-ordering-allowed-p nil)))
           ;; this ordering constraint can't contain another set of
           ;; lexemes matching left, so go on and try next constraint
           (return-from nil)))))


;;; Interface to generator - take an input MRS, ask for instantiated lexical
;;; items, instantiated and uninstantiated rules that might be applicable,
;;; and partial ordering on lexical items, and then call generator
;;; proper. Clear chart and analyses record before entry in case we don't make
;;; it into generation proper. Do it also in chart-generate since that is also
;;; an entry point

(defun generate-from-mrs (input-sem)
  (with-package (:lkb)
    (clear-gen-chart)
    (setf *cached-category-abbs* nil)
    (multiple-value-bind (lex-results grules lex-orderings)
        (mrs::collect-lex-entries-from-mrs input-sem)
      (multiple-value-bind (lex-items grules lex-orderings)
          (filter-generator-lexical-items (apply #'append lex-results)
             grules lex-orderings)
#|
        (dolist (lex lex-items)
           (format t "~%Id ~A, Index ~A, Lexical rules ~:A, Main rel sorts ~:A"
              (mrs::found-lex-lex-id lex)
              (gen-chart-dag-index
                 (existing-dag-at-end-of
                    (tdfs-indef (mrs::found-lex-inst-fs lex)) *semantics-index-path*)
                 nil)
              (mrs::found-lex-rule-list lex)
              (mapcar #'mrs::rel-sort (mrs::found-lex-main-rels lex))))
         (print
            (sort (remove-duplicates (mapcar #'mrs::found-lex-lex-id lex-items))
               #'string-lessp))
         (finish-output)
         (dolist (grule grules)
            (when (mrs::found-rule-p grule)
               (format t "~%Id ~A, Index ~A, Main rel sorts ~:A"
                  (mrs::found-rule-id grule)
                  (gen-chart-dag-index
                     (existing-dag-at-end-of
                        (tdfs-indef (mrs::found-rule-full-fs grule)) *semantics-index-path*)
                     nil)
                  (mapcar #'mrs::rel-sort (mrs::found-rule-main-rels grule)))))
|#
         (if lex-items
            (chart-generate input-sem lex-items grules lex-orderings)
            (progn
               (format t "~%Some lexical entries could not be found from MRS ~
                          relations - has function index-for-generator been run ~
                          yet?")
               nil))))))


(defun filter-generator-lexical-items (lex-items grules lex-orderings)
   (values
      (remove-if
          #'(lambda (x) 
              (or #+ignore (search "_CX" (string x))) ; contracted forms
                 (member x *duplicate-lex-ids* :test #'eq)) ; e.g. a -> an
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
;;; returns values: realisations, time taken (msecs), number of unification
;;; attempts, number of unification failures, number of active edges, number
;;; of inactive, total number of chart edges

(defun chart-generate (input-sem found-lex-items possible-grules
                       *lexemes-allowed-orderings*
                       &optional (*gen-first-only-p* *gen-first-only-p*))
  ;;(when (> (length found-lex-items) 80)
  ;;   (format t "~%More than 80 initial lexical items - skipping")
  ;;   (return-from chart-generate nil))
  (unless *intersective-rule-names*
    (format t "~%Warning: no intersective rules defined"))
  (let ((*safe-not-to-copy-p* t)
        (*filtered-tasks* 0) (*executed-tasks* 0) (*successful-tasks* 0) 
        (*unifications* 0) (*copies* 0) 
        (*non-intersective-rules*
          ;; this is all rules for best-first mode
          (remove-if
            #'(lambda (r)
                (or (and (not *gen-first-only-p*)
                         (member (rule-id r) *intersective-rule-names* :test #'eq))
                   (spelling-change-rule-p r)))
            possible-grules)))
    #+(and mcl powerpc) (setq aa 0 bb 0 cc 0 dd 0 ee 0 ff 0 gg 0 hh 0 ii 0 jj 0)
    (with-parser-lock ()
      ;; also an entry point so ensure chart is clear
      (clear-gen-chart) 
      (setf *cached-category-abbs* nil)
      (flush-heap *agenda*)
      (unless (catch 'first
                ;; Add lexical edges
                (mapc #'(lambda (found) 
                          (let* ((lex-entry-fs (mrs::found-lex-inst-fs found))
                                 (word (extract-orth-from-fs lex-entry-fs)))
                            (incf *copies*) ; each lex entry will be a copy
                            (with-agenda (when *gen-first-only-p* 
                                           (gen-lex-priority lex-entry-fs))
                              (gen-chart-add-inactive 
                               (make-g-edge
                                :id (next-edge) 
                                :category (indef-type-of-tdfs lex-entry-fs)
                                :rule word
                                :dag lex-entry-fs
                                :needed nil
                                :dag-restricted
                                (restrict-fs (tdfs-indef lex-entry-fs))
                                :rels-covered (mrs::found-lex-main-rels found)
                                :children nil
                                :leaves (list word)
                                :lex-ids (list (mrs::found-lex-lex-id found))
                                :lexemes (list found))
                               input-sem))))
                      found-lex-items)
                ;; Process tasks
                (loop 
                  until (empty-heap *agenda*)
                  do (funcall (heap-extract-max *agenda*))))
        (unless *gen-first-only-p*
          ;; perform adjunction phase - except this will have effectively been done
          ;; already if we're in best-first mode
          (multiple-value-bind (complete partial)
              (gen-chart-find-complete-edges
                 (gen-chart-retrieve-with-index *toptype* 'inactive) input-sem)
            (setq *gen-record*
               (nconc complete
                  (when *intersective-rule-names*
                     (gen-chart-adjoin-modifiers partial input-sem possible-grules)))))))
;;       #+(and mcl powerpc) (format t
;; "~%copy ~A, unif ~A, types ~A, complete copy ~A, psort read ~A,
;; unify-paths-dag-at-end-of ~A, morph-analyse ~A: total ~A"
;;           aa (- bb cc dd) cc dd ee ff gg (+ aa (- bb cc dd) cc dd ee ff gg))
      (let* ((inact-tot
              (length (gen-chart-retrieve-with-index *toptype* 'inactive)))
             (act-tot
              (length (gen-chart-retrieve-with-index *toptype* 'active))))
        (values
         ;; extract-string-from-gen-record looks after changes like
         ;; e.g. "a apple" -> "an apple"
         (extract-strings-from-gen-record found-lex-items)
         *filtered-tasks* *executed-tasks* *successful-tasks* *unifications* *copies*
         act-tot inact-tot)))))


(defun extract-strings-from-gen-record nil
  (loop for edge in *gen-record*
      nconc
        (list
         (fix-spelling
          ;; in spell.lsp
          (g-edge-leaves edge)))))


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
        (cond
           ((not (gen-chart-check-complete new input-sem))
              (push new partial))
           ((gen-chart-check-compatible new input-sem)
              (push new complete))
           (t (push new incompatible)))))
    (when *gen-adjunction-debug*
      (format t "~&Complete compatible edges: ~:A~%Complete incompatible edges: ~:A~
                    ~%Partial edges: ~:A"
              (mapcar #'g-edge-id complete) (mapcar #'g-edge-id incompatible)
              (mapcar #'g-edge-id partial)))
    (when (and *gen-equality-debug* (not *gen-adjunction-debug*))
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
;;         (when *sem-debugging*
;;           (mrs::output-mrs input-sem 'mrs::simple)
;;           (mrs::output-mrs mrs 'mrs::simple))  
         (or *bypass-equality-check*
             (mrs::mrs-equalp mrs input-sem nil *debugging*))))))


(defun gen-chart-root-edges (edge start-symbols)
  ;; c.f. create-new-root-edges in parse.lsp
   (loop for start-symbol in start-symbols        
       nconc
       (let ((tdfs (get-tdfs-given-id 
                    start-symbol)))
         (if tdfs
             (let ((unif 
                    (yadu tdfs (g-edge-dag edge))))
               (if unif
                   (let ((new-edge
                          (make-g-edge :dag unif
                                       :id (next-edge)
                                       :category (indef-type-of-tdfs unif)
                                       :rule 'root
                                       :needed nil
                                       :dag-restricted
                                       (restrict-fs (tdfs-indef unif))
                                       :children (list edge)
                                       :leaves (g-edge-leaves edge)
                                       :lex-ids (g-edge-lex-ids edge)
                                       :lexemes (g-edge-lexemes edge))))
                     (gen-chart-add-with-index new-edge)
                     (list
                      new-edge))))))))

(defun gen-filter-root-edges (edge start-symbols)
   ;; c.f. filter-root-edges in parse.lsp
   (dolist (start-symbol start-symbols)
      (let ((root-spec (get-tdfs-given-id start-symbol)))
         (when
            (and root-spec
               (unifiable-wffs-p
                  (tdfs-indef root-spec) (tdfs-indef (g-edge-dag edge))))
            (return (list edge))))))


;;; Chart indexing - on *semantics-index-path* values. May be full types or
;;; instance types. Seem to be mostly disjoint if not eq, so don't bother using
;;; a tree-like representation
;;;
;;; Apart from these fns, print-gen-chart, find-gen-edge-given-id and
;;; create-gen-chart-pointers are the only functions that need to know the
;;; internal representation of chart

(defun gen-chart-dag-index (index-dag edge-id)
   (if index-dag
      (let ((index (unify-get-type index-dag))) ; may be called inside unif context
         (when (and (consp index) (null (cdr index)))
            ;; simplify single-item disjunction - doesn't need to be stored as
            ;; an atomic type and will speed up subsequent type lookup
            (setq index (car index)))
         index)
      (progn
         ;;(cerror (format nil "use type ~A" *toptype*)
         ;;   "unexpectedly missing index for edge ~A: ~S" edge-id dag)
         (warn "unexpectedly missing index for edge ~A - using ~A" edge-id *toptype*)
         *toptype*)))


(defun gen-chart-add-with-index (edge &optional chart-index)
   (let ((index
           (or chart-index
              (gen-chart-dag-index
               (or 
                (existing-dag-at-end-of
                 (tdfs-indef (g-edge-dag edge)) *semantics-index-path*)
                (if *alt-semantics-index-path*
                    (existing-dag-at-end-of
                     (tdfs-indef (g-edge-dag edge)) 
                     *alt-semantics-index-path*)))
                 (g-edge-id edge)))))
      (setf (g-edge-index edge) index)
      (let ((entry (assoc index *gen-chart* :test #'equal))) ; may be a cons
         (unless entry
            (push (setq entry (list* index nil nil)) *gen-chart*))
         ;; active edges are stored in the cadr of chart entries, inactive
         ;; in the cddr
         (if (g-edge-needed edge) (push edge (cadr entry))
            (push edge (cddr entry)))
         index)))


(defun gen-chart-retrieve-with-index (index mode)
  ;; return all active/inactive edges in chart keyed by a type compatible with index
  (let ((res nil))
    (dolist (entry *gen-chart* res)
      (when (greatest-common-subtype (car entry) index)
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

(defun gen-chart-add-inactive (edge input-sem)
  ;; assume that all relations supplied in lexical entries and rules
  ;; are relevant to input-sem, i.e. that if we've generated an edge
  ;; it won't contain any relations that aren't in input-sem
  (let ((index (gen-chart-add-with-index edge)))
    ;; did we just find a result?
    (when *gen-first-only-p*
      (let ((complete (gen-chart-find-complete-edges (list edge) input-sem)))
        (setq *gen-record* complete)
        (when *gen-record*
          (throw 'first t))))
    ;; see if this new inactive edge can extend any existing active edges
    (mapc
       #'(lambda (act)
           (with-agenda (when *gen-first-only-p* 
                          (gen-rule-priority (edge-rule act)))
             (gen-chart-test-active edge act input-sem)))
       (gen-chart-retrieve-with-index index 'active))
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
                  (multiple-value-bind (act chart-index)
                       (gen-chart-create-active
                          rule edge gen-daughter-order head-index)
                    (when act
                      ;; and try and fill in next needed daughter of active edge
                      (gen-chart-extend-active act input-sem chart-index))))))
          *non-intersective-rules*)))


(defun gen-chart-create-active (rule edge gen-daughter-order head-index)
   (multiple-value-bind (unified-dag restricted chart-index)
          (gen-chart-try-unification rule
             (rule-full-fs rule)
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
                  (append
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
                  :lexemes (g-edge-lexemes edge))
               chart-index)))))


(defun gen-chart-test-active (inact act input-sem &optional one-off-p)
  ;; can extend active edge with inactive? First check to make sure new edge
  ;; would not use any relation from initial lexical items more than once.
  ;; Assumption here that rels are eq between alternative lexical items
  (when (and
           (or one-off-p ; intersection guaranteed to be OK if one-off-p
               ;; should probably do the rule compatibility test before this
               ;; since it's cheaper - alternatively might help
               ;; longer sentences to change intersection test on lists to
               ;; and-ing of bit vectors since symbolic values not used
               (not (gen-chart-set-intersection-p
                       (g-edge-rels-covered act) (g-edge-rels-covered inact))))
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
                     (append (g-edge-rels-covered act)
                             (g-edge-rels-covered inact))
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
                     :mod-index (g-edge-mod-index act))))
               (if one-off-p new-act
                 (gen-chart-extend-active new-act input-sem index-dag))))))))


(defun gen-chart-extend-active (act input-sem act-chart-index)
   (if (g-edge-needed act)
      ;; add newly extended active edge to chart, then look for any existing
      ;; inactive edges which can extend it
      (let ((index (gen-chart-add-with-index act act-chart-index)))
         (dolist (e (gen-chart-retrieve-with-index index 'inactive))
            (gen-chart-test-active e act input-sem)))
      ;; have ended up completing an active edge - forming a complete constituent
      (gen-chart-add-inactive
         (gen-chart-finish-active act) input-sem)))


(defun gen-chart-finish-active (act)
   ;; turn active into an inactive edge
   (setf (g-edge-category act) (indef-type-of-tdfs (g-edge-dag act)))
   (setf (g-edge-res act) nil)
   (setf (g-edge-lex-ids act)
      (apply #'append (g-edge-lex-ids act)))
   (setf (g-edge-leaves act)
      (apply #'append (g-edge-leaves act)))
   act)


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
                 (gen-chart-dag-index
                    (existing-dag-at-end-of
                       (tdfs-indef dag) (append (first needed) *semantics-index-path*))
                    nil))))
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
           (gen-chart-dag-index
              (x-existing-dag-at-end-of (tdfs-indef rule-tdfs)
                 (append (first needed) *semantics-index-path*))
              nil))
        (let ((dag (gen-chart-restrict-and-copy
                      (tdfs-at-end-of mother-path rule-tdfs))))
           (when dag (values dag (restrict-fs (tdfs-indef dag))))))))


(defun gen-chart-restrict-and-copy (dag)
   ;; delete arcs just holding constituents' feature structures -
   ;; before copying otherwise their copies would be thrown away
   ;; immediately we have to check whether any of the deleted dags
   ;; contain a cycle - if so then the whole rule application should
   ;; fail. C.f. parser function evaluate-unifications
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
;;;
;;; if we could find just one preferred partial edge at this point then we
;;; would potentially save a lot of work of inserting modifiers into all the
;;; other analyses
;;;
;;; find missing rels and look for sets of potential modifier edges that cover
;;; them, then try inserting modifier edges into partial edge and recompute
;;; nodes higher up in tree
;;;
;;; all edges created from now on have index *toptype* since we're not trying
;;; to retrieve them from the chart

(defun gen-chart-adjoin-modifiers (partial-edges input-sem possible-grules)
  (unless partial-edges (return-from gen-chart-adjoin-modifiers nil))
  (let ((*gen-modifier-active-edges* nil) 
        (*gen-composed-modifier-edges* (make-hash-table :test #'equal))
        (mod-candidate-edges (gen-chart-intersective-inactive-edges))
        (intersective-rules
          (remove-if-not
             #'(lambda (r)
                 (member (rule-id r) *intersective-rule-names* :test #'eq))
             possible-grules))
        (res nil))
    (declare (special *gen-modifier-active-edges* *gen-composed-modifier-edges*))
    (when *gen-adjunction-debug*
      (format t "~%Candidate modifier edges ~:A" 
              (mapcar #'edge-id mod-candidate-edges)))
    (dolist (partial partial-edges)
      (when *gen-adjunction-debug*
        (format t "~&---~%Partial edge [~A] spanning ~:A" (g-edge-id partial)
                (g-edge-leaves partial)))
      (let ((missing-rels
             (gen-chart-set-difference
              (mrs::psoa-liszt input-sem) (g-edge-rels-covered partial))))
        (when *gen-adjunction-debug*
          (format t "~&Missing relations ~(~:A~)" 
                  (mapcar #'mrs::rel-sort missing-rels)))
        (let ((mod-alt-list
               (gen-chart-mod-edge-partitions missing-rels 
                                              mod-candidate-edges)))
          (when (and *gen-adjunction-debug* (null mod-alt-list))
            (format t "~&No relation partitions found"))
          (dolist (mod-alt mod-alt-list)
            (when *gen-adjunction-debug*
              (format t "~&Trying relation partition ~(~:A~)"
                 (mapcar
                    #'(lambda (rels-and-edges)
                         (cons (mapcar #'mrs::rel-sort (car rels-and-edges))
                               (mapcar #'g-edge-id (cdr rels-and-edges))))
                    mod-alt)))
            (setq mod-alt
               (gen-chart-create-active-mod-edges mod-alt intersective-rules))
            (when (every #'identity (mapcar #'cdr mod-alt))
              (setq res
                (nconc
                  (gen-chart-find-complete-edges
                    (mapcan
                      #'(lambda (edge-and-mods)
                          (if (null (cddr edge-and-mods))
                             (list (car edge-and-mods))))
                      (gen-chart-try-modifiers (list* partial nil (mapcar #'cdr mod-alt))))
                    input-sem)
                  res)))))))
    res))


(defun gen-chart-intersective-inactive-edges nil
  ;; return a list of all inactive edges in chart which are able to function
  ;; as intersective modifiers
  (remove-if-not
     #'(lambda (e)
          (and
             ;; words like 'had' on their own with no semantics cannot be
             ;; intersective modifiers
             (g-edge-rels-covered e)
             (intersective-modifier-dag-p (tdfs-indef (g-edge-dag e)))))
     (gen-chart-retrieve-with-index *toptype* 'inactive)))


;;; Compute sets of partitions of input relations with each partition
;;; containing the set of modifier edges that cover those relations: e.g.
;;; ( (((r1 r2) e1 e2) ((r3 r4) e3)) (((r1) e4 e5 e6) ((r2 r3 r4) e5)) )

(defun gen-chart-mod-edge-partitions (rels edges)
  (let ((cache nil))
    ;; find subsets of rels that are covered by some edge in edges
    (dolist (e edges)
      (let ((rels-covered (g-edge-rels-covered e)))
        (when (gen-chart-subset-p rels-covered rels)
          (let ((entry (assoc rels-covered cache :test #'equal)))
            (if entry 
                (pushnew e (cdr entry))
              (push (cons rels-covered (list e)) cache))))))
    ;; collect all the subsets, and compute the partitions of rels that can be
    ;; constructed out of them
    (mapcar #'(lambda (p)
                (mapcar #'(lambda (s)
                            (append (list s)
                                    (cdr (assoc s cache :test #'equal))))
                        p))
            (gen-make-partitions nil (mapcar #'car cache) rels))))

(defun gen-make-partitions (partition rest set)
  (if (gen-partition-p partition set)
      (list partition)
    (loop for tail on rest
        nconc (when (gen-disjoint-p (car tail) partition)
                (gen-make-partitions (cons (car tail) partition) 
                                     (cdr tail) 
                                     set)))))

(defun gen-partition-p (subsets set)
  (let ((members (apply #'append subsets)))
    (dolist (x members)
      (unless (member x set :test #'eq)
        (return-from gen-partition-p nil)))
    (dolist (x set t)
      (unless (member x members :test #'eq)
        (return-from gen-partition-p nil)))))

(defun gen-disjoint-p (set subsets)
  (dolist (s subsets t)
    (dolist (x s)
      (when (member x set :test #'eq)
        (return-from gen-disjoint-p nil)))))

  
;;; Make active edges from inactive modifier edges, when not already done

(defun gen-chart-create-active-mod-edges (mod-alt intersective-rules)
  (flet
      ((active-from-inactive (inact)
         (mapcan
          #'(lambda (rule)
              (mapcan
                 #'(lambda (path)
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
                            (position path (cdr (rule-order rule)) :test #'eq))
                         (gen-chart-add-with-index act index-dag)
                         (when *gen-adjunction-debug*
                           (format t "~&Inactive [~A] -> active [~A]"
                                   (g-edge-id inact) (g-edge-id act)))
                         (list act))))
                 (cdr (rule-order rule))))
          intersective-rules)))
    (declare (special *gen-modifier-active-edges*))
    (mapcar
     #'(lambda (rels-and-edges)
         (cons (car rels-and-edges)
               (mapcan
                #'(lambda (inact)
                    (let ((mods (getf *gen-modifier-active-edges* inact t)))
                      (copy-list
                       (if (eq mods t)
                           (setf (getf *gen-modifier-active-edges* inact)
                             (active-from-inactive inact))
                         mods))))
                (cdr rels-and-edges))))
     mod-alt)))


;;; Take set of possible modifier edges and try to apply each at every node in
;;; partial tree. Only need to redo unifications above node replacements
;;;
;;; Edges resulting from insertion of modifiers and also ones recomputed above
;;; insertions are saved for use in other partial analyses where same insertion
;;; takes place
;;;
;;; Would be more efficient to stop just before reapplication of top rule if there
;;; are still modifiers left - of course as long as the top of the modifier can't
;;; be a root node

(defun gen-chart-try-modifiers (edge-and-mods)
   (if (or (null (cddr edge-and-mods))
           (member (car edge-and-mods) (cadr edge-and-mods) :test #'eq))
      ;; we've added all adjuncts or have already processed this node
      (list edge-and-mods)
      (progn
         (setq edge-and-mods
            (list* (car edge-and-mods) (cons (car edge-and-mods) (cadr edge-and-mods))
               (cddr edge-and-mods)))
         (let ((daughters-alts (list (cons nil (cdr edge-and-mods)))))
            (dolist (d (g-edge-children (car edge-and-mods)))
               (setq daughters-alts
                  (gen-chart-try-modifiers-daughter d daughters-alts)))
            (gen-chart-apply-modifiers
               (cons edge-and-mods
                  (if (cdr daughters-alts)
                     (mapcan
                        #'(lambda (mods-and-daughters)
                            (let ((new-edge
                                   (gen-chart-reapply-rule
                                      (edge-rule (car edge-and-mods))
                                      (reverse (car mods-and-daughters))
                                      (car edge-and-mods))))
                               (when new-edge
                                  (list
                                     (list* new-edge (cadr mods-and-daughters)
                                        (cddr mods-and-daughters))))))
                        (cdr daughters-alts)))))))))

(defun gen-chart-try-modifiers-daughter (d daughters-alts)
   (mapcan
      #'(lambda (alt)
          (mapcar
             #'(lambda (d-and-mods)
                 (cons (cons (car d-and-mods) (car alt)) (cdr d-and-mods)))
             (gen-chart-try-modifiers (cons d (cdr alt)))))
      daughters-alts))


(defun gen-chart-apply-modifiers (edge-and-mods-list)
   (declare (special *gen-composed-modifier-edges*))
   (let ((res nil))
      (dolist (edge-and-mods edge-and-mods-list)
        (let ((edge (car edge-and-mods)))
          ;; try and apply to edge one modifying active edge from an alternative
          (dolist (act-set (cddr edge-and-mods))
            (dolist (act act-set)
              (let ((new-edge
                      (and (greatest-common-subtype (g-edge-index edge) (g-edge-index act))
                         (or (gethash (cons edge act) *gen-composed-modifier-edges*)
                            (let ((e (gen-chart-test-active edge act nil t)))
                               (when e
                                 (gen-chart-add-with-index (gen-chart-finish-active e))
                                 (setf (gethash (cons edge act) *gen-composed-modifier-edges*)
                                    e)))))))
                (when new-edge
                  (setq res
                    (nconc res
                      ;; now go down into newly adjoined structure and try to
                      ;; add other adjuncts
                      (gen-chart-try-modifiers
                        (list* new-edge (cadr edge-and-mods)
                          (remove act-set (cddr edge-and-mods) :test #'eq)))))))))))
      (append edge-and-mods-list res)))


(defun gen-chart-reapply-rule (rule daughters orig)
  ;; apply rule to daughters, creating a new edge if successful
  (declare (special *gen-composed-modifier-edges*))
  (or (gethash (cons rule daughters) *gen-composed-modifier-edges*)
    (let ((unified-dag (rule-full-fs rule)))
      (with-unification-context (ignore)
         (loop 
            while unified-dag
            for path in (cdr (rule-order rule))
            for dtr in daughters
            while
            (or (null prev-lexemes)
                (gen-chart-ordering-allowed-p
                   prev-lexemes (g-edge-lexemes dtr))
                (setq unified-dag nil))
            as prev-lexemes = (append prev-lexemes (g-edge-lexemes dtr))
            as dtr-fs = (g-edge-dag dtr)
            do
            (progn
               (incf *executed-tasks*)
               ;; it appears that these unifications seldom fail, so don't
               ;; bother applying the quick check here
               (setq unified-dag
                  (yadu unified-dag (create-temp-parsing-tdfs dtr-fs path)))
               (when unified-dag (incf *successful-tasks*))))
         (when unified-dag
            (setq unified-dag
               (gen-chart-restrict-and-copy
                  (tdfs-at-end-of (car (rule-order rule)) unified-dag))))
         (when unified-dag 
            (let
               ((new-edge
                  (make-g-edge :dag unified-dag
                     :id (next-edge)
                     ;; category filled in by gen-chart-finish-active call below
                     :rule rule
                     :needed nil
                     :dag-restricted (restrict-fs (tdfs-indef unified-dag))
                     :rels-covered
                     (apply #'append
                        (if (mrs::found-rule-p rule)
                            (mrs::found-rule-main-rels rule))
                        (mapcar #'g-edge-rels-covered daughters))
                     :children daughters
                     :lex-ids (mapcar #'g-edge-lex-ids daughters)
                     :leaves (mapcar #'g-edge-leaves daughters)
                     :lexemes (apply #'append (mapcar #'g-edge-lexemes daughters))
                     :mod-index (g-edge-mod-index orig))))
               (gen-chart-add-with-index (gen-chart-finish-active new-edge))
               (setf (gethash (cons rule daughters) *gen-composed-modifier-edges*)
                  new-edge)))))))


;;; Print out contents of generator chart (tty output) - (print-gen-chart)

(defun print-gen-chart (&key (stream t))
   (format stream "~&------~%")
   (dolist (entry (reverse *gen-chart*)) ; order in which originally created
      (format stream "~%Vertex ~(~A~):~%" (car entry))
      (dolist (e (sort (append (cadr entry) (copy-list (cddr entry))) #'<
                       :key #'edge-id))
         (format stream "[~A] ~A~A ~30,5T=> (~{~:A~^ ~})  [~{~A~^ ~}]~%"
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
   (format stream "~%"))


;;; End of file
