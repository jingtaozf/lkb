;;; Copyright John Carroll 1998 All Rights Reserved.
;;; No use or redistribution without permission.
;;;
;;; CSLI, Stanford University, USA


;;;

(defstruct (dotted-chart-edge (:include edge))
   res ; feature name of mother in rule dag in active edges
   needed ; ordered list of names of daughter features still to be found
   )

(defstruct (gen-chart-edge (:include dotted-chart-edge))
   ;; category: not used
   ;; id, rule-number, leaves: filled in, not used by generation
   ;; algorithm itself, but needed for chart display etc
   lex-used ; indices to sets of generator lexical alternatives used so far
   )


(defvar *gen-chart* nil)
(defvar *gen-record* nil)
(defvar *gen-chart-unifs* 0)
(defvar *gen-chart-fails* 0)
(defvar *gen-filter-debug* nil)


(defparameter *semantics-index-path* '(synsem local cont index)
   "path used by generator to index chart")


(proclaim
   '(special *edge-id* *safe-not-to-copy-p* *orth-path* *toptype*
       *start-symbol* *debugging*
       mrs::*initial-semantics-path* mrs::*psoa-liszt-path*))


;;; utility functions

(defun gen-make-list-and-insert (len item index)
   ;; make list of length len and insert item at index (1-based)
   (let ((lst (make-list len)))
      (setf (nth (1- index) lst) item) lst))

(defun gen-copy-list-and-insert (lst item)
   ;; replace first nil with item in top-level copy of lst
   (substitute item nil lst :count 1))

(defun gen-chart-intersection-p (lst1 lst2)
   (member-if #'(lambda (x) (member x lst1 :test #'eq)) lst2))


;;;

(defun generate-from-mrs (input-sem)
   (let*
      ((found-lex-list
          (apply #'append (mrs::collect-lex-entries-from-mrs input-sem)))
       (filtered
          (remove 'THAT_C found-lex-list :key #'mrs::found-lex-lex-id))
       (empty
          (mapcar
            #'(lambda (x)
                (mrs::make-found-lex
                   :lex-id x
                   :inst-fs (lex-or-psort-full-fs (get-psort-entry x))))
            (remove "CONJ" mrs::*empty-semantics-lexical-entries* :key #'string
               :test #'search))))
      (if filtered
         (chart-generate input-sem (append filtered empty))
         (format t "~%No lexical entries could be found from MRS relations - have ~
                    functions index-lexicon and index-lexical-rules been run yet?"))))


;;; generate from an input MRS and a sequence of bags of lexical entry FSs
;;;
;;; constraints on generation:
;;;
;;; when completing an active edge ensure that all relations containing
;;; indices internal to the rule have been realised (ensures only
;;; maximal alternative phrase containing modifiers is available for
;;; incorporation into larger phrases)
;;;
;;; extending an active edge with an inactive must not result in the use
;;; of more than one entry derived from the same original rel (prevents e.g.
;;; same modifier being added repeatedly)
;;;
;;; final analysis must not have any semantics missing (e.g. makes sure
;;; all relevant modifiers have been realised)
;;;
;;; returns values: realisations, time taken (msecs), number of unification
;;; attempts, number of unification failures, number of active edges, number
;;; of inactive, total number of chart edges

(defun chart-generate (input-sem found-lex-items)
   (setq *edge-id* 0)
   (setq *gen-chart* nil)
   (setq *gen-record* nil)
   (let ((*safe-not-to-copy-p* t)
         (*gen-chart-unifs* 0) (*gen-chart-fails* 0)
         (start-time (get-internal-run-time)))
      #+powerpc(setq aa 0 bb 0 cc 0 dd 0 ee 0 ff 0 gg 0 hh 0 ii 0 jj 0)
      (dolist (found found-lex-items)
         (let* ((lex-entry-fs (mrs::found-lex-inst-fs found))
                (word (extract-orth-from-fs lex-entry-fs)))
            (gen-chart-add-inactive
               (make-gen-chart-edge
                  :id (next-edge) :rule-number word
                  :dag lex-entry-fs
                  :needed nil
                  :lex-used
                  (append (mrs::found-lex-main-rels found)
                          (mrs::found-lex-alternative-rels found)
                          (mrs::found-lex-message-rels found))
                  :children nil :leaves (list word))
              input-sem)))
      (setq *gen-record*
         (gen-chart-find-complete-edges input-sem))
      (let* ((total-time
                (truncate (* 1000 (- (get-internal-run-time) start-time))
                   internal-time-units-per-second))
             (inact-tot
                (length (gen-chart-retrieve-with-index *toptype* nil)))
             (act-tot
                (length (gen-chart-retrieve-with-index *toptype* t))))
         (values
            (mapcar #'gen-chart-edge-leaves *gen-record*)
            total-time *gen-chart-unifs* *gen-chart-fails*
            act-tot inact-tot (+ act-tot inact-tot)))))


;;; Finish off syntactically complete analyses, and return those that
;;; cover all of input semantics 

(defun gen-chart-find-complete-edges (input-sem)
   ;; from edges that use an entry from each of input lex-entry-alts, try to
   ;; derive all possible inactive edges with 'root' rule - from these retain
   ;; ones which have correct top category, and don't have any semantics missing.
   ;; Finally add these edges to chart, and return their realisations (leaves)
   ;; !!! assume that final orthography is purely the concatenation of the
   ;; orthographies of the lex entries passed in
   (let ((res nil))
      (dolist (e (gen-chart-retrieve-with-index *toptype* nil))
         ;; process has so far checked that we have not generated any relation more
         ;; than once - now check that we have generated them all
         (when (eql (length (gen-chart-edge-lex-used e))
                    (1- (length (mrs::psoa-liszt input-sem)))) ; *** prpstn_rel
            (dolist
               (new
                  (gen-chart-root-edges e
                     (if (listp *start-symbol*) *start-symbol*
                        (list *start-symbol*))))
               (when (gen-chart-check-complete new input-sem)
                  (gen-chart-add-with-index new
                     (gen-chart-dag-index (tdfs-indef (gen-chart-edge-dag new))))
                  (push new res)))))
      (nreverse res)))


(defun gen-chart-check-complete (edge input-sem)
   (let ((sem-fs
          (existing-dag-at-end-of (tdfs-indef (gen-chart-edge-dag edge))
             mrs::*initial-semantics-path*)))
      (if (mrs::is-valid-fs sem-fs)
         (let ((mrs (mrs::construct-mrs sem-fs)))
            ;; (mrs::output-mrs mrs 'mrs::simple)
            (and ; *** how do you check mrs for equality properly? (handel values
                 ; may be variously fully instantiated or not, or have different
                 ; names but refer to same relation if instantiated)
                 ; this is just a crude top-level check
                (equal (mrs::var-name (mrs::psoa-handel mrs))
                   (mrs::var-name (mrs::psoa-handel input-sem)))
                (equal (mrs::var-name (mrs::psoa-index mrs))
                   (mrs::var-name (mrs::psoa-index input-sem)))
                (eql (length (mrs::psoa-liszt mrs))
                   (length (mrs::psoa-liszt input-sem))))))))


(defun gen-chart-root-edges (edge start-symbols)
   ;; c.f. create-new-root-edges in parse.lsp
   (for start-symbol in start-symbols        
       filter
       (let ((tdfs (get-tdfs-given-id 
                    start-symbol)))
         (if tdfs
             (let ((unif 
                    (yadu tdfs (gen-chart-edge-dag edge))))
               (if unif
                   (let ((new-edge
                          (make-gen-chart-edge :dag unif
                                     :id (next-edge)
                                     :rule-number 'root
                                     :children (list edge)
                                     :leaves (gen-chart-edge-leaves edge))))
                      new-edge)))))))


;;; Chart indexing - on *semantics-index-path* values. May be full types or
;;; instance types. Seem to be mostly disjoint if not eq, so don't bother using
;;; a tree-like representation

(defun gen-chart-dag-index (dag)
   (let ((index-dag (existing-dag-at-end-of dag *semantics-index-path*)))
      (if index-dag
         (let ((index (dag-type index-dag)))
            (when (and (consp index) (null (cdr index)))
               ;; simplify single-item disjunction
               (setq index (car index)))
            index)
         (progn
            (cerror (format nil "use type ~A" *toptype*)
               "unexpectedly missing index for edge dag: ~S" dag)
            *toptype*))))


(defun gen-chart-add-with-index (edge index)
   ;; apart from these fns, show-gen-chart is only fn that needs to know the
   ;; internal representation of chart
   (let ((entry (assoc index *gen-chart* :test #'equal))) ; index type could be string
      (unless entry
         (push (setq entry (cons index nil)) *gen-chart*))
      (push edge (cdr entry))))


(defun gen-chart-retrieve-with-index (index activep)
   ;; return all active/inactive edges in chart keyed by a type compatible with
   ;; index
   (let ((res nil))
      (dolist (entry *gen-chart* res)
         (when (find-gcsubtype (car entry) index)
            (dolist (e (cdr entry))
               (when (eq activep (not (null (gen-chart-edge-needed e))))
                  (push e res)))))))


;;; Daughter features of rule, in order that they should be instantiated.
;;; !!! filling of children and leaves fields assumes that features are integers
;;; numbered from 1 as leftmost daughter

(defun gen-chart-rule-ordered-daughters (rule)
   (values (rule-daughters-apply-order rule)
      (position (car (rule-daughters-apply-order rule)) (cdr (rule-order rule)))))


;;; Core control functions. Processing inactive and active edges

(defun gen-chart-add-inactive (edge input-sem)
   (let
      ((index
          (gen-chart-dag-index (tdfs-indef (gen-chart-edge-dag edge)))))
      (gen-chart-add-with-index edge index)
      (setf (gen-chart-edge-dag-restricted edge)
         (restrict-fs (tdfs-indef (gen-chart-edge-dag edge))))
      ;; see if this new inactive edge can extend any existing active edges
      (dolist (e (gen-chart-retrieve-with-index index t))
         (gen-chart-test-active edge e input-sem))
      ;; see if we can create new active edges by instantiating the head
      ;; daughter of a rule
      (dolist (rule (get-indexed-rules (gen-chart-edge-dag edge) #'spelling-change-rule-p))
         (when *debugging*
            (format t "~&Trying to create new active edge from rule ~A and inactive edge ~A"
               (rule-id rule) (gen-chart-edge-id edge)))
         (multiple-value-bind (gen-daughter-order head-index) ; zero-based on daughters
               (gen-chart-rule-ordered-daughters rule)
            (let
               ((unified-dag
                  (gen-chart-try-unification (rule-full-fs rule)
                     (first gen-daughter-order) ; pick out head daughter
                     (nth head-index (rule-daughters-restricted rule)) ; head restrictor
                     (gen-chart-edge-dag edge)
                     (gen-chart-edge-dag-restricted edge)
                     (not (rest gen-daughter-order))
                     (first (rule-order rule)))))
               (when unified-dag
                  (gen-chart-extend-active
                     (make-gen-chart-edge
                        :id (next-edge) :rule-number (rule-id rule)
                        :dag unified-dag
                        :res (first (rule-order rule)) ; path of mother in fs
                        :needed (rest gen-daughter-order)
                        :lex-used (gen-chart-edge-lex-used edge)
                        :children
                        (cond
                           ((null (cdr gen-daughter-order)) (list edge))
                           ((eql head-index 0) (list edge nil))
                           (t (list nil edge)))
                        :leaves
                        (gen-make-list-and-insert (length gen-daughter-order)
                           (gen-chart-edge-leaves edge) (1+ head-index)))
                     input-sem)))))))


(defun gen-chart-test-active (inact act input-sem)
   ;; can extend active edge with inactive? First check to make sure new edge
   ;; would not generate any relation more than once
   (when (not (gen-chart-intersection-p (gen-chart-edge-lex-used act)
                 (gen-chart-edge-lex-used inact)))
      (when *debugging*
         (format t "~&Trying to extend active edge ~A with inactive edge ~A"
            (gen-chart-edge-id act) (gen-chart-edge-id inact)))
      (let ((unified-dag
               (gen-chart-try-unification (gen-chart-edge-dag act)
                  (first (gen-chart-edge-needed act))
                  nil
                  (gen-chart-edge-dag inact)
                  (gen-chart-edge-dag-restricted inact)
                  (not (rest (gen-chart-edge-needed act)))
                  (gen-chart-edge-res act))))
         (when unified-dag
            ;; remaining non-head daughters in active edge are filled in
            ;; left-to-right order
            (let ((new-act
                     (make-gen-chart-edge
                        :id (next-edge) 
                        :rule-number (gen-chart-edge-rule-number act)
                        :dag unified-dag
                        :res (gen-chart-edge-res act)
                        :needed (rest (gen-chart-edge-needed act))
                        :lex-used
                        (append (gen-chart-edge-lex-used act)
                           (gen-chart-edge-lex-used inact))
                        :children
                        (let ((combined ; *** not very clean, really need an index
                               (if (car (gen-chart-edge-children act))
                                  (list act inact) (list inact act))))
                           (if (rest (gen-chart-edge-needed act))
                              ;; empty branch always on right, though if head was 3rd
                              ;; or subsequent daughter there will also/instead be
                              ;; interspersed unfilled arg slots
                              (nconc combined (list nil)) combined))
                        :leaves
                        (gen-copy-list-and-insert (gen-chart-edge-leaves act)
                           (gen-chart-edge-leaves inact)))))
               (gen-chart-extend-active new-act input-sem))))))


(defun gen-chart-extend-active (act input-sem)
   ;; (show-gen-chart)
   (if (gen-chart-edge-needed act)
      ;; add newly extended active edge to chart, then
      ;; look for any existing inactive edges which can extend it
      (let*
         ((daughter-path
             (if (listp (first (gen-chart-edge-needed act)))
                (first (gen-chart-edge-needed act))
                (list (first (gen-chart-edge-needed act)))))
          (index
             (gen-chart-dag-index
                (existing-dag-at-end-of (tdfs-indef (gen-chart-edge-dag act))
                   daughter-path)))) 
         (gen-chart-add-with-index act index)
         (dolist (e (gen-chart-retrieve-with-index index nil))
            (gen-chart-test-active e act input-sem)))
      ;; have ended up completing an active edge - forming a complete constituent
      ;; so carry on processing it as an inactive edge
      ;; Check first that we have by now generated all relations from input
      ;; spec that refer to any indices that are internal to the constituent
      (let
         ((dag (gen-chart-edge-dag act)))
         (unless (gen-chart-inaccessible-needed-p
                    dag input-sem (gen-chart-edge-lex-used act)
                    (gen-chart-edge-leaves act))
            (setf (gen-chart-edge-dag act) dag)
            (setf (gen-chart-edge-res act) nil)
            (setf (gen-chart-edge-leaves act)
               (apply #'append (gen-chart-edge-leaves act)))
            (gen-chart-add-inactive act input-sem)))))


;;; In relations in input-sem that haven't yet been generated, are there any
;;; instance types which are internal to the dag - i.e. present in dag but are
;;; (i.e. have become) inaccessible?
;;; Present but inaccessible means: instance type is in dag semantics but not
;;; in *semantics-index-path* or rest of dag. (The only access we allow to
;;; semantics in rules is to *semantics-index-path*)

(defun gen-chart-inaccessible-needed-p (dag input-sem lex-used leaves)
   (let
      ((sem-fs
        (or (existing-dag-at-end-of (tdfs-indef dag)
               (append mrs::*initial-semantics-path* mrs::*psoa-liszt-path*)) ; RLISZT ***
            (existing-dag-at-end-of (tdfs-indef dag)
               (append mrs::*initial-semantics-path* '(LISZT LIST)))))) ; when not root ***
      (unless sem-fs
         (error "could not find semantics in ~A" 'gen-chart-inaccessible-needed-p))
      (let
         ((present-inaccessible-lvs
              (gen-chart-present-inaccessible-variables-in (tdfs-indef dag) sem-fs))
          (not-yet-generated-lvs nil))
         (dolist (rel (mrs::psoa-liszt input-sem))
            (unless (member rel lex-used :test #'eq)
               (dolist (fvpair (mrs::rel-flist rel))
                  (when (mrs::var-p (mrs::fvpair-value fvpair))
                     (pushnew
                        ;; lv in portion of input-sem that hasn't been generated yet
                        (mrs::make-instance-type (mrs::fvpair-value fvpair))
                        not-yet-generated-lvs)))))
         ;(print sem-fs)
         ;(print (list lex-used present-inaccessible-lvs not-yet-generated-lvs))
         (let ((to-be-generated-but-inaccessible-p
                  (gen-chart-intersection-p
                     present-inaccessible-lvs not-yet-generated-lvs)))
            (when (and to-be-generated-but-inaccessible-p *gen-filter-debug*)
               (format t "~&Filtered out candidate inactive edge covering ~A ~
                          with IVs ~(~A~) in semantics that are no longer reachable, ~
                          but which has still to generate relations containing ~(~A~)"
                  leaves present-inaccessible-lvs not-yet-generated-lvs))
            to-be-generated-but-inaccessible-p))))


(defun gen-chart-present-inaccessible-variables-in (dag sub-dag)
   ;; return set of instance types that are in sub-dag (semantic part in call)
   ;; but do not occur anywhere in remainder of dag U *semantics-index-path*
   (set-difference
      (gen-chart-instance-variables-in sub-dag nil nil)
      (gen-chart-instance-variables-in dag sub-dag
         (list
            (let ((type
                    (type-of-fs
                       (existing-dag-at-end-of dag *semantics-index-path*))))
               (if (consp type) (car type) type))))))


(defun gen-chart-instance-variables-in (dag exclude res)
   ;; iterate over all features present, necessarily using low-level access
   ;; functions. Don't go into exclude dag
   (invalidate-visit-marks)
   (when exclude
      ;; mark exclude dag as already visited
      (setf (dag-visit exclude) :visited))
   (gen-chart-instance-variables-in1 dag res))

(defun gen-chart-instance-variables-in1 (dag res)
   ;; mark dags already processed and don't re-enter them
   (cond
      ((eq (dag-visit dag) :visited))
      ((is-atomic dag)
         (dolist (type (type-of-fs dag))
            (when (instance-type-parent type) (pushnew type res))))
      (t
         (when (instance-type-parent (type-of-fs dag)) ; instance type could be non-atomic
            (pushnew (type-of-fs dag) res))
         (setf (dag-visit dag) :visited)
         (dolist (arc (dag-arcs dag))
            (setq res
               (gen-chart-instance-variables-in1 (dag-arc-value arc)
                  res)))))
   res)


;;; Unification routines, entered only through gen-chart-try-unification

(defun gen-chart-try-unification (rule-tdfs daughter-index daughter-restricted
      fs fs-restricted completedp mother-path)
   ;; attempt to apply a grammar rule - c.f. apply-immediate-grammar-rule
   ;; in parse.lsp
   (when
      (if daughter-restricted
         (restrictors-compatible-p daughter-restricted fs-restricted)
         (x-restrict-and-compatible-p
            (if (listp daughter-index)
               (x-existing-dag-at-end-of (tdfs-indef rule-tdfs) daughter-index)
               (x-get-dag-value (tdfs-indef rule-tdfs) daughter-index))
            fs-restricted))
      (gen-chart-evaluate-unification rule-tdfs daughter-index fs completedp
         mother-path)))


(defun gen-chart-evaluate-unification (rule-tdfs daughter-index fs completedp
      mother-path)
   ;; c.f. evaluate-unifications in parse.lsp
   ;; 
   ;; No orthography done here - it was done during the production of the initial
   ;; set of candidate lexical entries 
   ;;
   ;; unify path <daughter-index> of rule-tdfs with fs, then if completedp is true
   ;; return mother portion of rule-tdfs
   (with-unification-context (ignore)
      (incf *gen-chart-unifs*)
      (unless
         (yadu-features daughter-index rule-tdfs nil fs)
         (incf *gen-chart-fails*)
         (return-from gen-chart-evaluate-unification nil))
      (if completedp
         ;; delete arcs just holding constituents' feature structures - before copying
         ;; otherwise their copies would be thrown away immediately
         ;; we have to check whether any of the deleted dags contain a cycle -
         ;; if so then the whole rule application should fail
         (let*
            ((result (tdfs-at-end-of mother-path rule-tdfs))
             (real-dag (deref-dag (tdfs-indef result)))
             (new (clone-dag real-dag))
             (arcs-to-check nil))
            (flet ((member-with-cyclic-check (arc)
                       (when (member (dag-arc-attribute arc) *deleted-daughter-features*)
                          (push arc arcs-to-check)
                          t)))
               (setf (dag-arcs new)
                  (remove-if #'member-with-cyclic-check (dag-arcs new)))
               (setf (dag-comp-arcs new)
                  (remove-if #'member-with-cyclic-check (dag-comp-arcs new)))
               ;; take advantage of the fact that removed arcs might share structure
               ;; by checking them all at once
               (if (cyclic-dag-p (make-dag :type *toptype* :arcs arcs-to-check))
                  nil
                  (progn
                     ;; (setf (dag-copy new) 'copy)
                     (setf (dag-forward real-dag) new)
                     (copy-tdfs-elements result)))))
         (copy-tdfs-elements rule-tdfs))))



;;; Print out contents of generator chart (tty output) - (print-gen-chart)

(defun print-gen-chart ()
   (format t "~&------~%")
   (dolist (entry *gen-chart*)
      (format t "Vertex ~(~A~):~%" (car entry))
      (dolist (e (cdr entry))
         (format t "[~A] ~A~A ~30,5T=> (~{~:A~^ ~})  [~{~A~^ ~}]~%"
            (gen-chart-edge-id e)
            (gen-chart-edge-rule-number e)
            (if (gen-chart-edge-needed e)
               (format nil " / ~{~A~^ ~}" (gen-chart-edge-needed e))
               "")
            (gen-chart-edge-leaves e)
            (mapcan
               #'(lambda (x) (if x (list (gen-chart-edge-id x))))
               (gen-chart-edge-children e)))))
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
