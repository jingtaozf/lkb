;;; Copyright (c) 1998--2002
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.


(in-package :lkb)


;;; structure definitions are in parse.lsp since also used in interface

(defvar *gen-chart* nil)
(defvar *gen-record* nil)
(defvar *gen-rel-indexes* nil)
(defvar *non-intersective-rules* nil)
(defvar *lexemes-allowed-orderings* nil)

(defparameter *generator-input* nil)
(defparameter *gen-adjunction-debug* nil)
(defparameter *gen-equality-debug* nil)

(defvar *bypass-equality-check* nil)
(defvar *gen-packing-p* nil)

(defparameter %generator-lexical-items% nil)
(defparameter %generator-statistics% nil)


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

(defun gen-chart-set-disjoint-p (r1 r2)
   (not (logtest r1 r2)))

(defmacro gen-chart-set-equal-p (r1 r2)
   `(= ,r1 ,r2))

(defun gen-chart-subset-p (r1 r2)
   ;; is r1 a strict subset or equal to r2
   (= (logior r1 r2) r2))

(defun gen-chart-set-difference (r1 r2)
   (logand r1 (lognot r2)))

(defun gen-chart-set-union (r1 r2)
   (if r1 (logior r1 r2) r2))

(defun gen-chart-set-non-empty-p (r)
   (not (zerop r)))


(defun gen-chart-ordering-allowed-p (left right)
  ;; ordering will already have been checked within both left and right
  (flet ((lexeme-set-intersection-p (lst1 lst2)
            (dolist (x1 lst1 nil) (when (member x1 lst2 :test #'eq) (return t)))))
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


;;; Interface to generator - take an input MRS, ask for instantiated lexical
;;; items, instantiated and uninstantiated rules that might be applicable,
;;; and partial ordering on lexical items, and then call generator
;;; proper. Clear chart and analyses record before entry in case we don't make
;;; it into generation proper. Do it also in chart-generate since that is also
;;; an entry point

(defun generate-from-mrs (input-sem)
   (setf *generator-input* input-sem)
   (with-package (:lkb)
      (clear-gen-chart)
      (setf *cached-category-abbs* nil)
      (let (lex-results lex-items grules lex-orderings tgc tcpu conses symbols others
            (input-rels 0))
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
#|
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
                       (tdfs-indef (mrs::found-rule-full-fs grule)) *semantics-index-path*)
                    nil)
                 #-:gen-index *toptype*
                 (mapcar #'mrs::rel-pred (mrs::found-rule-main-rels grule)))))
|#
         (let ((rel-indexes nil) (rel-indexes-n -1))
            (dolist (lex lex-items)
               (dolist (rel (mrs::found-lex-main-rels lex))
                  (unless (getf rel-indexes rel)
                     (setf (getf rel-indexes rel) (incf rel-indexes-n)))))
            (dolist (grule grules)
               (when (mrs::found-rule-p grule)
                  (dolist (rel (mrs::found-rule-main-rels grule))
                     (unless (getf rel-indexes rel)
                        (setf (getf rel-indexes rel) (incf rel-indexes-n))))))
            (dolist (rel (mrs::psoa-liszt input-sem))
               (unless (getf rel-indexes rel)
                  (setf (getf rel-indexes rel) (incf rel-indexes-n))))
            (dolist (lex lex-items)
               (let ((rel-list (mrs::found-lex-main-rels lex)))
                  (setf (mrs::found-lex-main-rels lex) 0)
                  (dolist (rel rel-list)
                     (setf (mrs::found-lex-main-rels lex)
                        (logior (mrs::found-lex-main-rels lex) (ash 1 (getf rel-indexes rel)))))))
            (dolist (grule grules)
               (when (mrs::found-rule-p grule)
                  (let ((rel-list (mrs::found-rule-main-rels grule)))
                     (setf (mrs::found-rule-main-rels grule) 0)
                     (dolist (rel rel-list)
                        (setf (mrs::found-rule-main-rels grule)
                           (logior (mrs::found-rule-main-rels grule) (ash 1 (getf rel-indexes rel))))))))
            (dolist (rel (mrs::psoa-liszt input-sem))
               (setq input-rels
                  (logior input-rels (ash 1 (getf rel-indexes rel)))))
            (if lex-items
               (chart-generate
                  input-sem input-rels lex-items grules lex-orderings rel-indexes)
               (progn
                  (format t "~%Some lexical entries could not be found from MRS ~
                             relations - has function index-for-generator been run ~
                             yet?")
                  nil))))))


(defun filter-generator-lexical-items (lex-items grules lex-orderings)
   ;; (values lex-items grules lex-orderings))
   (values
      (remove-if
          #'(lambda (x) 
              (or #-ignore (search "_CX" (string x)) ; contracted forms
                  (member x *duplicate-lex-ids* :test #'eq))) ; e.g. a -> an
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
                       &optional (*gen-first-only-p* *gen-first-only-p*))
  ;;(when (> (length found-lex-items) 80)
  ;;   (format t "~%More than 80 initial lexical items - skipping")
  ;;   (return-from chart-generate nil))
  (setq %generator-lexical-items% found-lex-items)
  (unless *intersective-rule-names*
    (format t "~%Warning: no intersective rules defined") (force-output t))
  (let ((*safe-not-to-copy-p* t)
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
    #+(and mcl powerpc) (setq aa 0 bb 0 cc 0 dd 0 ee 0 ff 0 gg 0 hh 0 ii 0 jj 0)
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
                         (incf *copies*) ; each lex entry will be a copy
                         (with-agenda (when *gen-first-only-p* 
                                         (gen-lex-priority lex-entry-fs))
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
                  (mapcan
                     #'(lambda (e)
                         (delete-if-not
                            #'(lambda (u)
                               (and (gen-chart-check-covering u input-rels) ; redundant for consistent
                                    (gen-chart-check-compatible u input-sem)))
                            (if (or *gen-packing-p* (and *intersective-rule-names* partial))
                               ;; may need to un-adjoin even when packing is off
                               (unpack-edge! nil e)
                               (list e))))
                     (nconc consistent
                        (if (and *intersective-rule-names* partial)
                           (gen-chart-adjoin-modifiers partial input-rels
                              possible-grules)))))))
         #'(lambda (tgcu tgcs tu ts tr scons ssym sother &rest ignore)
             (declare (ignore tr ignore))
             (setq tgc (+ tgcu tgcs) tcpu (+ tu ts)
                   conses (* scons 8) symbols (* ssym 24) others sother)))
      (setq %generator-statistics%
         (nconc %generator-statistics%
            (pairlis '(:atgc :atcpu :aconses :asymbols :aothers)
                      (list tgc tcpu conses symbols others))))

      (values
         (extract-strings-from-gen-record) ; also spelling e.g. "a apple" -> "an apple"
         *filtered-tasks* *executed-tasks* *successful-tasks*
         *unifications* *copies*
         (length (gen-chart-retrieve-with-index *toptype* 'active))
         (length (gen-chart-retrieve-with-index *toptype* 'inactive))))))


(defun extract-strings-from-gen-record nil
   (loop for edge in *gen-record*
      collect
      (fix-spelling ; in spell.lsp
         (g-edge-leaves edge))))


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
   (or *bypass-equality-check*
      (let ((sem-fs
               (existing-dag-at-end-of (tdfs-indef (g-edge-dag edge))
                   mrs::*initial-semantics-path*)))
        (and sem-fs (dag-p sem-fs)
          (let ((mrs (mrs::construct-mrs sem-fs nil)))
;;            (when *debugging*
;;              (display-fs sem-fs "semstructure"))
;;            (when *sem-debugging*
;;              (mrs::output-mrs input-sem 'mrs::simple)
;;              (mrs::output-mrs mrs 'mrs::simple))  
             (mrs::mrs-equalp mrs input-sem nil *debugging*))))))


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
                      (let ((new-edge
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
                                          :lexemes (g-edge-lexemes edge))))
                        (gen-chart-add-with-index new-edge)
                        (list new-edge)))))))))


(defun gen-filter-root-edges (edges start-symbols)
   ;; c.f. filter-root-edges in parse.lsp
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
   (if index-dag
      (unify-get-type index-dag) ; may be called inside unif context
      (progn
         ;;(cerror (format nil "use type ~A" *toptype*)
         ;;   "unexpectedly missing index for edge ~A: ~S" edge-id dag)
         (warn "unexpectedly missing index for edge ~A - using ~A" edge-id *toptype*)
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
  (let ((index (gen-chart-add-with-index edge)))
    ;; no index, so not entered into the chart
    (unless index (return-from gen-chart-add-inactive nil))
    ;; did we just find a result?
    (when *gen-first-only-p*
      (let ((complete
              (delete-if-not
                 #'(lambda (u)
                     (and (gen-chart-check-covering u input-rels)
                          (gen-chart-check-compatible u input-sem)))
                 (if *gen-packing-p* (unpack-edge! nil edge) (list edge)))))
        (when complete
           (setq *gen-record* complete)
           (throw 'first t))))
    ;; see if this new inactive edge can extend any existing active edges
    (mapc
       #'(lambda (act)
           (with-agenda (when *gen-first-only-p* 
                          (gen-rule-priority (edge-rule act)))
             (gen-chart-test-active edge act input-sem input-rels)))
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
                  :lexemes (g-edge-lexemes edge))
               chart-index)))))


(defun gen-chart-test-active (inact act input-sem input-rels &optional one-off-p)
  ;; can extend active edge with inactive? First check to make sure new edge
  ;; would not use any relation from initial lexical items more than once.
  ;; Assumption here that rels are eq between alternative lexical items
  (when (and
           (or one-off-p ; intersection guaranteed to be OK if one-off-p
               ;; should probably do the rule compatibility test before this
               ;; since it's cheaper - alternatively might help
               ;; longer sentences to change intersection test on lists to
               ;; and-ing of bit vectors since symbolic values not used
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
                     :mod-index (g-edge-mod-index act))))
               (if one-off-p new-act
                 (gen-chart-extend-active new-act input-sem input-rels index-dag))))))))


(defun gen-chart-extend-active (act input-sem input-rels act-chart-index)
   (if (g-edge-needed act)
      ;; add newly extended active edge to chart, then look for any existing
      ;; inactive edges which can extend it
      (let ((index (gen-chart-add-with-index act act-chart-index)))
         (dolist (e (gen-chart-retrieve-with-index index 'inactive))
            (gen-chart-test-active e act input-sem input-rels)))
      ;; have ended up completing an active edge - forming a complete constituent
      (gen-chart-add-inactive
         (gen-chart-finish-active act) input-sem input-rels)))


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
;;;
;;; find missing rels and look for sets of potential modifier edges that cover
;;; them, then try inserting modifier edges into partial edge and recompute
;;; nodes higher up in tree
;;;
;;; all edges created from now on have index *toptype* since we're not trying
;;; to retrieve them from the chart

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
            *intersective-rule-names*)))
      (when *gen-adjunction-debug*
         (format t "~%Intersective inactive edges: ~:A" 
            (mapcar #'g-edge-id intersective-edges)))
      (let
         ((mod-candidate-edges
            (gen-chart-active-mod-candidate-edges
               intersective-edges possible-grules intersective-rules-and-daughters))
          (partial-extendable nil)
          (id 0))
         (dolist (partial partial-edges)
            ;; adjoin into partial analysis
            ;; reject any mod that overlaps with partial
            ;; (print (list '--- (g-edge-id partial) (g-edge-leaves partial)))
            ;; (print (gen-chart-set-rel-preds (g-edge-rels-covered partial)))
            (let ((non-overlapping
                   (remove-if-not
                      #'(lambda (mod) (gen-chart-set-disjoint-p mod (g-edge-rels-covered partial)))
                      mod-candidate-edges :key #'g-edge-rels-covered))
                  (sub-adjoined nil))
               (when non-overlapping
                  (incf id)
                  ;; adjoin into adjunct list, e.g. to get nested PPs
                  (dolist (int non-overlapping)
                     (setq sub-adjoined
                        (gen-chart-insert-adjunction int
                           (remove-if-not
                              #'(lambda (mod) (gen-chart-set-disjoint-p mod (g-edge-rels-covered int)))
                              non-overlapping :key #'g-edge-rels-covered)
                           id sub-adjoined)))
                  ;; union of mod rels must be equal to missing-rels, otherwise reject partial
                  (let
                     ((missing-rels
                        (gen-chart-set-difference input-rels (g-edge-rels-covered partial))))
                     ;; (print (list missing-rels (gen-chart-set-rel-preds missing-rels)))
                     (when
                        (and
                           (gen-chart-set-equal-p missing-rels
                              (reduce #'gen-chart-set-union non-overlapping
                                 :key #'g-edge-rels-covered))
                           (let ((adjoined
                                   (gen-chart-insert-adjunction partial non-overlapping id sub-adjoined)))
                               (and adjoined
                                  (gen-chart-set-equal-p missing-rels
                                     (reduce #'gen-chart-set-union adjoined :key #'g-edge-rels-covered)))))
                        (when *gen-adjunction-debug*
                           (format t
"~&Partial edge [~A], id ~A, spanning ~:A~%   modifiers ~A" (g-edge-id partial) id
                             (g-edge-leaves partial) (mapcar #'g-edge-id non-overlapping)))
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
       ;; (mapcan #'(lambda (e) (unpack-edge! nil e)) intersective-edges)
       intersective-edges))


;;;

(defun gen-chart-insert-adjunction (edge acts id adjoined)
   (unless (member nil (g-edge-children edge))
      ;; don't try to adjoin into the top of an active edge
      (dolist (act acts)
         (when (gen-chart-try-adjunction act edge)
            (pushnew act adjoined)
            (pushnew act (g-edge-adjuncts edge)))))
   (dolist (c (g-edge-children edge))
      (when c
         ;; don't try to adjoin into the needed daughter of an active edge
         (setq adjoined (gen-chart-insert-adjunction c acts id adjoined))))
   (dolist (p (g-edge-equivalent edge))
      (setq adjoined (gen-chart-insert-adjunction p acts id adjoined)))
   (dolist (p (g-edge-packed edge))
      (setq adjoined (gen-chart-insert-adjunction p acts id adjoined)))
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


;;; Print out contents of generator chart (tty output) - (print-gen-chart)

(defun print-gen-chart (&key concise (stream t))
   (flet ((print-edge (e stream concise)
            (format stream "[~A] ~A~A ~30,5T=> (~{~:A~^ ~})  [~{~A~^ ~}]~%"
               (g-edge-id e)
               (if (rule-p (g-edge-rule e)) (rule-id (g-edge-rule e))
                   (if concise (first (edge-lex-ids e)) (g-edge-rule e)))
               (if (g-edge-needed e)
                  (format nil " / ~{~A~^ ~}" (g-edge-needed e))
                  "")
               (g-edge-leaves e)
               (mapcan
                  #'(lambda (x) (if x (list (g-edge-id x))))
                  (g-edge-children e)))))
      (format stream "~&------~%")
      (dolist (entry (reverse *gen-chart*)) ; order in which originally created
         (format stream "~%Vertex ~(~A~):~%" (car entry))
         (dolist (e (sort (append (cadr entry) (copy-list (cddr entry))) #'<
                          :key #'edge-id))
            (print-edge e stream concise)
            (dolist (p (g-edge-equivalent e))
               (format stream " = packed ")
               (print-edge p stream concise)
            (dolist (p (g-edge-packed e))
               (format stream " > packed ")
               (print-edge p stream concise)))))
      (format stream "~%")))


;;; End of file
