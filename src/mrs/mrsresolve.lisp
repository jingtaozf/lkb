;;; Functions for checking MRS stuctures and outputting
;;; the possible scoped structures in a linearised notation

(in-package "MRS")

;;;
;;; ******** Global variables **********
;;;

;;; set in mrsglobals-eng

(defvar *scope-feat* nil)

(defvar *top-level-rel-types* nil)

(defvar *top-level-variables* nil
  "the variables which correspond to pronouns or proper names")

;;;
;;; ******** utility functions (some grammar specific) **********
;;;

;;; memoisation

(defvar *rel-handel-store* nil)
(defvar *conj-val-store* nil)
(defvar *conj-handel-val-store* nil)
(defvar *unbound-vars-store* nil)


(defun clear-scope-memos nil
  (setf *rel-handel-store* nil)
  (setf *conj-val-store* nil)
  (setf *unbound-vars-store* nil)
  (setf *conj-handel-val-store* nil))

;;; Utility functions for taking apart MRS structures.
;;; All the functions which rely on particular names for features or rels,
;;; or which identify variable sorts should be in this section

;;; the values of features may be var structures, lists of var structures 
;;; or constants

(defun proper-name-or-pronoun-rel (rel-sort)
  (member rel-sort *top-level-rel-types*))


(defun get-handel-args (rel)
  ;;; returns a list of ids of any handels which are values
  ;;; of roles in the rel 
  (mapcar #'cdr (get-handel-args-with-features rel)))


(defun get-handel-args-with-features (rel)
  ;;; returns a list of ids of any handels which are values
  ;;; of roles in the rel as a dotted pair, with the role
  ;;; as car
  (let ((existing (assoc rel *rel-handel-store*)))
    (if existing (cdr existing)
        (let ((res
               (for fvp in (rel-flist  rel)
                    append
                    (let ((val (fvpair-value fvp))
                          (feature (fvpair-feature fvp)))
                      (cond ((is-handel-var val)
                             (list (cons feature (var-id val))))
                            ((is-conj-var val)
                             (for val-id in (extract-handels-from-conj val)
                                  collect
                                  (cons feature val-id)))
                            ((listp val) 
                             (for val-el in val
                                  filter
                                  (if (and (var-p val-el) (is-handel-var val-el))
                                      (cons (fvpair-feature fvp) (var-id val-el)))))
                            (t nil))))))
          (push (cons rel res) *rel-handel-store*)
          res))))

(defun extract-handels-from-conj (val)
  ;;; example input
  #|
  #S(MRS::VAR :NAME "c16" 
              :TYPE *DIFF-LIST* 
              :EXTRA (#S(MRS::FVPAIR :FEATURE #S(TYPED-PATH 
                               :TYPED-FEATURE-LIST (#S(TYPE-FEATURE-PAIR :TYPE *DIFF-LIST* :FEATURE LIST))) 
              :VALUE (#S(MRS::HANDLE-VAR :NAME "h11" :TYPE (HANDLE) :EXTRA NIL :ID 11)
              #S(MRS::HANDLE-VAR :NAME "h17" :TYPE (HANDLE) :EXTRA NIL :ID 17)))) :ID 16))
                                                |#
  ;;; returns list of ids of any handel variables
    (let ((existing (assoc val *conj-handel-val-store*)))
      (if existing (cdr existing)
        (let ((res
               (let ((extra (var-extra val)))
                 (if (listp extra)
                     (for ex-el in extra
                          append
                          (if (fvpair-p ex-el)
                              (let ((new-val (fvpair-value ex-el)))
                                (if (listp new-val)
                                    (for var-el in new-val
                                         filter
                                         (if (is-handel-var var-el)
                                             (var-id var-el)))))))))))
          (push (cons val res) *conj-handel-val-store*)
          res))))
  

(defun is-handel-var (var)
    ;;; test is whether the string begins with #\h
  (and (var-p var)
       (eql (elt (var-name var) 0) #\h)))

(defun is-conj-var (var)
    ;;; test is whether the string begins with #\c
  (and (var-p var)
       (eql (elt (var-name var) 0) #\c)))

(defun is-handel-arg-rel (rel)
  ;;; true if the rel has an argument which is a handel
  (get-handel-args-with-features rel))

(defun nonquantified-var-p (var)
  ;;; true if the string identifier for the variable
  ;;; begins with anything other than x
  (and (var-p var)
       (not (eql (elt (var-name var) 0) '#\x))))

   
(defun is-quant-rel (rel)
  ;;; test is presence of a feature called BV!
  ;;; But also avoid including any "quantifiers" which are top-level rels,
  ;;; possibly including "the" (_def_rel) and demonstratives
  (dolist (fvpair (rel-flist  rel))
    (when (and (eql (fvpair-feature fvpair) *bv-feature*)
               (not (member (rel-sort rel) *top-level-rel-types* )))
      (return t))))

(defun get-bv-value (rel)
  ;;; returns the integer value of the variable corresponding to the
  ;;; feature BV
  ;;; assumes that there is only one such feature and 
  ;;; that its value is a var 
  (dolist (fvpair (rel-flist  rel))
    (when (eql (fvpair-feature fvpair) *bv-feature*)
      (return (get-var-num (fvpair-value fvpair))))))

(defvar *temp-q-rel-store* nil)

(defun find-unbound-vars (rels quant-rels)
  ;;; this is just called once, as part of the preprocessing of the
  ;;; MRS structure.
  ;;; It takes a list of rels which are quantifiers (i.e. have a BV feature)
  ;;; and another list of rels, and returns a list of the 
  ;;; variables in that list (as integers) which are not bound by the
  ;;; quant-rels and which do not occur as variables in
  ;;; pronouns or proper name relations
  ;;;
  ;;; As a side effect, pushes variables associated with pronouns or
  ;;; proper names, which are NOT explicitly bound by quantifiers
  ;;; onto *top-level-variables*
  ;;;
  ;;; It also creates a structure which relates quantifiers
  ;;; to qeqs involving relations which contain a variable they bind
  (setf *temp-q-rel-store* nil) ; needed because we haven't got qeqs yet
  (let ((quant-vars nil)
        (var-q-assoc nil))
    (for rel in quant-rels
         do
         (let ((quant-var (get-bv-value rel)))
           (unless quant-var 
             (struggle-on-error 
              "~%Rel ~A has an uninstantiated bound variable"
                    (rel-sort rel)))
           (when (member quant-var quant-vars)
             (struggle-on-error "~%Rel ~A has a duplicate bound variable"
                    (rel-sort rel)))
           (when quant-var
             (push quant-var quant-vars)
             (push (cons quant-var rel) var-q-assoc))))
    (remove-if #'(lambda (value)
                   (or (member (get-var-num value) quant-vars)
                       (member (get-var-num value) *top-level-variables*)))
               (for rel in rels
                    append
                    (let ((rel-vars (collect-vars-from-rel rel)))
                      (unless (member rel quant-rels)
                        (for var in rel-vars
                             do
                             (let ((associated-quantifier (cdr (assoc (get-var-num var) var-q-assoc))))
                               (when associated-quantifier
                                 (push (cons associated-quantifier rel)
                                       *temp-q-rel-store*)))))
                      (when (proper-name-or-pronoun-rel (rel-sort rel))
                        (for var in rel-vars
                             do
                             (unless (member (get-var-num var) quant-vars)
                               (pushnew (get-var-num var) *top-level-variables*))))
                       rel-vars)))))


(defun collect-vars-from-rel (rel)
  ;;; only called from find-unbound-vars 
  ;;;
  ;;; returns the variables from a relation (other than the handel)
  ;;; note that this returns the entire structures (necessary because
  ;;; of identification of sorts of variables)
  (for fvp in (rel-flist  rel)
       append 
       (let ((value (fvpair-value fvp)))
         (if (var-p value) 
             (cons value (extract-variables-from-conj value))
           (if (and (listp value) (var-p (car value)))
             value)))))

(defun collect-unbound-vars-from-rel (rel)
  ;;; collects all the variables from a rel which have to be bound
  ;;; off by some other rel: i.e. variables other than events,
  ;;; handels, and unspecified arguments
  ;;; which are not the values of a BV feature
  ;;; proper name  and pronoun rels act as implicit quantifiers themselves
  ;;; so that variables which are introduced by such rels are 
  ;;; calculated initially and stored in the global variable
  ;;; *top-level-variables*
  (let ((existing (assoc rel *unbound-vars-store*)))
    (if existing (cdr existing)
      (let ((res
             (for fvp in (rel-flist  rel)
                  append 
                  (unless (eql (fvpair-feature fvp) *bv-feature*)
                    (let ((value (fvpair-value fvp)))
                      (for el in (if (listp value) value (list value))
                           append
                           (let ((unbound-id (unbound-var-id el))
                                 (conjids
                                  (for conj-var in (extract-variables-from-conj value)
                                       filter
                                       (unbound-var-id conj-var))))
                             (if unbound-id (cons unbound-id conjids)
                               conjids))))))))
        (push (cons rel res) *unbound-vars-store*)
        res))))

(defun unbound-var-id (el)
  (if (and (var-p el)
           (not (nonquantified-var-p el))
           (not (member (get-var-num el) 
                        *top-level-variables*)))
      (get-var-num el)))


(defun extract-variables-from-conj (val)
  (if (is-conj-var val)
      (let ((existing (assoc val *conj-val-store*)))
        (if existing (cdr existing)
          (let ((res 
                 (let ((extra (var-extra val)))
                   (if (listp extra)
                       (for ex-el in extra
                            append
                            (if (fvpair-p ex-el)
                                (let ((new-val (fvpair-value ex-el)))
                                  (if (listp new-val) new-val))))))))
            (push (cons val res) *conj-val-store*)
            res)))))

;;;
;;; ******** Main code entry point  **********
;;;

;;; (check-mrs-results-file "Macintosh HD:lkb96:mrs:mrs-vit-tests")

(defun check-mrs-results-file (file-name)
  ;;; the file has the same format as that output by the mrs batch checking stuff
  ;;; however, it is necessary to manually remove the package symbols first
  (with-open-file
   (istream file-name :direction :input)
   (do* ((sentence (read istream nil 'eof) (read istream nil 'eof ))
         (mrsstruct (read istream nil 'eof) (read istream nil 'eof)))
        ((or (eql sentence 'eof) (eql mrsstruct 'eof)) nil)
        (when (and mrsstruct (psoa-p mrsstruct))
              (check-mrs-struct mrsstruct sentence)))))

(defvar *canonical-bindings* nil
"global variable which is set to the current set of bindings for the
printing routines -  convenient to make this global to keep printing generic")

(defun check-mrs-struct (mrsstruct &optional sentence)
  ;;; first output the existing structure in the indexed notation
  (clear-scope-memos)
  (setf *canonical-bindings* nil)
  (when sentence
    (format t "~%~A" sentence)
    (format t "~%FSs"))
  (output-mrs mrsstruct 'simple)
  (format t "~%Unscoped form")
  (output-mrs mrsstruct 'indexed)
  ;;; then try and find sets of bindings which will give a fully scoped 
  ;;; structure, and output the results
  (let ((binding-sets (make-scoped-mrs mrsstruct)))
    (if binding-sets
      (format t "~%Scoped form(s)")
      (format t "~%WARNING: Invalid MRS structure"))
    (for binding in binding-sets
          do
          (setf *canonical-bindings* (canonical-bindings binding))
          (output-connected-mrs mrsstruct 'indexed)
          (output-scoped-mrs mrsstruct))))


;;; slight variant on the above for PAGE toplevel

(defun scope-mrs-struct (mrsstruct)
  ;;; first output the existing structure in the indexed notation
  (setf *canonical-bindings* nil)
  (format t "~%Unscoped form")
  (output-mrs mrsstruct 'indexed)
  ;;; then try and find sets of bindings which will give a fully scoped 
  ;;; structure, and output the results
  (let ((binding-sets (make-scoped-mrs mrsstruct)))
    (if binding-sets
      (format t "~%Scoped form(s)")
      (format t "~%WARNING: Invalid MRS structure"))
    (for binding in binding-sets
          do
          (setf *canonical-bindings* (canonical-bindings binding))
          (output-connected-mrs mrsstruct 'indexed)
          (output-scoped-mrs mrsstruct))))

;;; ****** Functions for manipulating bindings *******

;;; bindings are an assoc list of number . equivalence set pairs
;;; So for example, if 1, 3 and 4 are mutually equivalent, but 2 is in a set by itself
;;; the value for bindings should be
;;;
;;; ((1 . (1 3 4))
;;;  (2 . (2))
;;;  (3 . (1 3 4))
;;;  (4 . (1 3 4)))

;;; Note that the stuff which sets up the hcons information (i.e. the functions
;;; in mrscons.lsp) get called from the function which sets up the
;;; initial bindings


(defun generate-top-bindings (handel rels existing-bindings &optional extra)
  ;;; this is called when all the rels should
  ;;; have handels which are equal to eachother, and to handel
  ;;; and to extra, if present
  (let ((new-equality nil))
    (for rel in rels
         do
         (pushnew
          (get-var-num (rel-handel rel))
          new-equality))
    (pushnew handel new-equality)
    (when extra
      (pushnew extra new-equality))
    (for binding in existing-bindings
         ;;; some of this structure is shared, but
         ;;; we never alter any binding destructively
         collect
           (cons (car binding) 
                 (if (member (car binding) new-equality)
                     new-equality
                   (cdr binding))))))

(defun adjust-bindings (bindings hole label)
  (for binding in bindings
       collect
       (cons (car binding) 
             (if (eql (car binding) hole)
                 (cons label (cdr binding))
               (if (eql (car binding) label) 
                   (cons hole (cdr binding))
                 (cdr binding))))))

(defun construct-initial-bindings (top-handel-var rels hcons)
  ;;; finds all the handels and constructs an
  ;;; initial list which just looks like
  ;;; ((1 . (1)) (2 . (2))) etc
  ;;;
  (let ((labels nil) (holes nil) 
        (top-handel (get-var-num top-handel-var)))
    (for rel in rels
         do
         (let ((var (rel-handel rel)))
           (unless (is-handel-var var)
             (struggle-on-error "~%Relation ~A has incorrect handel ~A"
                                (rel-sort rel) var))
           (pushnew (get-var-num var) labels) 
           (for handel-var in (get-handel-args rel)
                do
                (pushnew handel-var holes))))
    (pushnew top-handel holes) 
    (process-hcons hcons labels holes)
    (for handel in (remove-duplicates (append labels holes))
       collect
       (list handel handel))))


(defun get-bindings-for-handel (handel bindings)
  (let ((hb (assoc handel bindings)))
    (unless hb
      (struggle-on-error "Handel ~A not in bindings" handel))
    (if hb
        (cdr hb))))

           

;;; when we've finished, we want an old/new binding alist for printing etc

(defun canonical-bindings (bindings)
  (let ((result nil))
    (for binding in bindings
         do
         (unless (assoc (car binding) result)
           (for equiv in (cdr binding)
                do (push (cons equiv (car binding)) result))))
    result))


;;; ******* back to the main code ...  **********


(defstruct res-struct bindings other-rels)
;;; res-struct is defined because functions need to return both
;;; bindings and a list of unaccounted for rels and
;;; there's too much scope for confusion if I simply cons them

;;; Here's where stuff really starts happening ...
;;; The algorithm is straightforward - we start from the top handel
;;; and a set of rels whose position is unassigned.
;;; We generate all possible combinations of rels
;;; which might have the same handel as the top handel (taking into
;;; account hcons and the constraints imposed by the need to bind off 
;;; variables.  This gives us the first layer of the tree.
;;; Non-terminal nodes in the tree are relations which have handel
;;; arguments - we recurse on each of these

(defvar *scoping-calls* 0)

(defvar *max-scoping-calls* 0)

(defvar *quant-rels* nil)

(defvar *scoping-call-limit* 10000)

(defun make-scoped-mrs (mrsstruct)
  (setf *top-level-variables* nil)
  (let* ((rels (psoa-liszt mrsstruct))
         (hcons (psoa-h-cons mrsstruct))
         (quant-rels (for rel in rels filter (if (is-quant-rel rel) rel)))
         (implicit-existentials (find-unbound-vars rels quant-rels))
; find-unbound-vars has the side effect of pushing variables which
; are in pronoun or name rels into *top-level-variables* 
         (free-variables (for var in implicit-existentials
                               filter
                               (if (not (nonquantified-var-p var))
                                   var))))
    (if free-variables
        (progn
          (unless *giving-demo-p*
            (format t "~%Free variables in MRS: ~A" 
                    (mapcar #'var-name free-variables)))
          nil)
;;; variables must be bound by quantifiers unless they are in relations
;;; which license implicit existential binding
      (let ((top-handel (get-var-num (psoa-top-h mrsstruct))))
        (setf *quant-rels* quant-rels)
        ;;; should pass this around instead of global, when worked out
        ;;; code
        (setf *max-scoping-calls* (max *max-scoping-calls* *scoping-calls*))
        (setf *scoping-calls* 0)
        (catch 'up
          (for result in (create-scoped-structures top-handel nil 
                                 (construct-initial-bindings 
                                  (psoa-handel mrsstruct) 
                                  ; modified - now passes whole var instead of just
                                  ; id
                                  rels hcons)
                                 rels nil nil nil)
               filter
               ;;; the bindings slot of the result is a bindings set, the
               ;;; other-rels is a set of `left over' rels
               ;;; we don't want any of these at the top level
               (unless (res-struct-other-rels result) 
                 (res-struct-bindings result))))))))

(defstruct bindings-and-sisters
  bindings sisters pending-qeq)
;;; similarly, this structure is just for temporary storage of results

#|
qeqs

at the entry point for create-scoped-structures, there may be a 
pending-qeq.  This indicates that we are in a branch where
there has been some ARG with value h1 which is qeq h2, and we 
haven't terminated this with q2 yet.  In this case, we maintain the
pending-qeq while we go down a scope path of a quantifier,
and the possible-relations are just quantifiers or things whose handels
are or could be h2.  If we're looking at the restriction of the quantifier,
the pending-qeq is set based on any qeqs for the restriction.

we have a top-handel (i.e. a current `top-handel' - this function
is called recursively) and a set of rels, some of which may have the
same handel as the top-handel.  We have to generate all possibilities
for combinations of rels which COULD have the same handel
bindings - current bindings
bvs - a list of the variables which are bound by quantifiers we've
      already dealt with (nil on first call)
rels - rels we haven't incorporated yet (all rels on first call)
scoping-handels - handels which will outscope the handels we're
       about to incorporate (nil on first call) - added for outscopes
        constraint (not currently used)
pending-qeq - handel which must get used either this call
or modulo some number of quantifiers

|#


(defun create-scoped-structures (top-handel bvs bindings rels 
                                 scoping-handels pending-qeq scoped-p)
  (incf *scoping-calls*)
  (when (> *scoping-calls* *scoping-call-limit*)
    (unless *giving-demo-p* 
      (format t "~%Maximum scoping calls exceeded"))
    (throw 'up nil))
  (setf pending-qeq
    (let ((new-qeq (find-qeq top-handel bindings)))
      (if scoped-p
          (progn
            (when new-qeq
              (unless *giving-demo-p*
                (format t 
                        "~%Warning, qeq specified for scope handel ~A ignored" 
                        top-handel)))
            pending-qeq)
        new-qeq)))
;; (format t "~%current top ~A bvs ~A pending-qeq ~A scoped-p ~A bindings ~A"
;;          top-handel bvs pending-qeq scoped-p bindings)
  (if rels
    ; fail immediately if we're going to be left with
    ; an unsatisfiable handel arg
    (let* ((handel-args (for rel in rels
                                    append (get-handel-args rel)))
           (impossible-top-rels 
            (calculate-impossible-top-rels 
             top-handel bvs bindings rels 
             scoping-handels pending-qeq  handel-args))
           (impossible-handels (let ((tmp nil))
                                 (for rel in impossible-top-rels
                                      do 
                                      (pushnew 
                                       (get-var-num (rel-handel rel)) tmp))
                                 tmp))
           (possible-top-rels 
            ; the list of rels which wouldn't introduce any free variables
            ; and which don't have sisters (i.e. rels with the same handel)
            ; which introduce free variables
            (for rel in rels
                 filter 
                 (unless (or (member rel impossible-top-rels)
                          (member (get-var-num (rel-handel rel)) 
                                 impossible-handels))
                   rel)))
           (pending-top-rels
            ;; the list of rels which could have the same handel
            ;; as the pending-qeq
            (if pending-qeq
                (for rel in possible-top-rels
                     filter
                     (if (eql (get-var-num (rel-handel rel)) 
                                          pending-qeq)
                         rel))))
           (possible-quant-top-rels 
            (if pending-qeq
             (for rel in possible-top-rels
                 filter 
                 (if (is-quant-rel rel)
                   rel))))
           (top-rels ; the list of rels which have the same handel as the
                     ; top handel
            (for rel in rels
                 filter 
                 (if (eql (get-var-num (rel-handel rel)) 
                          top-handel)
                     rel))))
;      (format t "~%~A" possible-top-rels)
      (if (if pending-qeq 
              (and 
               (not top-rels)
               (or possible-quant-top-rels
                   pending-top-rels))
            ;; if we've got a pending-qeq, there can be no
            ;; known top-rels (maybe iffy?) and there must
            ;; be either possible quantifiers or relations
            ;; otherwise
            (and possible-top-rels 
                 (subsetp top-rels possible-top-rels)))
          ;; unless there are some possible rels and all the top rels
          ;; are possible, we fail
        (for rel-comb-and-bindings in 
             (make-combinations-of-top-rels 
              top-handel top-rels
              (set-difference possible-top-rels top-rels)
              pending-top-rels
              possible-quant-top-rels
              pending-qeq
              bindings)
             ;;; for each possible set of rels which can have their handels
             ;;; bound to the top-handel, generate a set of results
             append
             (sister-structure-scope 
              (car (bindings-and-sisters-sisters rel-comb-and-bindings))
              (cdr (bindings-and-sisters-sisters rel-comb-and-bindings)) 
              bvs 
              (bindings-and-sisters-bindings rel-comb-and-bindings) 
              (set-difference 
               rels 
               (bindings-and-sisters-sisters rel-comb-and-bindings))
              (cons top-handel scoping-handels)
              (bindings-and-sisters-pending-qeq rel-comb-and-bindings)))))))


(defun calculate-impossible-top-rels (top-handel bvs bindings rels 
                                      scoping-handels pending-qeq  handel-args)
 ;;; the list of rels which introduce variables which are
 ;;; currently free or which have a handel which is coindexed with a handel
 ;;; argument of one of the rels
 ;;; or which are `outscoped' by top-handel
 ;;; or which are outscoped by a handel which is not on the list
 ;;; of scoping-handels
 ;;; or which are qeq something which is not the pending-qeq
  (declare (ignore top-handel))
  (for rel in rels
       filter 
       (let ((handel-num (get-var-num (rel-handel rel))))
         (if (or (member handel-num handel-args)
                 ;; (violates-outscopes-p  
                 ;;  handel-num
                 ;;  top-handel scoping-handels bindings)
                 ;; violates-outscopes-p is in mrscons.lsp
                 ;; currently redundant                                        
                 (let ((vars (collect-unbound-vars-from-rel rel)))
                   (and vars (not (subsetp vars bvs))))
                 (not-qeq-p pending-qeq handel-num
                            bindings)
                 (and (is-quant-rel rel)
                     (not (check-quant-qeqs scoping-handels 
                                          pending-qeq rel bindings))))
             rel))))


(defun make-combinations-of-top-rels  (top-handel known-top-rels 
                                       other-possibles pending
                                       possible-quants pending-qeq bindings)
  ;;; If there is no pending-qeq,
  ;;; we need all possible combinations of rels which include the known 
  ;;; top rels, if there are any.  
  ;;; If there is a pending-qeq,
  ;;; we either need all the possible pending
  ;;; or all the combinations of possible-quants
  (if pending-qeq
      (let ((pendingless (set-difference other-possibles pending)))
        (append
         (if pending  
             (new-bindings-and-sisters pendingless (list pending) top-handel bindings pending-qeq nil))
         (if possible-quants
             (new-bindings-and-sisters (set-difference pendingless possible-quants) 
                                       (mapcar #'list possible-quants) 
                                       top-handel bindings nil pending-qeq))))
    (new-bindings-and-sisters other-possibles (list known-top-rels) top-handel bindings nil nil)))


(defun new-bindings-and-sisters (free fixed-list top-handel bindings binding-qeq result-qeq)            
  (let ((other-combinations 
         (generate-combinations free)))
      ;;; other-combinations is a list of sets of rels 
      ;;; including the empty set
    (for fixed in fixed-list
         append
         (for combination in other-combinations
              filter
              (let* ((combined-rels (append fixed combination))
                     (new-bindings 
                      (generate-top-bindings 
                       top-handel combined-rels bindings binding-qeq)))
                (if (and combined-rels new-bindings)
               ;;; combined rels might be nil in the case where there are 
               ;;; no known-top-rels
                    (make-bindings-and-sisters 
                     :sisters combined-rels 
                     :bindings new-bindings :pending-qeq result-qeq)))))))

;;; ******* Traversing the current leaves of the tree ********

(defun sister-structure-scope (first-rel top-rels bvs bindings 
                               other-rels scoping-handels pending-qeq)
  ;;; deals with multiple elements in a putative conjunction
 (if (is-handel-arg-rel first-rel) 
   (let* ((new-bvs (if (is-quant-rel first-rel) 
                     (cons (get-bv-value first-rel) bvs)
                     bvs))
          (handel-args (get-handel-args-with-features first-rel))
          (full-results (sister-args-scope handel-args new-bvs 
                                           bindings other-rels
                                           scoping-handels pending-qeq)))
     (if full-results
       (if top-rels
         (for res in full-results
              append
              (sister-structure-scope (car top-rels) (cdr top-rels)
                                      bvs (res-struct-bindings res) 
                                      (res-struct-other-rels res)
                                      scoping-handels pending-qeq))
         full-results)))
   (if top-rels
     (sister-structure-scope (car top-rels) (cdr top-rels)
                             bvs bindings other-rels scoping-handels
                             pending-qeq)
     (list (make-res-struct :bindings bindings :other-rels other-rels)))))


(defun sister-args-scope (top-handels bvs bindings other-rels scoping-handels
                          pending-qeq)
  ;;; this is necessary to deal with the case of relations which have
  ;;; multiple handle arguments
  (let ((results (create-scoped-structures 
                  (cdar top-handels) bvs
                  bindings other-rels scoping-handels pending-qeq
                  (eql (caar top-handels) *scope-feat*))))
    (if (cdr top-handels)
      (for res in results
              append
              (sister-args-scope (cdr top-handels) bvs
                                      (res-struct-bindings res) 
                                      (res-struct-other-rels res)
                                      scoping-handels pending-qeq))
      results)))

;;; **********  Generating all possible relation combinations **********                                                       

(defun generate-combinations (lst-of-rels)
  ;;; we first group the rels into clusters which share
  ;;; a handel, then generate all combinations of these
  ;;; clusters, then convert each of the results back into a 
  ;;; flat list
  ;;; luckily it seems rare for the list to be more than 3
  (for result in (generate-all-combinations (make-handel-clusters lst-of-rels))
       collect
       (apply #'append result)))

(defstruct (cluster)
  key
  rels)

(defun make-handel-clusters (rels)
  (let ((clusters nil))
    (dolist (rel rels)
      (let* ((handel (get-var-num (rel-handel rel)))
            (existing (find handel clusters :key #'cluster-key)))
        (if existing
          (push rel (cluster-rels existing))
          (push (make-cluster :key handel
                              :rels (list rel))
                clusters))))
    (mapcar #'cluster-rels clusters)))
          
(defun generate-all-combinations (lst)
  (if (null lst) '(nil)
      (let ((subcombs (generate-all-combinations (cdr lst))))
        (append subcombs
                (for subcomb in subcombs
                     collect
                     (cons (car lst) subcomb))))))


;;;; ******* Printing scoped structures *********

(defun output-connected-mrs (mrs-instance device &optional file-name)
     (if file-name
      (with-open-file (stream file-name :direction :output)
         (output-connected-mrs1 mrs-instance device stream))
      (output-connected-mrs1 mrs-instance device t)))
  
; Added option to print raw MRS structures along with pretty-printed ones.

(defun output-connected-mrs1 (mrs-instance device stream)   
  (def-print-operations device 0 stream)
  (cond ((psoa-p mrs-instance)
         (mrs-output-start-fn *mrs-display-structure*)
         (print-connected-psoa mrs-instance 0)
         (mrs-output-end-fn *mrs-display-structure*)
         (mrs-output-max-width-fn *mrs-display-structure*))
        (t (mrs-output-error-fn *mrs-display-structure* mrs-instance))))


(defun print-connected-psoa (psoa indentation)
  (declare (ignore indentation))
  (mrs-output-start-psoa *mrs-display-structure*
           (get-bound-var-value (psoa-handel psoa))
	   (get-bound-var-value (psoa-index psoa)))
  (mrs-output-start-liszt *mrs-display-structure*)
  (loop for rel in (psoa-liszt psoa)
        do
        (mrs-output-start-rel *mrs-display-structure*
                 (rel-sort rel) (get-bound-var-value (rel-handel rel)))
        (loop for feat-val in (rel-flist rel)
              do
             (mrs-output-label-fn *mrs-display-structure*
                      (fvpair-feature feat-val))
             (let ((value (fvpair-value feat-val)))
                      (if (var-p value)
                          (mrs-output-atomic-fn *mrs-display-structure*
                             (get-bound-var-value value))
                          (if (and (listp value)
                                   (var-p (car value)))
                            (mrs-output-list-fn *mrs-display-structure*
                             (mapcar #'get-bound-var-value value))
                            (mrs-output-atomic-fn *mrs-display-structure*
                                                  value)))))
        (mrs-output-end-rel *mrs-display-structure*))
  (mrs-output-end-liszt *mrs-display-structure*)
  (mrs-output-end-psoa *mrs-display-structure*))


;;; printing utility fn

(defun get-bound-var-value (var)
  (let ((new-binding (assoc (get-var-num var) *canonical-bindings*)))
    (if new-binding
      (concatenate 'string (subseq (var-name var) 0 1)
                   (format nil "~A" (cdr new-binding))) 
      (var-name var))))




;;; **** Printing conventional LFs *******
;;; this doesn't fit easily into the paradigm for printing unscoped structures

(defun get-true-var-num (var)
  (let ((new-binding (assoc (get-var-num var) *canonical-bindings*)))
    (if new-binding
      (cdr new-binding) 
      (get-var-num var))))


(defun output-scoped-mrs (mrs &key (stream t))
  (format stream "~%")
  (let* ((top-handel (get-true-var-num (psoa-handel mrs)))
         (rel-list (psoa-liszt mrs)))
    (output-scoped-rels top-handel rel-list stream)
    (format stream "~%")))

(defun output-scoped-rels (top-handel rel-list stream)
  (let ((top-rels (for rel in rel-list
                        filter
                        (if (eql 
                             (get-true-var-num (rel-handel rel)) 
                             top-handel)
                            rel))))
    (when (null (list-length top-rels))
      (struggle-on-error "Circular LISZT passed to output-scoped-mrs"))
    (if (and top-rels (list-length top-rels))
        (loop (output-scoped-rel (car top-rels) rel-list stream)
              (if (cdr top-rels)
                  (progn (format stream " /~A " #\\)
                         (setf top-rels (cdr top-rels)))
                (return)))
      (format t "~%Warning: unconnected structure passed to output-scoped-mrs"))))

(defun output-scoped-rel (rel rel-list stream)
  (let ((need-comma nil))
    (format stream "~A(" 
            (remove-right-sequence "_rel" (string-downcase (rel-sort rel))))
    (for feat-val in (rel-flist rel)
         do     
         (when need-comma (format stream ", "))
         (setf need-comma t)
         (let* ((var (fvpair-value feat-val))
                (conj-vars (extract-variables-from-conj var)))
           (when conj-vars
             (setf var conj-vars))
           (if (listp var)
             (progn 
               (format stream "(")
               (for val in var
                    do
                    (if (is-handel-var val)
                      (output-scoped-rels (get-true-var-num val) rel-list stream)
                      (format stream "~A " (remove-variable-junk 
                                      (if (var-p val)
                                        (get-bound-var-value val)
                                        val)))))
               (format stream ")"))
             (if (is-handel-var var)
                  (output-scoped-rels (get-true-var-num var) rel-list stream)
                  (format stream "~A" (remove-variable-junk 
                                  (if (var-p var)
                                    (get-bound-var-value var)
                                    var)))))))
    (format stream ")")))




    
  


    
    