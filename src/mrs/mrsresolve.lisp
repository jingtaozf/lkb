;;; Functions for checking MRS stuctures and outputting
;;; the possible scoped structures in a linearised notation

(in-package "MRS")

;;; Macros

(defmacro get-rel-handel-num (rel)
  `(get-var-num (rel-handel ,rel)))


(defmacro is-handel-var (var)
    ;;; test is whether the string begins with #\h
  `(and (var-p ,var)
       (char-equal (elt (var-name ,var) 0) #\h)))


(defmacro nonquantified-var-p (var)
  ;;; true if the string identifier for the variable
  ;;; begins with anything other than x
  `(and (var-p ,var)
       (not (eq (elt (var-name ,var) 0) '#\x))))


;;; Following three are wrappers for get-full-handel-args-with-features

(defmacro get-handel-args (rel)
  ;;; returns a list of ids of any handels which are values
  ;;; of roles in the rel 
  `(mapcar #'(lambda (res)
              (var-id (cdr res))) 
          (get-full-handel-args-with-features ,rel)))

(defmacro get-full-handel-args (rel)
  ;;; returns a list of any handels which are values
  ;;; of roles in the rel 
  `(mapcar #'cdr 
          (get-full-handel-args-with-features ,rel)))


(defmacro get-handel-args-with-features (rel)
  ;;; returns a list of ids of any handels which are values
  ;;; of roles in the rel as a dotted pair, with the role
  ;;; as car
  `(for res in 
       (get-full-handel-args-with-features ,rel)
       collect
       (cons (car res) (var-id (cdr res)))))


(defmacro is-handel-arg-rel (rel)
  ;;; true if the rel has an argument which is a handel
  `(get-handel-args-with-features ,rel))

;;;
;;; ******** utility functions (some grammar specific) **********
;;;

;;; memoisation

(defvar *rel-handel-store* nil)
(defvar *unbound-vars-store* nil)


(defun clear-scope-memos nil
  (setf *rel-handel-store* nil)
  (setf *unbound-vars-store* nil))

;;; Utility functions for taking apart MRS structures.
;;; All the functions which rely on particular names for features or rels,
;;; or which identify variable sorts should be in this section

;;; the values of features may be var structures, lists of var structures 
;;; or constants

(defun proper-name-or-pronoun-rel (rel-sort)
  (member rel-sort *top-level-rel-types* :test #'eq))


(defun get-full-handel-args-with-features (rel)
  (let ((existing (assoc rel *rel-handel-store* :test #'eq)))
    (if existing (cdr existing)
        (let ((res
               (for fvp in (rel-flist  rel)
                    append
                    (let ((val (fvpair-value fvp))
                          (feature (fvpair-feature fvp)))
                      (cond ((is-handel-var val)
                             (list (cons feature val)))
                            ((listp val) 
                             (for val-el in val
                                  filter
                                  (if (and (var-p val-el) (is-handel-var val-el))
                                      (cons (fvpair-feature fvp) val-el))))
                            (t nil))))))
          (push (cons rel res) *rel-handel-store*)
          res))))

  
   
(defun is-quant-rel (rel)
  ;;; test is presence of a feature called BV!
  ;;; But also avoid including any "quantifiers" which are top-level rels,
  ;;; possibly including "the" (_def_rel) and demonstratives
  (dolist (fvpair (rel-flist  rel))
    (when (and (eq (fvpair-feature fvpair) *bv-feature*)
               (not (member (rel-sort rel) *top-level-rel-types* 
                            :test #'eq)))
      (return t))))

(defvar *quant-list* nil)

(defun quick-is-quant-rel (rel)
  (member rel *quant-list* :test #'eq))

(defun get-bv-value (rel)
  ;;; returns the integer value of the variable corresponding to the
  ;;; feature BV
  ;;; assumes that there is only one such feature and 
  ;;; that its value is a var 
  (dolist (fvpair (rel-flist  rel))
    (when (eq (fvpair-feature fvpair) *bv-feature*)
      (return (get-var-num (fvpair-value fvpair))))))

(defvar *q-rel-store* nil
  "associated quantifier relations with relations that
   contain a variable they bind")

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
  (setf *quant-list* quant-rels)
  (setf *q-rel-store* nil) 
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
                      (unless (member rel quant-rels :test #'eq)
                        (for var in rel-vars
                             do
                             (let ((associated-quantifier 
                                    (cdr (assoc (get-var-num var) 
                                                var-q-assoc))))
                               (when associated-quantifier
                                 (add-to-qrel-store 
                                  associated-quantifier rel)))))
                      (when (proper-name-or-pronoun-rel (rel-sort rel))
                        (for var in rel-vars
                             do
                             (unless (member (get-var-num var) quant-vars)
                               (pushnew (get-var-num var) *top-level-variables*))))
                       rel-vars)))))

(defun add-to-qrel-store (associated-quantifier rel)
  (let ((existing (assoc associated-quantifier *q-rel-store*)))
    (if existing (push rel (cdr existing))
      (push (list associated-quantifier rel)
            *q-rel-store*))))

  
(defun collect-vars-from-rel (rel)
  ;;; only called from find-unbound-vars 
  ;;;
  ;;; returns the variables from a relation (other than the handel)
  ;;; note that this returns the entire structures (necessary because
  ;;; of identification of sorts of variables)
  (for fvp in (rel-flist  rel)
       filter
       (let ((value (fvpair-value fvp)))
         (if (var-p value) 
             value))))
             
(defun collect-unbound-vars-from-rel (rel)
  ;;; collects all the variables from a rel which have to be bound
  ;;; off by some other rel: i.e. variables other than events,
  ;;; handels, and unspecified arguments
  ;;; which are not the values of a BV feature
  ;;; proper name  and pronoun rels act as implicit quantifiers themselves
  ;;; so that variables which are introduced by such rels are 
  ;;; calculated initially and stored in the global variable
  ;;; *top-level-variables*
  (let ((existing (assoc rel *unbound-vars-store* :test #'eq)))
    (if existing (cdr existing)
      (let ((res
             (for fvp in (rel-flist  rel)
                  append 
                  (unless (eq (fvpair-feature fvp) *bv-feature*)
                    (let ((value (fvpair-value fvp)))
                      (for el in (if (listp value) value (list value))
                           filter
                           (unbound-var-id el)))))))
        (push (cons rel res) *unbound-vars-store*)
        res))))

(defun unbound-var-id (el)
  (if (and (var-p el)
           (not (nonquantified-var-p el))
           (not (member (get-var-num el) 
                        *top-level-variables*)))
      (get-var-num el)))


;;;
;;; ******** Main code entry point  **********
;;;

#|
(defvar *canonical-bindings* nil
"global variable which is set to the current set of bindings for the
printing routines -  convenient to make this global to keep printing generic")

; moved to mrsglobals.lisp
|#

(defun show-some-scoped-structures (mrsstruct binding-sets 
                                    &optional (stream t) (max 10))
  (if binding-sets
      (progn 
        (format stream "~%~A scoped form(s)" (length binding-sets))
        (when (and max (> (length binding-sets) max))
          (format stream "only printing first ~A" max)))
    (if *fragment-p*
        (format stream "~%Treated as fragment~%")
      (format stream "~%WARNING: No valid scopes~%")))
  (for binding in (if max
                      (subseq binding-sets 0 
                              (min (length binding-sets) max))
                    binding-sets)
       do
       (setf *canonical-bindings* (canonical-bindings binding))
       (output-connected-mrs1 mrsstruct 'indexed stream)
       (output-scoped-mrs mrsstruct :stream stream)
       (format stream "~%--------------------------~%"))
  (format stream "~%"))
  

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
          (get-rel-handel-num rel)
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

(defvar *holes* nil)

(defvar *label-hole-pairs* nil)

(defun construct-initial-bindings (top-handel-var rels hcons)
  ;;; finds all the handels and constructs an
  ;;; initial list which just looks like
  ;;; ((1 . (1)) (2 . (2))) etc
  ;;;
  (setf *label-hole-pairs* nil)
  (let ((labels nil) (holes nil) (equated-list nil)
        (top-handel (get-var-num top-handel-var)))
    (for rel in rels
         do
         (let ((var (rel-handel rel)))
           (unless (is-handel-var var)
             (struggle-on-error "~%Relation ~A has incorrect handel ~A"
                                (rel-sort rel) var))
           (pushnew (get-var-num var) labels)))
    (for rel in rels
         do
         (let ((varnum (get-var-num (rel-handel rel)))
               (local-holes (get-handel-args rel)))
             (for handel-var in local-holes
                  do
                  (when (member handel-var holes)
                    (struggle-on-error 
                     "~%Relation ~A has duplicate handel arg ~A"
                     (rel-sort rel) handel-var))
                  (pushnew handel-var holes)
                  (when (member handel-var labels)
                    (push handel-var equated-list)))
             (push (cons varnum local-holes)
                   *label-hole-pairs*)))
    (pushnew top-handel holes) 
    (process-hcons hcons labels holes equated-list)
    (setf *holes* holes)
    (for handel in (remove-duplicates (append labels holes))
       collect
       (list handel handel))))


(defun get-bindings-for-handel (handel bindings)
  (let ((hb (assoc handel bindings :test #'eq)))
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

(defvar *quant-rels* nil)


#+lkb
(lkb::def-lkb-parameter *scoping-call-limit* 10000
  "for MRS scoping machinery - maximum number of calls"
  :lkb)

#-lkb
(defparameter *scoping-call-limit* 10000)

(defvar *top-top* nil)

(defvar *starting-rels* nil)

(defvar *cached-scope-structures* 
    (make-hash-table :test #'equal))

(defun make-scoped-mrs (mrsstruct)
  (clear-scope-memos)
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
                    (remove-duplicates (mapcar #'var-name free-variables)
                                       :test #'equal)))
          nil)
;;; variables must be bound by quantifiers unless they are in relations
;;; which license implicit existential binding
      (let ((top-handel (get-var-num (psoa-top-h mrsstruct))))
        (setf *quant-rels* quant-rels)
        (setf *scoping-calls* 0)
        (setf *top-top* top-handel)
        (setf *starting-rels* rels)
        (clrhash *cached-scope-structures*)
        (catch 'up
          (for result in (create-scoped-structures top-handel nil 
                                 (construct-initial-bindings 
                                  (psoa-top-h mrsstruct) 
                                  rels hcons)
                                 rels nil nil nil nil)
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
        constraint
pending-qeq - handel which must get used either this call
or modulo some number of quantifiers

|#

(defvar *debug-scoping* nil)

(defun create-scoped-structures (top-handel bvs bindings rels 
                                 scoping-handels pending-qeq scoped-p 
                                 scoping-rels)
  (when *debug-scoping*
    (show-scope-so-far top-handel bindings rels pending-qeq))
  (incf *scoping-calls*)
  (when (> *scoping-calls* *scoping-call-limit*)
    (unless *giving-demo-p* 
      (format t "~%Maximum scoping calls exceeded"))
    (throw 'up nil))
  (or (cached-scope-results top-handel bvs bindings rels 
                            pending-qeq 
                            scoped-p scoping-rels)
      (let ((new-structures
             (fresh-create-scoped-structures 
              top-handel bvs bindings rels 
              scoping-handels pending-qeq scoped-p scoping-rels)))
        (cache-bindings top-handel bvs
                        pending-qeq scoped-p rels 
                        new-structures scoping-rels)
        new-structures)))


(defvar *cache-on* t)

(defstruct (cache-entry)
  handels results)

(defstruct (res-cache)
  top-handel
  bvs
  scoping-rels
  pending-qeq
  scoped-p
  relations
  results)

(defun cache-bindings (top-handel bvs pending-qeq scoped-p relations 
                       results scoping-rels)
  (when *cache-on*
    (let* ((already-seen-set (gethash relations *cached-scope-structures*)))
      (unless already-seen-set
        (let ((rels-left-handels nil))
          (for rel in relations
               do
               (pushnew (get-rel-handel-num rel) rels-left-handels)
               (for arg in (get-handel-args rel)
                    do
                    (pushnew arg rels-left-handels)))
          (setf already-seen-set 
            (make-cache-entry :handels rels-left-handels))
          (setf (gethash relations *cached-scope-structures*)
            already-seen-set)))
      (push (make-res-cache 
             :top-handel top-handel
             :bvs bvs
             :scoping-rels scoping-rels
             :pending-qeq pending-qeq
             :scoped-p scoped-p
             :relations relations
             :results results)
             (cache-entry-results already-seen-set)))))



(defun print-cache nil
  ;;; for debugging
  (maphash #'(lambda (k v) 
               (format t "~%hash (~A)" (length k))
               (for val in (cache-entry-results v)
                    do
                    (format t "~%~A ~A ~A ~A" 
                            (res-cache-top-handel val)
                            (res-cache-bvs val)
;                            (res-cache-scoping-rels val)
                            (res-cache-scoped-p val)
                            (res-cache-pending-qeq val))))
           *cached-scope-structures*))


(defun set-equal (set1 set2)
  (and (eql (length set1) (length set2))
       (for x in set1
            all-satisfy
            (member x set2 :test #'eq))))
  
(defun cached-scope-results (top-handel bvs bindings rels-left
                             pending-qeq scoped-p scoping-rels) 
  ;;; if we've seen the same set of prior relations
  ;;; we can just retrieve the bindings we got last time
  ;;; ignoring any that refer to rels we've seen
  (if *cache-on*
      (let ((cached (gethash rels-left
                             *cached-scope-structures*)))
        (if cached
            (let* ((cache-handel nil)
                  (new-results
                   (dolist (cache (cache-entry-results cached))
                     (when (and 
                            (eql pending-qeq
                                 (res-cache-pending-qeq cache))
                            (or (and scoped-p (res-cache-scoped-p cache))
                                (eql top-handel 
                                     (res-cache-top-handel cache)))
                            (equal bvs (res-cache-bvs cache))
                            (set-equal scoping-rels 
                                       (res-cache-scoping-rels cache))
                            )
                       (setf cache-handel (res-cache-top-handel cache))
                       (return (res-cache-results cache))))))
              (if new-results
                  (construct-combined-results
                   new-results bindings
                   top-handel 
                   (cache-entry-handels cached)
                   cache-handel)))))))
                   
                   
(defun construct-combined-results (new-results bindings 
                                   new-top rels-left-handels res-handel)      
  (for new-result in new-results
       collect
       (let ((res-bindings (res-struct-bindings new-result)))
         (make-res-struct
          :bindings (for res-binding in res-bindings
                         collect
                         (if (member (car res-binding) 
                                     rels-left-handels)
                             (if (not (eql new-top res-handel)) 
                                 (subst new-top res-handel
                                        res-binding)
                               res-binding)
                           (assoc (car res-binding) bindings)))
          :other-rels (res-struct-other-rels new-result)))))
    
              
(defun fresh-create-scoped-structures  (top-handel bvs bindings rels 
                                        scoping-handels pending-qeq 
                                        scoped-p scoping-rels)
  (if rels      ;; otherwise fail since a handel arg won't be satisfied
   (progn
    (setf pending-qeq 
      (determine-new-pending top-handel bindings pending-qeq scoped-p))
    (let* ((impossible-top-rels 
            (calculate-impossible-top-rels top-handel bvs bindings rels 
                            scoping-handels pending-qeq))
           (impossible-handels 
            (let ((tmp nil))
              (for rel in impossible-top-rels do 
                   (pushnew (get-rel-handel-num rel) tmp)) tmp))
           (possible-top-rels 
            ; rels which aren't impossible and don't have impossible sisters 
            (for rel in rels filter 
                 (unless (or (member rel impossible-top-rels :test #'eq)
                             (member (get-rel-handel-num rel) 
                                     impossible-handels)) rel)))
           (pending-top-rels ; rels which have same handel as pending-qeq
            (if pending-qeq
                (for rel in possible-top-rels filter
                     (if (eql (get-rel-handel-num rel) pending-qeq) rel))))
           (possible-quant-top-rels 
            (if pending-qeq
             (for rel in possible-top-rels filter 
                  (if (quick-is-quant-rel rel) rel))))
           (top-rels ; have the same handel as the top handel
            (for rel in rels filter 
                 (if (eql (get-rel-handel-num rel) top-handel) rel))))
      (if (if pending-qeq 
              (and  (or (not top-rels) *alex-mode*)
                   (or possible-quant-top-rels pending-top-rels))
            ;; if we've got a pending-qeq, there can be no
            ;; known top-rels (maybe iffy?) and there must
            ;; be either possible quantifiers or relations
            (and possible-top-rels 
                 (subsetp top-rels possible-top-rels)))
          ;; unless there are some possible rels and all the top rels
          ;; are possible, we fail
        (for rel-comb-and-bindings in 
             (make-combinations-of-top-rels top-handel top-rels
              (set-difference possible-top-rels top-rels)
              pending-top-rels possible-quant-top-rels
              pending-qeq bindings)
             ;;; for each possible set of rels which can have their handels
             ;;; bound to the top-handel, generate a set of results
             append
             (let ((sisters (bindings-and-sisters-sisters rel-comb-and-bindings)))
               (sister-structure-scope (car sisters) (cdr sisters) bvs 
                (bindings-and-sisters-bindings rel-comb-and-bindings) 
                (ordered-set-difference rels sisters)
                (ordered-insert top-handel scoping-handels)
                (bindings-and-sisters-pending-qeq rel-comb-and-bindings)
                scoping-rels)))
        (when *debug-scoping*
          (format t "~%Failure: ~%top-rels ~A~%pending-qeq  ~A~%" 
                  top-rels pending-qeq)
          nil))))))

(defun ordered-set-difference (lst to-go)
  ;; set-difference doesn't guarantee the order of results
  (for el in lst
       filter
       (unless (member el to-go :test #'eq)
           el)))

(defun ordered-insert (el lst)
  ;;; has to copy
  (cond ((null lst) (list el))
        ((> el (car lst)) (cons el lst))
        (t (cons (car lst) (ordered-insert el (cdr lst))))))

(defun determine-new-pending (top-handel bindings pending-qeq scoped-p)
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


(defun calculate-impossible-top-rels (top-handel bvs bindings rels 
                                      scoping-handels pending-qeq)
 ;;; the list of rels which introduce variables which are
 ;;; currently free or which have a handel which is coindexed with a handel
 ;;; argument of anything other than the current handel
 ;;; or which are `outscoped' by top-handel
 ;;; or which are outscoped by a handel which is not on the list
 ;;; of scoping-handels
 ;;; or which are qeq something which is not the pending-qeq
  (for rel in rels
       filter 
       (let ((handel-num (get-rel-handel-num rel)))
         (if (or (and (not (eql handel-num top-handel))
                      (member handel-num *holes*))
                 (let ((vars (collect-unbound-vars-from-rel rel)))
                   (and vars (not (subsetp vars bvs))))
                 (not-qeq-p pending-qeq handel-num
                            bindings)
                 (and (quick-is-quant-rel rel)
                     (check-quant-qeqs scoping-handels 
                                          pending-qeq rel bindings)))
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
      ;;; unless we're in alex-mode, there can be no known-top-rels
      ;;; if there is a pending-qeq
      (let ((pendingless (set-difference other-possibles pending)))
        (append
         (if pending  
             (new-bindings-and-sisters pendingless 
                                       (list (append pending known-top-rels))
                                       top-handel bindings pending-qeq nil))
         (if possible-quants
             (for res in
                  (new-bindings-and-sisters 
                   pendingless (list known-top-rels)
                   top-handel bindings nil pending-qeq)
                  filter
                  (if (intersection possible-quants 
                                    (bindings-and-sisters-sisters res))
                      res)))))
    (new-bindings-and-sisters other-possibles 
                              (list known-top-rels)
                              top-handel bindings nil nil)))


(defun new-bindings-and-sisters (free fixed-list top-handel 
                                 bindings binding-qeq result-qeq)            
  (let ((other-combinations 
         (generate-combinations free)))
      ;;; other-combinations is a list of sets of rels 
      ;;; including the empty set
    (for fixed in fixed-list
         append
         (for combination in other-combinations
              filter
              (let* ((combined-rels (append fixed combination)))
                     (if (mutually-compatible combined-rels)
                         (let ((new-bindings 
                                (generate-top-bindings 
                                 top-handel combined-rels 
                                 bindings binding-qeq)))
                           (if new-bindings
                               (make-bindings-and-sisters 
                                :sisters combined-rels 
                                :bindings new-bindings 
                                :pending-qeq result-qeq)))))))))

(defun mutually-compatible (sister-rels)
  (if sister-rels
      (let ((current-rels sister-rels))
        (loop (let ((start-rel (car current-rels)))
                (setf current-rels (cdr current-rels))
                (unless current-rels
                  (return t))
                (when (incompatible-rel-list start-rel current-rels)
                  (return nil)))))))

(defun incompatible-rel-list (rel1 rel-list)
  (for test-rel in rel-list
       some-satisfy
       (incompatible-rels test-rel rel1)))

(defun incompatible-rels (rel1 rel2)
  (cond ((quick-is-quant-rel rel1)
         (if (quick-is-quant-rel rel2)
             (incompatible-quantifiers rel1 rel2)
           (if (is-handel-arg-rel rel2)
               (incompatible-q-and-scope rel1 rel2)
             nil)))
        ((quick-is-quant-rel rel2)
         (if (is-handel-arg-rel rel1)
             (incompatible-q-and-scope rel2 rel1)
           nil))
        ;;; the case of the two scoped relations
        ;;; where one is qeq the other should have been allowed
        ;;; for earlier
        (t nil)))

  
                                
;;; ******* Traversing the current leaves of the tree ********

(defun sister-structure-scope (first-rel top-rels bvs bindings 
                               other-rels scoping-handels 
                               pending-qeq scoping-rels)
  ;;; deals with multiple elements in a putative conjunction
 (if (is-handel-arg-rel first-rel) 
   (let* ((new-bvs (if (quick-is-quant-rel first-rel) 
                     (ordered-insert (get-bv-value first-rel) bvs)
                     bvs))
          (handel-args (get-handel-args-with-features first-rel))
          (full-results (sister-args-scope handel-args new-bvs 
                                           bindings other-rels
                                           scoping-handels pending-qeq
                                           (cons first-rel scoping-rels))))
     (if full-results
       (if top-rels
         (for res in full-results
              append
              (sister-structure-scope (car top-rels) (cdr top-rels)
                                      bvs (res-struct-bindings res) 
                                      (res-struct-other-rels res)
                                      scoping-handels pending-qeq
                                      scoping-rels))
         full-results)))
   (if top-rels
     (sister-structure-scope (car top-rels) (cdr top-rels)
                             bvs bindings other-rels scoping-handels
                             pending-qeq scoping-rels)
     (list (make-res-struct :bindings bindings :other-rels other-rels)))))


(defun sister-args-scope (top-handels bvs bindings other-rels scoping-handels
                          pending-qeq scoping-rels)
  ;;; this is necessary to deal with the case of relations which have
  ;;; multiple handle arguments
  (let ((results (create-scoped-structures 
                  (cdar top-handels) bvs
                  bindings other-rels scoping-handels pending-qeq
                  (eq (caar top-handels) *scope-feat*) 
                  scoping-rels)))
    (if (cdr top-handels)
      (for res in results
              append
              (sister-args-scope (cdr top-handels) bvs
                                      (res-struct-bindings res) 
                                      (res-struct-other-rels res)
                                      scoping-handels pending-qeq
                                      scoping-rels))
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

(defun make-handel-clusters (rels)
  (let ((clusters nil)
        (bad-handels nil))
    (dolist (rel rels)
      (let* ((handel (get-rel-handel-num rel))
             (existing (assoc handel clusters)))
        (unless (member handel bad-handels)
          (if existing
              (if (incompatible-rel-list rel (cdr existing))
                  (push handel bad-handels)
                (push rel (cdr existing)))
              (push (cons handel (list rel))
                    clusters)))))
    (for cluster in clusters
         filter
         (unless (member (car cluster) bad-handels)
           (cdr cluster)))))
             
(defun generate-all-combinations (lst)
  (if (null lst) '(nil)
      (let ((subcombs (generate-all-combinations (cdr lst))))
        (append subcombs
                (for subcomb in subcombs
                     filter
                     (unless (for trial in (car lst)
                                  some-satisfy
                                  (for sub in subcomb
                                       some-satisfy
                                       (incompatible-rel-list 
                                        trial sub)))
                       (cons (car lst) subcomb)))))))


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
  (print-psoa psoa t))


;;; printing utility fn

(defun get-bound-var-value (var)
  (when (var-p var)
    (let ((new-binding (assoc (get-var-num var) *canonical-bindings*)))
      (if new-binding
          (concatenate 'string (subseq (var-name var) 0 1)
                       (format nil "~A" (cdr new-binding))) 
        (var-name var)))))




;;; **** Printing conventional LFs *******
;;; this doesn't fit easily into the paradigm for printing unscoped structures

(defun get-true-var-num (var)
  (when (var-p var)
    (let ((new-binding (assoc (get-var-num var) *canonical-bindings*)))
      (if new-binding
          (cdr new-binding) 
        (get-var-num var)))))

(defvar *output-scope-errors* nil)

(defvar *mrs-right-margin* 50)

(defun output-scoped-mrs (mrs &key (stream t))
  (format stream "~%")
  (let* ((top-handel (get-true-var-num (psoa-top-h mrs)))
         (rel-list (psoa-liszt mrs)))
    (setf *output-scope-errors* nil)
    (output-scoped-rels top-handel rel-list stream nil 0)
    (when *output-scope-errors*
      (unless (or *giving-demo-p* *debug-scoping*)
        (format t "~%WARNING: unconnected structure passed to output-scoped-mrs")))
    (format stream "~%")))

(defun output-scoped-rels (top-handel rel-list stream handels-so-far width)
  (if (member top-handel handels-so-far)
      (progn 
        (struggle-on-error "Circular structure passed to output-scoped-mrs")
        nil)
    (let ((top-rels (for rel in rel-list
                         filter
                         (if (eql 
                              (get-true-var-num (rel-handel rel)) 
                              top-handel)
                             rel))))
      (when (null (list-length top-rels))
      ;;; list-length returns nil if top-rels is a cycle
        (struggle-on-error "Circular LISZT passed to output-scoped-mrs"))
      (if (and top-rels (list-length top-rels))
          (loop
            (setf width
                   (output-scoped-rel (car top-rels) rel-list stream 
                                      (cons top-handel handels-so-far) width))
            (if (cdr top-rels)
                (progn (format stream " /~A " #\\)
                       (setf width (+ 4 width))
                       (setf top-rels (cdr top-rels))
                       (when (> width *mrs-right-margin*)
                         (format stream "~%")
                         (setf width 0)))
              (return)))
        (setf *output-scope-errors* t))))
  width)


(defun output-scoped-rel (rel rel-list stream handels-so-far width)
  (let ((need-comma nil)
        (current-string nil))
    (when (> width *mrs-right-margin*)
      (format stream "~%")
      (setf width 0))
    (setf current-string
      (format nil "~A(" 
              (remove-right-sequence "_rel" (string-downcase (rel-sort rel)))))
    (format stream "~A" current-string)
    (setf width (+ width (length current-string)))
    (for feat-val in (rel-flist rel)
         do     
         (when need-comma (format stream ", ") (setf width (+ width 2)))
         (setf need-comma t)
         (let* ((var (fvpair-value feat-val)))
             (if (is-handel-var var)
                 (setf width
                   (output-scoped-rels (get-true-var-num var) rel-list stream
                                       handels-so-far width))
               (progn
                 (setf current-string
                  (format nil "~A" (remove-variable-junk 
                                  (if (var-p var)
                                    (get-bound-var-value var)
                                    var))))
                 (format stream "~A" current-string)
                 (setf width (+ width (length current-string)))))))
    (format stream ")")
    (incf width)
    width))

(defun show-scope-so-far (top-handel bindings rels-left pending)
  (let ((*canonical-bindings* (canonical-bindings bindings)))
    (let ((true-top (cdr (assoc *top-top* *canonical-bindings*))))
      (when true-top
        (format t "~%pending: ~A top: ~A " pending top-handel)
        (output-scoped-rels true-top
                            (set-difference *starting-rels* rels-left) t nil 0)))))

    
  


    
    