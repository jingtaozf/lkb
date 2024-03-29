;;; Copyright (c) 1998-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see LICENSE for conditions

;;; Functions for checking MRS stuctures and outputting
;;; the possible scoped structures in a linearised notation

(in-package "MRS")

;;; Macros

(defmacro get-rel-handel-num (rel)
  `(get-var-num (rel-handel ,rel)))

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
  `(loop for res in 
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

(defparameter *scoping-partial-results-p* t)
(defparameter *scoping-intermediate-scopes* nil)

(defun clear-scope-memos nil
  (setf *rel-handel-store* nil)
  (setf *unbound-vars-store* nil))

;;; Utility functions for taking apart MRS structures.
;;; All the functions which rely on particular names for features or rels,
;;; or which identify variable sorts should be in this section

;;; the values of features may be var structures, lists of var structures 
;;; or constants

(defun top-level-rel-p (rel-pred)
  ;;; test changed to equal, because rels are strings in some grammars
  (member rel-pred *top-level-rel-types* :test #'equal))


(defun get-full-handel-args-with-features (rel)
  (let ((existing (assoc rel *rel-handel-store* :test #'eq)))
    (if existing (cdr existing)
        (let ((res
               (loop
                   for fvp in (rel-flist rel)
                   for feature = (fvpair-feature fvp)
                   for val = (fvpair-value fvp)
                   unless (member feature *scoping-ignored-roles* :test #'eq)
                   nconc
                     (if (is-handel-var val)
                         (list (cons feature val))
                       nil))))
          (push (cons rel res) *rel-handel-store*)
          res))))

  
   
(defun is-quant-rel (rel)
  ;;; test is presence of the body feature (prefered test)
  ;;; or membership of *quant-rel-types*
  ;;;
  ;;; Since *scope-feat* is required on all quantifers
  ;;; for the scope resolution code to work, this is a reasonable test
  ;;;
  ;;; The code also allows us to avoid including any "quantifiers" 
  ;;; which are top-level rels, which for some grammars might 
  ;;; include "the" (_def_rel) and demonstratives, but currently
  ;;; this is not done for Matrix grammars
  (and
   (if *quant-rel-types*
       (member (rel-pred rel) *quant-rel-types* 
               :test #'string-equal)
       (dolist (fvpair (rel-flist rel))
         (when (and (eq (fvpair-feature fvpair) *scope-feat*))
           (return t))))
   (not (member (rel-pred rel) *top-level-rel-types* :test #'equal))))


(defvar *quant-list* nil)

(defun quick-is-quant-rel (rel)
  (member rel *quant-list* :test #'eq))

(defun get-bv-value (rel)
  ;;; returns the integer value of the variable corresponding to 
  ;;; a specified feature (originally BV, now ARG0 in Matrix etc)
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
  ;;; It takes a list of rels which are quantifiers 
  ;;; and another list of rels, and returns a list of the 
  ;;; variables in that list (as integers) which are not bound by the
  ;;; quant-rels and which do not occur as variables in
  ;;; relations which are specified to occur at top level
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
    (loop for rel in quant-rels
         do
         (let ((quant-var (get-bv-value rel)))
           (unless quant-var 
             (struggle-on-error 
              "~%Rel ~A has an uninstantiated bound variable"
                    (rel-pred rel)))
           (when (member quant-var quant-vars)
             (struggle-on-error "~%Rel ~A has a duplicate bound variable"
                    (rel-pred rel)))
           (when quant-var
             (push quant-var quant-vars)
             (push (cons quant-var rel) var-q-assoc))))
    (remove-if #'(lambda (value)
                   (or (member (get-var-num value) quant-vars)
                       (member (get-var-num value) *top-level-variables*)))
               (loop for rel in rels
                    append
                    (let ((rel-vars (collect-vars-from-rel rel)))
                      (unless (member rel quant-rels :test #'eq)
                        (loop for var in rel-vars
                             do
                             (let ((associated-quantifier 
                                    (cdr (assoc (get-var-num var) 
                                                var-q-assoc))))
                               (when associated-quantifier
                                 (add-to-qrel-store 
                                  associated-quantifier rel)))))
                      (when (top-level-rel-p (rel-pred rel))
                        (loop for var in rel-vars
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
  (loop
      for fvp in (rel-flist  rel)
      for feature = (fvpair-feature fvp)
      unless (member feature *scoping-ignored-roles* :test #'eq)
      nconc
        (let ((value (fvpair-value fvp)))
          (and (var-p value) (list value)))))
             
(defun collect-unbound-vars-from-rel (rel)
  ;;; collects all the variables from a rel which have to be bound
  ;;; off by some other rel: i.e. variables other than events,
  ;;; handels, and unspecified arguments
  ;;; which are not the values of *bv-feature*
  ;;; proper name  and pronoun rels act as implicit quantifiers themselves
  ;;; so that variables which are introduced by such rels are 
  ;;; calculated initially and stored in the global variable
  ;;; *top-level-variables*
  (let ((existing (assoc rel *unbound-vars-store* :test #'eq)))
    (if existing (cdr existing)
      (let ((res (if (quick-is-quant-rel rel)
                      nil
             (loop
                 for fvp in (rel-flist  rel)
                 for feature = (fvpair-feature fvp)
                 unless (member feature *scoping-ignored-roles* :test #'eq)
                 append 
                    (let ((value (fvpair-value fvp)))
                      (loop for el in (if (listp value) value (list value))
                          nconc
                            (let ((id
                                   (unbound-var-id el)))
                              (if id (list id)))))))))
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
          (format stream "only printing first ~A~%" max)))
    (if *fragment-p*
        (format stream "~%Treated as fragment~%")
      (format stream "~%WARNING: No valid scopes~%")))
  (loop for binding in (if max
                      (subseq binding-sets 0 
                              (min (length binding-sets) max))
                    binding-sets)
       do
       (setf *canonical-bindings* (canonical-bindings binding))
       ;;; (output-connected-mrs1 mrsstruct 'indexed stream)
       (output-scoped-mrs mrsstruct :stream stream)
       (format stream "~%--------------------------~%"))
  (format stream "~%"))
  

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
    (loop for rel in rels
         do
         (pushnew
          (get-rel-handel-num rel)
          new-equality))
    (pushnew handel new-equality)
    (when extra
      (pushnew extra new-equality))
    (loop for binding in existing-bindings
         ;;; some of this structure is shared, but
         ;;; we never alter any binding destructively
         collect
           (cons (car binding) 
                 (if (member (car binding) new-equality)
                     new-equality
                   (cdr binding))))))

(defun adjust-bindings (bindings hole label)
  (loop for binding in bindings
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
    (loop for rel in rels
         do
         (let ((var (rel-handel rel)))
           (unless (is-handel-var var)
             (struggle-on-error "~%Relation ~A has incorrect handel ~A"
                                (rel-pred rel) var))
           (pushnew (get-var-num var) labels)))
    (loop for rel in rels
         do
         (let ((varnum (get-var-num (rel-handel rel)))
               (local-holes (get-handel-args rel)))
             (loop for handel-var in local-holes
                  do
                  (when (member handel-var holes)
                    (struggle-on-error 
                     "~%Relation ~A has duplicate handel arg ~A"
                     (rel-pred rel) handel-var))
                  (pushnew handel-var holes)
                  (when (member handel-var labels)
                    (push handel-var equated-list)))
             (push (cons varnum local-holes)
                   *label-hole-pairs*)))
    (pushnew top-handel holes) 
    (process-hcons hcons labels holes equated-list)
    (setf *holes* holes)
    (loop for handel in (remove-duplicates (append labels holes))
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
    (loop for binding in bindings
         do
         (unless (assoc (car binding) result)
           (loop for equiv in (cdr binding)
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
  (when *rel-handel-path*
    (clear-scope-memos)
    (setf *top-level-variables* nil)
    (setf *scoping-intermediate-scopes* nil)
    (let* ((rels (psoa-liszt mrsstruct))
           (hcons (psoa-h-cons mrsstruct))
           (quant-rels (loop for rel in rels when (is-quant-rel rel) collect rel))
           (implicit-existentials (find-unbound-vars rels quant-rels))
                                        ; find-unbound-vars has the side effect of pushing variables which
                                        ; are in pronoun or name rels into *top-level-variables* 
           (free-variables (loop for var in implicit-existentials
                                unless (nonquantified-var-p var)
                                collect var)))
      (if free-variables
          (progn
            (struggle-on-error "~%Free variables in MRS: ~A" 
                      (remove-duplicates (mapcar #'var-string free-variables)
                                         :test #'equal))
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
            (loop for result in (create-scoped-structures top-handel nil 
                                                     (construct-initial-bindings 
                                                      (psoa-top-h mrsstruct) 
                                                      rels hcons)
                                                     rels nil nil nil nil)
                 unless (res-struct-other-rels result) 
               ;;; the bindings slot of the result is a bindings set, the
               ;;; other-rels is a set of `left over' rels
               ;;; we don't want any of these at the top level
                 collect
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
    (throw 'up 
      (when *scoping-partial-results-p*
        (loop
            for candidate in *scoping-intermediate-scopes*
            unless (res-struct-other-rels candidate)
            collect (res-struct-bindings candidate)))))
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
          (loop for rel in relations
               do
               (pushnew (get-rel-handel-num rel) rels-left-handels)
               (loop for arg in (get-handel-args rel)
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
               (loop for val in (cache-entry-results v)
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
       (not
        (dolist (x set1)
           (unless
               (member x set2 :test #'eq)
               (return t))))))
  
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
  (loop for new-result in new-results
       collect
        (let ((res-bindings (res-struct-bindings new-result)))
          (first (push 
         (make-res-struct
          :bindings (loop for res-binding in res-bindings
                         collect
                         (if (member (car res-binding) 
                                     rels-left-handels)
                             (if (not (eql new-top res-handel)) 
                                 (subst new-top res-handel
                                        res-binding)
                               res-binding)
                           (assoc (car res-binding) bindings)))
          :other-rels (res-struct-other-rels new-result))
         *scoping-intermediate-scopes*)))))
    
              
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
              (loop for rel in impossible-top-rels do 
                   (pushnew (get-rel-handel-num rel) tmp)) tmp))
           (possible-top-rels 
            ; rels which aren't impossible and don't have impossible sisters 
            (loop for rel in rels 
                unless
                 (or (member rel impossible-top-rels :test #'eq)
                             (member (get-rel-handel-num rel) 
                                     impossible-handels))
                 collect rel))
           (pending-top-rels ; rels which have same handel as pending-qeq
            (if pending-qeq
                (loop for rel in possible-top-rels
                    when (eql (get-rel-handel-num rel) pending-qeq) 
                    collect rel)))
           (possible-quant-top-rels 
            (if pending-qeq
                (loop for rel in possible-top-rels 
                    when (quick-is-quant-rel rel) 
                   collect rel)))
           (top-rels ; have the same handel as the top handel
            (loop for rel in rels
                when (eql (get-rel-handel-num rel) top-handel) 
                collect rel)))
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
        (loop for rel-comb-and-bindings in 
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
  (loop for el in lst
       unless (member el to-go :test #'eq)
       collect el))

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
            (struggle-on-error
                      "~%qeq specified for scope handel ~A" 
                      top-handel))
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
  (loop for rel in rels
       nconc 
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
             (list rel)))))


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
             (loop for res in
                  (new-bindings-and-sisters 
                   pendingless (list known-top-rels)
                   top-handel bindings nil pending-qeq)
                  when (intersection possible-quants 
                                    (bindings-and-sisters-sisters res))
                  collect res))))
    (new-bindings-and-sisters other-possibles 
                              (list known-top-rels)
                              top-handel bindings nil nil)))


(defun new-bindings-and-sisters (free fixed-list top-handel 
                                 bindings binding-qeq result-qeq)            
  (let ((other-combinations 
         (generate-combinations free)))
      ;;; other-combinations is a list of sets of rels 
      ;;; including the empty set
    (loop for fixed in fixed-list
         append
         (loop for combination in other-combinations
              nconc
              (let* ((combined-rels (append fixed combination)))
                     (if (mutually-compatible combined-rels)
                         (let ((new-bindings 
                                (generate-top-bindings 
                                 top-handel combined-rels 
                                 bindings binding-qeq)))
                           (if new-bindings
                               (list 
                                (make-bindings-and-sisters 
                                :sisters combined-rels 
                                :bindings new-bindings 
                                :pending-qeq result-qeq))))))))))

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
  (dolist (test-rel rel-list)
       (when
           (incompatible-rels test-rel rel1)
         (return t))))

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
         (loop for res in full-results
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
     (list (first (push (make-res-struct :bindings bindings :other-rels other-rels) *scoping-intermediate-scopes*))))))


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
      (loop for res in results
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
  (loop for result in (generate-all-combinations (make-handel-clusters lst-of-rels))
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
    (loop for cluster in clusters
         unless (member (car cluster) bad-handels)
         collect (cdr cluster))))
             
(defun generate-all-combinations (lst)
  (if (null lst) '(nil)
      (let ((subcombs (generate-all-combinations (cdr lst))))
        (append subcombs
                (loop for subcomb in subcombs
                     unless (dolist (trial (car lst))
                                  (when
                                      (dolist (sub subcomb)
                                       (when
                                           (incompatible-rel-list 
                                            trial sub)
                                         (return t)))
                                   (return t)))
                      collect (cons (car lst) subcomb))))))


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
          (concatenate 'string (subseq (var-string var) 0 1)
                       (format nil "~A" (cdr new-binding))) 
        (var-string var)))))




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
    (let ((top-rels (loop for rel in rel-list
                         when (eql 
                              (get-true-var-num (rel-handel rel)) 
                              top-handel)
                         collect rel)))
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
              (remove-right-sequence 
               *sem-relation-suffix* (string-downcase (rel-pred rel)))))
    (format stream "~A" current-string)
    (setf width (+ width (length current-string)))
    (loop for feat-val in (rel-flist rel)
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

;;; Making a single scoped MRS

#| For the MRSs produced by the ERG, ignoring (for now) any
constraints other than qeqs - hypothesise that any scopable MRS
will have a scoped structure which corresponds to replacing all of the
qeqs with eqs, apart from the top one, and inserting in the topmost
position all the quantifiers in the order required by the variables
in their restrictors.

This is in fact not true, but the exceptions are so infrequent that
this may still be useful!
|#

#|
(defun test-one-scope (&optional test-list up-to)
  (let ((num 1))
    (dolist (eg *rmrs-test-suite*)
      (let ((sentence (car eg))
	    (parsenum (cdr eg)))
	(when (and sentence 
		   (if test-list (member num test-list) t)
		   (if up-to (> num up-to) t))
	  (let ((lkb::*show-parse-p* nil))
	    (lkb::do-parse-tty sentence)
	    (when lkb::*parse-record*
	      (let ((selected-parse (nth (- parsenum 1) lkb::*parse-record*)))
		(if (not selected-parse)
		    (format t "~%WARNING: Incorrect parse number in ~A" num)
		  (let ((mrs (extract-mrs selected-parse)))
		    (unless mrs (error "~%Can't extract MRS"))
		    (let* ((original-scopes 
			    (length (make-scoped-mrs mrs)))
			   (one-scope (produce-one-scope mrs))
			   (bindings (if one-scope 
					 (make-scoped-mrs one-scope))))
		      (if bindings
			  (if (cdr bindings)
			      (format t "~%Error: multiple scopes for ~A" num)
			    (if (extra-bindings-p (car bindings))
				(format t "~%Warning: incomplete scoping for ~A" 
					num)
			      (if (> original-scopes 0)
				  (format t "~%~A successfully scoped" num)
				(format t "~%Error: ~A scoped, original failed to scope" num))))
		    ;;; if no scopes
			(if (> original-scopes 0)
			    (format t "~%Error: no scopes for ~A, original scoped" num)
			  (format t "~%~A failed to scope, as did original" num)))
		      (when test-list
			(output-mrs1 mrs 'simple t)
			(output-mrs1 one-scope 'simple t)
			(pprint bindings)
			(when (or (cdr bindings) 
				  (extra-bindings-p (car bindings)))
			  (dolist (binding bindings)
			    (setf *canonical-bindings* binding)
			    (output-scoped-mrs one-scope :stream t))))))))))))
	  (setf num (+ 1 num)))))
|#

(defun extra-bindings-p (binding-set)
  (dolist (binding binding-set)
    (when 
	(let ((first (car binding)))
	  (dolist (other (cdr binding))
	    (unless (eql other first)
	      (return t))))
      (return t))))
		       

#|
;;; generator testing

(defun test-qeq-gen (&optional test-list up-to)
  (let ((num 1))
    (dolist (eg *rmrs-test-suite*)
      (let ((sentence (car eg))
	    (parsenum (cdr eg)))
	(when (and sentence 
		   (if test-list (member num test-list) t)
		   (if up-to (> num up-to) t))
	  (let ((lkb::*show-parse-p* nil))
	    (lkb::do-parse-tty sentence)
	    (when lkb::*parse-record*
	      (let ((selected-parse (nth (- parsenum 1) lkb::*parse-record*)))
		(if (not selected-parse)
		    (format t "~%WARNING: Incorrect parse number in ~A" num)
		  (let ((mrs (extract-mrs selected-parse)))
		    (unless mrs (error "~%Can't extract MRS"))
		    (let* ((lkb::*bypass-equality-check* :eqqeqs)
			   (%mrs-extras-defaults% nil)
			   (genresults (lkb::generate-from-mrs 
					(equate-all-qeqs mrs)))
			   (strgen (loop for res in genresults
				       collect
				       (format nil "~{~A~^ ~}" res))))
		      (unless (member sentence strgen :test #'string-equal)
			(format t "~%WARNING `~A' not generated" sentence))
		      (pprint strgen)))))))))
      (setf num (+ 1 num)))))
	 
|#

(defun equate-all-qeqs (mrsstruct)
  (let* ((copy-mrsstruct (copy-psoa-liszt-completely mrsstruct t))
         (qeq-list (psoa-h-cons copy-mrsstruct))
	 (equated-struct (equate-qeqs qeq-list copy-mrsstruct)))
      equated-struct))

(defun produce-one-scope (mrsstruct)
  ;;; returns nil in the case where something goes wrong
  (let ((copy-mrsstruct (copy-psoa-liszt-completely mrsstruct t)))
    (multiple-value-bind 
	(top-qeq top-qeq-hole-fvp) 
	(find-top-qeq-hole copy-mrsstruct)
      (let*
	  ((rels (psoa-liszt copy-mrsstruct))
	   (qeq-list (remove top-qeq (psoa-h-cons copy-mrsstruct)))
	   (equated-struct (equate-qeqs qeq-list copy-mrsstruct))
	   (q-rels (loop for rel in rels
		       when (is-quant-rel rel) collect rel))
	   (implicit-existentials (find-unbound-vars rels q-rels))
	   (free-variables (loop for var in implicit-existentials
			       unless (nonquantified-var-p var)
			       collect var)))
	(if free-variables
	    (progn
	      (struggle-on-error "~%Free variables in MRS: ~A"
				 (remove-duplicates (mapcar #'var-string free-variables)
						    :test #'equal))
	      nil) ; returns nil if there are free variables
	  (let
	      ((qlist (if q-rels 
			  (order-quantifiers equated-struct q-rels top-qeq))))
	    (if q-rels
		(if qlist
		    (let ((top-q-label (rel-handel (car qlist))))
		      (setf (psoa-h-cons equated-struct) 
			nil)
		      (if top-qeq-hole-fvp 
			  (setf (fvpair-value top-qeq-hole-fvp)
			    top-q-label)
			(setf 
			    (psoa-top-h equated-struct)
			  top-q-label))
		      equated-struct)
	      ;;; qrels but no qlist means failure - return nil
		  nil)
	  ;;; no quantifiers, so set the marg directly to
	  ;;; the outscpd
	      (progn
		(setf (psoa-h-cons equated-struct) 
		  nil)
		(when top-qeq-hole-fvp 
		  (setf (fvpair-value top-qeq-hole-fvp)
		    (hcons-outscpd top-qeq)))
		equated-struct))))))))
	  
(defun equate-qeqs (qeq-list mrsstruct)
  ;;; DESTRUCTIVE!
  ;;; replace all the qeqs in the hcons with equalities
  ;;; except the top one, in the case of an ERG-like grammar
  ;;; where the top handle is equated with a relation which has
  ;;; a qeq constraint
  (when qeq-list
    (loop for rel in (psoa-liszt mrsstruct)
	do
	  (loop for fvp in (rel-flist rel)
	      do
		(let ((val (fvpair-value fvp)))
		  (if (is-handel-var val)
		      (let ((equated-var (find-qeq-outscpd val qeq-list)))
			(when equated-var
			  (setf (fvpair-value fvp) equated-var))))))))
  mrsstruct)

(defun find-qeq-outscpd (val qeq-list)
  (dolist (qeq qeq-list)
    (when (eql-var-id val (hcons-scarg qeq))
      (return (hcons-outscpd qeq)))))

(defun find-top-qeq-hole (mrsstruct)
  ;;; grammar specific
  ;;; if the top handel is bound to something with an MARG
  ;;; follow the chain of relations down until we come to a qeq
  ;;; and then return the hole which is filled by a qeq
  (let ((top-h (psoa-top-h mrsstruct)))
    (if (and top-h
	     (var-p top-h))
      (let* ((var-num (var-id top-h))
	     (top-rels
	      (get-rels-with-label-num var-num mrsstruct)))
	(if (or (not top-rels) (cdr top-rels))		; should be unique
	    nil
	  (chain-down-margs (car top-rels) mrsstruct))))))

(defun chain-down-margs (rel mrsstruct)
  (let* ((marg-fvp (dolist (fvpair (rel-flist rel))
		    (when (eq (fvpair-feature fvpair) 'lkb::marg)
		      (return fvpair))))
	(marg-value 
	 (if marg-fvp
	       (get-var-num (fvpair-value marg-fvp)))))
    (if marg-value
	(let ((top-rels
	       (get-rels-with-label-num marg-value mrsstruct)))
	  (if top-rels
	      (if (cdr top-rels)
		  nil
		(chain-down-margs (car top-rels) mrsstruct))
	    (dolist (qeq (psoa-h-cons mrsstruct))
	      (when (eq marg-value (var-id (hcons-scarg qeq)))
		(return (values qeq marg-fvp)))))))))

(defparameter *contained-var-store* nil) 

(defun order-quantifiers (mrsstruct q-rels top-qeq)
  ;;; the quantifiers have had their restrictors fixed.
  ;;; for every quantifier we find the variable it binds
  ;;; and all variables in its restrictor.  We then attempt to find
  ;;; a valid ordering - if we can't, then we can't scope.
  (setf *contained-var-store* nil) 
  ;; prevent loops ...
  (let ((q-var-struct (catch 'vup
			  (loop for q in q-rels
			   collect
			     (let ((bv (get-bv-value q)))
			       (cons q
				     (remove bv 
					     (find-contained-variables 
					      q mrsstruct))))))))
    ;;; (pprint q-var-struct)
    (when q-var-struct
      (let* ((qs (catch 'qup (q-order q-var-struct nil)))
	     (ordered-rels (nreverse qs)))
	(when ordered-rels 
	  (link-q-bodies ordered-rels top-qeq))
;;;      (pprint ordered-rels)
	ordered-rels))))

(defun link-q-bodies (qlist top-qeq)
    (let* ((first-q (car qlist))
	   (first-q-body-fvp
	    (dolist (fvpair (rel-flist first-q))
	      (when (eq (fvpair-feature fvpair) *scope-feat*)
		(return fvpair)))))
      (if (cdr qlist)
	  (let ((next-q-label (rel-handel (cadr qlist))))
	    (setf (fvpair-value first-q-body-fvp)
	      next-q-label)
	    (link-q-bodies (cdr qlist) top-qeq))
	(when top-qeq
	    (setf (fvpair-value first-q-body-fvp)
	      (hcons-outscpd top-qeq))))))
    

(defun q-order (q-var-struct ordering-so-far)
  (let ((to-order nil))
    (dolist (q-s q-var-struct)
      (if (cdr q-s)
	  (push q-s to-order)
	(push (car q-s) ordering-so-far)))
    (unless ordering-so-far
      (throw 'qup nil))
    (if to-order
	(let ((progress nil))
	  (dolist (ordered-q-s ordering-so-far)
	    (let ((bv (get-bv-value ordered-q-s)))
	      (dolist (unordered-q-s to-order)
		(when (member bv (cdr unordered-q-s))
		  (setf progress t)
		  (setf (cdr unordered-q-s) 
		    (remove bv (cdr unordered-q-s)))))))
	  (unless progress
	    (throw 'qup nil))
	  (q-order to-order ordering-so-far))
      ordering-so-far)))
    

(defun get-rels-with-label-num (num mrsstruct)
  (let ((liszt (psoa-liszt mrsstruct)))
    (loop for rel in liszt
      when (equal num (get-rel-handel-num rel))
      collect rel)))

(defun get-normal-vars (rel)
  ;;; returns the non-handle variables from a relation
  (let ((vars nil))
    (dolist (fvp (rel-flist  rel))
      (let ((feature (fvpair-feature fvp)))
	(unless (member feature *scoping-ignored-roles* :test #'eq)
	  (let* ((value (fvpair-value fvp))
		 (id (unbound-var-id value)))
	    (when id
	      (pushnew id
		       vars :test #'equal))))))
    vars))

(defun find-contained-variables (rel mrsstruct)
  (when (member rel *contained-var-store*)
    (struggle-on-error "MRS is not a tree or a quantifier is not free")
    (throw 'vup nil))
  (push rel *contained-var-store*)
  (let ((normal-vars (get-normal-vars rel))
	(hargs (get-full-handel-args rel)))
    (dolist (harg hargs)
      (let* ((harg-num (get-var-num harg))
	     (sc-rels (get-rels-with-label-num harg-num mrsstruct)))
	(dolist (sc-rel sc-rels)
	  (dolist (var (find-contained-variables sc-rel mrsstruct))
	    (pushnew var normal-vars :test #'equal)))))
    normal-vars))
	      

 

