;;; Functions for checking MRS stuctures and outputting
;;; the possible scoped structures in a linearised notation

(in-package "MRS")

;;;
;;; ******** Global variables **********
;;;

;;; set in mrsglobals-eng
(defvar *top-level-rel-types* nil)

;;; obsolete ?
(defvar *top-level-variables* nil
  "the variables which correspond to pronouns or proper names")

;;;
;;; ******** utility functions (some grammar specific) **********
;;;

;;; Utility functions for taking apart MRS structures.
;;; All the functions which rely on particular names for features or rels,
;;; or which identify variable sorts should be in this section

;;; the values of features may be var structures, lists of var structures 
;;; or constants

(defun get-vars-from-value (val)
  ;;; takes a value, which may be a list of variables 
  ;;; or a single variable structure and returns a list of ids
  (if (var-p val)
    (list (var-id val))
    (if 
      (and (listp val) (var-p (car val)))
      (mapcar #'var-id val)
      nil)))

(defun proper-name-or-pronoun-rel (rel-sort)
  (member rel-sort *top-level-rel-types*))


(defun get-handel-args (rel)
  ;;; returns a list of ids of any handels which are values
  ;;; of roles in the rel 
  (for fvp in (rel-flist  rel)
       append 
       (let ((val (fvpair-value fvp)))
         (if (and (var-p val) (is-handel-var val))
           (list (var-id val))
           (if (listp val) 
                 (for val-el in val
                      filter
                      (if (and (var-p val-el) (is-handel-var val-el))
                        (var-id val-el)))
             nil)))))


(defun is-handel-var (var)
    ;;; test is whether the string begins with #\h
  (and (var-p var)
       (eql (elt (var-name var) 0) #\h)))

(defun is-handel-arg-rel (rel)
  ;;; true if the rel has an argument which is a handel
  (dolist (fvp (rel-flist  rel))
    (let ((var (fvpair-value fvp)))
         (when (or (and (var-p var) (is-handel-var var))
                 (and (listp var)
                      (for val-el in var
                           some-satisfy
                           (and (var-p val-el) (is-handel-var val-el)))))
           (return t)))))

(defun nonquantified-var-p (var)
  ;;; true if the string identifier for the variable
  ;;; begins with h or e or t
  (and (var-p var)
       (not (eql (elt (var-name var) 0) '#\x))))

   
(defun is-quant-rel (rel)
  ;;; test is presence of a feature called BV!
  ;;; But also avoid including any "quantifiers" which are top-level rels,
  ;;; including "the" (_def_rel) and demonstratives
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
  (let ((quant-vars nil))
    (for rel in quant-rels
         do
         (let ((quant-var (get-bv-value rel)))
;;; WK: removed code for VM-system: avoid generation of "errors           "
;;;           (unless quant-var 
;;;             (error "~%Rel ~A has an uninstantiated bound variable"
;;;                    (rel-sort rel)))
;;;           (when (member quant-var quant-vars)
;;;             (error "~%Rel ~A has a duplicate bound variable"
;;;                    (rel-sort rel)))
           (if quant-var
               (push quant-var quant-vars))))
    (remove-if #'(lambda (value)
                   (or (member (get-var-num value) quant-vars)
                       (member (get-var-num value) *top-level-variables*)))
               (for rel in rels
                    append
                    (let ((rel-vars (collect-vars-from-rel rel)))
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
           (list value)
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
  (for fvp in (rel-flist  rel)
       append 
       (unless (eql (fvpair-feature fvp) *bv-feature*)
                (let ((value (fvpair-value fvp)))
                  (for el in (if (listp value) value (list value))
                       filter
                       (if (and (var-p el)
                                (not (nonquantified-var-p el))
                                (not (member (get-var-num el) 
                                             *top-level-variables*)))
                         (get-var-num el)))))))


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
              (check-mrs-struct sentence mrsstruct)))))

(defvar *canonical-bindings* nil
"global variable which is set to the current set of bindings for the
printing routines -  convenient to make this global to keep printing generic")

(defun check-mrs-struct (sentence mrsstruct)
  ;;; first output the existing structure in the indexed notation
  (setf *canonical-bindings* nil)
  (format t "~%~A" sentence)
    (format t "~%FSs")
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


(defun generate-top-bindings (handel rels existing-bindings)
  ;;; this is called when all the rels should
  ;;; have handels which are equal to eachother, and to handel
  (let* ((rel-handels (remove-duplicates 
                       (for rel in rels collect (get-var-num (rel-handel rel)))))
         (new-equality (union (list handel) rel-handels)))
    (for binding in existing-bindings
         collect
           (cons (car binding) 
                 (if (member (car binding) new-equality)
                   (copy-list new-equality)
                   (copy-list (cdr binding)))))))
;;; all this copying may well be unnecessary
;;; but let's be safe for now

(defun adjust-bindings (bindings hole label)
  (for binding in bindings
         collect
           (cons (car binding) 
                 (if (eql (car binding) hole)
                   (cons label (copy-list (cdr binding)))
                   (if (eql (car binding) label) 
                     (cons hole (copy-list (cdr binding)))
                     (copy-list (cdr binding)))))))

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
;;; WK: removed code for VM: don't generate errors
;;;           (unless (is-handel-var var)
;;;             (error "~%Relation ~A has incorrect handel ~A"
;;;                    (rel-sort rel) var))
           (pushnew (get-var-num var) labels) 
           (for handel-var in (get-handel-args rel)
                do
                (pushnew handel-var holes))))
    (pushnew top-handel holes) ;; this may be wrong, given the use of prpstn etc
    (process-hcons hcons labels holes)
    (for handel in (remove-duplicates (append labels holes))
       collect
       (list handel handel))))


(defun get-bindings-for-handel (handel bindings)
  (let ((hb (find handel bindings :key #'car)))
;;;    (unless hb
;;;      (error "Handel ~A not in bindings" handel))
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
      (progn (format t "~%Free variables in MRS: ~A" free-variables)
             nil)
;;; variables must be bound by quantifiers unless they are in relations
;;; which license implicit existential binding
;;; Note that, as far as I can tell, MRS does not have the equivalent
;;; of VIT `free labels'
      (let ((top-handel (get-var-num (psoa-top-h mrsstruct))))
          (for result in (create-scoped-structures top-handel nil 
                                 (construct-initial-bindings 
                                  (psoa-handel mrsstruct) 
                                  ; modified - now passes whole var instead of just
                                  ; id
                                  rels hcons)
                                 rels nil)
               filter
               ;;; the bindings slot of the result is a bindings set, the
               ;;; other-rels is a set of `left over' rels
               ;;; we don't want any of these at the top level
               (unless (res-struct-other-rels result) 
                 (res-struct-bindings result)))))) )

(defstruct bindings-and-sisters
  bindings sisters)
;;; similarly, this structure is just for temporary storage of results

(defun create-scoped-structures (top-handel bvs bindings rels scoping-handels)
  ;;; we have a top-handel (i.e. a current `top-handel' - this function
  ;;; is called recursively) and a set of rels, some of which may have the
  ;;; same handel as the top-handel.  We have to generate all possibilities
  ;;; for combinations of rels which COULD have the same handel
  ;;; bindings - current bindings
  ;;; bvs - a list of the variables which are bound by quantifiers we've
  ;;;       already dealt with (nil on first call)
  ;;; rels - rels we haven't incorporated yet (all rels on first call)
  ;;; scoping-handels - handels which will outscope the handels we're
  ;;;        about to incorporate (nil on first call) - added for outscopes
  ;;;        constraint
  (if rels
    ; fail immediately if we're going to be left with
    ; an unsatisfiable handel arg
    (let* ((handel-args (for rel in rels
                                    append (get-handel-args rel)))
           (impossible-top-rels 
            ; the list of rels which introduce variables which are
            ; currently free or which have a handel which is coindexed with a handel
            ; argument of one of the rels
            ; or which are `outscoped' by top-handel
            ; or which are outscoped by a handel which is not on the list
            ; of scoping-handels
            (for rel in rels
                 filter 
                 (let ((vars (collect-unbound-vars-from-rel rel)))
                   (if (or (and vars (not (subsetp vars bvs)))
                           (member (get-var-num (rel-handel rel)) handel-args)
                           (violates-outscopes-p  (get-var-num (rel-handel rel)) 
                                                  top-handel scoping-handels bindings))
                     ; violates-outscopes-p is in mrscons.lsp
                     rel))))
           (impossible-handels 
            (remove-duplicates (for rel in impossible-top-rels
                                    collect (get-var-num (rel-handel rel)))))
           (possible-top-rels 
            ; the list of rels which wouldn't introduce any free variables
            ; and which don't have sisters (i.e. rels with the same handel)
            ; which introduce free variables
            (for rel in (set-difference rels impossible-top-rels)
                 filter 
                 (unless (member (get-var-num (rel-handel rel)) impossible-handels)
                   rel)))
           (top-rels ; the list of rels which have the same handel as the
                     ; top handel
            (for rel in rels
                          filter (if (eql (get-var-num (rel-handel rel)) top-handel)
                                   rel))))
      (if (and possible-top-rels (subsetp top-rels possible-top-rels))
        ; unless there are some possible rels and all the top rels
        ; are possible, we fail
        (for rel-comb-and-bindings in 
             (make-combinations-with-is-one-ofs top-handel top-rels
                           (set-difference possible-top-rels top-rels) 
                           ;; if this third argument is set to nil
                           ;; when there are top-rels, then 
                           ;; no rels can be added to
                           ;; an existing set of relations 
                           ;; which are known to fill a hole
                           bindings)
             ;;; for each possible set of rels which can have their handels
             ;;; bound to the top-handel, generate a set of results
             append
             (sister-structure-scope 
              (car (bindings-and-sisters-sisters rel-comb-and-bindings))
              (cdr (bindings-and-sisters-sisters rel-comb-and-bindings)) 
              bvs 
              (bindings-and-sisters-bindings rel-comb-and-bindings) 
              (set-difference rels 
                              (bindings-and-sisters-sisters rel-comb-and-bindings))
              (cons top-handel scoping-handels)))))))

(defun make-combinations-with-is-one-ofs  (top-handel known-top-rels other-possibles bindings)
  (let ((is-one-ofs (get-is-one-ofs top-handel)))
    (if is-one-ofs
      (for is-one-of in is-one-ofs
           append
           (let ((matching-rels  
                  (for rel in other-possibles
                          filter 
                          (if (eql (get-var-num (rel-handel rel)) is-one-of)
                                   rel))))
             (if matching-rels
               (make-combinations-of-top-rels top-handel 
                                              (append known-top-rels matching-rels)
                                              (set-difference other-possibles matching-rels)
                                          (adjust-bindings bindings top-handel is-one-of)))))
      (make-combinations-of-top-rels top-handel known-top-rels other-possibles bindings))))
      

(defun make-combinations-of-top-rels  (top-handel known-top-rels other-possibles bindings)
  ;;; we need all possible combinations of rels which include the known 
  ;;; top rels
    (let ((other-combinations (generate-combinations other-possibles)))
      ;;; other-combinations is is a list of sets of rels including the empty set
      (for other-combination in other-combinations
           filter
           (let* ((combined-rels (append other-combination known-top-rels))
                  (new-bindings (generate-top-bindings top-handel combined-rels bindings)))
             (if (and new-bindings combined-rels)
               ;;; combined rels might be nil in the case where there are 
               ;;; no known-top-rels
               (make-bindings-and-sisters :sisters combined-rels 
                                          :bindings new-bindings))))))

;;; ******* Traversing the current leaves of the tree ********

(defun sister-structure-scope (first-rel top-rels bvs bindings other-rels scoping-handels)
 (if (is-handel-arg-rel first-rel) 
   (let* ((new-bvs (if (is-quant-rel first-rel) 
                     (cons (get-bv-value first-rel) bvs)
                     bvs))
          (handel-args (get-handel-args first-rel))
          (full-results (sister-args-scope handel-args new-bvs bindings other-rels
                                           scoping-handels)))
     (if full-results
       (if top-rels
         (for res in full-results
              append
              (sister-structure-scope (car top-rels) (cdr top-rels)
                                      bvs (res-struct-bindings res) 
                                      (res-struct-other-rels res)
                                      scoping-handels))
         full-results)))
   (if top-rels
     (sister-structure-scope (car top-rels) (cdr top-rels)
                                      bvs bindings other-rels scoping-handels)
     (list (make-res-struct :bindings bindings :other-rels other-rels)))))


(defun sister-args-scope (top-handels bvs bindings other-rels scoping-handels)
  ;;; this is necessary to deal with the case of relations which have
  ;;; multiple handle arguments
  (let ((results (create-scoped-structures (car top-handels) bvs
                                                        bindings other-rels scoping-handels)))
    (if (cdr top-handels)
      (for res in results
              append
              (sister-args-scope (cdr top-handels) bvs
                                      (res-struct-bindings res) 
                                      (res-struct-other-rels res)
                                      scoping-handels))
      results)))

;;; **********  Generating all possible relation combinastions **********                                                       

(defun generate-combinations (lst-of-rels)
  ;;; we first group the rels into clusters which share
  ;;; a handel, then generate all combinations of these
  ;;; clusters, then convert each of the results back into a 
  ;;; flat list
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
    (if top-rels 
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
         (let ((var (fvpair-value feat-val)))
           (if (listp var)
             (progn 
               (format stream "(")
               (for val in var
                    do
                    (if (is-handel-var val)
                      (output-scoped-rels (get-true-var-num val) rel-list stream)
                      (format stream "~A" (remove-variable-junk 
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




    
  


    
    