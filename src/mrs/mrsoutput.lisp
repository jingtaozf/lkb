;;; Copyright (c) 1998--2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

;;; Creating MRS structures from results of parse
;;;
;;; Outputting MRS structures
;;;
;;; Ann Copestake (2000) - radically new cleaned-up version 
;;; removing VM clutter and historical notes

;;; now requires basemrs.lisp for structures and printing
;;; requires mrsglobals.lisp for global variables for paths etc.

(in-package :mrs)

;;; Assumptions about the grammar
;;;
;;; 1. fragmentness is indicated by *root-path* not being
;;;    *true-type*
;;;
;;; 2. There is a single `semantic' feature structure
;;; which can be used to build the MRS at the end of
;;;  *initial-semantics-path*
;;;
;;; 3. *psoa-index-path* - variable 
;;;    *psoa-liszt-path* - list of relations
;;;
;;; 4. *psoa-top-h-path* - handle variable 
;;;    *psoa-rh-cons-path* - hcons structure
;;;    not in handle-free fragment
;;;
;;; (5. *psoa-mode-path*   - mode (optional)
;;;    *psoa-info-s-path* - information structure (optional)
;;;    both removed)
;;;
;;; 6. variables
;;;    a. variable equivalence is indicated by structure sharing
;;;    (assumed to mean same lisp structure - see *named-nodes*)
;;;    b. determine-variable-type controls the letter used
;;;    to type the variable - see below
;;;    c. `extra' information associated with a variable is
;;;    built using create-index-property-list
;;;    all extra information must be represented by a path
;;;    with an atomic value terminating it
;;;    all features are valid as extras, except those which are in 
;;;    *ignored-extra-features*
;;;
;;; 7. liszt and relations
;;;    a. represented via a first/rest structure (*first-path* and
;;;    *rest-path*)
;;;    b. relation handel - *rel-handel-path* (not for hfree MRS)
;;;    c. relation predicate is either 
;;;       value of first element of *rel-name-path* 
;;;                  OR, if this doesn't exist
;;;       type of fs
;;;    d. arguments represented by features
;;;       (features ignored if member of *ignored-sem-features*)
;;;       if they are members of *value-feats* then assumed
;;;       to have a constant value - otherwise variable
;;;       features are ordered by *feat-priority-list*
;;;       this is relevant for the feature-free MRS rep
;;;
;;; 8. hcons
;;;    hcons is a list - relationship is represented by the
;;;    type of the list element - two arguments, represented
;;;    by *sc-arg-feature* and *outscpd-feature*
;;;
;;;    also allow for isect case: where there's a modifier anchor
;;;    consisting of a handle.index pair and a list of target pairs
;;;    interpreted as a disjunction.
;;;
;;;    anything else is ignored

;;; ********************************************************
;;;
;;; Entry points and globals
;;;
;;; First necessary to retrieve the structure from the result of
;;; a parse.  The FS returned will have an initial path to get to
;;; the MRS *initial-semantics-path*
;;; Following this gets you to a psoa structure
;;; 

(defun extract-mrs (parse)
  ;;; takes whatever structure the parser returns
  (let ((fs (get-parse-fs parse))) ;; get-parse-fs is system specific
    (when (is-valid-fs fs)
      (extract-mrs-from-fs fs))))
         
(defun extract-mrs-from-fs (fs)
  (let ((sem-fs (path-value fs *initial-semantics-path*)))
    (setf *fragment-p* (is-fragment-fs fs))
    ;; *fragment-p* controls whether the scoping code is run
    (if (is-valid-fs sem-fs)
        (construct-mrs sem-fs nil))))

(defun is-fragment-fs (fs)
  ;;; grammar specific
  (and *root-path* *true-type*
       (let ((root-value (path-value fs *root-path*)))
         (if root-value 
             (not
              (eql (fs-type root-value) *true-type*))))))


(defparameter *named-nodes* nil
  "an alist so that if a feature structure representing a variable is shared, the same variable will be used each time it is encountered")

;;
;; in LOGON MT mode, we use MRS variables as meta-level variables in transfer
;; rules, where the conversion code will have created variables before going
;; into construct-mrs(); `standard' calls are not affected.    (27-jan-04; oe)
;;
(defparameter *mrs-record-all-nodes-p* nil)

(defparameter *all-nodes* nil)

(defun lookup-mtr-node (dag)
  (rest (assoc dag *all-nodes*)))

;;; variables get unique names via the variable generator
;;; which can be passed as a parameter

(defvar *restart-variable-generator* t
  "if t the variable counter is restarted for each sentence
This is set to nil for work on discourse to avoid spurious 
duplicate variables")

(defun construct-mrs (fs &optional existing-variable-generator)
  (if existing-variable-generator
    (setf *variable-generator* existing-variable-generator)
    (when *restart-variable-generator* (init-variable-generator)))
  (unless existing-variable-generator 
    (setf *named-nodes* nil)
    (setf *all-nodes* nil))
  (let* ((top-h-fs (when *psoa-top-h-path*
                     (path-value fs *psoa-top-h-path*)))
         (index-fs (path-value fs *psoa-index-path*))
         (liszt-fs (path-value fs *psoa-liszt-path*))
         (h-cons-fs (when *psoa-rh-cons-path*
                      (path-value fs *psoa-rh-cons-path*)))
	 (a-cons-fs (when *psoa-a-cons-path*
                      (path-value fs *psoa-a-cons-path*)))
	 (ing-fs (path-value fs (list (vsym 'ING) (vsym 'LIST))))
	 ;;; FIX - hardwired names
	 (ing (when ing-fs (construct-ing ing-fs nil *variable-generator*)))
         (psoa
          (make-psoa
           :top-h (if top-h-fs
                    (create-variable top-h-fs *variable-generator*)
                    (when *rel-handel-path*
                      (create-new-handle-var *variable-generator*)))
           :index (when (is-valid-fs index-fs)
                    (create-variable index-fs *variable-generator*))
           :liszt (nreverse (construct-liszt 
                             liszt-fs nil *variable-generator*))
           :h-cons (nreverse (construct-h-cons 
                              h-cons-fs nil *variable-generator*))
           :a-cons (nreverse (construct-a-cons
                              a-cons-fs nil *variable-generator*))))
	 (psoa (if ing (convert-ing-to-ing-rels psoa ing nil)
		 psoa))
         (psoa (mt:map-mrs psoa :semi :forward))
         #-:logon
         (psoa (unfill-mrs psoa)))
    (when *mrs-record-all-nodes-p* (push (cons fs psoa) *all-nodes*))
    psoa))




;;; ***************************************************
;;;
;;; Variables
;;;
;;; ***************************************************


(defun get-var-num (var-struct)
  ;; Allow NIL argument
  (when (var-p var-struct)
    (var-id var-struct)))

(defun get-existing-variable (fs)
  (when (is-valid-fs fs)
    (let ((existing-variable (assoc fs *named-nodes*)))
      (if existing-variable (cdr existing-variable)
        nil))))

(defun create-new-handle-var (gen)
  (let* ((idnumber (funcall gen)))
    (make-var 
     :type "h"
     :id idnumber)))

(defun create-variable (fs gen)
  (when (is-valid-fs fs)
    (let ((existing-variable (assoc fs *named-nodes*)))
      (if existing-variable
        (cdr existing-variable)
        (let* ((idnumber (funcall gen))
               (var-type (determine-variable-type fs))
               (extra (create-index-property-list fs))
               (variable-identifier (make-var :type var-type
                                              :extra extra 
                                              :id idnumber)))
          (push (cons fs variable-identifier) *named-nodes*)
          (when *mrs-record-all-nodes-p*
            (push (cons fs variable-identifier) *all-nodes*))
          variable-identifier)))))

(defun create-indexing-variable (fs)
  ;;; this is called when we are building an mrs structure for indexing
  (when (is-valid-fs fs)
    (let* ((var-type (determine-variable-type fs))
           (extra (create-index-property-list fs)))
          (make-var :type var-type
		    :extra extra 
		    :id :dummy))))

;;; The `extra' information on variables is represented
;;; as a structure consisting of a combination of feature
;;; (possibly consisting of a composite structure created
;;; by interposing `.' between features)
;;; and an atomic value.  We do not maintain
;;; intermediate types on the path - all relevant information
;;; must be contained in the atomic types

(defun create-index-property-list (fs &optional path-so-far)
  (when (is-valid-fs fs)
    (setf fs (deref fs)))
  (if (is-valid-fs fs)
      (let ((label-list (fs-arcs fs)))
        (if (and label-list (consp label-list))
          (loop for feat-val in label-list
               append
               (let ((new-path (cons (car feat-val) path-so-far))
                     (next-fs (cdr feat-val)))
                 (unless (member (car feat-val) *ignored-extra-features*)
                   (create-index-property-list 
                    next-fs
                    new-path))))
          (if path-so-far
              (list
               (make-extrapair 
                :feature (make-mrs-feature (reverse path-so-far))
                :value (create-type 
                        (fs-type fs)))))))))

(defun make-mrs-feature (flist)
  (if (cdr flist)
      (intern (apply #'concatenate 'string
                     (string (car flist))
                     (mapcan #'(lambda (f) (list "." (string f))) (cdr flist)))
              *mrs-package*)
    (car flist)))


;;; ****************************************************
;;;
;;; LISZT construction
;;;
;;; ****************************************************


(defun construct-liszt (fs rels-list variable-generator)
  (if (is-valid-fs fs)
        (let ((label-list (fs-arcs fs)))
          (if label-list
              (let ((first-part (assoc (car *first-path*)
                                       label-list))
                    (rest-part (assoc (car *rest-path*)
                                      label-list)))
                (if (and first-part rest-part)
                    (let ((rel-struct
                           (create-rel-struct
                            (cdr first-part)
                            variable-generator nil)))
                      (when rel-struct
                        (push rel-struct
                              rels-list))
                      (construct-liszt
                       (cdr rest-part)
                       rels-list variable-generator))
                  rels-list))
            rels-list))))

;;; defined so it can also be used by the lexicon indexing
;;; code

(defun create-rel-struct (fs variable-generator indexing-p)
  ;;; indexing-p is set if this is being called when we're indexing lexical
  ;;; entries
  (when (is-valid-fs fs)
    (let* ((handel-pair (unless indexing-p
                          (extract-handel-pair-from-rel-fs fs)))
           (handle-var (when handel-pair
                         (create-variable 
                          (cdr handel-pair) variable-generator)))
           (pred (or (when *mrs-record-all-nodes-p*
                       (lookup-mtr-node 
                        (extract-pred-from-rel-fs fs :rawp t)))
                     (create-type (extract-pred-from-rel-fs fs))))
           (original-string (extract-original-string-from-rel-fs fs))
           (fvps (extract-fvps-from-rel-fs fs variable-generator 
                                           indexing-p original-string))
           (parameter-strings (get-fvps-parameter-strings fvps))
           (cfrom (extract-cfrom-from-rel-fs fs))
           (cto (extract-cto-from-rel-fs fs)))
      (unless (member pred *dummy-relations* :test #'equal)
        (let ((ep (make-char-rel 
                   :pred pred
                   :handel handle-var
                   :flist fvps
                   :parameter-strings parameter-strings
                   :str original-string
                   :cfrom cfrom
                   :cto cto)))
          (when *mrs-record-all-nodes-p* (push (cons fs ep) *all-nodes*))
          ep)))))
;;; FIX?? flist may be wrong way round

(defun get-fvps-parameter-strings (fvps)
  (loop for fvp in fvps
        for feat = (fvpair-feature fvp)
        when (member feat *value-feats*)
        collect fvp))

(defun extract-handel-pair-from-rel-fs (fs)
  (let ((label-list (fs-arcs fs)))
    (if *rel-handel-path*
        (assoc (car *rel-handel-path*)
               label-list))))

;;;
;;; we need a (vanilla) version of this even in non-LKB universes, e.g. for the
;;; use of the MRS code stand-alone with ECL (PET) and the XLE.
;;;                                                            (21-feb-05; oe)
(defun extract-original-string-from-rel-fs (fs)
  #-:lkb
  (declare (ignore fs))
  #+:lkb
  (if lkb::*recording-word*
      (let* ((label-list (fs-arcs fs))
             (origstr-fs
              (cdr (assoc lkb::*recording-word*
                          label-list))))
        (if origstr-fs
            (let ((fs-type (fs-type origstr-fs)))
              (if (stringp fs-type)
                  fs-type))))))
  
(defun extract-cfrom-from-rel-fs (fs)
  (let ((label-list (fs-arcs fs)))
    (if *rel-cfrom-feature*
	(let ((cfrom-fs
	       (cdr (assoc *rel-cfrom-feature*
			   label-list))))
	  (or (extract-integer-from-fs-type cfrom-fs)
	       -1)))))

(defun extract-integer-from-fs-type (fs)
  (if fs
      (let ((fs-type (fs-type fs)))
	(if (stringp fs-type)
	    (let ((res
		   (parse-integer fs-type :junk-allowed t)))
	      (if (integerp res)
		  res))))))

(defun extract-cto-from-rel-fs (fs)
  (let ((label-list (fs-arcs fs)))
    (if *rel-cto-feature*
	(let ((cto-fs
	       (cdr (assoc *rel-cto-feature*
			   label-list))))
	  (or (extract-integer-from-fs-type cto-fs)
	       -1)))))

(defun extract-pred-from-rel-fs (rel-fs &key rawp)
    (let* ((label-list (fs-arcs rel-fs))
           (pred (rest (assoc (car *rel-name-path*) label-list)))
           (pred-type (if pred (fs-type pred))))
      (when rawp (return-from extract-pred-from-rel-fs pred))
      (if (and pred-type
               (not (is-top-type pred-type))
               #+:logon
               (not (is-top-semantics-type pred-type)))
        pred-type
        (unless *rel-name-path* (fs-type rel-fs)))))

(defun extract-type-from-rel-fs (rel-fs)
  (fs-type rel-fs))

(defun extract-fvps-from-rel-fs (rel-fs variable-generator indexing-p str)
  (declare (ignore str))
  (let* ((label-list (fs-arcs rel-fs))
         (reduced-list
          (loop for fvp in label-list
              for feature = (car fvp)
              for value = (cdr fvp)
              unless (or (member feature *ignored-sem-features*)
                         (eql feature (car *rel-handel-path*))
                         (eql feature (car *rel-name-path*))
                         #+:lkb(eql feature lkb::*recording-word*))
              collect 
                (make-fvpair :feature feature
                             :value 
                             (or (when *mrs-record-all-nodes-p*
                                   (lookup-mtr-node value))
                                 (if (member feature *value-feats*)
                                     (create-type (fs-type value))
                                   ;; (substitute-ersatz 
                                   ;;  (create-type (fs-type value))
                                   ;;   str)
                                   (if indexing-p
                                     (create-indexing-variable value)   
                                     (create-variable
                                      value
                                      variable-generator))))))))
    (sort reduced-list #'feat-sort-func)))

(defun substitute-ersatz (const-str orig-str)
  ;;; this is a bit of a hack
  ;;; the preprocessor replaces e.g. 1992 with FourDigitErsatz
  ;;; which is a pain for any practical work with (R)MRSs
  ;;; So this code puts back the original string if available
  ;;; if there's an ersatz there
  ;;; This will only work if the parser can see the original 
  ;;; string
  (if (and orig-str (stringp const-str)  (stringp orig-str))
      (let ((const-length (length const-str)))
        (if (and (> const-length 6)
                 (string-equal "ersatz" const-str 
                               :start2 (- const-length 6)))
            orig-str
          const-str))
    const-str))

(defun create-type (sort)
  ;;; currently a no-op but kept
  ;;; because we keep needing to mess around when we create types
  sort)

(defun feat-sort-func (fvp1 fvp2)
  (let ((feat1 (fvpair-feature fvp1))
         (feat2 (fvpair-feature fvp2)))
    (feat-order-func feat1 feat2)))

(defun feat-order-func (feat1 feat2)
  (let ((remlist (member feat1 *feat-priority-list*)))
    (if remlist (or (member feat2 remlist)
                    (not (member feat2 *feat-priority-list*)))
      (unless (member feat2 *feat-priority-list*)
        (string-lessp feat1 feat2)))))


;;; *******************************************************
;;;
;;; HCONS
;;;
;;; *******************************************************

(defun construct-h-cons (fs constr-list variable-generator)
  (if (is-valid-fs fs)
      (let ((label-list (fs-arcs fs)))
        (if label-list
            (let ((first-part (assoc (car *first-path*)
                                     label-list))
                  (rest-part (assoc (car *rest-path*)
                                    label-list)))
              (if (and first-part rest-part)
                  (progn
                    (push (create-constr-struct
                           (cdr first-part)
                           variable-generator)
                          constr-list)
                    (construct-h-cons
                     (cdr rest-part)
                     constr-list variable-generator))
                constr-list))
          constr-list))))


(defun create-constr-struct (fs variable-generator)
  (if (is-valid-fs fs)
      (let* ((label-list (fs-arcs fs))
	     (rel (create-hcons-relation (fs-type fs)))
	     (scarg (assoc  *sc-arg-feature* label-list))
	     (outscpd (assoc *outscpd-feature* label-list)))
	(make-hcons 
	 :relation rel
	 :scarg (when scarg
		  (create-variable (cdr scarg) variable-generator))
	 :outscpd (when outscpd
		    (create-variable (cdr outscpd) 
				     variable-generator))))))

(defun create-hcons-relation (type)
  (cond ((eql type *qeq-type*) "qeq")
        (t (error "Unknown relation type ~A"))))



;;; *******************************************************
;;;
;;; ING (see rmrscomp grammar)
;;;
;;; *******************************************************

;;; extracts INGs from FS
;;; rmrscomp grammar has ING between a label for
;;; every conjunction and each relation's label
;;; including vacuous ones 

(defun construct-ing (fs constr-list variable-generator)
  (if (is-valid-fs fs)
      (let ((label-list (fs-arcs fs)))
        (if label-list
            (let ((first-part (assoc (car *first-path*)
                                     label-list))
                  (rest-part (assoc (car *rest-path*)
                                    label-list)))
              (if (and first-part rest-part)
                  (progn
                    (push (create-ing-struct
                           (cdr first-part)
                           variable-generator)
                          constr-list)
                    (construct-ing
                     (cdr rest-part)
                     constr-list variable-generator))
                constr-list))
          constr-list))))


(defun create-ing-struct (fs variable-generator)
  ;;; FIX names should not be hardwired
  (if (is-valid-fs fs)
      (let* ((label-list (fs-arcs fs))
	     (cnj (assoc (vsym 'CNJ) label-list))
	     (cnjed (assoc (vsym 'CNJED) label-list)))
	(make-in-group 
	 :label-a (when cnj
		    (create-variable (cdr cnj) variable-generator))
	 :label-b (when cnjed
		    (create-variable (cdr cnjed) 
				     variable-generator))))))

;;; this code removes vacuous INGs and converts the rest to
;;; explicit conjunctions

(defun convert-ing-to-ing-rels (mrs ings copy-p)
  (if ings
      (let* ((combined-ings (collect-ings ings))
	     (new-mrs (if copy-p (copy-psoa-completely mrs)
			mrs))
	     (new-relations nil)
	     (bindings nil))
	(dolist (ing-set combined-ings)
	  (if (cddr ing-set)
	    (push 
	     (make-ing-relation (car ing-set)
				(cdr ing-set))
	     new-relations)
	    (push (cons (var-id (car ing-set)) (var-id (cadr ing-set)))
		  bindings)))
	(setf (psoa-liszt new-mrs)
	  (append (psoa-liszt new-mrs) new-relations))
	(when bindings
	  (canonicalise-basemrs new-mrs
				bindings))
	new-mrs)
  mrs))

(defun collect-ings (ings)
  (let ((ing-alist nil))
    (dolist (ing ings)
      (let* ((label-a (in-group-label-a ing))
	     (label-b (in-group-label-b ing))
	     (existing (assoc label-a ing-alist)))
	(if existing
	    (push label-b (cdr existing))
	  (push (cons label-a (list label-b)) ing-alist))))
    ing-alist))

(defun make-ing-relation (label cnjed-list)
  (let ((arg-no 0)
	(flist nil))
    (dolist (cnjed cnjed-list)
      (push (make-fvpair :feature (vsym (format nil "ARG~A" arg-no))
			 :value cnjed)
	    flist)
      (incf arg-no))
    (make-rel :pred "ing_rel"
	      :handel label
	      :flist (nreverse flist))))

;;; the following (which should be moved to another file at some point,
;;; if it's kept) sticks an RMRS style quantifier back together - 
;;; just to test scoping with RMRSs

(defun remerge-quant-rels (mrs)
  (let ((qlbls nil)
	(norm-rels nil)
	(qrel-alist nil))
    (dolist (rel (psoa-liszt mrs))
      (when (eql (rel-pred rel) (vsym 'rstr))
	(push (rel-handel rel)
	      qlbls)))
    (dolist (rel (psoa-liszt mrs))
      (if (member (rel-handel rel) qlbls)
	  (let ((existing (assoc (rel-handel rel) qrel-alist)))
	    (if existing (push rel (cdr existing))
	      (push (cons (rel-handel rel) (list rel))
		    qrel-alist)))
	(push rel norm-rels)))
    (let ((new-mrs
	   (copy-psoa-completely mrs)))
      (setf (psoa-liszt new-mrs)
	(append norm-rels
		(loop for qrec in qrel-alist
		    collect
		      (make-mrs-style-qrel qrec))))
      new-mrs)))

(defun make-mrs-style-qrel (qrec)
  (let* ((qrels (cdr qrec))
	 (rstr (find (vsym 'rstr) qrels :key #'rel-pred))
	 (body (find (vsym 'body) qrels :key #'rel-pred))
	 (remainder (remove body (remove rstr qrels))))
    (unless (and rstr body remainder (not (cdr remainder)))
      (error "Unexpected quantifier structures"))
    (let ((rstr-hole (fvpair-value (car (rel-flist rstr))))
	  (body-hole (fvpair-value (car (rel-flist body)))))
	  (make-rel :pred (rel-pred (car remainder))
		    :handel (car qrec)
		    :flist (append
			    (rel-flist (car remainder))
			    (list 
			    (make-fvpair :feature (vsym 'rstr)
					 :value rstr-hole)
			    (make-fvpair :feature (vsym 'body)
					 :value body-hole)))))))
	
;;; *******************************************************
;;;
;;; ACONS (attachment constraints - experimental, for Berthold
;;;
;;; *******************************************************

#|
  ACONS - is a diff-list of structures 

  [ isect-mod
  MOD-ANC [ index-lbl-pair
            INDEX index
	    LTOP handle ]
  TARGET-ANCS <! [ index-lbl-pair
                   INDEX index
                   LTOP handle ] ... !> ]

in mrsglobals.lsp

*mod-spec-type* - "isect-mod"
  "the fs type associated with a modifier attachment relation"

*mod-anc* - "mod-anc"
  "the feature in the mod spec that leads to the modifier anchor")

*target-ancs* - "target-ancs"
"the feature in the mod spec that leads to the target list"

the mod-anc is an index-lbl-pair - the target-ancs are a diff list of these

*index-path* 

*ltop-path*
|#


(defun construct-a-cons (fs constr-list variable-generator)
  (if (is-valid-fs fs)
      (let ((label-list (fs-arcs fs)))
        (if label-list
            (let ((first-part (assoc (car *first-path*)
                                     label-list))
                  (rest-part (assoc (car *rest-path*)
                                    label-list)))
              (if (and first-part rest-part)
		  (let ((acons-str 
			 (create-aconstr-struct
			  (cdr first-part)
			  variable-generator)))
		    (when acons-str
		      (push acons-str constr-list)
		      (construct-a-cons
		       (cdr rest-part)
		       constr-list variable-generator)))
                constr-list))
          constr-list))))


(defun create-aconstr-struct (fs variable-generator)  
  (if (is-valid-fs fs)
      (let ((label-list (fs-arcs fs))
	    (cons-type (fs-type fs)))
	(if (eql cons-type *mod-spec-type*)
	    (let* ((mod-anc-fs (assoc *mod-anc* label-list))
		   (mod-anc-struct 
		    (create-index-lbl-pair (cdr mod-anc-fs) variable-generator))
		   (target-anc-fs (path-value fs 
					       *target-ancs-path*))
		   (target-anc-list 
		    (create-target-anc-list target-anc-fs 
					    nil variable-generator)))
	      (if (and mod-anc-struct target-anc-list)
		  (make-disj-cons 
		   :index-lbl mod-anc-struct
		   :target target-anc-list)))))))

(defun create-index-lbl-pair (fs variable-generator)
  (if (is-valid-fs fs)
      (let* ((label-list (fs-arcs fs))
	     (index (assoc (car *index-path*) label-list))
	     (lbl (assoc (car *ltop-path*) label-list)))
	(if (and index lbl)
	    (make-index-lbl 
	     :index (create-variable (cdr index) variable-generator)
	     :lbl (create-variable (cdr lbl) 
				   variable-generator))))))

(defun create-target-anc-list (fs constr-list variable-generator)
  (if (is-valid-fs fs)
      (let ((label-list (fs-arcs fs)))
        (if label-list
            (let ((first-part (assoc (car *first-path*)
                                     label-list))
                  (rest-part (assoc (car *rest-path*)
                                    label-list)))
              (if (and first-part rest-part)
                  (progn
                    (push (create-index-lbl-pair
                           (cdr first-part)
                           variable-generator)
                          constr-list)
                    (create-target-anc-list
                     (cdr rest-part)
                     constr-list variable-generator))
                constr-list))
          constr-list))))



;;; ************************************************************
;;;
;;; Other bits
;;;
;;; ************************************************************

(defun determine-variable-type (fs)
  (let ((type (create-type (fs-type fs))))
    (cond ((equal-or-subtype type *event-type*) "e")
          ((equal-or-subtype type *ref-ind-type*) "x")
          ((equal-or-subtype type *non_expl-ind-type*) "i")
          ((equal-or-subtype type *deg-ind-type*) "d")
          ((equal-or-subtype type *handle-type*) "h")  
          ((equal-or-subtype type *non_event-type*) "p")
          ((equal-or-subtype type *event_or_index-type*) "i")
          (t "u"))))

(defun ep-shorthand (ep)
  ;;
  ;; return compact short-hand representation; primarily for error messages.
  ;;
  (when (rel-p ep)
    (let ((pred (rel-pred ep))
          (parameters (get-rel-parameter-strings ep)))
      (format 
       nil 
       "~:[~(~a~)~;~s~]~@[(~{~s~^, ~})~]"
       (stringp pred) pred 
       (loop
           for parameter in parameters
           collect (fvpair-value parameter))))))
