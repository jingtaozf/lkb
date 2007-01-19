;;; Copyright (c) 2007
;;; Ann Copestake
;;; see `licence.txt' for conditions.

;;; Composition via algebra for RMRS
;;; This is (partly) and alternative to the code in comp.lisp and may
;;; be moved there eventually.  It uses the same approach to
;;; variable bindings, but differs in applying the algebra `properly'.
;;; There is code here to convert the XML rule formalism to the algebra
;;; and also code to convert the new compact formalism to the algebra.

;;; sement structures and print code are in basemrs.lisp

(in-package :mrs)

;;; note that this code is `clean': we assume that all messiness due
;;; to missing/incomplete rules is dealt with earlier.

;;; FIX - just for me!
(declaim (optimize (safety 3)))

(defstruct rmrs-algebra-rule 
  name
  condition
  dtrs
  arity
  head
  semstruct
  application-order)

(defvar *algebra-rule-instructions* nil)

(defstruct rmrs-algebra-tag 
  name
  semstruct)

(defvar *algebra-tag-instructions* nil)

(defun algebra-compose (rule-name dtrs)
  ;;; compose takes a rule name and a list of daughters
  ;;; the rule name is looked up in *algebra-rule-instructions*
  (unless dtrs ;;; really there always ought to be dtrs
             ;;; but this prevents errors on trees with just a rule name
    (error "~% defective tree"))
  (let* ((dtr-features (loop for dtr in dtrs
			   append (semstruct-features dtr)))
	 (rule-instruction (lookup-algebra-instructions rule-name 
							dtrs dtr-features))
	 ;; pass dtrs so we can make an appropriate default rule if needed
	 (dtr-hooks (loop for dtr in dtrs
			  collect (semstruct-hook dtr)))
	 (dtr-eps (loop for dtr in dtrs
			append (semstruct-liszt dtr)))
	 (dtr-rargs (loop for dtr in dtrs
			  append (semstruct-rmrs-args dtr)))
	 ;; in-gs gone
	 (dtr-hcons (loop for dtr in dtrs
			  append (semstruct-h-cons dtr)))
	 (dtr-binding-list 
	  (loop for dtr in dtrs
		append (semstruct-bindings dtr)))
	 (cfrom (calculate-cfrom-from-daughters dtrs))
	 (cto (calculate-cto-from-daughters dtrs))
	 (uninstantiated-semstruct 
	  (if rule-instruction 
		   (rmrs-algebra-rule-semstruct rule-instruction)))
	 (rule-features 
	  (if uninstantiated-semstruct 
	      (semstruct-features uninstantiated-semstruct)))
	 (rule-semstruct 
	  (if uninstantiated-semstruct 
	      (instantiate-new-semstruct
	       uninstantiated-semstruct 
	       cfrom cto)))
	 (result nil))
         ;; instantiate-new-semstruct creates a new copy of the rule semstruct
         ;; replacing the variables
    ;; cfrom cto are added to construction relations
    (unless rule-instruction
      (error "Failure to construct rule for ~A" rule-name))
    (setf result 
      (make-semstruct 
       :hook (semstruct-hook rule-semstruct)
       :features (append rule-features dtr-features)
       :liszt (append (semstruct-liszt rule-semstruct) dtr-eps)
       :rmrs-args (append (semstruct-rmrs-args rule-semstruct) dtr-rargs)
       :h-cons  (append (semstruct-h-cons rule-semstruct) dtr-hcons)
       :cfrom cfrom
       :cto cto
       :bindings
       (append
	(apply-algebra-instructions
	 rule-instruction 
	 rule-semstruct
	 dtr-hooks)
	(semstruct-bindings rule-semstruct)
	dtr-binding-list)))
    (when *trace-rmrs-composition-p*
      (format t "~%Applying rule ~A~%" rule-name)
      (unless rule-instruction
	(format t " (not found)"))
      ;;;(dolist (dtr dtrs)  
	;;;(internal-output-rmrs dtr 'vcompact t t))
      (internal-output-rmrs result 'vcompact t t))
    result))


(defun lookup-algebra-instructions (rule-name dtrs dtr-features)
  ;;; FIX - conversion just used in the short term
  (let* ((rule (lookup-instruction rule-name dtr-features))
	 (converted-rule (if rule
			     (convert-rmrs-rule rule))))
    #|
    (when (and converted-rule
	       (not (eql (rmrs-algebra-rule-arity converted-rule) 
			 (length dtrs))))
      (setf converted-rule 
      (rescue-incorrect-arity converted-rule dtrs))
      |#
    (unless (and converted-rule
		 (eql (rmrs-algebra-rule-arity converted-rule) 
			 (length dtrs)))
      (setf converted-rule
	(default-rule-creation rule-name dtrs)))
	;;; don't save a rule that's been created on the fly
	;;; RASP seems to use things like T/frag with varying arity
    converted-rule))

(defun add-rmrs-algebra-rule (rule)
  (let* ((rule-name (rmrs-rule-name rule))
	 (rule-set (find rule-name *algebra-rule-instructions*
				 :test #'equal :key #'rmrs-rule-set-name)))
    (if rule-set
      (setf (rmrs-rule-set-alternatives rule-set)
	(if (rmrs-algebra-rule-condition rule)
	    (append (rmrs-rule-set-alternatives rule-set) (list rule))
	  (cons rule (rmrs-rule-set-alternatives rule-set))))
      (push (make-rmrs-rule-set :name rule-name
				:alternatives (list rule)) 
	    *algebra-rule-instructions*))))

(defun default-rule-creation (rule-name dtrs)
  ;;; a default rule has a hook equal to a dtr slot and 
  ;;; equates the labels of the dtrs.  Obviously this is
  ;;; sometimes incorrect, but for some applications
  ;;; this is a better option than letting them all be unattached.
  (make-rmrs-algebra-rule 
   :name rule-name
   :dtrs dtrs
   :arity (length dtrs)
   :semstruct (default-semstruct-creation dtrs)
   :application-order
   (loop for x from 0 to (- (length dtrs) 1) collect x)
   ))

(defun default-semstruct-creation (dtrs)
  (let* ((shared-label (construct-grammar-var "H"))
	 (head-hook (make-hook :index (construct-grammar-var "X")
			       :ltop shared-label
			       :anchor (construct-grammar-var "H1"))))
    (make-semstruct 
     :hook head-hook
     :slots (cons (make-slot :name 0
			     :hook head-hook)
		  (if (cdr dtrs)
		      (loop for count from 1 to (- (length dtrs) 1) 
			  collect
			    (make-slot 
			     :name count
			     :hook (make-hook :ltop shared-label))))))))
   

;;; semantic head is a notion that applies when the algebra instructions are
;;; created.  The hook of the result is the hook of the semstruct that
;;; has acted as the functor in the `highest' application, which will always
;;; be the hook of the rule-semstruct for RASP-RMRS.  If the rule-semstruct
;;; introduces an ep, the hook will have to be specified (the ltop will
;;; correspond to the label of the ep, but the variable is less
;;; straightforward).  If the rule-semstruct doesn't contain an EP
;;; the rule hook will be the hook of the head dtr.

;;; since this code is specific for the RASP-RMRS case, there are
;;; no slots associated with the daughters.  The slots in the rule
;;; are independent of eachother and do not need to be renamed.
;;; We always have an application structure 
;;; (((rule dtr-a) dtr-b) dtr-c)
;;; thus the rule instruction is simplified to a list of daughters 
;;; in their application order 
;;; e.g., (0 1)
;;; the result is actually independent of order of application
;;; but it's useful to make the application order deterministic for
;;; debugging, at least. 

(defun apply-algebra-instructions (rule-instruction rule-semstruct dtr-hooks)
  (let ((slots (semstruct-slots rule-semstruct)))
    (loop for dtr in (rmrs-algebra-rule-application-order rule-instruction)
	nconc
	  (let ((dtr-hook (elt dtr-hooks dtr)))
	    (if dtr-hook
		(let ((slot (find dtr slots :key #'slot-name)))
		  (if slot
		      (apply-algebra-op slot dtr-hook))))))))
      

(defun apply-algebra-op (slot hook)
  ;;; applying an algebra operation is always just a matter of
  ;;; equating a slot (as given by op) with the hook
  ;;; and returning the new equalities. With this version, we have
  ;;; created new variable names for each semstruct so we just need to
  ;;; record bindings.
  ;;; index, xarg, ltop, anchor
  (let ((equalities nil)
	(slot-hook (slot-hook slot)))
    (when (and (hook-index hook) (hook-index slot-hook))
      (push (list (hook-index hook) (hook-index slot-hook))
	    equalities))
    (when (and (hook-xarg hook) (hook-xarg slot-hook))
      (push (list (hook-xarg hook) (hook-xarg slot-hook))
	    equalities))
    (when (and (hook-ltop hook) (hook-ltop slot-hook))
      (push (list (hook-ltop hook) (hook-ltop slot-hook))
	    equalities))
    (when (and (hook-anchor hook) (hook-anchor slot-hook))
      (push (list (hook-anchor hook) (hook-anchor slot-hook))
	    equalities))
    equalities))

(defun instantiate-tag-semstruct (semstruct cfrom cto lex)
  ;;; copies the semstruct, and replaces the variables
  ;;; via the same fn as used for the rules, and then replaces the lexically
  ;;; instantiated stuff appropriately
  (let ((new-semstruct (instantiate-new-semstruct semstruct cfrom cto)))
    (loop for ep in (semstruct-liszt new-semstruct)
	do
	  (let ((old-pred (rel-pred ep)))
	    (when (dummy-pred-p old-pred)
	      (setf (rel-pred ep)
		(make-realpred 
		 :lemma (string-downcase (word-info-lemma lex))
		 :pos (word-info-pos lex))))
	    (setf (char-rel-str ep)
		(if (word-info-p lex)
		    (word-info-original lex)))))
    (loop for rarg in (semstruct-rmrs-args new-semstruct)
         do
	  (let ((val (rmrs-arg-val rarg)))
	    (when (dummy-constant-p val)
	      (setf (rmrs-arg-val rarg)
		(string-downcase (word-info-lemma lex))))))
					; for constants in names etc
					; downcased, but may revisit
		                        ; this
    new-semstruct))


(defun instantiate-new-semstruct (semstruct cfrom cto)
  ;;; copies the semstruct, replacing variables with new ones
  ;;;
  ;;; we don't need to worry about creating new variables
  ;;; in the case where there aren't any present, as long as they aren't 
  ;;; labels or anchors associated with relations.  If they are,
  ;;; we create new labels (FIX - when we have clean rules we can stop
  ;;; doing this)
  (setf *local-var-context* nil)
;;;   (internal-output-rmrs semstruct 'vcompact t t)
  (let ((new-hook (renumbered-hook (semstruct-hook semstruct))))
    (make-semstruct
     :hook new-hook
     :slots (let ((slots (semstruct-slots semstruct)))
	      (loop for slot in slots
		  collect
		    (make-slot :hook (renumbered-hook (slot-hook slot))
			       :name (slot-name slot))))
     :liszt
     (loop for old-ep in (semstruct-liszt semstruct)
         collect
	   (let ((label (rel-handel old-ep))
		 (anchor (rel-anchor old-ep)))
	     (make-char-rel 
	      :handel 
	      (renumbered-or-created-handel label)
	      :anchor 
	      (renumbered-or-created-handel anchor)
	      :pred (rel-pred old-ep)
	      :flist
	      (loop for old-arg in (rel-flist old-ep)
		  collect
		    (generate-new-var old-arg))
	      :cfrom cfrom
	      :cto cto)))
     :rmrs-args
     (loop for old-rarg in (semstruct-rmrs-args semstruct)
         collect
           (let ((val (rmrs-arg-val old-rarg)))
             (make-rmrs-arg 
              :arg-type (rmrs-arg-arg-type old-rarg)
              :label (generate-new-var (rmrs-arg-label old-rarg))
              :val (if (var-p val)
                       (generate-new-var val)
		     val))))
     :h-cons
     (loop for old-hcons in (semstruct-h-cons semstruct)
         collect
           (make-hcons
	    :relation (hcons-relation old-hcons)
            :scarg (generate-new-var (hcons-scarg old-hcons))
	    :outscpd (generate-new-var (hcons-outscpd old-hcons))))
     :bindings
     (loop for eq in (semstruct-bindings semstruct)
	 collect
	   (loop for var in eq
	       collect
	       (generate-new-var var)))
     :cfrom cfrom
     :cto cto)))

(defun renumbered-or-created-handel (old-handel)
  (if old-handel
      (generate-new-var old-handel)
    (create-new-rmrs-var 
     :handle 
     *rmrs-variable-generator* nil)))


(defun renumbered-hook (old-hook)
  (make-hook :index (if (hook-index old-hook)
			(generate-new-var (hook-index old-hook)))
	     :xarg (if (hook-xarg old-hook)
			(generate-new-var (hook-xarg old-hook)))
	     :ltop (if (hook-ltop old-hook)
		       (generate-new-var (hook-ltop old-hook)))
	     :anchor (if (hook-anchor old-hook)
			 (generate-new-var (hook-anchor old-hook)))))

;;; tag stuff

(defun algebra-create-base-struct (tag lexeme)
    ;;; FIX - conversion just used in the short term
  (let ((converted-semstruct
	 (or (let ((already-done (find tag *algebra-tag-instructions*
				       :test #'equal 
				       :key #'rmrs-algebra-tag-name)))
	       (if already-done
		   (rmrs-algebra-tag-semstruct already-done)))
	     (let* ((tag-template (get-tag-template tag))
		    (tag-semstruct (convert-rmrs-tag-semstruct
				    (rmrs-tag-template-semstruct
				     (or tag-template
					 *default-template*))
				    tag)))
	       ;;; the conversion is potentially destructive
	       (unless tag-semstruct
		 (error "Didn't convert ~A" tag))
	       (push (make-rmrs-algebra-tag :name tag
					    :semstruct tag-semstruct)
		     *algebra-tag-instructions*)
	       tag-semstruct)))
	(from (word-info-from lexeme))
	(to (word-info-to lexeme)))
    (instantiate-tag-semstruct
     converted-semstruct
     from
     to
     lexeme)))

(defun show-algebra-rules nil
  (dolist (rule-set *algebra-rule-instructions*)
    (dolist (rule (rmrs-rule-set-alternatives rule-set))
      (show-algebra-rule rule))))

(defun show-algebra-rule (rule)
  (format t "~%~A ~A ~A ~A~%" 
	  (rmrs-algebra-rule-name rule)
	  (or (rmrs-algebra-rule-condition rule) "")
	  (rmrs-algebra-rule-dtrs rule)
	  (rmrs-algebra-rule-application-order rule))
  (internal-output-rmrs (rmrs-algebra-rule-semstruct rule) 
			'compact t t))

(defun show-algebra-tags nil
  (dolist (tag *algebra-tag-instructions*)
    (show-algebra-tag tag)))

(defun show-algebra-tag (tag)
  (format t "~%~A~%" 
	  (rmrs-algebra-tag-name tag))
  (internal-output-rmrs (rmrs-algebra-tag-semstruct tag) 
			'compact t t))


;;; Checking rules (currently called after conversion code, 
;;; but potentially general purpose)

;;; Checking rules

;;; 1. For all arguments, check that a) the argument label
;;; is equated to some anchor (whinge if not) and b) that the 
;;; argument value is one of a) a slot index b) the xarg or index
;;; of the hook c) is a handel value and is qeq the ltop of one
;;; of the slots (unless BODY)
;;; 2. check for the presence of a qeq.  If none exist,
;;; add eqs for all the ltops of the daughters to the hook ltop
;;; (i.e., add conjunction)

#|
|#

(defun check-algebra-rules nil
  (dolist (rule-set *algebra-rule-instructions*)
    (dolist (rule (rmrs-rule-set-alternatives rule-set))
      (check-algebra-rule rule)
      (show-algebra-rule rule))))

(defun check-algebra-tags nil
  (dolist (tag *algebra-tag-instructions*)
    (check-algebra-tag tag)
    (show-algebra-tag tag)))


(defun check-algebra-rule (rule)
  (let ((rule-name (rmrs-algebra-rule-name rule))
	(semstruct (rmrs-algebra-rule-semstruct rule)))
    (unless (semstruct-h-cons semstruct)
      (add-ltop-conjunction semstruct))
    ;;; add-ltop-conjunction does destructive modification
    (dolist (rarg (semstruct-rmrs-args semstruct))
      ;;; output error messages if problems
      (let ((value (rmrs-arg-val rarg))
	    (rarg-type (rmrs-arg-arg-type rarg)))
	(check-linked-arg-anchor 
	 (rmrs-arg-label rarg) semstruct rule-name rarg-type)
	(if (is-handel-var value)
	    (check-linked-handel-var value semstruct rule-name rarg-type)
	  (if (var-p value)
	      (check-linked-ordinary-var value semstruct rule-name rarg-type)
	    ;; else a constant, and OK
	    ))))))

(defun add-ltop-conjunction (semstruct)
  ;;; 
  (let ((hook-ltop (hook-ltop (semstruct-hook semstruct)))
	(slots (semstruct-slots semstruct))
	(eqs (semstruct-bindings semstruct)))
    (dolist (slot slots)
      (let* ((slot-ltop (hook-ltop (slot-hook slot))))
	(if slot-ltop
	    (unless (rule-vars-equated-p 
		     hook-ltop 
		     slot-ltop eqs)
	      (push (list hook-ltop slot-ltop)
		    (semstruct-bindings semstruct)))
	  (setf (hook-ltop (slot-hook slot))
	    hook-ltop))))))

(defun rule-vars-equated-p (var1 var2 eqs)
;;;  vara varb and some number of eqs
;;;  vara = varx varx = vary ... = varb
  (or (eql-var-id var1 var2)
      (let ((var1-equivs nil)
	    (other-eqs nil))
	(dolist (eq eqs)
	  (cond ((eql-var-id var1 (car eq)) 
		 (push (cadr eq) var1-equivs))
		((eql-var-id var1 (cadr eq)) (push (car eq) var1-equivs))
		(t (push eq other-eqs))))
	(or (member var2 var1-equivs :test #'eql-var-id)
	    (loop for new-var1 in var1-equivs
		thereis
		(rule-vars-equated-p new-var1 var2 other-eqs))))))
      
	
(defun check-linked-arg-anchor (label semstruct rule-name rarg-type)
  (let ((label-id (var-id label)))
    (unless
	(or 
	 (loop for ep in (semstruct-liszt semstruct)
	    thereis 
	      (let ((anchor (rel-anchor ep)))
		(and anchor
		     (equal label-id (var-id anchor)))))
	(loop for slot in (semstruct-slots semstruct)
	    thereis 
	      (let ((anchor (hook-anchor (slot-hook slot))))
		(and anchor
		     (equal label-id (var-id anchor))))))
      (format t "~%Error in rule ~A, RARG ~A: ~A is not anchored"
	      rule-name rarg-type label-id))))

(defun check-linked-handel-var (value semstruct rule-name rarg-type)
  ;;;  c) is a handel value and is qeq the ltop of one
  ;;; of the slots (unless BODY)
  (unless 
      (or (equal rarg-type "BODY")
       (let ((linked-label (dolist (qeq (semstruct-h-cons semstruct))
			     (when (eql-var-id value
					       (hcons-scarg qeq))
			       (return (hcons-outscpd qeq))))))
	 (and linked-label
	      (loop for slot in (semstruct-slots semstruct)
		    thereis
		      (let ((slot-hook (slot-hook slot)))
			(eql-var-id linked-label (hook-ltop slot-hook)))))))
   (format t "~%Error in rule ~A, RARG ~A: ~A is not qeq'd"
	      rule-name rarg-type (var-id value))))


(defun check-linked-ordinary-var (value semstruct rule-name rarg-type)
  ;;; the argument value is one of a) a slot index b) the xarg or index
  ;;; of the hook
  (let ((hook (semstruct-hook semstruct)))
    (unless (or (eql-var-id value (hook-index hook))
		(eql-var-id value (hook-xarg hook))
		(loop for slot in (semstruct-slots semstruct)
		    thereis
		      (let ((slot-hook (slot-hook slot)))
			(or (eql-var-id value (hook-index slot-hook))
			    (eql-var-id value (hook-xarg slot-hook))))))
      (format t "~%Error in rule ~A, RARG ~A: ~A is not equated"
	      rule-name rarg-type (var-id value)))))

;;; conversion of the XML formalism to the algebra

;;; Step 0 - interpret the rule according to algebra structures
;;; (reinterpret equalities as slots, move anchor into the hook)
;;; If there is no anchor, equate the anchor with the ltop.
;;; If the variables in the head or hook are unspecified,
;;; specify them and equate them.
;;; Step 1 - replace variables in anchor positions with `real' anchor
;;; variables, and delete ings.  
;;; Step 2 - construct the application order
;;; Step 3 - checking


;;; this is expected to be run-once code, just for the current files
;;; apart from Step 3


(defun convert-rmrs-rule (rule)
  (let* ((eqs (rmrs-rule-eqs rule))
	 (head (if (eql (rmrs-rule-head rule) -1) 
		   0 (rmrs-rule-head rule)))
	 (dtrs (rmrs-rule-dtrs rule))
	 (condition (rmrs-rule-condition rule))
	 (semstruct (rmrs-rule-semstruct rule))
	 (arity (rmrs-rule-arity rule))
	 (new-semstruct (replace-ings-in-rule-semstruct
			   (set-slots-in-rule-semstruct 
			    semstruct eqs 
			    head
			    (rmrs-rule-arity rule))))
	 (new-rule
	  (make-rmrs-algebra-rule 
	   :name (rmrs-rule-name rule)
	   :dtrs dtrs
	   :condition condition
	   :arity (rmrs-rule-arity rule)
	   :semstruct new-semstruct
	   :application-order
	     (cons head (loop for x from 0 to (- arity 1)
			    unless (eql head x)
			    collect x)))))
    (unless (semstruct-h-cons new-semstruct)
      (add-ltop-conjunction new-semstruct))
    new-rule))


(defun set-slots-in-rule-semstruct (semstruct eqs head dtr-arity)
  ;;; turn the equalities in the XML rules into explicit specifications
  ;;; of daughter hooks.  Equate the rule hook values and the head slot
  ;;; values if this is not done in the rule.
  (if semstruct
      (let ((new-semstruct
	     (copy-semstruct semstruct))
	    (hook (semstruct-hook semstruct))
	    (anchor (semstruct-slots semstruct))
	    (slots nil)
	    (equalities (preprocess-rule-equalities eqs dtr-arity)))
	(setf (semstruct-hook new-semstruct)
	    (make-hook :index (indices-index hook)
		       :ltop (indices-label hook)))
	(setf (hook-anchor (semstruct-hook new-semstruct))
	  (if (grammar-var-p anchor) 
	      anchor
	    (indices-label hook)))
	(dotimes (dtrnum dtr-arity)
	  (push
	   (construct-dtr-slot dtrnum 
			       (cdr (assoc dtrnum equalities))
			       head 
			       (semstruct-hook new-semstruct))
	   slots))
	(setf (semstruct-slots new-semstruct) slots)
	new-semstruct)
    (let ((default-hook (make-hook :ltop (construct-grammar-var "H100")
				   :anchor (construct-grammar-var "H101")
				   :xarg (construct-grammar-var "U102")
				   :index (construct-grammar-var "U103"))))
      (make-semstruct :hook default-hook
		      :slots (list (make-slot :hook default-hook
					      :name head))))))

;;; the spec says that equalities can have hook-el = hook-el,
;;; but in fact that happened very rarely - so I manually fixed the 
;;; grammar file so they didn't occur.  Note that the variables have to
;;; all exist outside the equality (but they did).
;;; (The code also allowed for any number of elements in an equality, but
;;; that didn't happen at all.)

(defun preprocess-rule-equalities (eqs dtr-arity)
  (let ((eq-a-list (loop for dtr from 0 to (- dtr-arity 1)
				 collect (list dtr))))
    (dolist (equality eqs)
      (let ((dtrnum nil) (var nil) (hook-el nil))
	(dolist (eq-el (equality-eq-els equality))
	  (if (pointer-p eq-el)
	      (progn
		(when (or dtrnum hook-el)
		  (error "Unexpected equality ~A" equality))
		(setf dtrnum (pointer-dtrnum eq-el))
		(setf hook-el (pointer-hook-el eq-el)))
	    (if (and (grammar-var-p eq-el) (not var))
		(setf var eq-el)
	      (error "Unexpected equality ~A" equality))))
	(unless (and dtrnum var hook-el)
	  (error "Unexpected equality ~A" equality))
	(push (cons hook-el var) (cdr (assoc dtrnum eq-a-list)))))
    eq-a-list))
	
	      
	      

(defun construct-dtr-slot (dtrnum eqrec head rule-hook)
  ;;; the eq-rec contains a list of hook-element.var appropriate
  ;;; for daughter dtrnum
  (let ((hook (make-hook)))
    (loop for eq in eqrec
	do
	  (cond ((equal (car eq) "INDEX")
		 (setf (hook-index hook) (cdr eq)))
		((equal (car eq) "LABEL")
		 (setf (hook-ltop hook) (cdr eq)))
		((equal (car eq) "ANCHOR")
		 (setf (hook-anchor hook) (cdr eq)))
		((equal (car eq) "XARG")
		 (setf (hook-xarg hook) (cdr eq)))
		(t (error "Bad ~A" eq))))
    ;;; following (long-winded) code equates 
    ;;; hook and head slot variables
    (when (eql dtrnum head)
      (cond ((and (hook-anchor hook) (hook-anchor rule-hook))
	     nil)
	    ((hook-anchor rule-hook)
	     (setf (hook-anchor hook) (hook-anchor rule-hook)))
	    ((hook-anchor hook) 
	     (setf (hook-anchor rule-hook) (hook-anchor hook)))
	    (t (let ((new-anchor (construct-grammar-var "H101")))
		 (setf (hook-anchor rule-hook) new-anchor)
		 (setf (hook-anchor hook) new-anchor))))
      (cond ((and (hook-ltop hook) (hook-ltop rule-hook))
	     nil)
	    ((hook-ltop rule-hook)
	     (setf (hook-ltop hook) (hook-ltop rule-hook)))
	    ((hook-ltop hook) 
	     (setf (hook-ltop rule-hook) (hook-ltop hook)))
	    (t (let ((new-ltop (construct-grammar-var "H100")))
		 (setf (hook-ltop rule-hook) new-ltop)
		 (setf (hook-ltop hook) new-ltop))))
      (cond ((and (hook-index hook) (hook-index rule-hook))
	     nil)
	    ((hook-index rule-hook)
	     (setf (hook-index hook) (hook-index rule-hook)))
	    ((hook-index hook) 
	     (setf (hook-index rule-hook) (hook-index hook)))
	    (t (let ((new-index (construct-grammar-var "U103")))
		 (setf (hook-index rule-hook) new-index)
		 (setf (hook-index hook) new-index)))))
    (make-slot :hook hook :name dtrnum)))
	
    

(defun replace-ings-in-rule-semstruct (semstruct)
  ;;; all variables in anchor position are replaced
  ;;; by new anchor variables.  these are added to the 
  ;;; corresponding relations.
  ;;; just create new vars
  ;;; An and replace Hn with An in 
  ;;; a) hook anchor
  ;;; b) anchor positions of slots
  ;;; add An to anchor positions in relations
  ;;; if there are INGs, then add equalities
  ;;; instead
  (let ((variable-matches 
	 (anchor-hook (semstruct-hook semstruct) nil)))
    (dolist (slot (semstruct-slots semstruct))
      (setf variable-matches 
	(anchor-hook (slot-hook slot) variable-matches)))
    (dolist (ep (semstruct-liszt semstruct))
      (anchor-ep ep variable-matches))
    (dolist (rmrs-arg (semstruct-rmrs-args semstruct))
      (anchor-rarg rmrs-arg variable-matches))
    (setf (semstruct-bindings semstruct)
      (convert-ings-to-equalities (semstruct-in-groups semstruct)))
    (setf (semstruct-in-groups semstruct) nil)
    semstruct))

(defun create-anchor-var (label-var)
  ;;; this naming is just for readability
  (let ((id (concatenate 'string "a" (subseq (var-id label-var) 1))))
    (make-grammar-var :type "h" :id id)))

(defun anchor-hook (hook label-anchor-pairs)
  (let* ((anchor-pos-var (hook-anchor hook))
	 (existing-anchor-var
	  (if anchor-pos-var
	      (cdr (assoc anchor-pos-var label-anchor-pairs))))
	 (anchor-var
	  (or existing-anchor-var
	      (if anchor-pos-var
		  (create-anchor-var anchor-pos-var)))))
    (when anchor-var
      (unless existing-anchor-var
	(push (cons anchor-pos-var anchor-var) label-anchor-pairs))
      (setf (hook-anchor hook) anchor-var))
    label-anchor-pairs))

(defun anchor-ep (rel label-anchor-pairs)
  (let* ((label-var (rel-handel rel))
	 (existing-anchor-var (cdr (assoc label-var label-anchor-pairs)))
	 (anchor-var
	  (or existing-anchor-var
	      (create-anchor-var label-var))))
    (unless existing-anchor-var
      (push (cons label-var anchor-var) label-anchor-pairs))
    (setf (rel-anchor rel) anchor-var)
    label-anchor-pairs))

(defun anchor-rarg (rarg label-anchor-pairs)
  (let* ((anchor-pos-var (rmrs-arg-label rarg))
	 (existing-anchor-var (cdr (assoc anchor-pos-var label-anchor-pairs)))
	 (anchor-var
	  (or existing-anchor-var
	      (create-anchor-var anchor-pos-var))))
    (unless existing-anchor-var
      (push (cons anchor-pos-var anchor-var) label-anchor-pairs))
    (setf (rmrs-arg-label rarg) anchor-var)
    label-anchor-pairs))

(defun convert-ings-to-equalities (ings)
  (let ((eqs nil))
    (dolist (ing ings)
      (let* ((inga (in-group-label-a ing))
	     (ingb (in-group-label-b ing)))
	  (push (list inga ingb) eqs)))
    eqs))
	
;;; conversion of tag entries


(defun convert-rmrs-tag-semstruct (tag-semstruct tag)
  ;;; If there is only one EP in the
  ;;; liszt, make the anchor the LTOP for now - it will get set
  ;;; appropriately in the replace-ings code.  If there is more than one EP
  ;;; then scream!  
  (let* ((new-semstruct
	  (copy-semstruct tag-semstruct))
	 (anchor (semstruct-slots tag-semstruct))
	 (hook (semstruct-hook tag-semstruct)))
      (unless anchor
	(when (cdr (semstruct-liszt new-semstruct))
	  (error "Cannot convert ~A automatically: multiple EPs and no anchor" tag))
	(setf anchor (indices-label hook)))
      (unless (indices-default hook)
	(setf (semstruct-hook new-semstruct) 
	  (make-hook :index (indices-index hook)
		     :ltop (indices-label hook)
		     :anchor anchor)))
      (setf (semstruct-slots new-semstruct) nil)
      (replace-ings-in-rule-semstruct
       new-semstruct)))



