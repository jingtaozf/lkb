(in-package :mrs)

;;; final output structure

(defstruct rmrs 
  eps
  bindings)

;;; the variables are replaced by canonical forms when 
;;; the structure is printed


;;; Building semantic structures via an algebra

(defstruct semstruct
  hook
;;;  holes
  eps
  binding-list)

;;; hook has an indices structure as a value
;;;
;;; holes would be a list of indices, tagged by some sort of name
;;; corresponding to the syntax
;;; however, for the robust semantic composition,
;;; we don't know anything about argument-hood lexically,
;;; and holes is generally unset, except (perhaps) for prepositions
;;; where the class may have known properties (this assumes the
;;; preposition/particle distinction is made)
;;; for now, I'll try and do without holes
;;;
;;; eps is a list of elementary predications
;;; the structure defined like this for MRS compatability

#|
(defstruct ep
  sort  ; relation name
  flist)
|#

#|
(defstruct (var)
  name
  type
  extra ; useful for e.g. agreement values
  id)
|#


(defstruct indices
  index
  ;;; extarg - may be needed, even for the handle-free MRS
  )

;;; Rules

(defstruct rmrs-rule 
  name
  dtrs
  arity
  head
  semstruct
  eqs)

(defstruct equality
  ;;; just used in rules - eqs in rule is a list
  ;;; of these
  eq-els)

(defstruct pointer
  ;;; just used in rule equalities - a pointer points
  ;;; to a variable in a dtr
  dtrnum
  hook-el)

;;; Tag information

(defstruct rmrs-tag-template
  name
  semstruct)

(defparameter *DEFAULT-TEMPLATE*
    (MAKE-RMRS-TAG-TEMPLATE
     :NAME "DEFAULT"
     :SEMSTRUCT
     (MAKE-SEMSTRUCT 
      :HOOK (make-indices :index "U") 
      :EPS (LIST (MAKE-EP :sort "DUMMY-PRED" 
                          :flist (LIST "U"))))))

(defun dummy-pred-p (str)
  (equal str "DUMMY-PRED"))

(defun make-dummy-pred nil
  "DUMMY-PRED")

(defparameter *rmrs-var-types*
    '((#\e . :event)
      (#\x . :object)
      (#\u . :other)
      (#\E . :event)
      (#\X . :object)
      (#\U . :other)
      (#\e . "EVENT")
      (#\x . "OBJECT")
      (#\u . "OTHER")))
      

(defun find-var-type (str)
  (let ((letter (elt str 0)))
    (or (cdr (assoc letter *rmrs-var-types*))
        :other)))

(defun find-var-letter (type)
  (or (car (rassoc type *rmrs-var-types* :test #'equal))
      (car (rassoc (string type) *rmrs-var-types* :test #'equal))
      #\u))

;;; Main entry point

(defun construct-sem-for-tree (tree &optional (ostream t))
  ;;; takes a tree and returns a semstruct - guaranteed
  ;;; - unless there's a syntax error in input data
  (initialize-rmrs-variables)
  (let* ((semstruct
          (construct-sem-for-tree-aux tree))
         (canonical-bindings 
          (close-bindings (semstruct-binding-list semstruct))))
    (output-rmrs1 (make-rmrs 
                  :eps (semstruct-eps semstruct)
                  :bindings canonical-bindings)
                 'xml ostream)))

(defstruct binding 
  var
  id
  equivs
  canonical)

(defun lookup-canonical-var (var binding-list)
  ;;; returns the canonical var
  (let* ((id (var-id var))
         (var-entry (find id binding-list :key #'binding-id)))
    (if var-entry
        (binding-canonical var-entry)
        var)))
        
(defun close-bindings (bindings)
  ;;; the bindings are a list of pairs, linking ids
  ;;; this function effectively performs the transitive
  ;;; closure, resulting in a list where each id used
  ;;; is paired with its canonical var (could be the same)
  (let ((canonical-bindings nil))
    (dolist (pair bindings)
      (dolist (el pair)
        (unless (member (var-id el) 
                        canonical-bindings
                        :key #'binding-id)
          (push (make-binding :var el :id (var-id el))
                 canonical-bindings))))             
    (dolist (pair bindings)
      (when (and (car pair) (cadr pair))
        (let* ((var1-id (var-id (car pair)))
               (var2-id (var-id (cadr pair))))
          (when (cddr pair) 
            (error "Non binary binding ~A unexpected" pair))
          (augment-bindings var1-id var2-id
                            canonical-bindings nil)
          (augment-bindings var2-id var1-id
                            canonical-bindings nil))))
    (dolist (binding canonical-bindings)
      (setf (binding-canonical binding)
        (let* ((canon-id
               (apply #'min (cons (binding-id binding)
                                  (binding-equivs binding))))
               (canon-entry (find canon-id 
                                  canonical-bindings 
                                  :key #'binding-id)))
          (binding-var canon-entry))))
    ;; i.e., sort all the equivalents, including the original var
    canonical-bindings))
        
(defun augment-bindings (main-var second-var bindings done)
  (unless (or (eql main-var second-var) (member main-var done))
    (let ((main-var-entry (find main-var bindings :key #'binding-id)))
      (unless main-var-entry
        (error "Incorrectly set up bindings for ~A" main-var))
      (unless (member second-var (binding-equivs main-var-entry))
        (dolist (id (binding-equivs main-var-entry))
          (augment-bindings second-var id
                            bindings (cons main-var done))
          (augment-bindings id second-var 
                            bindings (cons main-var done)))
        (push second-var (binding-equivs main-var-entry))))))

;;; the code assumes a tree structure, where there's some
;;; notion of a node, which either contains a base tag/lexeme
;;; specification or a rule-name plus dtrs.  The functions
;;; daughter-nodes-p, get-rule-name, get-dtr-nodes, get-lexical-tag
;;; and get-lexeme are all defined for the particular format
;;; they all take a node.  The calling function is also
;;; format specific, because there may be other stuff in the file

;;; there's no particular reason why we need an overt rule name
;;; on the node - we just need some form of identification of
;;; a recipe for composition


(defun construct-sem-for-tree-aux (tree-node)
  (if (daughter-nodes-p tree-node)
      (let ((rule-name (get-rule-name tree-node))
	    (dtr-nodes (get-dtr-nodes tree-node)))
	(compose rule-name
		 (loop for dtr in dtr-nodes
		     collect
		       (construct-sem-for-tree-aux dtr))))
    (let ((base-tag (get-lexical-tag tree-node))
	  (lexeme (get-lexeme tree-node)))
      (create-base-struct base-tag lexeme))))

;;; composition

(defvar *local-var-context* nil)

(defun compose (rule-name dtrs)
  ;;; compose takes a rule name and a list of daughters
  ;;; the rule name is looked up in *rule-instructions*
  ;;; this may give full instructions, or just mark the
  ;;; head.  If there's no instruction, default composition
  ;;; alone operates.
  (unless dtrs ;;; really there always ought to be dtrs
             ;;; but this prevents errors on trees with just a rule name
    (error "~% defective tree"))
  (let ((rule-instruction (lookup-instruction rule-name))
	(dtr-hooks (loop for dtr in dtrs
		       collect (semstruct-hook dtr)))
	(dtr-eps (loop for dtr in dtrs
		     append (semstruct-eps dtr)))
	(dtr-binding-list (loop for dtr in dtrs
		     append (semstruct-binding-list dtr)))
	(semhead nil)
	(semstruct nil)
	(equalities nil))      	 
    (when (and rule-instruction
	       (eql (rmrs-rule-arity rule-instruction) 
		    (length dtrs)))
      ;;; fixed arity assumption may cause probs,
      ;;; but assume for now
      (setf semhead (rmrs-rule-head rule-instruction))
      (setf *local-var-context* nil)
      (setf semstruct 
	(when (rmrs-rule-semstruct rule-instruction)
	  (construct-new-semstruct
	   (rmrs-rule-semstruct rule-instruction))))
      ;;; if semstruct has been computed - this affects the
      ;;; equality computation via *local-var-context*
      (setf equalities 
	(compute-equalities dtr-hooks
			    (rmrs-rule-eqs rule-instruction))))
    (make-semstruct :hook 
		    (if semhead		; a number indicating the dtr
			(semstruct-hook 
			 (elt dtrs semhead))
                      (if (and (not rule-instruction)
                               (not (cdr dtrs)))
                          (semstruct-hook (car dtrs))
                                ;;; assume semhead is single dtr 
                                ;;; if unary rule
                        (generate-new-var "u")))
		    :eps (if semstruct 
			     (append 
			      (semstruct-eps semstruct) dtr-eps)
			   dtr-eps)
		    :binding-list 
		    (append equalities dtr-binding-list))))

;;; Tag lookup
;;; 

(defun create-base-struct (tag lexeme)
  (let ((tag-template (get-tag-template tag)))
    (construct-new-semstruct
     (rmrs-tag-template-semstruct
       (or tag-template
	   *default-template*))
       lexeme)))

;;; A new semstruct may be created either for a lexical tag or for
;;; a semstruct contributed by a grammar rule.  In either case,
;;; a new set of variables has to be created, which are unique
;;; for this semstruct in this derivation, and a base binding list
;;; is also created

;;; In the case of tag lookup, the pred corresponding to
;;; the lexeme will be substituted.  For instance:
;;;
;;; <index> e
;;; <ep> <pred><arg>e</ep>
;;;
;;; <index> e44
;;; <ep> cat<arg>e44</ep>
;;;

#|
 In the case of a rule, when a new semstruct is created,
 the variables also affect the equalities in the rule.
 For instance:

<rule>
<semstruct>
 <index> e 
 <ep> <realpred> p-agt <arg> e <arg> x2 </ep>
</semstruct>
<eq> VP.index e </eq>
<eq> NP.index x </eq>
</rule>

goes to

<rule>
<semstruct>
 <index> e101 
 <ep> <realpred> p-agt <arg> e101 <arg> x201 </ep>
</semstruct>
<eq> VP.index e101 </eq>
<eq> NP.index x101 </eq>
</rule>

|#

(defvar *var-count* 0)

(defun initialize-rmrs-variables nil
  (setf *var-count* 0))

(defun generate-new-var (name)
  ;;; name is a string - e.g. "e2"
  (or (rest (assoc name *local-var-context* :test #'equal))
      (let* ((var-type (find-var-type name))
	     (id (incf *var-count*))
	     (varstruct (make-var :id id :type var-type)))
	(push (cons name varstruct) *local-var-context*)
	varstruct)))

(defun construct-new-semstruct (semstruct &optional pred)
  ;;; *local-var-context* gets used when interpreting the eqs
  ;;; in the case of a rule
  ;;; pred should only be set in the case of a new tag
  (setf *local-var-context* nil)
  (let* ((new-hook (generate-new-hook (semstruct-hook semstruct))))
    (make-semstruct :hook new-hook
		    :eps
		    (loop for old-ep in (semstruct-eps semstruct)
			collect
			  (make-ep :sort 
				   (let ((old-pred (ep-sort old-ep)))
				     (if (dummy-pred-p old-pred)
					 pred
				       old-pred))
				   :flist
				   (loop for old-arg in (ep-flist old-ep)
					  collect
					  (generate-new-var old-arg)))))))

(defun generate-new-hook (old-hook)
  ;;; needs fixing if more than just an index
  (make-indices :index 
		(generate-new-var (indices-index old-hook))))

(defun compute-equalities (dtr-hooks equalities)
  ;;; equality components in rules are either 
  ;;; integers (indicating dtr-hooks) or
  ;;; variables which should correspond to the *local-var-context*
  ;;;
  ;;; this function returns a list of lists of variables
  (let ((real-eqs nil))
    (dolist (equality equalities)
      (let ((els (equality-eq-els equality))
	    (new-els nil))
	(dolist (el els)
	  (let  ((new-var 
		  (if (pointer-p el)
		      (get-var-for-pointer el dtr-hooks)
		    (rest (assoc el *local-var-context* :test #'equal)))))
	    (when new-var
	      (push new-var new-els))))
	(when new-els
	  (push new-els real-eqs))))
    real-eqs))
	      
(defun get-var-for-pointer (pointer dtr-hooks)
  (let* ((dtr-num (pointer-dtrnum pointer))
	 (hook (elt dtr-hooks dtr-num)))
    (if (and hook (indices-p hook))
	(cond ((equal (pointer-hook-el pointer)
		      "INDEX") 
	       (indices-index hook))
	      (t nil)))))

	

