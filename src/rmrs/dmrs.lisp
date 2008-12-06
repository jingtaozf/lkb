;;; Copyright (c) 2008
;;;   Ann Copestake
;;;   see `licence.txt' for conditions.

(in-package :mrs)

;;; DMRS structure

#|

A DMRS is a connected acyclic graph.  All nodes are RMRS
predicate symbols with the exception that there is exactly one node
labelled LTOP with one or more arcs which link it to other
node(s).  Arcs may be directed or non-directed.
The LTOP arc(s) are unlabelled, all other directed arcs have labels
of the form ARG, ARG/h, ARG/= or ARG/neq, where ARG is taken from the
RMRS inventory of arguments.  There may also be undirected /= arcs.

|#

(defstruct (dmrs)
  nodes
  links)

(defstruct dmrs-node
  ;;; the LTOP node is always id 0, others may be anything - but
  ;;; code for conversion from RMRS uses the anchors.
  ;;; cfrom and cto as in (R)MRS
  ;;; charvar and label are for convenience in processing
  ;;; char var is the variable characteristic of the RMRS predication
  ;;; (always the ARG0, but not all ARG0s are charvars)
  ;;; label is the label of the RMRS predication
  id
  pred
  cfrom
  cto
  charvar
  label)

(defstruct dmrs-link
  ;;; from and to are DMRS node ids, pre and post make up the arc
  ;;; label
  from to pre post)

(defstruct dmrs-ngroup 
  ;;; this is only used when constructing a DMRS - a group of
  ;;; nodes (the elements) which share a label.  The targets are
  ;;; distinguished by having no outgoing arcs.
  label
  elements
  targets)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; OUTPUT

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun simple-output-dmrs (dmrs)
  (format t "~%")
  (dolist (node (dmrs-nodes dmrs))
    (let ((pred (dmrs-node-pred node))
	  (id (dmrs-node-id node)))
    (format t "~A:~A~%"
	    id
	    (if (realpred-p pred)
		(convert-realpred-to-string
		 (realpred-lemma pred)
		 (realpred-pos pred)
		 (realpred-sense pred))
	      pred))))
    (format t "~%")
  (dolist (link (sort-dmrs-links-by-from (dmrs-links dmrs) 
					 (dmrs-nodes dmrs)))
    (let ((from (dmrs-link-from link))
	  (pre (dmrs-link-pre link))
	  (post (dmrs-link-post link)))
    (format t "~A ~A ~A~%" 
	    (if (eql from 0) "LTOP" from)
	    (cond ((and pre post) (format nil "~A/~A" pre post))
		  (pre (format nil "~A" pre))
		  (post (format nil "/~A" post))
		  (t ""))
	    (dmrs-link-to link))))
  (format t "~%"))
	    
(defun sort-dmrs-links-by-from (links nodes)
  (let ((ltop-links nil)
	(non-ltop-links nil))
    (dolist (link links)
      (if (eql (dmrs-link-from link) 0)
	  (push link ltop-links)
	(push link non-ltop-links)))
    (append ltop-links
	    (sort non-ltop-links 
		  #'(lambda (link1 link2)
		      (member (dmrs-link-from link2)
			      (member (dmrs-link-from link1)
				      nodes :key #'dmrs-node-id)
			      :key #'dmrs-node-id))))))

;;; LaTeX output etc

#|

Draw the nodes across the top, and create arcs as straight lines

Fill the first line with links, trying shortest first, avoiding 
overlaps and touching links.  Then next line etc till all done.

NODE1            NODE2              NODE3         NODE4

  ----------------->                  <-------------
       ARC                                  ARC

  ------------------------------------>
       ARC     

LTOPs indicated by *
       
|#


#|

(defstruct dmrs-layout-node
  id
  label
  ltop
  x)

(defstruct dmrs-layout-link
  label
  left-x
  right-x
  direction ;; :l :r or nil
  y)

(defun layout-dmrs (dmrs)
  ;;; units have to be greater than character width
  (let* ((node-x 0)
	 (start-y 0)
	 (y-increment 10)
	 (x-increment 10)
	 (char-conversion 2) ;; multiple of 2
	 (tmp-links nil) ;; list organised by length
	 (final-links nil)
	 (layout-nodes
	  (loop for node in (dmrs-nodes dmrs)
	      collect
		(let* ((pred (dmrs-node-pred node))
		       (id (dmrs-node-id node))
		       (predstring
			(if (realpred-p pred)
			    (convert-realpred-to-string
			     (realpred-lemma pred)
			     (realpred-pos pred)
			     (realpred-sense pred))
			  pred))
		       (node-width (* char-conversion 
				      (length predstring)))
		       (current-x (+ node-x (/ node-width 2))))
		  (setf node-x (+ node-x node-width x-increment))
		  (make-dmrs-layout-node :id id
					 :label predstring
					 :x node-x)))))
    (dolist (link (dmrs-links dmrs))
      (let* ((from (dmrs-link-from link))
	     (to (dmrs-link-to link)))
	(if (eql from 0) ;; LTOP
	    (mark-ltop-node to layout-nodes)
	  (let* ((pre (dmrs-link-pre link))
		 (post (dmrs-link-post link))
		 (link-label
		  (cond ((and pre post) (format nil "~A/~A" pre post))
			(pre (format nil "~A" pre))
			(post (format nil "/~A" post))
			(t "")))
		 (from-x (find-dmrs-node-x from layout-nodes))
		 (to-x (find-dmrs-node-x to layout-nodes))
		 (left-x (if (< from-x to-x) from-x to-x))
		 (right-x (if (< from-x to-x) to-x from-x))
		 (new-link
		  (make-dmrs-layout-link
		   :label link-label
		   :left-x left-x
		   :right-x right-x
		   :direction (if pre (if (< from-x to-x) :r :l)
				nil)))
		 (length (- right-x left-x))
		 (length-set (assoc length tmp-links)))
	    (if length-set (push new-link (cdr length-set))
	      (push (cons length (list new-link)) tmp-links))))))
    (dolist (length-set tmp-links)
      (setf (cdr length-set) (sort (cdr length-set) 
				   #'< :key #'left-x)))
    (setf tmp-links (sort tmp-links #'< :key #'car))
    (let ((current-y start-y))
      (when tmp-links
	(loop 
	  (multiple-value-bind 
	      (done-links new-tmp) 
	      (fit-all-links tmp-links current-y)
	    (setf final-links (append final-links done-links))
	    (unless new-tmp (return))
	    (setf tmp-links new-tmp)
	    (setf current-y (+ current-y y-increment))))))
    (construct-dmrs-diagram layout-nodes final-links)))

(defun mark-ltop-node (node-id nodes)
  (let ((node (find node-id layout-nodes :key #'dmrs-layout-node-id)))
    (when node (setf (dmrs-layout-node-ltop node) t))))

(defun find-dmrs-node-x (node-id nodes)
  (let ((node (find node-id layout-nodes :key #'dmrs-layout-node-id)))
    (if node (dmrs-layout-node-x node))))

(defun fit-all-links (to-fit y)
  ;;; fits all the links we can on one y, returns a list of fitted links
  ;;; with the y set, and a list of ones that don't fit
  (let ((y-set nil)
	(unfitted nil))
    (dolist (length-set to-fit)
      ;; if speed were an issue, we would return when we'd 
      ;; got to a point where we knew there could be no space
      ;; in the y-set for links of a certain length, but ignore for
      ;; now
      (let ((unfitted-length nil))
	(dolist (link (cdr length-set))
	  (if (link-fits link y-set)
	      (progn (setf (dmrs-layout-link-y link) y)
		     (push link y-set))
	    (if unfitted-length 
		(push link (cdr unfitted-length))
	      (setf unfitted-length (cons (car length-set) 
					  (list link))))))
	(when unfitted-length
	  (setf (cdr unfitted-length)
	    (nreverse (cdr unfitted-length)))
	  (push unfitted-length
		unfitted))))
    (values y-set 
	    (nreverse unfitted))))

(defun link-fits (link fitted-set)
  (let ((link-left (dmrs-layout-link-left-x link))
	(link-right (dmrs-layout-link-right-x link)))
    (loop for fitted-link in fitted-set
	  all-satisfy
	  (or (< link-right (dmrs-layout-link-left-x link)) 
	      (> link-left (dmrs-layout-link-right-x link))))))



|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; RMRS to DMRS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(let ((*package* (find-package :mrs)))
  (with-open-file (istream "~/lingo/lkb/src/rmrs/eg.rmrs" 
		   :direction :input)
    (loop
      (let ((rmrs (parse-xml-removing-junk istream)))
	    (when (and rmrs (not (xml-whitespace-string-p rmrs)))
	      (rmrs-to-dmrs (read-rmrs rmrs nil)))))))
|#

#|

To transform an RMRS into a DMRS:

1. Each predicate in the RMRS becomes a node in the DMRS graph.

2. Each variable y in the RMRS is associated with a unique 
predicate which owns it (call this predicate charp(y)).  

steps 1 and 2 - extract-rmrs-nodes (checked by check-char-vars)
anchors are used as ids.  

3. For every non-characteristic argument A (except BV arguments)
  in the RMRS which is anchored to a predicate P and has a variable
  value y, such that \charp(y) is P', an arc exists in the DMRS
  from P to P'.  The arcs wil be referred to as {\bf variable
    arcs}.  The preslash label on that arc is equal to the argument
type of A (e.g., ARG1).  

done by extract-rmrs-var-links

If the labels of P and P' are
  equated, then the post-slash label is =.  If P has a hole argument
  which is qeq the label of P', then the post-slash label is h.  If
  P and P' have labels which are unrelated in the RMRS, the
  post-slash label is neq.

4. For every case where a set S of predicates in the RMRS have equal
labels and the linkages are not fully represented by equalities on
variable arcs (taking transitive connections into account), we define
an ordering P0 to PN on S by character position of the
predicates.  There is a non-directional /= link between P0 and each
other predicate in P1 to PN considered successively which is
not linked by previous operations.

the second part of 3 and step 4 are done by determine-label-equality-sets 
(which sets up the sets, and also finds the `targets' (5)) and 
make-dmrs-eq-links, which adds labels to existing variable arcs 
and creates /eq arcs if no arcs exist.

5. If a label lb labels a set of predicates, then
  the head predicates in that set are defined as those which
have no outgoing variable arcs to other elements in the set.

done by determine-label-equality-sets 

6. If the RMRS LTOP has value lb labelling a set of predicates, then the
LTOP node in the DMRS is linked to each head predicate in that set.

make-dmrs-ltop-links

7. For every argument A which is anchored to a predicate P and has
a hole value h, such that h is qeq a label lb which
labels a set of predicates S:

a) If P has exactly one argument A' with a value y such that P' = \charp(y) 
is a member of S, then there is an ARG/h arc from P to P', where ARG is the 
argument type of A. (Note that this covers the RSTR case because there 
will be a BV argument.)

this condition is checked by char-var-selects

b) Otherwise, there are ARG/h arcs from P to each of the head predicates in S.

make-dmrs-handel-links

8 If the preslash label on the arc uniquely determines the
  postslash label, the slash and the postslash label may be omitted.
(e.g., we write RSTR, rather than RSTR/h).

FIX - not done (output only)

|#

(defun rmrs-to-dmrs (rmrs) convert an RMRS to a DMRS
  (let ((nodes (extract-rmrs-nodes rmrs))) 
    (check-char-vars nodes) error if charvar property doesn't hold
    (let* ((var-links (extract-rmrs-var-links rmrs nodes))
	   (dgroups (determine-label-equality-sets nodes var-links))
	   ;; nodes which share a lable
	   (eqlinks (make-dmrs-eq-links var-links dgroups))
	   (hlinks (make-dmrs-handel-links rmrs dgroups nodes))
	   (ltoplinks (make-dmrs-ltop-links rmrs dgroups))
	   ;;; link types are distinguished for convenience
	   (dmrs
      (make-dmrs :nodes nodes
		 :links (append
			 ltoplinks
			 (loop for var-link in var-links
			     collect
			       (let ((post (dmrs-link-post var-link)))
				 (unless post
				   (setf (dmrs-link-post var-link) :neq))
				 var-link))
			 eqlinks 
			 hlinks))))
;;;      (pprint dgroups)
      (simple-output-dmrs dmrs))))

(defun extract-rmrs-nodes (rmrs)
  (loop for rel in (rmrs-liszt rmrs)
      collect
	(make-dmrs-node
	 :id (var-id (rel-anchor rel))
	 :pred (rel-pred rel)
	 :cfrom (rel-cfrom rel)
	 :cto (rel-cto rel)
	 :charvar (if (or (and (realpred-p (rel-pred rel))
			       (not (equal (realpred-pos (rel-pred rel)) "q")))
			  (member (rel-pred rel)
				  '("pron_rel") :test #'equal))
		      (var-id (car (rel-flist rel))))
	 :label (var-id (rel-handel rel)))))

(defun check-char-vars (nodes)
  (let ((vars nil))
    (dolist (node nodes)
      (let ((char-var (dmrs-node-charvar node)))
	(if (and char-var (member char-var vars))
	    (error "~%Duplicate char var ~A in ~A" char-var nodes)
	  (push char-var vars))))))

(defun extract-rmrs-var-links (rmrs nodes)
  (loop for rmrs-arg in (rmrs-rmrs-args rmrs)
      unless (or (equal (var-type (rmrs-arg-val rmrs-arg)) "h")
		 (equal (var-type (rmrs-arg-val rmrs-arg)) "u"))
      collect
	(let* ((var (rmrs-arg-val rmrs-arg))
	       (var-id (var-id var))
	       (from-node-id (var-id (rmrs-arg-label rmrs-arg)))
	       (to-node (car (member var-id nodes :key #'dmrs-node-charvar))))
	  (make-dmrs-link
	   :from from-node-id
	   :to (if to-node (dmrs-node-id to-node))
	   :pre (rmrs-arg-arg-type rmrs-arg)))))




(defun determine-label-equality-sets (nodes links)
  ;;; returns nodes grouped by equality of labels,
  ;;; with the target nodes identified
  ;;; 
  (let ((ngroups nil))
    (dolist (node (reverse nodes))
      ;;; end up with textual order!
      (let* ((label (dmrs-node-label node))
	     (node-id (dmrs-node-id node))
	     (ngroup (car (member label ngroups :key #'dmrs-ngroup-label))))
	(if ngroup
	    (push node-id (dmrs-ngroup-elements ngroup))
	  (push (make-dmrs-ngroup :label label
				  :elements (list node-id))
		ngroups))))
    (dolist (ngroup ngroups)
      (let* ((elements (dmrs-ngroup-elements ngroup)))
	(setf (dmrs-ngroup-targets ngroup)
	  (loop for element in elements
	      unless
		(dolist (link links)
		  (when (and (eql (dmrs-link-from link) element)
			     (member (dmrs-link-to link) elements))
		    (return t)))
	      collect element))))
    ngroups))


(defun make-dmrs-eq-links (var-links dgroups) 
  ;;; go through the dgroups, adding equalities post slash to the
  ;;; var-links.  If at the end there are unlinked elements in the
  ;;; dgroup, create-eqlinks from the textually first element to other
  ;;; elements until all are linked.
  ;;; textually first is interpreted here by order from RMRS - FIX by using
  ;;; CFROM order
  (loop for dgroup in dgroups
    append
    (let ((linked (dmrs-ngroup-elements dgroup))
	  (done-pairs nil))
      (dolist (var-link var-links)
	(let ((from (dmrs-link-from var-link))
	      (to (dmrs-link-to var-link)))
	  (when (and (member from linked) 
		     (member to linked)) 
	    (setf (dmrs-link-post var-link) :eq)
	    (push (cons from to) done-pairs))))
      (let* ((complete-groups (link-transitive-closure done-pairs))
	     (lead (car linked)))
;;;	(format t "~% LINKED ~A" linked)
;;;	(format t "~% GROUPS ~A" complete-groups)
	(loop for other in (cdr linked)
	    unless 
	      (member-if #'(lambda (group)
			     (and (member lead group)
				  (member other group)))
			 complete-groups)
	    collect
	      (progn (setf complete-groups 
		       (combine-groups lead other complete-groups))
		     (make-dmrs-link :from lead
				     :to other
				     :pre nil
				     :post :eq)))))))

#|

(defun test-dmrs-linking (linked done-pairs)
  ;;; this tests the code above
  ;;; linked is a list of elements corresponding to
  ;;; all the elements which should be linked,
  ;;; in textual order.  done-pairs is a list of pairs which
  ;;; are already linked.  This should return a set of extra links
  ;;; as necessary to make everything linked
  ;;; all of which should have as the first element, the first
  ;;; element of linked.
  (let* ((complete-groups (link-transitive-closure done-pairs))
	 (lead (car linked)))
	(loop for other in (cdr linked)
	    unless 
	      (member-if #'(lambda (group)
			     (and (member lead group)
				  (member other group)))
			 complete-groups)
	    collect
	      (progn (setf complete-groups 
		       (combine-groups lead other complete-groups))
		     (cons lead other)))))

(test-dmrs-linking '(a b c d e f) '((b . c) (e . f))) 
((A . B) (A . D) (A . E))

(test-dmrs-linking '(a b c d e f) '((a . b) (e . f)))
((A . C) (A . D) (A . E))

(test-dmrs-linking '(a b c d e f) '((a . b) (b . c) (e . f)))
((A . D) (A . E))

(test-dmrs-linking '(a b c d e f) nil)
((A . B) (A . C) (A . D) (A . E) (A . F))


|#

(defun link-transitive-closure (links)
  (let ((merged-groups nil))
    (dolist (link links)
      (let* ((a (car link)) 
	     (b (cdr link)))
	(setf merged-groups (combine-groups a b merged-groups))))
    merged-groups))

(defun combine-groups (a b groups)
  (let ((a-group nil)
	(b-group nil)
	(others nil))
    (dolist (group groups)
      (cond ((member a group)
	     (setf a-group group)
	     (if (member b group)
		 (setf b-group group)))
	    ((member b group)
	     (setf b-group group))
	    (t (push group others))))
    (cond ((and a-group b-group (eq a-group b-group)) groups)
	  ((and a-group b-group) 
	       (cons (append a-group b-group) others))
	  (a-group (cons (cons b a-group) others))
	  (b-group (cons (cons a b-group) others))
	  (t (cons (list a b) others)))))

	  
(defun make-dmrs-handel-links (rmrs ngroups nodes)
  (loop for rmrs-arg in (rmrs-rmrs-args rmrs)
      when (equal (var-type (rmrs-arg-val rmrs-arg)) "h")
      append
	;; BODY args won't have a qeq, so the code
	;;; has to allow for this
	(let* ((hole (rmrs-arg-val rmrs-arg))
	       (hole-id (var-id hole))
	       (from-node-id (var-id (rmrs-arg-label rmrs-arg)))
	       (qeq (car (member hole-id 
				 (rmrs-h-cons rmrs) 
				 :key #'(lambda (x) 
					  (var-id (hcons-scarg x))))))
	       (target-label (if qeq (var-id (hcons-outscpd qeq))))
	       (ngroup (car (member target-label ngroups 
				    :key #'dmrs-ngroup-label)))
	       (elements (if ngroup (dmrs-ngroup-elements ngroup)))
	       (targets (if ngroup (dmrs-ngroup-targets ngroup)))
	       (to-node-ids (if (cdr elements)
				(or (char-var-selects from-node-id
						      elements rmrs nodes)
				 targets)
			      elements)))
	  (loop for to-node-id in to-node-ids
		collect
		(make-dmrs-link
		 :from from-node-id
		 :to to-node-id
		 :pre (rmrs-arg-arg-type rmrs-arg)
		 :post :h)))))

(defun char-var-selects (from-node-id elements rmrs nodes)
  (let* ((main-args
	  (loop for rmrs-arg in (rmrs-rmrs-args rmrs)
	      when (and (eql (var-id (rmrs-arg-label rmrs-arg)) from-node-id)
			(not (equal (var-type (rmrs-arg-val rmrs-arg)) "h")))
	      collect
		(let* ((var (rmrs-arg-val rmrs-arg))
		       (var-id (var-id var))
		       (to-node
			(car (member var-id nodes 
				     :key #'dmrs-node-charvar))))
		  (dmrs-node-id to-node))))
	 (inherent-args
	  (loop for rmrs-rel in (rmrs-liszt rmrs)
	      when (eql (var-id (rel-anchor rmrs-rel)) from-node-id)
	      collect
		(let* ((var (car (rel-flist rmrs-rel)))
		       (var-id (var-id var))
		       (to-node
			(car (member var-id nodes 
				     :key #'dmrs-node-charvar))))
		  (dmrs-node-id to-node))))
	 (non-char-args (loop for arg in inherent-args
			    unless (eql arg from-node-id)
			    collect arg))
	 (to-nodes (append non-char-args main-args)))
 ;;;   (format t "~%char-var to nodes ~A" to-nodes)
    (let ((targets (intersection to-nodes elements)))
 ;;;     (format t "~%char-var ~A" targets)
      (if (and targets (not (cdr targets)))
	  targets))))
		 
(defun make-dmrs-ltop-links (rmrs ngroups)
  (let* ((ltop-label (var-id (rmrs-top-h rmrs)))
	 (ngroup (car (member ltop-label ngroups :key #'dmrs-ngroup-label))))
    (when ngroup
      (loop for target in (dmrs-ngroup-targets ngroup)
	collect
	    (make-dmrs-link :from 0 :to target)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; DMRS to RMRS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
