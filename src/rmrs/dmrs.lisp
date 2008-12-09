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
  label
  carg)

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



(defun simple-output-dmrs (dmrs &optional (stream t))
  (format stream "~%")
  (dolist (node (dmrs-nodes dmrs))
    (let ((pred (dmrs-node-pred node))
	  (id (dmrs-node-id node))
	  (carg (dmrs-node-carg node)))
    (format stream "~A:~A~A~%"
	    id
	    (if (realpred-p pred)
		(convert-realpred-to-string
		 (realpred-lemma pred)
		 (realpred-pos pred)
		 (realpred-sense pred))
	      pred)
	    (if carg (format nil "/~A" carg)
	      ""))))
    (format stream "~%")
  (dolist (link (sort-dmrs-links-by-from (dmrs-links dmrs) 
					 (dmrs-nodes dmrs)))
    (let ((from (dmrs-link-from link))
	  (pre (dmrs-link-pre link))
	  (post (dmrs-link-post link)))
    (format stream "~A ~A ~A~%" 
	    (if (eql from 0) "LTOP" from)
	    (cond ((and pre post) (format nil "~A/~A" pre post))
		  (pre (format nil "~A" pre))
		  (post (format nil "/~A" post))
		  (t ""))
	    (dmrs-link-to link))))
  (format stream "~%"))
	    
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
overlaps.  Then next line etc till all done.

NODE1            NODE2              NODE3         NODE4*

   --------------->                    <-----------
       ARC                                  ARC

   --------------------------------->
       ARC     

LTOPs indicated by *

layout provides for x distances as starting at 0, calculated on basis
of characters

first node x (x1) is at half its width in characters
x2 is at half node1 width, plus half node2 width, plus x increment

y layout starts at 0 and increments by 1 - intention is that
this is converted into appropriate factor for the actual
drawing program (and recalculated so origin is bottom left)

|#



(defstruct dmrs-layout-node
  id
  label
  ltop
  start-x
  mid-x)

(defstruct dmrs-layout-link
  label
  left-x
  right-x
  direction ;; :l :r or nil
  y)

(defun layout-dmrs (dmrs layout-type stream)
  (let* ((x-increment 10) 
	 ;; constant - should be at least the length of the longest link label
	 ;; in characters - e.g. ARG1/neq
	 (node-x 0) ;; tmp variable - centre of node
	 (start-x 0) ;; tmp variable - start of node
	 (next-x 0) ;; tmp variable - start of next node
	 (tmp-links nil) ;; list organised by length
	 (final-links nil)
	 (layout-nodes
	  (loop for node in (sort (copy-list (dmrs-nodes dmrs)) 
				  #'sort-dmrs-pred)
	      collect
		(let* ((pred (dmrs-node-pred node))
		       (id (dmrs-node-id node))
		       (predstring
			(format nil "~A~A~A"
				(if (realpred-p pred)
				    (convert-realpred-to-string
				     (realpred-lemma pred)
				     (realpred-pos pred)
				     (realpred-sense pred))
				  pred)
				(if (dmrs-node-carg node)
				    "/" "")
				(or (dmrs-node-carg node)
				    "")))
		       (node-width (length predstring)))
		  (setf start-x next-x)
		  (setf node-x (+ next-x (truncate node-width 2)))
		  (setf next-x (+ next-x node-width x-increment))
		  (make-dmrs-layout-node 
		   :id id
		   :label predstring
		   :start-x start-x
		   :mid-x node-x)))))
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
      (setf (cdr length-set) 
	(sort (cdr length-set) 
	      #'< :key #'dmrs-layout-link-left-x)))
    (setf tmp-links (sort tmp-links #'< :key #'car))
    (let ((current-y 0))
      (when tmp-links
	(loop 
	  (multiple-value-bind 
	      (done-links new-tmp) 
	      (fit-all-links tmp-links current-y)
	    (setf final-links (append final-links done-links))
	    (unless new-tmp (return))
	    (setf tmp-links new-tmp)
	    (setf current-y (+ current-y 1))))))
    (ecase layout-type
      (:clim
       (lkb::construct-dmrs-clim-diagram layout-nodes final-links stream))
      (:latex 
       (construct-dmrs-latex-diagram layout-nodes final-links stream)))))

(defun sort-dmrs-pred (node1 node2)
  ;;; sortal order - < cfrom, then cto, then real 
  ;;; before construction, then (assume both construction)
  ;;; quantifier before non-quantifier, then alphabetic
  ;;;
  ;;; not entirely optimal
  (let ((cfrom1 (dmrs-node-cfrom node1))
	(cfrom2 (dmrs-node-cfrom node2)))
    (if (or (null cfrom1) (null cfrom2) (eql cfrom1 cfrom2))
	(let ((cto1 (dmrs-node-cto node1))
	      (cto2 (dmrs-node-cto node2)))
	  (if (or (null cto1) (null cto2) (eql cto1 cto2))
	      (let* ((pred1 (dmrs-node-pred node1))
		     (pred2 (dmrs-node-pred node2)))
		(if (and (realpred-p pred1)
			 (realpred-p pred2))
		    (string-lessp (realpred-lemma pred1)
				  (realpred-lemma pred2))
		  (if (and (not (realpred-p pred1))
			   (not (realpred-p pred2)))
		      (let ((qp1 (quantifier-pred-p pred1))
			    (qp2 (quantifier-pred-p pred2)))
			(if (or qp1 qp2 (not (and qp1 qp2)))
			    qp1
			  (string-lessp pred1 pred2)))
		    (realpred-p pred1))))
	    (< cto1 cto2)))
      (< cfrom1 cfrom2))))

(defun mark-ltop-node (node-id nodes)
  (let ((node (find node-id nodes :key #'dmrs-layout-node-id)))
    (when node (setf (dmrs-layout-node-ltop node) t))))

(defun find-dmrs-node-x (node-id nodes)
  (let ((node (find node-id nodes :key #'dmrs-layout-node-id)))
    (if node (dmrs-layout-node-mid-x node))))

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
    (every #'(lambda (fitted-link)
	       (or (<= link-right (dmrs-layout-link-left-x fitted-link)) 
		   (>= link-left (dmrs-layout-link-right-x fitted-link))))
	        fitted-set)))

#|

we now have something like:

(#S(DMRS-LAYOUT-NODE :ID 10001 :LABEL "_the_q" :LTOP NIL :START-X 0 :MID-X 3)
 #S(DMRS-LAYOUT-NODE :ID 10002 :LABEL "_cat_n_1" :LTOP NIL :START-X 16 :MID-X 20)
 #S(DMRS-LAYOUT-NODE :ID 10003 :LABEL "_bark_v_1" :LTOP NIL :START-X 34 :MID-X 38)
 #S(DMRS-LAYOUT-NODE :ID 10004 :LABEL "_sleep_v_1" :LTOP T :START-X 53 :MID-X 58))
(#S(DMRS-LAYOUT-LINK :LABEL "ARG1/EQ" :LEFT-X 20 :RIGHT-X 38 :DIRECTION :L :Y 0)
 #S(DMRS-LAYOUT-LINK :LABEL "RSTR/H" :LEFT-X 3 :RIGHT-X 20 :DIRECTION :R :Y 0)
 #S(DMRS-LAYOUT-LINK :LABEL "ARG1/NEQ" :LEFT-X 20 :RIGHT-X 58 :DIRECTION :L :Y 1))

and we need to render it in our system of choice - and also in
CLIM, though this is never the system of choice ...

CLIM rendering is in lkb-acl-rmrs.lisp

|#

(defun construct-dmrs-latex-diagram (nodes links stream)
  ;;; this produces a picture to be included in a LaTeX document
  (let* ((y-factor 5)
	 (y-start 4)
	 (text-height 3)
	 (offset 2)
	 (picture-height (+ text-height
			    y-start
			    (* y-factor 
			       (+ 1 (loop for link in links
					maximize
					  (dmrs-layout-link-y link))))))
	 (picture-width 
	  (+ 10 ; fudge factor - should really calculate width
	     (loop for node in nodes
		 maximize
		   (dmrs-layout-node-mid-x node))))
	 (text-y (- picture-height text-height)))
    (format stream "\\setlength{\\unitlength}{0.3em}~%")
    (format stream "\\begin{picture}(~A,~A)~%" picture-width picture-height)
    (format stream "\\thicklines~%")
    (dolist (node nodes)
      (let ((x (dmrs-layout-node-mid-x node))
	    (label (dmrs-layout-node-label node)))
	(format stream "\\put(~A,~A){\\makebox(0,0){~A~A}}~%"
		x text-y (latex-escape-string label) 
		(if (dmrs-layout-node-ltop node) "*" ""))))
    (dolist (link links)
      (let* ((left-x (+ offset (dmrs-layout-link-left-x link)))
	     (link-length (- (- (dmrs-layout-link-right-x link)
				(dmrs-layout-link-left-x link))
			     (* 2 offset)))
	     (link-y (- text-y (+ y-start
				  (* y-factor (dmrs-layout-link-y link)))))
	     (label-y (- link-y 2))
	     (label-x (+ left-x (truncate link-length 2)))
	     (direction (dmrs-layout-link-direction link)))
	(format stream "\\put(~A,~A){\\~A(~A,0){~A}}~%"
		(if (eql direction :l) 
		    (+ left-x link-length)
		  left-x)
		link-y
		(if direction "vector" "line")
		(if (eql direction :l) 
		    -1 1)
		link-length)
	(format stream "\\put(~A,~A){\\makebox(0,0){{\\small ~A}}}~%"
		label-x label-y 
		(latex-escape-string (dmrs-layout-link-label link)))))
    (format stream "\\end{picture}~%")))

#|
\setlength{\unitlength}{0.3em}
\begin{picture}(78,17)
\thicklines
\put(3,14){\makebox(0,0){\_the\_q}}
\put(20,14){\makebox(0,0){\_cat\_n\_1}}
\put(38,14){\makebox(0,0){\_purr\_v\_1}}
\put(58,14){\makebox(0,0){\_sleep\_v\_1}}
\put(5,10){\vector(1,0){13}}
\put(12,8){\makebox(0,0){{\small RSTR}}}
\put(36,10){\vector(-1,0){14}}
\put(29,8){\makebox(0,0){{\small ARG1/eq}}}
\put(56,5){\vector(-1,0){34}}
\put(39,3){\makebox(0,0){{\small ARG1/neq}}}
\end{picture}
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; RMRS to DMRS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(let ((*package* (find-package :mrs)))
  (with-open-file (istream "~/lingo/lkb/src/rmrs/eg.rmrs" 
		   :direction :input)
      (let ((rmrs (parse-xml-removing-junk istream)))
	(when (and rmrs (not (xml-whitespace-string-p rmrs)))
	  (let ((dmrs (rmrs-to-dmrs (read-rmrs rmrs nil))))
	    (when dmrs (simple-output-dmrs dmrs)))))))
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
which is qeq the label of P', then the post-slash label is h.  
If the hole argument is eq the label of P', then heq. If
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
a hole value h, such that h is qeq (or =) a label lb which
labels a set of predicates S:

a) If P has exactly one argument A' with a value y such that P' = \charp(y) 
is a member of S, then there is an ARG/h (or ARG/heq) arc from 
P to P', where ARG is the argument type of A. (Note that 
this covers the RSTR case because there will be a BV argument.)

this condition is checked by char-var-selects

b) Otherwise, there are ARG/h (or ARG/heq) arcs from P to each of the 
head predicates in S.

make-dmrs-handel-links

8 If the preslash label on the arc uniquely determines the
  postslash label, the slash and the postslash label may be omitted.
(e.g., we write RSTR, rather than RSTR/h).

FIX - not done (output only)
|#

(defun rmrs-to-dmrs (rmrs) 
  ;;; convert an RMRS to a DMRS
  (let* ((nodes (extract-rmrs-nodes rmrs))
	 (dup-errors
	  (check-char-vars nodes)))
    (if dup-errors 
	(progn (format t "~A~%" dup-errors)
	       nil)
      (let* ((var-links (extract-rmrs-var-links rmrs nodes))
	     (unlinked-errors
	      (check-char-vars2 var-links nodes)))
	(if unlinked-errors 
	    (progn (format t "~A~%" unlinked-errors)
		   nil)
	  (let*
	      ((dgroups (determine-label-equality-sets nodes var-links)) 
	        ;; nodes which share a label
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
			dmrs))))))
    
;;;      (pprint dgroups)
;;;      (simple-output-dmrs dmrs)
;;;      (layout-dmrs dmrs))))

(defun extract-rmrs-nodes (rmrs)
  ;;; FIX - the char var test is whether there's an ARG0 class variable
  ;;; and the predicate is not a quantifier.  This is probably too generous.
  (let ((nodes
	 (loop for rel in (rmrs-liszt rmrs)
	     collect
	       (let ((pred (rel-pred rel))) 
	       (make-dmrs-node
		:id (var-id (rel-anchor rel))
		:pred pred
		:cfrom (rel-cfrom rel)
		:cto (rel-cto rel)
		:charvar (if (if (realpred-p pred)
				 (not (equal (realpred-pos pred) 
					     "q"))
			       (not (quantifier-pred-p (rel-pred rel))))
			     (var-id (car (rel-flist rel))))
		:label (var-id (rel-handel rel)))))))
    (dolist (rmrs-arg (rmrs-rmrs-args rmrs))
      (when (and (stringp (rmrs-arg-val rmrs-arg))
	       (equal (rmrs-arg-arg-type rmrs-arg) "CARG"))
	  (let* ((node (car (member (var-id (rmrs-arg-label rmrs-arg))
				    nodes :key #'dmrs-node-id))))
	    (when (and node (dmrs-node-p node))
	      (setf (dmrs-node-carg node)
		(rmrs-arg-val rmrs-arg))))))
    nodes))


(defun quantifier-pred-p (pred)
  (let ((qpos (position  #\q pred)))
    (and qpos (> qpos 0) (< qpos (- (length pred) 1))
	 (char= (elt pred (- qpos 1)) #\_)
	 (char= (elt pred (+ qpos 1)) #\_))))
	

(defun check-char-vars (nodes)
  (let ((vars nil)
	(duplicates nil))
    (dolist (node nodes)
      (let ((char-var (dmrs-node-charvar node)))
	(if (and char-var (member char-var vars))
	    (pushnew char-var duplicates)
	  (push char-var vars))))
    (if duplicates
      (format nil "~%Duplicate char vars ~A in ~A" duplicates nodes))))

(defun extract-rmrs-var-links (rmrs nodes)
  (loop for rmrs-arg in (rmrs-rmrs-args rmrs)
      unless (or (not (var-p (rmrs-arg-val rmrs-arg)))
		 (equal (var-type (rmrs-arg-val rmrs-arg)) "h")
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

(defun check-char-vars2 (links nodes)
  (let ((unlinked nil))
    (dolist (link links)
      (unless (dmrs-link-to link)
	(push (cons (dmrs-link-from link) (dmrs-link-pre link))
	      unlinked)))
    (if unlinked
      (format nil "~%Unlinked vars ~A in ~A" unlinked nodes))))


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
      when (and (var-p (rmrs-arg-val rmrs-arg))
		(equal (var-type (rmrs-arg-val rmrs-arg)) "h"))
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
	       (target-label (if qeq (var-id (hcons-outscpd qeq))
			       hole-id))
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
		 :post (if qeq :h :heq))))))

(defun char-var-selects (from-node-id elements rmrs nodes)
  (let* ((main-args
	  (loop for rmrs-arg in (rmrs-rmrs-args rmrs)
	      when (and (eql (var-id (rmrs-arg-label rmrs-arg)) from-node-id)
			(not (equal (var-type (rmrs-arg-val rmrs-arg)) "h")))
              append
		(let* ((var (rmrs-arg-val rmrs-arg))
		       (var-id (var-id var))
		       (to-node
			(car (member var-id nodes 
				     :key #'dmrs-node-charvar))))
		  (if to-node
		      (list (dmrs-node-id to-node))))))
	 (inherent-args
	  (loop for rmrs-rel in (rmrs-liszt rmrs)
	      when (eql (var-id (rel-anchor rmrs-rel)) from-node-id)
	      append
		(let* ((var (car (rel-flist rmrs-rel)))
		       (var-id (var-id var))
		       (to-node
			(car (member var-id nodes 
				     :key #'dmrs-node-charvar))))
		  (if to-node
		      (list (dmrs-node-id to-node))))))
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
