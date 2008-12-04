;;; Copyright (c) 2008
;;;   Ann Copestake
;;;   see `licence.txt' for conditions.

(in-package :mrs)

;;; DMRS structure

(defstruct (dmrs)
  nodes
  links)

(defstruct dmrs-node
  id
  pred
  cfrom
  cto
  charvar
  label)

(defstruct dmrs-link
  from to pre post)

(defstruct dmrs-ngroup
  label
  elements
  targets)


;;; (defstruct realpred
;;;  lemma
;;;  pos
;;;  sense)

#|
(let ((*package* (find-package :mrs)))
  (with-open-file (istream "~/lingo/lkb/src/rmrs/eg.rmrs" 
		   :direction :input)
    (loop
      (let ((rmrs (parse-xml-removing-junk istream)))
	    (when (and rmrs (not (xml-whitespace-string-p rmrs)))
	      (rmrs-to-dmrs (read-rmrs rmrs nil)))))))
|#

(defun rmrs-to-dmrs (rmrs)
  (let ((nodes (extract-rmrs-nodes rmrs)))
    ;; use the anchors as ids
    (check-char-vars nodes)
    (let* ((var-links (extract-rmrs-var-links rmrs nodes))
	   (dgroups (determine-label-equality-sets nodes var-links))
	   (eqlinks (make-dmrs-eq-links var-links dgroups))
	   (hlinks (make-dmrs-handel-links rmrs dgroups nodes))
	   (ltoplinks (make-dmrs-ltop-links rmrs dgroups))
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
      (pprint dgroups)
      (output-dmrs dmrs))))

(defun output-dmrs (dmrs)
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
	;; BODY args won't have a qeq
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

    