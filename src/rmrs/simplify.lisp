;;; Copyright (c) 2008
;;;   Ann Copestake
;;;   see `LICENSE' for conditions.

(in-package :mrs)

;;; DMRS simplification


;;; Complex name simplification

;;; some examples


#|
(setf *simple-test-eg*
  (read-single-dmrs-from-string
   "<dmrs cfrom='-1' cto='-1'>
<node nodeid='10001' cfrom='0' cto='14'><gpred>proper_q_rel</gpred><sortinfo/></node>
<node nodeid='10002' cfrom='0' cto='14'><gpred>compound_rel</gpred><sortinfo cvarsort='e' sf='prop' tense='untensed' mood='indicative' prog='minus' perf='minus'/></node>
<node nodeid='10003' cfrom='0' cto='6'><gpred>proper_q_rel</gpred><sortinfo/></node>
<node nodeid='10004' cfrom='0' cto='6' carg='George'><gpred>named_rel</gpred><sortinfo cvarsort='x' pers='3' num='sg' ind='plus' pt='notpro'/></node>
<node nodeid='10005' cfrom='7' cto='14'><gpred>compound_rel</gpred><sortinfo cvarsort='e' sf='prop' tense='untensed' mood='indicative' prog='minus' perf='minus'/></node>
<node nodeid='10006' cfrom='7' cto='9'><gpred>proper_q_rel</gpred><sortinfo/></node>
<node nodeid='10007' cfrom='7' cto='9' carg='W'><gpred>named_rel</gpred><sortinfo cvarsort='x' pers='3' num='sg' ind='plus' pt='notpro'/></node>
<node nodeid='10008' cfrom='10' cto='14' carg='Bush'><gpred>named_rel</gpred><sortinfo cvarsort='x' pers='3' num='sg' ind='plus'/></node>
<node nodeid='10009' cfrom='15' cto='20'><realpred lemma='fall' pos='v' sense='1'/><sortinfo cvarsort='e' sf='prop' tense='past' mood='indicative' prog='minus' perf='minus'/></node>
<link from='10001' to='10008'><rargname>RSTR</rargname><post>H</post></link>
<link from='10002' to='10008'><rargname>ARG1</rargname><post>EQ</post></link>
<link from='10002' to='10004'><rargname>ARG2</rargname><post>NEQ</post></link>
<link from='10003' to='10004'><rargname>RSTR</rargname><post>H</post></link>
<link from='10005' to='10008'><rargname>ARG1</rargname><post>EQ</post></link>
<link from='10005' to='10007'><rargname>ARG2</rargname><post>NEQ</post></link>
<link from='10006' to='10007'><rargname>RSTR</rargname><post>H</post></link>
<link from='10009' to='10008'><rargname>ARG1</rargname><post>NEQ</post></link>
</dmrs>"
   ))

(dmrs-simplify-complex-names *simple-test-eg*)
|#
;;; *simple-test-eg*


#|

   "<dmrs cfrom='-1' cto='-1'>
<node nodeid='10001' cfrom='0' cto='9'><gpred>proper_q_rel</gpred><sortinfo/></node>
<node nodeid='10002' cfrom='0' cto='9'><gpred>compound_rel</gpred><sortinfo cvarsort='e' sf='prop' tense='untensed' mood='indicative' prog='minus' perf='minus'/></node>
<node nodeid='10003' cfrom='0' cto='3'><gpred>proper_q_rel</gpred><sortinfo/></node>
<node nodeid='10004' cfrom='0' cto='3' carg='Kim'><gpred>named_rel</gpred><sortinfo cvarsort='x' pers='3' num='sg' ind='plus' pt='notpro'/></node>
<node nodeid='10005' cfrom='4' cto='9' carg='Sandy'><gpred>named_rel</gpred><sortinfo cvarsort='x' pers='3' num='sg' ind='plus'/></node>
<node nodeid='10006' cfrom='10' cto='17'><realpred lemma='sleep' pos='v' sense='1'/><sortinfo cvarsort='e' sf='prop' tense='pres' mood='indicative' prog='minus' perf='minus'/></node>
<link from='10001' to='10005'><rargname>RSTR</rargname><post>H</post></link>
<link from='10002' to='10005'><rargname>ARG1</rargname><post>EQ</post></link>
<link from='10002' to='10004'><rargname>ARG2</rargname><post>NEQ</post></link>
<link from='10003' to='10004'><rargname>RSTR</rargname><post>H</post></link>
<link from='10006' to='10005'><rargname>ARG1</rargname><post>NEQ</post></link>
</dmrs>"



"<dmrs cfrom='-1' cto='-1'>
<node nodeid='10001' cfrom='0' cto='14'><gpred>proper_q_rel</gpred><sortinfo/></node>
<node nodeid='10002' cfrom='0' cto='14'><gpred>compound_rel</gpred><sortinfo cvarsort='e' sf='prop' tense='untensed' mood='indicative' prog='minus' perf='minus'/></node>
<node nodeid='10003' cfrom='0' cto='6'><gpred>proper_q_rel</gpred><sortinfo/></node>
<node nodeid='10004' cfrom='0' cto='6' carg='George'><gpred>named_rel</gpred><sortinfo cvarsort='x' pers='3' num='sg' ind='plus' pt='notpro'/></node>
<node nodeid='10005' cfrom='7' cto='14'><gpred>compound_rel</gpred><sortinfo cvarsort='e' sf='prop' tense='untensed' mood='indicative' prog='minus' perf='minus'/></node>
<node nodeid='10006' cfrom='7' cto='9'><gpred>proper_q_rel</gpred><sortinfo/></node>
<node nodeid='10007' cfrom='7' cto='9' carg='W'><gpred>named_rel</gpred><sortinfo cvarsort='x' pers='3' num='sg' ind='plus' pt='notpro'/></node>
<node nodeid='10008' cfrom='10' cto='14' carg='Bush'><gpred>named_rel</gpred><sortinfo cvarsort='x' pers='3' num='sg' ind='plus'/></node>
<node nodeid='10009' cfrom='15' cto='20'><realpred lemma='fall' pos='v' sense='1'/><sortinfo cvarsort='e' sf='prop' tense='past' mood='indicative' prog='minus' perf='minus'/></node>
<link from='10001' to='10008'><rargname>RSTR</rargname><post>H</post></link>
<link from='10002' to='10008'><rargname>ARG1</rargname><post>EQ</post></link>
<link from='10002' to='10004'><rargname>ARG2</rargname><post>NEQ</post></link>
<link from='10003' to='10004'><rargname>RSTR</rargname><post>H</post></link>
<link from='10005' to='10008'><rargname>ARG1</rargname><post>EQ</post></link>
<link from='10005' to='10007'><rargname>ARG2</rargname><post>NEQ</post></link>
<link from='10006' to='10007'><rargname>RSTR</rargname><post>H</post></link>
<link from='10009' to='10008'><rargname>ARG1</rargname><post>NEQ</post></link>
</dmrs>"
|#

;;; Simplifying a file of examples


(defun dmrs-name-simplification-test (ifile ofile)
  ;;; (dmrs-name-simplification-test "rmrs/named_rel_examples.txt" "rmrs/nr-out.xml")
  (with-open-file (istream ifile :direction :input)
		  (let ((dmrs-xml (parse-xml-removing-junk istream)))
		    (when (and dmrs-xml 
			       (not (xml-whitespace-string-p dmrs-xml)))
		      (let ((dmrs-list (read-dmrs-list dmrs-xml)))
			(with-open-file (ostream ofile
					 :direction :output 
					 :if-exists :supersede
					 :if-does-not-exist :create)
			    (dolist (dmrs dmrs-list)
			      (let ((new-struct 
				     (dmrs-simplify-complex-names dmrs)))
				(output-dmrs1 new-struct 'dxml ostream)))))))))


;;; Main functions
;;;
;;; this is also called from lkb-acl-rmrs so it can be run interactively

#|

The name simplification code uses the assumption that a complex name has a tree structure, with the root being a named_rel (usually the surname in a personal name), and binary branches given by compound relations.  The simplified structure has a single named_rel node, with the CARG value corresponding to the names in the branches - it therefore looks like the usual one-element name, except for the complex CARG value. Note that properq quantifiers are also attached to the named_rels and one will be left by this process. 

This therefore calls for a recursive algorithm, where at each named node the compound branches are explored, or we terminate if there are no more branches.  Values which will eventually become the new name are accumulated.  ids for the nodes to be deleted are also accumulated, as are the links.  The actual deletion and new name setting happens when we have got back down to the root.

Apart from the root, no node in the tree should have any other links - if it does, we must not simplify.  If something unexpected is seen, the new-name is set to nil to signal the problem and the process unwinds.  (Note that, in Lisp, it is usually more efficient to do this than to throw an exception, since that means the system has to clean up the stack.)

The behaviour with conjunctions can be complex.  The system starts by finding all terminal nodes, defining this as cases where a node has a link which is not a compound or properq.  There are still some cases where simplification may occur
inside a conjoined name.  

|#


(defun dmrs-simplify-complex-names (dmrs-struct) 
  (let ((name-ids nil) (name-nodes nil)
	(proper-q nil) (compounds nil) 
	(others nil) (compound-nodes nil)
	(name-compounds nil))
    (dolist (node (dmrs-nodes dmrs-struct))
      (let ((id (dmrs-node-id node)))
	(cond ((equal (dmrs-node-pred node) "proper_q_rel") 
	       (push id proper-q))
	      ((equal (dmrs-node-pred node) "compound_rel") 
	       (push id compounds)
	       (push node compound-nodes))
	      ((equal (dmrs-node-pred node) "compound_name_rel") 
	       ;;; older versions of the grammar
	       (push id compounds)
	       (push node compound-nodes))
	      ((equal (dmrs-node-pred node) "named_rel") 
	       (push id name-ids)
	       (push node name-nodes))
	      (t (push id others)))))
    (dolist (node compound-nodes)
      (when (and (member (get-node-target-id node dmrs-struct "ARG1") 
			 name-ids) 
                 (member (get-node-target-id node dmrs-struct "ARG2") 
			 name-ids))
	(push (dmrs-node-id node) name-compounds)))
    ;; 
    (if name-compounds
	;;; we want to try and simplify
	;;;
	;;; find the name parts which are pointed to by something else
	;;; other than proper_q and compound
	;;; there may be several of these in a sentence with multiple 
	;;; names
	(let ((terminal-names nil))
	  (dolist (other-id others)
	    (dolist (target (get-id-all-target-ids 
			     other-id dmrs-struct))
	      (dolist (name-node name-nodes)
		(when (eql target (dmrs-node-id name-node)) 
		  (pushnew name-node terminal-names)))))
	  (if terminal-names
	      (progn 
		(dolist (terminal-name terminal-names)
		  (multiple-value-bind 
		      (new-name delete-nodes delete-links)
			 (dmrs-simplify-complex-name terminal-name
						     name-ids
						     name-compounds
						     compounds
						     proper-q
						     dmrs-struct t)
		    (when new-name
		      ;; no new-name signals it's a weird
		      ;; structure, in which case we don't touch it	
		      (reset-terminal-name-node-values 
		       terminal-name new-name)
		     (delete-nodes-from-dmrs dmrs-struct delete-nodes)
		     (delete-links-from-dmrs dmrs-struct delete-links)
		      )))
		;; return a modified structure
		dmrs-struct)
	        ;;; else    
	        ;;; no terminal names 
	      	;;; unexpected structure - just return untouched structure
	    dmrs-struct))
        ;;; else
	;;; no compound names- just return untouched structure
      dmrs-struct)))

(defun dmrs-simplify-complex-name (name name-ids name-compounds compounds
				   proper-q dmrs-struct first-call-p)
  ;;; on first-call-p we have a node which has links from
  ;;; outside
  (let* ((new-name nil)	(to-delete-nodes nil)
	 (to-delete-links nil)
	 (attached-links (get-node-all-links name dmrs-struct))
	 (attached-proper-q-links 
	  (loop for link in attached-links
	      when (member (dmrs-link-from link) proper-q)
	      collect link))
	 (iffy-compound-links 
	  (loop for link in attached-links
	      when (and (member (dmrs-link-from link) compounds)
		    (not (member (dmrs-link-from link) name-compounds)))
	      collect link))
	 ;;; this detects cases where a compound link exists to a 
	 ;;; conjunction
	 (attached-name-compound-links 
	  (loop for link in attached-links
	      when (member (dmrs-link-from link) name-compounds)
	      collect link))
	 (backward-compound-links
	  (loop for link in attached-name-compound-links
	      when (equal (dmrs-link-pre link) "ARG1")
	      collect link)))
    (if (and  (null iffy-compound-links)
	      attached-proper-q-links 
	       (null (cdr attached-proper-q-links))
	       ;; exactly one proper-q
	       (if first-call-p
		   (eql (length attached-name-compound-links)
			(length backward-compound-links))
		   (eql (length attached-links) 
			(+ 1 (length attached-name-compound-links)))))
      ;;; ensure that there is
      ;;; nothing else attached apart from compounds and properq
      ;;; unless we're processing the terminal node
      ;;; - even with the terminal node, exclude cases where 
      ;;; we have a compound relation which isn't in the name-compounds
      ;;; because this could be a coordination, which gives weird results.
	(progn
	  (dolist (back-link backward-compound-links)
	    (let* ((attached-compound-id (dmrs-link-from back-link)) 
		   (next-up-name-link
		    (get-id-target-link attached-compound-id dmrs-struct
					"ARG2"))
		   (next-up-name-id 
		    (if next-up-name-link
			(dmrs-link-to next-up-name-link)))
		   (next-up-name 
		    (if next-up-name-id
			(get-node-with-id 
			 next-up-name-id
			 dmrs-struct))))
	      (unless next-up-name
	  ;;; something is wrong - bail out, setting new-name to nil
	  ;;; to indicate problem
		(setf new-name nil)
		(return))
	      (multiple-value-bind
		  (name-bits delete-nodes delete-links)
		  (dmrs-simplify-complex-name next-up-name 
					      name-ids name-compounds 
					      compounds
					      proper-q dmrs-struct nil)
		(unless name-bits
		  (setf new-name nil)
		  (return))
		(setf new-name (append name-bits new-name))
		(setf to-delete-links (append delete-links to-delete-links))
		(setf to-delete-nodes (append delete-nodes to-delete-nodes))
		(push attached-compound-id to-delete-nodes)
		(push back-link to-delete-links)
		(push next-up-name-link to-delete-links))))
	;;; nodes for attached-compound, 
	;;; name, the proper_q and all associated links
	;;; are put on the to-delete lists
	  (setf new-name (cons (list (dmrs-node-carg name)
					   (dmrs-node-cfrom name)
					   (dmrs-node-cto name))
				     new-name))
	  (unless first-call-p 
	    (push (dmrs-node-id name) to-delete-nodes)
	    (push (car attached-proper-q-links)
		  to-delete-links)
	    (push (dmrs-link-from (car attached-proper-q-links)) 
		  to-delete-nodes)))
      ;;; else - something is wrong
      (setf new-name nil))
    (values new-name to-delete-nodes to-delete-links)))

(defun reset-terminal-name-node-values (terminal-name new-name)
  (setf (dmrs-node-cfrom terminal-name) 
    (apply #'min (mapcar #'second new-name)))
  (setf (dmrs-node-cto terminal-name) 
    (apply #'max (mapcar #'third new-name)))
  (setf (dmrs-node-carg terminal-name) 
  ;;; we can only tell how the name fits together from
  ;;; the order given by cfrom etc
  ;;; so name-bits is a list of pairs of (carg.cfrom)
  ;;; e.g. (("A" 3 4)("B" 2 3)("D" 6 7)) gives "B A D"
  (let* ((sorted-name-bits (sort new-name #'< :key #'cadr))
	 (name-strings (mapcar #'car sorted-name-bits)))
    ;;; use the iteration construct in format
    (format nil "~A~{ ~A~}" 
	    (car name-strings) (cdr name-strings)))))

