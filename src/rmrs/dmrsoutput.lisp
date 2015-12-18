;;; Creating DMRS structures directly from results of parse

(in-package :mrs)

;;; Assumptions about the grammar
;;;
;;; AS MRS
;;;
;;; 1. fragmentness is indicated by *root-path* not being
;;;    *true-type*
;;;
;;; 2. There is a single `semantic' feature structure
;;; which can be used to build the DMRS at the end of
;;;  *initial-semantics-path*
;;;
;;; analogous to MRS but with nodes replacing relations and variables
;;; 3. 
;;;    *psoa-liszt-path* - list of nodes
;;;    *psoa-top-h-path* - pointer to node
;;;    *psoa-index-path* - node
;;;
;;;
;;;    a. determine-variable-type controls the letter used
;;;    to type the node - see below
;;;    c. `extra' information associated with a node is
;;;    built using create-index-property-list
;;;    FIX - not done yet
;;;    all extra information must be represented by a path
;;;    with an atomic value terminating it
;;;    all features are valid as extras, except those which are in 
;;;    *ignored-extra-features*
;;;
;;; 7. nodes
;;;    a. represented via a first/rest structure (*first-path* and
;;;    *rest-path*)
;;;    b. relation predicate is either 
;;;       value of first element of *rel-name-path* 
;;;                  OR, if this doesn't exist
;;;       type of fs
;;;    c. type is recorded via nodetype feature (built in)
;;;    d. links represented by features
;;;       arg1 arg2 arg3 rstr (built in)
;;;       pointing to a node of type h-link, eq-link, neq-link 
;;;       (built in) to code, with a feature TARGET (built in)
;;;       pointing to the `to' node
;;;
;;;
;;;       FIX - CARG eventually?
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

;;; for simplicity redefined show-mrs-dmrs-window-really 
;;; to extract a DMRS if lkb::*dmrs-grammar-p* is set
;;;  (setf lkb::*dmrs-grammar-p* t)

(defparameter *dmrs-nodes* nil)
(defparameter *dmrs-link-list* nil)

(defun extract-dmrs (parse)
  ;;; takes whatever structure the parser returns
  (let ((fs (get-parse-fs parse))) ;; get-parse-fs is system specific
    (when (is-valid-fs fs)
	  (extract-dmrs-from-fs fs))))
         
(defun extract-dmrs-from-fs (fs)
  (let ((sem-fs (path-value fs *initial-semantics-path*)))
    (setf *fragment-p* nil)
    ;; for MRS *fragment-p* controls whether the scoping code is run
    ;; we don't want that for DMRS yet
    (if (is-valid-fs sem-fs)
        (construct-dmrs sem-fs nil))))

;;; *restart-variable-generator*
;;; used here for node-id

(defun construct-dmrs (fs &optional existing-variable-generator)
  (if existing-variable-generator
    (setf *variable-generator* existing-variable-generator)
    (when *restart-variable-generator* (init-variable-generator)))
    (setf *dmrs-nodes* nil)
    (setf *dmrs-link-list* nil)
    (let* (
	   ;;(top-h-fs (when *psoa-top-h-path*
           ;;          (path-value fs *psoa-top-h-path*)))
           ;;(index-fs (path-value fs *psoa-index-path*))
         (node-list-fs (path-value fs *psoa-liszt-path*))
         (dmrs
          (make-dmrs
	   ;;; FIX
	   ;;; top and index not in dmrs structures yet
           :nodes (nreverse (construct-dmrs-comp-nodes 
                             node-list-fs nil *variable-generator*))
	   ;;; construct partial links as a side-effect of creating nodes
	   ;;; with feature structure recorded for each `to'
	   ;;; then fill in the `to' id
	   :links (loop for link in *dmrs-link-list*
		      collect
			(let* ((link-struct (car link))
			       (to-node-fs (cdr link))
			       (to-node (cdr (assoc to-node-fs *dmrs-nodes*)))
			       (to-id (when (and to-node
						 (dmrs-node-p to-node))
					(dmrs-node-id to-node))))
			  (unless to-id
			    (error "Node uncreated for link ~A" link-struct))
			  (setf (dmrs-link-to link-struct) to-id)
			  link-struct)))))
    dmrs))
;;; ignore VPM for now



;;; ****************************************************
;;;
;;; node list construction
;;;
;;; ****************************************************




(defun construct-dmrs-comp-nodes (fs nodes-list variable-generator)
  (if (is-valid-fs fs)
        (let ((label-list (fs-arcs fs)))
          (if label-list
              (let ((first-part (assoc (car *first-path*)
                                       label-list))
                    (rest-part (assoc (car *rest-path*)
                                      label-list)))
                (if (and first-part rest-part)
                    (let ((node
                           (create-dmrs-comp-node
                            (cdr first-part)
                            variable-generator)))
                      (when node
                        (push node
                              nodes-list))
                      (construct-dmrs-comp-nodes
                       (cdr rest-part)
                       nodes-list variable-generator))
                  nodes-list))
            nodes-list))))


(defun create-dmrs-comp-node (fs gen)
  (when (is-valid-fs fs)
    (let ((existing-node (assoc fs *dmrs-nodes*)))
      (if existing-node
        (cdr existing-node)
        (let* ((idnumber (funcall gen))
;               (var-type (determine-variable-type fs))
;		(extra (create-index-property-list fs))
	       (pred 
		(create-type (extract-pred-from-rel-fs fs)))	       
	       ;;   (original-string (extract-original-string-from-rel-fs fs))
	       (label-list (fs-arcs fs))
	       ;; for side-effect - ignore warning
	       (links (extract-dmrs-links-from-fs 
		       label-list idnumber))
	       (nodetype-val (dolist (pair label-list)
			       (when (eql 'lkb::nodetype (car pair))
				 (return (cdr pair)))))
	       (nodetype (if nodetype-val
			     (determine-variable-type nodetype-val)))
;           (parameter-strings (get-fvps-parameter-strings fvps))
;           (cfrom (extract-cfrom-from-rel-fs fs))
;           (cto (extract-cto-from-rel-fs fs))
	       (node (make-dmrs-node :id idnumber
				     :pred pred
				     :cvtype nodetype)))
	  (declare (ignore links))
	       (push (cons fs node) *dmrs-nodes*)
	       node)))))

;; (defun create-index-property-list (fs &optional path-so-far)
;; hope this will work as in MRS


#|
(defstruct dmrs-node
  id
  pred
  cfrom
  cto
  carg ;; constant arguments
  cvtype
  cvextra
)
|#


;;; (defun extract-cfrom-from-rel-fs (fs)
;;; (defun extract-cto-from-rel-fs (fs)
;;; in MRS code - hope this will work

;;; (defun extract-pred-from-rel-fs (rel-fs &key rawp)
;;; seems to work

;;;(defun determine-variable-type (fs)
;;; as MRS



;;; the global list of links constructed
;;; has complete link structures apart from the to id
;;; because we don't necessarily know this when the link is constructed
;;; 
;;; so we store the structure consed to the fs for the to node,
;;; which can then be looked up in the complete node list at the end


#|
(defstruct dmrs-link
  ;;; from and to are DMRS node ids, pre and post make up the arc
  ;;; label
  from to pre post)

post may be :eq :neq :h (or :heq but not in the dmrscomp grammar)
|#

(defun convert-post-type (post-type)
  ;;; similar fn exists in dmrs.lisp
    (cond ((or (equal post-type 'LKB::H-LINK) (equal  post-type :H)) :H)
	((or (equal post-type 'LKB::EQ-LINK) (equal  post-type :EQ)) :EQ)
	((or (equal post-type 'LKB::NEQ-LINK) (equal  post-type :NEQ)) :NEQ)
	((or (equal post-type 'LKB::HEQ-LINK) (equal  post-type :HEQ)) :HEQ)
	(t (error "Unexpected post type ~S" post-type))))


(defun extract-dmrs-links-from-fs (label-list from-id)
  ;;; extracts a combination of links and other features
  (loop for fvp in label-list
      for feature = (car fvp)
      for value = (cdr fvp)
      when (member feature '(lkb::arg1 lkb::arg2 lkb::arg3 lkb::rstr))
      do
	(let ((to-fs (cdr (assoc 'lkb::target (fs-arcs value))))
	      (link
	       (make-dmrs-link 
		:from from-id
		:pre feature
		:post (convert-post-type 
		       (create-type (fs-type value))))))
	  (unless to-fs 
	    (error "Missing to for link"))
	  (push (cons link to-fs) *dmrs-link-list*))))
	       

