;;; Copyright Ann Copestake 1992-1998
;;; All Rights Reserved.
;;; No use or redistribution without permission.
;;; 
;;; Ann Copestake

(in-package :cl-user)

;;; generator output functions which are not dialect specific

(defun show-gen-result nil
   (if *gen-record*
      (draw-active-list
         (sort
            (mapcar
               #'(lambda (edge)
                   (cons (format nil "~{~A~^ ~}" 
                                 (fix-spelling (g-edge-leaves edge)))
                      edge))
               *gen-record*)
            #'string-lessp :key #'car)
         "Generated Sentences"
         (list
            (cons "Feature structure"
               #'(lambda (edge)
                   (display-fs (g-edge-dag edge)
                      (format nil "Edge ~A G - Tree FS" (g-edge-id edge)))))
            (cons "Edge"
               #'(lambda (edge)
                   (display-parse-tree edge nil)))))
      (format t "~%No strings generated")))

#|     
      (let ((possible-edge-name
               (ask-for-lisp-movable "Current Interaction" 
                  `(("No generation results - specify an edge number" . ,*edge-id*)) 60)))
         (when possible-edge-name
            (let* ((edge-id (car possible-edge-name))
                   (edge-record (find-gen-edge-given-id edge-id)))
               (when edge-record 
                  (display-parse-tree edge-record nil)))))))
|#
            
(defun show-gen-edge nil
   (let ((possible-edge-name
            (ask-for-lisp-movable "Current Interaction" 
               `(("Specify an edge number" . ,*edge-id*)) 60)))
      (when possible-edge-name
         (let* ((edge-id (car possible-edge-name))
                (edge-record (find-gen-edge-given-id edge-id)))
            (when edge-record 
               (display-parse-tree edge-record t))))))


(defun find-gen-edge-given-id (edge-id)
   (dolist (entry *gen-chart*)
      (dolist (edge (cdr entry))
         (when (eql edge-id (edge-id edge))
            (return-from find-gen-edge-given-id edge)))))


;;; parse output functions which are not dialect specific

(defun show-parse nil
  (if *parse-record*
      (with-parser-lock ()
	#+(and :allegro :clim)(show-parse-tree-frame *parse-record*)
	#-(and :allegro :clim) 
	(dolist (edge *parse-record*)
	  (display-parse-tree edge nil))
	;; replace old tree display with new frame in ACL CLIM
	;; FIX need to replicate in MCL and CG
	(when (fboundp 'mrs::output-mrs-after-parse)
	  (funcall 'mrs::output-mrs-after-parse *parse-record*)))
    (progn
      (lkb-beep)
      (format t "~%No parses found"))))

#|     
     (let ((possible-edge-name
               (ask-for-lisp-movable "Current Interaction" 
                  `(("No parses - specify an edge number" . ,*edge-id*)) 60)))
         (when possible-edge-name
            (let* ((edge-id (car possible-edge-name))
                  (edge-record (find-edge-given-id edge-id)))
               (when edge-record 
                  (display-parse-tree edge-record nil)))))))
|#
                  
(defun show-parse-edge nil
   (let ((possible-edge-name
            (ask-for-lisp-movable "Current Interaction" 
               `(("Specify an edge number" . ,*edge-id*)) 60)))
      (when possible-edge-name
         (let* ((edge-id (car possible-edge-name))
               (edge-record (find-edge-given-id edge-id)))
            (when edge-record 
               (display-parse-tree edge-record t))))))

;;; display-parse-tree is dialect specific

(defun find-edge-given-id (edge-id)
  (or
   (dotimes (i *chart-limit*)
      (let ((chart-entry (aref *chart* i 0)))
         (if (chart-entry-p chart-entry) 
            (let ((edge 
                     (for config in (chart-entry-configurations chart-entry) 
                        car-filter
                        (let ((edge (chart-configuration-edge config)))
                           (if (eql edge-id (edge-id edge))
                              edge)))))
               (when edge (return edge))))))
   (find edge-id *morph-records* :key #'edge-id)))


;;; called from display-parse-tree

(defun make-edge-symbol (edge-id &optional morph-p)
   ;; create it uninterned so data put on to its property list doesn't hang
   ;; around after all other references to it have gone
   (make-symbol
      (if (stringp edge-id)
         edge-id
         (format nil "~A~A" (if morph-p 'medge 'edge) edge-id))))


(defun find-category-abb (fs)
  ;;; Two versions of this - one as in the original LKB and another 
  ;;; which is for page emulation.
  ;;; The LKB version is simple - it
  ;;; checks to see whether fs is subsumed by any of the
  ;;; special templates which are listed in *category-display-templates*
  ;;; and uses the template name as a symbol if it does
  ;;;
  ;;; The PAGE emulation version relies on unification
  ;;; a - the template's type is ignored 
  ;;; b - the unification is checked on a portion of the FS
  ;;;     reached by the *label-fs-path*
  ;;; c - there are two types of templates - label and meta
  ;;;     The label templates provide the first half of the node label
  ;;;     then the meta template is checked - if this is satisfied,
  ;;;     the path *recursive-path* is followed into the fs
  ;;;     and this is checked against the *local-path*
  ;;;     of the label nodes, and so on recursively
  ;;;     This gives nodes like S/NP

  ;;; Longer term, rules should be indexed by these categories.
   (or (cdr (assoc fs *cached-category-abbs*))
       (let ((abb
              (if (and (eql *lkb-system-version* :page)
                       (not *simple-tree-display*))
                 (calculate-tdl-label fs)
                 (dolist (tmpl *category-display-templates*)
                    (let* ((tmpl-entry (get-psort-entry tmpl))
                           (tmpl-fs (if tmpl-entry (tdfs-indef (lex-or-psort-full-fs tmpl-entry)))))
                       (when (and tmpl-fs (dag-subsumes-p tmpl-fs (tdfs-indef fs)))
                          (return tmpl)))))))
          (push (cons fs abb) *cached-category-abbs*)
          abb)))
        

;;; code after this point is for the PAGE simulation version

; structures and globals

(defvar *label-display-templates* nil)
(defvar *meta-display-templates* nil)

(defstruct (label-template)
  label
  fs)

(defstruct (meta-template)
  prefix
  suffix
  fs)

;;; Initialisation stuff

(defun split-up-templates nil
  ; called when *category-display-templates*
  ; is set up by the code in tdllexinput.lsp
  (setf *label-display-templates* nil)
  (setf *meta-display-templates* nil)
  (for tmpl in *category-display-templates*
       do
       (let* ((tmpl-entry (get-psort-entry tmpl))
              (tmpl-fs (if tmpl-entry 
                           (tdfs-indef 
                                  (lex-or-psort-full-fs tmpl-entry)))))
         (if tmpl-fs 
           (if (label-template-fs-p tmpl-fs)
               (push (make-label-template
                      :fs tmpl-fs 
                      :label (get-string-path-value tmpl-fs *label-path*
                                                   tmpl))
                     *label-display-templates*)
             (push (make-meta-template
                    :fs tmpl-fs 
                    :prefix (get-string-path-value tmpl-fs *prefix-path*
                                                  tmpl)
                    :suffix (get-string-path-value tmpl-fs *suffix-path*
                                                  tmpl))
                   *meta-display-templates*))
           (format t "~%Warning: no valid fs for ~A" tmpl)))))


(defun label-template-fs-p (fs)
  (let ((type (type-of-fs fs)))
    (subtype-or-equal (if (listp type) (car type) type) 
         *label-template-type*)))

; extracting label string

(defun get-string-path-value (tmpl-fs path tmpl)
  ;;; it is an error for the structure not to have the 
  ;;; feature which has been declared to provide the label name
  ;;; and for this not to be a string
  ;;; If this occurs, a warning message is printed
  ;;; and the template name is used instead
  (if path
      (let* ((dag-found (existing-dag-at-end-of tmpl-fs path))
             (dag-value (if dag-found (type-of-fs dag-found)))
             (label (if (stringp (car dag-value)) 
                        (car dag-value))))
        (or label
            (progn 
              (format t "~%Warning: no ~A in ~A, template name used instead"
                      path tmpl)
              (string tmpl))))
    ""))


;;; Calculating a tree node label for a fs

(defun calculate-tdl-label (fs)
  (let ((fs-node (existing-dag-at-end-of (tdfs-indef fs) 
                                         *label-fs-path*)))
    (if fs-node
        (string-upcase
         (concatenate 'string 
                      (match-label fs-node)
                      (check-meta fs-node)))
      "UNK")))

; matching the label part

(defun match-label (fs)
  (or
   (dolist (tmpl *label-display-templates*)
     (when (template-match-p (label-template-fs tmpl) fs)
       (return (label-template-label tmpl))))
   "?"))

(defun match-meta-label (fs)
  (or
   (dolist (tmpl *label-display-templates*)
     (when (meta-template-match-p (label-template-fs tmpl) fs)
       (return (label-template-label tmpl))))
   "?"))

; checking for slash etc

(defun check-meta (fs)
  (let ((meta-fs 
         (unless (diff-list-funny-stuff fs *recursive-path*)
           (existing-dag-at-end-of fs *recursive-path*))))
    (if (null meta-fs)
        ""
      (dolist (meta-tmpl *meta-display-templates*)
        (when (template-match-p (meta-template-fs meta-tmpl) fs)
          (return (concatenate 'string 
                               (meta-template-prefix meta-tmpl) 
                               (match-meta-label meta-fs)
                               (meta-template-suffix meta-tmpl))))))))

(defun diff-list-funny-stuff (fs path)
  (let ((diff-list-pos (position *diff-list-list* path)))
    (if diff-list-pos
        (let ((diff-list-fs 
               (existing-dag-at-end-of fs (subseq path 0 diff-list-pos))))
          (and diff-list-fs
               (empty-diff-list-p diff-list-fs))))))

(defun empty-diff-list-p (fs)
  (let ((list-val (existing-dag-at-end-of fs (list *diff-list-list*)))
        (last-val (existing-dag-at-end-of fs (list *diff-list-last*))))
    (eq list-val last-val)))

(defun template-match-p (tmpl-fs fs)
  ;;; the test is whether all the `real' parts of the
  ;;; template fs (i.e. the bits apart from e.g. LABEL-NAME)
  ;;; unify with the node
  (for feat in (get-real-templ-feats tmpl-fs)
       all-satisfy
       (let ((real-templ-fs (get-dag-value tmpl-fs feat))
             (sub-fs (get-dag-value fs feat)))
         (and sub-fs
              (unifiable-wffs-p real-templ-fs sub-fs)))))

(defun meta-template-match-p (tmpl-fs fs)
  ;;; the test is whether all the parts of the
  ;;; template fs after the *local-path*
  ;;; unify with the node or, if there is a null *local-path*,
  ;;; whether the `real' parts of the template unify
  (if *local-path*
      (let ((real-templ-fs (existing-dag-at-end-of tmpl-fs *local-path*)))
        (if real-templ-fs
            (unifiable-wffs-p real-templ-fs fs)))
    (template-match-p tmpl-fs fs)))
  

(defun get-real-templ-feats (tmpl-fs)
  (let ((feats (top-level-features-of tmpl-fs)))
    (set-difference feats (list (car *label-path*)
                                (car *prefix-path*)
                                (car *suffix-path*)))))
                              
;;; JAC added the following to parseout.lsp, but better here


(defun tree-node-text-string (x)
   (let ((full-string
           (typecase x
              (symbol (symbol-name x))
              (string x)
              (t (princ-to-string x)))))
      (if (> (length full-string) 30)
         (subseq full-string 0 30)
         full-string)))

;;; Stuff moved from xxx_specific/chartout.lsp

;;; Graphical display of generator chart (show-gen-chart) (show-gen-chart t)

(defun show-gen-chart (&optional all-p) 
   (let ((root (make-symbol "")))
      (create-gen-chart-pointers root all-p)
      (draw-chart-lattice root
         (format nil "Generator Chart (~A edges)" (if all-p "all" "inactive"))
         t)
      root))


(defun create-gen-chart-pointers (root all-p)
   ;; create a global mapping from edge-ids to symbols, not interned - so we don't
   ;; end up hanging on to old edges
   (let ((edge-symbols nil))
      (dolist (entry *gen-chart*)
         (dolist (e (cdr entry))
            (push
               (list* (dotted-edge-id e)
                  (make-edge-symbol (dotted-edge-id e))
                  (dotted-edge-needed e))
               edge-symbols)))
      (dolist (entry *gen-chart*)
         (let ((chart-index (string-downcase (symbol-name (car entry)))))
            (dolist (e (cdr entry))
               (let ((edge-symbol
                        (cadr (assoc (dotted-edge-id e) edge-symbols))))
                  (setf (get edge-symbol 'chart-edge-span)
                     (if (dotted-edge-needed e)
                        (concatenate 'string chart-index " A") chart-index))
                  (setf (get edge-symbol 'chart-edge-contents) e)
                  (if (dotted-edge-children e)
                     (dolist (c (dotted-edge-children e))
                        (when c
                           (push edge-symbol
                              (get (cadr (assoc (dotted-edge-id c) edge-symbols))
                                 'chart-edge-descendents))))
                     (push edge-symbol (get root 'chart-edge-descendents)))))))
      (unless all-p
         ;; remove intermediate links consisting of active edges
         (dolist (pair edge-symbols)
            (setf (get (cadr pair) 'chart-edge-descendents)
               (create-gen-chart-pointers-collapse
                  (get (cadr pair) 'chart-edge-descendents)
                  edge-symbols))))))


(defun create-gen-chart-pointers-collapse (nodes edge-symbols)
   (mapcan
      #'(lambda (node)
          (if (cddr (find node edge-symbols :key #'cadr))
             (create-gen-chart-pointers-collapse
                (get node 'chart-edge-descendents) edge-symbols)
             (list node)))
      nodes))


;;; Graphical display of parse chart (show-chart)

(defun show-chart nil 
   (let ((root (make-symbol "")))
         (setf (get root 'chart-edge-descendents)
            (make-array *chart-limit* :initial-element nil))
         (let*
            ((end (create-chart-pointers root))
             (word-alt-sets
                ;; each element is a set to allow for multi-word lexical entries
                ;; at each position in input
                (coerce (subseq (get root 'chart-edge-descendents) 0 end) 'list)))
            (setf (get root 'chart-edge-descendents) (apply #'append word-alt-sets))
            (draw-chart-lattice root
               (format nil "Parse Chart for \"~A...\""
                  (car (get root 'chart-edge-descendents)))
               t)
            root)))


(defun create-chart-pointers (root)
   ;; create a global mapping from edge-ids to symbols, and also (below) a local
   ;; one (per-string position) from lexical items to symbols, neither set
   ;; of symbols interned - so we don't end up hanging on to old edges
   (let ((edge-symbols nil))
      (dotimes (vertex (1- *chart-limit*))
         (when (aref *chart* (1+ vertex) 0)
           (dolist (span (chart-entry-configurations 
                          (aref *chart* (1+ vertex) 0)))
               (let ((e (chart-configuration-edge span)))
                  (push
                     (cons (edge-id e) (make-edge-symbol (edge-id e)))
                     edge-symbols)))))
      (dotimes (vertex (1- *chart-limit*))
         (if (aref *chart* (1+ vertex) 0) 
             (create-chart-pointers1 (1+ vertex) root edge-symbols)
           (unless (aref *morphs* (1+ vertex))
            (return vertex))))))


(defun create-chart-pointers1 (right-vertex root edge-symbols)
   (let ((lex-pairs nil))
      (dolist (span (chart-entry-configurations (aref *chart* right-vertex 0)))
         (let*
            ((e (chart-configuration-edge span))
             (edge-symbol (cdr (assoc (edge-id e) edge-symbols))))
            (setf (get edge-symbol 'chart-edge-span)
              (format nil "~A-~A" 
                      (chart-configuration-begin span) right-vertex))
            (setf (get edge-symbol 'chart-edge-contents) e)
            (if (edge-children e)
               (dolist (c (edge-children e))
                  (when c
                     (push edge-symbol
                        (get (cdr (assoc (edge-id c) edge-symbols))
                           'chart-edge-descendents))))
               (let*
                  ((lex (car (edge-leaves e)))
                   (pair (assoc lex lex-pairs :test #'equal)))
                  (unless pair
                     (push (setq pair (cons lex (make-symbol lex))) lex-pairs))
                  (push edge-symbol (get (cdr pair) 'chart-edge-descendents))
                  (pushnew (cdr pair)
                           (aref (get root 'chart-edge-descendents) 
                                 (1- right-vertex)))))))))


;;; make a copy of an existing root and descendent chart lattice, filtered such that
;;; only edges which are ancestors or descendents of given edge are present

(defun filtered-chart-lattice (node edge found)
   ;; the plist found keeps track of nodes that have already been processed,
   ;; recording their new names
   (labels
      ((super-chart-edge-path-p (e)
          ;; path from e recursively through children to edge?
          (and e ; don't blow up on active edges
             (or (eq e edge)
                (some #'super-chart-edge-path-p (edge-children e)))))
       (sub-chart-edge-path-p (e edge)
          ;; path from edge recursively through children to e?
          (and edge
             (or (eq e edge)
                (some #'(lambda (c) (sub-chart-edge-path-p e c)) (edge-children edge))))))
      (cond
         ((not (or (null (get node 'chart-edge-contents))
                   (super-chart-edge-path-p (get node 'chart-edge-contents))
                   (sub-chart-edge-path-p (get node 'chart-edge-contents) edge)))
            (values nil found))
         ((getf found node)
            (values (getf found node) found))
         (t
            (let ((new (make-symbol (symbol-name node))))
               (setq found (list* node new found))
               (let ((new-ds nil))
                  (dolist (d (get node 'chart-edge-descendents))
                     (multiple-value-bind (new-d new-found)
                                          (filtered-chart-lattice d edge found)
                        (setq found new-found)
                        (when new-d
                           (setf (get new-d 'chart-edge-span) (get d 'chart-edge-span))
                           (setf (get new-d 'chart-edge-contents) (get d 'chart-edge-contents))
                           (push new-d new-ds))))
                  (setf (get new 'chart-edge-descendents) (nreverse new-ds)))
               (values new found))))))


;;; takes an edge and builds the tree below it for input
;;; to the graph package - then displays it with active nodes

(defun display-parse-tree (edge display-in-chart-p)
  (when display-in-chart-p 
    (display-edge-in-chart edge))
  (let ((edge-symbol (make-new-parse-tree edge 1)))
    (draw-new-parse-tree edge-symbol 
			 (format nil "Edge ~A ~A" (edge-id edge) 
				 (if (g-edge-p edge) "G" "P"))
			 nil)))
   
(defun make-new-parse-tree (edge level)
  (with-unification-context (nil)
    (copy-parse-tree (rebuild-edge (car (make-new-parse-tree1 edge level))))))

(defun make-new-parse-tree1 (edge level)
  ;; show active edge nodes at first level but not thereafter
  (if (and (> level 1) 
	   (dotted-edge-p edge) 
	   (dotted-edge-needed edge))
      (mapcan #'(lambda (c) 
		  (when c 
		    (make-new-parse-tree1 c (1+ level))))
	      (edge-children edge))
    (let ((edge-symbol (make-edge-symbol (edge-id edge)))
	  (daughters (edge-children edge)))
      (setf (get edge-symbol 'edge-record) edge)
      (setf (get edge-symbol 'daughters) 
	(if daughters
	    (mapcan #'(lambda (dtr)
			(if dtr
			    (make-new-parse-tree1 dtr (1+ level))
			  ;; active chart edge daughter
			  (list (make-symbol ""))))
		    daughters)
	  (make-lex-and-morph-tree edge 1)))
      (list edge-symbol))))
  
(defun make-lex-and-morph-tree (edge level)
  (let ((leaf-symbol (make-edge-symbol (car (edge-leaves edge))))
	(mdaughter (edge-morph-history edge)))
    (when (> level 1) 
      (setf (get leaf-symbol 'edge-record) edge))
    (when mdaughter
      (setf (get leaf-symbol 'daughters)
	(make-lex-and-morph-tree mdaughter (1+ level))))
    (list leaf-symbol)))

;;
;; Reconstruct a parse from the chart 
;;

(defun rebuild-edge (edge-symbol)
  (let* ((edge (get edge-symbol 'edge-record))
         (rule (and edge (edge-rule edge)))
         (dtrs (mapcar #'rebuild-edge (get edge-symbol 'daughters))))
    (if edge
	(setf (get edge-symbol 'edge-fs)
	  (if (rule-p rule)
	      (reapply-rule rule dtrs (edge-spelling-change edge))
	    (copy-tdfs-completely (edge-dag edge))))
      (setf (get edge-symbol 'edge-fs) (get (car dtrs) 'edge-fs))))
  edge-symbol)

(defun reapply-rule (rule daughters nu-orth)
  ;; Since all the tree unifications are in one big unification
  ;; context, we need to make a copy of each rule each time it is used
  (let ((rule-dag (copy-tdfs-completely (rule-full-fs rule))))
    ;; Re-do rule unifications
    (loop 
	for path in (cdr (rule-order rule))
        for dtr in daughters
	as dtr-fs = (get dtr 'edge-fs)
	do (when dtr-fs
	     (setf rule-dag
	       (yadu rule-dag (create-temp-parsing-tdfs dtr-fs path)))))
    (unless rule-dag (error "Unifications failed to reunify"))
    ;; Re-do spelling change
    (let ((orth-fs (when nu-orth 
		     (copy-tdfs-completely (get-orth-tdfs nu-orth))))
          (mother-fs (tdfs-at-end-of (car (rule-order rule)) rule-dag)))
      (when orth-fs
	(setf mother-fs (yadu mother-fs orth-fs)))
      (unless mother-fs (error "Orthography failed to reunify"))
      ;; Return the result
      mother-fs)))

(defun copy-parse-tree (edge-symbol)
  (when (get edge-symbol 'edge-fs)
    (setf (get edge-symbol 'edge-fs) 
      (copy-tdfs-elements (get edge-symbol 'edge-fs))))
  (mapc #'copy-parse-tree (get edge-symbol 'daughters))
  edge-symbol)

(defun get-string-for-edge (edge-symbol)
  (let ((edge-record (get edge-symbol 'edge-record)))
     (if edge-record
	 (progn
	   (values (tree-node-text-string 
		    (or (when (get edge-symbol 'edge-fs)
			  (find-category-abb (get edge-symbol 'edge-fs)))
			(edge-category edge-record))) nil))
       (values (tree-node-text-string edge-symbol) t))))


;;; convert tree into a nested list - for simple printing of structure
;;; (dolist (parse *parse-record*) (pprint (parse-tree-structure parse)))

(defun parse-tree-structure (edge)
   (car (parse-tree-structure1 edge 1)))

(defun parse-tree-structure1 (edge level)
  ;; show active edge nodes at first level but not thereafter
  (if (and (> level 1) 
	   (dotted-edge-p edge) 
	   (dotted-edge-needed edge))
      (mapcan #'(lambda (c) 
		  (when c 
		    (parse-tree-structure1 c (1+ level))))
	      (edge-children edge))
      (let ((daughters (edge-children edge)))
         (list
          (if (and *dont-show-lex-rules*
                   (lexical-rule-p (edge-rule edge)))
             (car (edge-leaves edge))
             (if daughters
                (cons (tree-node-text-string
                       (or (find-category-abb 
                            (edge-dag edge))
                           (edge-category edge)))
                  (mapcan
                   #'(lambda (dtr)
                       (if dtr
                          (parse-tree-structure1 dtr (1+ level))
                          ;; active chart edge daughter
                          (list ""))) 
                   daughters))
                (if *dont-show-morphology*
                   (car (edge-leaves edge))
                   (cons (car (edge-leaves edge))
                     (morph-tree-structure
                        (edge-rule edge) (edge-morph-history edge))))))))))

(defun morph-tree-structure (rule edge)
   (if rule
      (cons (if (rule-p rule) (rule-id rule) rule)
         (if edge
            (morph-tree-structure nil (edge-morph-history edge))))))

