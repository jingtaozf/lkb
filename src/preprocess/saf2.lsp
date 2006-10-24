;; Copyright (c) 2006
;;;   Ben Waldron;
;;; see `licence.txt' for conditions.

(in-package :lkb)

(defvar saf::*gmap*)
(defvar *saf-v* -1)

(defvar *HIDDEN-smaf-id-to-edge-id* nil) ;; MUST reset when initializing tchart
(defvar *smaf-id-to-edge-id* nil) ;; MUST reset when initializing tchart
(defvar *chart-node* -1) ;; MUST reset when initializing tchart
(defvar *smaf-node-to-chart-node* nil) ;; MUST reset when initializing tchart

;; hack: passes fallback edges from SAF-tchart construction function 
;;       to parse function
(defvar *fallback-medges* nil)

;;
;; SAF -> tchart
;;

(defun saf-setup-morphs (saf)
  (saf-to-tchart saf))

;;

(defun xml-saf-to-tchart (xml)
  (saf-to-tchart (smaf::xml-to-saf-object xml)))

(defun new-tchart ()
  (setf *tchart* (make-tchart))
  (setf *tchart-max* 0)
  (setf *fallback-medges* nil)
  (setf *smaf-id-to-edge-id* nil)
  (setf *HIDDEN-smaf-id-to-edge-id* nil)
  )

(defun get-edge-by-id (id &optional (tchart *tchart*))
  (find id (get-edges tchart) :key #'edge-id :test #'=))

;; used in test for duplicate chart/tchart edges
(defun dup-edge= (x y)
  (cond
   ;; token edges
   ((and (typep x 'token-edge)
	 (typep y 'token-edge))
    (token-edge= x y))
   ;; morph edges
   ((and (typep x 'morpho-stem-edge) 
	 (typep y 'morpho-stem-edge))
    nil ;; NOT YET IMPLEMENTED
    )
   ;; chart edges
   ((and (typep x 'edge) 
	 (typep y 'edge))
    nil) ;; NOT YET IMPLEMENTED
   ;; different edge types
   (t
    (error "edges expected"))))

(defun clean-tchart (&optional (tchart *tchart*))
  (let ((dups (get-duplicate-edge-sets tchart)))
    ;(print-tchart)
    (replace-dup-children dups)
    ;(print-tchart)
    (replace-dup-edges dups tchart)
    ;(print-tchart)
    ))

(defun replace-dup-edges (dups &optional (tchart *tchart*))
  (loop 
      for i from 0 upto (1- *chart-limit*)
      do
	(setf (aref tchart i 0)
	  (loop 
	      for cc in (aref tchart i 0)
	      for e = (chart-configuration-edge cc)
	      if (member e dups :key #'car)
	      collect cc))
	(setf (aref tchart i 1)
	  (loop 
	      for cc in (aref tchart i 1)
	      for e = (chart-configuration-edge cc)
	      if (member e dups :key #'car)
	      collect cc))))


(defun replace-dup-children (dups)
  (loop
      for dup in dups
      for e = (car dup)
      for children = (edge-children e)
      for new-children = 
	(loop
	    for child in children
	    for new-child = (get-dup-edge child dups)
			    ;;	    do (unless (eq child new-child)
			    ;;		 (format t "~&; Warning: altering edge ~a (replacing child ~a by duplicate edge ~a)" e child new-child))
	    collect new-child)
      do
	(setf (edge-children e) new-children)))

(defun get-dup-edge (e dups)
  (car (get-dup-set e dups)))

(defun get-dup-set (e dups)
  (find e dups :test #'dup-edge= :key #'car))

(defun get-duplicate-edge-sets (&optional (tchart *tchart*))
  (loop
      with dups
      for e in (reverse (get-edges tchart))
      for dup = (get-dup-set e dups)
      do
	(cond
	 (dup
	  ;; duplicate
	  (format t "~&WARNING: pruning duplicate edge ~a" e)
	  (setf (cdr (last dup)) (list e))
	  )
	 (t
	  (push (list e) dups)))
      finally (return dups)))

;; generate sentence strings for all token paths through tchart
(defun tchart-to-sentence-strings nil
  (loop
      with edge-paths = (get-edge-paths-x2y 0 *tchart-max*)
      for edge-path in edge-paths
      collect
	(with-output-to-string (s)
	  (princ (edge-string (pop edge-path)) s)
	  (loop
	      for edge in edge-path
	      for string = (edge-string edge)
	      do
		(princ #\Space s)
		(princ string s)))))

;; generate sentence strings for all token paths through S(M)AF XML input
(defun xml-to-sentence-strings (xml &optional (stream t))
  (if (lxml::xml-whitespace-p xml)
      (return-from xml-to-sentence-strings))
  (if (null xml)
      (return-from xml-to-sentence-strings))
  (let ((len (length xml)))
    (when (string= "" (subseq xml (1- len) len))
      (setf xml (subseq xml 0 (1- len)))))
  (when xml
    (let* ((saf (saf::xml-to-saf-object xml))
	   (id (saf::saf-id saf)))
      (saf-to-tchart saf)
      (loop
	  for sent in (tchart-to-sentence-strings)
	  for str = (format nil "~%;~a~%~a" id sent)
	  do
	    (princ str stream)
	  ;collect str
		  ))))

;; read file containing one S(M)AF XML per line, and generate set sentence test items
;; (for itsdb)
(defun file-to-sentence-strings (filename)
  (with-open-file (s filename :external-format :utf-8)
    (with-open-file (s-out (format nil "~a.items" filename)
		     :direction :output :if-exists :supersede :external-format :utf-8)
    (loop
	for line = (read-line s nil nil)
	while line
	;append 
	do
	  (handler-case
	      (xml-to-sentence-strings line s-out)
	    #+:allegro
	    (EXCL:INTERRUPT-SIGNAL () (error "Interrupt-Signal"))
	    (error (condition)
	      (format t  "~&Error: ~A~%whilst processing ~a~%" condition line))
	    )
	  ))))

;; use to process set of files containing SAF XML segments into sentences
#+:allegro
(defun file-pattern-to-sentence-strings (pattern)
  (require :osi)
  (loop
    for filename in
	(excl.osi:command-output (format nil "ls ~a" pattern))
      do 
	(format t "~%; [processing file ~a]" filename)
	(file-to-sentence-strings filename)
	))

;; if set, perform cleanup operations after converting
;; SAF to tchart
(defvar *clean-tchart-p* nil)


(defun saf-to-tchart (saf &key (filter #'identity))
  (new-tchart)
  (initialize-smaf-node-to-chart-node saf)
  (saf-lattice-to-tchart (smaf::saf-lattice saf)
			:filter filter
			:addressing (smaf::saf-meta-addressing (smaf::saf-meta saf)))
  (if *clean-tchart-p*
      (clean-tchart *tchart*)) ;; FIXME! disabled until bug fixed in clean-tchart
  *tchart*)

;; init=0 ... final=latticeSize
(defun initialize-smaf-node-to-chart-node (saf)
  (let* ((lattice (smaf::saf-lattice saf))
	 (init (and lattice (smaf::saf-lattice-start-node lattice)))
	 (final (and lattice (smaf::saf-lattice-end-node lattice)))
	 (edges (and lattice (smaf::saf-lattice-edges lattice)))
	 (meta (smaf::saf-meta saf))
	 (addressing (smaf::saf-meta-addressing meta))
	 )
    (setf *chart-node* -1)
    (setf *smaf-node-to-chart-node* nil)
    (when lattice
      (if init
	  (and (push (cons init 0) *smaf-node-to-chart-node*)
	       (setf *chart-node* 0))
	(format t "~%WARNING: no init node in SMAF lattice"))
      (if final
	  (push (cons final (smaf::get-smaf-lattice-size saf)) *smaf-node-to-chart-node*)
	(format t "~%WARNING: no final node in SMAF lattice"))
      ;; create nicely ordered mapping into chart nodes
      (loop
	  for e in (smaf::sort-edges-by-from edges :addressing addressing)
	  for source = (smaf::saf-edge-source e)
	  for target = (smaf::saf-edge-target e)
	  do
	    (smaf-node-to-chart-node source)
	    (smaf-node-to-chart-node target))
      ;(print *smaf-node-to-chart-node*)
      )))

;; token edge -> t1 ...
;; morph edge -> m1 ...
(defun initialize-smaf-id-to-edge-id-from-tchart nil
  (setf *smaf-id-to-edge-id* nil)
  ;; tedges
  (loop
      for tedge in (get-tedges)
      for id = (edge-id tedge)
      do
	(push (cons (format nil "t~a" id) id) *smaf-id-to-edge-id*))
  ;; medges
  (loop
      for medge in (get-medges)
      for id = (edge-id medge)
      do
	(push (cons (format nil "m~a" id) id) *smaf-id-to-edge-id*))
  *smaf-id-to-edge-id*)

(defun saf-lattice-to-tchart (saf-lattice &key (filter #'identity) addressing)
  (loop 
      for e in 
	(loop for f in (and saf-lattice (smaf::saf-lattice-edges saf-lattice))
	    when (funcall filter f)
	    collect f)
      if (string= (saf::l-edgeType e) "tok")
      collect e into toks
      else if (string= (saf::l-edgeType e) "tok+morph")
      collect e into tokMorphs
      else if (string= (saf::l-edgeType e) "morph")
      collect e into morphs
      else do (format t "~&WARNING: SMAF edge ~a has unknown edgeType '~a' (allowed values: 'tok' 'tok+morph' 'morph')" (smaf::saf-edge-id e) (saf::l-edgeType e))
      finally     
	(loop for e in toks
	    do 
	      (augment-tchart-from-saf-edge e 
					    #'saf-edge-to-tedge
					    addressing))
	(loop for e in tokMorphs
	    do 
	      (augment-tchart-from-saf-edge e 
					    #'saf-edge-to-tedge
					    addressing)
	      (augment-tchart-from-saf-edge e 
					    #'saf-edge-to-medge
					    addressing))
	(loop for e in morphs
	    do 
	      (augment-tchart-from-saf-edge e 
					    #'saf-edge-to-medge
					    addressing))
	))

#+:null
(defun next-tchart-edge-id (&optional (tchart *tchart*))
  (let ((edges (get-edges tchart)))
    (if edges
	(apply #'max (mapcar #'edge-id edges))
      0)))

;; to do: replace global *tchart* + *tchart-max* + ??? with objects
(defun augment-tchart-from-saf-edge (saf-edge fn addressing)
  (loop
      with edges = (funcall fn saf-edge addressing)
      for edge in edges
      if (smaf::saf-fs-feature-value 
	  (smaf::saf-edge-l-content saf-edge) "fallback")
      do
	 ;; fallback edges are stored and resurrected during parsing if
	;; lexical lookup fails
	(push edge *fallback-medges*)
      else
      do
      (augment-tchart-from-saf-edge-aux edge)))

(defun augment-tchart-from-saf-edge-aux (edge)
  (let* ((from (edge-from edge))
	 (to (edge-to edge))
	 (cc))
    
    (unless (and (integerp from) (integerp to))
      (format t "~&WARNING: ignoring malformed chart edge '~a' (from='~a', to='~a')"
	      (edge-id edge) from to)
      (return-from augment-tchart-from-saf-edge-aux))
    
    (setf cc (make-chart-configuration :begin from :end to :edge edge))
    (setf (aref *tchart* to 0) (push cc (aref *tchart* to 0)))
    (setf (aref *tchart* from 1) (push cc (aref *tchart* from 1)))
    (when (> to *tchart-max*)
      ;;(format t "~%WARNING: increasing *tchart-max* to ~a" to)
      (setf *tchart-max* to)))
  *tchart*)

;; [bmw] very basic unknown word mechanism (doesn't require SMAF XML input)
;; set to grammar type for unknown words
;; (must also set *fallback-pos-p* to T)
(defvar smaf::*unknown-word-type* nil)

;; generate fallback edges, then add them to tchart
(defun augment-tchart-with-fallback-morphop nil
  (loop
      for medge in (get-fallback-morphop-edges)
      for children = (edge-children medge)
;;      for grammar-type = 
;;	(dag-type
;;	 (tdfs-indef
;;	  (lex-entry-full-fs
;;	   (slot-value medge 'l-content))))
      do
	(format t "~&;;; WARNING: adding fallback edge ~a for unknown token ~a"
		(edge-id medge) children)
	(add-edge-to-tchart medge)))

;; generate fallback edges
(defun get-fallback-morphop-edges nil
  (cond
   (smaf::*unknown-word-type*
    ;; very basic mechanism, same for all unknown words
    (loop
	for tedge in (get-unanalysed-and-unspanned-tedges)
	for e-from = (edge-from tedge)
	for e-to = (edge-to tedge)
	for children = (list tedge)
	for leaf-edges = children
	for children-words =
	  (loop for l in leaf-edges
	      collect (token-edge-string l))
	for cfrom = (edge-cfrom tedge)
	for cto = (edge-cto tedge)
	for form = (str-list-2-str children-words)
	for stem = (string-upcase form)
	for dummy-entry = 
	  (get-dummy-unexpanded-lex-entry
	   stem
	   :unifs (list (cons :|type| (2-str smaf::*unknown-word-type*)))
	   :gmap '((:|type| NIL :sym)))
	collect
	  (make-morpho-stem-edge 
	   :id (next-edge)
	   :children children
	   :leaves (loop for x in leaf-edges collect (edge-string x))
	   :from e-from
	   :to e-to
	   :cfrom cfrom
	   :cto cto
	   :string form
	   :word (string-upcase form)
	   :current (string-upcase form)
	   :stem stem
	   :partial-tree nil
	   :l-content (cons :full dummy-entry)
	   )))
   (t
    ;; more sophisticated mechanism
    ;; (eg. map POS tags into grammar types)
    (loop
	with tedges = (get-unanalysed-and-unspanned-tedges)
	for medge in *fallback-medges*
	for children = (edge-children medge)
	when (intersection children tedges)
	     ;; eg. any children are unanalysed
	collect medge))))

;; make cc from edge
;; and slot into *tchart* from/to array
(defun add-edge-to-tchart (edge)
  (let* ((from (edge-from edge))
	 (to (edge-to edge))
	 (cc (make-chart-configuration :begin from :end to :edge edge)))
    (setf (aref *tchart* to 0) (push cc (aref *tchart* to 0)))
    (setf (aref *tchart* from 1) (push cc (aref *tchart* from 1)))
    (when (> to *tchart-max*)
      ;;(format t "~%WARNING: increasing *tchart-max* to ~a" to)
      (setf *tchart-max* to)))
  *tchart*)

;; input: edge of type 'tok' or 'tok+morph'
(defun saf-edge-to-tedge (saf-edge addressing)
  (unless (or (string= "tok" (saf::l-edgeType saf-edge))
	      (string= "tok+morph" (saf::l-edgeType saf-edge)))
    (error "edgeType='tok' expected (got '~a')" (saf::l-edgeType saf-edge)))
  (with-slots (smaf::id smaf::source smaf::target smaf::from smaf::to smaf::l-content) saf-edge
    (let* ((tokenStr (smaf::saf-fs-feature-value smaf::l-content "tokenStr"))
	  (e-id (if (string= "tok" (saf::l-edgeType saf-edge))
		    (smaf-id-to-edge-id smaf::id)
		  (HIDDEN-smaf-id-to-edge-id smaf::id :token)))
	  (tedge
	   (make-token-edge 
	    :id e-id
	    :from (smaf-node-to-chart-node smaf::source)
	    :to (smaf-node-to-chart-node smaf::target)
	    :string tokenStr
	    :cfrom (smaf::point-to-char-point smaf::from addressing)
	    :cto (smaf::point-to-char-point smaf::to addressing)
	    :word (string-upcase tokenStr)
	    :leaves (list tokenStr))))
      (list tedge))))

(defun HIDDEN-smaf-id-to-edge-id (smaf-id hidden)
  (let* ((h-str
	  (case hidden
	    (:token "T")
	    (t (error "expected :token, got ~S" hidden)))) 
	 (id (concatenate 'string h-str smaf-id))
	 (match (cdr (assoc id *HIDDEN-smaf-id-to-edge-id* :test #'string=))))
    (if match
	(return-from HIDDEN-smaf-id-to-edge-id match))
    ;; new edge id
    (push (cons id (incf *edge-id*)) *HIDDEN-smaf-id-to-edge-id*)
    *edge-id*))

(defun smaf-id-to-edge-id (smaf-id)
  (let ((match (cdr (assoc smaf-id *smaf-id-to-edge-id* :test #'string=))))
    (if match
	(return-from smaf-id-to-edge-id match))
    ;; new edge id
    (push (cons smaf-id (incf *edge-id*)) *smaf-id-to-edge-id*)
    *edge-id*))

(defun edge-id-to-smaf-id (edge-id)
  (let ((match (car (rassoc edge-id *smaf-id-to-edge-id* :test #'equalp))))
    (if match
	(return-from edge-id-to-smaf-id match))))

;; init/final dealt with via correct initialization of mapping
(defun smaf-node-to-chart-node (smaf-node)
  (let ((match (cdr (assoc smaf-node *smaf-node-to-chart-node* :test #'string=))))
    (if match
	(return-from smaf-node-to-chart-node match))
    ;; new chart node
    (push (cons smaf-node (incf *chart-node*)) *smaf-node-to-chart-node*)
    *chart-node*))
    
;; id: first char ignored, rest gives integer
#+:null
(defun id-to-int (id &key (generate t))
  (handler-case 
      (let ((i (if id (parse-integer (subseq id 1)))))
	(if i
	    (and (setf *edge-id* (max i *edge-id*))
		 i)
	  (if generate
	      (- (incf *edge-id*)))))
    (error (condition)
      (error "unable to convert malformed id `~a': ~a" id condition))))

;; input: saf edge of type 'tok' or 'tok+morph'
(defun saf-edge-to-medge (saf-edge addressing)
  (unless (or (string= "morph" (saf::l-edgeType saf-edge))
	       (string= "tok+morph" (saf::l-edgeType saf-edge)))
    (error "'morph' edge expected (got '~a')" (saf::l-edgeType saf-edge)))
  ;; assumes tedges already in chart
  (with-slots (smaf::id smaf::source smaf::target smaf::deps smaf::l-content smaf::from smaf::to) saf-edge
    (let* ((children 
	    (cond
	     ((string= "tok+morph" (saf::l-edgeType saf-edge))
	      ;; find hidden 'tok' edge
	      (list (smaf-id-to-token-edge smaf::id (get-tedges *tchart*) 
					   :hidden :token)))
	     (t
	      ;; derive child edges from saf deps
	      (loop for d in smaf::deps
		  for tedge = (smaf-id-to-token-edge d (get-tedges *tchart*))
		  if tedge
		  collect tedge
		  else 
		  do (format t "~&WARNING: missing SMAF edge '~a' (child of '~a')" d smaf::id)))))
	   (leaf-edges children) ;;fix me
	   (children-words
	    (loop for l in leaf-edges
		collect (token-edge-string l)))
	   (form (str-list-2-str children-words))
	   (stem (string-upcase 
		  (or (smaf::saf-fs-feature-value smaf::l-content "stem")
		      form)))
	   (partialTree (smaf::saf-fs-feature-value smaf::l-content "partialTree"))
	   (partialTree2 (smaf::saf-fs-feature-value smaf::l-content "+partialTree"))
	   (gmap-unifs (smaf::get-gmap-unifs smaf::l-content))
	   (dummy-entry 
	    (get-dummy-unexpanded-lex-entry form 
					    :unifs gmap-unifs
					    :gmap saf::*gmap*
					    :rmrs (smaf::saf-fs-feature-value smaf::l-content "rmrs")
					    ))
	   (inject (get-inject gmap-unifs saf::*gmap*))
	   (l-content 
	    (if (smaf::saf-fs-feature-value 
		 (smaf::saf-edge-l-content saf-edge) "inject")
		(and inject (cons :inject inject))
	      (and dummy-entry (cons :full dummy-entry))))
	   (e-from (smaf-node-to-chart-node smaf::source))
	   (e-to (smaf-node-to-chart-node smaf::target))
    	   (cfrom (or (smaf::point-to-char-point smaf::from addressing)
		      (get-min-edge-cfrom children)))
	   (cto (or (smaf::point-to-char-point smaf::to addressing)
		    (get-max-edge-cto children)))
	   (partial-tree 
	    (or (smaf::saf-fs-partial-tree-2-list-partial-tree partialTree)
		(smaf::saf-plus-2-list-partial-tree partialTree2)
		))
	   err-flag medges medge
	   )
      

      
      (unless (or stem dummy-entry)
	(format t "~&WARNING: no stem/gType for SMAF edge '~a'" smaf::id)
	(setf err-flag t))
      (unless (integerp e-from)
	(format t "~&WARNING: missing source for SMAF edge '~a'" smaf::id)
	(setf err-flag t))
      (unless (integerp e-to)
	(format t "~&WARNING: missing target for SMAF edge '~a'" smaf::id)
	(setf err-flag t))
      (when (and (integerp e-from)
		 (integerp e-to)
		 (not (= e-from (leaf-edges-from leaf-edges))))
	(format t "~&WARNING: source mismatch between SMAF edge '~a' and it's daughters" smaf::id)
	(setf err-flag t))
      (when (and (integerp e-from)
		 (integerp e-to)
		 (not (= e-to (leaf-edges-to leaf-edges))))
	(format t "~&WARNING: target mismatch between SMAF edge '~a' and it's daughters" smaf::id)
	(setf err-flag t))
      (unless err-flag
	(push
	 (setf medge 
	   (make-morpho-stem-edge 
	    :id (smaf-id-to-edge-id smaf::id)
	    :children children
	    :leaves (loop for x in leaf-edges collect (edge-string x))
	    :from e-from
	    :to e-to
	    :cfrom cfrom
	    :cto cto
	    :string form
	    :word (string-upcase form)
	    :current (string-upcase form)
	    :stem stem
	    :partial-tree partial-tree
	    :l-content l-content
	    ))
	 medges)
	(when (smaf::saf-fs-feature-value 
	       (smaf::saf-edge-l-content saf-edge) "analyseMorph")
	  (loop
	      with morph-analyses = (get-morph-analyses (string-upcase form))
	      for (stem . partial-tree) in morph-analyses
	      when partial-tree
	      do
		(push
		 (make-morpho-stem-edge 
		  :id (next-edge) 
		  :children (list medge)
		  :leaves (loop for x in leaf-edges collect (edge-string x))
		  :from e-from
		  :to e-to
		  :cfrom cfrom
		  :cto cto
		  :string form
		  :word (string-upcase form)
		  :current stem
		  :stem stem
		  :partial-tree partial-tree
		  :l-content l-content
		  )
		 medges))))
      medges)))

(defun get-inject (unifs gmap)
  (loop
      for (feat . val) in unifs
      for (feat2 path type) = (find feat gmap :key #'first)
      for decoded-val = (if (eq type :sym) 
			    (intern val) 
			  val)
      do
	(setf feat2 feat2) ;; hack to get rid of compiler warning
      collect
	(make-unification
	   :lhs (make-path :typed-feature-list path)
	   :rhs (make-u-value :type decoded-val))))

(defun smaf-id-to-token-edge (id tedges &key hidden)
  (if hidden
      (find (HIDDEN-smaf-id-to-edge-id id hidden) tedges :key #'token-edge-id :test #'=)
    (find (smaf-id-to-edge-id id) tedges :key #'token-edge-id :test #'=)))

#+:null
(defun str-2-int (str)
  (if str (parse-integer str)))

;;
;;					;

;;

(defun mps-str (x)
  (or (encode-mixed-as-str x) ""))

(defvar mrs::*main-semantics-path*)

;(GET-DUMMY-UNEXPANDED-LEX-ENTRY "LDA"
;         :UNIFS ((:|carg| . "bmw[Li+].CC(C)[N-]C(C)C") (:|pred|)
;                 (:|type| . "n_proper_nale"))
;         :GMAP ((:|carg| (SYNSEM LKEYS KEYREL CARG) :STR)
;                (:|pred| (SYNSEM LKEYS KEYREL PRED) :SYM) (:|type| NIL :SYM)))
;; mixed up code: unifs shadowing unifs!
(defun get-dummy-unexpanded-lex-entry (orth &key unifs gmap rmrs)
  (let* ((unifs2
	  (loop
	      for (key . val) in unifs
	      for (c-dummy c-path c-type) = (find key gmap :key #'car)
	      for val2 = (mps-str (if (eq c-type :sym) 
				      (intern val) 
				    val))
	      collect (list val2 c-path '(MIXED))
	      do 
		(setf c-dummy c-dummy)))
	 (lex-entry (get-dummy-unexpanded-lex-entry2 orth :unifs unifs2))
	 ;; need: mrs::*semi* and mrs::*meta-semi*
	 (mrs (and rmrs (mrs::convert-rmrs-to-mrs rmrs))) 
	 (rels (and mrs (mrs::psoa-liszt mrs)))
	 (rels-unifs 
	  (and rels (mrs::create-unifs-from-rels2 rels mrs::*main-semantics-path*))))
    (when lex-entry
      (setf (lex-entry-unifs lex-entry)
	(append rels-unifs (lex-entry-unifs lex-entry))))
    lex-entry))

;(GET-DUMMY-UNEXPANDED-LEX-ENTRY2 "LDA"
;           :UNIFS (("\"bmw[Li+].CC(C)[N-]C(C)C\"" (SYNSEM LKEYS KEYREL CARG) (MIXED))
;                   ("" (SYNSEM LKEYS KEYREL PRED) (MIXED))
;                   ("n_proper_nale" NIL (MIXED))))
;; (val path type)*
(defun get-dummy-unexpanded-lex-entry2 (orth &key unifs)
  (when unifs
    (loop
	with dfn = 
	  '((:ID :|name| "" (SYM)) 
	    (:ORTH :|orthography| "" (STR-RAWLST))
	    (:UNIFS :|orthography| "(stem)" (STR-LST)))
	with vals = (list "" orth)
	with keys = '(:|name| :|orthography|);;!
	for (val path type) in unifs
	for key from 0
	do 
	  (push (list :unifs key (format nil "~S" path) type) dfn)
	  (push val vals)
	  (push key keys)
	finally       
	  (return (make-psort-struct2 vals keys :dfn dfn)))))

(defun read-smaf-conf (x)
  (format t "~&;;; reading SMAF config file '~a'" x)
  (saf::get-saf-l-map x)
  t)

;;

(defun run-parse-server (&rest rest)
  (apply 'saf::run-parse-server rest))

(defun run-fspp-server (&rest rest)
  (apply 'saf::run-fspp-server rest))

;;

;; print token counts for all unanalysed tokens
(defun report-unanalysed-tokens nil
  (let ((hash (make-hash-table :test #'equalp))
	count-toks)
    (loop
	for tok in *unanalysed-tokens*
	for count = (or (gethash tok hash) 0)
	do
	  (setf (gethash tok hash)
	    (1+ count)))
    (setf count-toks
      (loop
	  for tok being each hash-key in hash
	  for count = (gethash tok hash)
	  collect (cons count tok)))
    (loop
	for (count . tok) in (sort count-toks #'> :key #'car)
	do
	  (format t "~%~a ~a" count tok))))
