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
  (setf *smaf-id-to-edge-id* nil)
  (setf *HIDDEN-smaf-id-to-edge-id* nil)
  )

(defun get-edge-by-id (id &optional (tchart *tchart*))
  (find id (get-edges tchart) :key #'edge-id :test #'=))

;;!
(defun dup-edge= (x y)
  (cond
   ;; token edges
   ((and (typep x 'token-edge)
	 (typep y 'token-edge)
	 )
    (string= (token-edge-word x)
	     (token-edge-word y)))
   ;; morph edges
   ((and (typep x 'morpho-stem-edge) 
	 (typep y 'morpho-stem-edge))
    (string= (morpho-stem-edge-word x)
	     (morpho-stem-edge-word y)))
   ;; chart edges
   ((and (typep x 'edge) 
	 (typep y 'edge))
    nil)
   ;; different edge types
   (t
    nil)))

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

(defun saf-to-tchart (saf &key (filter #'identity))
  (new-tchart)
  (initialize-smaf-node-to-chart-node saf)
  (saf-lattice-to-tchart (smaf::saf-lattice saf)
			:filter filter
			:addressing (smaf::saf-meta-addressing (smaf::saf-meta saf)))
  ;;(clean-tchart *tchart*) FIXME! disabled until bug fixed in clean-tchart
  *tchart*)

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
  (let
      ((edge (funcall fn saf-edge addressing)))
    (when edge
      (let* (
	     (from (edge-from edge))
	     (to (edge-to edge))
	     (cc (make-chart-configuration :begin from :end to :edge edge)))
	(unless (and (integerp from) (integerp to))
	  (format t "~&WARNING: ignoring malformed chart edge '~a' (from='~a', to='~a')"
		  (edge-id edge) from to)
	  (return-from augment-tchart-from-saf-edge))
	(setf (aref *tchart* to 0) (push cc (aref *tchart* to 0)))
	(setf (aref *tchart* from 1) (push cc (aref *tchart* from 1)))
	(when (> to *tchart-max*)
	  ;;(format t "~%WARNING: increasing *tchart-max* to ~a" to)
	  (setf *tchart-max* to)))))
  *tchart*)

;; input: edge of type 'tok' or 'tok+morph'
(defun saf-edge-to-tedge (saf-edge addressing)
  (unless (or (string= "tok" (saf::l-edgeType saf-edge))
	      (string= "tok+morph" (saf::l-edgeType saf-edge)))
    (error "edgeType='tok' expected (got '~a')" (saf::l-edgeType saf-edge)))
  (with-slots (smaf::id smaf::source smaf::target smaf::from smaf::to smaf::l-content) saf-edge
    (let ((tokenStr (smaf::saf-fs-feature-value smaf::l-content "tokenStr"))
	  (e-id (if (string= "tok" (saf::l-edgeType saf-edge))
		    (smaf-id-to-edge-id smaf::id)
		  (HIDDEN-smaf-id-to-edge-id smaf::id :token))))
      (make-token-edge 
       :id e-id
       :from (smaf-node-to-chart-node smaf::source)
       :to (smaf-node-to-chart-node smaf::target)
       :string tokenStr
       :cfrom (smaf::point-to-char-point smaf::from addressing)
       :cto (smaf::point-to-char-point smaf::to addressing)
       :word (string-upcase tokenStr)
       :leaves (list tokenStr)))))

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
	   (stem (or (smaf::saf-fs-feature-value smaf::l-content "stem")
		     form))
	   (partialTree (smaf::saf-fs-feature-value smaf::l-content "partialTree"))
	   (gmap-unifs (smaf::get-gmap-unifs smaf::l-content))
	   (dummy-entry 
	    (get-dummy-unexpanded-lex-entry form 
					    :unifs gmap-unifs
					    :gmap saf::*gmap*
					    :rmrs (smaf::saf-fs-feature-value smaf::l-content "rmrs")
					    ))
	   (e-from (smaf-node-to-chart-node smaf::source))
	   (e-to (smaf-node-to-chart-node smaf::target))
    	   (cfrom (or (smaf::point-to-char-point smaf::from addressing)
		      (get-min-edge-cfrom children)))
	   (cto (or (smaf::point-to-char-point smaf::to addressing)
		    (get-max-edge-cto children)))
	   err-flag
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
	 :partial-tree (smaf::saf-fs-partial-tree-2-list-partial-tree partialTree)
	 :l-content dummy-entry
	 )))))

(defun get-min-edge-cfrom (edges)
  (when edges
    (loop
	for e in edges
	minimize (edge-cfrom e))))

(defun get-max-edge-cto (edges)
  (when edges
    (loop
	for e in edges
	maximize (edge-cto e))))

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
    (setf (lex-entry-unifs lex-entry)
      (append rels-unifs (lex-entry-unifs lex-entry)))
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

