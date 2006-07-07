;; Copyright (c) 2006
;;;   Ben Waldron;
;;; see `licence.txt' for conditions.

(defpackage :saf
  (:use :common-lisp) 
  (:export))

(defvar saf::*smaf-gmap*)

(in-package :lkb)

(defvar *saf-dir* nil)
(defvar *saf-v* -1)
(defvar *saf-l-map* nil)

(defvar *char-map-add-offset*)

(defvar *HIDDEN-smaf-id-to-edge-id* nil) ;; MUST reset when initializing tchart
(defvar *smaf-id-to-edge-id* nil) ;; MUST reset when initializing tchart
(defvar *chart-node* -1) ;; MUST reset when initializing tchart
(defvar *smaf-node-to-chart-node* nil) ;; MUST reset when initializing tchart

(defstruct saf
  meta
  lattice)

(defstruct saf-lattice
  start-node
  end-node
  nodes
  edges)

(defstruct saf-edge
  id
  type
  source
  target
  from
  to
  deps
  content
  l-content
  )

(defstruct saf-fv
  feature
  value)

(defstruct saf-meta
  document
  addressing
  olac
  text)

(defmethod print-object ((object saf) stream)
  (format 
   stream 
   "#[SAF]"
   ;(length (saf-lattice (saf-lattice-edges object)))
   ))

;;
;; code to convert SAF XML into SAF object
;;

;; fix_me: get rid of global *saf-l-map*
(defun xml-to-saf-object (xml &key (saf-dir "~") (l-map *saf-l-map*))
  (let ((*saf-dir* saf-dir))
    (unless *saf-l-map*
      (setf l-map (saf::get-default-saf-l-map)))
    (saf::instantiate-l-content
     (lxml-to-saf-object (xml-to-lxml xml))
     l-map) 
    ))

(defun lxml-to-saf-object (lxml)
  (let* ((lxml-body (elements-only
		     (check-doctype 
		      (remove-xml-header lxml)
		      '("smaf" "saf" "maf"))))
	 (lxml-body-1 (first lxml-body)))
    (cond
     ((null lxml-body)
      (error "empty xml body"))
     ((cdr lxml-body)
      (error "xml body expected to contain single element"))
     (t
      (saf-lxml-to-saf-object lxml-body-1)))))

(defun saf-lxml-to-saf-object (lxml)
  (unless (member (string (lxml-elt-name lxml)) '("maf" "saf" "smaf")
		  :test #'string=)
    (error "smaf, saf element expected as body"))
  (let* ((saf-attributes
	  (lxml-elt-attributes lxml))
	 (lxml (cdr lxml))
	 (text
	  (if (eq '|text| (lxml-elt-name (car lxml)))
	      (pop lxml)))
	 (olac 
	  (if (eq '|olac:olac| (lxml-elt-name (car lxml)))
	      (pop lxml))))
    (make-saf
     :meta (get-saf-meta saf-attributes :olac olac :text text)
     :lattice (get-saf-lattice lxml))))

(defun get-saf-meta (saf-attributes &key olac text)
  (let ((doc
	 (second (member '|document| saf-attributes))))
    (if doc
	(unless (eq :absolute (car (pathname-directory (pathname doc))))
	  (setf doc
	    (merge-pathnames doc (make-pathname :directory *saf-dir*)))))
    (make-saf-meta
     :document doc
     :addressing (intern (string-downcase 
			  (or
			   (second (member '|addressing| saf-attributes))
			   '|char|)))
     :olac (get-olac-meta olac)
     :text text)))

(defun get-olac-meta (lxml-olac)
  (loop
      for e in (lxml-elts lxml-olac)
      collect
	(make-saf-fv :feature (string (lxml-elt-name e))
		     :value (lxml-elt-text-content e))))

#+:null
(defun make-saf-lattice-from-sequence (lxml &key init final)
  (let ((*saf-v* -1)
	nodes edges)
    (setf init (or init (format nil "v~a" (incf *saf-v*))))
    (loop
	with source = init
	with target
	for e = (pop lxml)
	while e
	do
	  (if (and (null lxml) final)
	      (setf target final)
	    (setf target (format nil "v~a" (incf *saf-v*))))
	  (when (null lxml)
	    (setf final target))
	  (push
	   (case (lxml-elt-name e)
	     (|annot|
	      (lxml-annot-to-edge e :source source :target target))
	     ;; special case
	     (|token|
	      (lxml-token-to-edge e :source source :target target))
	     ;; special case
	     (|wordForm|
	      (lxml-wordform-to-edge e :source source :target target))
	     ;; special case
	     (|sentence|
	      (lxml-sentence-to-edge e :source source :target target))
	     (t
	      (error "unhandled saf edge type: ~a" (lxml-elt-name e))))
	   edges)
	  (setf source target))
    (loop 
	for e in edges
	do
	  (pushnew (saf-edge-source e) nodes :test #'string=)
	  (pushnew (saf-edge-target e) nodes :test #'string=))
    (make-saf-lattice
     :start-node init
     :end-node final
     :nodes nodes
     :edges edges)))

(defun get-saf-lattice (lxml)
  (cond
   ((member (lxml-elt-name (car lxml)) '(|fsm| |lattice|));; SAF / SMAF
    (when (cdr lxml)
      (error "no elements expected after fsm/lattice"))
    (get-saf-lattice-from-fsm (car lxml)))
   (t
    (error "malformed lattice: ~a" lxml)
    )))

(defun get-saf-lattice-from-fsm (lxml-fsm)
  (let* ((fsm-attributes (lxml-elt-attributes lxml-fsm))
	 (nodes 
	  (loop for x in (lxml-elt-elts lxml-fsm "state") 
	      collect (lxml-elt-attr x "id")))
	 (token-edges 
	  (loop for e in (lxml-elt-elts lxml-fsm "token")
	      collect (lxml-token-to-edge e)))
	 (annot-edges 
	  (append
	   (loop for e in (lxml-elt-elts lxml-fsm "annot");; SAF
	       collect (lxml-annot-to-edge e))
	   (loop for e in (lxml-elt-elts lxml-fsm "edge");; SMAF
	       collect (lxml-annot-to-edge e))))
	 (wordform-edges 
	  (loop for e in (lxml-elt-elts lxml-fsm "wordForm") ;;shorthand
	      collect (lxml-wordform-to-edge e)))
	 (sentence-edges 
	  (loop for e in (lxml-elt-elts lxml-fsm "sentence") ;;shorthand
	      collect (lxml-sentence-to-edge e)))
	 (all-edges (append token-edges annot-edges 
			    wordform-edges sentence-edges))
	 (start-node (second (member '|init| fsm-attributes)))
	 (end-node (second (member '|final| fsm-attributes)))
	 )
    ;; fix missing details
    ;; ensure nodes is complete
    (loop for e in all-edges
	do
	  (pushnew (saf-edge-source e) nodes :test #'string=)
	  (pushnew (saf-edge-target e) nodes :test #'string=))
    (unless (and start-node end-node)
      ;; attempt to guess, based on string< order
      (format t "~%;;; WARNING: final/init nodes should both be specified (will guess based on string< order)")
      (loop 
	  with max
	  with min
	  for n in nodes
	  do
	    (when (or (null min) (string< n min)) 
	      (setf min n))
	    (when (or (null max) (string> n max)) 
	      (setf max n))
	  finally
	    (unless start-node 
	      (format t "~%;;; using init node = '~a'" min)
	      (setf start-node min))
	    (unless end-node 
	      (format t "~%;;; using final node = '~a'" max)	      
	      (setf end-node max))))
    (when
	(check-saf-lattice-consistency start-node end-node nodes all-edges)
      (make-saf-lattice 
       :start-node start-node
       :end-node end-node
       :nodes nodes
       :edges all-edges))))

(defun get-smaf-lattice-size (saf)
  ;; num nodes minus 1, or zero
  (let ((n (get-smaf-lattice-node-count saf)))
    (if (zerop n)
	0 ;; ??
      (- n 1))))

(defun get-smaf-lattice-node-count (saf)
  ;; number of unique nodes
  (loop
      with nodes
      with lattice = (saf-lattice saf)
      with edges = (and lattice (saf-lattice-edges lattice))
      for e in edges
      for source = (saf-edge-source e)
      for target = (saf-edge-target e)
      do
	(pushnew source nodes :test 'string=)
	(pushnew target nodes :test 'string=)
      finally
	(return (length nodes))))

(defun check-saf-lattice-consistency (start-node end-node nodes edges)
  (declare (ignore start-node end-node nodes))
  (let ((consistent t))
    ;; check for duplicate edge ids
    (loop
	for e in edges
	for id = (edge-id e)
	if (member id ids :test 'string=)
	do (format t "~%;;; WARNING: invalid lattice input (duplicate id: ~S)" id)
	   (setf consistent nil)
	else
	collect id into ids)
    consistent))

(defun lxml-state-to-node (lxml-state)
  (lxml-elt-attr lxml-state "id"))

(defun saf-type (str)
  (intern str :keyword))

;; call lxml-annot-to-edge instead?
(defun lxml-token-to-edge (lxml-token &key type source target)
  (make-saf-edge
   :id (lxml-elt-attr lxml-token "id")
   :type (saf-type (or type :|token|))
   :source (or source (lxml-elt-attr lxml-token "source"))
   :target (or target (lxml-elt-attr lxml-token "target"))
   :from (lxml-elt-attr lxml-token "from")
   :to (lxml-elt-attr lxml-token "to")
   :content (lxml-elt-attr lxml-token "value")))

(defun lxml-annot-to-edge (lxml-annot &key type source target)
  (let* ((id (lxml-elt-attr lxml-annot "id"))
	 (fs-list (lxml-elt-elts lxml-annot "fs"))
	 (slots (lxml-elt-elts lxml-annot "slot"))
	 (fs (if (cdr fs-list)
		 (error "max 1 fs element allowed in wordform")
	       (car fs-list)))
	 (content 
	  (or 
	   (lxml-elt-text-content2 lxml-annot)
	   (lxml-elt-attr lxml-annot "value") 
	   (lxml-fs-content-to-fs fs) 
	   (lxml-slots-to-fs slots)))
	 (source (or source (lxml-elt-attr lxml-annot "source")))
	 (target (or target (lxml-elt-attr lxml-annot "target")))
	 )
    (unless content
      (format t "~&WARNING: no/unknown content for SMAF edge '~a'" id))
    (unless source
      (format t "~&WARNING: missing source for SMAF edge '~a'" id))
    (unless target
      (format t "~&WARNING: missing target for SMAF edge '~a'" id))
    (make-saf-edge
     :id id
     :type (saf-type (or type (lxml-elt-attr lxml-annot "type")))
     :source source
     :target target
     :deps (split-str-on-spc (lxml-elt-attr lxml-annot "deps"))
     :content content
     :from (or (lxml-elt-attr lxml-annot "from") 
	       (lxml-elt-attr lxml-annot "cfrom"))
     :to (or
	  (lxml-elt-attr lxml-annot "to")
	  (lxml-elt-attr lxml-annot "cto"))
     )))

;; special case
(defun lxml-wordform-to-edge (lxml-wordform &key source target)
  (lxml-annot-to-edge lxml-wordform :type :|wordForm|
		      :source source :target target))

;; special case
(defun lxml-sentence-to-edge (lxml-sentence &key source target)
  (lxml-token-to-edge lxml-sentence :type :|sentence|
		      :source source :target target))

(defun lxml-fs-content-to-fs (lxml)
  (cond
   ((null lxml)
    nil)
   ((stringp lxml) ;; shorthand
    lxml)   
   ((eq (lxml-elt-name lxml) '|fs|)
    (loop for f in (lxml-elt-elts lxml "f")
	collect (make-saf-fv
		 :feature (lxml-elt-attr f "name")
		 :value (lxml-fs-content-to-fs (first (lxml-elt-contents f))))))
   ((eq (lxml-elt-name lxml) '|binary|)
    :binary-ignored)
   ((eq (lxml-elt-name lxml) '|symbol|)
    :symbol-ignored)
   ((eq (lxml-elt-name lxml) '|numeric|)
    :numeric-ignored)
   ((eq (lxml-elt-name lxml) '|string|)
    (let ((str (first (lxml-elt-contents lxml))))
      (unless (stringp str)
	(error "string expected"))
      str))))

(defun lxml-slots-to-fs (lxml-slots)
  (loop
      for s in lxml-slots
      for feat = (lxml-elt-attr s "name")
      for val = (first (lxml-elt-contents s))
      for val-str = (if (stringp val)
			val
		      (error "string expected"))
      collect 
	(make-saf-fv :feature feat
		     :value val-str)))



;;
;; batch processing
;; SENTENCE -> PARSE
;;

(defun process-standoff-sentence-file (filename &key show-parse)
  (process-saf-file-sentences filename :show-parse show-parse))
  
(defun process-saf-file-sentences (filename &key (show-parse t) reset-unanalysed-tokens)
  (with-open-file 
      (ofile 
       (merge-pathnames 
	(make-pathname 
	 :name (format nil "~a.out" (pathname-name (pathname filename))))
	(pathname filename))
       :direction :output
       :if-exists :overwrite
       :if-does-not-exist :create)
    (format t "~&;;; Input sentence file: ~a" filename)
    (format t "~&;;; Output file: ~a" (namestring ofile))
    (process-saf-sentences
     (xml-to-saf-object 
      (read-file-to-string filename)
      :saf-dir (pathname-directory (pathname filename)))
     :ostream ofile
     :show-parse show-parse
     :reset-unanalysed-tokens reset-unanalysed-tokens)))

(defun process-saf-sentences (saf &key (ostream t) show-parse reset-unanalysed-tokens)
  (let* ((textfilename (saf-meta-document (saf-meta saf)))
	 (text
	  (if textfilename
	      (read-file-to-string textfilename))))
    (format t "~&;;; Data file: ~a" (saf-meta-document (saf-meta saf)))
    (format ostream "~a"
	    (preprocessor::saf-header :addressing :|char|
			:document (saf-meta-document (saf-meta saf))))
    (when reset-unanalysed-tokens
      (setf *unanalysed-tokens* nil))
    (loop 
	for s in 
	  (sort (loop for e in (saf-lattice-edges (saf-lattice saf))
		    when (eq :|sentence| (saf-edge-type e))
		    collect e)
		#'< 
		:key #'(lambda (x)
			 (or
			  (point-to-char-point 
			   (saf-edge-from x) :|char|)
			  -1)))
	for from = (saf-edge-from s)
	for to = (saf-edge-to s)
	unless (and from to) do
	  (format t "~&~%CANNOT PROCESS SENTENCE ~a due to null pointer: from=~a to=~a" 
		  (saf-edge-id s) from to)
	       
	when (and from to) do
	  (format t "~&~%PROCESSING SENTENCE ~a: ~& ~a" 
		  (saf-edge-id s)
		  (x-span text from to
			  (saf-meta-addressing (saf-meta saf)))
		  )
	  (time
	   (handler-case 
	       (cond
		((saf-meta-document (saf-meta saf))
		 (let ((*generate-messages-for-all-unanalysed-tokens* t)
		       ;(*char-map-add-offset* 
			;(point-to-char-point (saf-edge-from s) :|char|))
		       )
		   (setf *char-map-add-offset* (point-to-char-point (saf-edge-from s) :|char|))
		   (x-parse text 
			    (saf-edge-from s) 
			    (saf-edge-to s)
			    (saf-meta-addressing (saf-meta saf))
			    :document (saf-meta-document (saf-meta saf))
			    :char-map #'char-map-add-x
			    :show-parse show-parse)))
		(t
		 (x-parse (saf-edge-content s) 
			  nil
			  nil
			  nil
			  :document nil
			  :show-parse show-parse)))
	     (storage-condition (condition)
	       (format t "~&Memory allocation problem: ~A" condition))
	     #+:allegro
	     (EXCL:INTERRUPT-SIGNAL () (error "Interrupt-Signal"))
	     (error (condition)
	       (format t  "~&Error: ~A" condition))
	     ))
	  (dump-sentence-analyses s ostream))
    (format ostream "~&</saf>")))

#+:mrs
(defun dump-sentence-analyses (s &optional (stream t))
  (dump-sentence-analyses2 :s-id(saf-edge-id s) :stream stream))
  
#+:mrs
;;based on mrs::output-mrs-after-parse
(defun dump-sentence-analyses2 (&key (s-id (format nil "s~a"(id-to-int nil))) 
				     (stream t))
  (let ((*print-circle* nil))
    (loop for edge in *parse-record* 
	do
	  (let ((mrs (mrs::extract-mrs edge)))
	    (format stream "~&<annot type='parse' deps='~a' edge='~a'>" ;;move edge into content
		    s-id
		    (lkb::edge-id edge))
	    ;;(format stream "~&~A~&" 
	    ;;(lkb::parse-tree-structure edge))
	    (mrs::output-rmrs1 (mrs::mrs-to-rmrs mrs) 'mrs::xml stream)
	    (format stream "~&</annot>")))))

;;
;; SAF -> tchart
;;

(defun saf-num-lattice-nodes (saf)
  (length (saf-lattice-nodes (saf-lattice saf))))

(defun saf-setup-morphs (saf)
  (saf-to-tchart saf))

;;

(defun xml-saf-to-tchart (xml)
  (saf-to-tchart (xml-to-saf-object xml)))

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
  (saf-lattice-to-tchart (saf-lattice saf)
			:filter filter
			:addressing (saf-meta-addressing (saf-meta saf)))
  (clean-tchart *tchart*)
  *tchart*)

(defun initialize-smaf-node-to-chart-node (saf)
  (let* ((lattice (saf-lattice saf))
	 (init (and lattice (saf-lattice-start-node lattice)))
	 (final (and lattice (saf-lattice-end-node lattice)))
	 (edges (and lattice (saf-lattice-edges lattice)))
	 (meta (saf-meta saf))
	 (addressing (saf-meta-addressing meta))
	 )
    (setf *chart-node* -1)
    (setf *smaf-node-to-chart-node* nil)
    (when lattice
      (if init
	  (and (push (cons init 0) *smaf-node-to-chart-node*)
	       (setf *chart-node* 0))
	(format t "~%WARNING: no init node in SMAF lattice"))
      (if final
	  (push (cons final (get-smaf-lattice-size saf)) *smaf-node-to-chart-node*)
	(format t "~%WARNING: no final node in SMAF lattice"))
      ;; create nicely ordered mapping into chart nodes
      (loop
	  for e in (sort-edges-by-from edges :addressing addressing)
	  for source = (saf-edge-source e)
	  for target = (saf-edge-target e)
	  do
	    (smaf-node-to-chart-node source)
	    (smaf-node-to-chart-node target))
      ;(print *smaf-node-to-chart-node*)
      )))

(defun sort-edges-by-from (edges &key addressing)
  (sort (copy-list edges)
	'<
	:key (lambda (x)
	       (point-to-char-point (saf-edge-from x) addressing))
	))

(defun saf-lattice-to-tchart (saf-lattice &key (filter #'identity) addressing)
  (loop 
      for e in 
	(loop for f in (and saf-lattice (saf-lattice-edges saf-lattice))
	    when (funcall filter f)
	    collect f)
      if (string= (saf::l-edgeType e) "tok")
      collect e into toks
      else if (string= (saf::l-edgeType e) "tok+morph")
      collect e into tokMorphs
      else if (string= (saf::l-edgeType e) "morph")
      collect e into morphs
      else do (format t "~&WARNING: SMAF edge ~a has unknown edgeType '~a' (allowed values: 'tok' 'tok+morph' 'morph')" (saf-edge-id e) (saf::l-edgeType e))
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
  (with-slots (id source target from to l-content) saf-edge
    (let ((tokenStr (saf-fs-feature-value l-content "tokenStr"))
	  (e-id (if (string= "tok" (saf::l-edgeType saf-edge))
		    (smaf-id-to-edge-id id)
		  (HIDDEN-smaf-id-to-edge-id id :token))))
      (make-token-edge 
       :id e-id
       :from (smaf-node-to-chart-node source)
       :to (smaf-node-to-chart-node target)
       :string tokenStr
       :cfrom (point-to-char-point from addressing)
       :cto (point-to-char-point to addressing)
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
    
;; should not be being called directly!
;; id: first char ignored, rest gives integer
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
  (with-slots (id source target deps l-content from to) saf-edge
    (let* ((children 
	    (cond
	     ((string= "tok+morph" (saf::l-edgeType saf-edge))
	      ;; find hidden 'tok' edge
	      (list (smaf-id-to-token-edge id (get-tedges *tchart*) 
					   :hidden :token)))
	     (t
	      ;; derive child edges from saf deps
	      (loop for d in deps
		  for tedge = (smaf-id-to-token-edge d (get-tedges *tchart*))
		  if tedge
		  collect tedge
		  else 
		  do (format t "~&WARNING: missing SMAF edge '~a' (child of '~a')" d id)))))
	   (leaf-edges children) ;;fix me
	   (children-words
	    (loop for l in leaf-edges
		collect (token-edge-string l)))
	   (form (str-list-2-str children-words))
	   (stem (or (saf-fs-feature-value l-content "stem")
		     form))
	   (partialTree (saf-fs-feature-value l-content "partialTree"))
	   (gmap-unifs (get-gmap-unifs l-content))
	   (dummy-entry 
	    (get-dummy-unexpanded-lex-entry form :unifs gmap-unifs))
	   (e-from (smaf-node-to-chart-node source))
	   (e-to (smaf-node-to-chart-node target))
    	   (cfrom (or (point-to-char-point from addressing)
		      (get-min-edge-cfrom children)))
	   (cto (or (point-to-char-point to addressing)
		    (get-max-edge-cto children)))
	   err-flag
	   )
      (unless (or stem dummy-entry)
	(format t "~&WARNING: no stem/gType for SMAF edge '~a'" id)
	(setf err-flag t))
      (unless (integerp e-from)
	(format t "~&WARNING: missing source for SMAF edge '~a'" id)
	(setf err-flag t))
      (unless (integerp e-to)
	(format t "~&WARNING: missing target for SMAF edge '~a'" id)
	(setf err-flag t))
      (when (and (integerp e-from)
		 (integerp e-to)
		 (not (= e-from (leaf-edges-from leaf-edges))))
	(format t "~&WARNING: source mismatch between SMAF edge '~a' and it's daughters" id)
	(setf err-flag t))
      (when (and (integerp e-from)
		 (integerp e-to)
		 (not (= e-to (leaf-edges-to leaf-edges))))
	(format t "~&WARNING: target mismatch between SMAF edge '~a' and it's daughters" id)
	(setf err-flag t))
      (unless err-flag
	(make-morpho-stem-edge 
	 :id (smaf-id-to-edge-id id)
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
	 :partial-tree (saf-fs-partial-tree-2-list-partial-tree partialTree)
	 :l-content dummy-entry
	 )))))

;; extract gMap.* features
;; OUT: (:feat . "val")*
(defun get-gmap-unifs (l-content)
  (loop 
      for fv in l-content
      for feat = (saf-fv-feature fv)
      for val = (saf-fv-value fv)
      when (string= (subseq feat 0 5)
		    "gMap.")
      collect
	(cons
	 (intern (subseq feat 5) :keyword)
	 val)))
	 
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

(defun saf-fs-partial-tree-2-list-partial-tree (fs)
  (if (null fs)
      nil
    (let* ((first (saf-fs-path-value '("first") fs))
	   (rule2 (saf-fs-path-value '("rule") first)) 
	   (str2 (saf-fs-path-value '("str") first))
	   (rule (if (stringp rule2)
		     (intern rule2)
		   (error "string expected for saf-fs 'rule': ~a" rule2)))
	   (str (if (stringp str2)
		     (intern str2)
		   (error "string expected for saf-fs 'str': ~a" str2)))
	   (rest (saf-fs-path-value '("rest") fs)))
      (cons (list rule str) (saf-fs-partial-tree-2-list-partial-tree rest)))))
  
(defun saf-fs-path-value (path fs)
  (cond
   ((null fs)
    nil)
   ((null path)
    fs)
   ((listp fs)
    (saf-fs-path-value 
     (cdr path)
     (saf-fs-feature-value fs (car path))))))

(defun saf-fs-feature-value (fs feature)
  (let ((x (find feature fs 
		 :key #'saf-fv-feature
		 :test #'string=)))
    (if x
	(saf-fv-value x))))

(defun smaf-id-to-token-edge (id tedges &key hidden)
  (if hidden
      (find (HIDDEN-smaf-id-to-edge-id id hidden) tedges :key #'token-edge-id :test #'=)
    (find (smaf-id-to-edge-id id) tedges :key #'token-edge-id :test #'=)))

;; fix_me: addressing should be passed down
(defun point-to-char-point (point addressing)
  (if (null point)
      (return-from point-to-char-point))
  (unless addressing
    (error "ADDRESSING cannot be null"))
  (cond
   ((string= addressing :|char|) 
    ;(ignore-errors 
     (parse-integer point)
    ; )
    )
    ((string= addressing :|xpoint|) -1)
    (t (error "unknown addressing scheme '~a'" addressing))))

#+:null
(defun str-2-int (str)
  (if str (parse-integer str)))

;;
;;					;

(defvar *saf-document* nil)

(defun x-parse (text from to addressing &key document 
					     (char-map #'identity) 
					     (show-parse t))
  (unless (preprocessor:preprocessor-initialized-p)
    (error "please load preprocessor"))
  (setf *saf-document* document)
  (let ((str 
	 (cond
	  ((and from to addressing)
	   (x-span text from to addressing))
	  ((and (null from) (null to) (null addressing))
	   text)
	  (t
	   (error "from/to/addressing=~a/~a/~a" from to addressing))))
	(preprocessor:*local-to-global-point-mapping* char-map)
	(*text* text)
	(old-x-fspp-global (preprocessor::x-fspp-global preprocessor::*preprocessor*))
	)
    (setf *sentence* str)
;    
;    (format t "~%~%=.~a.~%~%" preprocessor:*local-to-global-point-mapping*)
;    
    (parse (preprocessor:x-preprocess str :format :maf) show-parse)
    (setf (preprocessor::x-fspp-global preprocessor::*preprocessor*)
      old-x-fspp-global)
    t))

(defun char-map-add-x (point)
  (if point
      (format nil "~a" (+ (or *char-map-add-offset* 0) (point-to-char-point point :|char|)))))

;;

(defun saf-fv-value! (x)
  (saf-fv-value x))

(defun saf-edge-l-content! (x)
  (saf-edge-l-content x))

#+:null
(defun parse-x nil
  (let ((*morph-option* :with-tokeniser-partial-tree))
    (parse *x)))

#+:null
(defun parse-y nil
  (let ((*morph-option* :with-tokeniser-partial-tree))
    (parse *y)))

(defun mps-str (x)
  (or (encode-mixed-as-str x) ""))

(defun get-dummy-unexpanded-lex-entry (orth &key unifs)
  (loop
      for (key . val) in unifs
      for (c-dummy c-path c-type) = (find key saf::*smaf-gmap* :key #'car)
      for val2 = (mps-str (if (eq c-type :sym) 
			      (intern val) 
			    val))
      collect (list val2 c-path '(MIXED)) into unifs
      do 
	(setf c-dummy c-dummy)
      finally 
	(return 
	  (get-dummy-unexpanded-lex-entry2 orth :unifs unifs))))

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
