;; Copyright (c) 2006
;;;   Ben Waldron;
;;; see `licence.txt' for conditions.

;;
;; code to convert SAF XML into SAF object
;;

;;
;; TODO: complete move to :saf namespace
;;

(in-package :saf)

(defvar *char-map-add-offset*)

(defvar *lmap* nil)
(defvar *dir* nil)

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

(defun saf-lxml-to-saf-object (lxml)
  (unless (member (string (lxml::lxml-elt-name lxml)) '("maf" "saf" "smaf")
		  :test #'string=)
    (error "smaf, saf element expected as body"))
  (let* ((saf-attributes
	  (lxml::lxml-elt-attributes lxml))
	 (lxml (cdr lxml))
	 (text
	  (if (eq 'lxml::|text| (lxml::lxml-elt-name (car lxml)))
	      (pop lxml)))
	 (olac 
	  (if (eq 'lxml::|olac:olac| (lxml::lxml-elt-name (car lxml)))
	      (pop lxml))))
    (make-saf
     :meta (get-saf-meta saf-attributes :olac olac :text text)
     :lattice (get-saf-lattice lxml))))

;; fix_me: get rid of global *lmap*
(defun xml-to-saf-object (xml &key (dir "~") (l-map *lmap*))
  (let ((*dir* dir))
    (unless *lmap*
      (setf l-map (saf::get-default-saf-l-map)))
    (saf::instantiate-l-content
     (lxml-to-saf-object (lxml::xml-to-lxml xml))
     l-map)
    ))

(defun lxml-to-saf-object (lxml)
  (let* ((lxml-body (lxml::elements-only
		     (lxml::check-doctype 
		      (lxml::remove-xml-header lxml)
		      '("smaf" "saf" "maf"))))
	 (lxml-body-1 (first lxml-body)))
    (cond
     ((null lxml-body)
      (error "empty xml body"))
     ((cdr lxml-body)
      (error "xml body expected to contain single element"))
     (t
      (saf-lxml-to-saf-object lxml-body-1)))))

(defun get-saf-meta (saf-attributes &key olac text)
  (let ((doc
	 (second (member 'lxml::|document| saf-attributes))))
    (if doc
	(unless (eq :absolute (car (pathname-directory (pathname doc))))
	  (setf doc
	    (merge-pathnames doc (make-pathname :directory *dir*)))))
    (make-saf-meta
     :document doc
     :addressing (intern (string-downcase 
			  (or
			   (second (member 'lxml::|addressing| saf-attributes))
			   'lxml::|char|)))
     :olac (get-olac-meta olac)
     :text text)))

(defun get-olac-meta (lxml-olac)
  (loop
      for e in (lxml::lxml-elts lxml-olac)
      collect
	(make-saf-fv :feature (string (lxml::lxml-elt-name e))
		     :value (lxml::lxml-elt-text-content e))))

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
	   (case (lxml::lxml-elt-name e)
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
	      (error "unhandled saf edge type: ~a" (lxml::lxml-elt-name e))))
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
   ((member (lxml::lxml-elt-name (car lxml)) '(lxml::|fsm| lxml::|lattice|));; SAF / SMAF
    (when (cdr lxml)
      (error "no elements expected after fsm/lattice"))
    (get-saf-lattice-from-fsm (car lxml)))
   (t
    (error "malformed lattice: ~a" lxml)
    )))

(defun get-saf-lattice-from-fsm (lxml-fsm)
  (let* ((fsm-attributes (lxml::lxml-elt-attributes lxml-fsm))
	 (nodes 
	  (loop for x in (lxml::lxml-elt-elts lxml-fsm "state") 
	      collect (lxml::lxml-elt-attr x "id")))
	 (token-edges 
	  (loop for e in (lxml::lxml-elt-elts lxml-fsm "token")
	      collect (lxml-token-to-edge e)))
	 (annot-edges 
	  (append
	   (loop for e in (lxml::lxml-elt-elts lxml-fsm "annot");; SAF
	       collect (lxml-annot-to-edge e))
	   (loop for e in (lxml::lxml-elt-elts lxml-fsm "edge");; SMAF
	       collect (lxml-annot-to-edge e))))
	 (wordform-edges 
	  (loop for e in (lxml::lxml-elt-elts lxml-fsm "wordForm") ;;shorthand
	      collect (lxml-wordform-to-edge e)))
	 (sentence-edges 
	  (loop for e in (lxml::lxml-elt-elts lxml-fsm "sentence") ;;shorthand
	      collect (lxml-sentence-to-edge e)))
	 (all-edges (append token-edges annot-edges 
			    wordform-edges sentence-edges))
	 (start-node (second (member 'lxml::|init| fsm-attributes)))
	 (end-node (second (member 'lxml::|final| fsm-attributes)))
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
       :edges all-edges)
      )))

;; when set, clobber edges will destroy competing non-clobber edges/paths
(defvar *clobber-p* nil)

(defun postprocess-lattice (lattice)
  ;; apply clobber edges
  (if *clobber-p*
      (clobber lattice)))

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
	for id = (saf-edge-id e)
	if (member id ids :test 'string=)
	do (format t "~%;;; WARNING: invalid lattice input (duplicate id: ~S)" id)
	   (setf consistent nil)
	else
	collect id into ids)
    consistent))

(defun lxml-state-to-node (lxml-state)
  (lxml::lxml-elt-attr lxml-state "id"))

(defun saf-type (str)
  (intern str :keyword))

;; call lxml-annot-to-edge instead?
(defun lxml-token-to-edge (lxml-token &key type source target)
  (make-saf-edge
   :id (lxml::lxml-elt-attr lxml-token "id")
   :type (saf-type (or type 'lxml::|token|))
   :source (or source (lxml::lxml-elt-attr lxml-token "source"))
   :target (or target (lxml::lxml-elt-attr lxml-token "target"))
   :from (lxml::lxml-elt-attr lxml-token "from")
   :to (lxml::lxml-elt-attr lxml-token "to")
   :content (lxml::lxml-elt-attr lxml-token "value")))

(defun lxml-rmrs-to-rmrs (lxml)
  (if lxml
      (list
       (make-saf-fv
	:feature :rmrs
	:value (mrs::read-rmrs 
		(car 
		 ;; necessary since read-rmrs expects :mrs-interned symbols
		 (shift-package lxml :mrs)) :rasp)))))
  
(defun lxml-annot-to-edge (lxml-annot &key type source target)
  (let* ((id (lxml::lxml-elt-attr lxml-annot "id"))
	 (fs-list (lxml::lxml-elt-elts lxml-annot "fs"))
	 (slots (lxml::lxml-elt-elts lxml-annot "slot"))
	 (rmrs (lxml::lxml-elt-elts lxml-annot "rmrs"))
	 (fs (if (cdr fs-list)
		 (error "max 1 fs element allowed in wordform")
	       (car fs-list)))
	 (content 
	  (or 
	   (lxml::lxml-elt-text-content2 lxml-annot) ;; simple content embedded
	   (lxml::lxml-elt-attr lxml-annot "value") ;; simple content value attr
	   (append ;; complex content
	    (lxml-fs-content-to-fs fs) 
	    (lxml-slots-to-fs slots)
	    (lxml-rmrs-to-rmrs rmrs)
	    )
	   ))
	 (source (or source (lxml::lxml-elt-attr lxml-annot "source")))
	 (target (or target (lxml::lxml-elt-attr lxml-annot "target")))
	 )
    (unless content
      (format t "~&WARNING: no/unknown content for SMAF edge '~a'" id))
    (unless source
      (format t "~&WARNING: missing source for SMAF edge '~a'" id))
    (unless target
      (format t "~&WARNING: missing target for SMAF edge '~a'" id))
    (make-saf-edge
     :id id
     :type (saf-type (or type (lxml::lxml-elt-attr lxml-annot "type")))
     :source source
     :target target
     :deps (lkb::split-str-on-spc (lxml::lxml-elt-attr lxml-annot "deps"))
     :content content
     :from (or (lxml::lxml-elt-attr lxml-annot "from") 
	       (lxml::lxml-elt-attr lxml-annot "cfrom"))
     :to (or
	  (lxml::lxml-elt-attr lxml-annot "to")
	  (lxml::lxml-elt-attr lxml-annot "cto"))
     )))

;; special case
(defun lxml-wordform-to-edge (lxml-wordform &key source target)
  (lxml-annot-to-edge lxml-wordform :type 'lxml::|wordForm|
		      :source source :target target))

;; special case
(defun lxml-sentence-to-edge (lxml-sentence &key source target)
  (lxml-token-to-edge lxml-sentence :type 'lxml::|sentence|
		      :source source :target target))

(defun lxml-fs-content-to-fs (lxml)
  (cond
   ((null lxml)
    nil)
   ((stringp lxml) ;; shorthand
    lxml)   
   ((eq (lxml::lxml-elt-name lxml) 'lxml::|fs|)
    (loop for f in (lxml::lxml-elt-elts lxml "f")
	collect (make-saf-fv
		 :feature (lxml::lxml-elt-attr f "name")
		 :value (lxml-fs-content-to-fs (first (lxml::lxml-elt-contents f))))))
   ((eq (lxml::lxml-elt-name lxml) 'lxml::|binary|)
    :binary-ignored)
   ((eq (lxml::lxml-elt-name lxml) 'lxml::|symbol|)
    :symbol-ignored)
   ((eq (lxml::lxml-elt-name lxml) 'lxml::|numeric|)
    :numeric-ignored)
   ((eq (lxml::lxml-elt-name lxml) 'lxml::|string|)
    (let ((str (first (lxml::lxml-elt-contents lxml))))
      (unless (stringp str)
	(error "string expected"))
      str))))

(defun lxml-slots-to-fs (lxml-slots)
  (loop
      for s in lxml-slots
      for feat = (lxml::lxml-elt-attr s "name")
      for val = (first (lxml::lxml-elt-contents s))
      for val-str = (if (stringp val)
			val
		      (error "string expected"))
      collect 
	(make-saf-fv :feature feat
		     :value val-str)))



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

(defun saf-fv-value! (x)
  (saf-fv-value x))

(defun saf-edge-l-content! (x)
  (saf-edge-l-content x))

;; extract gMap.* features
;; OUT: (:feat . "val")*
(defun get-gmap-unifs (l-content)
  (loop 
      for fv in l-content
      for feat = (saf-fv-feature fv)
      for val = (saf-fv-value fv)
      when (and
	    (> (length feat) 4)
	    (string= (subseq feat 0 5)
		    "gMap."))
      collect
	(cons
	 (intern (subseq feat 5) :keyword)
	 val)))
	 
(defun process-saf-sentences (saf &key (ostream t) show-parse reset-unanalysed-tokens pprint)
  (let* ((textfilename (saf-meta-document (saf-meta saf)))
	 (text
	  (if textfilename
	      (lkb::read-file-to-string textfilename))))
    (format t "~&;;; Data file: ~a" (saf-meta-document (saf-meta saf)))
    (format ostream "~a"
	    (preprocessor::saf-header :addressing 'lxml::|char|
			:document (saf-meta-document (saf-meta saf))))
    (when reset-unanalysed-tokens
      (setf lkb::*unanalysed-tokens* nil))
    (loop 
	for s in 
	  (sort (loop for e in (saf-lattice-edges (saf-lattice saf))
		    when (eq 'lxml::|sentence| (saf-edge-type e))
		    collect e)
		#'< 
		:key #'(lambda (x)
			 (or
			  (point-to-char-point 
			   (saf-edge-from x) 'lxml::|char|)
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
		 (let ((lkb::*generate-messages-for-all-unanalysed-tokens* t)
		       ;(*char-map-add-offset* 
			;(point-to-char-point (saf-edge-from s) 'lxml::|char|))
		       )
		   (setf *char-map-add-offset* (point-to-char-point (saf-edge-from s) 'lxml::|char|))
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
	  (dump-sentence-analyses s :stream ostream :pprint pprint))
    (format ostream "~&</saf>")))

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
	(lkb::*text* text)
	(old-x-fspp-global (preprocessor::x-fspp-global preprocessor::*preprocessor*))
	)
    (setf lkb::*sentence* str)
;    
;    (format t "~%~%=.~a.~%~%" preprocessor:*local-to-global-point-mapping*)
;    
    (lkb::parse (preprocessor:x-preprocess str :format :maf) show-parse)
    (setf (preprocessor::x-fspp-global preprocessor::*preprocessor*)
      old-x-fspp-global)
    t))

(defun char-map-add-x (point)
  (if point
      (format nil "~a" (+ (or *char-map-add-offset* 0) (point-to-char-point point 'lxml::|char|)))))

(defun point-to-char-point (point addressing)
  (if (null point)
      (return-from point-to-char-point))
  (unless addressing
    (error "ADDRESSING cannot be null"))
  (cond
   ((string= addressing 'lxml::|char|) 
    ;(ignore-errors 
     (parse-integer point)
    ; )
    )
    ((string= addressing 'lxml::|xpoint|) -1)
    (t (error "unknown addressing scheme '~a'" addressing))))

#+:mrs
(defun dump-sentence-analyses (s &key (stream t))
  (dump-sentence-analyses2 :s-id (saf-edge-id s) :stream stream))

#+:mrs
;;based on mrs::output-mrs-after-parse
(defun dump-sentence-analyses2 (&key (s-id) 
				     (stream t))
  (let ((*print-circle* nil))
    (loop for edge in lkb::*parse-record* 
	do
	  (let ((mrs (mrs::extract-mrs edge)))
	    (format stream "<annot type='parse' deps='~a'>" ;;move edge into content
		    (or s-id ""))
	    (format stream "<slot name='edge'>~a</slot>" (lkb::xml-escape (lkb::2-str (lkb::edge-id edge))))
	    ;;(format stream "~&~A~&" 
	    ;;(lkb::parse-tree-structure edge))
	    (let ((mrs::*write-compact-xml* t))
	      (setf mrs::*write-compact-xml* mrs::*write-compact-xml*) ;;avoid compiler warning
	      (mrs::output-rmrs1 (mrs::mrs-to-rmrs mrs) 'mrs::xml stream))
	    (format stream "~&</annot>")))))

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
  
(defun saf-num-lattice-nodes (saf)
  (length (saf-lattice-nodes (saf-lattice saf))))

(defun sort-edges-by-from (edges &key addressing)
  (sort (copy-list edges)
	'<
	:key (lambda (x)
	       (or (point-to-char-point (saf-edge-from x) addressing)) 0)
	))

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
     (smaf::xml-to-saf-object 
      (lkb::read-file-to-string filename)
      :dir (pathname-directory (pathname filename)))
     :ostream ofile
     :show-parse show-parse
     :reset-unanalysed-tokens reset-unanalysed-tokens)))

;;
;; point schemes:
;; - char
;; - xpoint
;; - line
;;

(defun x-span (text from to addressing)
  (let ((cfrom (point-to-char-point from addressing))
	(cto (point-to-char-point to addressing)))
    (cond
     ((string= "char" addressing)
      (and cfrom cto
	   (subseq text cfrom cto)))
     ((string= "xpoint" addressing)
      (error "addressing scheme 'xpoint' not implemented"))
     (t
      (error "unknown addressing scheme '~a'" addressing)))))

(defun shift-package (lxml package)
  (loop
      for x in lxml
      collect 
	(cond
	 ((listp x)
	  (shift-package x package))
	 ((symbolp x)
	  (intern (string x) package))
	 (t
	  x))))

;;
;; clobber code
;;

;; add 1-paths from node-z to agenda
(defun update-paths-x2y-agenda (node-z agenda &key lattice)
  (unless lattice
    (error "missing LATTICE argument"))
  (loop
      for edge in (get-edges-source node-z :lattice lattice)
      for source = (saf-edge-source edge)
      for target = (saf-edge-target edge)
      do (pushnew (cons source target) agenda
		  :test #'equalp))
  agenda)

;; returns array of forward pointers for paths node-x to node-y
(defun get-paths-x2y (node-x node-y &key lattice)
  (unless lattice
    (error "missing LATTICE argument"))
  (let* (;; create hash to store paths from x
	 (paths-from-x (make-hash-table :test #'equalp))
	 ;; initialise agenda
	 agenda)
    (setf agenda (update-paths-x2y-agenda node-x nil :lattice lattice))
    (unless (equalp node-x node-y)
      ;; process agenda items...
      (loop 
	  with processed = nil
	  while agenda
		;; next item
	  for item = (pop agenda)
	  for source = (car item)
	  for target = (cdr item)
	  unless (member item processed :test #'equalp)
	  do
	    ;(format t "~&item ~a" item)
	    ;; update array
	    (setf (gethash target paths-from-x)
	      (cons source
		    (gethash target paths-from-x)))
	    (unless (equalp target node-y)
	      ;; no loops, so no need to look further
	      (setf agenda
		(update-paths-x2y-agenda target agenda :lattice lattice)))
	    (push item processed)))
    ;; pick out result
    paths-from-x))

;; return edge set taken from all paths node-x to node-y
(defun get-edges-x2y (node-x node-y &key lattice)
  (unless lattice
    (error "missing LATTICE argument"))
  (loop
      with hash = (get-paths-x2y node-x node-y :lattice lattice)
      for target being each hash-key in hash
      for sources = (gethash target hash)
      append
	(loop for source in sources
	    append (get-edges-source-target source target :lattice lattice))))

;; return edges from source node to target node
(defun get-edges-source-target (source target &key lattice)
  (unless lattice
    (error "missing LATTICE argument"))
  ;; FIXME: inefficient
  (loop 
      for edge in (saf-lattice-edges lattice)
      for source1 = (saf-edge-source edge)
      for target1 = (saf-edge-target edge)
      when (and (equalp source1 source)
		(equalp target1 target))
      collect edge))

;; return outgoing edges from source node
(defun get-edges-source (source &key lattice)
  (unless lattice
    (error "missing LATTICE argument"))
  ;; FIXME: inefficient
  (loop 
      for edge in (saf-lattice-edges lattice)
      for source1 = (saf-edge-source edge)
      when (equalp source1 source)
      collect edge))

;; when set, clobber rules enabled
(defvar *warning-clobber* nil)

;; for each clobber edge, remove all non-clobber edges between
;; source and target nodes
(defun clobber (lattice)
  (let* ((edges (saf-lattice-edges lattice))
	 (clobber-edges
	  (loop 
	      for edge in edges
	      for l-content = (saf-edge-l-content edge)
	      when (saf-fs-feature-value l-content "clobber")
	      collect edge)))
    (loop
	for clobber-edge in clobber-edges
	for source = (saf-edge-source clobber-edge)		    
	for target = (saf-edge-target clobber-edge)
	for edges = (get-edges-x2y source target :lattice lattice)
	for clobbered-edges =
	  ;; FIXME: inefficient
	  (loop
	      for edge in edges
	      unless (member edge clobber-edges)
	      do
		(if *warning-clobber*
		    (format t "~%;;; WARNING: edge ~a clobbered" (saf-edge-id edge)))
	      and
	      collect edge)
	do
	  (setf (saf-lattice-edges lattice)
	    (set-difference (saf-lattice-edges lattice) clobbered-edges))))
    lattice)