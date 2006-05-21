;; Copyright (c) 2006
;;;   Ben Waldron;
;;; see `licence.txt' for conditions.

(defpackage :saf
  (:use :common-lisp) 
  (:export))

(in-package :lkb)

(defvar *saf*)
(defvar *saf-dir* nil)
(defvar *saf-v* -1)
(defvar *saf-l-map* nil)

(defvar *char-map-add-offset*)

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
  form ; fix me
  )

(defstruct saf-fv
  feature
  value)

(defstruct saf-meta
  document
  addressing
  olac)

(defmethod print-object ((object saf) stream)
  (format 
   stream 
   "#[SAF]"
   ;(length (saf-lattice (saf-lattice-edges object)))
   ))

;; code to convert SAF (or MAF) XML into SAF object

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
		      '("saf" "maf"))))
	 (lxml-body-1 (first lxml-body)))
    (cond
     ((null lxml-body)
      (error "empty xml body"))
     ((cdr lxml-body)
      (error "xml body expected to contain single element"))
     (t
      (saf-lxml-to-saf-object lxml-body-1)))))

(defun saf-lxml-to-saf-object (lxml)
  (unless (member (string (lxml-elt-name lxml)) '("maf" "saf")
		  :test #'string=)
    (error "saf or maf element expected as body"))
  (let* ((saf-attributes
	  (lxml-elt-attributes lxml))
	 (lxml (cdr lxml))
	 (olac 
	  (if (eq '|olac:olac| (lxml-elt-name (car lxml)))
	      (pop lxml))))
    (make-saf
     :meta (get-saf-meta saf-attributes olac)
     :lattice (get-saf-lattice lxml))))

(defun get-saf-meta (saf-attributes lxml-olac)
  (declare (ignore lxml-olac))
  (let ((doc
	 (second (member '|document| saf-attributes))))
    (if doc
	(unless (eq :absolute (car (pathname-directory (pathname doc))))
	  (setf doc
	    (merge-pathnames doc (make-pathname :directory *saf-dir*)))))
    (make-saf-meta
     :document doc
     :addressing (intern (string-downcase (second (member '|addressing| saf-attributes))))
     :olac :ignored)))

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
	     (|token|
	      (lxml-token-to-edge e :source source :target target))
	     (|annot|
	      (lxml-annot-to-edge e :source source :target target))
	     (|wordForm|
	      (lxml-wordform-to-edge e :source source :target target))
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
   ((eq '|fsm| (lxml-elt-name (car lxml)))
    (when (cdr lxml)
      (error "no elements expected after fsm"))
    (get-saf-lattice-from-fsm (car lxml)))
   (t
    (make-saf-lattice-from-sequence lxml))))

(defun get-saf-lattice-from-fsm (lxml-fsm)
  (let* ((fsm-attributes (lxml-elt-attributes lxml-fsm))
	 (nodes 
	  (loop for x in (lxml-elt-elts lxml-fsm "state") 
	      collect (lxml-elt-attr x "id")))
	 (token-edges 
	  (loop for e in (lxml-elt-elts lxml-fsm "token")
	      collect (lxml-token-to-edge e)))
	 (annot-edges 
	  (loop for e in (lxml-elt-elts lxml-fsm "annot")
	      collect (lxml-annot-to-edge e)))
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
    (loop for e in all-edges
	do
	  (pushnew (saf-edge-source e) nodes :test #'string=)
	  (pushnew (saf-edge-target e) nodes :test #'string=))
    (unless (and start-node end-node)
      (loop 
	  with max and max-id
	  with min and min-id
	  for n in nodes
	  for id = (id-to-int n)
	  do
	    (when (or (null min) (< id min-id)) 
	      (setf min n) (setf min-id id))
	    (when (or (null max) (> id max-id)) 
	      (setf max n) (setf max-id id))
	  finally
	    (unless start-node (setf start-node min))
	    (unless end-node (setf end-node max))))
    (make-saf-lattice 
     :start-node start-node
     :end-node end-node
     :nodes nodes
     :edges all-edges)))

(defun lxml-state-to-node (lxml-state)
  (lxml-elt-attr lxml-state "id"))

(defun saf-type (str)
  (intern str :keyword))

(defun lxml-token-to-edge (lxml-token &key type source target)
  (make-saf-edge
   :id (lxml-elt-attr lxml-token "id")
   :type (saf-type (or type :|token|))
;   :type (or type :leaf)
   :source (or source (lxml-elt-attr lxml-token "source"))
   :target (or target (lxml-elt-attr lxml-token "target"))
   :from (lxml-elt-attr lxml-token "from")
   :to (lxml-elt-attr lxml-token "to")
   :content (lxml-elt-attr lxml-token "value")))

(defun lxml-annot-to-edge (lxml-annot &key type source target)
  (let* ((fs-list (lxml-elt-elts lxml-annot "fs"))
	 (fs (if (cdr fs-list)
		 (error "max 1 fs element allowed in wordform")
	       (car fs-list))))
    (make-saf-edge
     :id (lxml-elt-attr lxml-annot "id")
     :type (saf-type (or type (lxml-elt-attr lxml-annot "type")))
     :source (or source (lxml-elt-attr lxml-annot "source"))
     :target (or target (lxml-elt-attr lxml-annot "target"))
     :deps (split-str-on-spc (lxml-elt-attr lxml-annot "deps"))
     :content (or (lxml-elt-attr lxml-annot "value") (lxml-fs-content-to-fs fs))
     :from (lxml-elt-attr lxml-annot "from")
     :to (lxml-elt-attr lxml-annot "to")
     )))

(defun lxml-wordform-to-edge (lxml-wordform &key source target)
  (lxml-annot-to-edge lxml-wordform :type :|wordForm|
		      :source source :target target))
  
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
;;based on mrs::output-mrs-after-parse
(defun dump-sentence-analyses (s &optional (stream t))
  (let ((*print-circle* nil))
    (loop for edge in *parse-record* 
	do
	  (let ((mrs (mrs::extract-mrs edge)))
	    (format stream "~&<annot type='parse' deps='~a' edge='~a'>" ;;move edge into content
		    (saf-edge-id s)
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

;(defvar *saf-setup-morphs* '(:|token|))
;
;(defun saf-setup-morphs (saf)
;  (case *morph-option*
;    (:with-tokeniser-partial-tree ;; take both tok and morph edges
;	(saf-to-tchart saf))
;    (t ;; take only tok edges
;     (saf-to-tchart saf
;		    :filter #'(lambda (x) (member (saf-edge-type x) *saf-setup-morphs*))))))

(defun saf-setup-morphs (saf)
  (case *morph-option*
    (:with-tokeniser-partial-tree ;; take both tok and morph edges
	(saf-to-tchart saf))
    (t ;; take only tok edges
     (saf-to-tchart saf
		    :filter #'(lambda (x) 
				(equal "tok" 
				       (saf-fs-feature-value 
					(saf-edge-l-content x) 
					"edgeType")))))))


;;

(defun xml-maf-to-tchart (xml)
  (saf-to-tchart (xml-to-saf-object xml)))
  
(defun saf-to-tchart (saf &key (filter #'identity)
			       tchart)
  (unless tchart
    (setf *tchart* (make-tchart))
    (setf *tchart-max* 0))
  (setf *saf* saf)
  (saf-lattice-to-edges (saf-lattice saf)
			:filter filter
			:addressing (saf-meta-addressing (saf-meta saf)))
  *tchart*)

(defun saf-lattice-to-edges (saf-lattice &key (filter #'identity) addressing)
  (loop 
      for e in 
	(loop for f in (saf-lattice-edges saf-lattice)
	    when (funcall filter f)
	    collect f)
      when (eq (saf-edge-type e) :|token|)
      collect e into tokens
      when (eq (saf-edge-type e) :|wordForm|)
      collect e into wordForms
      finally     
	(loop for e in (append tokens wordForms)
	    do (augment-tchart-from-saf-edge e :addressing addressing))))

(defun next-tchart-edge-id (&optional (tchart *tchart*))
  (let ((edges (get-edges tchart)))
    (if edges
	(apply #'max (mapcar #'edge-id edges))
      0)))

;; to do: replace global *tchart* + *tchart-max* + ??? with objects
(defun augment-tchart-from-saf-edge (saf-edge &key addressing)
  (let* ((edge  (saf-edge-to-edge saf-edge :addressing addressing))
	 (from (edge-from edge))
	 (to (edge-to edge))
	 (cc (make-chart-configuration :begin from
				       :end to
				       :edge edge)))
    (setf (aref *tchart* to 0) (push cc (aref *tchart* to 0)))
    (setf (aref *tchart* from 1) (push cc (aref *tchart* from 1)))
    (when (> to *tchart-max*)
      ;(format t "~%WARNING: increasing *tchart-max* to ~a" to)
      (setf *tchart-max* to))
    *tchart*))

(defun saf-edge-to-edge (saf-edge &key addressing)
  (case (saf-edge-type saf-edge)
    (:|token| (saf-edge-to-tedge saf-edge :addressing addressing))
    (:|wordForm| (saf-edge-to-medge saf-edge))))

(defun saf-edge-to-tedge (saf-edge &key addressing)
  (unless (eq :|token| (saf-edge-type saf-edge))
    (error ":|token| edge expected"))
  (with-slots (id source target from to l-content) saf-edge
    (let ((tokenStr (saf-fs-feature-value 
		     l-content 
		     "tokenStr")))
      (make-token-edge 
       :id (id-to-int id)
       :from (id-to-int source)
       :to (id-to-int target)
       :string tokenStr
       :cfrom (point-to-char-point from addressing)
       :cto (point-to-char-point to addressing)
       :word (string-upcase tokenStr)
       :leaves (list tokenStr)))))

(defun saf-edge-to-medge (saf-edge)
  (unless (eq :|wordForm| (saf-edge-type saf-edge))
    (error ":|wordForm| edge expected"))
  ;; assume tedges already in chart
  (with-slots (id source target deps l-content) saf-edge
    (let* ((children 
	    (loop for d in deps
		collect (id-to-token-edge d (get-tedges *tchart*)))) 
	   (leaf-edges children) ;;fix me
	   (children-words
	    (loop for l in leaf-edges
		collect (token-edge-string l)))
	   (form (str-list-2-str children-words))
	   (stem (saf-fs-feature-value l-content "stem"))
	   (partialTree (saf-fs-feature-value l-content "partialTree"))
	   )
      (unless (= (id-to-int source) (leaf-edges-from leaf-edges))
	(error "source must match that of leaf edges"))
      (unless (= (id-to-int target) (leaf-edges-to leaf-edges))
	(error "target must match that of leaf edges"))
      (make-morpho-stem-edge 
       :id (id-to-int id)
       :children children
       :leaves (loop for x in leaf-edges collect (edge-string x))
       :from (id-to-int source)
       :to (id-to-int target)
       :string form
       :word (string-upcase form)
       :current (string-upcase form)
       :stem stem
       :partial-tree (saf-fs-partial-tree-2-list-partial-tree partialTree)
       ))))

;(defun saf-edge-to-medge (saf-edge)
;  (unless (eq :|wordForm| (saf-edge-type saf-edge))
;    (error ":|wordForm| edge expected"))
;  ;; assume tedges already in chart
;  (with-slots (id source target deps content) saf-edge
;    (let* ((children 
;	    (loop for d in deps
;		collect (id-to-token-edge d (get-tedges *tchart*)))) 
;	   (leaf-edges children) ;;fix me
;	   (children-words
;	    (loop for l in leaf-edges
;		collect (token-edge-string l)))
;	   (form (str-list-2-str children-words))
;	   )
;      (unless (= (id-to-int source) (leaf-edges-from leaf-edges))
;	(error "source must match that of leaf edges"))
;      (unless (= (id-to-int target) (leaf-edges-to leaf-edges))
;	(error "target must match that of leaf edges"))
;      (make-morpho-stem-edge 
;       :id (id-to-int id)
;       :children children
;       :leaves (loop for x in leaf-edges collect (edge-string x))
;       :from (id-to-int source)
;       :to (id-to-int target)
;       :string form
;       :word (string-upcase form)
;       :current (string-upcase form)
;       :stem (saf-fs-path-value '("stem") (saf-edge-content saf-edge))
;       :partial-tree (partial-tree-from-content (saf-edge-content saf-edge))
;       ))))

;(defun partial-tree-from-content (content)
;  (saf-fs-partial-tree-2-list-partial-tree
;   (saf-fs-path-value '("partial-tree") content)))

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

(defun id-to-token-edge (id tedges)
  (find (id-to-int id) tedges :key #'token-edge-id :test #'=))

;; fix_me: addressing should be passed down
(defun point-to-char-point (point addressing)
  (if (null point)
      (return-from point-to-char-point))
  (unless addressing
    (setf addressing (saf-meta-addressing (saf-meta *saf*))))
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

;; id: first char ignored, rest gives integer
(defun id-to-int (id)
  (handler-case 
      (let ((i (if id (parse-integer (subseq id 1)))))
	(if i
	    (and (setf *edge-id* (max i *edge-id*))
		 i)
	  (- (incf *edge-id*))))
    (error (condition)
      (error "unable to convert malformed id `~a': ~a" id condition))))

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
    (setf (preprocessor::x-fspp-global preprocessor::*preprocessor*) ;;hack: fix_me
      (push (preprocessor::make-x-fsr 
	     :type :replace
	     :source "<[^>]*>"
	     :scanner (ppcre:create-scanner "<[^>]*>")
	     :target " ")
	    (preprocessor::x-fspp-global preprocessor::*preprocessor*)))
    (setf (preprocessor::x-fspp-global preprocessor::*preprocessor*) ;;hack: fix_me
      (push (preprocessor::make-x-fsr 
	     :type :replace
	     :source "^[^<]*>"
	     :scanner (ppcre:create-scanner "^[^<]*>")
	     :target " ")
	    (preprocessor::x-fspp-global preprocessor::*preprocessor*)))
    (setf (preprocessor::x-fspp-global preprocessor::*preprocessor*) ;;hack: fix_me
      (push (preprocessor::make-x-fsr 
	     :type :replace
	     :source "<[^>]*$"
	     :scanner (ppcre:create-scanner "<[^>]*$")
	     :target " ")
	    (preprocessor::x-fspp-global preprocessor::*preprocessor*)))
    (setf (preprocessor::x-fspp-global preprocessor::*preprocessor*) ;;hack: fix_me
      (push (preprocessor::make-x-fsr 
	     :type :replace
	     :source "\\n"
	     :scanner (ppcre:create-scanner "\\n")
	     :target " ")
	    (preprocessor::x-fspp-global preprocessor::*preprocessor*)))
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