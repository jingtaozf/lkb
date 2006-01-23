;; Copyright (c) 2006
;;;   Ben Waldron;
;;; see `licence.txt' for conditions.

(in-package :lkb)

(defvar *saf*)
(defvar *saf-dir* nil)
(defvar *saf-v* -1)

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
  daughters
  content
  form ; fix me
  )

(defstruct saf-fv
  feature
  value)

(defstruct saf-meta
  document
  addressing
  olac)

;; code to convert SAF (or MAF) XML into SAF object

(defun xml-to-saf-object (xml &key (saf-dir "~"))
  (let ((*saf-dir* saf-dir))
    (lxml-to-saf-object (xml-to-lxml xml))))

(defun lxml-to-saf-object (lxml)
  (let* ((lxml-body (check-doctype 
		     (remove-xml-header lxml)
		     '("saf" "maf")))
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
	  (if (eq (intern "olac:olac") (lxml-elt-name (car lxml)))
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
     :addressing (second (member '|addressing| saf-attributes))
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
	      (error "unhandled saf edge type")))
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
      (error "no elementes expected after fsm"))
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
	      collect (lxml-sentence-to-edge e))))
    (make-saf-lattice 
     :start-node (second (member '|init| fsm-attributes))
     :end-node (second (member '|final| fsm-attributes))
     :nodes nodes
     :edges (append token-edges annot-edges 
		    wordform-edges sentence-edges))))

(defun lxml-state-to-node (lxml-state)
  (lxml-elt-attr lxml-state "id"))

(defun lxml-token-to-edge (lxml-token &key type source target)
  (make-saf-edge
   :id (lxml-elt-attr lxml-token "id")
   :type (or type :leaf)
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
     :type (or type (lxml-elt-attr lxml-annot "type"))
     :source (or source (lxml-elt-attr lxml-annot "source"))
     :target (or target (lxml-elt-attr lxml-annot "target"))
     :daughters (split-str-on-spc (lxml-elt-attr lxml-annot "daughters"))
     :content (lxml-fs-content-to-fs fs)
     :form (lxml-elt-attr lxml-annot "form") ;; should go in content
     )))

(defun lxml-wordform-to-edge (lxml-wordform &key source target)
  (lxml-annot-to-edge lxml-wordform :type :non-leaf
		      :source source :target target))
  
(defun lxml-sentence-to-edge (lxml-sentence &key source target)
  (lxml-token-to-edge lxml-sentence :type :sentence
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
;; XML serialization
;;

(defun get-timestamp nil
  (multiple-value-bind
      (second minute hour date month year dummy1 dummy2 dummy3)
      (decode-universal-time (get-universal-time) 0)
    (+ dummy1 dummy2 dummy3)
    (format nil "~2,'0d:~2,'0d:~2,'0d ~d/~2,'0d/~d (UTC)"
	    hour
	    minute
	    second
	    month
	    date
	    year)))

(defun maf-header (&key (addressing :xchar) document)
  (saf-header :maf t :addressing addressing :document document))

(defun saf-header (&key (addressing :xchar) document (maf nil))
  (format nil
	  "<?xml version='1.0' encoding='UTF8'?><!DOCTYPE ~a SYSTEM '~a.dtd'><~a~a addressing='~a'><olac:olac xmlns:olac='http://www.language-archives.org/OLAC/1.0/' xmlns='http://purl.org/dc/elements/1.1/' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xsi:schemaLocation='http://www.language-archives.org/OLAC/1.0/ http://www.language-archives.org/OLAC/1.0/olac.xsd'><creator>LKB</creator><created>~a</created></olac:olac>"
	  (if maf "maf" "saf")
	  (if maf "maf" "saf")
	  (if maf "maf" "saf")
	  (if document
	      (format nil " document='~a'" (xml-escape (2-str document)))
	    "")
	  (xml-escape (2-str addressing))
	  (xml-escape (get-timestamp))))
;;
;; batch processing
;; SENTENCE -> PARSE
;;

(defun process-standoff-sentence-file (filename &key show-parse)
  (process-saf-file-sentences filename :show-parse show-parse))
  
(defun process-saf-file-sentences (filename &key show-parse)
  (with-open-file 
      (ofile 
       (merge-pathnames 
	(make-pathname 
	 :name (format nil "~a.out" (pathname-name (pathname filename))))
	(pathname filename))
       :direction :output
       :if-exists :overwrite
       :if-does-not-exist :create)
    (format t "~%OUTPUT FILE: ~a" (namestring ofile))
    (process-saf-sentences
     (xml-to-saf-object 
      (read-file-to-string filename)
      :saf-dir (pathname-directory (pathname filename)))
     :ostream ofile
     :show-parse show-parse)))

(defun process-saf-sentences (saf &key (ostream t) show-parse)
  (let* ((textfilename (saf-meta-document (saf-meta saf)))
	 (text
	  (if textfilename
	      (read-file-to-string textfilename))))
    (format ostream "~a"
	    (saf-header :addressing "char"
			:document (saf-meta-document (saf-meta saf))))
    (loop for s in 
	  (sort (copy-list (saf-lattice-edges (saf-lattice saf)))
		#'string< :key #'saf-edge-from)
	do
	    (cond
	     ((saf-meta-document (saf-meta saf))
	      (let ((*char-map-add-offset* 
		     (point-to-char-point (saf-edge-from s) "char")))
		(setf *char-map-add-offset* *char-map-add-offset*)
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
	      
	    (dump-sentence-analyses s ostream))
    (format ostream "~&</saf>")))

;;based on mrs::output-mrs-after-parse
(defun dump-sentence-analyses (s &optional (stream t))
  (let ((*print-circle* nil))
    (loop for edge in *parse-record* 
	do
	  (let ((mrs (mrs::extract-mrs edge)))
	    (format stream "~&<annot type='parse' daughters='~a' edge='~a'>" ;;move edge into content
		    (saf-edge-id s)
		    (lkb::edge-id edge))
	    ;;(format stream "~&~A~&" 
	    ;;(lkb::parse-tree-structure edge))
	    (mrs::output-rmrs1 (mrs::mrs-to-rmrs mrs) 'mrs::xml stream)
	    (format stream "~&</annot>")))))

;(defun xml-sentences-to-saf-object (xml)
;  (lxml-to-saf-object (xml-to-lxml xml)))

;(defun lxml-sentences-to-saf-object (lxml)
;  (setf lxml 
;    (first (check-doctype (remove-xml-header lxml) "sentences")))
;  (make-saf 
;   :meta (make-saf-meta 
;	  :document (lxml-elt-attr lxml "document")
;	  :addressing (lxml-elt-attr lxml "addressing"))
;   :lattice (lxml-sentences-to-lattice lxml)))

;(defun lxml-sentences-to-lattice (lxml)
;  (unless (eq '|sentences| (lxml-elt-name lxml))
;    (error "<sentences> expected"))
;  (let ((contents (lxml-elt-contents lxml)))
;    (make-saf-lattice
;     :start-node 0
;     :end-node (length contents)
;     :nodes (loop for s in contents
;		collect (lxml-elt-attr s "id"))
;     :edges (loop for s in contents
;		collect (lxml-sentence2-to-edge s)))))

;(defun lxml-sentence2-to-edge (lxml)
;  (unless (eq '|sentence| (lxml-elt-name lxml))
;    (error "<sentence> expected"))
;  (make-saf-edge
;   :type :sentence
;   :id (lxml-elt-attr lxml "id")
;   :from (lxml-elt-attr lxml "from")
;   :to (lxml-elt-attr lxml "to")))

;;
;; SAF -> tchart
;;

(defun saf-num-lattice-nodes (saf)
  (length (saf-lattice-nodes (saf-lattice saf))))

(defun saf-setup-morphs (saf)
  (case *morph-option*
    (:with-tokeniser-partial-tree
	(saf-to-tchart saf))
    (t 
     (saf-to-tchart saf
		    :filter #'(lambda (x) (eq (saf-edge-type x) :leaf))))))

;;

(defun xml-maf-to-tchart (xml)
  (saf-to-tchart (xml-to-saf-object xml)))
  
(defun saf-to-tchart (saf &key (filter #'identity)
			       tchart)
  (unless tchart
    (setf *tchart* (make-tchart))
    (setf *tchart-max* 0))
  (setf *saf* saf)
;  (unless (member (saf-meta-addressing (saf-meta saf)) '("xpoint" "char")
;		  :test #'string=)
;    (error "Unhandled addressing attribute (~a)" (saf-meta-addressing (saf-meta saf))))
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
      when (eq (saf-edge-type e) :leaf)
      collect e into leafs
      when (eq (saf-edge-type e) :non-leaf)
      collect e into non-leafs
      finally     
	(loop for e in (append leafs non-leafs)
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
    (:leaf (saf-edge-to-tedge saf-edge :addressing addressing))
    (:non-leaf (saf-edge-to-medge saf-edge))))

(defun saf-edge-to-tedge (saf-edge &key addressing)
  (unless (eq :leaf (saf-edge-type saf-edge))
    (error "leaf edge expected"))
  (with-slots (id source target from to content) saf-edge
      (make-token-edge 
       :id (id-to-int id)
       :from (id-to-int source)
       :to (id-to-int target)
       :string content
       :cfrom (point-to-char-point from addressing)
       :cto (point-to-char-point to addressing)
       :word (string-upcase content)
       :leaves (list content))))

(defun saf-edge-to-medge (saf-edge)
  (unless (eq :non-leaf (saf-edge-type saf-edge))
    (error "non-leaf edge expected"))
  ;; assume tedges already in chart
  (with-slots (id source target daughters content form) saf-edge
    (let* ((children 
	    (loop for d in daughters
		collect (id-to-token-edge d (get-tedges *tchart*)))) 
	   (leaf-edges children) ;;fix me
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
       :stem (saf-fs-path-value '("stem") (saf-edge-content saf-edge))
       :partial-tree (partial-tree-from-content (saf-edge-content saf-edge))
       ))))

(defun partial-tree-from-content (content)
  (let ((x (saf-fs-path-value '("partial-tree") content)))
    (if (stringp x)
	(read-from-string x))))

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
  (unless addressing
    (setf addressing (saf-meta-addressing (saf-meta *saf*))))
  (cond
    ((string= addressing "char") (parse-integer point))
    ((string= addressing "xpoint") -1)))

;; id: first char ignored, rest gives integer
(defun id-to-int (id)
  (let ((i (if id (parse-integer (subseq id 1)))))
    (if i
	(and (setf *edge-id* (max i *edge-id*))
	     i)
      (- (incf *edge-id*)))))
