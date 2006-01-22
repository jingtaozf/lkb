;;; Copyright (c) 2005-2006
;;;   Ben Waldron;
;;; see `licence.txt' for conditions.

(in-package :lkb)

;; code to convert SAF (or MAF) XML into SAF object

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

(defvar *saf-dir* nil)
(defun xml-to-saf-object (xml &key (saf-dir "~"))
  (let ((*saf-dir* saf-dir))
    (lxml-to-saf-object (xml-to-lxml xml))))

(defun xml-to-lxml (xml)
  (discard-whitespace (net.xml.parser:parse-xml xml)))

;; if xml header, remove and check correct
(defun remove-xml-header (lxml)
  (let ((lxml-xml (and (eq :xml (lxml-pi-name (car lxml)))
		       (pop lxml))))
    (if (and lxml-xml
	     (not (string= "1.0" 
		      (lxml-pi-attr lxml-xml "version" :keyword t))))
	(error "expected XML version 1.0: got ~a" lxml-xml)
      LXML)))

(defun check-doctype (lxml doctype)
  (unless (listp doctype)
    (setf doctype (list doctype)))
  (let ((lxml-doctype (and (eq :doctype (lxml-pi-name (car lxml)))
			   (pop lxml))))  
    (if (and lxml-doctype
	     (not (member (string (second lxml-doctype)) doctype
			  :test #'string=)))
	(error "problem with DOCTYPE declaration")
      lxml)))

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

(defvar *saf-v* -1)
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
     
     