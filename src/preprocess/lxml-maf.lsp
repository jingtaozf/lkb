;;; Copyright (c) 2005-2006
;;;   Ben Waldron;
;;; see `licence.txt' for conditions.

(in-package :lkb)

;; code to convert MAF XML into SAF object

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

(defun xml-to-saf-object (xml)
  (lxml-to-saf-object (xml-to-lxml xml)))

(defun xml-to-lxml (xml)
  (discard-whitespace (net.xml.parser:parse-xml xml)))

;; if xml header, remove and check correct
(defun remove-xml-header (lxml)
  (let ((lxml-xml (and (eq :xml (lxml-pi-name (car lxml)))
			      (pop lxml))))
    (unless (string= "1.0" 
		     (lxml-pi-attr lxml-xml "version" :keyword t))
      (error "expected XML version 1.0: got ~a" lxml-xml)))
  lxml)

(defun check-doctype (lxml doctype)
  (let ((lxml-doctype (and (eq :doctype (lxml-pi-name (car lxml)))
			   (pop lxml))))
    (unless (eq (intern doctype :keyword) (second lxml-doctype))
      (error "expected DOCTYPE ~a got ~a" doctype lxml-doctype)))
  lxml)
  

(defun lxml-to-saf-object (lxml)
  (let* ((lxml-body (check-doctype 
		     (remove-xml-header lxml)
		     "maf"))
	 (lxml-body-1 (first lxml-body)))
    (cond
     ((null lxml-body)
      (error "empty xml body"))
     ((cdr lxml-body)
      (error "xml body expected to contain single element"))
     (t
      (maf-lxml-to-saf-object lxml-body-1)))))

(defun maf-lxml-to-saf-object (lxml)
  (unless (eq (intern "maf") (lxml-elt-name lxml))
    (error "maf element expected as body"))
  (let* ((maf-attributes
	  (lxml-elt-attributes lxml))
	 (lxml (cdr lxml))
	 (olac 
	  (if (eq (intern "olac:olac") (lxml-elt-name (car lxml)))
	      (pop lxml)))
	 (fsm
	  (if (eq (intern "fsm") (lxml-elt-name (car lxml)))
	      (pop lxml))))
    ;(print maf-attributes)
    ;(print olac)
    ;(print fsm)
    (when lxml
      (error "unhandled elements inside maf element: ~a" lxml))
    (make-saf
     :meta (get-maf-meta maf-attributes olac)
     :lattice (get-maf-lattice fsm))))

(defun get-maf-meta (maf-attributes lxml-olac)
  (declare (ignore lxml-olac))
  (make-saf-meta
   :document (second (member '|document| maf-attributes))
   :addressing (second (member '|addressing| maf-attributes))
   :olac :ignored))

(defun get-maf-lattice (lxml-fsm)
  (let* ((fsm-attributes (lxml-elt-attributes lxml-fsm))
	 (nodes 
	  (loop for x in (lxml-elt-elts lxml-fsm "state") 
	      collect (lxml-elt-attr x "id")))
	 (token-edges 
	  (loop for e in (lxml-elt-elts lxml-fsm "token")
	      collect (lxml-token-to-edge e)))
	 (wordform-edges 
	  (loop for e in (lxml-elt-elts lxml-fsm "wordForm")
	      collect (lxml-wordform-to-edge e))))
    (make-saf-lattice 
     :start-node (second (member '|init| fsm-attributes))
     :end-node (second (member '|final| fsm-attributes))
     :nodes nodes
     :edges (append token-edges wordform-edges))))

(defun lxml-state-to-node (lxml-state)
  (lxml-elt-attr lxml-state "id"))

(defun lxml-token-to-edge (lxml-token)
  (make-saf-edge
   :id (lxml-elt-attr lxml-token "id")
   :type :leaf
   :source (lxml-elt-attr lxml-token "source")
   :target (lxml-elt-attr lxml-token "target")
   :from (lxml-elt-attr lxml-token "from")
   :to (lxml-elt-attr lxml-token "to")
   :content (lxml-elt-attr lxml-token "value")))

(defun lxml-wordform-to-edge (lxml-wordform)
  (let* ((fs-list (lxml-elt-elts lxml-wordform "fs"))
	 (fs (if (cdr fs-list)
		 (error "max 1 fs element allowed in wordform")
	       (car fs-list))))
    (make-saf-edge
     :id (lxml-elt-attr lxml-wordform "id")
     :type :non-leaf
     :source (lxml-elt-attr lxml-wordform "source")
     :target (lxml-elt-attr lxml-wordform "target")
     :daughters (split-str-on-spc (lxml-elt-attr lxml-wordform "daughters"))
     :content (lxml-fs-content-to-fs fs)
     :form (lxml-elt-attr lxml-wordform "form") ;; should go in content
     )))

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
		 :value (lxml-fs-content-to-fs (first (lxml-elt-contents f)))))
    )
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
      str))
   ))
     
     