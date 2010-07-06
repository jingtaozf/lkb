;; Copyright (c) 2006
;;;   Ben Waldron;
;;; see `LICENSE' for conditions.

;;
;; code to convert SAF XML into SAF object
;;

;;
;; TODO: complete move to :saf namespace
;;

(in-package :saf)

(defun saf-lxml-to-saf-object (lxml)
  (unless (member (lxml::lxml-elt-name lxml) '(:|saf| :|smaf|))
    (error "smaf/saf element expected as body"))
  (let* ((saf-attributes
	  (lxml::lxml-elt-attributes lxml))
	 (lxml (cdr lxml))
	 (text
	  (if (eq :|text| (lxml::lxml-elt-name (car lxml)))
	      (pop lxml)))
	 (olacStr (string (lxml::lxml-elt-name (car lxml)))) 
	 (olac 
	  (if (or (string= "olac" olacStr) ;;S-xml
		  (string= "olac:olac" olacStr)) ;; pxml
	      (pop lxml))))
;	  (if (eq :|olac:olac| (lxml::lxml-elt-name (car lxml)))
;	      (pop lxml))))
    (make-saf
     :meta (get-saf-meta saf-attributes :olac olac :text text)
     :lattice (get-saf-lattice lxml))))

;; fix_me: get rid of global *config*
(defun xml-to-saf-object (xml &key (dir "~") (config (config)))
  (let ((*dir* dir))
    (saf::instantiate-l-content
     (lxml-to-saf-object (lxml::xml-to-lxml xml))
     config)
    ))

(defun lxml-to-saf-object (lxml)
  (cond
   ((null lxml)
    (error "empty xml body"))
   (t
    (saf-lxml-to-saf-object lxml))))

(defun get-saf-meta (saf-attributes &key olac text)
  (let ((doc
	 (second (member :|document| saf-attributes))))
    (if doc
	(unless (eq :absolute (car (pathname-directory (pathname doc))))
	  (setf doc
	    (merge-pathnames doc (make-pathname :directory *dir*)))))
    (make-saf-meta
     :document doc
     :addressing (get-addressing saf-attributes)
     :olac (get-olac-meta olac)
     :text text)))

(defun get-addressing (saf-attribs)
  (let ((addr (second (member :|addressing| saf-attribs))))
    (if addr
	(intern (string-downcase addr) :keyword)
      :|char|)))

(defun make-feat (x)
  (intern (string x) :keyword))

(defun get-olac-meta (lxml-olac)
  (loop
      for e in (lxml::lxml-elts lxml-olac)
      collect
	(make-saf-fv :feature (make-feat (lxml::lxml-elt-name e))
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
   ((member (lxml::lxml-elt-name (car lxml)) '(:|fsm| :|lattice|));; SAF / SMAF
    (when (cdr lxml)
      (error "no elements expected after fsm/lattice"))
    (get-saf-lattice-from-fsm (car lxml)))
   (t
    (error "malformed lattice: ~a" lxml)
    )))

(defun get-saf-lattice-from-fsm (lxml-fsm)
  (let* ((fsm-attributes (lxml::lxml-elt-attributes lxml-fsm))
	 (nodes 
	  (loop for x in (lxml::lxml-elt-elts lxml-fsm :|state|) 
	      collect (lxml::lxml-elt-attr x :|id|)))
	 (token-edges 
	  (loop for e in (lxml::lxml-elt-elts lxml-fsm :|token|)
	      collect (lxml-token-to-edge e)))
	 (annot-edges 
	  (append
	   (loop for e in (lxml::lxml-elt-elts lxml-fsm :|annot|);; SAF
	       for e2 = (lxml-annot-to-edge e)
	       when e2
	       collect e2)
	   (loop for e in (lxml::lxml-elt-elts lxml-fsm :|edge|);; SMAF
	       for e2 = (lxml-annot-to-edge e)
	       when e2
	       collect e2)
	   ))
	 (wordform-edges 
	  (loop for e in (lxml::lxml-elt-elts lxml-fsm :|wordForm|) ;;shorthand
	       for e2 = (lxml-wordform-to-edge e)
	       when e2
	       collect e2)
	      
	      
	 ; collect (lxml-wordform-to-edge e))
	 )
	 (sentence-edges 
	  (loop for e in (lxml::lxml-elt-elts lxml-fsm :|sentence|) ;;shorthand
	      collect (lxml-sentence-to-edge e)))
	 (all-edges (append token-edges annot-edges 
			    wordform-edges sentence-edges))
	 (start-node (second (member :|init| fsm-attributes)))
	 (end-node (second (member :|final| fsm-attributes)))
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
      (clobber lattice)
    lattice))

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
  (lxml::lxml-elt-attr lxml-state :|id|))

(defun saf-type (str)
  (intern str :keyword))

;; call lxml-annot-to-edge instead?
(defun lxml-token-to-edge (lxml-token &key type source target)
  (make-saf-edge
   :id (lxml::lxml-elt-attr lxml-token :|id|)
   :type (saf-type (or type :|token|))
   :source (or source (lxml::lxml-elt-attr lxml-token :|source|))
   :target (or target (lxml::lxml-elt-attr lxml-token :|target|))
   :from (lxml::lxml-elt-attr lxml-token :|from|)
   :to (lxml::lxml-elt-attr lxml-token :|to|)
   :content (lxml::lxml-elt-attr lxml-token :|value|)))

(defparameter *HOOK-lxml-rmrs-to-mrs-fn* #'(lambda (x) (declare (ignore x))))

(defun lxml-annot-to-edge (lxml-annot &key type source target)
  (let* ((id (lxml::lxml-elt-attr lxml-annot :|id|))
	 (fs-list (lxml::lxml-elt-elts lxml-annot :|fs|))
	 (slots (lxml::lxml-elt-elts lxml-annot :|slot|))
	 (rmrs (lxml::lxml-elt-elts lxml-annot :|rmrs|))
	 (fs (if (cdr fs-list)
		 (error "max 1 fs element allowed in wordform")
	       (car fs-list)))
	 (content 
	  (or 
	   (lxml::lxml-elt-text-content2 lxml-annot) ;; simple content embedded
	   (lxml::lxml-elt-attr lxml-annot :|value|) ;; simple content value attr
	   (append ;; complex content
	    (lxml-fs-content-to-fs fs) 
	    (lxml-slots-to-fs slots)
	    (funcall *HOOK-lxml-rmrs-to-mrs-fn* rmrs)
;	    #+:mrs (lxml-rmrs-to-rmrs rmrs)
	    )
	   ))
	 (source (or source (lxml::lxml-elt-attr lxml-annot :|source|)))
	 (target (or target (lxml::lxml-elt-attr lxml-annot :|target|)))
	 )
    (unless content
      (format t "~&WARNING: no/unknown content for SMAF edge '~a'" id))
    (unless source
      (format t "~&WARNING: missing source for SMAF edge '~a'" id))
    (unless target
      (format t "~&WARNING: missing target for SMAF edge '~a'" id))
    (when (string= source target)
      (format t "~&ERROR: identical 'source' and 'target' on SMAF edge '~a'" id)
      (return-from lxml-annot-to-edge nil))
    (make-saf-edge
     :id id
     :type (saf-type (or type (lxml::lxml-elt-attr lxml-annot :|type|)))
     :source source
     :target target
     :deps (split-str-on-spc (lxml::lxml-elt-attr lxml-annot :|deps|))
     :content content
     :from (or (lxml::lxml-elt-attr lxml-annot :|from|) 
	       (lxml::lxml-elt-attr lxml-annot :|cfrom|))
     :to (or
	  (lxml::lxml-elt-attr lxml-annot :|to|)
	  (lxml::lxml-elt-attr lxml-annot :|cto|))
     )))

(defun split-str-on-spc (str)
  (mapcar #'car (split-on-spc str)))

;; return list of (WORD-STRING FROM TO)
;; where FROM, TO are char offsets
(defun split-on-spc (preprocessed-string)
  (remove 
   ""
   (loop 
       with c-list = (coerce preprocessed-string 'list)
       with c-list-word
       with from = 0
       for c in c-list
       for i from 1 to (length c-list) 
       if (char= c #\Space) collect (list (coerce (nreverse c-list-word) 'string) from (1- i)) into words
       and do (setf from i)
       and do (setf c-list-word nil)
       else do (push c c-list-word)
       finally 
	 (return (append words
			 (list (list (coerce (nreverse c-list-word) 'string)
				     from i)))))
   :key #'car
   :test #'string=))

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
   ((eq (lxml::lxml-elt-name lxml) :|fs|)
    (loop for f in (lxml::lxml-elt-elts lxml :|f|)
	collect (make-saf-fv
		 :feature (make-feat (lxml::lxml-elt-attr f :|name|))
		 :value (lxml-fs-content-to-fs (first (lxml::lxml-elt-contents f))))))
   ((eq (lxml::lxml-elt-name lxml) :|binary|)
    :binary-ignored)
   ((eq (lxml::lxml-elt-name lxml) :|symbol|)
    :symbol-ignored)
   ((eq (lxml::lxml-elt-name lxml) :|numeric|)
    :numeric-ignored)
   ((eq (lxml::lxml-elt-name lxml) :|string|)
    (let ((str (first (lxml::lxml-elt-contents lxml))))
      (unless (stringp str)
	(error "string expected"))
      str))))

(defun lxml-slots-to-fs (lxml-slots)
  (loop
      for s in lxml-slots
      for feat = (lxml::lxml-elt-attr s :|name|)
      for val = (first (lxml::lxml-elt-contents s))
      for val-str = (if (stringp val)
			val
		      (error "string expected"))
      collect 
	(make-saf-fv :feature (make-feat feat)
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
     (saf-fs-feature-value2 fs (car path))))))

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
      for feat-str = (string feat)
      for val = (saf-fv-value fv)
      when (and
	    (> (length feat-str) 4)
	    (string= (subseq feat-str 0 5)
		    "gMap."))
      collect
	(cons
	 (intern (subseq feat-str 5) :keyword)
	 val)))
	 
(defun read-file-to-string (filename &key (numchars -1))
  (coerce 
   (with-open-file (ifile filename
		    :direction :input)
     (loop
	 with i = 0
	 for c = (read-char ifile nil)
	 while (and c (not (= i numchars)))
	 collect c
	 do 
	   (incf i)))
   'string))

(defun char-map-add-x (point)
  (if point
      (format nil "~a" (+ (or *char-map-add-offset* 0) (point-to-char-point point :|char|)))))

(defun saf-fs-partial-tree-2-list-partial-tree (fs)
  (if (null fs)
      nil
    (let* ((first (saf-fs-path-value '(:first) fs))
	   (rule2 (saf-fs-path-value '(:rule) first)) 
	   (str2 (saf-fs-path-value '(:str) first))
	   (rule (if (stringp rule2)
		     (intern rule2)
		   (error "string expected for saf-fs 'rule': ~a" rule2)))
	   (str (if (stringp str2)
		     (intern str2)
		   (error "string expected for saf-fs 'str': ~a" str2)))
	   (rest (saf-fs-path-value '(:rest) fs)))
      (cons (list rule str) (saf-fs-partial-tree-2-list-partial-tree rest)))))


(defun saf-num-lattice-nodes (saf)
  (length (saf-lattice-nodes (saf-lattice saf))))

(defun sort-edges-by-from (edges &key addressing)
  (sort (copy-list edges)
	'<
	:key (lambda (x)
	       (or (point-to-char-point (saf-edge-from x) addressing) 0))
	))

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
     ((eq :|char| addressing)
      (and cfrom cto
	   (subseq text cfrom cto)))
     ((eq :|xpoint| addressing)
      (error "addressing scheme 'xpoint' not implemented"))
     (t
      (error "unknown addressing scheme '~a'" addressing)))))

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
	  for item = (pop agenda)
	  for source = (car item)
	  for target = (cdr item)
	  while agenda
		;; next item
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

;; paths of exactly length len
#+:null
(defun annot-paths (annot lattice &key len)
  (cond
   ((zerop len))
   ((= 1 len)
    (list (list annot)))
   (t
    (loop
	with next-node = (saf-edge-target annot)
	for next-annot in (get-edges-source next-node :lattice lattice)
	append
	  (loop 
	      for path in (annot-paths next-annot lattice :len (1- len))
	      collect (push annot path))))))

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

;; when set, clobber rules enabled
(defvar *warning-clobber* nil)

;; attempt to read int from "clobber" field
;; if fail, 0
(defun get-clobber-level (edge)
  (let* ((l-content (saf-edge-l-content edge))
	 (clobber (saf-fs-feature-value2 l-content :|clobber|)))
    (or 
     (and (null clobber) 0) ;; in case we have null val
     (parse-integer clobber :junk-allowed t) 
     0)))
  

;; for each clobber edge, remove all edge with a lower clobber level between
;; source and target nodes according to clobebr level (see below)
(defun clobber (lattice)
  (let* ((edges (saf-lattice-edges lattice))
	 (clobber-edges
	  (loop 
	      for edge in edges
	      unless (zerop (get-clobber-level edge))
	      collect edge)))
    (loop
	for clobber-edge in clobber-edges
	for clobber-level = (get-clobber-level clobber-edge)
	for source = (saf-edge-source clobber-edge)		    
	for target = (saf-edge-target clobber-edge)
	for edges = (get-edges-x2y source target :lattice lattice)
	for clobbered-edges =
	  ;; FIXME: inefficient
	  (loop
	      for edge in edges
	      for clobber-level2 = (get-clobber-level edge)
	      when (or
		    ;; clobber level positive: clobber if abs STRICTLY greater
		    (and (> clobber-level 0)
			 (> (abs clobber-level) (abs clobber-level2)))
		    ;; clobber level negative: clobber if abs EQUAL OR greater
		    (and (< clobber-level 0)
			 (>= (abs clobber-level) (abs clobber-level2))))
	      do
		(if *warning-clobber*
		    (format t "~%;;; WARNING: edge ~a clobbered" (saf-edge-id edge)))
	      and
	      collect edge)
	do
	  (setf (saf-lattice-edges lattice)
	    (set-difference (saf-lattice-edges lattice) clobbered-edges))))
    lattice)
