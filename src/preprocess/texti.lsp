;;; Copyright (c) 2005-2006
;;;   Ben Waldron;
;;; see `licence.txt' for conditions.

(in-package :lkb)

(defvar *maf-token-id-counter* 0)

;;;
;;; BASIC TEXT <-> BASIC XML
;;;

;; map text string into well-formed XML
;; NOTE: preprocess-sentence-string is embedded here until we can fix it to 
;; handle character positions
(defun basic-text-to-basic-xml (text-str)
  ;; todo: fix preprocess-sentence-string to preserve char offsets
  ;;        and move this to basic-xml-to-maf-tokens
  (setf text-str (preprocess-sentence-string text-str))
  
  (let ((strm (make-string-output-stream)))
    ;; todo: xml declaration belongs in serialize-xml function due to 'encoding'
    (format strm "<?xml version='1.0' encoding='UTF8'?>")
    (format strm "<text>")
    (format strm "~a" (xml-escape text-str))
    (format strm "</text>")
    (get-output-stream-string strm)))

(defun basic-xml-to-basic-text (xml)
  (let* ((p-xml (net.xml.parser:parse-xml xml))
	 (text (second (car (member '|text| p-xml :key #'car))))
	 (x-from (char-offset-to-xpoint 0))
	 (x-to (char-offset-to-xpoint (length text)))
	 (xrange (cons x-from x-to)))
    (Xpoint-range xml xrange)))

;;;
;;; BASIC XML TO MAF TOKENS
;;; also MAF TOKENS <-> TCHART

(defun tchart-to-maf-tokens (&optional (tchart *tchart*))
  (tchart-to-maf tchart :wordforms nil))

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
	  "<?xml version='1.0' encoding='UTF8'?><!DOCTYPE ~a SYSTEM '~a.dtd'><~a document='~a' addressing='~a'><olac:olac xmlns:olac='http://www.language-archives.org/OLAC/1.0/' xmlns='http://purl.org/dc/elements/1.1/' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xsi:schemaLocation='http://www.language-archives.org/OLAC/1.0/ http://www.language-archives.org/OLAC/1.0/olac.xsd'><creator>LKB</creator><created>~a</created></olac:olac>"
	  (if maf "maf" "saf")
	  (if maf "maf" "saf")
	  (if maf "maf" "saf")
	  (xml-escape (2-str (or document "?")))
	  (xml-escape (2-str addressing))
	  (xml-escape (get-timestamp))))

;; preprocess-sentence-string cannot be used here until we fix it to return xpoints
(defun basic-xml-to-maf-tokens (xml)
  (setf *maf-token-id-counter* 0)
  (let* ((p-xml (net.xml.parser:parse-xml xml))
	 (text (second (car (member '|text| p-xml :key #'car))))
	 (x-from (char-offset-to-xpoint 0))
	 (x-to (char-offset-to-xpoint (length text)))
	 (xrange (cons x-from x-to))
	 (x-ranges (mapcar #'cdr (split-on-spc
				  ;(preprocess-sentence-string 
				   (Xpoint-range xml xrange)
				  ; )
				  )))
	 (strm (make-string-output-stream)))
    (format strm "~a" (maf-header))
    (mapcar #'(lambda (x) 
		(format strm "~a"
			(xrange-to-token xml 
					 (cons (char-offset-to-xpoint (first x)) 
					       (char-offset-to-xpoint (second x))))))
	    x-ranges)
    (format strm "</maf>")
    (get-output-stream-string strm)))

(defun xrange-to-token (xml xrange)
  ;; todo: escape attribute val strings
  (format nil "<token id='~a' from='~a' to='~a' value='~a' source='...' target='...'/>"
	  (next-maf-token-id)
	  (car xrange)
	  (cdr xrange)
	  (Xpoint-range xml xrange)))

(defun next-maf-token-id nil
  (format nil "t~a" (incf *maf-token-id-counter*)))
 
(defun char-offset-to-xpoint (i)
  (unless (integerp i)
    (error "char offset must be integer"))
  (format nil "/1/1.~a" i))

(defun xpoint-to-char-offset (xp)
  (unless (stringp xp)
    (error "expected Xpoint as string"))
  (unless (and (>= (length xp) 5)
	       (string= (subseq xp 0 5) "/1/1."))
    ;;temporary hack
    (return-from xpoint-to-char-offset -1)
    (error "unhandled Xpoint ~a (work in progress)" xp))
  (read-from-string (subseq xp 5)))

;; assume for now XML root element contains only CDATA
(defun Xpoint-range (xml xrange)
  (let ((x-from (car xrange))
	(x-to (cdr xrange)))
    (let* ((p-xml (net.xml.parser:parse-xml xml))
	   (text (second (car (member '|text| p-xml :key #'car))))
	   (c-from (xpoint-to-char-offset x-from))
	   (c-to (xpoint-to-char-offset x-to))
	   )
      (unless (and (>= c-from 0)
		   (<= c-to (length text)))
	(error "Xpoint out of range"))
      (subseq text c-from c-to))))

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

;;;
;;; MAF TOKENS TO MAF WORDFORMS
;;;

;;;
;;; tchart -> maf-wordforms.xml
;;;

(defun tchart-to-maf-wordforms (&optional (tchart *tchart*))
  (tchart-to-maf tchart :wordforms t))

;;;
;;; MAF to tchart mapping
;;;

(defun leaf-edges-from (leaf-edges)
  (unless leaf-edges
    (error "leaf-edges is null"))
  (apply #'min (mapcar #'edge-from leaf-edges)))

(defun leaf-edges-to (leaf-edges)
  (unless leaf-edges
    (error "leaf-edges is null"))
  (apply #'max (mapcar #'edge-to leaf-edges)))

(defun leaf-edges-xfrom (leaf-edges)
  (unless leaf-edges
    (error "leaf-edges is null"))
  (loop
      with xmin = (token-edge-xfrom (pop leaf-edges))
      for tedge in leaf-edges
      for xfrom = (token-edge-xfrom tedge)
      when (x< xfrom xmin)
      do (setf xmin xfrom)
      finally (return xmin)))

(defun leaf-edges-xto (leaf-edges)
  (unless leaf-edges
    (error "leaf-edges is null"))
  (loop
      with xmax = (token-edge-xto (pop leaf-edges))
      for tedge in leaf-edges
      for xto = (token-edge-xto tedge)
      do (print xto)
      when (x> xto xmax)
      do (setf xmax xto)
      finally (return xmax)))

;; assume xpoint order is string order for now
(defun x< (x y)
  (string< x y))

(defun x> (x y)
  (string> x y))

(defun x= (x y)
  (string= x y))



(defun split-str-on-spc (str)
  (mapcar #'car (split-on-spc str)))

(defun check-state-declared (id states)
    (unless
	(member id states 
		:test #'string=
		:key #'(lambda (x)
			 (lxml-elt-attr x "id")))
      (error "state ~a referenced but not explicitly declared in ~a"
	     id states)))
        
;;;
;;; tchart to MAF mapping
;;;

(defun tchart-to-maf (&optional (tchart *tchart*) &key (wordforms t))
  (let* ((strm (make-string-output-stream))
	 (tedges (get-tedges tchart))
	 (medges (get-medges tchart))
	 )
    ;(format strm "<?xml version='1.0' encoding='UTF8'?>")
    ;(format strm "<!DOCTYPE maf SYSTEM 'maf.dtd' [<!ENTITY text SYSTEM 'text.xml'>]>")
    (format strm "~a" (maf-header))

;    ;; xml token elements
;    (mapcar #'(lambda (x)
;		(format strm "~a"
;			(tedge-to-token-xml x)))
;	    tedges)
    (if wordforms (format strm "~a" (fsm-xml tedges medges)))
    
    (format strm "</maf>")
    (get-output-stream-string strm)))

(defun fsm-xml (tedges medges)
  (let* ((strm (make-string-output-stream))
	 (v-min (loop for x in tedges minimize (edge-from x)))
	 (v-max (1+ (loop for x in tedges maximize (edge-from x))))
	 )
    (format strm "<fsm init='v~a' final='v~a'>" v-min v-max)
    
    (loop
	for i from v-min to v-max
	do (format strm "<state id='v~a'/>" i))
    
    (loop
	for tedge in tedges
	do (format strm "~a" (tedge-to-token-xml tedge)))
    
    (loop
	for medge in medges
	do (format strm "~a" (medge-to-wordform-xml medge)))
    
    (format strm "</fsm>")
    (get-output-stream-string strm)))

(defun medge-to-wordform-xml (medge)
  (with-slots (from to string stem partial-tree) medge
    (concatenate 'string
      ;(format nil "<transition source='v~a' target='v~a'>" from to)
      (format nil "<wordForm form='~a' tag='~a' daughters='~a' source='v~a' target='v~a'>" 
	      string 
	      (if (caar partial-tree)
		  (cl-ppcre:regex-replace "_INFL_RULE$" (string (caar partial-tree)) "")
		"")
	      (edge-to-tokens-id-str medge)
	       from to)
      (format nil "<fs>")
      (format nil "~a" (stem-to-fs stem))
      (if partial-tree
	  (format nil "~a" (partial-tree-to-fs partial-tree)))
      (format nil "</fs>")
      (format nil "</wordForm>")
      ;(format nil "</transition>")
      )))

;; store as lisp list text
(defun partial-tree-to-fs (p-tree)
  (concatenate 'string
    (format nil "<f name='partial-tree'>")
    (xml-escape (format nil "~S" p-tree))
    (format nil "</f>")))
  
(defun stem-to-fs (stem)
  (concatenate 'string
    (format nil "<f name='stem'>")
    (xml-escape (format nil "~a" stem))
    (format nil "</f>")))
  
(defun edge-to-leaf-token-edges (edge)
  (cond
   ((token-edge-p edge)
    (edge-id edge))
   (t
    (loop
	with children = (or (edge-children edge)
			    (edge-tchildren edge)
			    (error "children or tchildren expected in edge ~a" edge))
	with tedges = (extract-descendent-tedges children)
	for tedge in tedges 
	collect tedge))))

(defun edge-to-tokens-id-str (edge)
  (concatenate-strings
   (cdr
    (loop
	for tedge in (edge-to-leaf-token-edges edge)
	collect " "
	collect (format nil "t~a" (edge-id tedge))))))

(defun extract-descendent-tedges (children)
  (loop
      for child in children
      append 
	(cond 
	 ((edge-children child)
	  (extract-descendent-tedges (edge-children child)))
	 ((edge-tchildren child)
	  (extract-descendent-tedges (edge-tchildren child)))
	 (t 
	  (if (token-edge-p child)
	    "error unexpected non-token-edge leaf edge ~a" child)
	  (list child)))))

;; using cfrom/cto in place of xfrom/xto
(defun tedge-to-token-xml (tedge)
  (with-slots (id from to string cfrom cto) tedge
  (format nil "<token id='~a' from='~a' to='~a' value='~a' source='v~a' target='v~a'/>"
	  (xml-escape (format nil "t~a" id))
	  (xml-escape (format nil ".~a" (or cfrom "?")))
	  (xml-escape (format nil ".~a" (or cto "?")))
;	  (xml-escape (or xfrom "?")) 
;	  (xml-escape (or xto "?"))
	  (xml-escape string)
	  (xml-escape (or (2-str from) "?")) 
	  (xml-escape (or (2-str to) "?"))	  
	  )))

(defun get-edges (&optional (tchart *tchart*))
  (loop
      for i from 1 to (1- *chart-limit*)
      for ccs-incident = (aref tchart  i 0)
      append
	(loop
	    for cc in ccs-incident
	    for edge = (chart-configuration-edge cc)
	    when (edge-p edge)
	    collect edge)))

(defun get-tedges (&optional (tchart *tchart*))
  (loop
      for i from 1 to (1- *chart-limit*)
      for ccs-incident = (aref tchart  i 0)
      append
	(loop
	    for cc in ccs-incident
	    for edge = (chart-configuration-edge cc)
	    when (token-edge-p edge)
	    collect edge)))

(defun get-medges (&optional (tchart *tchart*))
  (loop
      for i from 1 to (1- *chart-limit*)
      for ccs-incident = (aref tchart  i 0)
      append
	(loop
	    for cc in ccs-incident
	    for edge = (chart-configuration-edge cc)
	    when (morpho-stem-edge-p edge)
	    collect edge)))

