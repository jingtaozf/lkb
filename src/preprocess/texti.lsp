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
#+:null
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

#+:null
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

;; preprocess-sentence-string cannot be used here until we fix it to return xpoints
#+:null
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
    (format strm "~a" (preprocessor::maf-header)) ;; fix_me
    (mapcar #'(lambda (x) 
		(format strm "~a"
			(xrange-to-token xml 
					 (cons (char-offset-to-xpoint (first x)) 
					       (char-offset-to-xpoint (second x))))))
	    x-ranges)
    (format strm "</maf>")
    (get-output-stream-string strm)))

#+:null
(defun xrange-to-token (xml xrange)
  ;; todo: escape attribute val strings
  (format nil "<token id='~a' from='~a' to='~a' value='~a' source='...' target='...'/>"
	  (next-maf-token-id)
	  (car xrange)
	  (cdr xrange)
	  (Xpoint-range xml xrange)))

#+:null
(defun next-maf-token-id nil
  (format nil "t~a" (incf *maf-token-id-counter*)))
 
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
;  (unless leaf-edges
;    (error "leaf-edges is null"))
  (apply #'min (mapcar #'edge-from leaf-edges)))

(defun leaf-edges-to (leaf-edges)
;  (unless leaf-edges
;    (error "leaf-edges is null"))
  (apply #'max (mapcar #'edge-to leaf-edges)))

#+:null
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

#+:null
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

#+:null
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

(defun tchart-to-saf (&optional (tchart *tchart*) &key (wordforms t))
  (tchart-to-maf tchart :wordforms wordforms :saf t))

(defun tchart-to-maf (&optional (tchart *tchart*) &key (wordforms t) saf)
  (initialize-smaf-id-to-edge-id-from-tchart)
  (let* ((strm (make-string-output-stream))
	 (tedges (get-tedges tchart))
	 (medges (if wordforms
		     (get-medges tchart))))
    (if saf
	(format strm "~a" (preprocessor::saf-header))
      (format strm "~a" (preprocessor::maf-header)))
    (format strm "~a" (fsm-xml tedges medges :saf saf))
    (if saf
	(format strm "</saf>")
      (format strm "</maf>"))
    (get-output-stream-string strm)))

(defun fsm-xml (tedges medges &key saf)
  (let* ((strm (make-string-output-stream))
	 (v-min (loop for x in tedges minimize (edge-from x)))
	 (v-max (1+ (loop for x in tedges maximize (edge-from x)))))
    (format strm "<fsm init='v~a' final='v~a'>" v-min v-max)
    ;; states
    (loop
	for i from v-min to v-max
	do (format strm "<state id='v~a'/>" i))
    ;; token edges
    (loop
	for tedge in tedges
	do (format strm "~a" (tedge-to-token-xml tedge :saf saf)))
    ;; wordform edges
    (loop
	for medge in medges
	do (format strm "~a" (medge-to-wordform-xml medge :saf saf)))
    (format strm "</fsm>")
    (get-output-stream-string strm)))

(defun medge-to-wordform-xml (medge &key saf)
  (with-slots (from to string stem partial-tree id) medge
    (cond
     (saf
      (concatenate 'string
	(format nil "<annot type='wordForm' id='~a' deps='~a' source='v~a' target='v~a'>" 
		;(if (caar partial-tree)
		 ;   (cl-ppcre:regex-replace "_INFL_RULE$" (string (caar partial-tree)) "")
		  ;"")
		(edge-id-to-smaf-id id)
		(edge-to-tokens-id-str medge)
	       from to)
	(format nil "<fs>")
	(format nil "~a" (stem-to-fs stem))
	(if partial-tree
	    ;(format nil "~a" (partial-tree-to-fs-lazy partial-tree)))
	    (format nil "~a" (partial-tree-to-fs partial-tree)))
	(format nil "</fs>")
	(format nil "</annot>")))
     (t
      (concatenate 'string
	(format nil "<wordForm tokens='~a' source='v~a' target='v~a'>" 
		(edge-to-tokens-id-str medge)
	       from to)
	(format nil "<fs>")
	(format nil "~a" (stem-to-fs stem))
	(if partial-tree
	    (format nil "~a" (partial-tree-to-fs partial-tree)))
	    ;(format nil "~a" (partial-tree-to-fs-lazy partial-tree)))
	(format nil "</fs>")
	(format nil "</wordForm>"))
      ))))

;; store as lisp list text
(defun partial-tree-to-fs (p-tree)
  (format nil "<f name='partial-tree'>~a</f>"
	  (partial-tree-to-fs2 p-tree)))

(defun partial-tree-to-fs2 (p-tree)
  (if (null p-tree)
      ""
    (format nil "<fs><f name='first'>~a</f><f name='rest'>~a</f></fs>"
	    (partial-tree-elt-to-fs (car p-tree))
	    (partial-tree-to-fs2 (cdr p-tree)))))

(defun partial-tree-elt-to-fs (p-tree-elt)
  (format nil "<fs><f name='rule'>~a</f><f name='str'>~a</f></fs>"
	  (xml-escape (format nil "~a" (first p-tree-elt)))
	  (xml-escape (format nil "~a" (second p-tree-elt)))))

(defun partial-tree-to-fs-lazy (p-tree)
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
	for id = (edge-id tedge)
	collect " "
	collect (edge-id-to-smaf-id id)))))

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
(defun tedge-to-token-xml (tedge &key saf)
  (with-slots (id from to string cfrom cto) tedge
    (cond
     (saf
      (format nil "<annot type='token' id='~a' from='~a' to='~a' value='~a' source='v~a' target='v~a'/>"
	      (xml-escape (format nil "t~a" id))
	      (xml-escape (2-str (or cfrom "?")))
	      (xml-escape (2-str (or cto "?")))
	      (xml-escape string)
	      (xml-escape (or (2-str from) "?")) 
	      (xml-escape (or (2-str to) "?")))      
      )
     (t
      (format nil "<token id='~a' from='~a' to='~a' value='~a' source='v~a' target='v~a'/>"
	      (xml-escape (format nil "t~a" id))
	      (xml-escape (2-str (or cfrom "?")))
	      (xml-escape (2-str (or cto "?")))
	      (xml-escape string)
	      (xml-escape (or (2-str from) "?")) 
	      (xml-escape (or (2-str to) "?")))
      ))))

;; assume xpoint order is string order for now
(defun x< (x y)
  (string< x y))

(defun x> (x y)
  (string> x y))

(defun x= (x y)
  (string= x y))

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
(defun xpoint-range (xml xrange)
  (let ((x-from (car xrange))
	(x-to (cdr xrange)))
    (let* ((p-xml (xml:parse-xml xml))
	   (text (second (car (member '|text| p-xml :key #'car))))
	   (c-from (xpoint-to-char-offset x-from))
	   (c-to (xpoint-to-char-offset x-to)))
      (unless (and (>= c-from 0)
		   (<= c-to (length text)))
	(error "Xpoint out of range"))
      (subseq text c-from c-to))))

