;;; Copyright (c) 2005
;;;   Ben Waldron;
;;; see `licence.txt' for conditions.

(in-package :lkb)

(defvar *token-id* 0)

;;;

(defun pretty-print-xml (xml)
  (coerce
   (loop
       for c across xml
       if (char= #\< c)
       append (list #\Newline)
       append (list c))
   'string))
  

;;;
;;; BASIC TEXT <-> BASIC XML
;;;

;; map text string into well-formed XML
(defun basic-text-to-basic-xml (text-str)
  ;; todo: fix preprocess-sentence-string to preserve char offsets
  ;;        and move this to basic-xml-to-maf-tokens
  (setf text-str (preprocess-sentence-string text-str))
  
  (let ((strm (make-string-output-stream)))
    ;; todo: xml declaration belongs in serialize-xml function due to 'encoding'
    (format strm "<?xml version='1.0' encoding='UTF8'?>")
    (format strm "<text>")
    (format strm "~a" (wrap-cdata text-str))
    (format strm "</text>")
    (get-output-stream-string strm)))

(defun basic-xml-to-basic-text (xml)
  (let* ((p-xml (net.xml.parser:parse-xml xml))
	 (text (second (car (member '|text| p-xml :key #'car))))
	 (x-from (char-offset-to-xpointer 0))
	 (x-to (char-offset-to-xpointer (length text)))
	 (xrange (cons x-from x-to)))
    (XPointer-range xml xrange)))
  
;; return CDATA-wrapped text
(defun wrap-cdata (str)
  (concatenate-strings
   (cdr 
    (loop 
	with subs = (cons 0 (append (ppcre:all-matches "]]>" str)
				    (list (length str))))
	while subs
	do (print subs)
	collect "]]&gt;"
	collect "<![CDATA["
	collect (subseq str (pop subs) (pop subs))
	collect "]]>"))))
  
(defun concatenate-strings (x)
  (apply #'concatenate
	 (cons 'string x)))

;;;
;;; BASIC XML TO MAF TOKENS
;;; also MAF TOKENS <-> TCHART

(defun tchart-to-maf-tokens (tchart)
  (tchart-to-maf tchart))

(defun basic-xml-to-maf-tokens (xml)
  (setf *token-id* 0)
  (let* ((p-xml (net.xml.parser:parse-xml xml))
	 (text (second (car (member '|text| p-xml :key #'car))))
	 (x-from (char-offset-to-xpointer 0))
	 (x-to (char-offset-to-xpointer (length text)))
	 (xrange (cons x-from x-to))
	 (x-ranges (mapcar #'cdr (split-on-spc
				  ;(preprocess-sentence-string 
				   (XPointer-range xml xrange)
				  ; )
				  )))
	 (strm (make-string-output-stream)))
    (format strm "<?xml version='1.0' encoding='UTF8'?>")
    (format strm "<!DOCTYPE maf SYSTEM 'maf.dtd' [<!ENTITY text SYSTEM 'text.xml'>]>")
    (format strm "<maf addressing='XPointer' creator='lkb-maf-tokens' date='2005-07-11' language='en.US'>")
    (mapcar #'(lambda (x) 
		(format strm "~a"
			(xrange-to-token xml 
					 (cons(char-offset-to-xpointer (first x)) 
					      (char-offset-to-xpointer (second x))))))
	    x-ranges)
    (format strm "</maf>")
    (get-output-stream-string strm)))

(defun xrange-to-token (xml xrange)
  ;; todo: escape attribute val strings
  (format nil "<token id='~a' from='~a' to='~a' value='~a'/>"
	  (next-token-id)
	  (car xrange)
	  (cdr xrange)
	  (XPointer-range xml xrange)))

(defun next-token-id nil
  (format nil "t~a" (incf *token-id*)))
 
(defun char-offset-to-xpointer (i)
  (unless (integerp i)
    (error "char offset must be integer"))
  (format nil "/1/~a" i))

;; assume for now XML root element contains only CDATA
(defun XPointer-range (xml xrange)
  (let ((x-from (car xrange))
	(x-to (cdr xrange)))
    (unless (and (stringp x-from)
		 (stringp x-to)
		 (string= (subseq x-from 0 3) "/1/")
		 (string= (subseq x-to 0 3 )"/1/"))
      (error "unhandled XPointers ~a ~a (work in progress" x-from x-to))
    (let* ((p-xml (net.xml.parser:parse-xml xml))
	   (text (second (car (member '|text| p-xml :key #'car))))
	   (c-from (read-from-string (subseq x-from 3)))
	   (c-to (read-from-string (subseq x-to 3)))
	   )
      (unless (and (>= c-from 0)
		   (<= c-to (length text)))
	(error "XPointer out of range"))
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
       if (char= c #\Space) collect (list (coerce (nreverse c-list-word) 'string) from i) into words
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
;;; *tchart* -> maf-wordforms.xml
;;;

(defun tchart-to-maf-wordforms (tchart)
  (tchart-to-maf tchart :wordforms t))

#+:null
(defun maf-to-tchart (xml)
  
  )

(defun tchart-to-maf (tchart &key wordforms)
  (let* ((strm (make-string-output-stream))
	 (tedges (get-tedges tchart))
	 (medges (get-medges tchart))
	 )
    (format strm "<?xml version='1.0' encoding='UTF8'?>")
    (format strm "<!DOCTYPE maf SYSTEM 'maf.dtd' [<!ENTITY text SYSTEM 'text.xml'>]>")
    (format strm "<maf addressing='XPointer' creator='lkb-maf-wordforms' date='2005-07-11' language='en.US'>")
    ;; xml token elements
    (mapcar #'(lambda (x)
		(format strm "~a"
			(tedge-to-token-xml x)))
	    tedges)
    (if wordforms (format strm "~a" (fsm-xml tedges medges)))
    
    (format strm "</maf>")
    (get-output-stream-string strm)))

(defun fsm-xml (tedges medges)
  (let* ((strm (make-string-output-stream))
	 (v-min (loop for x in tedges minimize (edge-from x)))
	 (v-max (loop for x in tedges maximize (edge-from x)))
	 )
    (format strm "<fsm init='v~a' final='v~a'>" v-min v-max)
    
    (loop
	for i from v-min to v-max
	do (format strm "<state id='v~a'/>" i))
    
    (loop
      for x in medges
	do (format strm "~a" (medge-to-transition-xml x)))
    
    (format strm "</fsm>")
    (get-output-stream-string strm)
    ))

(defun medge-to-transition-xml (medge)
  (with-slots (from to string stem) medge
    (concatenate 'string
      (format nil "<transition source='v~a' target='v~a'>" from to)
      (format nil "<wordForm form='~a' tag='' tokens='~a'>" string (edge-to-tokens medge))
      (format nil "<fs>")
      (format nil "<f name='orth'>")
      (format nil "<string>~a</string>" stem)
      (format nil "</f>")
      (format nil "</fs>")
      (format nil "</wordForm>")
      (format nil "</transition>"))))

(defun edge-to-tokens (edge)
  (cond
   ((token-edge-p edge)
    (format nil "t~a" (edge-id edge)))
   (t
    (concatenate-strings
     (cdr 
      (loop
	  with children = (or (edge-children edge)
			      (edge-tchildren edge)
			      (error "children or tchildren expected in edge ~a" edge))
	  with tedges = (extract-descendent-tedges children)
	  for tedge in tedges 
	  for id = (edge-id tedge)
	  collect " "
	  collect (format nil "t~a" id))
      )))))
	  
	  

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

(defun tedge-to-token-xml (tedge)
  ;; todo: escape attribute val strings
  (with-slots (id from to string) tedge
  (format nil "<token id='t~a' from='~a' to='~a' value='~a'/>"
	  id 
	  (format nil "?~a?" from) 
	  (format nil "?~a?" to)
	  string)))

(defun get-tedges (tchart)
  (loop
      for i from 1 to *tchart-max*
      for ccs-incident = (aref tchart  i 0)
      append
	(loop
	    for cc in ccs-incident
	    for edge = (chart-configuration-edge cc)
	    when (token-edge-p edge)
	    collect edge)))

(defun get-medges (tchart)
  (loop
      for i from 1 to *tchart-max*
      for ccs-incident = (aref tchart  i 0)
      append
	(loop
	    for cc in ccs-incident
	    for edge = (chart-configuration-edge cc)
	    when (morpho-stem-edge-p edge)
	    collect edge)))