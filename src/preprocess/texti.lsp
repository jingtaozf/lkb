;;; Copyright (c) 2005
;;;   Ben Waldron;
;;; see `licence.txt' for conditions.

(in-package :lkb)

(defvar *token-id* 0)
(defvar *token-vertex* 0)

;;;

;; input xml must have NO superfluous space
(defun pretty-print-xml (xml)
  (coerce
   (loop
       with i = -1
       with last
       with last2
       for c across xml
       if (and (char= #\< last) (char= #\/ c)) do (decf i)
       if (and (char= #\< last) (or (null last2) (char= #\> last2)))
       append (list #\Newline) into x and
       append (loop for n from 1 to i
		  collect #\Space) into x
       if (and (char= #\< last) (char= #\/ c)) do (decf i)
       if (and (char= #\< last) (char= #\! c)) do (decf i)
       if (and (char= #\/ last) (char= #\> c)) do (decf i)
       if (char= #\< c) do (incf i)
       if last append (list last) into x
       do (setf last2 last) (setf last c)
       finally (return (append x (list c))))
   'string))
  

;;;
;;; BASIC TEXT <-> BASIC XML
;;;

;; map text string into well-formed XML
;; NOTE: preprocess-sentence-string is embedded here until we can fix it to handle character positions
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
	 (x-from (char-offset-to-xpointer 0))
	 (x-to (char-offset-to-xpointer (length text)))
	 (xrange (cons x-from x-to)))
    (XPointer-range xml xrange)))

;; escape string for use as XML text
(defun xml-escape (str)
  (coerce 
   (loop
       for c across str
       if (char= #\" c) append '(#\& #\q #\u #\o #\t #\;)
       else if (char= #\' c) append '(#\& #\a #\p #\o #\s #\;)
       else if (char= #\& c) append '(#\& #\a #\m #\p #\;)
       else if (char= #\< c) append '(#\& #\l #\t #\;)
       else if (char= #\> c) append '(#\& #\g #\t #\;)
       else append (list c))
   'string))

;; return CDATA-wrapped text
(defun wrap-cdata (str)
  (concatenate-strings
   (cdr 
    (loop 
	with subs = (cons 0 (append (ppcre:all-matches "]]>" str)
				    (list (length str))))
	while subs
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


;; CLIM display routines
(defun print-maf-tokens nil
  (let ((frame (clim:make-application-frame 'xml-maf-tokens)))
    (clim:run-frame-top-level frame)))


(defun print-maf-wordforms nil
  (let ((frame (clim:make-application-frame 'xml-maf-tokens)))
    (clim:run-frame-top-level frame)))


(defun disp-xml-maf-tokens (mframe stream &key max-width max-height)
  (declare (ignore mframe max-width max-height))
  (if *tchart*
      (clim:with-text-style (stream (lkb-parse-tree-font))
	(format stream "~a" (pretty-print-xml (tchart-to-maf-tokens *tchart*))))
    (format stream "No tchart (please parse something first).")))

(define-lkb-frame xml-maf-tokens
    ()
  :display-function 'disp-xml-maf-tokens 
  :width 400 
  :height 400)

(define-lkb-frame xml-maf-wordforms
    ()
  :display-function 'disp-xml-maf-tokens 
  :width 400 
  :height 400)

(defun disp-xml-maf-wordforms (mframe stream &key max-width max-height)
  (declare (ignore mframe max-width max-height))
  (if *tchart*
      (clim:with-text-style (stream (lkb-parse-tree-font))
	(format stream "~a" (pretty-print-xml (tchart-to-maf-wordforms *tchart*))))
    (format stream "No tchart (please parse something first).")))

;; end of CLIM display routines

;; TODO: LUI display???

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
  (format nil "/1/1/~a" i))

(defun xpointer-to-char-offset (xp)
  (unless (stringp xp)
    (error "expected XPointer as string"))
  (unless (and (>= (length xp) 5)
	       (string= (subseq xp 0 5) "/1/1/"))
    ;;temporary hack
    (return-from xpointer-to-char-offset -1)
    (error "unhandled XPointer ~a (work in progress)" xp))
  (read-from-string (subseq xp 5)))

;; assume for now XML root element contains only CDATA
(defun XPointer-range (xml xrange)
  (let ((x-from (car xrange))
	(x-to (cdr xrange)))
    (let* ((p-xml (net.xml.parser:parse-xml xml))
	   (text (second (car (member '|text| p-xml :key #'car))))
	   (c-from (xpointer-to-char-offset x-from))
	   (c-to (xpointer-to-char-offset x-to))
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
;;; tchart -> maf-wordforms.xml
;;;

(defun tchart-to-maf-wordforms (tchart)
  (tchart-to-maf tchart :wordforms t))

;;;
;;; MAF to tchart mapping
;;;

(defun maf-to-tchart (xml)
  (setf *tchart* (make-tchart))
  (setf *tchart-max* 0)
  
  (let* ((lxml (net.xml.parser:parse-xml xml))
	 (lxml-e (pop lxml)))
    ;; check for <?xml version="1.0" ...>
    (unless (and (eq :xml (lxml-pi-name lxml-e))
		 (string= "1.0" (lxml-pi-attr lxml-e "version" :keyword t)))
      (error "expected XML version 1.0: got ~a" lxml-e))
    ;; check for <!DOCTYPE maf ...>
    (setf lxml-e (pop lxml))
    (unless (and (eq :doctype (lxml-pi-name lxml-e))
		 (eq (intern "maf" :keyword) (second lxml-e)))
      (error "expected DOCTYPE maf: got ~a" lxml-e))
    ;; now process maf element
   (setf lxml-e (pop lxml))
   
   (setf *token-vertex* 0)
   
   (maf-lxml-to-tchart lxml-e)
   )
  *tchart*)

(defun maf-lxml-to-tchart (lxml)
  (unless (eq (intern "maf")
	      (lxml-elt-name lxml))
    (error "expected lxml root element maf: got ~a" lxml))
  (unless (string= "XPointer"
		   (lxml-elt-attr lxml "addressing"))
    (error "Unhandled addressing attribute in ~a" lxml))
  ;; date ignored
  ;; language ignored
  (let* ((contents (cdr lxml))
	 (tokens (loop
		    for x in contents
		    when (eq (intern "token")
			     (lxml-elt-name x))
		     collect x))
	 (tedges (mapcar #'token-lxml-to-tedge tokens))
	 (fsms (loop
		   for x in contents
		   when (eq (intern "fsm")
			    (lxml-elt-name x))
		   collect x))
	 )
    (unless (= 1 (length fsms))
      (error "Multiple fsm elements are not handled at present"))
    ;; create tedges
    (place-tedges-in-tchart tedges)
    )
  )

(defun place-tedges-in-tchart (tedges)
  (loop
      for tedge in tedges
      for from = (edge-from tedge)
      for to = (edge-to tedge)
      for cc = (make-chart-configuration :begin from
					 :end to
					 :edge tedge)
      do
	(setf (aref *tchart* to 0) (push cc (aref *tchart* to 0)))
	(setf (aref *tchart* from 1) (push cc (aref *tchart* to 1)))
	(when (> to *tchart-max*)
	  (setf *tchart-max* to))))
	
	  
(defun fsm-lxml-to-medges (lxml)
  (unless (eq (intern "fsm") (lxml-elt-name lxml))
    (error "fsm lxml element expected: got ~a" lxml))
  (let ((init (lxml-elt-attr lxml "init"))
	(final (lxml-elt-attr lxml "final"))
	(states (loop
		    for x in (cdr lxml)
		    when (eq (intern "state")
			     (lxml-elt-name x))
		    collect x))
	(transitions (loop
		    for x in (cdr lxml)
		    when (eq (intern "transition")
			     (lxml-elt-name x))
			 collect x)))
    (check-state-declared init states)
    (check-state-declared final states)
    (mapcar #'(lambda (x)
		(check-state-declared
		 (lxml-elt-attr x "source") states))
	    transitions)
    (mapcar #'(lambda (x)
		(check-state-declared
		 (lxml-elt-attr x "target") states))
	    transitions)
    
    transitions))

(defun check-state-declared (id states)
    (unless
	(member id states 
		:test #'string=
		:key #'(lambda (x)
			 (lxml-elt-attr x "id")))
      (error "state ~a referenced but not explicitly declared in ~a"
	     id states)))
  

;; we assume for now that tokens are presented in edge order
(defun token-lxml-to-tedge (lxml)
  (unless (eq (intern "token") (lxml-elt-name lxml))
    (error "token lxml element expected: got ~a" lxml))
  (let* ((id (lxml-elt-attr lxml "id"))
	 (from (lxml-elt-attr lxml "from"))
	 (to (lxml-elt-attr lxml "to"))
	 (value (lxml-elt-attr lxml "value"))
	 
	 (e-id (token-lxml-id-to-token-edge-id id))
	 (e-to (incf *token-vertex*))
	 (e-from (1- e-to)))
    (make-token-edge 
     :id e-id
     :from e-from
     :to e-to
     :string value
     :xfrom (xpointer-to-char-offset from)
     :xto (xpointer-to-char-offset to)
     :word (string-upcase value)
     :leaves (list value))))

(defun token-lxml-id-to-token-edge-id (lxml-id)
  (let ((val (read-from-string (subseq lxml-id 1))))
    (unless (integerp val)
      (error "token edge id could not be extracted from token maf id ~a"
	     val))
    val))  

;;

(defun lxml-elt-p (x)
  (listp x))

(defun lxml-elt-name (lxml-elt)
  (unless (lxml-elt-p lxml-elt)
    (error "lxml element expected: got ~a" lxml-elt))
  (let ((car (car lxml-elt)))
    (typecase car
      (symbol car)
      (list (car car))
     (t (error "expected symbol or list as car of lxml element: got ~a" car)))))      

(defun lxml-elt-attr (lxml-elt attrib-str &key keyword)
  (unless (lxml-elt-p lxml-elt)
    (error "lxml element expected: got ~a" lxml-elt))
  (unless (stringp attrib-str)
    (error "string name of lxml attribute expected: got ~a" attrib-str))
  (let ((attrib (if keyword
		    (intern attrib-str :keyword)
		  (intern attrib-str)))
	(car (car lxml-elt)))
    (typecase car
     (symbol nil)
     (list (second (member attrib (cdr car))))
     (t (error "expected symbol or list as car of lxml element: got ~a" car)))))
       
(defun lxml-pi-name (lxml-pi)
  (unless (lxml-pi-p lxml-pi)
    (error "lxml element expected: got ~a" lxml-pi))
  (let ((car (car lxml-pi)))
    (typecase car
      (symbol car)
      (list (car car))
     (t (error "expected symbol or list as car of lxml element: got ~a" car)))))      

(defun lxml-pi-p (x)
  (listp x))

(defun lxml-pi-attr (lxml-pi attrib-str &key keyword)
  (unless (lxml-pi-p lxml-pi)
    (error "lxml element expected: got ~a" lxml-pi))
  (unless (stringp attrib-str)
    (error "string name of lxml attribute expected: got ~a" attrib-str))
  (let ((attrib (if keyword
		    (intern attrib-str :keyword)
		  (intern attrib-str))))
    (second (member attrib (cdr lxml-pi)))))
        
;;;
;;; tchart to MAF mapping
;;;

(defun tchart-to-maf (tchart &key (wordforms t))
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
	 (v-max (1+ (loop for x in tedges maximize (edge-from x))))
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
  (with-slots (from to string stem partial-tree) medge
    (concatenate 'string
      (format nil "<transition source='v~a' target='v~a'>" from to)
      (format nil "<wordForm form='~a' tag='~a' tokens='~a'>" 
	      string (cadr partial-tree) (edge-to-tokens medge))
      (format nil "<fs>")
      (format nil "<f name='orth'>")
      (format nil "<string>~a</string>" stem)
      (format nil "</f>")
      (if partial-tree
	  (format nil "~a" (partial-tree-to-fs partial-tree)))
      (format nil "</fs>")
      (format nil "</wordForm>")
      (format nil "</transition>"))))

;; only handles simplest case
(defun partial-tree-to-fs (p-tree)
  (unless 
      (and (= 1 (length p-tree))
	   (= 2 (length (car p-tree)))
	   (symbolp (first (car p-tree)))
	   (stringp (second (car p-tree))))
    (error "at present we only handle partial-trees of form ((RULE \"ORTH\"))"))
    (concatenate 'string
      (format nil "<f name='partial-tree'>")
      (format nil "<f name='rule'>")
      (format nil "<string>~a</string>" (first (car p-tree)))
      (format nil "</f>")
      (format nil "<f name='orth'>")
      (format nil "<string>~a</string>" (second (car p-tree)))
      (format nil "</f>")
      (format nil "</f>")
      ))
  

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
      for i from 1 to (1- *chart-limit*)
      for ccs-incident = (aref tchart  i 0)
      append
	(loop
	    for cc in ccs-incident
	    for edge = (chart-configuration-edge cc)
	    when (token-edge-p edge)
	    collect edge)))

(defun get-medges (tchart)
  (loop
      for i from 1 to (1- *chart-limit*)
      for ccs-incident = (aref tchart  i 0)
      append
	(loop
	    for cc in ccs-incident
	    for edge = (chart-configuration-edge cc)
	    when (morpho-stem-edge-p edge)
	    collect edge)))

;;;

(defun parse-from-maf (maf &optional 
			   (show-parse-p *show-parse-p*) 
			   (first-only-p *first-only-p*))
  
  ;(check-morph-options bracketed-input)
  (let* ((*active-parsing-p* (if *bracketing-p* nil *active-parsing-p*))
         (first-only-p (if (and first-only-p 
                                (null *active-parsing-p*)
                                (greater-than-binary-p))
                         (format 
                          t 
                          "~&Passive best-first mode only available for ~
                           unary and binary rules.~%~
                           Disabling best-first mode: setting ~
                           *first-only-p* to `nil'.~%")
                         first-only-p)))
    ;; eg. user-input -> ("the" "dog" "barks")
    ;(multiple-value-bind (user-input brackets-list)
    ;    (if *bracketing-p*
    ;      (initialise-bracket-list bracketed-input)
    ;      (values bracketed-input nil))

    ;(when (> (length user-input) *chart-limit*)
    ;    (error "~%Sentence `~a' too long - ~A words maximum ~
    ;            (see documentation for *chart-limit*)" 
					;           user-input *chart-limit*))
    (clear-chart)
    (maf-to-tchart maf)
      (let* (;(*brackets-list* brackets-list)
	     (len-tokens (apply #'max 
				(mapcar #'token-edge-to 
					(get-tedges *tchart*))))
	    (*executed-tasks* 0) (*successful-tasks* 0)
            (*contemplated-tasks* 0) (*filtered-tasks* 0)
            (*parser-rules* (get-matching-rules nil nil))
            (*parser-lexical-rules* (get-matching-lex-rules nil))
            (*lexical-entries-used* nil)
            (*minimal-vertex* 0)
            (*maximal-vertex* len-tokens)
            ;;
            ;; shadow global variable to allow best-first mode to decrement for
            ;; each result found; eliminates need for additional result count.
            ;;                                              (22-jan-00  -  oe)
            (*first-only-p*
             (cond
              ((null first-only-p) nil)
              ((and (numberp first-only-p) (zerop first-only-p)) nil)
              ((numberp first-only-p) first-only-p)
              (t 1))))
        (declare (special *minimal-vertex* *maximal-vertex*))
        (with-parser-lock ()
          (flush-heap *agenda*)
          ;(clear-chart)
          (setf *cached-category-abbs* nil)
          (setf *parse-record* nil)
          (setf *parse-times* (list (get-internal-run-time)))
          (let ((*safe-not-to-copy-p* t))
	    ;(break)
	    ;(instantiate-chart-with-tokens user-input)
	    (ecase *morph-option*
	      (:default (instantiate-chart-with-morphop))
	      (:external-rule-by-rule 
	       (instantiate-chart-with-morphop))
	      ;;; *foreign-morph-fn* is set and will be called
	      (:external-partial-tree
	       (instantiate-chart-with-morpho-stem-edges))
	      (:with-tokeniser-partial-tree nil)
	      (:with-tokeniser-retokenise nil))
	    ;(break)
	    (instantiate-chart-with-stems-and-multiwords)
            ;(catch :best-first
              (add-words-to-chart (and first-only-p (null *active-parsing-p*)
                                       (cons 0
					     len-tokens
					     ;(length user-input)
					     )))
              (if *active-parsing-p*
                (complete-chart)
                (loop 
                    until (empty-heap *agenda*)
                    do (funcall (heap-extract-max *agenda*))))
	      ;)
            (unless first-only-p
              ;;
              ;; best-first (passive or active mode) has already done this
              ;; incrementally in the parse loop
              ;;
              (setf *parse-record* 
                (find-spanning-edges 0 
				     len-tokens
				     ;(length user-input)
				     ))))
          (push (get-internal-run-time) *parse-times*))
        (when show-parse-p (show-parse))
        (values *executed-tasks* *successful-tasks* 
                *contemplated-tasks* *filtered-tasks*))
      ;)
    ))

