;;; Copyright (c) 2005
;;;   Ben Waldron;
;;; see `licence.txt' for conditions.

(in-package :lkb)

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

;;
;; LXML
;;

(defun lxml-elt-p (x)
  (listp x))

(defun lxml-elt-contents (lxml-elt)
  (unless (lxml-elt-p lxml-elt)
    (error "lxml element expected: got ~a" lxml-elt))
  (cdr lxml-elt))

(defun lxml-elt-attributes (lxml-elt)
  (unless (lxml-elt-p lxml-elt)
    (error "lxml element expected: got ~a" lxml-elt))
  (if (listp (car lxml-elt))
    (cdar lxml-elt)))

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
       
(defun lxml-elt-elts (lxml-elt elt-str &key keyword)
  (unless (lxml-elt-p lxml-elt)
    (error "lxml element expected: got ~a" lxml-elt))
  (unless (stringp elt-str)
    (error "string name of lxml element expected"))
  (let ((elt-name (if keyword
		      (intern elt-str :keyword)
		    (intern elt-str))))
    (loop for e in (cdr lxml-elt)
	when (eq elt-name (lxml-elt-name e))
	collect e)))
       
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

;;
;; XML
;;

;; input xml must have NO superfluous space
(defun pretty-print-xml (xml)
  (coerce
   (cdr ;; remove initial newline due to code below
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
	finally (return (append x (list c)))))
    'string))
  
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
#+:preprocessor
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
  
(defun discard-whitespace (lxml)
  (if (listp lxml)
       (loop
	   for x in lxml 
	   unless (whitespace-p x)
	   collect (discard-whitespace x))
    lxml))

(defun whitespace-p (str)
  (and (stringp str)
       (every #'whitespace-char-p
	      (coerce str 'list))))

(defun whitespace-char-p (c)
  (or (char= #\Space c)
      (char= #\Newline c)))

;; misc fns

(defun concatenate-strings (x)
  (apply #'concatenate
	 (cons 'string x)))

(defun read-file-to-string (filename)
  (coerce 
   (with-open-file (ifile filename
		    :direction :input)
     (loop
	 for c = (read-char ifile nil)
	 while c
	 collect c))
   'string))

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

