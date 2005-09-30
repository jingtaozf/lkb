;;; Copyright (c) 2005
;;;   Ben Waldron;
;;; see `licence.txt' for conditions.

(in-package :lkb)

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

;;

(defun lxml-elt-p (x)
  (listp x))

(defun lxml-elt-val (lxml-elt)
  (unless (lxml-elt-p lxml-elt)
    (error "lxml element expected: got ~a" lxml-elt))
  (unless (= (length (cdr lxml-elt)) 1)
    (error "we handle only simple string values"))
  (second lxml-elt))

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

(defun whitespace-p (str)
  (and (stringp str)
       (every #'whitespace-char-p
	      (coerce str 'list))))

(defun whitespace-char-p (c)
  (or (char= #\Space c)
      (char= #\Newline c)))

