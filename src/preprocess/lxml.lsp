;;; Copyright (c) 2006
;;;   Ben Waldron;
;;; see `licence.txt' for conditions.

(defpackage :lxml
  (:use :common-lisp) 
  (:export #:shift-package))

(in-package :lxml)

(defun xml-whitespace-char (c)
  (let ((c-code (char-code c)))
    (or (eq c-code #x20)
	(eq c-code #x9)
	(eq c-code #xD)
	(eq c-code #xA))))

(defun xml-whitespace-p (str)
  (and 
   (stringp str)
   (loop for c across str
	 when (not (xml-whitespace-char c))
	 do (return nil)
	 finally (return t))))

;; for use with S-XML
(defun xml-to-lxml (xml)
  (with-input-from-string (s xml)
    (xml:parse-xml s)))

;; for use with pxml
#+:null
(defun xml-to-lxml (xml)
  (lkb::with-package (:keyword)
    (remove-xml-header-and-doctype
     (discard-whitespace
      (with-input-from-string (s xml)
	(xml:parse-xml s))))))

;; if xml header, remove and check correct
(defun remove-xml-header-and-doctype (lxml)
  (if (and (listp (car lxml))
	   (eq :xml (lxml-pi-name (car lxml))))
      (pop lxml))
  (if (and (listp (car lxml))
	   (eq :doctype (lxml-pi-name (car lxml))))
      (pop lxml))
  (if (listp (car lxml))
      (car lxml)
    lxml))
  
(defun elements-only (lxml)
  (loop for x in lxml
      when (and (listp x) (not (keywordp (car x))))
      collect x
      when (and (symbolp x) (not (keywordp x)))
      collect x))

;;
;; LXML
;;

(defun lxml-elt-p (x)
  (listp x))

(defun lxml-elt-contents (lxml-elt)
  (unless (lxml-elt-p lxml-elt)
    (error "lxml element expected: got ~a" lxml-elt))
  (cdr lxml-elt))

(defun lxml-elt-text-content (lxml-elt)
  (let ((contents (lxml-elt-contents lxml-elt)))
    (if (and (null (cdr contents))
	     (stringp (car contents)))
	(car contents)
      (error "~&Text content expected. Got: ~a" contents))))
  
(defun lxml-elt-text-content2 (lxml-elt)
  (let ((contents (lxml-elt-contents lxml-elt)))
    (if (and (null (cdr contents))
	     (stringp (car contents)))
	(car contents))))
  
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

(defun lxml-elt-attr (lxml-elt attrib)
  (unless (lxml-elt-p lxml-elt)
    (error "lxml element expected: got ~a" lxml-elt))
  (let ((car (car lxml-elt)))
    (typecase car
     (symbol nil)
     (list 
      (let ((x (second (member attrib (cdr car)))))
	(if (stringp x) x)
	)
      )
     (t (error "expected symbol or list as car of lxml element: got ~a" car)))))
       
(defun lxml-elt-elts (lxml-elt elt-name)
  (unless (lxml-elt-p lxml-elt)
    (error "lxml element expected: got ~a" lxml-elt))
    (loop for e in (cdr lxml-elt)
	when (eq elt-name (and (lxml-elt-p e) 
			       (lxml-elt-name e)))
	collect e))

(defun lxml-elts (lxml)
  (elements-only 
   (cdr lxml)))
       
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

(defun lxml-pi-attr (lxml-pi attrib)
  (unless (lxml-pi-p lxml-pi)
    (error "lxml element expected: got ~a" lxml-pi))
    (second (member attrib (cdr lxml-pi))))

;;
;; XML
;;

;; return CDATA-wrapped text
#+:preprocessor
(defun wrap-cdata (str)
  (lkb::concatenate-strings
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
	   unless (xml-whitespace-p x)
	   collect (discard-whitespace x))
    lxml))

;; misc fns

(defun shift-package (lxml package)
  (loop
      for x in lxml
      collect 
	(cond
	 ((listp x)
	  (shift-package x package))
	 ((symbolp x)
	  (intern (string x) package))
	 (t
	  x))))

;; fix me
(in-package :lkb)

(defun concatenate-strings (x)
  (apply #'concatenate
	 (cons 'string x)))

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

;;
;; XML test suite
;;

(defvar *xml-test-neg* nil)
(defvar *xml-test-pos* nil)

(defun xml-test-initialise nil
  (setf *xml-test-neg* nil)
  (setf *xml-test-pos* nil))

(defun xml-test-file (filename)
  (handler-case
      (with-open-file (s filename) 
	(xml:parse-xml s)
	(push filename *xml-test-pos*))
    (error (condition)
      (format t  "~&FILENAME: ~a Error: ~A" filename condition)
      (push (cons filename condition) *xml-test-neg*))))

#+:allegro
(defun xml-test-files (pattern)
  (xml-test-initialise)
  (loop
    for filename in
	(excl.osi:command-output (format nil "ls ~a" pattern))
      do 
	(xml-test-file filename)
	))
