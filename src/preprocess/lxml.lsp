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

(defun concatenate-strings (x)
  (apply #'concatenate
	 (cons 'string x)))

