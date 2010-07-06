;; Copyright (c) 2006
;;;   Ben Waldron;
;;; see `LICENSE' for conditions.

;;
;; code to convert SAF object to XML
;;

(in-package :saf)

(defun to-xml (saf &key (format :smaf))
  (let* ((meta (saf-meta saf))
	(lattice (saf-lattice saf))
	(start-node (saf-lattice-start-node lattice))
	(end-node (saf-lattice-end-node lattice))
	(nodes (saf-lattice-nodes lattice))
	(edges (saf-lattice-edges lattice))
	(strm (make-string-output-stream)))
    (format strm "~a" 
	    (saf-header meta :doctype format))
    (format strm "<~a init='v~a' final='v~a'~a>"
	    (if (eq format :saf)
		"fsm"
	      "lattice")
	    start-node
	    end-node
	    "" ;;TODO: opt cfrom/cto
;	    (if (eq :smaf format)
;		(format nil " cfrom='~a' cto='~a'"
;			(or (funcall *local-to-global-point-mapping* (point2str (car *span*))) "")
;			(or (funcall *local-to-global-point-mapping* (point2str (cdr *span*))) ""))
;	      "")
	    )
    (if (eq :saf format)
	(nodes-to-xml-states nodes strm))
    (loop
	for edge in edges
	do
	  (format strm "~a"
		  (edge-to-xml edge :doctype format)))
    (format strm "~a" 
	    (if (eq format :saf)
		"</fsm>"
	      "</lattice>"))
    (format strm "</~a>" (string-downcase (string format)))
    (get-output-stream-string strm)))

(defun nodes-to-xml-states (nodes strm)
  (loop
      for node in nodes
      do
	(format strm "<state id='v~a'/>" (xml-str node))))

#+:null
(defun point2str (x)
  (if x (format nil "~a" x)))

(defun edge-to-xml (annot &key doctype)
  (let* ((id (saf-edge-id annot))
	 (source (saf-edge-source annot))
	 (target (saf-edge-target annot))
	 (from (saf-edge-from annot))
	 (to (saf-edge-to annot))
	 (content (saf-edge-content annot))
	 (type (saf-edge-type annot))
	 elt attr-from attr-to)
    (case doctype
      (:smaf
       (setf elt "edge")
       (setf attr-from "cfrom")
       (setf attr-to "cto"))
      (:saf
       (setf elt "annot")
       (setf attr-from "from")
       (setf attr-to "to"))
      (t
       (error "unexected doctype")))
    (cond
     ((eq type :|ersatz|)
      (format nil "<~a type='ersatz' id='t~a' ~a='~a' ~a='~a' source='v~a' target='v~a'><slot name='name'>~a</slot><slot name='surface'>~a</slot></~a>"
	      elt
	      (xml-str id)
	      attr-from (xml-str from)
	      attr-to (xml-str to)
	      (xml-str source)
	      (xml-str target)
	      (xml-str (saf:saf-fs-feature-value2 content :|name|))
	      (xml-str (saf:saf-fs-feature-value2 content :|surface|))
	      elt
	      ))
      ((eq type :|token|)
       (format nil "<~a type='token' id='t~a' ~a='~a' ~a='~a' source='v~a' target='v~a'>~a</~a>"
	       elt
	       (xml-str id)
	       attr-from (xml-str from)
	       attr-to (xml-str to)
	       (xml-str source)
	       (xml-str target)
	       (xml-str content)
	       elt
	       )))))
    
(defun xml-str (x)
  (xml-escape (2-str x)))

(defun 2-str (x)
  (cond
   ((stringp x) x)
   ((symbolp x) (symb-2-str x))
   ((numberp x) (num-2-str x))
   ((pathnamep x) (namestring x))
   (t (error "unhandled type"))))

(defun symb-2-str (symb)
  (unless (symbolp symb)
    (error "symbol expected"))
  (cond
   ((null symb) "")
   (t (string-downcase (string symb)))))

(defun num-2-str (num)
  (if (null num)
      (return-from num-2-str))
  (unless (numberp num)
    (error "number expected"))
  (format nil "~a" num))
  
;;
;; XML serialization
;;

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

(defun get-timestamp nil
  (multiple-value-bind
      (second minute hour date month year dummy1 dummy2 dummy3)
      (decode-universal-time (get-universal-time) 0)
    (declare (ignore dummy1 dummy2 dummy3))
    (format nil "~2,'0d:~2,'0d:~2,'0d ~d/~2,'0d/~d (UTC)"
	    hour
	    minute
	    second
	    month
	    date
	    year)))

(defun saf-header (meta &key (doctype :saf))
    (let* ((addressing (saf-meta-addressing meta))
	   (document (saf-meta-document meta))
	   (doctype-str (string-downcase (string doctype))))
    (format nil
	    "<?xml version='1.0' encoding='UTF-8'?><!DOCTYPE ~a SYSTEM '~a.dtd'><~a~a~a>~a<olac:olac xmlns:olac='http://www.language-archives.org/OLAC/1.0/' xmlns='http://purl.org/dc/elements/1.1/' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xsi:schemaLocation='http://www.language-archives.org/OLAC/1.0/ http://www.language-archives.org/OLAC/1.0/olac.xsd'><identifier>s~a</identifier><creator>~a</creator><created>~a</created></olac:olac>"
	    doctype-str
	    doctype-str
	    doctype-str
	    (if document
		(format nil " document='~a'" (xml-escape (string document)))
	    "")
	    (if (or (eq :saf doctype))
		(format nil " addressing='~a'" (xml-escape (string addressing)))
	      "")
	    ""
;	    (if (eq :smaf doctype)
;		(format nil "<text>~a</text>" (xml-escape *text*))
;	      "")
	    (gen-id)
	    "x-preprocessor 1.00"
	    (xml-escape (get-timestamp)))))

(defvar *gen-id* 0)
(defun gen-id nil
  (incf *gen-id*))
