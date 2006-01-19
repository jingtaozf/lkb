;;; Copyright (c) 2006
;;;   Ben Waldron;
;;; see `licence.txt' for conditions.

(in-package :lkb)

(defun process-standoff-sentence-file (filename)
  (process-saf-sentences
   (xml-sentences-to-saf-object 
    (read-file-to-string filename))))

(defun read-file-to-string (filename)
  (coerce (with-open-file (ifile filename
			   :direction :input)
	    (loop
		for c = (read-char ifile nil)
		while c
		collect c))
	  'string))
  

(defun process-saf-sentences (saf)
  (let* ((textfilename (saf-meta-document (saf-meta saf)))
	 (text
	 (read-file-to-string textfilename))
	 (ofilename (format nil "~a.out" textfilename)))
    (with-open-file (ofile ofilename
		     :direction :output
		     :if-exists :overwrite
		     :if-does-not-exist :create)
      (format ofile "<analyses>")
      (loop for s in (saf-lattice-edges (saf-lattice saf))
	  do
	    (let ((*xchar-map-add-offset* 
		   (point-to-char-point (saf-edge-from s) "xchar")))
	      (setf *xchar-map-add-offset* *xchar-map-add-offset*)
	      (x-parse
	       (x-span text 
		       (saf-edge-from s) 
		       (saf-edge-to s)
		       (saf-meta-addressing (saf-meta saf)))
	       :char-map #'xchar-map-add-x)
	      ;(let ((mrs::*rmrs-xml-output-p* t))
		(dump-sentence-analyses s ofile)
		;)
	      ))
      (format ofile "~&</analyses>")
      )))

;;based on mrs::output-mrs-after-parse
(defun dump-sentence-analyses (s &optional (stream t))
  (let ((*print-circle* nil))
    (loop for edge in *parse-record* 
	do
	  (let ((mrs (mrs::extract-mrs edge)))
	    (format stream "~&<analysis daughters='~a' edge='~a'>"
		    (saf-edge-id s)
		    (lkb::edge-id edge))
	    ;(format stream "~&~A~&" 
		;    (lkb::parse-tree-structure edge))
	    (mrs::output-rmrs1 (mrs::mrs-to-rmrs mrs) 'mrs::xml stream)
	    (format stream "~&</analysis>")
	    ))
    ))

(defun x-span (text from to addressing)
  (cond
   ((string= "xchar" addressing)
    (subseq text 
	    (point-to-char-point from addressing)
	    (point-to-char-point to)))
   ((string= "xpoint" addressing)
    (error "addressing scheme 'xpoint' not implemented"))
   (t
    (error "unknown addressing scheme '~a'" addressing))))

(defun xml-sentences-to-saf-object (xml)
  (lxml-sentences-to-saf-object (xml-to-lxml xml)))

(defun lxml-sentences-to-saf-object (lxml)
  (setf lxml (first
	      (check-doctype (remove-xml-header lxml) "sentences")))
  (make-saf :meta (make-saf-meta :document (lxml-elt-attr lxml "document")
				 :addressing "xchar")
	    :lattice (lxml-sentences-to-lattice lxml)))

(defun lxml-sentences-to-lattice (lxml)
  (unless (eq '|sentences| (lxml-elt-name lxml))
    (error "<sentences> expected"))
  (let ((contents (lxml-elt-contents lxml)))
    (make-saf-lattice
     :start-node 0
     :end-node (length contents)
     :nodes (loop for s in contents
		collect (lxml-elt-attr s "id"))
     :edges (loop for s in contents
		collect (lxml-sentence-to-edge s)))))

(defun lxml-sentence-to-edge (lxml)
  (unless (eq '|sentence| (lxml-elt-name lxml))
    (error "<sentence> expected"))
  (make-saf-edge
   :type :sentence
   :id (lxml-elt-attr lxml "id")
   :from (lxml-elt-attr lxml "from")
   :to (lxml-elt-attr lxml "to")))
  

