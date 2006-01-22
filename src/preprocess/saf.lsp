;;; Copyright (c) 2006
;;;   Ben Waldron;
;;; see `licence.txt' for conditions.

(in-package :lkb)

(defun process-standoff-sentence-file (filename)
  (with-open-file (ofile 
		   (merge-pathnames 
		    (make-pathname :name (format nil "~a.out" 
						 (pathname-name (pathname filename))))
		    (pathname filename))
		   :direction :output
		   :if-exists :overwrite
		   :if-does-not-exist :create)
    (format t "~%OUTPUT FILE: ~a" (namestring ofile))
    (process-saf-sentences
     (xml-to-saf-object 
      (read-file-to-string filename)
      :saf-dir (pathname-directory (pathname filename)))
     :ostream ofile)))

(defun read-file-to-string (filename)
  (coerce (with-open-file (ifile filename
			   :direction :input)
	    (loop
		for c = (read-char ifile nil)
		while c
		collect c))
	  'string))

(defun process-saf-sentences (saf &key (ostream t))
  (let* ((textfilename (saf-meta-document (saf-meta saf)))
	 (text
	  (read-file-to-string textfilename)))
    (format ostream "~a"
	    (saf-header :addressing "char"
			:document (saf-meta-document (saf-meta saf))))
    (loop for s in 
	  (sort (copy-list (saf-lattice-edges (saf-lattice saf)))
		#'string> :key #'saf-edge-from)
	do
	  (let ((*char-map-add-offset* 
		 (point-to-char-point (saf-edge-from s) "char")))
	    (setf *char-map-add-offset* *char-map-add-offset*)
	    (x-parse text 
		     (saf-edge-from s) 
		     (saf-edge-to s)
		     (saf-meta-addressing (saf-meta saf))
		     :document (saf-meta-document (saf-meta saf))
		     :char-map #'char-map-add-x
		     :show-parse nil)
	    (dump-sentence-analyses s ostream)))
    (format ostream "~&</saf>")))

;;based on mrs::output-mrs-after-parse
(defun dump-sentence-analyses (s &optional (stream t))
  (let ((*print-circle* nil))
    (loop for edge in *parse-record* 
	do
	  (let ((mrs (mrs::extract-mrs edge)))
	    (format stream "~&<annot type='parse' daughters='~a' edge='~a'>" ;;move edge into content
		    (saf-edge-id s)
		    (lkb::edge-id edge))
	    ;(format stream "~&~A~&" 
		;    (lkb::parse-tree-structure edge))
	    (mrs::output-rmrs1 (mrs::mrs-to-rmrs mrs) 'mrs::xml stream)
	    (format stream "~&</annot>")
	    ))
    ))

(defun x-span (text from to addressing)
  (cond
   ((string= "char" addressing)
    (subseq text 
	    (point-to-char-point from addressing)
	    (point-to-char-point to addressing)))
   ((string= "xpoint" addressing)
    (error "addressing scheme 'xpoint' not implemented"))
   (t
    (error "unknown addressing scheme '~a'" addressing))))

(defun xml-sentences-to-saf-object (xml)
  (lxml-to-saf-object (xml-to-lxml xml)))

(defun lxml-sentences-to-saf-object (lxml)
  (setf lxml (first (check-doctype (remove-xml-header lxml) "sentences")))
  (make-saf :meta (make-saf-meta :document (lxml-elt-attr lxml "document")
				 :addressing (lxml-elt-attr lxml "addressing"))
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
		collect (lxml-sentence2-to-edge s)))))

(defun lxml-sentence2-to-edge (lxml)
  (unless (eq '|sentence| (lxml-elt-name lxml))
    (error "<sentence> expected"))
  (make-saf-edge
   :type :sentence
   :id (lxml-elt-attr lxml "id")
   :from (lxml-elt-attr lxml "from")
   :to (lxml-elt-attr lxml "to")))
  

