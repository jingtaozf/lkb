;;; Copyright (c) 2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :mrs)

;;; ANNLT specific

;;; called from comp.lisp - data structures within trees

(defun daughter-nodes-p (node)
  (listp node))

(defun get-rule-name (node)
  (string (first node)))

(defun get-dtr-nodes (node)
  (rest node))

(defun get-lexical-tag (node)
  (let* ((str (de-xml-str (string node)))
         (uscore-pos (position #\_ str)))
    (subseq str (+ 1 uscore-pos))))    

(defun get-lexeme (node)
  (let* ((xml-str (string node))
	 (str (de-xml-str xml-str))
         (uscore-pos (position #\_ str))
         (notag (subseq str 0 uscore-pos))
         (tag (subseq str uscore-pos))
         (colon-pos (position #\: notag :from-end t))
         (suffix-pos (position #\+ notag)))
    (make-word-info
      :lemma 
      (if suffix-pos
          (subseq notag 0 suffix-pos)
        (if (and colon-pos (> uscore-pos (+ 1 colon-pos)))
            (subseq notag 0 colon-pos)
          notag))
      :pos
      (tag-letters tag)
      :from (get-cfrom xml-str)
      :to (get-cto xml-str))))


(defun tag-letters (tag)
  ;;; e.g., _NP1
  (cond ((and (eql (elt tag 1) #\N) (eql (elt tag 2) #\P)) tag)
        ;;; various sorts of NPs - will correspond to named_rel etc
        ;;; in ERG
        ((eql (elt tag 1) #\N) "n")
        ((eql (elt tag 1) #\V) "v")
        ((eql (elt tag 1) #\J) "j")
        ((eql (elt tag 1) #\R) "r")
        ((eql (elt tag 1) #\P) "p")
	((eql (elt tag 1) #\A) "q")
        (t "x")))


(defun de-xml-str (str)
  ;;; <w S='Y' C='W'>He:1_PPHS1</w>
  ;;; to He:1_PPHS1
  (let* ((first-end (position #\> str))
         (after-tag (subseq str (+ 1 first-end)))
         (second-first (position #\< after-tag)))
    (subseq after-tag 0 second-first)))

(defun get-cfrom (str)
  ;;; <w s="19" e="24">bark+ed_VVD</w>
  ;;; extract 19
  (let ((first-s (position #\s str)))
    (if (and (char= (elt str (+ 1 first-s)) #\=)
	     (char= (elt str (+ 2 first-s)) #\"))
	(parse-integer (subseq str (+ 3 first-s)) :junk-allowed t)
      nil)))

(defun get-cto (str)
  ;;; <w s="19" e="24">bark+ed_VVD</w>
  ;;; extract 24
  (let ((first-e (position #\e str)))
    (if (and (char= (elt str (+ 1 first-e)) #\=)
	     (char= (elt str (+ 2 first-e)) #\"))
	(parse-integer (subseq str (+ 3 first-e)) :junk-allowed t)
      nil)))


  
;;; top level call
;;; multiple files


#|
(simple-process-rasp-file 
 (make-pathname 
   :device "d"
   :directory "/lingo/lkb/src/rmrs/annlt-test/"
   :name "rasp.out")
 "xxx" nil)

|#

(defun simple-process-rasp-file (ifile ofile xml-p)
 (clear-rule-record)
 (read-rmrs-grammar 
  (make-pathname 
   :device "d"
   :directory "/lingo/lkb/src/rmrs/annlt-test/"
   :name "gram14.1.rmrs"))
 (read-rmrs-tag-templates 
  (make-pathname :device "d"
		 :directory "/lingo/lkb/src/rmrs/annlt-test/"
		 :name "lex14.1.rmrs"))
  (rmrs-from-file ifile ofile xml-p))

;;; File wrapper - note use of handler-case
;;; All rather hacky due to need to cope with errors in input

(defun rmrs-from-file (filename output xml-p)
  ;;; if xml-p is true, the input is xmlified and we retain
  ;;; the structure.  when xml-p is nil, the input isn't xml
  ;;; (or only has xml-type tags for characters), and we construct a
  ;;; rmrs-list file as output
  (with-open-file (istream filename :direction :input)
    (with-open-file (ostream output :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
      (unless xml-p
	(format ostream "<?xml version='1.0'?> <!DOCTYPE rmrs-list SYSTEM \"/homes/aac10/lingo/lkb/src/rmrs/rmrs.dtd\" >")
	(format ostream "~%<rmrs-list>"))
      (loop (let* ((markup (if xml-p (read-xml-characters istream)))
                   ;; read in XML
                   (original (read istream nil nil))
                   (id (read istream nil nil))
                   (tree (read istream nil nil)))
              (declare (ignore id))
	      (when markup
		(output-rmrs-file-markup ostream markup))
	      ;; output XML unchanged (except for whitespace)
	      (unless tree
		(when xml-p
		  (unless (and markup
			       (dolist (char (coerce markup 'list))
				 (unless (whitespacep char)
				   (return t))))
		  ;; hack round lack of markup at end when RASP
		  ;; misbehaves
		    (format ostream "~%</P>~%</TEXT>~%</DOC>~%</CORPUS>~%")))
		(return))
              (when original
                #|
                blank lines in RASP cause the following
                () 0 ; ()
                
                (X)
                so we ignore cases where there's no sentence
		|#
		(when xml-p
                  (format ostream
                          "~%<S>")
                  (format ostream
                          "~%<string>~%~S~%</string>" original)
                  (format ostream
                          "~%<tree>~%~S~%</tree>"
                          tree))
		(handler-case
		    (progn
		      (unless (equal tree '(X))
			(construct-sem-for-tree tree ostream))
		      (finish-output ostream))
		  (storage-condition (condition)
		    (format ostream "~%Memory allocation problem: ~A~%" condition))
		  (error (condition)
		    (format ostream "~%Error: ~A~%" condition))
		  (serious-condition (condition)
		    (format ostream "~%Something nasty: ~A~%" condition)))
		(when xml-p
		  (format ostream
			  "~%</S>")))))
      (unless xml-p
	(format ostream "~%</rmrs-list>")))))
              
(defun read-xml-characters (istream)
  ;;; allow for arbitrary xml stuff in between what we care about
  ;;; if xml-p is nil, this is a noop
  ;;; otherwise we scan forward looking for the first
  ;;; P> followed by ( - maybe with whitespace
  (let* ((stuff nil)
	 (next-char (peek-char t istream nil nil)))
    (if (eql next-char #\()
	nil
      (progn 
	(loop
	  (let ((input-char1 (read-char istream nil nil)))
	    (unless input-char1 (return))
	    (push input-char1 stuff)
	    (when (eql input-char1 #\P)
	      (let ((input-char2 (read-char istream nil nil)))
		(push input-char2 stuff)
		(when (eql input-char2 #\>)
		  (let ((paren-test
			 (loop (let ((input-char-inner 
				      (peek-char nil istream nil nil)))
				 (cond ((null input-char-inner) (return :eof))
				       ((eql input-char-inner #\()
					(return :read))
				       ((whitespacep input-char-inner)
					(read-char istream nil nil)
					(push input-char-inner stuff))
				       (t (read-char istream nil nil)
					  (push input-char-inner stuff)
					  (return nil)))))))
		    (if (or (eql paren-test :read)
			    (eql paren-test :eof))
			(return))))))))
	(coerce (nreverse stuff) 'string)))))

(defun output-rmrs-file-markup (ostream markup)
  (when markup
    (format ostream "~A" markup)))

;;; Example of use with QA experiments

#+:excl
(defun process-rasp-files nil
  ;;; clear and load the grammars
 (clear-rule-record)
 (read-rmrs-grammar "~aac10/lingo/newlkb/src/rmrs/annlt-test/gram14.1.rmrs")
 (read-rmrs-tag-templates "~aac10/lingo/newlkb/src/rmrs/annlt-test/lex14.1.rmrs")
 (let* ((ifiles
         ;;; (directory "~aac10/lingo/newlkb/src/rmrs/annlt-test/jan28/*"))
         ;;; (directory "/local/scratch/sht25/parses/*"))
         (directory "/local/scratch/aac10/qatest/parses/*"))
        (ofiles (directory "/local/scratch/aac10/qatest/rmrs/*"))
        (ofile-qnos (loop for ofile in ofiles
                        collect
                          (extract-qa-file-identifier 
                           (file-namestring ofile)))))
    (loop for ifile in ifiles
        do
          (let* ((namestring (file-namestring ifile))
                 (qno (extract-qa-file-identifier namestring)))
            (format t "~%Processing file ~A" namestring)
            (when
                (and (not (member qno ofile-qnos
                              :test #'string-equal))
                     (equal (subseq namestring 
                                    (- (length namestring) 2))
                            "gz"))
              (excl::shell 
               (concatenate 
                   'string "gunzip -c < " 
                   ;;; "/local/scratch/sht25/parses/"
                   "/local/scratch/aac10/qatest/parses/"
                   namestring "> /tmp/pfile"))
              (let ((new-file (concatenate 'string 
                                "/local/scratch/aac10/qatest/rmrs/"
                                "top_docs."
                                qno "." "rmrs"))
                    (err-file (concatenate 'string 
                                "/local/scratch/aac10/qatest/rmrs-errs/" 
                                "top_docs."
                                qno "." "errors")))
                (rmrs-from-file "/tmp/pfile" 
                                         "/tmp/rfile" t)
                (excl::shell "rm /tmp/pfile")
                (when (probe-file "/tmp/rfile")
                  ;; change the dtd to the right thing
                  (excl::shell 
                   (concatenate 'string  
                     "/homes/sht25/Clconversion/chg_dtd.p \"/homes/sht25/QA/unified\" \"/usr/groups/mphil/qa03/dtd/analysis\" CORPUS CORPUS /tmp/rfile > " new-file))
		  ;;; validate the XML
                  (excl::shell 
                   (concatenate 'string
                     "xmlnorm -Vs " new-file " 2>| " err-file))
                  ;;; note we're redirecting std err
		  ;;; gzip the file
                  (excl::shell (concatenate 'string "gzip " 
                                            new-file)))
                (excl::shell "rm /tmp/rfile")))))))


#+:excl
(defun revalidate-rmrs-files nil
  (let* ((ifiles
          (directory "/local/scratch/aac10/trec8qa/rmrs/*")))
    (loop for new-file in ifiles
        do
          (let* ((namestring (file-namestring new-file))
                 (qno (extract-qa-file-identifier namestring))
                 (err-file2 (concatenate 'string 
                                "/local/scratch/aac10/trec8qa/rmrs-errs2/" 
                                "top_docs."
                                qno "." "errors")))
             (when (equal (subseq namestring 
                                  (- (length namestring) 2))
                          "gz")
               (excl::shell 
               (concatenate 
                   'string "gunzip -c < "
                   "/local/scratch/aac10/trec8qa/rmrs/"
                   namestring "> /tmp/tfile"))
               (excl::shell 
                 (concatenate 'string
                   "xmlnorm -Vs /tmp/tfile 2>| " err-file2))
                  ;;; note we're redirecting std err
               (excl::shell "rm /tmp/tfile"))))))

(defun extract-qa-file-identifier (namestring)
  ;;; e.g. top_docs.1.parses.gz
  ;;; returns "1"
  (let* ((dot-pos (position #\. namestring))
        (dot-pos2 (if dot-pos
                      (position #\. namestring :start (+ 1 dot-pos)))) 
        (qno (if (and dot-pos dot-pos2)
                 (subseq namestring (+ 1 dot-pos) dot-pos2)
               namestring)))
    qno))
  
