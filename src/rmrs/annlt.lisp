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

(defparameter *rasp-full-numbers-p* t)

(defparameter *initial-rasp-num* nil)

(defun get-cfrom (str)
  ;;; <w s="19" e="24">bark+ed_VVD</w>
  ;;; extract 19
  (let ((first-s (position #\s str)))
    (if (and (char= (elt str (+ 1 first-s)) #\=)
	     (char= (elt str (+ 2 first-s)) #\"))
	(let ((spec-num
	       (parse-integer (subseq str (+ 3 first-s)) :junk-allowed t)))
	  (if (and spec-num 
		   (integerp spec-num))
	      (if *initial-rasp-num*
		  (- spec-num *initial-rasp-num*)
		spec-num)
	    nil))
      nil)))

(defun get-cto (str)
  ;;; <w s="19" e="24">bark+ed_VVD</w>
  ;;; extract 24
  (let ((first-e (position #\e str)))
    (if (and (char= (elt str (+ 1 first-e)) #\=)
	     (char= (elt str (+ 2 first-e)) #\"))
	(let ((spec-num
	       (parse-integer (subseq str (+ 3 first-e)) :junk-allowed t)))
	  (if (and spec-num 
		   (integerp spec-num))
	      (if *initial-rasp-num*
		  (- spec-num *initial-rasp-num*)
		spec-num)
	    nil))
      nil)))

;;; temporary function to make numbering start at 0
;;; for each sentence

;;; call with most-positive-fixnum to be safe
;;; setf *initial-rasp-num* whatever
;;; deduct this from all cfrom cto


(defun scan-rasp-for-first-num (tree-node min)
  (if (daughter-nodes-p tree-node)
      (let ((dtr-nodes (get-dtr-nodes tree-node)))
	(loop for dtr in dtr-nodes
	    do
	      (let ((min-dtr (scan-rasp-for-first-num dtr min)))
		(when (< min-dtr min)
		  (setf min min-dtr))))
	min)
    (let ((cfrom (get-cfrom (string tree-node))))
      (or cfrom most-positive-fixnum))))

  
;;; top level call
;;; multiple files


#|
(simple-process-rasp-file 
 (make-pathname 
   :device "d"
   :directory "/lingo/lkb/src/rmrs/annlt-test/"
   :name "semtest.rasp")
 "semtest.rmrs" nil)

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
			(when *rasp-full-numbers-p* 
			  (setf *initial-rasp-num* nil)
			  (setf *initial-rasp-num*
			    (scan-rasp-for-first-num 
			     tree most-positive-fixnum)))
			(construct-sem-for-tree tree :rasp ostream))
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
  
;;; *********** code for `parsing' tsg rules **************
;;;
;;; This is very crude - the idea is to output
;;;
;;; <rule>
;;; <name>N1/n_n1</name>
;;; <dtrs><dtr>N1</dtr><dtr>OPT</dtr><dtr>N2</dtr></dtrs>
;;; <head>N2</head>
;;; </rule>
;;;
;;; from
;;; 
;;; PSRULE N1/n_n1 : N1[POSS -] --> N0[POSS -, NTYPE NORM] (+pco) 
;;;                                 H1[NTYPE NORM, MOD -, -ta].
;;; Mostly from the name but need the rest to get OPTS
;;;
;;;
;;; need to treat NG opt specially eventually

(defun make-tsg-break-table nil 
  (lkb::define-break-characters '(#\[ #\] #\.)))

#|
(mrs::parse-tsg-file "rmrs/annlt-test/tsg-frag"  
		     "rmrs/annlt-test/tsg-frag.out")
|#

(defun parse-tsg-file (ifile ofile)
  (with-open-file (istream ifile :direction :input)
    (with-open-file (ostream ofile :direction :output
		     :if-exists :supersede)
      (let ((*readtable* (make-tsg-break-table)))
	(loop (let ((next-char (peek-char t istream nil 'eof)))
		(when (eql next-char 'eof) (return))
		(cond ((eql next-char #\;) 
		       (read-line istream))
					; one line comments
		      (t (multiple-value-bind
			     (name mother dtr-strs dtrs)
			     (parse-tsg-rule istream)
			   (unless name
			     (return))
			   (output-skeleton-tsg-rule 
			    name mother dtr-strs dtrs
			    ostream))))))))))

(defun output-skeleton-tsg-rule (name mother dtr-strs dtrs 
				 ostream)
  ;;; can't tell for sure which the head is,
  ;;; except for unary rules
  (let* ((real-dtr-names (construct-tsg-dtr-names dtr-strs))
	 (names real-dtr-names)
	 (next-name nil))
    (format ostream "~%<rule>")
    (format ostream "~%<name>~A</name>" name)
    (format ostream "~%<dtrs>")
    (dolist (dtr dtrs)
      (unless dtr
	(setf next-name (car names))
	(setf names (cdr names)))
      (format ostream "<dtr>~A</dtr>" (if dtr "OPT" next-name))) 
    (format ostream "</dtrs>")
    (format ostream "~%<head>~A<head>" 
	    (or (guess-tsg-head mother real-dtr-names)
		"FIX_ME"))
    (format ostream "~%</rule>")
    (finish-output ostream)))

(defun construct-tsg-dtr-names (dtr-strs)
  (let ((types nil))
    (loop for dtr-str in dtr-strs
      collect
      (let* ((type (elt dtr-str 0))
	     (type-count (assoc type types)))
	(if type-count
	    (let ((count (cdr type-count)))
	      (setf (cdr type-count)
		(+ 1 count))
	      (format nil "~A~A" type count))
	  (let ((count 1))
	    (push (cons type count)
		  types)
	    (format nil "~A" type)))))))

(defun guess-tsg-head (mother real-dtr-names)
  (declare (ignore mother)) ;;; FIX later
  (if (cdr real-dtr-names)
      nil
    (car real-dtr-names)))
    

(defun parse-tsg-rule (istream)
  (let ((dtrs nil))
    (lkb::check-for-string "PSRULE" istream)
    (multiple-value-bind (name mother dtr-strs)
	(parse-tsg-name istream)
      (lkb::check-for-string ":" istream)
      (parse-tsg-non-opt istream)
      (lkb::check-for-string "-->" istream)
      (setf dtrs (parse-tsg-dtrs istream))
      (lkb::check-for-string "." istream)
      (values name mother dtr-strs dtrs))))

(defun parse-tsg-name (istream)
  ;;; given N1/n_n1 
  ;;; outputs "N1/n_n1" "N1" ("n" "n1")
  (let ((mlist nil)
	(mother nil)
	(dtrs nil)
	(dlist nil)
	(full nil))
    (peek-char t istream nil nil)
    (loop (let ((next-char (peek-char nil istream nil 'eof)))
               (push next-char full)		
	    (cond ((eql next-char 'eof) (error "End of file in name"))
		  ((eql next-char #\/)
		   (read-char istream)
		   (return))
		  (t (read-char istream)
		   (push next-char mlist)))))
    (setf mother (coerce (nreverse mlist) 'string))
    (loop (let ((next-char (peek-char nil istream nil 'eof)))
	    (cond ((eql next-char 'eof) (error "End of file in name"))
		  ((eql next-char #\space) 
		   (push (coerce (nreverse dlist) 'string)
			 dtrs)
		   (setf dlist nil)
		   (return))
		  ((eql next-char #\_)
		   (push next-char full)
		   (read-char istream)
		   (unless dlist
		     (error "Empty dtr"))
		   (push (coerce (nreverse dlist) 'string)
			 dtrs)
		   (setf dlist nil))
		  (t (push next-char dlist)
		     (push next-char full)
		     (read-char istream)))))
    (values (coerce (nreverse full) 'string)
	    mother
	    (nreverse dtrs))))

(defun parse-tsg-non-opt (istream)
  (let* ((name (read istream nil nil))
	 (new-char (peek-char t istream nil nil)))
    (declare (ignore name))
    (when (eql new-char #\[)
      (let ((next (peek-char #\] istream nil nil)))
	(unless next
	  (error "File ends inside []"))
	(read-char istream)))))
     
(defun parse-tsg-dtrs (istream)
  ;;; count the dtrs, return the opts
  ;;; warn if there's a +
  (let ((dtrs nil))
    (loop 
      (let ((next-char (peek-char t istream nil nil)))
	(cond ((null next-char) (return))
	      ((eql next-char #\.) (return))
	      ((char= next-char #\()
	       (read-char istream)
	       (parse-tsg-non-opt istream)
	       (lkb::check-for-string ")" istream)
	       (let ((next-char (peek-char t istream nil nil)))
		 (when (eql next-char #\+)
		   (read-char istream)
		   (format t "Warning + in rule at ~A" 
			   (file-position istream))))
	       (push t dtrs))
	      (t (parse-tsg-non-opt istream)
		 (push nil dtrs)))))
    dtrs))


