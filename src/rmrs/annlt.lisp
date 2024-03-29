;;; Copyright (c) 2003--2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `LICENSE' for conditions.

(in-package :mrs)

;;; ANNLT specific

;;; The following parameters might usefully be set in a user-specific
;;; file for RASP-to-RMRS conversion.  Alternatively see testing.lisp

(defparameter *rasp-rmrs-gram-file* nil
  "grammar file for rasp to rmrs conversion")

;;; same for all variants
  
(defparameter *rasp-rmrs-tag-file* nil
  "tag file for rasp to rmrs conversion")

;;; same for all variants


(defparameter *rasp-xml-word-p* nil
  "set for versions of the RASP output which have the XML-style word structures")
;;; t for semtest
;;; nil for qa

(defparameter *rasp-xml-type* :none
  "either :standard, :none or :qa 
   if :standard, RASP is embedded in XML, if :qa settings as
   for qa experiments, if :none - raw RASP output")
;;; :none for semtest
;;; :qa for qa

(defparameter *rasp-xml-tag* "P"
  "when using XML input, this controls the tag we look for")
;;; "P" for all variants

(defparameter *rasp-input-file* nil
  "file for RASP input")

#|
(make-pathname :device "d" :directory "/lingo/lkb/src/rmrs/qa/"
	       :name "top_docs.24.parses")
|#

(defparameter *rasp-rmrs-output-file* nil
  "file for RASP-RMRS output")

#|
"semtest-rasp.rmrs"
(make-pathname :device "d" :directory "/lingo/lkb/src/rmrs/qa/"
	       :name "top_docs.24.rmrs")
|#

(defparameter *rasp-full-numbers-p* t
  "Controls whether RASP characterization is per file or per sentence")

;;; Calling RASP to RMRS code

(defun simple-process-rasp-file nil
 (clear-rule-record)
 (read-rmrs-grammar *rasp-rmrs-gram-file*)
 (read-rmrs-tag-templates *rasp-rmrs-tag-file*)
 (rmrs-from-file *rasp-input-file* *rasp-rmrs-output-file* *rasp-xml-type*))


;;; File wrapper - note use of handler-case
;;; All rather hacky due to need to cope with errors in input

(defparameter *initial-rasp-num* nil)

(defparameter *renumber-hack* nil
  "Hope this is a temporary measure while RASP trees and ERG are numbering differently")

(defparameter *predicted-pos* 1)

(defun rmrs-from-file (filename output xml-type)
  ;;; xml-type control if/how input is xmlified.  
  ;;; The wrappers etc are determined
  ;;; by the value of xml-type - currently :standard, :qa or :none
  ;;; when xml-type is :none, the input isn't xml
  ;;; (or only has xml-type tags for characters), and we construct a
  ;;; rmrs-list file as output
  (with-open-file (istream filename :direction :input)
    (with-open-file (ostream output :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
      (output-rmrs-xml-file-header ostream xml-type)
      (let ((sentence-count 0))
        (loop (let* ((markup (if (or (eql xml-type :qa)
				     (eql xml-type :standard))
				 (find-lisp-in-xml-tag istream 
						       *rasp-xml-tag*)))
                     ;; read in XML
                     (original (read istream nil nil))
                     (id (read istream nil nil))
                     (tree (read istream nil nil)))
                (declare (ignore id))
                (when markup
		  (format ostream "~A" markup))
                ;; output XML unchanged (except for whitespace)
                (unless tree
		  ;; there was a hack to get round lack of markup 
		  ;; at end when RASP misbehaves but now seems better
		  ;; to use Perl
                  (return))
                (when original
                  #|
                  blank lines in RASP cause the following
                  () 0 ; ()
                  
                  (X)
                  so we ignore cases where there's no sentence
                  |#
                  (unless (eql xml-type :none)
                    (setf sentence-count (+ 1 sentence-count))
                    (format ostream
                            "~%<S id='~A'>" sentence-count)
                    (format ostream
                            "~%<string>~%~S~%</string>" original)
                    (if *rasp-xml-word-p*    ; can't show tree
                        ; because XML tags clash
                        (format ostream
                                "~%<tree></tree>"
                                tree)
                      (format ostream
                                "~%<tree>~%~S~%</tree>"
                                tree)))
                  (handler-case
                      (progn
                        (unless (equal tree '(X))
                          (when *rasp-full-numbers-p* 
                            (setf *initial-rasp-num* nil)
                            (setf *initial-rasp-num*
                              (scan-rasp-for-first-num 
                               tree most-positive-fixnum)))
                          (setf *predicted-pos* 1)
                          (construct-sem-for-tree tree :rasp ostream original))
                        (finish-output ostream))
                    (storage-condition (condition)
                      (format ostream "~%Memory allocation problem: ~A~%" condition))
                    (error (condition)
                      (format ostream "~%Error: ~A~%" condition))
                    (serious-condition (condition)
                      (format ostream "~%Something nasty: ~A~%" condition)))
		  (unless (eql xml-type :none)
                    (format ostream
                            "~%</S>"))))))
      (output-rmrs-xml-file-end ostream xml-type))))


(defun output-rmrs-xml-file-header (ostream xml-type)
  (ecase xml-type
    (:none
	(format ostream "<?xml version='1.0'?> <!DOCTYPE rmrs-list SYSTEM \"/homes/aac10/lingo/lkb/src/rmrs/rmrs.dtd\" >")
	(format ostream "~%<rmrs-list>"))
    (:standard
     (format ostream "<?xml version='1.0'?> <!DOCTYPE CORPUS SYSTEM \"/usr/groups/mphil/qa05/dtd/analysis.dtd\" > 
<CORPUS> 
<DOC> 
<DOCNO/>
<TEXT>
<P>"))
    (:qa nil)))


(defun output-rmrs-xml-file-end (ostream xml-type)
  (ecase xml-type
    (:none
     (format ostream "~%</rmrs-list>"))
    (:standard
     (format ostream "</P></TEXT></DOC></CORPUS>~%"))
    (:qa nil)))
    

(defun find-lisp-in-xml-tag (istream tag-searched)
  ;;; contributed by Fabre
  ;;;
  ;;; allow for arbitrary xml stuff in between what we care about
  ;;; if xml-p is nil, this is a noop
  ;;; otherwise we scan forward looking for the first
  ;;; P> followed by ( - maybe with whitespace
  (let* ((stuff nil)
         (next-char (peek-char t istream nil nil)))
    (if (eql next-char #\()
        nil
      (progn
        ;; search for beginning of XML tag
        (loop
          (let ((input-char1 (read-char istream nil nil)))
            (unless input-char1 (return))
            (push input-char1 stuff)
            ;; when beginning of tag detected...
            (when (eql input-char1 #\<)
              (let ((tag-target (coerce tag-searched 'list))
                    (tag-found-p nil)
                    (end-found-p))
                ;; read the tag until whitespace or end of tag
                (loop
                  (let ((input-char2 (peek-char nil istream nil nil)))
                    (cond ((null input-char2) (return))
			  ;; so far tag looks like the one we're 
			  ;; searching for
                          ((and tag-target
                                (char-equal input-char2 (pop tag-target)))
                           (push input-char2 stuff)
                           (read-char istream nil nil))
			  ;; reached the end of the tag and it's what we
			  ;; were searching for
                          ((and (null tag-target)
                                (or (whitespacep input-char2)
                                    (eql input-char2 #\>)))
                           (setq tag-found-p t)
                           (return))
                          ;; bugger, not this one yet
                          (t
                           (push input-char2 stuff)
                           (read-char istream nil nil)
                           (return)))))
                (if tag-found-p
                    ;; if not yet found end of tag (eg. if there are
                    ;; attributes), search for the end.
                    (loop
                      (let ((input-char3 (read-char istream nil nil)))
                        (push input-char3 stuff)
                        ;; when end of tag finally found...
                        (when (eql input-char3 #\>)
                          ;; continue reading the stream until opening
                          ;; parenthesis is found
                          (let ((paren-test
                                 (loop (let ((input-char-inner
                                              (peek-char nil istream nil nil)))
                                         (cond ((null input-char-inner)
						(return :eof))
                                               ((eql input-char-inner #\()
                                                (return :read))
                                               ((eql input-char-inner #\<)
                                                (return :morexml))
                                               ((whitespacep input-char-inner)
                                                (read-char istream nil nil)
                                                (push input-char-inner stuff))
                                               (t (read-char istream nil nil)
                                                  (push input-char-inner stuff)
                                                  (return nil)))))))
                            (cond ((or (eql paren-test :read)
                                       (eql paren-test :eof))
                                   (setq end-found-p t)
                                   (return))
                                  ((eql paren-test :morexml)
				;;; (format t "Gargl, this tag contains more XML elements~%")
                                ;;; is this an error - AAC?
				   nil)))))))
                (if end-found-p
                    (return))))))
        (coerce (nreverse stuff) 'string)))))

;;; called from comp.lisp - data structures within trees

(defun daughter-nodes-p (node)
  (listp node))

(defun get-rule-name (node)
  (string (first node)))

(defun get-dtr-nodes (node)
  (rest node))


#|
some of the files just have
|Beijing:18_NP1|
others have `XML' e.g. <w S='Y' C='W'>He:1_PPHS1</w>
|#


(defun get-lexical-tag (node)
  (let* ((str (if *rasp-xml-word-p*
		  (de-xml-str (string node))
		(string node)))	      
         (uscore-pos (position #\_ str)))
    (if uscore-pos
	(subseq str (+ 1 uscore-pos)))))

;;; Note that ' is removed from the lexeme to avoid XML errors
;;; - also will match better with ERG output

(defun get-lexeme (node original)
  ;;; count is an attempt to keep track of where we are in an original
  ;;; string
  (let* ((xml-str (string node))
	 (str (if *rasp-xml-word-p* 
		  (de-xml-str xml-str)
		xml-str))
         (uscore-pos (position #\_ str))
         (notag (if uscore-pos (subseq str 0 uscore-pos) str))
         (tag (if uscore-pos (subseq str uscore-pos)))
         (colon-pos (position #\: notag :from-end t))
         (suffix-pos (position #\+ notag))
         (count (if original (get-word-count str colon-pos uscore-pos))))
    (when (and count (not (= count *predicted-pos*)))
      (setf count *predicted-pos*))
    (setf *predicted-pos* (+ 1 *predicted-pos*))
    ;;; the count isn't correct - try seeing whether keeping track 
        ;;; of the leaf nodes works instead
    (make-word-info
     :lemma 
     (remove #\'
      (if suffix-pos
          (subseq notag 0 suffix-pos)
        (if (and colon-pos (> uscore-pos (+ 1 colon-pos)))
            (subseq notag 0 colon-pos)
          notag)))
      :pos (if tag
	       (tag-letters tag)
	     "x")
      :original (if (and count (<= count (length original)))
                    (remove #\' (elt original (- count 1))))
      :from (get-cfrom xml-str)
      :to (get-cto xml-str))))


(defun tag-letters (tag)
  ;;; e.g., _NP1
  (cond ((eql (elt tag 1) #\N) "n")
        ((eql (elt tag 1) #\V) "v")
        ((eql (elt tag 1) #\J) "j")
        ((eql (elt tag 1) #\R) "r")
        ((eql (elt tag 1) #\I) "p")
	((and (eql (elt tag 1) #\A)
	      (eql (elt tag 2) #\T)) "q")
	((and (eql (elt tag 1) #\D)
	      (eql (elt tag 2) #\D)) "q")
	((and (eql (elt tag 1) #\C)
	      (eql (elt tag 2) #\C)) "c")
        (t "x")))


(defun de-xml-str (str)
  ;;; <w S='Y' C='W'>He:1_PPHS1</w>
  ;;; to He:1_PPHS1
  (let* ((first-end (position #\> str))
         (after-tag (subseq str (+ 1 first-end)))
         (second-first (position #\< after-tag)))
    (subseq after-tag 0 second-first)))

(defun get-word-count (str colon-pos uscore-pos)
  ;;; "Margaret:17_NP1"
  ;;; extract 17
  (if (and (not *rasp-xml-word-p*)
           colon-pos uscore-pos)    
      (let ((count 
             (parse-integer (subseq str (+ 1 colon-pos) uscore-pos) 
                            :junk-allowed t)))
        (if (and count
                 (integerp count))
            count
          nil))
    nil))

(defun get-cfrom (str)
  ;;; <w s="19" e="24">bark+ed_VVD</w>
  ;;; extract 19
  (if *rasp-xml-word-p*
      (let ((first-s (position #\s str)))
	(if (and first-s ; *** jac 8/11/04
	         (char= (elt str (+ 1 first-s)) #\=)
		 (member (elt str (+ 2 first-s)) '(#\" #\'))) ; *** jac 22/9/04
	    (let ((spec-num
		   (parse-integer (subseq str (+ 3 first-s)) :junk-allowed t)))
	      (if (and spec-num 
		       (integerp spec-num))
		  (cond ((and *initial-rasp-num* *renumber-hack* )
			 (- spec-num (+ 2 *initial-rasp-num*)))
		        (*initial-rasp-num*
			 (- spec-num *initial-rasp-num*))
			(*renumber-hack* 
			 (- spec-num 2))
			(t
			 spec-num))
		nil))
	  nil))))

(defun get-cto (str)
  ;;; <w s="19" e="24">bark+ed_VVD</w>
  ;;; extract 24
  (if *rasp-xml-word-p*
      (let ((first-e (position #\e str)))
	(if (and (char= (elt str (+ 1 first-e)) #\=)
		 (member (elt str (+ 2 first-e)) '(#\" #\'))) ; *** jac 22/9/04
	    (let ((spec-num
		   (parse-integer (subseq str (+ 3 first-e)) :junk-allowed t)))
	      (if (and spec-num 
		       (integerp spec-num))
		  (cond ((and *initial-rasp-num* *renumber-hack*)
			 (- spec-num (+ 1 *initial-rasp-num*)))
		        (*initial-rasp-num*
			 (- spec-num *initial-rasp-num*))
			(*renumber-hack* 
			 (- spec-num 1))
			(t
			 spec-num))
		nil))
	  nil))))

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
  

;;; Example of use with QA experiments

#+:excl
(defun process-rasp-files nil
  ;;; clear and load the grammars
  (let ((*rasp-xml-word-p* nil))
    #|
        (wanted (mapcar #'(lambda (x) (format nil "~A" x))
        '(24))))
        |#
 (clear-rule-record)
 (read-rmrs-grammar (make-pathname 
    :directory "/homes/aac10/lingo/lkb/src/rmrs/annlt-test/"
    :name "gram14.1.rmrs"))
 (read-rmrs-tag-templates 
  (make-pathname :directory "/homes/aac10/lingo/lkb/src/rmrs/annlt-test/"
		  :name "lex14.1.rmrs"))
 (let* ((ifiles
         (directory "/usr/groups/mphil/qa03/parses/*"))
        (ofiles (directory "/local/scratch/aac10/qatest05/rmrs/*"))
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
                (and ; (member qno wanted :test #'string-equal)
                     (not (member qno ofile-qnos
                              :test #'string-equal))
                     (equal (subseq namestring 
                                    (- (length namestring) 2))
                            "gz"))
              ; (format t "~%File ~A wanted" namestring)
              (excl::shell 
               (concatenate 
                   'string "gunzip -c < " 
                    "/usr/groups/mphil/qa03/parses/"
                    namestring "> /tmp/pfile"))
              ; (format t "~%File ~A unpacked" namestring)
              (let ((new-file (concatenate 'string 
                                "/local/scratch/aac10/qatest05/rmrs/"
                                "top_docs."
                                qno "." "rmrs"))
                    (err-file (concatenate 'string 
                                "/local/scratch/aac10/qatest05/rmrs-errs/" 
                                "top_docs."
                                qno "." "errors")))
                (rmrs-from-file "/tmp/pfile" 
                                         "/tmp/rfile" :qa)
                (excl::shell "rm /tmp/pfile")
                (when (probe-file "/tmp/rfile")
                  ;; change the dtd to the right thing
                  (excl::shell 
                   (concatenate 'string  
                     "/homes/sht25/Clconversion/chg_dtd.p \"/homes/sht25/QA/unified\" \"/usr/groups/mphil/qa05/dtd/analysis\" CORPUS CORPUS /tmp/rfile > " new-file))
		  ;;; validate the XML
                  (excl::shell 
                   (concatenate 'string
                     "xmlnorm -Vs " new-file " 2>| " err-file))
                  ;;; note we're redirecting std err
		  ;;; gzip the file
                  (excl::shell (concatenate 'string "gzip " 
                                            new-file)))
                (excl::shell "rm /tmp/rfile"))))))))

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
;;;
;;; AAC - 2006
;;;
;;; modifications for tsg15 - remove everything after : (semantics)
;;; try and hack round daughter mismatches
;;; not tried again on tsg14 (hope this is unnecessary)



;;; a couple of utility functions

(defun make-tsg-break-table nil 
  ;;; define-break-characters in io-general/utils.lsp
  (let ((temporary-readtable (copy-readtable *readtable*)))
      (dolist (break-char '(#\[ #\] #\.))
         (set-macro-character break-char
            #'(lambda (stream x) (declare (ignore stream)) x)
            nil
            temporary-readtable))
      temporary-readtable))

(defun rmrs-check-for-string (str istream)
  ;;; check-for-string in io-general/utils.lsp
  (loop for character in (coerce str 'list)
      do
	(let ((next-char (peek-char t istream nil 'eof)))
	  (if (char= next-char character)
	      (read-char istream)
	      (error 
                 "~%Syntax error: ~A expected and not found at position ~A" 
                 character (file-position istream))))))

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
  (let* ((real-dtr-names (construct-tsg-dtr-names dtr-strs dtrs))
	 (names real-dtr-names)
	 (next-name nil))
    (format ostream "~%<rule>")
    (format ostream "~%<name>~A</name>" name)
    (format ostream "~%<comment>AUTO</comment>")
    (format ostream "~%<dtrs>")
    (dolist (dtr dtrs)
      (unless (listp dtr)
	(setf next-name (car names))
	(setf names (cdr names)))
      (format ostream "<dtr>~A</dtr>" (if (listp dtr) "OPT" next-name))) 
    (format ostream "</dtrs>")
    (format ostream "~%<head>~A</head>" 
	    (or (guess-tsg-head mother real-dtr-names dtrs)
		"FIX_ME"))
    (format ostream "~%</rule>~%")
    (finish-output ostream)))

(defun construct-tsg-dtr-names (dtr-strs dtrs)
  (let ((dtr-count 0))
    (dolist (dtr dtrs)
      (unless (listp dtr)
	(setf dtr-count (+ 1 dtr-count))))
    (unless (eql (length dtr-strs) dtr-count)
      (format t "~%Warning ~A doesn't match ~A" dtr-strs dtrs)
      (setf dtr-strs dtrs))
    (let ((types nil))
      (loop for dtr-str in dtr-strs
	  collect
	    (let* ((type (if (listp dtr-str)
			     "OPT"
			     (string-upcase  dtr-str)))
		   (type-count (assoc type types :test #'equal)))
	      (if type-count
		  (let ((count (cdr type-count)))
		    (setf (cdr type-count)
		      (+ 1 count))
		    (format nil "~A~A" type count))
		(let ((count 1))
		  (push (cons type count)
			types)
		  (format nil "~A" type))))))))

(defun guess-tsg-head (mother real-dtr-names dtrs)
  (declare (ignore mother)) ;;; FIX later
  (if (cdr real-dtr-names)
      (let ((syn-head (find-tsg-syn-head dtrs)))
	(if (and syn-head (> (length real-dtr-names) syn-head))
	    (elt real-dtr-names syn-head)
	  nil))
    (car real-dtr-names)))

(defun find-tsg-syn-head (dtrs)
  (let ((head-count 0))
    (dolist (dtr dtrs)
      (cond ((listp dtr) nil)
	    ((eql (elt dtr 0) #\H)
	     (return head-count))
	    (t (setf head-count (+ 1 head-count))
	       nil)))))


(defun parse-tsg-rule (istream)
  (let ((dtrs nil))
    (rmrs-check-for-string "PSRULE" istream)
    (multiple-value-bind (name mother dtr-strs)
	(parse-tsg-name istream)
      (rmrs-check-for-string ":" istream)
      (parse-tsg-non-opt istream)
      (rmrs-check-for-string "-->" istream)
      (setf dtrs (parse-tsg-dtrs istream))
      (peek-char #\. istream nil nil)
      (rmrs-check-for-string "." istream)
      (values name mother dtr-strs dtrs))))

(defun parse-tsg-name (istream)
  ;;; given N1/n_n1 
  ;;; outputs "N1/n_n1" "N1" ("n" "n1")
  ;;; strip the -r
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
	    (loop for dtr in (nreverse dtrs)
		collect
		  (let ((count (length dtr)))
		    (if (and (char-equal (elt dtr (- count 1)) #\r)
			     (char-equal (elt dtr (- count 2)) #\-))
			(subseq dtr 0 (- count 2))
		      dtr))))))

(defun parse-tsg-non-opt (istream)
  (let* ((name (read istream nil nil))
	 (new-char (peek-char t istream nil nil)))
    (when (eql new-char #\[)
      (let ((next (peek-char #\] istream nil nil)))
	(unless next
	  (error "File ends inside []"))
	(read-char istream)))
    (string name)))
     
(defun parse-tsg-dtrs (istream)
  ;;; parse the dtrs, return the symbols
  ;;; warn if there's a +
  (let ((dtrs nil))
    (loop 
      (let ((next-char (peek-char t istream nil nil)))
	(cond ((null next-char) (return))
	      ((eql next-char #\.) (return))
	      ((eql next-char #\:) (return))
	      ;;; ignore all semantics stuff
	      ((char= next-char #\()
	       (read-char istream)
	       (let ((name (parse-tsg-non-opt istream)))
		 (rmrs-check-for-string ")" istream)
		 (let ((next-char (peek-char t istream nil nil)))
		   (when (eql next-char #\+)
		     (read-char istream)
		     (format t "~%Warning + in rule at ~A" 
			     (file-position istream))))
		 (push (list name) dtrs)))
	      (t 
		 (push (parse-tsg-non-opt istream) dtrs)))))
    (nreverse dtrs)))

#| 

Simple qa code.

|#

(defparameter *qa-test-suite*
    '((24 "When did Nixon visit China?" 1)))

(defun make-a-file-name (num)
  (let ((filename (format nil "top_docs.~A.rmrs" num)))
    (make-pathname :directory "/local/scratch/aac10/qatest05/rmrs/"
		   :name filename)))

(defun extract-qa-structs (a-file)
  (let ((qa-structs nil))
    (with-open-file (istream a-file :direction :input)
      (let ((xml (parse-xml-removing-junk istream)))
	(unless (equal (car xml) 'corpus)
	  (error "~A is not a valid qa file" a-file))
	(loop for doc in (cdr xml)
	    unless (stringp doc)
	    do
	      (unless (equal (caar doc) 'doc)
		(error "~A is not a valid qa file" a-file))
	      (loop for thing in (cdr doc)
		  unless (stringp thing)
		  do
		    (let ((tag (car thing)))
		      (when (eql tag 'text)
			(loop for text-el in (cdr thing)
			    unless (stringp text-el)
			    do
			      (let ((subtag (car text-el)))
				(when (eql subtag 'p)
				  (loop for p-el in (cdr text-el)
				      unless (stringp p-el)
				      do
					(let ((subsubtag (car p-el)))
					  (when (eql (car subsubtag) 's)
					    (push
					     (extract-qa-struct p-el)
					     qa-structs)))))))))))
	(nreverse qa-structs)))))

(defstruct qa-struct 
  str
  tree 
  rmrs)

(defun extract-qa-struct (s)
  (let* ((real-stuff 
	  (loop for thing in (cdr s)
	      unless (stringp thing)
	      collect thing))
	 (str (second (first real-stuff)))
	(tree (second real-stuff))
	(rmrs (read-rmrs (third real-stuff) :rasp)))
    (declare (ignore tree))
    (make-qa-struct :str str
		    :rmrs rmrs)))
	
#+:lkb
(defun test-qa-eg (egnum)
  (let* ((eg (assoc egnum *qa-test-suite*))
	 (input (second eg))
	 (parse-number (third eg))
	 (q-rmrs (rmrs-for-sentence input parse-number))
	 (a-file (make-a-file-name egnum)))
    (when (and q-rmrs a-file)
	 (let ((qa-recs nil)
	       (qa-structs (extract-qa-structs a-file)))
	   (dolist (qa-struct qa-structs)
	     (let* ((a-rmrs (qa-struct-rmrs qa-struct))
		    (original (qa-struct-str qa-struct))
		    (comparison-records 
		     (compare-rmrs q-rmrs a-rmrs nil)))
	       (when comparison-records
		 (let
		     ((score (qa-score (car comparison-records)))
		      (to-beat (fifth qa-recs)))
		   (if to-beat
		       (when (> score (car (fifth qa-recs)))
			 (format t "~%New top rank ~A" original)
			 (setf qa-recs 
			   (subseq 
			    (sort (cons (cons score original)
					qa-recs) #'> :key #'car)
			    0 4)))
		     (setf qa-recs
		       (sort (cons (cons score original)
				   qa-recs) #'> :key #'car)))))))
	   (pprint qa-recs)))))
      
      
(defun qa-score (comp-record)
  ;;; not serious at this point, just to see whether anything works
  (let ((score 0))
    (dolist (rel-match (rmrs-comparison-record-matched-rels comp-record))
      (let* ((rel (match-rel-record-rel2 rel-match))
	     (pred (rel-pred rel)))
	(setf score (+ score
		       (cond ((equal pred "named_rel") 10)
			     ((realpred-p pred)
			      (let ((pos (realpred-pos pred)))
				(cond ((equal pos "n") 5)
				      ((equal pos "v") 4)
				      ((equal pos "j") 2)
				      ((equal pos "r") 1)
				      (t 0))))
			     (t 0))))))
    score))

