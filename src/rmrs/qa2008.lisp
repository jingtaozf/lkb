;;; Copyright (c) 2003--2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `LICENSE' for conditions.

(in-package :mrs)

(defparameter  *anchor-rmrs-p* t)

#|
(process-rasp-files-2008 "/usr/groups/mphil/qa08/parses-new/"
			 "/usr/groups/mphil/qa08/rmrs/ans-new/"))

(process-rasp-files-2008 "/anfs/bigdisc/aac10/trec8-2010-in/"
			 "/anfs/bigdisc/aac10/trec8-2010-out/")
|#
			 
(defun process-rasp-files-2008 (idirectory odirectory)
  ;;; clear and load the grammars
  (let ((*rasp-xml-word-p* t)
	(*anchor-rmrs-p* t))
    (clear-rule-record)
 (read-rmrs-grammar (make-pathname 
    :directory "/homes/aac10/lingo/lkb/src/rmrs/rasp3/"
    :name "gram15-general.rmrs"))
 (read-rmrs-tag-templates 
  (make-pathname :directory "/homes/aac10/lingo/lkb/src/rmrs/rasp3/"
		  :name "lex15.rmrs"))
 (let* ((ifiles (directory idirectory))
        (ofiles (directory odirectory))
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
		    idirectory
                    namestring "> /tmp/pfile"))
              (format t "~%File ~A unpacked~%" namestring)
              (let ((new-file (concatenate 'string 
                                odirectory
                                "top_docs."
                                qno "." "rmrs"))
                    (err-file (concatenate 'string 
                                "~aac10/rmrs-errs-new/"
                                "top_docs."
                                qno "." "errors")))
                (rmrs-from-file-2008 "/tmp/pfile" 
                                         "/tmp/rfile" :qa)
                (excl::shell "rm /tmp/pfile")
                (when (probe-file "/tmp/rfile")
		  (excl::shell (format nil "cp /tmp/rfile ~A" new-file))
                  (excl::shell 
                   (concatenate 'string
                     "xmlnorm -s " new-file " 2>| " err-file))
                  ;;; note we're redirecting std err
		  ;;; gzip the file
                  (excl::shell (concatenate 'string "gzip " 
                                            new-file))
		  (excl::shell "rm /tmp/rfile"))))))))) 


(defun rmrs-from-file-2008 (filename output xml-type)
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
      (let ((sentence-count 0))
        (loop
	  (when
	      (scan-for-string "<arborescence><![CDATA[" istream ostream)
	    (return))
		;;; scan-for-string reads from istream and writes to ostream
		;;; until it finds the sequence or hits the end of the stream
	        ;;; returns t in latter case
	  (let ((tree (read istream nil nil nil)))
	    (unless tree 
	      (format ostream "Error: Unexpected end of tree")
	      (return))
	    (terpri ostream)
	    (write tree :stream ostream)
	    (terpri ostream)
	    (when
		(scan-for-string "</arborescence>" istream ostream)
	        (format ostream "Error: Ill-formed arborescence")
	        (return))
	    (handler-case
		(progn
		  (construct-sem-for-tree tree :rasp ostream nil)
		  (finish-output ostream))
	      (storage-condition (condition)
		(format ostream "~%Error: Memory allocation problem: ~A~%" condition))
	      (error (condition)
		(format ostream "~%Error: ~A~%" condition))
	      (serious-condition (condition)
		(format ostream "~%Error: Something nasty: ~A~%" condition)))))))))

(defun scan-for-string (str istream ostream)
  (let* ((str-seq (coerce str 'list))
	(remainder str-seq))
    (loop 
      (when (null remainder) (return))
      (let ((chr (read-char istream nil nil)))
	(unless chr (return t))
	;;; return t at end of file
	(when chr
	  (princ chr ostream)
	  (if (char= chr (first remainder))
	      (setf remainder (rest remainder))
	    (setf remainder str-seq)))))))




;;;; converting Dan's output to RMRS

#| (convert-fine-system-output "~/qa2008/quesmrs/questions/result-corrected.ques" 
"/usr/groups/mphil/qa07/ques.txt"
"~/qa2008/quesrmrs/trec8/")

(convert-fine-system-output "~/qa2008/quesmrs/questions/test1.ques" 
"/usr/groups/mphil/qa07/ques.txt"
"~/qa2008/quesrmrs/trec8-new/")
|#

#|
(dolist (f (directory "~/qa2008/quesrmrs/trec8/")) 
	   (excl::shell 
	    (format nil "xmlnorm -s ~/qa2008/quesrmrs/trec8/~A 2>| ~/qa2008/quesrmrs/trec8/~A.err" (file-namestring f) (file-namestring f))))
|#


(defparameter *var-extra-conversion-table*
'(
  ((pers 1) .  (pers 1))
  ((pers 2) .  (pers 2))
  ((pers 3) .  (pers 3))
  
  ((div -) . (div -))
  ((div +) . (div +))
  ((ind -) . (ind -))
  ((ind +) . (ind +))
  
  ((num sg) . (num sg))

  ((tense untensed) . (tense u))
  ((tense basic_tense) . (tense u))
  ((tense no_tense) . (tense u))
  ((tense nontense) . (tense u))
  ((tense future) . (tense future))
  ((tense present) . (tense present))
  ((tense past) . (tense past))
;;;  ((e.tense nonpresent) . (tense non-present))
  ((tense nonpresent) . (tense u))
  ;;; my version of the DTD doesn't have `non-present'
  ;;; replace this with line above if using a DTD that does
  ((tense nonpast) . (tense non-past))
  
 ;;; note the interpretation is intended to be that the 
 ;;; first match is taken.  For RMRS->MRS conversion, there's
 ;;; a sl problem in that nontense and no_tense are 
 ;;; both possible values corresponding to (tense u)
 ;;; and that this also corresponds to the `don't know'
 ;;; case.  We therefore need to translate the RMRS `u'
 ;;; into `basic_tense'
))

(defun convert-fine-system-output (ifile qtxt odir)
  (let ((*anchor-rmrs-p* t)
	(qlist nil))
    (with-open-file (qstream qtxt :direction :input)
      (loop (let ((next-q (read-line qstream nil nil)))
	      (unless next-q (return))
	      (push next-q qlist)))
      (setf qlist (nreverse qlist)))
    (with-open-file (istream ifile :direction :input)
      (loop (let ((fsout (read-line istream nil nil)))
	      (unless fsout (return))
	      (let ((scount (extract-fine-system-number fsout))
		    (mrs-string (extract-fine-system-mrs fsout)))
		(when (and (integerp scount)
			   (stringp mrs-string)
			   (not (equal mrs-string "")))
		  (let ((q (elt qlist (- scount 1))))
		    (when q
		      (with-input-from-string (mstream mrs-string)
			(let* ((mrs (read-mrs mstream))
			       (rmrs (mrs-to-rmrs mrs)))
			  (when rmrs
			    (let ((ofile (format nil "~Aq~A.rmrs" odir scount)))
			      (with-open-file 
				  (ostream ofile :direction :output 
				   :if-exists :supersede)
				(format ostream "~%<S id='~A'>" scount)
				(format ostream
                                     "~%<string>~%~S~%</string>" q)
				(output-rmrs1 rmrs 'xml ostream)
				(format ostream "</S>~%")
				(finish-output ostream)))))))))))))))

(defun extract-fine-system-mrs (str)
  ;;; compare extract-fine-system-sentence
  (if (find #\@ str)
      (let ((ampcount 0)
	    (sstart nil) 
	    (send nil))
	(dotimes (n (length str)) 
	  (let ((char (elt str n))) 
	    (when (eql char #\@) 
	      (setf ampcount (+ 1 ampcount))
	      (when (eql ampcount 13)
		(setf sstart (+ 1 n)))))
	  (when (eql ampcount 14)
	    (setf send n)
	    (return)))
	(if (and sstart send)
	    (subseq str sstart send)
	  str))
    str))

(defun extract-fine-system-number (str)
  ;;; compare extract-fine-system-sentence
  (let ((apos (position #\@ str)))
	(if apos
	    (parse-integer (subseq str 0 apos) :junk-allowed t))))