;; Copyright (c) 2003--2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `LICENSE' for conditions.
;;;
;;; Temporary expedient until we can link up with the fine system

(in-package :mrs)


;;; ************* Temporary !!!! ************************

(defparameter *rmrs-test-suite*
'(    
("It rained." . 1) ; 1
("Abrams barked." . 1) 
("The window opened." . 1) 
("Abrams chased Browne." . 1) 
("Abrams handed Browne the cigarette." . 1) ; 5
("Abrams handed the cigarette to Browne." . 1) 
("Abrams bet Browne a cigarette that it rained." . 1) 
("Abrams knew that it rained." . 1) 
("Abrams intended to bark." . 1) 
("Abrams intended Browne to bark." . 1) 
("Every cat barked." . 1) ; 11
("Every cat chased some dog." . 2)
("My cat barked." . 1)
("It barked." . 1)
("The cat chased it." . 1)
("The cat chased itself." . 1) ; 16
("The cat chased one." . 4)
("Mine barked." . 1)
("That opened." . 1)
("Cats bark." . 1)
("Tobacco arrived." . 1) ; 21
("Some bark." . 1)
("Some of the cats bark." . 1)
("No cat barked." . 1)
("Did the dog bark?" . 1)
("Which dog barked?" . 1) ; 26
("Whose dog barked?" . 1)
("Chase Browne!" . 1)
("Abrams wondered which dog barked." . 1)
("Abrams wondered whether Browne barked." . 1)
("The dog that Browne chased barked." . 1) ; 31
("The dog to chase is barking." . 1)
("The dog was chased by Browne." . 2)
("The dog chased by Browne barked." . 2)
("The dog is barking." . 1)
("The dog has barked." . 1) ; 36
("The dog has been barking." . 1)
("The dog had been barking." . 1)
("The dog will bark." . 1)
("The dog is going to bark." . 2)
("The dog could bark." . 1) ; 41
("The dog couldn't bark." . 1) ; 
("The old dog barked." . 1)
("The dog barked softly." . 1)
("The dog probably barked." . 1)
("The dog barked in the garden." . 1) ; 46
("The dog barks now." . 1)
("The garden dog barked." . 1)
("The tobacco garden dog barked." . 2)
("The cat is old." . 1)
("The cat is in the garden." . 1) ; 51
("The barking dog chased Browne." . 1)
("Chased dogs bark." . 1)
("Chasing the cat is old." . 1)
("That the cat chases Browne is old." . 1)
("Dogs chase whatever barks." . 1) ; 56
("The dog barked every day." . 1)
("When did the dog bark?" . 2)
("Three of the dogs bark." . 1)
("Three bark." . 1)
("Browne's dog barks." . 1) ; 61
("Browne's barks." . 2)
("Twenty three dogs bark." . 1)
("Two hundred twenty dogs bark." . 1)
("Abrams arrived by car." . 1)
("Abrams kept barking." . 2) ; 66
("Browne squeezed the cat in." . 1)
("Browne squeezed in the cat." . 1)
("The picture of Abrams arrived." . 1)
("Abrams wiped the table clean." . 1)
("Abrams put Browne in the garden." . 1) ; 71
("The dog will bark if Browne arrives." . 1)
("Abrams and Browne arrived." . 1)
("Abrams, Browne and the dog arrived." . 1)
("The dog arrived and barked." . 1)
("The dog arrived and Browne barked." . 1) ; 76
("The dog barked didn't it?" . 1)
("It is obvious that the dog barked." . 1)
("Abrams promised Browne to bark." . 1)
("Abrams seems to bark." . 1)
("Abrams believes Browne to be barking." . 1) ; 81
("It bothered Abrams that Browne barked." . 1)
("It took Abrams ten minutes to arrive." . 2)
("Abrams left it to Browne to bark." . 2)
("Abrams strikes Browne as old." . 1)
("Browne considers Abrams old." . 1) ; 86
("Abrams liked the idea that Browne could bark." . 1)
("Abrams barked from ten to three." . 1)
("Abrams was very old." . 1)
("Nearly every dog barked." . 1)
("Abrams barked very softly." . 1) ; 91
("Browne's chasing of cats bothered Abrams." . 1)
("It bothered Browne that Abrams chased cats." . 1)
("June third arrived." . 1)
("Abrams arrived at three twenty." . 1)
("Browne arrived on Tuesday morning." . 1) ; 96
("The cats found a way to bark." . 5)
("The happier dog chased Browne." . 1)
("There were cats in the garden." . 1)
("That dog chased Browne." . 1)
("Somebody chased Abrams." . 1) ; 101
("How happy was Abrams?" . 1)
("The number five bothers Browne." . 1)
("Abrams could." . 1)
("Browne tried to." . 1)
("Don't bark!" . 1) ; 106
("The dog arrived barking." . 1)
;;; new test suite items after here
("The dog liked barking." . 2)		; contrast with `kept barking'
("The dog barks if Browne arrives." . 1) ; variant on 72 without `will'

("Abrams bet Browne a cigarette that it barked." . 1) ; variant on 7 and 8
("Abrams knew that it barked." . 1)	; because we can't get the expl it 
("The dog is the cat." . 1)		; identity form of be
("The dog has the cat." . 1)            ; non aux have
("The dog does the cat." . 1)           ; non aux do 
("The dog was chased." . 1)             ; passive without `by'

))


#|
(defun output-sem-test-suite nil
  (with-open-file (ostream "semtest-erg.rmrs" :direction :output
                   :if-exists :supersede)
    (output-rmrs-xml-file-header ostream :standard)
    (let ((num 1))
      (dolist (eg *rmrs-test-suite*)
        (let ((sentence (car eg))
              (parsenum (cdr eg)))
          (format ostream "~%<S id='~A'>" num)
          (setf num (+ 1 num))
          (if sentence
              (format ostream
                      "~%<string>~%~S~%</string>" sentence)
            (format ostream
                    "~%<string></string>"))
          (format ostream
                  "~%<tree></tree>")
    ;;; for rasp output compatibility
          (let ((rmrs (rmrs-for-sentence sentence parsenum)))
            (if rmrs
                (mrs::output-rmrs1 rmrs 'mrs::xml ostream)
              (format ostream
                      "~%<rmrs></rmrs>")))
          (format ostream "</S>~%")))
      (output-rmrs-xml-file-end ostream :standard)
      (finish-output ostream))))
|#

#|

;;; Temporary
;;; FIX this to read from ERG file?

(defun compare-eg (egnum strpos-p)
  (let* ((eg (nth (- egnum 1)
		  *rmrs-test-suite*))
	 (input (car eg))
	 (parse-number (cdr eg))
         (rasp-rmrs 
          (nth (- egnum 1)
               (mrs::read-rmrs-file "rmrs/rasp3/annlt.rmrs" :rasp)))
	 (erg-rmrs
	  (rmrs-for-sentence input parse-number)))
    (dolist (comparison-record (mrs::compare-rmrs erg-rmrs rasp-rmrs strpos-p))
      ;;; (pprint comparison-record)
      (lkb::show-mrs-rmrs-compare-window erg-rmrs rasp-rmrs 
				    comparison-record input))))

;;; test cases for making sure the comparison code is doing what it's
;;; expected to

(defparameter *comp-test-suite*
    '(
      ("It rained." 1 "It rained." 1)
      ("Abrams barked." 1 "Browne barked." 1)
      ("The cat chased the cat." 1 "The cat chased the cat." 1)
      ("The cat chased the cat." 1 "The cat chased the dog." 1)
      ("The dog chased the cat." 1 "The cat chased the dog." 1)
      ("The big dog chased the cat." 1 "The big cat chased the dog." 1)
      ("The cat barked." 1 "The dog barked." 1)
      ("The fat cat barked." 1 "The fat dog barked." 1)))


(defun test-eg (egnum same-source-p)
  (let* ((eg (nth (- egnum 1)
		  *comp-test-suite*))
	 (input1 (first eg))
	 (parse-number1 (second eg))
	 (input2 (third eg))
	 (parse-number2 (fourth eg)) 
	 (rmrs1 (rmrs-for-sentence input1 parse-number1))
	 (rmrs2 (rmrs-for-sentence input2 parse-number2)))
    (dolist (comparison-record 
		(time
		(mrs::compare-rmrs rmrs1 rmrs2 same-source-p)))
      (show-mrs-rmrs-compare-window rmrs1 rmrs2
				    comparison-record 
				    (concatenate 'string input1 " / " input2)))))
    

|#

#|

;;; qa question preparation

0) null .tsdbrc

1) Remove Ersatzing from preprocessing

2) Parse the QA questions till satisfied and Annotate trees
(make sure to set the Process | Variables Chart to a safe size)

If need to update with existing parse selection - middle button on old (gold),
select new and `Trees Update' (setting Trees | Switches | Automatic Update
off, perhaps)

3) Set the LKB to *recording-word* mode

4) define output-rmrs-from-itsdb as below

5) add the following to redwoods.lisp export-tree

(when (smember :qa *redwoods-export-values*)
          (mrs::output-rmrs-from-itsdb
           (+ parse-id offset) 
           (or (get-field :o-input item) (get-field :i-input item))
           mrs))

(setf tsdb::*redwoods-export-values* '(:qa))

6) Trees Switches Thinning Export should be set

7) Trees Export

|#

#|
(defun output-rmrs-from-itsdb (i-id sentence mrs)
  (let* ((rmrs (mrs::mrs-to-rmrs mrs))
         (filename (format nil "q~A.rmrs" i-id)))
  (with-open-file (ostream filename :direction :output
                       :if-exists :supersede)
        (output-header-blah ostream)
        (format ostream "~%<S id='~A'>" i-id)
        (if sentence
            (format ostream
                    "~%<string>~%~S~%</string>" sentence)
          (format ostream
                  "~%<string></string>"))
        (format ostream
                "~%<tree></tree>")
    ;;; for rasp output compatibility
        (if rmrs
            (mrs::output-rmrs1 rmrs 'mrs::xml ostream)
          (format ostream
                  "~%<rmrs></rmrs>"))
        (format ostream "</S>")
        (output-end-blah ostream)
        (finish-output ostream))
  (excl::shell 
   (concatenate 'string
     "xmlnorm -Vs " filename " 2>| " (format nil "q~A.errors" i-id)))))

(defun output-header-blah (ostream)
  (format ostream
"<?xml version='1.0'?> 
<!DOCTYPE CORPUS SYSTEM \"/usr/groups/mphil/qa05/dtd/analysis.dtd\" > 
<CORPUS> 
<DOC> 
<DOCNO/>
<TEXT>
<P>"))

(defun output-end-blah (ostream)
  (format ostream
"</P></TEXT></DOC></CORPUS>~%"))

|#

;;; tsg15 testing

#|

(rasp3-out
	 (make-pathname 
	     :device "c"
	     :directory "/d/rasp-rmrs/test-sets/"
	     :name "annlt.trees")
	 (make-pathname 
	     :device "c"
	     :directory "/d/rasp-rmrs/test-sets/"
	     :name "annlt.rmrs"))

(rasp3-out
	 (make-pathname 
	     :device "c"
	     :directory "/d/rasp-rmrs/test-sets/"
	     :name "me0196.trees")
	 (make-pathname 
	     :device "c"
	     :directory "/d/rasp-rmrs/test-sets/"
	     :name "me0196.rmrs"))
	 
	 
	 
	 
;;;	 (make-pathname 
;;;	  :directory "/homes/aac10/rasp-rmrs/test-sets/"
;;;	  :name "robust.trees")
	


(defun rasp3-out (ifile ofile)
  (let ((*rasp-rmrs-gram-file*
	 "rmrs/rasp3/gram15-general.rmrs")
	(*rasp-rmrs-tag-file*
	 "rmrs/rasp3/lex15.rmrs")
	(*rasp-xml-word-p* t)
	(*renumber-hack* t)
	(*rasp-xml-type* :none))
    (setf *algebra-rule-instructions* nil)
    (setf *algebra-tag-instructions* nil)
    (clear-rule-record)
    (read-rmrs-grammar *rasp-rmrs-gram-file*)
    (read-rmrs-tag-templates *rasp-rmrs-tag-file*)
    (with-open-file  (istream ifile
		      :direction :input)
    (with-open-file  (ostream ofile
		      :direction :output :if-exists :supersede)
      (format ostream "<rmrs-list>~%")
      (loop 
	(let* ((tagged (read istream nil nil))
	       (number (read istream nil nil))
	       (tree (read istream nil nil)))
	  (declare (ignore number))
	  (unless tree
	    (return))
	  (when tree
;;;	    (pprint tree)
	    (construct-sem-for-tree 
	     tree
	     :rasp ostream tagged))))
      (format ostream "</rmrs-list>~%")))))


|#

#| 

algebra testing

1. rule conversion

(progn 
(let ((*rasp-rmrs-gram-file*
	 "rmrs/rasp3/gram15-general.rmrs")
	(*rasp-xml-word-p* t)
	(*renumber-hack* t)
	(*rasp-xml-type* :none))
    (clear-rule-record)
    (read-rmrs-grammar *rasp-rmrs-gram-file*))



(dolist (rulename '("S/np_vp" "NP/det_n1" "N1/ap_n1" "AP/a1" "A1/a"))
  (let ((rule (find rulename 
		    *rule-instructions* :key #'rmrs-rule-name :test #'equal)))
    (when rule (convert-rmrs-rule rule))))

(show-algebra-rules)
)


(convert-rmrs-rule (find "S/np-nt-adv_s" *rule-instructions* :key #'rmrs-rule-name :test #'equal))

(convert-rmrs-rule (find "S/np_vp" *rule-instructions* :key #'rmrs-rule-name :test #'equal))

(convert-rmrs-rule (find "V1/v_inf" *rule-instructions* :key #'rmrs-rule-name :test #'equal))

(setf *anchor-rmrs-p* nil)

(rasp3-out "rmrs/rasp3/single.trees" "foo1")

(setf *anchor-rmrs-p* t)

(rasp3-out "rmrs/rasp3/single.trees" "foo2.rmrs")

(rasp3-out
	 (make-pathname 
	     :device "c"
	     :directory "/d/rasp-rmrs/test-sets/"
	     :name "annlt.trees")
	 
	 "annlt.rmrs")

(rasp3-out "rmrs/rasp3/selected.trees" "rmrs/rasp3/annlt.rmrs")

|#

#|

(defun process-citation-files nil
  ;;; clear and load the grammars
  (let ((*rasp-xml-word-p* t)
	(*anchor-rmrs-p* t)
	(*rasp-rmrs-gram-file*
	 "rmrs/rasp3/gram15-general.rmrs")
	(*rasp-rmrs-tag-file*
	 "rmrs/rasp3/lex15.rmrs")
	(*renumber-hack* nil))
   (setf *algebra-rule-instructions* nil)
   (setf *algebra-tag-instructions* nil)
   (setf *initial-rasp-num* nil)
 (clear-rule-record)
 (read-rmrs-grammar *rasp-rmrs-gram-file*)
 (read-rmrs-tag-templates *rasp-rmrs-tag-file*)
 (let* ((ifiles
         (directory "~/citation/xml-parsed/*")))
    (loop for ifile in ifiles
        do
          (let* ((namestring (file-namestring ifile)))
            (format t "~%Processing file ~A" namestring)
	    (excl::shell 
	     (concatenate 
		 'string "gunzip -c < " 
		 "~/citation/xml-parsed/"
                    namestring "> /tmp/pfile"))
              ; (format t "~%File ~A unpacked" namestring)
	    (let ((new-file (concatenate 'string 
			      "~/citation/xml-reparsed3/"
			      (subseq namestring
				      0 (- (length namestring) 3)
				      )))
		  (err-file (concatenate 'string  
			      "~/citation/xml-errs/"
			      namestring)))
	      (declare (ignore err-file))
	      (with-open-file  (istream "/tmp/pfile"
				:direction :input)
		(with-open-file  (ostream new-file
				  :direction :output :if-exists :supersede)
		  (let ((in-rmrs-p nil)
			(in-tree-p nil)
			(tree-lines nil))
		  (loop 
		    (let ((new-line (read-line istream nil nil)))
		      (unless new-line (return))
		      (cond ((line-matches new-line "<PARSE>")
			     (setf in-tree-p t)
			     (write-line new-line ostream))
			    ((line-matches new-line "*** Error")
			     ;;; ignore errors in parse
			     (write-line new-line ostream))
			    ((line-matches new-line "</PARSE>")
			     (setf in-tree-p nil)
			     (write-line new-line ostream)
			     (finish-output ostream)
			     (handler-case
				 (progn
				   (format ostream "<rmrs-list>~%")
				   (construct-sem-for-tree 
				    (read-from-string 
				     (apply #'concatenate 'string 
					    (nreverse tree-lines)))
				    :rasp ostream)
				   (format ostream "</rmrs-list>~%")
				   (finish-output ostream))
			       (storage-condition (condition)
				 (progn
				   (format ostream "~%<!-- Memory allocation problem: ~A -->" condition)
				   (format ostream "~%</rmrs-list>~%")))
			       (error (condition)
				 (progn
				   (format ostream "~%<!-- Error: ~A -->" condition)
				   (format ostream "~%</rmrs-list>~%")))
			       (serious-condition (condition)
				 (progn
				   (format ostream "~%<!-- Something nasty: ~A -->" condition)
				   (format ostream "~%</rmrs-list>~%")
				      
				   ))))
			    ((line-matches new-line "<RMRS></RMRS>")
			     (setf in-rmrs-p nil))
			    ((line-matches new-line "<RMRS>")
			     (setf in-rmrs-p t))
			    ((line-matches new-line "</RMRS>")
			     (setf in-rmrs-p nil))
			    ((line-matches new-line "</rmrs-list>")
			     (setf in-rmrs-p nil))
			    (t 
			     (when in-tree-p
			       (push new-line tree-lines))
			     (unless in-rmrs-p
			       (write-line new-line ostream)))))))))
	      (excl::shell "rm /tmp/pfile")
	      (when (probe-file new-file)
		  ;;; validate the XML
;;;                  (excl::shell 
;;;                   (concatenate 'string
;;;                     "xmlnorm -Vs " new-file " 2>| " err-file))
                  ;;; note we're redirecting std err
		  ;;; gzip the file
		(excl::shell (concatenate 'string "gzip " 
					  new-file)))))))))

(defun line-matches (new-line match)
  (let ((ml (length match)))
    (and new-line
	 (>= (length new-line) ml)
	 (string-equal (subseq new-line 0 ml) match))))

|#