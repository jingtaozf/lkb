;; Copyright (c) 2003--2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.
;;;
;;; Temporary expedient until we can link up with the fine system

(in-package :mrs)


;;; ************* Temporary !!!! ************************

(defparameter *rmrs-test-suite*
'(    
("It rained" . 1) ; 1
("Abrams barked" . 1) 
("The window opened" . 1) 
("Abrams chased Browne" . 1) 
("Abrams handed Browne the cigarette" . 1) ; 5
("Abrams handed the cigarette to Browne" . 1) 
("Abrams bet Browne a cigarette that it rained" . 1) 
("Abrams knew that it rained" . 1) 
("Abrams intended to bark" . 1) 
("Abrams intended Browne to bark" . 1) 
("Every cat barked" . 1) ; 11
("Every cat chased some dog" . 1)
("My cat barked" . 1)
("It barked" . 1)
("The cat chased it" . 1)
("The cat chased itself" . 1) ; 16
("The cat chased one" . 1)
("Mine barked" . 1)
("That opened" . 1)
("Cats bark" . 1)
("Tobacco arrived" . 1) ; 21
("Some bark" . 1)
("Some of the cats bark" . 1)
("No cat barked" . 1)
("Did the dog bark" . 1)
("Which dog barked" . 1) ; 26
("Whose dog barked" . 1)
("Chase Browne" . 1)
("Abrams wondered which dog barked" . 1)
("Abrams wondered whether Browne barked" . 1)
("The dog that Browne chased barked" . 2) ; 31
("The dog to chase is barking" . 1)
("The dog was chased by Browne" . 1)
("The dog chased by Browne barked" . 2)
("The dog is barking" . 1)
("The dog has barked" . 1) ; 36
("The dog has been barking" . 1)
("The dog had been barking" . 1)
("The dog will bark" . 1)
("The dog is going to bark" . 1)
("The dog could bark" . 1) ; 41
("The dog couldn't bark" . 1) ; 
("The old dog barked" . 1)
("The dog barked softly" . 1)
("The dog probably barked" . 1)
("The dog barked in the garden" . 1) ; 46
("The dog barks now" . 1)
("The garden dog barked" . 1)
("The tobacco garden dog barked" . 2)
("The cat is old" . 1)
("The cat is in the garden" . 1) ; 51
("The barking dog chased Browne" . 1)
("Chased dogs bark" . 1)
("Chasing the cat is old" . 1)
("That the cat chases Browne is old" . 1)
("Dogs chase whatever barks" . 1) ; 56
("The dog barked every day" . 1)
("When did the dog bark" . 3)
("Three of the dogs bark" . 1)
("Three bark" . 1)
("Browne's dog barks" . 1) ; 61
("Browne's barks" . 2)
("Twenty three dogs bark" . 1)
("Two hundred twenty dogs bark" . 1)
("Abrams arrived by car" . 1)
("Abrams kept barking" . 1) ; 66
("Browne squeezed the cat in" . 1)
("Browne squeezed in the cat" . 1)
("The picture of Abrams arrived" . 1)
("Abrams wiped the table clean" . 1)
("Abrams put Browne in the garden" . 1) ; 71
("The dog will bark if Browne arrives" . 1)
("Abrams and Browne arrived" . 1)
("Abrams, Browne and the dog arrived" . 1)
("The dog arrived and barked" . 1)
("The dog arrived and Browne barked" . 1) ; 76
("The dog barked didn't it" . 1)
("It is obvious that the dog barked" . 1)
("Abrams promised Browne to bark" . 1)
("Abrams seems to bark" . 1)
("Abrams believes Browne to be barking" . 1) ; 81
("It bothered Abrams that Browne barked" . 1)
("It took Abrams ten minutes to arrive" . 2)
("Abrams left it to Browne to bark" . 2)
("Abrams strikes Browne as old" . 1)
("Browne considers Abrams old" . 1) ; 86
("Abrams liked the idea that Browne could bark" . 1)
("Abrams barked from ten to three" . 1)
("Abrams was very old" . 1)
("Nearly every dog barked" . 1)
("Abrams barked very softly" . 1) ; 91
("Browne's chasing of cats bothered Abrams" . 1)
("It bothered Browne that Abrams chased cats" . 1)
("June third arrived" . 1)
("Abrams arrived at three twenty" . 1)
("Browne arrived on Tuesday morning" . 1) ; 96
("The cats found a way to bark" . 5)
("The happier dog chased Browne" . 1)
("There were cats in the garden" . 1)
("That dog chased Browne" . 1)
("Somebody chased Abrams" . 1) ; 101
("How happy was Abrams" . 1)
("The number five bothers Browne" . 1)
("Abrams could" . 1)
("Browne tried to" . 1)
("Don't bark" . 1) ; 106
("The dog arrived barking" . 1)))


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

(defun rasp-semtest-out nil
  (let ((*rasp-rmrs-gram-file*
	 ;; (make-pathname 
	 ;; :device "d"
	 ;; :directory "/lingo/lkb/src/rmrs/annlt-test/"
	 ;; :name "gram14.1.rmrs")
	(make-pathname 
	:directory "/homes/aac10/lingo/lkb/src/rmrs/annlt-test/"
	:name "gram14.1.rmrs")
	)
	(*rasp-rmrs-tag-file*
	 ;;(make-pathname :device "d" :directory "/lingo/lkb/src/rmrs/annlt-test/"
	;;		:name "lex14.1.rmrs")
	(make-pathname :directory "/homes/aac10/lingo/lkb/src/rmrs/annlt-test/"
	:name "lex14.1.rmrs")
	)
	(*rasp-xml-word-p* t)
	(*rasp-xml-type* :none)
	(*rasp-input-file*
	 ;; (make-pathname :device "d" 
	;;		:directory "/lingo/lkb/src/rmrs/annlt-test/"
	;;		:name "semtest.rasp")
	 (make-pathname :directory "/homes/aac10/lingo/lkb/src/rmrs/annlt-test/"
	       :name "semtest.rasp")
	 )
	(*rasp-rmrs-output-file*
	 "semtest-rasp.rmrs"))
    (simple-process-rasp-file)))



;;; Temporary
;;; FIX this to read from ERG file?

(defun compare-eg (egnum strpos-p)
  (let* ((eg (nth (- egnum 1)
		  *rmrs-test-suite*))
	 (input (car eg))
	 (parse-number (cdr eg))
         (rasp-rmrs 
          (nth (- egnum 1)
               (mrs::read-rmrs-file "semtest-rasp.rmrs" :rasp)))
	 (erg-rmrs
	  (rmrs-for-sentence input parse-number)))
    (dolist (comparison-record (mrs::compare-rmrs erg-rmrs rasp-rmrs strpos-p))
      (lkb::show-mrs-rmrs-compare-window erg-rmrs rasp-rmrs 
				    comparison-record input))))

;;; older than above
(defun compare-eg (egnum)
  (let* ((erg-mrs 
	 (nth (- egnum 1)
	      (mrs::read-mrss-from-file "rmrs/annlt-test/semtest.erg")))
	(rasp-rmrs 
	 (nth (- egnum 1)
	      (mrs::read-rmrs-file "semtest.rmrs" :rasp)))
       (erg-rmrs
	      (mrs::mrs-to-rmrs erg-mrs)))
    (dolist (comparison-record (mrs::compare-rmrs erg-rmrs rasp-rmrs t))
      (show-mrs-rmrs-compare-window erg-rmrs rasp-rmrs 
				    comparison-record (format nil "~A" egnum)))))
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

4) define output-rmrs-from-fine-system as below

5) add the following to redwoods.lisp export-tree

(when (smember :qa *redwoods-export-values*)
          (mrs::output-rmrs-from-fine-system
           (+ parse-id offset) 
           (or (get-field :o-input item) (get-field :i-input item))
           mrs))

(setf tsdb::*redwoods-export-values* '(:qa))

6) Trees Switches Thinning Export should be set

7) Trees Export

|#

#|
(defun output-rmrs-from-fine-system (i-id sentence mrs)
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

(defun rasp3-out nil
  (let ((*rasp-rmrs-gram-file*
	(make-pathname 
	:directory "/homes/aac10/rasp-rmrs/"
	:name "gram15.rmrs")
	)
	(*rasp-rmrs-tag-file*
	(make-pathname :directory "/homes/aac10/rasp-rmrs/"
	:name "lex15.rmrs")
	)
	(*rasp-xml-word-p* t)
	(*rasp-xml-type* :none))
    (clear-rule-record)
    (read-rmrs-grammar *rasp-rmrs-gram-file*)
    (read-rmrs-tag-templates *rasp-rmrs-tag-file*)
    (construct-sem-for-tree '(|T/txt-sc1/-+|
 (|S/np_vp|
  (|NP/n1-name| (|N1/n-name| |<w s='2' e='7'>Abrams_NP1</w>|))
  (|V1/v| |<w s='9' e='14'>bark+ed_VVD</w>|))
 (|End-punct3/-| |<w s='15' e='15'>._.</w>|)) :rasp t "test")))


|#

