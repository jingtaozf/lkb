;; Copyright (c) 2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

;;; RMRS windows

;;;    RMRS comparison - to-do
;;;
;;;    put on a menu somewhere 
;;;    ("RMRS compare" :value rmrs-compare :active ,*mrs-loaded*)
;;; 
;;;    (rmrs-compare (funcall 'show-mrs-rmrs-compare-window rmrs1 rmrs2 
;;;      comparison-record title))
;;;

(define-lkb-frame mrs-rmrs
    ((mrsstruct :initform nil
                :accessor mrs-rmrs-mrsstruct)
     (rmrs :initform nil
                :accessor mrs-rmrs-rmrs))
  :display-function 'show-mrs-rmrs  
  :width *parse-window-width* 
  :height *parse-window-height*)

;;; new compare functionality

(define-lkb-frame mrs-rmrs-compare
    ((rmrs1 :initform nil
	    :accessor mrs-rmrs-compare-rmrs1)
     (rmrs2 :initform nil
	    :accessor mrs-rmrs-compare-rmrs2)
     (comparison-record :initform nil
	    :accessor mrs-rmrs-compare-comparison-record))
  :display-function 'show-mrs-rmrs-compare
  :width (* 2 *parse-window-width*) 
  :height *parse-window-height*)

(defun show-mrs-rmrs-window (edge &key mrs rmrs title)
  (mp:run-function "RMRS"
   #'show-mrs-rmrs-window-really edge :mrs mrs :rmrs rmrs :title title))
  
(defun show-mrs-rmrs-window-really (edge &key mrs rmrs title)
  (let ((mframe (clim:make-application-frame 'mrs-rmrs))
        (mrsstruct (or mrs (when edge (mrs::extract-mrs edge)))))
    (setf (mrs-rmrs-mrsstruct mframe) 
      mrsstruct)
    (setf (mrs-rmrs-rmrs mframe) 
      (or rmrs (mrs::mrs-to-rmrs mrsstruct)))
    (setf (clim:frame-pretty-name mframe) (or title "Robust MRS"))
    (clim:run-frame-top-level mframe)))

(defun show-mrs-rmrs-compare-window (rmrs1 rmrs2 comparison-record title)
    (mp:run-function "RMRS comparison" 
		     #'show-mrs-rmrs-compare-window-really 
		     rmrs1 rmrs2 comparison-record title))

(defun show-mrs-rmrs-compare-window-really (rmrs1 rmrs2 comparison-record
					    title)
  (let ((mframe (clim:make-application-frame 'mrs-rmrs-compare)))
    (setf *normal* (clim:parse-text-style (make-active-fs-type-font-spec)))
    (setf *bold* (clim:merge-text-styles '(nil :bold nil) *normal*))
    (setf (mrs-rmrs-compare-rmrs1 mframe) 
      rmrs1)
    (setf (mrs-rmrs-compare-rmrs2 mframe) 
      rmrs2)
    (setf (mrs-rmrs-compare-comparison-record mframe) 
      comparison-record)
    (setf (clim:frame-pretty-name mframe) (or title "RMRS comparison"))
    (clim:run-frame-top-level mframe)))


(defun show-mrs-rmrs (mframe stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let ((rmrs (mrs-rmrs-rmrs mframe)))
    (if rmrs
      (clim:with-text-style (stream (lkb-parse-tree-font))
        (mrs::output-rmrs1 rmrs 'mrs::compact stream t))
      (format stream "~%::: RMRS structure could not be extracted~%"))))

(defun show-mrs-rmrs-compare (mframe stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let ((rmrs1 (mrs-rmrs-compare-rmrs1 mframe))
	(rmrs2 (mrs-rmrs-compare-rmrs2 mframe))
	(comparison-record (mrs-rmrs-compare-comparison-record mframe)))
    (if (and rmrs1 rmrs2 comparison-record)
        (clim:with-text-style (stream *normal*)
	  (clim:with-output-recording-options (stream :draw nil :record t)
	    (let  
		((ep-pts1 (mrs::output-rmrs1 rmrs1 
					     'mrs::compact-chars stream t)))
	      (move-to-x-y stream 0 0)
	      (let ((ep-pts2
		     (mrs::output-rmrs1 rmrs2 'mrs::compact-two stream t)))
		(add-comparison-pointers stream ep-pts1 ep-pts2 
					 comparison-record)))))
      (format stream "~%::: RMRS structures could not be extracted~%"))))

(defun add-comparison-pointers (stream ep-pts1 ep-pts2 comparison-record)
  (when (and ep-pts1 ep-pts2 comparison-record)
    (loop for match in (mrs::rmrs-comparison-record-matched-rels comparison-record)
	do
	  (let* ((ep1 (mrs::match-rel-record-rel1 match))
		 (found-ep1 (find ep1 ep-pts1 :key #'mrs::rmrs-position-ep))
		 (ep2 (mrs::match-rel-record-rel2 match))
		 (found-ep2 (find ep2 ep-pts2 :key #'mrs::rmrs-position-ep)))
	    (when (and found-ep1 found-ep2)
	      (let ((ep1-pos (mrs::rmrs-position-position found-ep1))
		    (ep2-pos (mrs::rmrs-position-position found-ep2)))
		(clim:draw-line* 
		 stream
		 (+ (position-x ep1-pos) 10)
		 (+ (position-y ep1-pos) 10)
		 (- (position-x ep2-pos) 10)
		 (+ (position-y ep2-pos) 10)
		 :ink (determine-comparison-line-colour
		       (mrs::match-rel-record-pred-comp-status match)
		       (mrs::match-rel-record-var-comp-status match)))))))))
		       

(defun determine-comparison-line-colour (pred-comp-type var-comp-type)
  ;;; if we assume that the deep-grammar derived RMRS is
  ;;; in position 1, then the :sub2 and :comp cases are
  ;;; generally unexpected - hence use red/magenta for these
  (declare (ignore var-comp-type))
  (ecase pred-comp-type
    (:equal +green-flipping-ink+)
    (:sub1 +blue-flipping-ink+)
    (:sub2 +red-flipping-ink+)
    (:comp +magenta-flipping-ink+)))
    
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
("Every cat chased some dog." . 1)
("My cat barked." . 1)
("It barked." . 1)
("The cat chased it." . 1)
("The cat chased itself." . 1) ; 16
("The cat chased one." . 1)
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
("The dog that Browne chased barked." . 2) ; 31
("The dog to chase is barking." . 2)
("The dog was chased by Browne." . 1)
("The dog chased by Browne barked." . 2)
("The dog is barking." . 1)
("The dog has barked." . 1) ; 36
("The dog has been barking." . 1)
("The dog had been barking." . 1)
("The dog will bark." . 1)
("The dog is going to bark." . 1)
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
("When did the dog bark?" . 3)
("Three of the dogs bark." . 2)
("Three bark." . 1)
("Browne's dog barks." . 1) ; 61
("Browne's barks." . 2)
("Twenty three dogs bark." . 1)
("Two hundred twenty dogs bark." . 1)
("Abrams arrived by car." . 1)
("Abrams kept barking." . 1) ; 66
("Browne squeezed the cat in." . 1)
("Browne squeezed in the cat." . 1)
("The picture of Abrams arrived." . 1)
("Abrams wiped the table clean." . 1)
("Abrams put Browne in the garden." . 1) ; 71
("The dog will bark if Browne arrives." . 1)
("Abrams and Browne arrived." . 1)
("Abrams, Browne and the dog arrived." . 3)
("The dog arrived and barked." . 1)
("The dog arrived and Browne barked." . 1) ; 76
("The dog barked, didn't it?" . 1)
("It is obvious that the dog barked." . 1)
("Abrams promised Browne to bark." . 1)
("Abrams seems to bark." . 1)
("Abrams believes Browne to be barking." . 1) ; 81
("It bothered Abrams that Browne barked." . 1)
("It took Abrams ten minutes to arrive." . 2)
("Abrams left it to Browne to bark." . 1)
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
("The number five bothers Browne." . 2)
("Abrams could." . 1)
("Browne tried to." . 1)
("Don't bark!" . 1) ; 106
("The dog arrived barking." . 1)))


(defun compare-eg (egnum)
  (let* ((eg (nth (- egnum 1)
		  *rmrs-test-suite*))
	 (input (car eg))
	 (parse-number (cdr eg))
	(rasp-mrs 
	 (nth (- egnum 1)
	      (mrs::read-rmrs-file "semtest.rmrs" :rasp)))
	 (erg-mrs
	  (rmrs-for-sentence input parse-number)))
    (dolist (comparison-record (mrs::compare-rmrs erg-mrs rasp-mrs t input))
      (show-mrs-rmrs-compare-window erg-mrs rasp-mrs 
				    comparison-record input))))

#|

(defun compare-eg (egnum)
  (let* ((erg-mrs 
	 (nth (- egnum 1)
	      (mrs::read-mrss-from-file "rmrs/annlt-test/semtest.erg")))
	(rasp-rmrs 
	 (nth (- egnum 1)
	      (mrs::read-rmrs-file "semtest.rmrs" :rasp)))
       (erg-rmrs
	      (mrs::mrs-to-rmrs erg-mrs)))
    (dolist (comparison-record (mrs::compare-rmrs erg-rmrs rasp-rmrs t 
						  (format nil "~A" egnum)))
      (show-mrs-rmrs-compare-window erg-rmrs rasp-rmrs 
				    comparison-record (format nil "~A" egnum)))))

|#

;;; test cases for making sure the comparison code is doing what it's
;;; expected to

(defparameter *comp-test-suite*
    '(
      ("It rained." 1 "It rained." 1)
      ("Abrams barked." 1 "Browne barked." 1)
      ("The cat chased the cat." 1 "The cat chased the cat." 1)
      ("The cat chased the cat." 1 "The cat chased the dog." 1)
      ("The dog chased the cat." 1 "The cat chased the dog." 1)
      ("The big dog chased the cat." 1 "The big cat chased the dog." 1)))


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
		(mrs::compare-rmrs rmrs1 rmrs2 same-source-p 
				   (concatenate 'string input1 " / " input2))))
      (show-mrs-rmrs-compare-window rmrs1 rmrs2
				    comparison-record 
				    (concatenate 'string input1 " / " input2)))))
    

(defun rmrs-for-sentence (input parse-number)
  (let ((*show-parse-p* nil))
    (do-parse-tty input)
    (unless *parse-record*
      (error "Parse failed"))
    (let ((selected-parse (nth (- parse-number 1) *parse-record*)))
      (unless selected-parse
	(error "Incorrect parse number"))
      (let ((mrs (mrs::extract-mrs selected-parse)))
	(unless mrs (error "~%Can't extract MRS"))
	(mrs::mrs-to-rmrs mrs)))))
