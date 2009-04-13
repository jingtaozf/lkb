;;; Copyright (c) 2009
;;;   Ann Copestake
;;;   see `licence.txt' for conditions.

(in-package :mrs)

;;; Anaphora on DMRS


#|

A sequence of MRSs is regarded as a discourse.  We add a new structure
discourse-item, to add extra information needed when treating a structure
as part of a discourse.  Anaphora resolution consists of building up a 
discourse model with possible referents, items assumed to be anaphoric 
 (a subset of the referents) and anaphoric links between an anaphoric item 
 and a referent.

 The data structures and general idea will work with MRS or RMRS too, but
 for this code, we assume DMRSs

Stages in building the model

0) turn a sequence of DMRSs into a discourse model - i.e., a sequence of 
discourse items
1) extract the referents
2) extract the anaphoric items
3) add a-links

|#


(defstruct discourse-item
  utterance-id
  mrs ; may be an mrs, rmrs or dmrs
  referents ; some are marked as anaphoric
  a-links  ;; from anaphora in this discourse-item to referents
  ;; which may be in other items.  Possibly should be at a higher 
  ;; level. 
  )

(defstruct referent
  d-id
  anaphoric-p ; t if anaphor
  node	   ;; cache
  features ;; cache features
  )

(defstruct rfeatures
  pronoun
  reflexive
  definite
  natural-gender
  pair
  animacy
  main-clause
  argnum
  )

(defstruct a-link
  anaphor    ; d-id
  referent ; d-id
  score ; integer value
  in ;; boolean value - t means this link is being counted in this
  ;; model, nil that it isn't
  features ;; cache features to simplify reranking
  )

(defstruct lfeatures
  sentence-distance ; 0 for same sentence 
  token-distance
  )

(defstruct d-id
  ;; dual part id so referents are unique
  id
  utterance-id
  )

#|

(defstruct (dmrs)
  nodes
  links)

(defstruct dmrs-node
  ;;; the LTOP node is always id 0, others may be anything - but
  ;;; code for conversion from RMRS uses the anchors.
  ;;; cfrom and cto as in (R)MRS
  ;;; charvar and label are for convenience in processing
  ;;; char var is the variable characteristic of the RMRS predication
  ;;; (always the ARG0, but not all ARG0s are charvars)
  ;;; label is the label of the RMRS predication
  id
  pred
  cfrom
  cto
  carg
  charvar ;; for RMRS to DMRS only
  label ;; for RMRS to DMRS only
  )

(defstruct dmrs-link
  ;;; from and to are DMRS node ids, pre and post make up the arc
  ;;; label
  from to pre post)

|#

(defun dmrs-list-to-discourse (dmrs-list)
  (let ((discourse-base (convert-dmrs-list dmrs-list)))
    (extract-dmrs-referents discourse-base)
    ;; sets referents
    (extract-dmrs-anaphora discourse-base)
    ;; sets anaphora
    (add-a-links discourse-base)
    discourse-base))

;;; Step 0 - take a sequence of DMRSs and convert it to a series
;;; of discourse items

(defun convert-dmrs-list (dmrs-list)
  (loop for item in dmrs-list
      and item-number from 1
      collect
	(make-discourse-item
	 :utterance-id item-number
	 :mrs item
	 )))

;;; Step 1 - extract referents

(defun extract-dmrs-referents (discourse-items)
  (dolist (discourse-item discourse-items)
    (let ((utterance-id (discourse-item-utterance-id discourse-item)))
      (setf (discourse-item-referents discourse-item)
	(loop for node in (dmrs-nodes (discourse-item-mrs discourse-item))
	    when (potential-referent-p node)
	    collect 
	      (make-referent 
	       :d-id (make-d-id :id 
				(dmrs-node-id node)
				:utterance-id utterance-id)
	       :node node))))))

(defun potential-referent-p (node)
  ;;; type is individual
  ;;; FIX
  (equal (dmrs-node-cvtype node) "x"))

;;; Step 2 - extract anaphoric items

(defun extract-dmrs-anaphora (discourse-items)
  (dolist (discourse-item discourse-items)
    (let* ((referents (discourse-item-referents discourse-item)))
      (loop for referent in referents
	  when (anaphoric-p (referent-node referent))
	  collect 
	    (setf (referent-anaphoric-p referent) t)))))


(defun anaphoric-p (node)
  ;;; pronoun, definite etc
  ;;; FIX
  (let ((pred (dmrs-node-pred node)))
    (and (stringp pred)
	 (string-equal (dmrs-node-pred node) "pron_rel"))))


;;; Step 3 - add a-links

(defun add-a-links (discourse-items)
  (let ((past-items nil))
    (dolist (discourse-item discourse-items)
      (dolist (referent (discourse-item-referents discourse-item))
	(when (referent-anaphoric-p referent)
	  (let ((new-links
		 (find-a-links referent discourse-item 
			       past-items)))
	    ;; forget cataphora in future sentences!
	    (setf (discourse-item-a-links discourse-item)
	      (append new-links
		      (discourse-item-a-links discourse-item))))))
      (push discourse-item past-items))))


(defun find-a-links (anaphor discourse-item past-items)
  ;;; FIX
  ;;; past-items are in reverse order, most recent first
  (append
   (loop for referent in (discourse-item-referents discourse-item)
       unless (or (eql referent anaphor)
		  (violates-binding-conditions 
		   anaphor referent discourse-item)
		  (referent-property-mismatch 
		   anaphor referent))
       collect (make-a-link :anaphor (referent-d-id anaphor)
			    :referent (referent-d-id referent)))
   ;;; don't look back if prontype='refl'
   (if (node-properties-p (referent-node anaphor)
			  "prontype" "refl")    
       nil
     (loop for past-item in past-items
	 append
	   (loop for referent in (discourse-item-referents past-item)
	       unless (referent-property-mismatch 
		       anaphor referent)
	       collect (make-a-link :anaphor (referent-d-id anaphor)
				    :referent (referent-d-id referent)))))))

(defun node-properties-p (node feature value)
  ;;; feature and value are specified as strings
  ;;; this function takes care of nastinesses like packages
  ;;; though not very efficiently
  (let ((properties (dmrs-node-cvextra node)))
    (dolist (property properties)
      (let ((f (extrapair-feature property))
	    (v (extrapair-value property)))
	(when 
	    (and (string-equal (string f) feature)
		 (string-equal (string v) value))
	  (return t))))))

(defun referent-property-mismatch (anaphor referent)
  ;;; FIX
  (declare (ignore anaphor referent))
  nil)

(defun violates-binding-conditions (anaphor referent discourse-item)
  ;;; this of course is indefinitely complex
  ;;; in the medium term, allow patterns to be specified in
  ;;; DMRS such that if the configuration matches the pattern
  ;;; we have a violation
  (if (node-properties-p (referent-node anaphor)
			 "prontype" "refl")
      nil 
    ;;; FIX!
    ;;; non-reflexive
    ;;; crudely block all cases where the elements are
    ;;; linked from the same node
    (let* ((dmrs (discourse-item-mrs discourse-item))
	   (alinked (collect-linked-from anaphor dmrs))
	   (rlinked (collect-linked-from referent dmrs)))
      (intersection alinked rlinked))))

(defun collect-linked-from (ref dmrs)
  (let ((ref-id (d-id-id (referent-d-id ref)))
	(links (dmrs-links dmrs)))
    (loop for link in links
	when (eql (dmrs-link-to link) ref-id)
	collect
	  (dmrs-link-from link))))
  
  
;;; *************************************************
;;;
;;; Output (as XML for now)
;;;
;;; *************************************************

#| 

<!ELEMENT discourse (discourse-item)*>

<!ELEMENT discourse-item (dmrs, (referent)*, (a-link)*) >
<!ATTLIST discourse-item
          utterance-id CDATA #REQUIRED >
|#
	  
(defun output-discourse (discourse ostream)
  (output-discourse-start discourse ostream)
  (dolist (discourse-item discourse)
    (output-discourse-item-start discourse-item ostream)
    (output-dmrs1 (discourse-item-mrs discourse-item) 'dxml ostream)
    (dolist (referent (discourse-item-referents discourse-item))
      (output-referent referent ostream))
    (dolist (alink (discourse-item-a-links discourse-item))
      (output-alink alink ostream))
    (output-discourse-item-end discourse-item ostream))
  (output-discourse-end discourse ostream))
  

(defun output-discourse-start (discourse ostream)
  (declare (ignore discourse))
  (write-string 
   "<?xml version='1.0'?> <!DOCTYPE discourse SYSTEM \"/homes/aac10/lingo/lkb/src/rmrs/discourse.dtd\" >" ostream)
  (terpri ostream)
  (write-string "<discourse>" ostream)
  )

(defun output-discourse-end (discourse ostream)
  (declare (ignore discourse))
  (terpri ostream)
  (write-string "</discourse>" ostream)
  (terpri ostream))


(defun output-discourse-item-start (discourse-item ostream)
  (terpri ostream)
  (write-string "<discourse-item utterance-id='" ostream)
  (princ (discourse-item-utterance-id discourse-item) ostream)
  (write-string "'>" ostream)
  )

(defun output-discourse-item-end (discourse-item ostream)
  (declare (ignore discourse-item))
  (write-string "</discourse-item>" ostream)
  )


#| 

<!ELEMENT referent EMPTY >
<!ATTLIST referent
          id CDATA #REQUIRED 
          utterance-id CDATA #REQUIRED 
	  anaphoric (plus|minus|u) #REQUIRED >

	  |#

(defun output-referent (referent ostream)
  (let ((d-id (referent-d-id referent)))
    (terpri ostream)
    (write-string "<referent id='" ostream)
    (princ (d-id-id d-id) ostream)
    (write-string "' utterance-id='" ostream)
    (princ (d-id-utterance-id d-id) ostream)
    (write-string "' anaphoric='" ostream)
    (princ (if (referent-anaphoric-p referent) "plus"
	     "minus") ostream)
    (write-string "'/>" ostream)))

#|

<!ELEMENT a-link EMPTY >
<!ATTLIST a-id CDATA #REQUIRED 
          a-uid CDATA #REQUIRED 
          r-id CDATA #REQUIRED 
          r-uid CDATA #REQUIRED >

|#
	  
(defun output-alink (alink ostream)
  (let ((anaphor (a-link-anaphor alink))
	(referent (a-link-referent alink)))
    (terpri ostream)
    (write-string "<a-link a-id='" ostream)
    (princ (d-id-id anaphor) ostream)
    (write-string "' a-uid='" ostream)
    (princ (d-id-utterance-id anaphor) ostream)
    (write-string "' r-id='" ostream)
    (princ (d-id-id referent) ostream)
    (write-string "' r-uid='" ostream)
    (princ (d-id-utterance-id referent) ostream)
    (write-string "'/>" ostream)))

  

	   

;;; *************************************************
;;;
;;; Utility functions
;;;
;;; *************************************************

;;; extract-fine-system fns are copied from qa2008.lisp

(defparameter *discourse-sets*
    '(

;;; 1
      
;;;     (21.34) Victoria Chen, Chief Financial Officer of Megabucks Banking Corp since 2004, saw her pay jump 20%, to $1.3 million, as the 37-year-old also became the Denver-based financial-services company's president.
;;; It has been ten years since she came to Megabucks from rival Lotsabucks. 
      (1 2) 


      
;;; (21.36) According to Doug, Sue just bought a 1961 Ford Falcon.
;;; a. But that turned out to be a lie.
;;; b. But that was false.
;;; c. That struck me as a funny way to describe the situation.

;;; 2      
      (3 4) 
;;; 3           
      (3 5)
;;; 4
      (3 6)
;;; (a) Mrs. Martin was so very kind as to send Mrs. Goddard a beautiful goose.      
					; 7 fails
;;; 5
;;; (b) He had gone round one day to bring her some walnuts.      
      (8) 
     
;;; 6      
;;; (c) I saw this beautiful Ford Falcon today.     
      (9) 
      
;;; 7      
;;; 21.38) I am going to the butcher's to buy a goose.      
      (10)

;;; 8
;;; (21.39) It concerns a white stallion which I have sold to an officer. 
;;; But the pedigree of the white stallion was not fully established.      
      (11 12)

;;; 9      
;;; (21.40) I read about it in The New York Times.      
      (13)

;;; 10      
;;; (21.41) Emma smiled and chatted as cheerfully as she could.     
      (14)


;;;  (21.42) 
;;;a. John went to Bob's party, and parked next to a classic Ford Falcon.
;;;b. He went inside and talked to Bob for more than an hour.
;;;c. Bob told him that he recently got engaged.
;;;d. ?? He also said that he bought it yesterday.
;;;d. He also said that he bought the Falcon yesterday.    
 
;;; 11      
      (15 16 17 18)
      
      
;;; 12      
      (15 16 17 19)

;;; 13     
;;;(21.43) Even before she saw it, Dorothy had been thinking about the Emerald City every day.      
      (20)

;;; 14
;;;(21.44) Every dancer brought her left arm forward.     
      (21)

;;; 15      
;;;(21.45) I just bought a copy of Thoreau's Walden. I had bought one five years ago. That one had been very tattered; this one was in much better condition.      
      (22 23 24)
      
;;; 16       
 ;;; (21.46) I almost bought a 1961 Ford Falcon today, but a door had a dent and the engine seemed noisy.     
      (25)

;;; 17       
;;;(21.47) I'm interested in buying a Mac laptop. 
;;;They are very stylish.      
      (26 27)
 
;;; 18       
;;; (21.48) In March in Boulder you have to wear a jacket.      
      (28)

;;; 19      
;;; (a) It was Frodo who carried the ring.     
      (29)
      

;;; (b) It was good that Frodo carried the ring      
					; 30 fails

;;; 20          
;;; John has a Ford Falcon.
;;; It is red.      
      (31 32)

;;; 21          
;;; John has a Ford Falcon.
;;; They are red.      
      (33 34)
      
      
;;; 22      
;;; John has three Ford Falcons.
;;; It is red.      
      (35 36)

;;; 23      
;;; John has three Ford Falcons.
;;; They are red.      
      (37 38)

;;; 24
;;; (21.50) IBM announced a new machine translation product yesterday. 
;;; They have been working on it for 20 years.     
      (39 40)

;;; 25      
;;; (21.51) John has a Ford. 
;;; He is attractive.       
      (41 42)

;;; 26      
;;; (21.52) John has a Ford. 
;;; It is attractive.       
      (43 44)

;;; 27      
;;; (21.53) John bought himself a new Ford. 
      
      (45) 

;;; 28      
;;;(21.54) John bought him a new Ford. 

      (46) 

;;; 29      
;;; (21.55) John said that Bill bought him a new Ford. 

      (47) 

;;; 30      
;;; (21.56) John said that Bill bought himself a new Ford. 

      (48) 

;;; 31      
;;; (21.57) He said that he bought John a new Ford. 

      (49) 

 ;;; 32     
;;; (21.58) John parked his car in the garage after driving it around for hours.        
      (50)
      
;;; 33      
;;; (21.59) The doctor found an old map in the captain's chest. 
;;; Jim found an even older map hidden on the shelf. 
;;; It described an island.     
      
      (51 52 53)

;;; 34      
;;; (21.60) Billy Bones went to the bar with Jim Hawkins. 
;;; He called for a glass of rum.


      (54 55)

;;; 35      
;;; (21.61) Jim Hawkins went to the bar with Billy Bones. 
;;; He called for a glass of rum.
      
      (56 57)

;;; 36      
;;; (21.62) Billy Bones had been thinking about a glass of rum ever since the pirate ship docked. 
;;; He hobbled over to the Old Parrot bar. 
;;; Jim Hawkins went with him. 
;;; He called for a glass of rum.
          
      (58 59 60 61)

;;; 37      
;;; (21.63) Long John Silver went with Jim to the Old Parrot. 
;;; Billy Bones went with him to the Old Anchor Inn. 
    
      (62 63)

;;; 38      
;;; (21.64) John telephoned Bill. 
;;; He lost the laptop.

      (64 65)

;;; 39      
;;; (21.65) John criticized Bill. 
;;; He lost the laptop.
      
      (66 67)

;;; 40      
;;; (21.66) John saw a beautiful 1961 Ford Falcon at the used car dealership.
;;; He showed it to Bob.
;;; He bought it.
     
    (68 69 70) 
      ))
;;; ignore the rest for now

#|

(excl::shell "rm /home/aac10/anaphora-test/*")

(convert-fine-system-output-to-discourses "~/anaphora.1" "/home/aac10/anaphora-test/")

(convert-fine-system-output-to-discourses "~/anaphora.1" "/home/aac10/anaphora-test/" '(30))

|#

(defun convert-fine-system-output-to-discourses (ifile odir &optional dnums)
  (let ((*anchor-rmrs-p* t)
	(drecord nil))
    ;;; read in all the discourses and store them in drecord
    (with-open-file (istream ifile :direction :input)
      (loop (let ((fsout (read-line istream nil nil)))
	      (unless fsout (return))
	      (let* ((scount (extract-fine-system-number fsout))
		     (dcount (if (integerp scount)
				 (loop for disc in *discourse-sets*
				 and temp-count from 1
				 when (member scount disc)
				 return temp-count)))
		     (mrs-string (extract-fine-system-mrs fsout)))
		;;;
		(when (and dcount
		           (stringp mrs-string)
			   (not (equal mrs-string "")))
		  (with-input-from-string (mstream mrs-string)
		    (let* ((mrs (read-mrs mstream))
			   (rmrs (mrs-to-rmrs mrs))
			   (dmrs (rmrs-to-dmrs rmrs)))
		      (when dmrs
			(let ((drec (assoc dcount drecord)))
			  (if drec
			      (push dmrs (cdr drec))
			    (push (cons dcount (list dmrs))
				  drecord)))))))))))
    ;;; convert each item in drecord to a discourse
    (dolist (ditem drecord)
      (let* ((dcount (car ditem))
	     (dmrss (reverse (cdr ditem)))
	     (discourse (dmrs-list-to-discourse dmrss))) 
	(unless (and dnums (not (member dcount dnums)))
	  (let ((ofile (format nil "~Ad~A.dmrs" odir dcount)))
	    (with-open-file 
		(ostream ofile :direction :output
		 :if-does-not-exist :create
		 :if-exists :supersede)
	      (output-discourse discourse ostream)
	      (finish-output ostream))))))))


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

;;;; scratch code

#|

(with-open-file (istream "~/tst.raw" :direction :input)
  (let ((mrs-string (read-line istream nil nil)))
    (with-input-from-string (mstream mrs-string)
      (let ((rmrs (mrs-to-rmrs (read-mrs mstream))))
	(rmrs-to-dmrs rmrs)))))

;;;	(output-dmrs1 (rmrs-to-dmrs rmrs) 'dxml t)))))

;;; (with-open-file (ostream file-name :direction :output
;;;			  :if-exists :supersede
;;;			  :if-does-not-exist :create)
;;;	(layout-dmrs dmrsstruct :svg ostream))


;;; testing well-formedness of dmrs

(let ((*anchor-rmrs-p* t))
    (with-open-file (istream "~/anaphora.1" :direction :input)
      (loop (let ((fsout (read-line istream nil nil)))
	      (unless fsout (return))
	      (let* ((scount (extract-fine-system-number fsout))
		     (mrs-string (extract-fine-system-mrs fsout)))
		;;;
		(when (and 
		           (stringp mrs-string)
			   (not (equal mrs-string "")))
		  (with-input-from-string (mstream mrs-string)
		    (let* ((mrs (read-mrs mstream))
			   (rmrs (mrs-to-rmrs mrs))
			   (dmrs (rmrs-to-dmrs rmrs)))
		      (when dmrs
			(with-open-file 
			    (ostream "~/dmrs-test.dmrs" 
			     :direction :output
			     :if-does-not-exist :create
			     :if-exists :supersede)
			  (write-string 
			  "<?xml version='1.0'?> <!DOCTYPE dmrs SYSTEM \"/homes/aac10/lingo/lkb/src/rmrs/dmrs.dtd\" >" ostream)
			  (output-dmrs1 dmrs 'dxml ostream)
			  (finish-output ostream)
			  (excl::shell 
			   (concatenate 'string
			     "xmlnorm -s ~/dmrs-test.dmrs 2>| ~/dmrs-tmp/"
			     (format nil "~A" scount)
			   ".errs"))))))))))))



|#