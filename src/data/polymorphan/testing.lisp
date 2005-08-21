(in-package :lkb)

;;; The idea of this artificial grammar is to test the tokenisation
;;; handling and morphology components of the LKB.  
;;; This code calls the script in such a way as to load different
;;; versions of the rules and then calls appropriate tests, writing the
;;; results to a file.  This is compared to a reference version.

(defparameter *infl-variant* "basic")

#|
"basic"
"bigger"
"buildfilter"
"escape"
"feed"
"match"
"errors"
|#

(defun test-morph nil
  ;;;
  (let ((*show-parse-p* nil))
  ;;;
  ;;; basic
  ;;;
  (setf *infl-variant* "basic")
  (read-script-file-aux "data/polymorphan/script")
  (with-open-file (ostream "data/polymorphan/basic-test" :direction :output
		   :if-does-not-exist :create :if-exists :supersede)
    (dolist (ms '(nil t))
      (setf *most-specific-only-p* ms)
      (dolist (word '(rained partied partyed coyed duped dupeed hated 
		      hatted hateed raind rainned rainied coyd coyyed 
		      dupped dupd wrote writed dreamed dreamt hit hitted
		      unrained))
	(multiple-value-bind (et st ct ft mt)
	    (do-parse-tty (string word)) 
	  (format ostream "~%~A ~A ~A ~A ~A ~A ~A" word ms et st ct ft mt))))
    (dolist (word '(rain party coy dupe hate
		    hat write dream hit))
      (format ostream "~%~A" (full-morph-generate (string word) 'ed-suffix))
      (format ostream "~%~A" (full-morph-generate (string word) 's-suffix))
      (format ostream "~%~A" (full-morph-generate (string word) 'un-prefix))))
  ;;;
  ;;; bigger - for testing affixes which are shorter than stems
  ;;;
  (with-open-file (ostream "data/polymorphan/bigger-test" :direction :output
		   :if-does-not-exist :create :if-exists :supersede)
    (setf *infl-variant* "bigger")
    (read-script-file-aux "data/polymorphan/script")
  ;;; should get warnings on load
  ;;; Warning: SMALLER can feed itself
  ;;; Warning: BIGGER can feed itself
    (dolist (word '(vark))
      (multiple-value-bind (et st ct ft mt)
	  (do-parse-tty (string word))
	(format ostream "~%~A ~A ~A ~A ~A ~A" word et st ct ft mt)))
    ;;; vark matches longer stem: varkvark
    (handler-case 
	(do-parse-tty "aard")
      (error (condition) (format ostream "~%~A" condition))))
  ;;; error expected here
  ;;;
  ;;; "buildfilter"
  ;;;
  (with-open-file (ostream "data/polymorphan/buildfilter-test" :direction :output
		   :if-does-not-exist :create :if-exists :supersede)
    (setf *infl-variant* "buildfilter")
    (read-script-file-aux "data/polymorphan/script")
  ;;; should get warnings on load
    (print-lrfsm :stream ostream))
  ;;;
  ;;; "escape"
  ;;;
  (with-open-file (ostream "data/polymorphan/escape-test" :direction :output
		   :if-does-not-exist :create :if-exists :supersede)
    (setf *infl-variant* "escape")
    (read-script-file-aux "data/polymorphan/script")
  ;;; should be no warnings on load
    (show-letter-sets :stream ostream)
    (show-morph-rules :stream ostream))
  ;;;
  ;;; "feed"
  ;;;
  (with-open-file (ostream "data/polymorphan/feed-test" :direction :output
		   :if-does-not-exist :create :if-exists :supersede)
    (setf *infl-variant* "feed")
    (read-script-file-aux "data/polymorphan/script")
    (dolist (word '(rainaz rainax rainaw))
      (multiple-value-bind (et st ct ft mt)
	  (do-parse-tty (string word))
	(format ostream "~%~A ~A ~A ~A ~A ~A" word et st ct ft mt))))
  ;;;
  ;;; "match"
  ;;; 
  (with-open-file (ostream "data/polymorphan/match-test" :direction :output
		   :if-does-not-exist :create :if-exists :supersede)
    (setf *infl-variant* "match")
    (read-script-file-aux "data/polymorphan/script")
    (show-morph-rules :stream ostream)
    (dolist (ms '(nil t))
      (setf *most-specific-only-p* ms)
      (dolist (word '(aaard aard bard aaardc))
	(multiple-value-bind (et st ct ft mt)
	    (do-parse-tty (string word))
	  (format ostream "~%~A ~A ~A ~A ~A ~A" word et st ct ft mt)))))
  ;;;
  ;;; errors (nothing to record in a file here)
  ;;; all rules are buggy and (show-morph-rules) should give NIL
  (setf *infl-variant* "errors")
  (read-script-file-aux "data/polymorphan/script")
  ))
    





#|

end of systematic stuff - FIX - tokeniser and MWE testing is yet todo 

|#

;;; tokeniser instantiations have format 
;;; left-vertex right-vertex cfrom cto string

(defparameter *test-tokens1*
  '((0 1 0 1 "a")
    (1 2 1 2 "b")
    (2 3 2 3 "c")))

(defparameter *test-tokens2*
  '((0 1 0 1 "a")
    (1 2 1 2 "b")
    (0 2 0 2 "ab")
    (2 3 2 3 "c")
    (3 5 3 5 "doo")
    (4 6 4 6 "poo")))
    


;;; eventually we need to try compounds 
;;; which will mean putting in a replacement for the morphophonemic
;;; component
