;;; Copyright (c) 1993-2005
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;;   Bernard Jones
;;;   see `licence.txt' for conditions.

(in-package :lkb)

;;; The built-in morphophonological component
;;; 

;;; AAC April 2005 - the time has finally arrived to try and sort this out!
;;; A complete rational reconstruction is easier than 
;;; looking at the old code
;;;
;;; Get rid of all the recursive stuff here and turn it
;;; into a system which can be used to put an edge onto the LKB
;;; *tchart*.  
;;;
;;; For now, get rid of infix (which probably didn't work anyway)
;;; and (temporarily unless speed is OK) forget about letter trees
;;; (August - speed seems OK, no need for letter trees)
;;;
;;; integrated reading into the relevant function in the lex rule reader
;;; which should support a more flexible positioning of the % declarations
;;;

#| 
 code assumes #\\ is escape character
              #\* is null character
              #\! introduces a letter set
	      #\? a wild card character

these were variables but this code might not work if they were changed,
to something unexpected, so don't give people the temptation!
|#

(defstruct letter-set 
  var char letters)
;;; e.g., %(letter-set (!c bdfglmnprstz))
;;; goes to var !c, char #\c, and letters (#\b #\d etc)
;;; var is used in order to have a relatively readable way of associating
;;; a binding with a letter set.

(defmethod print-object ((x letter-set) stream)
  (format stream "#[letter-set ~a]" (letter-set-var x)))

(defstruct letter-wild-card
  char letters)
;;; e.g., %(wild-card (?c bdf))

(defstruct morph-rule 
  class rules subrules letter-sets id)
;;; class is prefix or suffix
;;; rules are the ids of the rules that use these patterns
;;; e.g., (3sg-v_irule)
;;;
;;; 3sg-v_irule :=
;;; %suffix (!s !ss) (!ss !ssses) (ss sses)
;;; sing-verb.
;;;
;;; each pattern like (!s !ss) is a separate subrule
;;;
;;; new style
;;; +ed := suffix & 
;;; [ PATTERN "(* ed) (!ty !tied) (e ed) (!t!v!c !t!v!c!ced)" ].
;;;
;;; letter-sets may be treated as global (old style) or
;;; local (new style)


(defstruct morph-subrule
  under surface)
;;; subrules map surface to underlying forms
;;; internally under and surface are a list 
;;; containing characters of letter set structures

;;; Global variable initialisation

(defvar *morph-rule-set* nil)

(defvar *letter-set-list* nil)

(defvar *letter-wild-card-list* nil)

;; [bmw] temporary (?) hack to allow morph rules with 
;;       null spelling change component
(defparameter *allow-null-morph-rules* nil)

;;; 

(defun in-morph-rule-set-p (rule)
  (member (rule-id rule) *morph-rule-set* :key #'morph-rule-rules 
	  :test #'member))

;;; printing functions: mostly for debugging purposes

(defun show-letter-sets (&key (stream t))
  (dolist (ls (reverse *letter-set-list*))
    (format stream "~%~A ~A" (letter-set-var ls) (letter-set-letters ls))))

(defun show-wildcard-sets (&key (stream t))
  (dolist (ls (reverse *letter-wild-card-list*))
    (format stream "~%~A ~A" 
	    (letter-wild-card-char ls) 
	    (letter-wild-card-letters ls))))

(defun show-morph-rules (&key (stream t))
  (dolist (mr (reverse *morph-rule-set*))
    (with-slots (name) mr
      (format stream "~&~%~a" (string-downcase (string name)))
      (pprint-morph-rule name :stream stream))))

(defun pprint-morph-rule (morph-rule-name &key (stream t))
  (with-slots (name class subrules) 
      (find morph-rule-name *morph-rule-set* 
	    :key #'morph-rule-rules :test #'member)
    (let ((s (make-string-output-stream)))
      (format s "~% %~a (UNDERLYING / SURFACE)" class)
      (loop
	  for subrule in subrules
	  do
	    (format s "~%~2T~a / ~a"
		    (show-morph-subpattern (morph-subrule-under subrule) class)
		    (show-morph-subpattern (morph-subrule-surface subrule) class))
	  finally
	    (if stream 
		(princ (get-output-stream-string s))
	      (return (get-output-stream-string s)))))))


(defun show-morph-subpattern (pattern class)
  (let ((printstr (loop for char in (if (eql class 'suffix)
					(reverse pattern) pattern)
		      collect
			(if (letter-set-p char) 
			    (letter-set-var char)
			  (char-downcase char)))))
     (format nil "~{~A~}" (or printstr '("")))))


;;; Reset function 
(defun reset-morph-var () 
  (setf *morph-rule-set* nil)
  (setf *letter-set-list* nil)
  (setf *letter-wild-card-list* nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Morphological rule I/O functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-morphology-notation-table nil
  (let ((temporary-readtable (copy-readtable nil)))
;;; (set-syntax-from-char to-char #\* temporary-readtable temporary-readtable)
;;; can be used to make to-char `normal' but I don't think this is needed now
    temporary-readtable))



(defun read-morphology-letter-set (istream)
  ;;; e.g. %(letter-set (!c bdfglmnprstz))
  ;;; or   %(wild-card (?c bdf))
  (let* ((*readtable* (make-morphology-notation-table))
	 ;; this restores the initial readtable
	 ;; necessary because ! etc may be break characters
	 ;; This is now called from within a TDL reader.
	 (string-thing (read-line istream nil nil))
	 (start-pos
	  (if string-thing
	      (position '#\( string-thing)))
	 (main-string (if start-pos (subseq string-thing (+ 1 start-pos))))
	 (real-start-pos (if main-string (position '#\( main-string)))
	 (real-string (if real-start-pos 
			  (string-upcase
			   (subseq main-string (+ 1 real-start-pos))))))
    (if real-string 
	(cond 
	 ((string-equal (subseq main-string 0 10) "letter-set")
	  (let ((var (parse-letter-set-var real-string)))
	    (when var
	      (push
	       (create-letter-set 
		var 
		(parse-letter-set-letters real-string))
	       *letter-set-list*))))
	 ((string-equal (subseq main-string 0 9) "wild-card")
	  (let ((var (parse-wild-card-var real-string)))
	    (when var
	      (push
	       (create-letter-wild-card 
		var 
		(parse-letter-set-letters real-string))
	       *letter-wild-card-list*))))
	 (t (morph-read-cerror (format nil "~%Illformed morphological specification ~S: letter-set or wild-card expected" string-thing))))
      (morph-read-cerror 
       (format nil "~%Incomplete or malformed letter set specification ~S ignored" 
	       string-thing)))))

  
(defun parse-letter-set-var (str)
  ;;; expecting ! followed by a single character
  (let* ((pling-pos (position #\! str))
	 (next-char (if pling-pos (elt str (+ 2 pling-pos)))))
    (if (and pling-pos (whitespacep next-char))
	(subseq str 0 2)
      (morph-read-cerror
	(format nil "~%Incorrectly written letter set in ~A" str)))))
      

(defun parse-wild-card-var (str)
  ;;; expecting ? followed by a single character
  (let* ((pling-pos (position #\? str))
	 (next-char (if pling-pos (elt str (+ 2 pling-pos)))))
    (if (and pling-pos (whitespacep next-char))
	(subseq str 0 2)
      (morph-read-cerror 
	     (format nil "~%Incorrectly written wild card set in ~A" str)))))

(defun parse-letter-set-letters (str)
  ;;; takes a string and returns a list of all characters it contains
  ;;; a ) is taken as the end of the string, unless there's a \ in
  ;;; front of it.  \\ is needed to include \.  whitespace is ignored
  (let ((initial-chars (coerce (subseq str 2) 'list))
	(chars nil)
	(escape-p nil))
    (dolist (char initial-chars)
      (cond ((whitespacep char) (setf escape-p nil))
	    ((char= char #\\) (if escape-p 
				  (progn (pushnew char chars) (setf escape-p nil))
				(setf escape-p t)))
	    ((char= char #\)) (if escape-p 
				  (progn (pushnew char chars) (setf escape-p nil))
				(return)))
	    (t (setf escape-p nil) (pushnew char chars))))
    (nreverse chars)))


(defun read-morphology-affix (id istream)
  ;;; e.g., %suffix (!s !ss) (!ss !ssses) (ss sses)
  ;;; called with id which is the rule name (e.g., 3sg-v_irule)
  (let* ((*readtable* (make-morphology-notation-table))
	 (string-thing (read-line istream))
	 (class (read-affix-class string-thing))
	 (pattern-start (position #\( string-thing :start 1))
	 (toanalyse 
	  (if pattern-start (string-upcase 
			     (subseq string-thing (+ 1 pattern-start))))))
    (cond ((and pattern-start class)
	   (read-morphology-pattern id class toanalyse *letter-set-list* nil))
	  ((null pattern-start)
	   (morph-read-cerror 
	    (format nil "~%Problem in ~A: no patterns" id)))
	  (t (morph-read-cerror 
	     (format nil "~%Problem in ~A: not specified as suffix or prefix" id))))))


(defun read-morphology-pattern (rule-id class toanalyse letter-sets pattern-id)
  (let ((method-list nil)
	(pattern-start 0)
	(id (or rule-id pattern-id)))
    (loop 
      (unless toanalyse (return nil))
      (multiple-value-bind (pattern end-pos)
	  (read-morphology-subpattern toanalyse id letter-sets)
	;; errors checked inside read-morph-subpattern
	(unless end-pos (return nil))
	(push pattern method-list)
	(setf pattern-start (position #\( toanalyse :start end-pos))
	(unless pattern-start (return nil))
	(setf toanalyse (subseq  toanalyse (+ 1 pattern-start)))))
    ;;; note that the subrule list ends up reversed: this is what is wanted
    ;;; because the most specific subrules will occur first in the rule
    ;;; structure.  subrules are checked in order and only the most
    ;;; specific applicable rule that matches the stem is used 
    ;;; (obviously when we're parsing we don't know the stem, so this
    ;;; requires returning all subrules that match)
    (morph-input 
     class 
     (if rule-id (list rule-id))
     method-list
     id)))

  
(defun read-affix-class (str)
  ;;; takes a string beginning %suffix or %prefix
  (cond ((string-equal str "suffix" :start1 1 :end1 7) 'suffix)
        ((string-equal str "prefix" :start1 1 :end1 7) 'prefix)
	(t
	 (morph-read-cerror
	  (format nil "~%Incorrect morphology specification ~A: suffix or prefix expected"
		 str)))))

(defun read-morphology-subpattern (str id local-letter-sets)
  ;;; takes something like "(* a) (* \!) (* \*;)"
  ;;; and returns a pattern corresponding to the first element
  ;;; plus the position of the ) closing the pattern 
  ;;; note that ( and ) can be escaped
  ;;; since this reader has to explode anyway, replaces affix-explode
  ;;; and returns two lists of character-specs
  ;;; replacing * with nothing, !x with the corresponding letter-set
  ;;; structure and ?z with wild-card structure.  
  ;;; Other characters are unchanged.
  ;;; e.g. * -> nil
  ;;;      ed -> (#\e #\d)
  ;;;      !t!v!c -> < three letter set records >
    (let ((initial-chars (coerce str 'list))
	  (escape-p nil)
	  (part1 nil)
	  (part2 nil)
	  (part1-p t)
	  (letter-set-p nil)
	  (wild-card-p nil)
	  (count 0))
      (dolist (char initial-chars)
	(incf count)
	(cond ((whitespacep char)
	       (cond ((not part1-p)
		      (morph-read-cerror
		       (format nil "~%Unexpected character ~A in ~A" char id)))
		     (letter-set-p
		      (morph-read-cerror
		       (format nil "~%Nothing following character set character '~a' in ~a"
			      #\! id)))
		     (wild-card-p
		      (morph-read-cerror
		       (format nil "~%Nothing following character set character '~a' in ~a"
			      #\? id)))
		     (t ;; assume in surface part of pattern
		      (setf escape-p nil)
		      (setf part1-p nil))))
	      ((char= char #\\)
	       (if escape-p 
		   (progn (if part1-p (push char part1) (push char part2))
			  (setf escape-p nil))
		 (setf escape-p t)))
	      ((char-equal char #\*) 
	       (if escape-p 
		   (progn (if part1-p (push char part1) (push char part2))
			  (setf escape-p nil))
		 nil))
	      ((char-equal char #\!)
	       (if escape-p 
		   (progn (if part1-p (push char part1) (push char part2))
			  (setf escape-p nil))
		 (setf letter-set-p t)))
	      ((char-equal char #\?)
	       (if escape-p 
		   (progn (if part1-p (push char part1) (push char part2))
			  (setf escape-p nil))
		 (setf wild-card-p t)))
	      ((char= char #\)) 
	       (if escape-p 
		   (progn (if part1-p (push char part1) (push char part2))
			  (setf escape-p nil))
		 (return)))
	      (letter-set-p 
	       (let ((ls (letter-set-for-char char id local-letter-sets)))
		 (when ls
		   (if part1-p (push ls part1) (push ls part2))
		   (setf letter-set-p nil))))
	      (wild-card-p 
	       (let ((ls (wild-char-set-for-char char id)))
		 (when ls
		   (if part1-p (push ls part1) (push ls part2))
		   (setf wild-card-p nil))))
	      (t (setf escape-p nil) 
		 (if part1-p (push char part1) (push char part2)))))
      (cond (letter-set-p (morph-read-cerror 
			  (format nil "~%Nothing following character set character '~a' in ~a"
				  #\! id)))
	     (wild-card-p (morph-read-cerror 
			  (format nil "~%Nothing following character set character '~a' in ~a"
				  #\? id)))
	     (t (values (cons (nreverse part1) (nreverse part2))
			count)))))

(defun letter-set-for-char (char id local-letter-sets)
  (let ((char-set (find char (or local-letter-sets
				 *letter-set-list*)
			:key #'letter-set-char)))
    (or char-set
	(morph-read-cerror
	  (format nil "~%Undefined letter set `~a~A' in ~a" 
		  #\! char id)))))

(defun wild-char-set-for-char (char str)
  (let ((char-set (find char
			*letter-wild-card-list* 
			:key #'letter-wild-card-char)))
    (or char-set
	(morph-read-cerror 
	  (format nil "~%Undefined wild card `~a~A' in ~a" 
		  #\? char str)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Alternative morphological specification
;;; letter set and affixation patterns in types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun extract-affixation-specifications nil
  ;;; this code sets up the morph-rule data structures but
  ;;; does not instantiate the associated rules
  (let ((affix-types 
	 (loop for type in (retrieve-descendants *affix-type*)
	     unless (ltype-daughters type)
	     collect type)))
    (dolist (affix-type affix-types)
      (let* ((affix-name (ltype-name affix-type))
	     (letter-sets (extract-letter-sets affix-type affix-name))
	     (affix-class (extract-affix-class affix-type affix-name))
	     (pattern-string (extract-affix-pattern affix-type)))
	(cond ((and affix-class pattern-string)
	       ;;; adjust pattern for peculiarities of existing fn
	       (read-morphology-pattern 
		nil affix-class 
		(string-upcase (subseq pattern-string 1)) 
		letter-sets affix-name))
	      ;; affix-name if for identification
	      (affix-class
	       (format t 
		       "~%Warning: no pattern specified for affix ~A, ignored" affix-class))
	      (pattern-string
	       (format t 
		       "~%Warning: suffix/prefix not specified for affix ~A, ignored" pattern-string))
	      (t nil))))))

(defun extract-rule-affixation-type (rule-fs rule-id)
  ;;; this is called as rules are expanded: the rule is added
  ;;; to the appropriate set of morph patterns
  (let ((affixation-fs
	 (get-dag-value (tdfs-indef rule-fs) *rule-affix-feature*)))
    (if affixation-fs
	(let ((affixation (type-of-fs affixation-fs)))
	  (when affixation
	    (let ((morph-rule-entry 
		   (find affixation *morph-rule-set* 
			 :key #'morph-rule-id)))
	      (when morph-rule-entry
		(push rule-id (morph-rule-rules morph-rule-entry)))))))))
	  
(defun extract-affix-class (type-record type)
  (let ((type-constraint (ltype-constraint type-record)))
    (if type-constraint
	(let ((affix-class-fs 
	       (get-dag-value type-constraint *affix-class-feature*)))
	  (if affix-class-fs
	      (let ((affix-class (type-of-fs affix-class-fs)))
		(cond ((member affix-class '(suffix prefix))
		       affix-class)
		      ((string-equal affix-class "suffix") 
		       'suffix)
		      ((string-equal affix-class "prefix") 
		       'prefix)
		      ((stringp affix-class) 
		       (error "~%Unknown class ~A on affix type ~A" 
			      affix-class type))
		      (t nil))))))))

(defun extract-affix-pattern (type-record)
  (let ((type-constraint (ltype-constraint type-record)))
    (if type-constraint
	(let ((affix-pattern-fs 
	       (get-dag-value type-constraint *affix-pattern-feature*)))
	  (if affix-pattern-fs
	      (let ((affix-pattern (type-of-fs affix-pattern-fs)))
		(if (stringp affix-pattern)
		    affix-pattern
		  nil)))))))

(defun extract-letter-sets (type-record type)
  (let ((type-constraint (ltype-constraint type-record)))
    (unless type-constraint
      (error "~%No constraint on affix type ~A" type))
    (let ((letter-sets-fs (get-dag-value 
			   type-constraint *letter-set-feature*)))
      (when letter-sets-fs
	(extract-letter-sets-from-list letter-sets-fs nil type)))))

(defun extract-letter-sets-from-list (letter-set-fs sets-so-far type)
  (if (eql (type-of-fs letter-set-fs) *empty-list-type*)
      sets-so-far
    (let ((letter-set-pair (get-dag-value letter-set-fs (car *list-head*))))
      (unless letter-set-pair
	(error "~%Malformed letter-set specification on ~A" type))
      (let ((letter-set-char (type-of-fs (get-dag-value letter-set-pair 
					    *letter-set-char-feature*)))
	    (letter-set-match (type-of-fs (get-dag-value letter-set-pair 
					     *letter-set-match-feature*)))
	    (remainder (get-dag-value letter-set-fs (car *list-tail*))))
	(unless (and letter-set-char 
		     (stringp letter-set-char)
		     (char= (elt letter-set-char 0) #\!)
		     letter-set-match
		     (stringp letter-set-match)
		     remainder)
	  (error "~%Malformed letter-set specification on ~A" type))
	(extract-letter-sets-from-list 
	 remainder 
	 (cons (create-letter-set (string-upcase letter-set-char) 
				  (coerce (string-upcase 
					   letter-set-match) 'list)) 
				  sets-so-far)
	 type)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Morphological rule compilation functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-letter-set (set rule-set)
  ;;; e.g., input is !c (#\d #\b)
  (let* ((char (elt set 1)))
    (make-letter-set :var set
		     :char char
		     :letters rule-set)))

(defun create-letter-wild-card (set rule-set)
  ;;; e.g., input is ?c (#\d #\b)
  (let* ((char (elt set 1)))
    (make-letter-wild-card :char char
		     :letters rule-set)))

;;; Turn rules into a rule-structure

(defun morph-input (class lexrules subrules name)
  ;; [bmw] temporary (?) hack to allow morph rules with 
  ;;       null spelling change component
  (if *allow-null-morph-rules*
      (return-from morph-input
	(morph-input-allow-null-morph-rules class lexrules subrules name)))
  (let ((subrule-structs 
	 (loop for pair in subrules
	     nconc
	       ;; e.g. (!ty !tied) - surface is second
	       (let* ((surface (cdr pair))
		      (underlying (car pair))
		      (surface-wild
		       (collect-wildcard-letters surface underlying
						 name))
		      (underlying-wild
		       (collect-wildcard-letters underlying surface
						 name)))
		 (if (null surface)
		     (morph-read-cerror
		       (format nil "~%Error: pattern with no affixation ignored in ~A" name))
		   (if (or surface-wild underlying-wild)
		       (expand-morph-subrules surface underlying
					      surface-wild
					      underlying-wild class)
		     (list
		      (create-morph-subrule surface underlying class))))))))
    (if subrule-structs
	(push
	 (make-morph-rule
	  :class class
	  :rules lexrules
	  :subrules subrule-structs
	  :id name) 
	 *morph-rule-set*)
      (morph-read-cerror
	(format nil "~%Error: no valid morphological patterns in ~A: rule-ignored" name)))))


;; [bmw] temporary (?) hack to allow morph rules with 
;;       null spelling change component
(defun morph-input-allow-null-morph-rules (class lexrules subrules name)
  (let ((subrule-structs 
	 (loop for pair in subrules
	     nconc
	       ;; e.g. (!ty !tied) - surface is second
	       (let* ((surface (cdr pair))
		      (underlying (car pair))
		      (surface-wild
		       (collect-wildcard-letters surface underlying
						 name))
		      (underlying-wild
		       (collect-wildcard-letters underlying surface
						 name)))
		 (if (or surface-wild underlying-wild)
		     (expand-morph-subrules surface underlying
					    surface-wild
					    underlying-wild class)
		   (list
		    (create-morph-subrule surface underlying class)))
		 ))))
    (push
     (make-morph-rule
      :class class
      :rules lexrules
      :subrules subrule-structs
      :id name) 
     *morph-rule-set*)
    ))

(defun collect-wildcard-letters (pattern other rulename)
  (loop for thing in pattern
      when (or
	    (letter-wild-card-p thing)
	    (and (letter-set-p thing)
		  (not (member thing other))))
      collect 
	(if (letter-set-p thing)
	    (progn 
	      (format t "~%Warning: legacy mode - treating unmatched letterset !~A in ~A as wild-card" (letter-set-char thing) rulename)
	      thing)
	  thing)))

(defun create-morph-subrule (surface underlying class)
  ;;; reverse the specs for suffixes to make application more efficient
    (make-morph-subrule
     :surface (if (eql class 'suffix) 
		  (reverse surface) surface)
     :under (if (eql class 'suffix) 
		(reverse underlying) underlying)))


(defun expand-morph-subrules (surface underlying surface-wild underlying-wild
			      class)
  (let ((subrules nil)
	(surface-char-set (expand-morph-set surface surface-wild))
	(underlying-char-set (expand-morph-set underlying underlying-wild)))
    (dolist (surface-pattern surface-char-set)
      (dolist (underlying-pattern underlying-char-set)
	(push 
	 (create-morph-subrule surface-pattern underlying-pattern class)
	 subrules)))
    subrules))

(defun expand-morph-set (pattern wild)
  (let ((wild-letter (car wild)))
    (if wild-letter
	(let* ((letters (if (letter-wild-card-p wild-letter)
			    (letter-wild-card-letters wild-letter)
			  (letter-set-letters wild-letter)))
	       (replaced-patterns 
		(loop for char in letters
		    collect
		      (substitute char wild-letter pattern :count 1))))
	  (loop for replaced-pattern in replaced-patterns
	      nconc
		(expand-morph-set replaced-pattern (cdr wild))))
      (list pattern))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions for morphological analysis
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; This is called from the parser with an uppercase string.  
;;; We attempt to find any applicable affixes and return a list
;;; for each one that works.  Each element is a pair of the form with 
;;; the affix removed (according to the rules) plus the name of the rule
;;; associated with the affix.  
;;; e.g., given "CANNED" this might return (("CAN" . past-v_irule)
;;;                                          ("CANN" . past-v_irule))
;;;     "CANS" -> (("CAN" . 3sg-v_irule) ("CAN" . pl-noun_irule))
;;;     "CANNINGS" -> (("CANNING" . 3sg-v_irule) ("CANNING" . pl-noun_irule))
;;;     "UNCANNED" -> (("CANNED" . un_rule) ("UNCAN" . past-v_irule)
;;;                    ("UNCAN" . past-v_irule))
;;;
;;; No check is done for stems - this is purely based on the affixes
;;; and spelling rules.  Only one level is stripped at a time.
;;; The assumption is that all the checking is done by the calling code.

(defun one-step-morph-analyse (string)
  (let* ((input-chars (coerce string 'list))
	 (analysis-results nil)
	 (suffix-set (reverse input-chars)))
    ;;; use reversed input if we're checking a suffix
    (dolist (rule *morph-rule-set*)
      (let ((suffix-p (eql (morph-rule-class rule) 'suffix))
	    (rule-names (morph-rule-rules rule)))
	(dolist (subrule (morph-rule-subrules rule))
	  (multiple-value-bind (matchp unmatched bindings)
	      (match-rule-letters (if suffix-p suffix-set input-chars)
				  (morph-subrule-surface subrule)
				  nil)
	    ;;; bindings are set so that e.g.
	    ;;; when the subrule says (!t!v!c !t!v!c!ced) and the input
	    ;;; is "TAPPED" we get ((!t . #\t) (!v . #\a) (!c . #\p))
	    ;;; Bindings must be consistent.
	    ;;; Bindings are then used when recreating the input form.
	    (when matchp
	      (let ((new-form 
		     (create-other-morph-form unmatched 
					     (morph-subrule-under subrule)
					     bindings suffix-p)))
		(dolist (rule-name rule-names)
		  (push (cons new-form rule-name)
			analysis-results))))))))
    (remove-duplicates 
     analysis-results :test #'equalp)))

(defun match-rule-letters (next-input rule-spec binding-list)
  (cond ((null rule-spec) (values t next-input binding-list))
	;;; matched everything - we're done
	((null next-input) nil) 
	;;; letters left in the subrule but no input left - fail
	((letter-set-p (car rule-spec))
	 (let ((new-bindings 
		(match-letter-set (car next-input)
				  (car rule-spec) binding-list)))
	   (if new-bindings
	       (match-rule-letters (cdr next-input)
				   (cdr rule-spec)
				   new-bindings))))
	;;; letter set - must respect any previous bindings
	;;; if none and the match works, add to bindings
	((eql (car next-input) (car rule-spec))
	 (match-rule-letters (cdr next-input)
			     (cdr rule-spec)  binding-list))
	;;; characters match - recurse on the rest
	(t nil)))

(defun match-letter-set (input-letter letter-set bindings)
  ;;; returns bindings (possibly updated) if successful, nil otherwise
  (let* ((var (letter-set-var letter-set))
	 (already-bound (assoc var bindings :test #'equal)))
    (if already-bound 
	(if (eql input-letter (cdr already-bound))
	    bindings ;; bound and OK
	  nil) ;; bound to something else
      ; else unbound
      (if (member input-letter (letter-set-letters letter-set))
	  ; matches letter set
	  (cons (cons (letter-set-var letter-set) input-letter)
		bindings) ; add to bindings
	nil)))) ; input not member of letter set

(defun create-other-morph-form (unmatched other bindings suffix-p)
  ;;; we've got a match 
  ;;; unmatched are the letters we didn't get to,
  ;;; other is the spec for the other side of the subrule
  ;;; (i.e., the underlying form if we're analysing and the surface
  ;;; form if we're generating)
  ;;; bindings tell us how to replace letter sets, suffix-p tells
  ;;; us which end to put the letters!
  (let* ((new-letters
	  (append 
	   (if bindings
	       (loop for letter in other
		   collect
		     (if (letter-set-p letter)
			 (cdr (assoc (letter-set-var letter) 
				     bindings :test #'equal))
		       ;; no error testing here - assume
		       ;; rules have been checked to make sure
		       ;; we won't end up with an unbound letter set here
		       letter))
	     other)
	   unmatched))
	 (ordered-letters (if suffix-p (reverse new-letters) new-letters)))
    (coerce ordered-letters 'string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions for morphological generation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun morph-generate (string rule-name)
  ;;; for instance given "CAN" and PAST-V_IRULE
  ;;; this should output "CANNED"
  ;;; This works in just the same way as analysis, with surface
  ;;; and underlying sides swapped, except that we typically
  ;;; know the rule for generation.  This in turn means that
  ;;; while the analysis side cannot sensibly have a null affix
  ;;; the generation side often will.
  ;;; When we are generating, we accept the first matching subrule only.
  (let* ((input-chars (coerce string 'list))
	 (rule (find rule-name *morph-rule-set* 
		     :key #'morph-rule-rules
		     :test #'member))
	 (rule-class (and rule (morph-rule-class rule)))
	 (suffix-p (eql rule-class 'suffix)))
    (if rule
      (dolist (subrule (morph-rule-subrules rule))
        (multiple-value-bind (matchp unmatched bindings)
            (match-rule-letters (if suffix-p (reverse input-chars) input-chars)
                                (morph-subrule-under subrule)
                                nil)
          (when matchp
            (let ((new-form 
                   (create-other-morph-form unmatched 
                                           (morph-subrule-surface subrule)
                                           bindings suffix-p)))
              (return new-form)))))
      string)))
