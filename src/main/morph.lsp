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
;;;
;;; integrated reading into the relevant function in the lex rule reader
;;; which should support a more flexible positioning of the % declarations
;;;
;;; FIX - support old LKB format too - not that important

(defparameter *letter-set-escape-char* #\\)
(defparameter *letter-set-null-char* #\*)
(defparameter *letter-set-character-set-char* #\!)
(defparameter *letter-set-wild-card-char* #\?)

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
  class name subrules)
;;; class is prefix or suffix
;;; name is the id of the rule
;;; e.g., 3sg-v_irule
;;;
;;; 3sg-v_irule :=
;;; %suffix (!s !ss) (!ss !ssses) (ss sses)
;;; sing-verb.
;;;
;;; each pattern like (!s !ss) is a separate subrule

(defstruct morph-subrule
  under surface)
;;; subrules map surface to underlying forms
;;; internally under and surface are a list 
;;; containing characters of letter set structures

;;; Global variable initialisation

(defvar *morph-rule-set* nil)

(defvar *letter-set-list* nil)

(defvar *letter-wild-card-list* nil)

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

(defun read-morphology-letter-set (istream)
  ;;; e.g. %(letter-set (!c bdfglmnprstz))
  ;;; or   %(wild-card (?c bdf))
  (let* ((*readtable* (copy-readtable nil))
	 ;; this restores the initial readtable
	 ;; necesary because ! etc may be break characters
	 ;; This is now called from within a TDL reader.
	 (string-thing (read-line istream))
	 (form
	  (with-package (:lkb)
	    (read-from-string string-thing nil :eof 
		     :start
		     (position '#\( string-thing)))))
    (cond
     ((eql form :eof) (error "~%Incomplete letter set"))
     ((not (listp form)) 
      (error "~%Illformed morphological specification ~S: letter set expected" string-thing))
     ((eql (car form) 'letter-set)
      (push
       (create-letter-set (second form)) 
       *letter-set-list*))
     ((eql (car form) 'wild-card)
      (push
       (create-letter-wild-card (second form)) 
       *letter-wild-card-list*))
     (t (error "~%Illformed morphological specification ~S: letter set expected" string-thing)))))

(defun read-morphology-affix (id istream)
  ;;; e.g., %suffix (!s !ss) (!ss !ssses) (ss sses)
  ;;; called with id which is the rule name (e.g., 3sg-v_irule)
  (let* ((*readtable* (copy-readtable nil))
	 (string-thing (read-line istream))
	 (start-pos 1) ;; ignore the %
	 (method-list nil)
	 (class nil))
    (loop
      (with-package (:lkb)
	(multiple-value-bind (form end-value)
	    (read-from-string string-thing nil 
			      :eof :start start-pos)
	  (when (eql form :eof) (return))
	  ;; this did allow local letter sets
	  ;; but I've removed this
	  ;;;
	  ;;; first set class to prefix or suffix, then loop
	  ;;; reading subrules
	  (if class 
	      (push form method-list)
	    (progn 
	      (unless (or (eql form 'suffix)
			  (eql form 'prefix))
		(error "~%Incorrect morphology specification ~A: suffix or prefix expected"
		       string-thing))
	      (setf class form)))
	  (setf start-pos end-value))))
    ;;; note that the subrule list ends up reversed: this is what is wanted
    ;;; because the most specific subrules will occur first in the rule
    ;;; structure.  subrules are checked in order and only the most
    ;;; specific applicable rule that matches the stem is used 
    ;;; (obviously when we're parsing we don't know the stem, so this
    ;;; requires returning all subrules that match)
    (morph-input 
     class 
     id 
     method-list
     *letter-set-list*
     *letter-wild-card-list*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Morphological rule compilation functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-letter-set (new-rule)
  ;;; e.g., input is (!c bdfglmnprstz))
  (let* ((set (car new-rule))
	 (rule-name (coerce (string set) 'list))
	 (rule-set (second new-rule)))
    (unless (and (eql (car rule-name) #\!) 
		 (not (cddr rule-name)))
      (error "~%letter-set does not correspond to a single character ~A"
	     new-rule))
    (make-letter-set :var set
		     :char (second rule-name)
		     :letters (coerce (string rule-set) 'list))))

(defun create-letter-wild-card (new-rule)
  ;;; e.g., input is (?c bdf))
  (let* ((set (car new-rule))
	 (rule-name (coerce (string set) 'list))
	 (rule-set (second new-rule)))
    (unless (and (eql (car rule-name) #\?) 
		 (not (cddr rule-name)))
      (error "~%wild-card does not correspond to a single character ~A"
	     new-rule))
    (make-letter-wild-card 
		     :char (second rule-name)
		     :letters (coerce (string rule-set) 'list))))


;;; Turn rules into a rule-structure

(defun morph-input (class name subrules l-set w-set)
  (unless subrules (error "~%No subrules"))
  (let ((subrule-structs 
	 (loop for pair in subrules
	     nconc
	       ;; e.g. (!ty !tied) - surface is second
	       (let* ((surface (affix-explode (second pair) l-set w-set))
		      (underlying (affix-explode (first pair) l-set w-set))
		      (surface-wild
		       (collect-wildcard-letters surface underlying
						 name))
		      (underlying-wild
		       (collect-wildcard-letters underlying surface
						 name)))
		 (if (null surface)
		     (progn (format t "~%Warning: pattern with no affixation ignored in ~A" name)
			    nil)
		   (if (or surface-wild underlying-wild)
		       (expand-morph-subrules surface underlying
					      surface-wild
					      underlying-wild class)
		     (list
		      (create-morph-subrule surface underlying class))))))))
    (push
     (make-morph-rule
      :class class
      :name name
      :subrules subrule-structs) 
     *morph-rule-set*)))

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

(defun affix-explode (affix-spec l-set w-set)
  ;;; This code returns a list of character-specs
  ;;; replacing * with nothing, !x with the corresponding letter-set
  ;;; structure and ?z with wild-card structure.  
  ;;; Other characters are unchanged.
  ;;; e.g. * -> nil
  ;;;      ed -> (#\e #\d)
  ;;;      !t!v!c -> < three letter set records >
  (let ((chars (coerce (string affix-spec) 'list))
	(new-spec nil))
    (loop
      (unless chars (return nil))
      (let ((char (car chars)))
	(setf chars (cdr chars))
	(cond 
	 ;; escape character
	 ((char-equal char *letter-set-escape-char*)
	  (setf char (car chars))
	  (setf chars (cdr chars))
	  (if (null char)
	      (error "nothings following escape character '~a' in '~a'"
		     *letter-set-escape-char* affix-spec))
	  (push char new-spec))
	 ;; null character
	 ((char-equal char *letter-set-null-char*) nil)
	 ;; letter-set variable
	 ((char-equal char *letter-set-character-set-char*) 
	  (setf char (car chars))
	  (setf chars (cdr chars))
	  (if (null char)
	      (error "nothings following character set character '~a' in '~a'"
		     *letter-set-character-set-char* affix-spec))
	  (let ((char-set (find char
				l-set :key #'letter-set-char)))
	    (unless char-set
	      (error "~%undefined character set ~a~A in '~a'" 
		     *letter-set-character-set-char* char affix-spec))
	    (push char-set new-spec)))
	 ;; wild card
	 ((char-equal char *letter-set-wild-card-char*) 
	  (setf char (car chars))
	  (setf chars (cdr chars))
	  (if (null char)
	      (error "nothings following wild card character '~a' in '~a'"
		     *letter-set-wild-card-char* affix-spec))
	  (let ((char-set (find char
				w-set :key #'letter-wild-card-char)))
	    (unless char-set
	      (error "~%undefined wild card ~a~A in '~a'" 
		     *letter-set-wild-card-char* char affix-spec))
	    (push char-set new-spec)))
	 (t (push char new-spec)))))
    (nreverse new-spec)))


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
	    (rule-name (morph-rule-name rule)))
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
		  (push (cons new-form rule-name)
			analysis-results)))))))
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
	 (already-bound (assoc var bindings)))
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
			 (cdr (assoc (letter-set-var letter) bindings))
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
	 (rule (find rule-name *morph-rule-set* :key #'morph-rule-name))
	 (rule-class (morph-rule-class rule))
	 (suffix-p (eql rule-class 'suffix)))
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
	    (return new-form)))))))
