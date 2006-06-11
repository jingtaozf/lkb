;;; Copyright (c) 1991-2005
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

;;; AAC April 2005 - rationalisation of morphology and MWEs 
;;; - also preparation for chart output from tokeniser

;;; Notes 1. removed ltemplates - I don't have a grammar etc to test
;;; this mechanism, but I rather think it will be superseded by the
;;; new approach when we get this fully implemented
;;; 2. sensible messages when missing lexical entries (or unknown words)
;;; 3. more or less minor FIXes as below (unify-in)

(in-package :lkb)

;;; This file implements the chart parser.

;;; The chart structure
;;;
;;; The chart is an array indexed by the vertex number
;;;
;;; v 0 gives edges ending at vertex v
;;; v 1 gives edges starting at vertex v
;;; 
;;; This is slightly inelegant in that the size of the array is set.

(defvar *parser-rules* nil)
(defvar *parser-lexical-rules* nil)

;;; *chart-limit* is defined in globals.lsp

(defvar *text* nil)
(defvar *sentence* nil)

(defparameter *tchart-max* 0
  "set by the tokeniser to the maximum vertex in the tchart")

(defparameter *chart-max* 0
  "set by the stem edge addition code to the maximum vertex in the chart")

(defvar *chart-dependencies*)
    
(defvar *chart-generation-counter* 0
  "a counter used by the user interface to make sure parse tree windows 
   etc have the current chart")

(defvar *generate-messages-for-all-unanalysed-tokens* nil)
(defvar *unanalysed-tokens* nil)

;;;
;;; recently added variant: use active key-driven parsing strategy; this seems
;;; to 
;;;
;;;   - perform better than the default unidirectional passive breadth-first
;;;     search (up to 40 % time reduction on longer VerbMobil sentences);
;;;   - simplify best-first search and generalize to more than just binary
;;;     branching rules;
;;;   - outperform passive best-first mode modestly.
;;;
;;; until tested somewhat better, the active parser is in experimental state,
;;; disabled by default, and hidden in `active.lsp'.
;;;                                                        (20-jun-99  -  oe)
;;;
;;; now that we have tested the active parser a bit (and people had the time
;;; to get used to the idea of active parsing :-), turn it on by default.
;;;                                                        (20-jan-00  -  oe)
(defvar *active-parsing-p* t)

(defvar *chart* (make-array (list *chart-limit* 2))) 

(defvar *tchart* (make-array (list *chart-limit* 2))) 
;;; for now it seems best to keep the token and morphophonology
;;; chart separate from the `real' one.  It seems possible that
;;; the number of vertices in *chart* may end up being
;;; greater than the number in *tchart* if we allow compounding.
;;; Keeping the charts distinct also avoids having to filter
;;; out the token edges and the morphop edges.

(defun make-tchart nil (make-array (list *chart-limit* 2)))

(defvar *parse-record* nil)

;;; *chart* contains lists of chart-configurations

(defstruct (chart-configuration) 
  begin edge roots end)
;;; this is specialised for the active parser (see active.lsp)
;;; begin is a vertex - edge is an edge 

;;; an edge is a structure
;;; it has the following properties:
;;; id - unique
;;; score - for best-first
;;; category - eg S VP etc - now a type
;;; rule - either the word (storms etc) or the grammar rule itself which has
;;;        been applied to create that edge  
;;; dag - the dag associated with the constituent
;;; odag - something to do with packing?
;;; dag-restricted - for packing
;;; leaves - orthography of whatever this edge has been formed from
;;; lex-ids - like leaves, but identifiers of whole structures
;;; parents - for active parsing
;;; children - daughter edges
;;; tchildren - token daughter edges (for debugging)
;;; orth-tdfs - fs containing orthography
;;; partial-tree - specification of affixes etc
;;; from - vertex
;;; to - vertex
;;; label - ?
;;; head - used in maxent.lisp
;;; cfrom - character position
;;; cto - character position
;;; string - original string - potentially redundant
;;;          because it could be recovered from cfrom and cto
;;;          but useful to keep it locally
;;; mrs - generation etc
;;;
;;; `foo,' `bar', and `baz' are junk slots for various pieces of code (e.g. the
;;; Redwoods annotation tool and PCFG estimation) to use at their discretion.
;;;                                                           (28-oct-02; oe)
;;; packed 
;;; equivalent 
;;; frozen 
;;; adjuncts 
;;; unpacking

(defstruct
   (edge
      (:constructor make-edge-x
                    (&key id score category rule dag odag 
                          (dag-restricted (when dag
                                            (restrict-fs (tdfs-indef dag))))
                          leaves lex-ids parents children tchildren
                          orth-tdfs
			  partial-tree
                          (from (when (edge-p (first children))
                                  (edge-from (first children))))
                          (to (when (edge-p (first (last children)))
                                (edge-to (first (last children)))))
                          label head
			  (cfrom (if (edge-p (first children))
				     (edge-cfrom (first children))
				   -1))
                          (cto (if (edge-p (first (last children)))
				   (edge-cto (first (last children)))
				 -1))
                          string mrs
                          foo bar baz
                          packed equivalent frozen adjuncts unpacking)))
   id score category rule dag odag dag-restricted leaves lex-ids
   parents children tchildren orth-tdfs partial-tree from to label head
   cfrom cto string mrs foo bar baz
   packed equivalent frozen adjuncts unpacking)

(defstruct (token-edge (:include edge))
  word maf-id xfrom xto)

(defstruct (morpho-stem-edge (:include edge))
  word stem current
  l-content ;; temporary hack (bmw)
  ) 

(defun make-edge (&rest rest)
  (apply #'make-edge-x rest))

(defmethod print-object ((instance chart-configuration) stream)
  (format stream "[~S ~S]" (chart-configuration-edge instance)
	  (chart-configuration-roots instance)))

(defmethod print-object ((instance edge) stream)
  (format 
   stream 
   "#[Edge # ~d: `~(~a~)' <~{~a~^ ~}>"
   (edge-id instance)
   (let ((rule (edge-rule instance)))
     (if rule 
       (typecase rule
         (string rule)
         (rule (rule-id rule)))
       (edge-category instance)))
   (loop 
       for child in (edge-children instance)
       collect (if (edge-p child) (edge-id child) "_")))
  ;; print cfrom/cto
  (with-slots (cfrom cto) instance
    (format stream "~:[~2*~; (~A c ~A) ~]"
	    (and cfrom cto) cfrom cto))
  (format stream "]"))

(defmethod print-object ((instance token-edge) stream)
  (format 
   stream 
   "#[Token edge # ~d: ~S ~S ~S"
   (token-edge-id instance)
   (token-edge-word instance)
   (token-edge-from instance)
   (token-edge-to instance))  
  ;; print cfrom/cto
  (with-slots (cfrom cto) instance
    (format stream "~:[~2*~; (~A c ~A) ~]"
	    (and cfrom cto) cfrom cto))
  (format stream "]"))

(defmethod print-object ((instance morpho-stem-edge) stream)
  (format 
   stream 
   "#[Morph edge # ~d: ~S ~S ~S ~S ~A"
   (morpho-stem-edge-id instance)
   (morpho-stem-edge-word instance)
   (morpho-stem-edge-current instance)
   (morpho-stem-edge-from instance)
   (morpho-stem-edge-to instance)
   (morpho-stem-edge-partial-tree instance))
  ;; print cfrom/cto
  (with-slots (cfrom cto) instance
    (format stream "~:[~2*~; (~A c ~A) ~]"
	    (and cfrom cto) cfrom cto))
  (format stream "]"))

(defvar *edge-id* 0)

;;;
;;; when unpacking, edges are a lot cheaper, as we are deterministic at this
;;; point (e.g. there will be very few failing unifications, if any).  hence,
;;; impose a separate limit for unpacking, although it could be argued that we
;;; should count on top of the edges built already (like PET does).  i prefer
;;; independent counting, however, since active edges (in generation at least)
;;; are also included in the edge count, but we could dispose of them when we
;;; move on into unpacking.                                    (26-nov-04; oe)
;;;
(defparameter *unpack-edge-allowance* 25000)

(defparameter %edge-allowance% 0)

(defun next-edge (&optional type)
  (if (eq type :unpack)
    (incf (statistics-uedges *statistics*))
    (incf (statistics-pedges *statistics*)))
  (when (eq type :unpack)
    ;;
    ;; _fix_me_
    ;; better generalize all of this, maybe encapsulate state variables.
    ;;                                                       (16-dec-03; oe)
    (when (> (incf %edge-allowance%) *unpack-edge-allowance*)
      (error "~%Edge allowance overrun (~a)" *edge-id*)))
  (when (> *edge-id* (+ *maximum-number-of-edges* %edge-allowance%))
    (error "~%Probable runaway rule: parse/generate aborted ~
            (see documentation for *maximum-number-of-edges*)"))
  (incf *edge-id*))

;;;
;;; keep track of lexical items used in each parse: whenever we use the same
;;; lexical entry the second time, we better copy it first to avoid spurious
;;; reentrancies.  this mechanism should suffice to remove a test in the parser
;;; that currently checks for duplicates among the edges feeding into a rule.
;;; --- come to think of it, this might also be useful in resetting temporary
;;; pointers in feature structures that are part of the grammar ...
;;;
(defvar *lexical-entries-used* nil)

;;; *****************************************************************
;;;
;;; Agenda stuff - the agenda is represented as a heap (as in of Cormen,
;;; Leiserson, and Rivest).  The heap is a tree stored in an array: the car of
;;; each array element is its key and the cdr is the value.
;;;
;;; ******************************************************************

(defmacro parent (i)
  `(the fixnum (floor (the fixnum ,i) 2)))

(defmacro left (i)
  `(the fixnum (* (the fixnum ,i) 2)))

(defmacro right (i)
  `(the fixnum (1+ (* (the fixnum ,i) 2))))

(defmacro fast-aref (a x)
  `(aref (the (simple-array t (*)) ,a) ,x))

(defmacro heap-size (a)
  `(the fixnum (fast-aref ,a 0)))

(defun heapify (a i)
  (let* ((l (left i))
	 (r (right i))
	 (largest (if (and (<= l (heap-size a))
			   (> (car (fast-aref a l)) 
			      (car (fast-aref a r))))
		      l
		    i)))
    (when (and (<= r (heap-size a))
	       (> (car (fast-aref a r)) (car (fast-aref a largest))))
      (setq largest r))
    (unless (eql largest i)
      (rotatef (fast-aref a i) (fast-aref a largest))
      (heapify a largest))))

(defun heap-insert (a key value)
  (incf (heap-size a))
  (when (>= (heap-size a)  (array-dimension a 0))
    (error "~%Too many pending tasks, probable runaway rule: parse/generate aborted ~
            (see documentation for *maximum-number-of-tasks*)"))
  (loop 
      with i = (heap-size a)
      while (and (> i 1)
		 (< (car (fast-aref a (parent i))) key))
      do (setf (fast-aref a i) (fast-aref a (parent i)))
	 (setf i (parent i))
      finally (setf (fast-aref a i) (cons key value)))
  value)

(defun heap-extract-max (a)
  (when (< (heap-size a) 1)
    (error "~%This shouldn't happen!  Something's wrong with the parser."))
  (let ((max (shiftf (fast-aref a 1) (fast-aref a (heap-size a)))))
    (decf (heap-size a))
    (heapify a 1)
    (let ((entry (rest max)))
      (setf (rest max) nil)
      entry)))

(defun heap-extract-max-full (a)
  (when (< (heap-size a) 1)
    (error "~%This shouldn't happen!  Something's wrong with the parser."))
  (let ((max (shiftf (fast-aref a 1) (fast-aref a (heap-size a)))))
    (decf (heap-size a))
    (heapify a 1)
    (let ((key (first max))
          (value (rest max)))
      (setf (rest max) nil)
      (cons key value))))

(defun make-heap ()
  (let ((heap (make-array (list *maximum-number-of-tasks*))))
    (setf (aref heap 0) 0)
    heap))

(defun empty-heap (a)
  (zerop (aref a 0)))

(defun flush-heap (a)
  (setf (heap-size a) 0))

(defvar *agenda* (make-heap))

(defmacro with-agenda (priority &body body)
  `(if ,priority
       (heap-insert *agenda* ,priority #'(lambda () ,@body))
     (progn
       ,@body)))

(defun clear-chart nil
   (incf *chart-generation-counter*)
   (setf *parse-record* nil) 
   (loop for i from 0 upto (1- *chart-limit*)
       do (setf (aref *chart* i 0) nil)
	  (setf (aref *tchart* i 0) nil)
	  (setf (aref *chart* i 1) nil)
	  (setf (aref *tchart* i 1) nil))
   (setf *tchart-max* 0)
   (setf *chart-max* 0)
   (setf *edge-id* 0)
   (setf %edge-allowance% 0)
   (when *active-parsing-p* (clear-achart)))

(defvar *cached-category-abbs* nil
  "variable used in output to avoid recomputation of tree nodes")

#+(or :allegro :lispworks)
(defvar *parser-lock* (mp:make-process-lock))

(defmacro with-parser-lock ((&optional foo) &body body)
  (declare (ignore foo))
  #+(or :allegro :lispworks)
  `(mp:with-process-lock (*parser-lock*)
     ,@body)
  #-(or :allegro :lispworks)
  `(progn ,@body))

;;;
;;; satisfy measurement fetish: list used to store (cpu) time used to find
;;; individual readings: bottom element is start time for parse(), topmost is
;;; end time; in best-first mode, additional elements record time for finding
;;; an analysis, one per reading.                          (24-feb-99  -  oe)
;;;
(defparameter *parse-times* nil)

(defparameter *show-parse-p* t)

(defvar *brackets-list* nil)

;;; ****************************************************************
;;;
;;; Entry point to this group of functions is parse which is passed the
;;; sentence as a list of strings and is called from the top level
;;;
;;; ****************************************************************

;;; Revised approach to tokenisation, morphology and MWES
;;; AAC April 2005
;;; The chart is used for all of these replacing various
;;; subsidiary structures.  
;;; 
;;; The standard system has four phases:
;;; 
;;; Phase 1 - token list is used to instantiate the chart
;;; for simple tokenisers.  For more complex cases, the tokeniser
;;; outputs a chart (possibly in XML if a separate tokeniser).
;;; *tchart* is instantiated with token-edges
;;;
;;; Phase 2 - the morphophonology component instantiates the
;;; chart with possible stems plus affixes.  In the revised
;;; version of the LKB this is done on a rule-by-rule basis.
;;; *tchart* is instantiated with morpho-stem edges
;;; The edges are associated with a partially specified derivation tree. 
;;; In the case where an external morphophonology component operates
;;; (such as sppp) *tchart* is instantiated with morpho-stem-edges
;;; with stem set
;;; 
;;; Phase 3 - lexical lookup including MWE lookup
;;; - instantiate *chart* with normal edges
;;;
;;; Phase 4 - parsing - application of morphosyntax and lexical rules
;;; along with grammar rules.  The morphosyntax rules respect
;;; the partially specified derivation tree from phase 2.
;;; Instantiate the chart with edges
;;;
;;; The following are possible variations (in principle)
;;;
;;; Input source:
;;; 1a) user input from LKB
;;; 1b) real input (with markup)
;;; 1c) test suite input
;;; 
;;; Preprocessor:
;;; 2a) tokens only: `standard' preprocessor (strings)
;;;                : `char' preprocessor (marked up with character positions)
;;; 2b) tokens and external morphophonology
;;;  i) partial-tree morphology (current sppp case)
;;;     :with-tokeniser-partial-tree 
;;;  ii) `word' morphosyntax
;;;       i.e., morphology specified as a chart with morphemes separated
;;;     :with-tokeniser-retokenise
;;; Partial tree morphology (for 2a case):
;;; 3a) (new) internal LKB morphology (one rule at a time)
;;;     the default
;;; 3b) external morphology (one rule at a time)
;;;     :external-rule-by-rule 
;;; 3c) external morphology (full partial tree, as old LKB)
;;;     :external-partial-tree

(defun check-morph-options (input)
  ;;; make sure that plausible functions have been specified
  ;;; try and deal with all ickinesses until interface is 
  ;;; clarified here
  (unless (member *morph-option*
		  '(:default :external-rule-by-rule 
		    :external-partial-tree :with-tokeniser-partial-tree  
		    :with-tokeniser-retokenise))
    (format t "~%Unrecognised *morph-option* switch ~A: resetting to default"
	    *morph-option*)
    (setf *morph-option* :default))
  (when (and (listp input) (consp (first input)))
    ;;; current test for sppp
    (setf *morph-option* :with-tokeniser-partial-tree))
  (if (or (eql *morph-option* :external-rule-by-rule)
	    (eql *morph-option* :external-partial-tree))
    (unless *foreign-morph-fn*
      (format t "~%*morph-option* ~A requires external morphology function 
*foreign-morph-fn* which is not supplied - resetting *morph-option* to default"
	      *morph-option*)
      (setf *morph-option* :default))
    (when *foreign-morph-fn*
      (format t "~%*morph-option* ~A requires *foreign-morph-fn* to be nil - resetting it"
	      *morph-option*)
      (setf *foreign-morph-fn* nil))))  

(defvar *maf-p* nil)
(defvar *abort-parse-after-morphosyntax* nil)

;; example input: 
;; - basic: "the" "dog" "barks"
;; - bracketed: "Kim" "(" "(" "likes" ")" "Sandy" ")"
;; - chared: #S(CHARED-WORD :WORD "The" :CFROM 0 :CTO 2) 
;;           #S(CHARED-WORD :WORD "cat" :CFROM 4 :CTO 6)
;;           #S(CHARED-WORD :WORD "barks" :CFROM 8 :CTO 12))
;; - (s)(m)af xml: "<?xml version='1.0' encoding='UTF8'?><!DOCTYPE maf SYSTEM 'maf.dtd'><maf document='text.xml' addressing='char'><olac:olac xmlns:olac='http://www.language-archives.org/OLAC/1.0/' xmlns='http://purl.org/dc/elements/1.1/' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xsi:schemaLocation='http://www.language-archives.org/OLAC/1.0/ http://www.language-archives.org/OLAC/1.0/olac.xsd'><creator>LKB</creator><created>20:46:50 1/18/2006 (UTC)</created></olac:olac><fsm init='v0' final='v3'><state id='v0'/><state id='v1'/><state id='v2'/><state id='v3'/><token id='t1' from='0' to='3' value='the' source='v0' target='v1'/><token id='t2' from='4' to='7' value='dog' source='v1' target='v2'/><token id='t3' from='8' to='13' value='barks' source='v2' target='v3'/><wordForm form='the' tag='' daughters='t1' source='v0' target='v1'><fs><f name='stem'>THE</f></fs></wordForm><wordForm form='dog' tag='' daughters='t2' source='v1' target='v2'><fs><f name='stem'>DOG</f></fs></wordForm><wordForm form='barks' tag='PLUR_NOUN_ORULE' daughters='t3' source='v2' target='v3'><fs><f name='stem'>BARK</f><f name='partial-tree'>((PLUR_NOUN_ORULE &quot;BARKS&quot;))</f></fs></wordForm><wordForm form='barks' tag='THIRD_SG_FIN_VERB_ORULE' daughters='t3' source='v2' target='v3'><fs><f name='stem'>BARK</f><f name='partial-tree'>((THIRD_SG_FIN_VERB_ORULE &quot;BARKS&quot;))</f></fs></wordForm></fsm></maf>"
;; - SAF object: #[SAF]
;; - sppp: ((:END . 1) 
;;          (:START . 0) 
;;          (:ANALYSES 
;;            ((:RULES) 
;;             (:INFLECTION) 
;;             (:STEM . "kim"))) 
;;          (:TO . 3)
;;          (:FROM . 0)
;;          (:FORM . "kim"))
;;         ((:END . 2) 
;;          (:START . 1)
;;          (:ANALYSES
;;            ((:RULES 
;;              ((:FORM . "sleeps") 
;;               (:ID . PLUR_NOUN_INFL_RULE))
;;              ((:FORM . "sleeps.") 
;;               (:ID . PUNCT_PERIOD_RULE))) 
;;             (:INFLECTION) 
;;             (:STEM . "sleep"))
;;            ((:RULES 
;;              ((:FORM . "sleeps") 
;;               (:ID . THIRD_SG_FIN_VERB_INFL_RULE))
;;              ((:FORM . "sleeps.") 
;;               (:ID . PUNCT_PERIOD_RULE)))
;;             (:INFLECTION) 
;;             (:STEM . "sleep"))) 
;;          (:TO . 10) 
;;          (:FROM . 4) 
;;          (:FORM . "sleeps."))
(defun parse (input &optional 
                              (show-parse-p *show-parse-p*) 
                              (first-only-p *first-only-p*))
  ;;
  ;; keep track of mutual dependencies between various configurations:
  ;;   - input bracketing is only available in passive mode;
  ;;   - passive best-first restricted to unary and binary rules.
  ;;
  (check-morph-options input)
  (reset-statistics)
  (let* ((*active-parsing-p* (if *bracketing-p* nil *active-parsing-p*))
         (first-only-p (if (and first-only-p 
                                (null *active-parsing-p*)
                                (greater-than-binary-p))
                         (format 
                          t 
                          "~&Passive best-first mode only available for ~
                           unary and binary rules.~%~
                           Disabling best-first mode: setting ~
                           *first-only-p* to `nil'.~%")
                         first-only-p))
	 (input (if #+:maf (xml-p input) #-:maf nil
		    #+:maf (xml-to-saf-object input) #-:maf nil;; extract object from maf xml
		    input))
	 (maf-p #+:maf (saf-p input) #-:maf nil)
	 (*maf-p* maf-p)
         ;;
         ;; input originating from SPPP is a set of SPPP tokens
         ;;
         (spppp (and (listp input) (consp (first input))))
	 (string-p (stringp input))
	 length-user-input)
    (when string-p
	(format t "~&WARNING: [parse] invalid input: '~a'" input)
	(return-from parse))
    ;; eg. user-input -> ("the" "dog" "barks")
    (multiple-value-bind (user-input brackets-list)
        (if (and *bracketing-p* (not maf-p) (not spppp))
          (initialise-bracket-list input)
          (values input
		  nil))
      (setf length-user-input
        (cond
         (maf-p
          #+:maf (get-smaf-lattice-size user-input) #-:maf nil)
         (spppp (loop
                    for token in input
                    maximize (rest (assoc :end token))))
         (t (length user-input))))
      
      (when (> length-user-input *chart-limit*)
        (error "~%Sentence `~a' too long - ~A words maximum ~
                (see documentation for *chart-limit*)" 
               user-input *chart-limit*))

      (let ((*brackets-list* brackets-list)
            (*parser-rules* (get-matching-rules nil nil))
            (*parser-lexical-rules* (get-matching-lex-rules nil))
            (*lexical-entries-used* nil)
            (*minimal-vertex* 0)
            (*maximal-vertex* length-user-input)
            ;;
            ;; shadow global variable to allow best-first mode to decrement for
            ;; each result found; eliminates need for additional result count.
            ;;                                              (22-jan-00  -  oe)
            (*first-only-p*
             (cond
              ((null first-only-p) nil)
              ((and (numberp first-only-p) (zerop first-only-p)) nil)
              ((numberp first-only-p) first-only-p)
              (t 1))))
        (declare (special *minimal-vertex* *maximal-vertex*))
        (with-parser-lock ()
          (flush-heap *agenda*)
          (clear-chart)
          (setf *cached-category-abbs* nil)
          (setf *parse-record* nil)
          (setf *parse-times* (list (get-internal-run-time)))
          (let ((*safe-not-to-copy-p* t))
	    ;; instantiate token chart
	    (instantiate-chart-with-tokens user-input)
	    (ecase *morph-option*
	      (:default (instantiate-chart-with-morphop))
	      (:external-rule-by-rule 
	       (instantiate-chart-with-morphop))
	      ;;; *foreign-morph-fn* is set and will be called
	      (:external-partial-tree
	       (instantiate-chart-with-morpho-stem-edges))
	      (:with-tokeniser-partial-tree nil)
	      (:with-tokeniser-retokenise nil))
	    (instantiate-chart-with-stems-and-multiwords)
	    (when *generate-messages-for-all-unanalysed-tokens*
	      (generate-messages-for-all-unanalysed-tokens *tchart*))
	    (when *abort-parse-after-morphosyntax*
	      (return-from parse))
            (catch :best-first
              (add-words-to-chart (and first-only-p (null *active-parsing-p*)
                                       (cons *minimal-vertex* *maximal-vertex*)))
              (if *active-parsing-p*
                (complete-chart)
                (loop 
                    until (empty-heap *agenda*)
                    do (funcall (heap-extract-max *agenda*)))))
            (unless first-only-p
              ;;
              ;; best-first (passive or active mode) has already done this
              ;; incrementally in the parse loop
              ;;
              (setf *parse-record* 
                (find-spanning-edges *minimal-vertex* *maximal-vertex*))))
          (push (get-internal-run-time) *parse-times*))
        (when show-parse-p (show-parse))
        (values
         (statistics-etasks *statistics*)
         (statistics-stasks *statistics*)
         -1
         (statistics-ftasks *statistics*)
         (statistics-mtasks *statistics*))))))

(defun file-xml-p (filename)
  (xml-p
   (read-file-to-string filename :numchars 5)))

(defun xml-p (input)
  (and (stringp input) 
       (> (length input) 4)
       (string= "<?xml" (subseq input 0 5))))

;;; *****************************************************
;;;
;;; Tokenisation
;;;
;;; *****************************************************

(defstruct chared-word
  word
  cfrom
  cto)

(defvar mrs::*psoa-liszt-path*)

(defun set-characterization-tdfs (tdfs cfrom cto)
  (cond
   ((and cfrom cto)
    (let ((indef (set-characterization-indef (tdfs-indef tdfs) cfrom cto)))
      (when indef ;; is this necessary ???
	(make-tdfs :indef indef
		   :tail (copy-tdfs-tails tdfs)))))
   (t
    tdfs)))

#+:null
(defun set-characterization-tdfs-within-unification-context (tdfs cfrom cto)
  (cond
   ((and cfrom cto)
    (let ((indef (set-characterization-indef-within-unification-context (tdfs-indef tdfs) cfrom cto)))
      (when indef ;; is this necessary ???
	(make-tdfs :indef indef
		   :tail (copy-tdfs-tails tdfs)))))
   (t
    tdfs)))

#+:null  
(defun set-characterization-tdfs-within-unification-context (tdfs cfrom cto)
  (cond
   ((and cfrom cto)
    (set-characterization-indef-within-unification-context (tdfs-indef tdfs) cfrom cto))
   (t
    tdfs)))
  
(defun set-characterization-indef (indef-dag cfrom cto)
  (with-unification-context (dummy)
    (set-characterization-indef-within-unification-context indef-dag cfrom cto)
    (copy-dag indef-dag)))

;; must call copy-dag to get result
#+:null
(defun set-characterization-indef-within-unification-context (indef-dag cfrom cto)
    (let* ((cfrom-str (2-str cfrom))
	   (cto-str (2-str cto))
	   (rels (mrs::get-rels-list indef-dag))
	   (message (mrs::get-message indef-dag)))
      (when message
	(let* ((message-cfrom-dag (mrs::path-value message '(CFROM)))
	       (message-cto-dag (mrs::path-value message '(CTO))))
	  (when (and message-cfrom-dag message-cto-dag
		     (or (eq *toptype* (dag-type message-cfrom-dag)) 
			 (eq *toptype* (dag-type message-cto-dag))))
	    (setf (dag-new-type message-cfrom-dag) cfrom-str)
	    (setf (dag-new-type message-cto-dag) cto-str))))
      (loop
	  for rel in rels
	  for rel-cfrom-dag = (mrs::path-value rel '(CFROM))
	  for rel-cto-dag = (mrs::path-value rel '(CTO))
	  when (or (eq *toptype* (dag-type rel-cfrom-dag)) 
		   (eq *toptype* (dag-type rel-cto-dag)))
	  do 
	    (setf (dag-new-type rel-cfrom-dag) cfrom-str)
	    (setf (dag-new-type rel-cto-dag) cto-str))))

(defun set-characterization-indef-within-unification-context (indef-dag cfrom cto)
  (let ((*safe-not-to-copy* nil))
    (setf *safe-not-to-copy* *safe-not-to-copy*) ;; to avoid compiler warning
    (let* ((replace-alist (list (cons 'cfrom  
				      (format nil "~A" cfrom))
				(cons 'cto  
				      (format nil "~A" cto)))))
	     ;;; need to restrict replacement to the RELS list
	     ;;; otherwise get MSG clashes
      (replace-dag-types indef-dag 
			 (append mrs::*initial-semantics-path*
				 mrs::*psoa-liszt-path*) 
			 replace-alist))))

;; imported from erg/lkb/xmlify.lsp
;; cfrom/cto replacement could be made more precise
;; note that instantiated CFROM/CTO show up in parse tree FS
;;  only for the lexically grounded preds 
#+:null
(defun set-characterization (tdfs cfrom cto)
  (let ((*safe-not-to-copy* nil))
    (setf *safe-not-to-copy* *safe-not-to-copy*) ;; to avoid compiler warning
    (when (and (tdfs-p tdfs) (integerp cfrom)
	       (> cfrom -1) (integerp cto)
	       (> cto -1)) 
      (let* ((replace-alist (list (cons 'cfrom  
					(format nil "~A" cfrom))
				  (cons 'cto  
					(format nil "~A" cto))))
	     (new-dag (tdfs-indef tdfs))
	     ;;; need to restrict replacement to the RELS list
	     ;;; otherwise get MSG clashes
	     (retyped-dag
	      (replace-dag-types new-dag 
				 (append mrs::*initial-semantics-path*
				       mrs::*psoa-liszt-path*) 
				 replace-alist)))
	(when retyped-dag
	  (setf (tdfs-indef tdfs) retyped-dag))))))

(defun instantiate-chart-with-tokens (preprocessed-input)
  ;;; this is for the trivial case where the
  ;;; input is a list with no ambiguity about token boundaries
  ;;; (bmw) the above is no longer the case
  ;;; FIX - we need a better method of switching here
  #+:maf
  (if (saf-p preprocessed-input)
      (return-from instantiate-chart-with-tokens
	(saf-setup-morphs preprocessed-input)))
  (if (consp (first preprocessed-input))
      (sppp-setup-morphs preprocessed-input)
    ;; _default case_
    ;;eg. (instantiate-chart-with-tokens ("The" "dog" "barks"))
    ;; calls: (add-token-edge "The" "THE" 0 1 nil nil)
    ;;        (add-token-edge "dog" "DOG" 0 1 nil nil)
    ;;        (add-token-edge "the" "BARKS" 0 1 nil nil)
    ;; returns: *tchart*
    (let ((current 0)
	  (chared-words-p (chared-word-p (first preprocessed-input))))
      (dolist (token preprocessed-input)
        (let* ((base-word 
		(if chared-words-p 
		    (chared-word-word token)
		  token))
	       (word (string-upcase base-word))
	       (cfrom (if chared-words-p
			  (chared-word-cfrom token)
			-1))
	       (cto (if chared-words-p
			  (chared-word-cto token)
			-1))
               (new (+ current 1)))
	  (add-token-edge base-word word current new cfrom cto)
	  (setf current new)))))
  *tchart*)

(defun instantiate-token-chart (chart-spec)
  ;;; so far this is only for testing
  ;;; chart-spec should be a list each element of which is
  ;;; a list
  ;;; (left-vertex right-vertex cfrom cto string)
  (clear-chart)
  (dolist (edge-spec chart-spec)
    (add-token-edge (string-upcase (fifth edge-spec))
		    (fifth edge-spec)
		    (first edge-spec)
		    (second edge-spec)
		    (third edge-spec)
		    (fourth edge-spec))))

;; eg. (add-token-edge "The" "THE" 0 1 nil nil)
;; 
(defun add-token-edge (base-word word from to cfrom cto)
  (when (> to *tchart-max*)
    (setf *tchart-max* to))
  (let ((existing-ccs (aref *tchart* to 0))
	(edge (make-token-edge :id (next-edge)
			       :string base-word
			       :word word
			       :leaves (list base-word)
			       :from from
			       :to to
			       :cfrom cfrom
			       :cto cto)))
    (unless (token-edge-match edge existing-ccs)
      (let ((cc (make-chart-configuration :begin from
					  :edge edge
					  :end to)))
	  (push cc (aref *tchart* to 0))
	  (push cc (aref *tchart* from 1)))
      edge)))


(defun token-edge-match (edge cclist)
  (member-if  #'(lambda (x)
		       (and (eql (edge-from edge) 
				 (chart-configuration-begin x))
			    (eql (edge-to edge) 
				 (chart-configuration-end x))
			    (equal (token-edge-word edge)
				   (token-edge-word
				    (chart-configuration-edge x)))))
	      cclist))
				  



;;; *****************************************************
;;;
;;; Morphophonology
;;;
;;; *****************************************************

;;; We have a chart instantiated with token-edges
;;; This phase calls the LKB built-in spelling component (or some other 
;;; morphological engine), which ultimately provides a stem and a tree of
;;; rules to apply to it.
;;; The revised LKB morph code does this one rule at a time but the case
;;; where a complete analysis is done in one step is also supported.
;;;

;;; partial-tree handling
;;; 
;;;
;;; FIX 
;;; currently we assume that a partial tree is in fact a
;;; list of partial-tree nodes, but this will have to change
;;; when we allow for compounding.  To try and make this simpler
;;; the code mostly abstracts from the partial tree encoding details
;;; search for WARNING for exceptions

(defmacro make-pt-node (rule str)
  `(list ,rule ,str))

(defmacro pt-node-rule (ptnode)
  `(car ,ptnode))

(defmacro pt-node-string (ptnode)
  `(cadr ,ptnode))

(defmacro add-pt-node-to-tree (ptnode partial-tree)
  ;;; sometimes called with partial-tree being nil
  `(cons ,ptnode ,partial-tree))

(defmacro max-one-deep-p (partial-tree)
  ;;; may be called with partial-tree being nil
  `(not (cdr ,partial-tree)))

(defmacro partial-trees-equal (partial-tree1 partial-tree2)
  `(tree-equal ,partial-tree1 ,partial-tree2))

;;; end partial tree handling
    
;;; start of code specific to default *morph-option* (i.e., :default)
;;; or :external-rule-by-rule 

(defparameter *morph-agenda* nil)

(defparameter *morphophon-cache* nil)

(defun instantiate-chart-with-morphop nil
  (dotimes (current *tchart-max*)
    ;; for each left vertex in chart
    (let ((token-ccs (aref *tchart* current 1)))
      ;; collect edges incident on vertex
      (dolist (token-cc token-ccs)
	;; for each such edge
	(let* ((token-edge (chart-configuration-edge token-cc))
	       (word (token-edge-word token-edge)))
	  (add-morpho-partial-edges 
	   word
	   token-edge)))))
  *tchart*)
  

#|
Redoing this from the initial version:

Partial-tree is a set of partial trees
(although for now, unpack when we add a stem edge because I don't want to 
rewrite the rest of the code immediately)

 complication 1 - analyses that have no morphological effect, contributed by
 irregular forms (or otherwise)

 call on "BET" with rules nil - find passive, psp, past
    recurse on BET with rules (passive psp past) - if nothing can feed 
    any of these rules, we will stop there

    complication 2 - rules that extend the length of the string - 
    e.g. Spanish stems are longer than the inflected forms.  
    As with null effects, this doesn't matter unless we get into a 
    recursive situation.

    complication 3 - as we're analysing, we want to block impossible 
    analyses based on the rule feeding, but when we return to a string 
    having analysed with different rules, we may need to redo.  
    This led to a potential bug in the old code.

If affixes always shortened the string, we could order them so that we
never revisited a string we'd analysed.  i.e. we could search breadth first,
longest string first.  This isn't the case, but it's probably close enough
that the potential reanalysis won't hurt.

Use an agenda so that we can investigate possibilities breadth first.

Abstract example:

initial agenda contains

((str (nil)))

this is popped

(morph-analyse-reg-irreg str) =>
((str1 . r1a)
 (str1 . r1b)
 (str2 . r2a)
 (str2 . r2b)
 (str . r3a)
 (str . r3b))

In the agenda, these are packed and sort by length of string - 
assume str1 > str and str2 < str 

for the special case where the morphological analysis allows a 
zero affixation, we bypassing the agenda mechanism and
add them as partial trees to give the set of partial trees 
(nil (r2a.str) (r2b.str))

(note r1a.str is an abbreviation for (r1a str) here - i.e. the 
      nodes in partial trees are actually lists of rule and form)
note also that we need to have the element nil in the partial-tree-set

suppose that r1a feeds r2a.str and r1b.str feeds r2a.str but none 
of the others feed

agenda

((str1 ((r1a.str) (r1b.str) (r1a.str r2a.str) (r1b.str r2a.str))
 (str2 ((r2a.str) (r2b.str))))

str1 is top, so we investigate it.  

agenda is

((str2 ((r2a.str) (r2b.str))))

since we pop

Suppose:
(morph-analyse-reg-irreg str1) =>

((str2 . r2c))

We add to the agenda for str2

((str2 ((r2a.str) (r2b.str) (r2c.str1 r1a.str) (r2c.str1 r1b.str))))

Because the morphophonology could have affixed forms that were shorter
than the stem, we can't guarantee not to redo work.  For instance,
in the example above, suppose

(morph-analyse-reg-irreg str2) =>
((str1 . r2d))

we've taken str1 off the agenda, so we'll end up reanalysing it.
However, the morphophon results are cached, so the effects are 
relatively limited.

|#

;;; Morphology agenda

(defmacro make-morph-agenda-item (str partial-tree-set)
  `(cons ,str ,partial-tree-set))

(defmacro morph-agenda-string (agenda-item)
  `(car ,agenda-item))

(defmacro morph-agenda-partial-tree-set (agenda-item)
  `(cdr ,agenda-item))

(defmacro morph-agenda-latest-rules (agenda-item)
   ;;; WARNING: assumes partial tree is a list!
  `(loop for pt in (morph-agenda-partial-tree-set ,agenda-item)
	 collect (pt-node-rule (car pt))))

(defun morph-agenda-match (str)
  (dolist (item *morph-agenda*)
    (when (string-equal str (morph-agenda-string item))
      (return item))))

(defun insert-morph-agenda-item (str partial-tree-set)
  ;;; if the string matches an existing item, then add the
  ;;; partial trees into that record.  Otherwise, insert
  ;;; in string length order, longest first
  (incf (statistics-mtasks *statistics*))
    (if *morph-agenda* 
	(let* ((strlength (length str))
	       (checked nil)
	       (new-agenda
		(do ((remainder *morph-agenda* (cdr remainder)))
		    ((not remainder) nil)
		  (let ((item (car remainder)))
		    (cond ((> strlength (length (morph-agenda-string item)))
			   (return 
			     (append (nreverse checked)
				     (list
				      (make-morph-agenda-item 
				       str 
				       partial-tree-set))
				     remainder)))
			  ((string-equal str (morph-agenda-string item))
			   (setf (morph-agenda-partial-tree-set item)
			     (append partial-tree-set
				     (morph-agenda-partial-tree-set item)))
			   ;;; don't think we need to test for duplicates here
			   (return *morph-agenda*))
			  (t 
			   (push item checked)))))))
	  (setf *morph-agenda* 
	    (or new-agenda 
		(nreverse 
		 (cons (make-morph-agenda-item str partial-tree-set)
		       checked)))))
	  (setf *morph-agenda* 
	    (list 
	     (make-morph-agenda-item str partial-tree-set)))))

;;; end agenda functions

;;; Main code

(defun add-morpho-partial-edges (unanalysed token-edge)
  (setf *morph-agenda* nil)
  (setf *morphophon-cache* nil)
  (insert-morph-agenda-item unanalysed (list nil))
  (loop (unless *morph-agenda* (return nil))
    (let* ((agenda-item (pop *morph-agenda*)))
      (analyse-agenda-item agenda-item token-edge))))
  
(defun analyse-agenda-item (agenda-item token-edge)
  (let* ((unanalysed (morph-agenda-string agenda-item))
	 (partial-tree-set (morph-agenda-partial-tree-set agenda-item))
	 (last-rule-ids (morph-agenda-latest-rules agenda-item))
	 (partial-trees-plus-nulls partial-tree-set))
    (when (dolist (last-rule-id last-rule-ids)
	    (when
		(or (null last-rule-id)
		    (spelling-rule-feeds-p last-rule-id)) 
	      (return t)))
	  ;;; no point continuing to morph-analyse if there's no
	  ;;; rule that changes morphology that can feed the
	  ;;; one we've seen     
      (let ((one-steps (morph-analyse-reg-irreg unanalysed)))
	(setf partial-trees-plus-nulls
	  (null-morphophon-combinations unanalysed
	   (loop for morph-pair in one-steps
	       when (string-equal (car morph-pair) unanalysed)
	       collect (cdr morph-pair))
	   partial-tree-set))
;;; (morph-analyse-reg-irreg "CANNED") returns
;;; (("CAN" . PAST-V_IRULE) ("CAN" . PSP-V_IRULE) ("CANNED" . MY_RULE))
;;; the new analyses get merged here with each other and the agenda
	(dolist (morph-pair one-steps)
	  (unless (string-equal (car morph-pair) unanalysed)
	    (let* ((newstr (car morph-pair))
		   (rule (cdr morph-pair))
		   (rule-entry (or (get-lex-rule-entry rule)
				   (error "~%Rule ~A returned from morph analyser is undefined (error in grammar loading?)" rule)))
		   (ptnode (make-pt-node rule unanalysed))
		   (new-partial-tree-set
			(loop for partial-tree in partial-trees-plus-nulls
			    when
			      (or 
				  (null partial-tree)
				  (check-nosp-feeding 
				   (get-lex-rule-entry 
				    (pt-node-rule (car partial-tree)))
			       ;;; WARNING
				   rule-entry))
			    collect 
			      (progn 
				(when (>= (length partial-tree)
					  *maximal-lex-rule-applications*)
				  (error "~%Probable runaway morphological rule in ~A: analysis aborted ~
            (see documentation for *maximal-lex-rule-applications*)" partial-tree))
				(add-pt-node-to-tree ptnode partial-tree)))))
	      (when new-partial-tree-set
		(insert-morph-agenda-item newstr new-partial-tree-set)))))))
    (add-as-unanalysed unanalysed token-edge partial-trees-plus-nulls)))
    
(defun add-as-unanalysed (unanalysed token-edge partial-tree-set)
  ;;; try and terminate the recursion, recording the analyses via
  ;;; the partial tree mechanism.  Unpack the partial trees and add a 
  ;;; new edge for each for now.
  (when (lookup-word *lexicon* unanalysed)
    (dolist (partial-tree partial-tree-set)
      (add-morpho-stem-edge-from-token-edge unanalysed partial-tree 
					    token-edge))))

(defun morph-analyse-reg-irreg (unanalysed)
  (or (cdr (assoc unanalysed 
		  *morphophon-cache* :test #'string-equal))
      (let* ((res
	     (if *foreign-morph-fn*
		 (apply *foreign-morph-fn* (list unanalysed))
	       (append (find-irregular-morphs unanalysed)
		       (one-step-morph-analyse unanalysed))))
	     (filtered-res 
	      (loop for poss in res
		  when 
		    (morph-not-blocked-p unanalysed (car poss)
					 (cdr poss))
		  collect poss)))
	(push (cons unanalysed filtered-res) *morphophon-cache*)
	filtered-res)))

(defun morph-not-blocked-p (surface underlying rule)
  ;;; given a pattern (* ed) (!ty !tied) (e ed) (!t!v!c !t!v!c!ced) 
  ;;; we may only want to match the most specific case
  ;;; for instance, given `hated' we don't want to match this to the
  ;;; stem `hat' because it should have the (!t!v!c !t!v!c!ced) pattern
  ;;; Similarly, if we have an irregular form `slept' we don't
  ;;; want to match `sleeped'
  ;;; *irregular-forms-only-p* (if only for backward compatibility)
  (or *foreign-morph-fn*
      ;;; we assume no blocking with external morphology
      (if (or *most-specific-only-p* *irregular-forms-only-p*)
	  (let* ((irreg-surface-list
		  (morph-matches-irreg-list underlying rule)))
	    (if irreg-surface-list
	    ;;; only OK if the specified form is one of the irregular
	    ;;; forms
		(member surface irreg-surface-list :test #'equal)
	      ;; no irreg form
	      (if *most-specific-only-p*
	      ;;; then we need to see if we have the most specific pattern
	      ;;; which we can just get by calling morph-generate
	      ;;; A more sophisticated version might allow multiple results
	      ;;; here
		  (let ((most-specific
			 (morph-generate underlying rule)))
		    (equal surface most-specific))
	      ;;; otherwise only *irregular-forms-only-p*
	      ;;; is set and we're only blocking by irregulars, so 
	      ;;; we're OK here
		t)))
    ;;; we're not doing any blocking so it's OK
	t)))
	  

(defun null-morphophon-combinations (unanalysed null-rules partial-tree-set)
  ;;; we have a set of rules that can apply to a string without
  ;;; morphophonological effect.  While such rules would normally be
  ;;; specified as lexical, special cases of morphophonologically active
  ;;; rules (irregulars in particular) may arise here.  We filter these
  ;;; out from the usual agenda mechanism, to prevent looping.
  ;;; 
  ;;; returns a set of partial-trees
    (append partial-tree-set
	    (apply-morph-null-rules partial-tree-set null-rules
				    unanalysed)))
				    

(defun apply-morph-null-rules (partial-tree-set null-rules unanalysed)
  (let ((new-pt-set
	 (loop for null-rule in null-rules
	     for rule-entry = (get-lex-rule-entry null-rule)
	     for ptnode = (list null-rule unanalysed)
	     nconc
	       (loop for partial-tree in partial-tree-set
		   when
		     (or (null partial-tree)
			 (check-nosp-feeding
			  (get-lex-rule-entry 
			   (pt-node-rule (car partial-tree)))
			       ;;; WARNING
			  rule-entry))
		   collect
		     (progn 
		       (when (>= (length partial-tree)
				 *maximal-lex-rule-applications*)
			 (error "~%Probable runaway morphological rule in ~A: analysis aborted ~
            (see documentation for *maximal-lex-rule-applications*)" partial-tree))
		       (add-pt-node-to-tree ptnode partial-tree))))))
    (if new-pt-set
	(append new-pt-set
		(apply-morph-null-rules new-pt-set
					null-rules unanalysed))
      nil)))

  

;;; end stuff specific to the :default and :external-rule-by-rule case

(defun add-morpho-stem-edge-from-token-edge (stem partial-tree token-edge)  
  ;;; This is called from the version with the rule-by-rule morphology
  ;;; (for the irregs and as the base case with regular morphology).
  ;;; It is also called when we have a version of morphology
  ;;; giving full partial trees and we're using the tokeniser
  ;;; (:external-partial-tree is the value of *morph-option*).
  ;;; But most of the work is done in add-morpho-stem-edge
  ;;; which is called from the sppp stuff (i.e. external
  ;;; tokeniser and partial tree morphology).
  ;;; The macro with-slots establishes a lexical environment for 
  ;;; referring to the slots in token-edge as though they were variables
  ;;; with the given slot name.  
  (with-slots (from to word string cfrom cto leaves) token-edge
    (add-morpho-stem-edge
     stem partial-tree from to word string cfrom cto 
     leaves token-edge)))

(defun add-morpho-stem-edge (stem partial-tree 
			     from to word string cfrom cto tleaves
			     tedge)
  ;;; this is the (re)entry point for the sppp code
  ;;;
  ;;; Copy most of token-edge
  ;;; Add partial-tree and stem
  ;;; Put into chart at same places
  (if
      (and 
       (or (member *morph-option* '(:default :external-rule-by-rule))
	   ;;; if we're proceeding rule by rule we've already
	   ;;; done this checking
	   (and (lookup-word *lexicon* stem)
		(or (max-one-deep-p partial-tree)
		    (check-rule-filter-morph partial-tree string))))
       (not (morpho-stem-edge-match 
		 from to stem partial-tree (aref *tchart* from 1))))
      (let* ((new-edge (make-morpho-stem-edge
			:id (next-edge)
			:word word
			:string string
			:stem stem
			:current stem
			:partial-tree partial-tree
			:leaves tleaves
			:children (if (token-edge-p tedge)
				      (list tedge))
			:from from
			:to to
			:cfrom cfrom
			:cto cto))
	     (cc (make-chart-configuration :begin from
					   :edge new-edge
					   :end to)))
	(push cc (aref *tchart* from 1))
	(push cc (aref *tchart* to 0))
	new-edge)
    nil))

;; cclist = edges leaving 'from'
(defun morpho-stem-edge-match (from to stem partial-tree cclist)
  (dolist (x cclist)
    ;; for each edge
    (when
	(and (eql from (chart-configuration-begin x))
	     (eql to (chart-configuration-end x))
	     (morpho-stem-edge-p (chart-configuration-edge x))
	     (equal stem
		    (morpho-stem-edge-stem
		     (chart-configuration-edge x)))
	     (partial-trees-equal partial-tree
			 (edge-partial-tree
			  (chart-configuration-edge x))))
      (return-from morpho-stem-edge-match x))))



;;; *************************************************************
;;;
;;; This alternative version is for the one step morphophonology case
;;; where the foreign-morph-fn returns a full partial-tree 
;;; and a stem - this is called when
;;; :external-partial-tree is the value of *morph-option*

(defun instantiate-chart-with-morpho-stem-edges nil
  (dotimes (current *tchart-max*)
    (let ((token-ccs (aref *tchart* current 1)))
      (dolist (token-cc token-ccs)
	(let* ((token-edge (chart-configuration-edge token-cc))
	       (word (token-edge-word token-edge))
	       (morph-specs
		(union
		 (filter-for-irregs
		  (remove-duplicates
		   (apply *foreign-morph-fn*
			  (list word))
		   :test #'equalp))
		 ;; filter-for-irregs is in rules.lsp
		 (find-irregular-morphs-old word) :test #'equalp)))
	  (dolist (morph-spec morph-specs)
	    (add-morpho-stem-edge-from-token-edge 
	     (car morph-spec)
	     (cdr morph-spec)
	     token-edge)))))))

;;; *foreign-morph-fn* is assumed to return something like
;;; ("A" (RULE1 "AB") (RULE2 "ABC"))
;;; as the old morph-analyse did

;;; *************************************************************
;;; this stuff is only for the case where we have not done the
;;; morphophonology rule by rule


(defun check-rule-filter-morph (partial-tree string)
  ;;; This is called with a partial-tree that will be
  ;;; tried later on in the parser proper.
  ;;; Make sure we can get actual rules, and, if we can,
  ;;; filter impossible combinations now according to the
  ;;; rule filter.
  ;;;
  ;;; WARNING: assumes partial tree is a list!
  (let ((rule-entries 
	 (loop for rule-info in partial-tree
	     collect
	       (let* ((rule-id (pt-node-rule rule-info))
		      (rule-entry (get-lex-rule-entry rule-id)))
		 (or rule-entry
		     (progn 
		       (format t 
			       "~%Warning: rule ~A specified by ~
                                       morphology for ~A was not found"
			       rule-id string)
		       (return-from check-rule-filter-morph nil)))))))
    (check-rule-filter-morph-aux rule-entries)))
	
(defun check-rule-filter-morph-aux (rule-list)
  (loop
      for (first . rest) on rule-list
      while rest
      always (check-nosp-feeding (first rest) first)))

;;; end of Phase 2 code

;;; *****************************************************
;;;
;;; Lexical lookup and MWEs - Phase 3
;;;
;;; *****************************************************

	
;;; multi-word entries may have affixation on any member
;;; but they span several vertices.  It's therefore
;;; necessary to treat them as individual words with respect to
;;; the spelling component.
;;; Putative multiword entries are checked
;;; when we get to the rightmost element.
	
(defun instantiate-chart-with-stems-and-multiwords nil
  (dotimes (current *tchart-max*)
    (dolist (cc (aref *tchart* current 1))
      (let ((edge (chart-configuration-edge cc)))
	(when (morpho-stem-edge-p edge)
	  (add-single-and-multiple-senses edge)))))
  ;;; FIX - the active-parser doesn't necessarily
  ;;; put the stem edges directly onto the chart - need
  ;;; to find out how else to check them to get warning messages
  (unless (and *active-parsing-p*
	       (or *first-only-p* *chart-dependencies*))
      (check-stem-coverage *tchart-max*)))

(defun add-single-and-multiple-senses (morpho-stem-edge)
  ;;; (format t "~%add-single-and-multiple-senses: ~A ~A" (edge-from morpho-stem-edge) (edge-to morpho-stem-edge))
  (let* ((to (edge-to morpho-stem-edge))
	 (edge-stem (morpho-stem-edge-stem morpho-stem-edge))
	 (from (edge-from morpho-stem-edge))
	 (cfrom (edge-cfrom morpho-stem-edge))
	 (cto (edge-cto morpho-stem-edge))
	 (partial-tree (edge-partial-tree morpho-stem-edge))
	 (edge-string (edge-string morpho-stem-edge))
	 (edge-entries (get-edge-entries morpho-stem-edge)))
    ;;; FIX when the precheck code is written the entries
    ;;; will be on the edges
    (dolist (entry edge-entries)
      (let ((entry-orth-list (mapcar #'string-upcase (lex-entry-orth entry)))) 
	(if (cdr entry-orth-list)
	    ;; multi-word
	    (check-multi-word entry-orth-list
			      edge-stem
			      edge-string entry 
			      from to cto partial-tree morpho-stem-edge)
	(add-stem-edge edge-stem
	 edge-string from to cfrom cto partial-tree entry morpho-stem-edge))))))
	     ;;; FIX - put back unify-in

(defun get-edge-entries (morpho-stem-edge)
  (with-slots (l-content stem) morpho-stem-edge
    (if l-content 
	(list l-content) 
      (get-unexpanded-lex-entry stem))))     

(defun add-stem-edge (edge-stem
		      edge-string from to cfrom cto partial-tree entry dtr)
    #+:arboretum (declare (special *mal-active-p*))
;;    (let* ((expanded-entry (get-lex-entry-from-id (lex-entry-id entry))))
;; (bmw) above code seems unnnecessary
    (let* ((expanded-entry (get-expanded-lex-entry entry)))
      ;; side effect of calling get-lex-entry-from-id is
      ;;  instantiation of :full-fs slot
      (when (and expanded-entry
		 #+:arboretum
		 (or *mal-active-p* 
		     (not (mal-lex-entry-p expanded-entry))))
	(let* ((new-fs
		(copy-lex-fs-as-needed
		 (lex-entry-full-fs entry)
		 :cfrom cfrom :cto cto))
	       (new-edge
		(make-edge :id (next-edge) 
			   :category (indef-type-of-tdfs new-fs)
			   :rule edge-stem
			   :dag new-fs
			   :leaves (list edge-string)
			   :lex-ids (list (lex-entry-id entry))
			   :from from
			   :to to
			   :tchildren (list dtr)
			   :partial-tree partial-tree
			   :string edge-string
			   :cfrom cfrom
			   :cto cto))
	       (cc (make-chart-configuration :begin from
					     :edge new-edge
					     :end to)))
	  (when (> to *chart-max*)
            (setf *chart-max* to))
          (cond
           (*active-parsing-p*
            (lexical-task (lex-priority new-edge) cc))
           (t
            (push cc (aref *chart* from 1))
            (push cc (aref *chart* to 0))))))))

;; fix_me (too much potential copying)
(defun copy-lex-fs-as-needed (tdfs &key cfrom cto)
  (when *characterize-p*
    (setf tdfs (set-characterization-tdfs tdfs cfrom cto)))
  (cond
   ((or *recording-word*
        (smember tdfs *lexical-entries-used*))
    (copy-tdfs-completely tdfs))
   (t 
    (push tdfs *lexical-entries-used*)
    tdfs)))

(defun check-multi-word (entry-orth-list edge-stem
			 edge-string unexpanded-entry from to cto 
			 partial-tree dtr)
  ;;; we have a possible match on a multiword
  ;;; i.e., something which corresponds to multiple tokens
  ;;; according to the tokeniser.  For every valid multiword
  ;;; we find, we will add an edge to the chart via add-stem-edge
  ;;;
  ;;; a multiword entry gives a list of stems
  ;;;
  (let* ((inflection-position (lex-entry-infl-pos unexpanded-entry))
	 (number-of-words (length entry-orth-list))
	 (to-be-accounted-for (reverse entry-orth-list))
	 (current-entry-stem (car to-be-accounted-for)))
    (unless (string-equal current-entry-stem edge-stem)
      (return-from check-multi-word nil))
    ;; only check multi-words when we have the rightmost
    (when (< to number-of-words)
      (return-from check-multi-word nil)) ; too near start of sentence
    ;; because of tokeniser ambiguity this is not a perfect check
    ;; but the more complex cases will be caught below
    (when (and partial-tree
	       (not (eql inflection-position number-of-words)))
      (return-from check-multi-word nil)) ; inflection not allowed here
    (check-multi-and-add-edges entry-orth-list (cdr to-be-accounted-for) from
			 nil to cto inflection-position
			 unexpanded-entry 
			 partial-tree edge-string dtr)))
			 

(defun check-multi-and-add-edges (entry-orth-list remaining-words 
			    from cfrom to cto inflection-position 
				  unexpanded-entry 
				  partial-tree amalgamated-string dtr)
  ;; check we have some match on each element
  ;; and find partial tree(s) on inflected position
  ;; this is called initially when we've got a match on the rightmost
  ;; element.  We add edges for each match.  Note that because
  ;; tokenisation is now non-deterministic, and because
  ;; there may be ambiguity in the morphology, we may end up with several
  ;; edges being added.  
  ;; e.g. ("a" "b" "c") as entry-orth-list with inflection
  ;; position 2 would match a set of contiguous edges where 
  ;; edge 1 had stem "a" and no partial tree
  ;; edge 2 had stem "b" and an optional partial tree
  ;; edge 3 we already know has stem "c" when this is called
  ;;
  ;; Note that, despite the name `inflection position' an MWE like 
  ;; "chest pain" would match "chest painless" (if the inflection position
  ;; was set to 2 and there was a productive affix "less", despite the 
  ;; fact that "less" would be derivational.  This would have to be controlled
  ;; by the grammar (presumably by whatever mechanism was used to block 
  ;; derivation occurring in the wrong place anyway).
  ;;
  (if remaining-words
      (let ((ccs (aref *tchart* from 0))
	    (entry-stem (car remaining-words)))
	(dolist (cc ccs)
	  (let ((edge (chart-configuration-edge cc)))
	      (when
		  (and 
		   (morpho-stem-edge-p edge)
		   (equal entry-stem (morpho-stem-edge-stem edge))
		   (if (eql (length remaining-words)
			    inflection-position)
		       (progn 
			 (setf partial-tree 
			   (morpho-stem-edge-partial-tree edge))
			 t)
		     (not (morpho-stem-edge-partial-tree edge))))
		(check-multi-and-add-edges 
		 entry-orth-list (cdr remaining-words) (edge-from edge)
		 (edge-cfrom edge)
		 to cto inflection-position unexpanded-entry 
		 partial-tree
		 (concatenate 'string (edge-string edge) " "
			      amalgamated-string)
		 dtr)))))
    (add-stem-edge
     (format nil "~{~A ~}" entry-orth-list)
     amalgamated-string
     ;; FIX - when cfrom cto is universal we can replace this
     ;; by a lookup in the original characters.  Currently
     ;; this is a bit of a hack since
     ;; we just guess that the strings of the individual words
     ;; were split by spaces.
     from to cfrom cto partial-tree 
     unexpanded-entry dtr)))

;;; **************************************************
;;;
;;; Unknown words / messages about missing words
;;;
;;; **************************************************

#|

The idea is to allow for ambiguity in the token input.  We scan
initially, recording how far we can get in res-array, which stores
t at index n if there is a potential span between 0 and n.  If we get
to a point where there's a gap, we generate a warning and perhaps
an unknown word, treat the gap as filled and go on from there.

|#

(defun check-stem-coverage (max)
  (let ((res-array (make-array (list (+ max 1)) :initial-element nil)))
    (setf (aref res-array 0) t)
    (check-stem-coverage-aux 0 res-array)
    (generate-messages-and-unknown-words res-array max)))

(defun generate-messages-and-unknown-words (res-array max-dim)
 ;;; (format t "~% generate-messages-and-unknown-words ~A" res-array)
  (unless (aref res-array max-dim)
    (dotimes (current (+ 1 max-dim))
      (unless (aref res-array current)
	(let ((token-ccs (aref *tchart* current 0)))
	  (dolist (cc token-ccs)
	    (let ((token-entry (chart-configuration-edge cc)))
	      (unless *generate-messages-for-all-unanalysed-tokens*
		(format t "~%No analysis found corresponding to token ~a-~a ~A" 
			(token-edge-from token-entry)
			(token-edge-to token-entry)
			(token-edge-word token-entry)))
	      )))
	      ;;;    FIX    (generate-unknown-word-entries stem-string)
	(setf (aref res-array current) t)
	(check-stem-coverage-aux current res-array)
	(generate-messages-and-unknown-words res-array max-dim)
	(return)))))

(defun generate-messages-for-all-unanalysed-tokens (tchart)
  (let ((medges (get-medges tchart))
	(tedges (get-tedges tchart)))
    (loop for tok in tedges
	unless (member tok medges 
		       :test #'(lambda (x y)
				 (member x (edge-children y))))
	do
	  (with-slots (from to word) tok
	    (push word *unanalysed-tokens*)
	    (format t "~&No lexical analysis found corresponding to token ~a-~a ~A"
		    from to word)))))

(defun check-stem-coverage-aux (start res-array)
  ;;; (format t "~%check-stem-coverage-aux ~A ~A" start res-array)
  (let ((stem-ccs
	 (aref *chart* start 1))
	(end-points nil))
    (dolist (cc stem-ccs)
;;;      (pprint cc)
      (let ((end-point (chart-configuration-end cc)))
	(unless (member end-point end-points)
	  (push end-point end-points)
	  (setf (aref res-array end-point) t))))
    (dolist (end end-points)
      (check-stem-coverage-aux end res-array))))
  

;;; *****************************************************
;;;
;;; Morphosyntax interaction with phase 4
;;;
;;; Morphological and lexical rule application
;;;
;;; *****************************************************

;;; We now have a chart with edges corresponding to stems which
;;; may need some morphological rules to be added.
;;; This is complicated because the morphological rules can be 
;;; interleaved with arbitrary numbers of lexical rules.
;;; 
;;; We want to combine this with parsing proper.  
;;; Ordinary grammar rules are not allowed to apply to things with a 
;;; partial-tree.  Spelling-change rules are only allowed to apply
;;; when the next thing on the partial-tree is them.  Lexical
;;; rules can apply any time.

(defun find-spelling-info (edge)
  ;;; WARNING - assumes partial tree is a list
  (let* ((partial-tree (edge-partial-tree edge)))
    ;;; list of (rule "FORM")
    (if partial-tree
	(let* ((current-rule-info (first partial-tree))
	       (new-orth (pt-node-string current-rule-info))
	       (orth-tdfs (when new-orth 
			    (make-orth-tdfs new-orth)))
	       (rule-id (pt-node-rule current-rule-info))
	       (rule-entry (get-lex-rule-entry rule-id)))
	  (if rule-entry
	      (values rule-entry orth-tdfs (rest partial-tree))
	    (progn 
	      (format t 
		      "~%Warning: rule ~A specified by ~
                                       morphology was not found"
		      rule-id)
	      nil))))))
	  

(defun apply-immediate-spelling-rule (rule orth-tdfs remaining-tree 
				      left-vertex child-edge right-vertex f)
;;; this is a special case of apply-immediate-grammar-rule
  #+:pdebug
  (format
   t
   "~&apply-immediate-spelling-rule(): `~(~a~) <-- ~d~^ [~d -- ~d]"
   (rule-id rule) (edge-id child-edge) left-vertex right-vertex)
  (when 
      (and (check-rule-filter rule (edge-rule child-edge) 0)
	   (restrictors-compatible-p 
	    (car (rule-daughters-restricted-reversed rule))
				     (edge-dag-restricted 
				      child-edge)))
      (multiple-value-bind (unification-result first-failed-p)
	  (evaluate-unifications rule (list (edge-dag child-edge))
				 orth-tdfs
				 nil nil (edge-cfrom child-edge) (edge-cto child-edge))
        (if unification-result
            (let* ((new-edge 
                    (make-edge :id (next-edge)
                               :category (indef-type-of-tdfs unification-result)
                               :rule rule
                               :children (list child-edge)
                               :dag unification-result 
                               :lex-ids (edge-lex-ids child-edge)
                               :leaves (edge-leaves child-edge)
			       :partial-tree remaining-tree)))
              #+pdebug (format t " ... success.~%")
              (activate-context left-vertex new-edge right-vertex f)
              t)
          (progn
            #+pdebug (format t " ... ~:[fail~;throw~].~%" first-failed-p)
            (if first-failed-p nil t))))))

;;; **************************************************************
;;;
;;; Parsing - Phase 4 proper
;;;
;;; *************************************************************

(defun add-words-to-chart (f)
  (dotimes (current *chart-max*)
    (let ((stem-ccs (aref *chart* (1+ current) 0)))
      (dolist (stem-cc stem-ccs)
	(let ((edge (chart-configuration-edge stem-cc)))
	  (add-word edge f 
		    (chart-configuration-begin stem-cc)
		    (1+ current)))))))

(defun add-word (edge f left-vertex right-vertex)
  (declare (ignore left-vertex))
  (unless *active-parsing-p*
    (with-agenda (when f (lex-priority edge))
      (activate-context-no-add (edge-from edge) edge right-vertex f))))

(defun unify-in-word (tdfs word-string)
  (declare (ignore word-string))
  tdfs)

(defun activate-context (left-vertex edge right-vertex f)
  #+:pdebug
  (format 
   t 
   "~&activate-context(): edge # ~d: [~d -- ~d];~%"
   (edge-id edge) left-vertex right-vertex)

  (add-to-chart left-vertex edge right-vertex f)
  (activate-context-no-add left-vertex edge right-vertex f)) 
  
(defun activate-context-no-add (left-vertex edge right-vertex f) 
  ;; when we have a partial-tree specification on the edge, one
  ;; option is the corresponding morphological rule
  ;; When this is applied, the new edge has a record 
  ;; that removes that element from the partial-tree
  (multiple-value-bind (spelling-rule orth-tdfs remaining-morph) 
      (find-spelling-info edge)
    (when spelling-rule
      (apply-immediate-spelling-rule spelling-rule orth-tdfs remaining-morph
				     left-vertex edge right-vertex f))
    (dolist (rule (if spelling-rule
		      (loop for lr in *parser-lexical-rules*
			  when (check-sp-lr-feeding spelling-rule lr)
			  collect lr)
		    *parser-rules*))
      ;; when we have a partial-tree specification, we have all
      ;; lexical rules here and no parser rules
      ;;
      ;; grammar rule application is attempted 
      ;; when we've got all the bits
      (try-grammar-rule-left rule
			     ;; avoid a call to reverse here in a fairly tight loop
			     (rule-daughters-restricted-reversed rule)
			     left-vertex
			     right-vertex
			     (list edge)
			     f
			     (1- (length (the list (rule-daughters-apply-order rule)))))
      ;; when we don't build up the chart in strict left-to-right
      ;; order (as when we're doing a best-first search), we need to
      ;; check for rule applying to the right as well as to the left.
      ;; WARNING: this will only work correctly if all rules are no
      ;; more than binary branching!!
      (when (and f (cdr (rule-daughters-restricted rule)))
	(try-grammar-rule-right rule
				(rule-daughters-restricted rule)
				left-vertex
				right-vertex
				(list edge)
				f 0)))))


(defun add-to-chart (left edge right f)
  (let ((config (make-chart-configuration :begin left :edge edge :end right)))
    (push config (aref *chart* right 0))
    ;; index chart edges by the start vertex too
    (push config (aref *chart* left 1))
    ;; Did we just find a parse?
    (when (and f (eql left (car f))
	       (eql right (cdr f)))
      (let ((result (find-spanning-edge config (car f) (cdr f))))
        (when result
          (push (get-internal-run-time) *parse-times*)
          (setf *parse-record* (nconc result *parse-record*))
          (when *first-only-p*
            (when (zerop (decf *first-only-p*))
              (throw :best-first t))))))))


(defun try-grammar-rule-left (rule rule-restricted-list left-vertex 
			      right-vertex child-edge-list f n)
  ;; Application of a grammar rule: Every time an edge is added to the chart,
  ;; a check is made to see whether its addition passes the rule application
  ;; filter.
  ;; If yes collect the dags associated with the
  ;; children, perform the unifications specified by the rule, and if the
  ;; unification(s) succeed, create a new edge (for the mother), record its
  ;; dag and associated information, add this to the chart, and invoke the
  ;; same process recursively.
  (declare (type fixnum n))
  #+:pdebug
  (format
   t
   "~&try-grammar-rule-left(): `~(~a~) [~d] <-- ~{~d~^ ~} [~d -- ~d]~%"
   (rule-id rule) (length rule-restricted-list)
   (loop for e in  child-edge-list collect (edge-id e))
   left-vertex right-vertex)
  (if (and (check-rule-filter rule (edge-rule (car child-edge-list)) n)
	   (restrictors-compatible-p (car rule-restricted-list) 
				     (edge-dag-restricted 
				      (car child-edge-list))))
      (if (cdr rule-restricted-list) ;; we need more daughters
	  (let ((entry (aref (the (simple-array t (* *)) *chart*) 
			     left-vertex 0)))
	    (if entry
		(dolist (config entry t)
		  (unless (edge-partial-tree 
			   (chart-configuration-edge config))
		    (unless
			;; inner recusive call returns nil in cases when first
			;; unif attempt fails - if this happens there's no point
			;; continuing with other alternatives here
			(try-grammar-rule-left 
			 rule
			 (cdr rule-restricted-list)
			 (chart-configuration-begin config)
			 right-vertex
			 (cons (chart-configuration-edge config) child-edge-list)
			 f (1- n))
		      #+:pdebug
		      (format
		       t
		       "~&try-grammar-rule-left(): ~
                    `~(~a~) [~d] <-- ~{~d~^ ~} [~d -- ~d] ... throw~%"
		       (rule-id rule) (length rule-restricted-list)
		       (loop for e in  child-edge-list collect (edge-id e))
		       left-vertex right-vertex)
		      #-:vanilla (return-from try-grammar-rule-left nil)
		      ;; nil returned from the inner call is a signal
		      ;; that unification of the edge we're triggering off
		      ;; failed, so success with any combination is impossible
		      #+:vanilla t)))
	      t))
                  ;; must return t, because we don't want an outer
                  ;; loop to throw 
	;; we've got all the bits
	(with-agenda (when f (rule-priority rule))
	  (apply-immediate-grammar-rule rule left-vertex right-vertex
					child-edge-list f t)))
    (progn (incf (statistics-ftasks *statistics*)) t)))

(defun try-grammar-rule-right (rule rule-restricted-list left-vertex 
			       right-vertex child-edge-list f n)
  (declare (type fixnum n))
  (if (and (check-rule-filter
              rule (edge-rule (car child-edge-list)) n)
	   (restrictors-compatible-p (car rule-restricted-list)
				     (edge-dag-restricted 
                                      (car child-edge-list))))
      (if (cdr rule-restricted-list)
	  (let ((entry (aref (the (simple-array t (* *)) *chart*)
                             right-vertex 1)))
	    (if entry
		(dolist (config entry t)
		  (unless (edge-partial-tree 
			    (chart-configuration-edge config))
		    (unless
			(try-grammar-rule-right
			 rule
			 (cdr rule-restricted-list)
			 left-vertex
			 (chart-configuration-end config)
			 (cons (chart-configuration-edge config) child-edge-list)
			 f (1+ n))
		      #-:vanilla
		      (return-from try-grammar-rule-right nil)
		      #+:vanilla t)))
	      t))
	;; we've got all the bits
	(with-agenda (when f (rule-priority rule))
	  (apply-immediate-grammar-rule rule left-vertex right-vertex 
					child-edge-list f nil)))
    (progn (incf (statistics-ftasks *statistics*)) t)))


(defparameter *debugging* nil)

(defun apply-immediate-grammar-rule (rule left-vertex right-vertex 
				     child-edge-list f backwardp)
  ;; attempt to apply a grammar rule when we have all the parts which match
  ;; its daughter categories
  (cond ((and *brackets-list*
            (not (consistent-bracketing-p (mapcar #'(lambda (edge)
                                                 (cons (edge-from edge)
                                                       (edge-to edge)))
                                             child-edge-list)
                                          *brackets-list*)))
         t) ; t because we don't want the first-failed-p effect 
        (t
    #+:pdebug
    (format
     t
     "~&try-immediate-grammar-rule(): `~(~a~) <-- ~{~d~^ ~} [~d -- ~d]"
     (rule-id rule) (loop for e in  child-edge-list collect (edge-id e))
     left-vertex right-vertex)
    (let ((child-edge-list-reversed (reverse child-edge-list)))
      (multiple-value-bind (unification-result first-failed-p)
	  (evaluate-unifications rule (mapcar #'edge-dag 
					      child-edge-list-reversed)
				 nil child-edge-list-reversed backwardp
				 (loop for edge in child-edge-list
				     minimize (edge-cfrom edge))
				 (loop for edge in child-edge-list
				     maximize (edge-cto edge)))
        (if unification-result
            (let* ((edge-list
                    (if backwardp child-edge-list child-edge-list-reversed))
                   (new-edge 
                    (make-edge :id (next-edge)
                               :category (indef-type-of-tdfs unification-result)
                               :rule rule
                               :children edge-list
                               :dag unification-result 
                               :lex-ids (mapcan
                                         #'(lambda (child)
                                             (copy-list (edge-lex-ids child)))
                                         edge-list)
                               :leaves (mapcan
                                        #'(lambda (child)
                                            (copy-list (edge-leaves child)))
                                        edge-list)
			       :partial-tree 
			       (edge-partial-tree (first edge-list))
			       ;; should be unary-rule if there is a partial
			       ;; tree (worry about compounds later ...)
			       ;; WARNING
			       )))
              #+pdebug (format t " ... success.~%")
              (activate-context left-vertex new-edge right-vertex f)
              t)
          (progn
            #+pdebug (format t " ... ~:[fail~;throw~].~%" first-failed-p)
            (if first-failed-p nil t))))))))


(defun evaluate-unifications (rule child-fs-list 
			      &optional nu-orth child-edge-list backwardp cfrom cto)
  ;; An additional optional argument is given. This is the new orthography if
  ;; the unification relates to a morphological process. If it is present, it
  ;; is inserted in the resulting fs
  (let*
      ((current-tdfs (rule-full-fs rule))
       (rule-daughter-order
	(if backwardp 
	    (rule-daughters-order-reversed rule) 
	  (cdr (rule-order rule))))
       (n -1)
       (new-orth-fs (when nu-orth
                      (if (tdfs-p nu-orth) nu-orth (make-orth-tdfs nu-orth)))))
    ;; shouldn't strictly do this here because we may not need it but
    ;; otherwise we get a nested unification context error - cache the values
    ;; for an edge
    (declare (type fixnum n))
    (with-unification-context (ignore)
      (dolist (rule-feat rule-daughter-order)
	(incf n)
	(let ((child-edge (pop child-edge-list)))
	  (cond
	   ((zerop n))
	   ((x-restrict-and-compatible-p
	     (if (listp rule-feat)
	       (x-existing-dag-at-end-of 
                (deref-dag (tdfs-indef current-tdfs)) rule-feat)
	       (x-get-dag-value 
                (deref-dag (tdfs-indef current-tdfs)) rule-feat))
	     (edge-dag-restricted child-edge)))
	   (t (incf (statistics-ftasks *statistics*))
	      (return-from evaluate-unifications nil))))
	(incf (statistics-etasks *statistics*))
	(let ((child (pop child-fs-list)))
	  ;; If two daughters are eq, the unifier's subgraph sharing code may
	  ;; cause spurious coreferences in the result
	  (when (member child child-fs-list :test #'eq)
	    (setq child (copy-tdfs-completely child)))
	  (if (setq current-tdfs
		(yadu current-tdfs
		      (create-temp-parsing-tdfs child rule-feat)))
	      (incf (statistics-stasks *statistics*))
	    (return-from
		evaluate-unifications 
	      (values nil (eql n 0))))))	; first attempt failed?
      ;; if (car (rule-order rule)) is NIL - tdfs-at-end-of will return the
      ;; entire structure
      (let ((result (tdfs-at-end-of (car (rule-order rule)) current-tdfs)))
	(when new-orth-fs
	  (setq result (yadu result new-orth-fs))) 
	(when result
	  ;; delete arcs just holding constituents' feature structures -
	  ;; before copying otherwise their copies would be thrown away
	  ;; immediately we have to check whether any of the deleted dags
	  ;; contain a cycle - if so then the whole rule application should
	  ;; fail
	  (let* ((real-dag (deref-dag (tdfs-indef result)))
		 (new (clone-dag real-dag))
		 (arcs-to-check nil))
	    (flet ((member-with-cyclic-check (arc)
		     (when (member (dag-arc-attribute arc) 
				   *deleted-daughter-features* :test #'eq)
		       (push arc arcs-to-check)
		       t)))
	      (setf (dag-arcs new)
		(remove-if #'member-with-cyclic-check (dag-arcs new)))
	      (setf (dag-comp-arcs new)
		(remove-if #'member-with-cyclic-check (dag-comp-arcs new)))
	      ;; take advantage of the fact that removed arcs might share
	      ;; structure by checking them all at once
	      (let ((res
		     (and
		      (not (cyclic-dag-p
			    (make-dag :type *toptype* 
				      :arcs arcs-to-check)))
		      (setf (dag-forward real-dag) new)
;		      (copy-tdfs-elements result)
		      (progn
			(if *characterize-p*
			    (set-characterization-indef-within-unification-context 
			     (tdfs-indef result) cfrom cto))
			(copy-tdfs-elements result)))))
		(or res
		    ;; charge copy failure to last successful unification
		    (progn (incf (statistics-stasks *statistics*)) nil))))))))))


(defun create-temp-parsing-tdfs (tdfs flist)
  (if (null flist) tdfs
    (let ((indef-dag (create-dag))
          (tail nil))
      (unify-list-path flist indef-dag (tdfs-indef tdfs))
      (when (tdfs-tail tdfs)
        (let ((path (create-path-from-feature-list
                     (if (listp flist) flist (list flist)))))
          (loop for tail-element in (tdfs-tail tdfs)
               do
               (push (add-path-to-tail path tail-element) tail))))
      (make-tdfs :indef indef-dag :tail tail))))



;;; ***********************************************************
;;;
;;; Finishing off
;;;
;;; **********************************************************

(defun find-spanning-edges (start-vertex end-vertex)
  ;; Returns all edges between two vertices and checks for root conditions -
  ;; used to see if a parse has been found.
  (let ((start-symbols (if (listp *start-symbol*)
			   *start-symbol*
			 (list *start-symbol*)))
	(configs (aref *chart* end-vertex 0)))
      (loop for item in configs
	   append
	    (when 
		(and (eql (chart-configuration-begin item) start-vertex)
		     (not (edge-partial-tree (chart-configuration-edge item))))
	     ;; root may be a list of (td)fs with the interpretation that
	     ;; if any of them match the parse is OK
	     (if (null start-symbols)
		 (list (chart-configuration-edge item))
	       (if *substantive-roots-p*
		   (create-new-root-edges item start-symbols
					  start-vertex end-vertex)
		 (filter-root-edges item start-symbols)))))))

;; Decide if a single edge is a successful parse (when looking for the
;; first parse only).

(defun find-spanning-edge (item start-vertex end-vertex)
  (when (and (eql (chart-configuration-begin item) start-vertex)
	     (not (edge-partial-tree (chart-configuration-edge item))))
    (let ((start-symbols (if (listp *start-symbol*)
                           *start-symbol*
                           (list *start-symbol*))))
      (if (null start-symbols)
        (list (chart-configuration-edge item))
        (if *substantive-roots-p*
	  (create-new-root-edges item start-symbols start-vertex end-vertex)
          (filter-root-edges item start-symbols))))))

(defparameter *additional-root-condition* nil
  "defined in mrs/idioms.lisp")

(defun filter-root-edges (item roots)
  (loop
      with edge = (if (edge-p item) item (chart-configuration-edge item))
      with tdfs = (edge-dag edge)
      for root in roots
      for rtdfs = (get-tdfs-given-id root) 
		  ;; might be a type
		  ;; or a root entry
      thereis (when 
                  (and rtdfs 
                       (yaduablep rtdfs tdfs)
                       (if *additional-root-condition*
                           (funcall *additional-root-condition* tdfs)
                         t))
                (list edge))))

;;; FIX - can't really have substantive-roots-p and idioms
;;; need to stop it being set if idioms are loaded

(defun create-new-root-edges (item start-symbols start-vertex end-vertex)
  (loop for start-symbol in start-symbols        
       nconc
       (let ((rtdfs (get-tdfs-given-id 
                    start-symbol)))
         (if rtdfs
            (let ((unif
                    (yadu rtdfs
                          (edge-dag 
                            (chart-configuration-edge item)))))
               (if unif
                   (let ((new-edge
                          (make-edge :dag (copy-tdfs-elements unif)
                                     :id (next-edge)
                                     :category
                                     (indef-type-of-tdfs unif)
                                     :rule start-symbol
                                     :children 
                                     (list (chart-configuration-edge item))
                                     :lex-ids (edge-lex-ids
                                               (chart-configuration-edge item))
                                     :leaves
                                     (edge-leaves 
                                      (chart-configuration-edge item))
                                     :from start-vertex
                                     :to end-vertex)))
                     (add-to-chart start-vertex
                                   new-edge
                                   end-vertex
				   ;; Don't (recursively) check for success
				   nil)
                     (list new-edge))))))))

;;; ***************************************************************
;;;
;;; TTY printout of chart
;;; chart edges are ordered on: right vertex, left vertex, edge id
;;;
;;; ***************************************************************

(defun print-chart (&key frozen concise (stream t))
  (format stream "~% > chart dump:~%")
  (loop
      for i from 1 to *chart-max*
      do (print-chart-entry i (aref *chart* i 0) 
			    :frozen frozen :concise concise :stream stream))
  (terpri stream))

(defun print-tchart (&key frozen concise (stream t) (tchart *tchart*))
;;;  called by print-token-chart-toplevel
  (format stream "~% > token/spelling chart dump:~%")
  (loop
      for i from 1 to *tchart-max*
      do (print-chart-entry i (aref tchart i 0) 
			    :frozen frozen :concise concise :stream stream))
  (terpri stream))

(defun print-chart-entry (vertex item &key frozen concise (stream t))
  (when item 
    (terpri stream)
    (dolist
	(configuration
            (sort (copy-list item)
		  #'(lambda (span1 span2)
		      (cond
		       ((eql (chart-configuration-begin span1)
			     (chart-configuration-begin span2))
			(< (edge-id (chart-configuration-edge span1))
			   (edge-id (chart-configuration-edge span2))))
		       (t
                        (< (chart-configuration-begin span1)
                           (chart-configuration-begin span2)))))))
      (print-chart-item configuration vertex 
			:frozen frozen :concise concise :stream stream))))


(defun print-chart-item (item 
                         &optional end 
                         &key (frozen nil frozenp)
                              concise (stream t))
  (declare (ignore concise))
  (let ((edge (if (edge-p item) item (chart-configuration-edge item)))
        (begin (unless (edge-p item) (chart-configuration-begin item)))
        (roots (unless (edge-p item) (chart-configuration-roots item))))
    (when (or (null frozenp) (eq (edge-frozen edge) frozen))
      (format 
       stream 
       "~&~:[~2*~;~A-~A ~][~A] ~A => ~A~A [~{~A~^ ~}]"
       (and begin end) begin end
       (edge-id edge)
       (if (token-edge-p edge) 
	   (token-edge-word edge)
	 (if (morpho-stem-edge-p edge)
	     (format nil "~A+~{~A ~}" (morpho-stem-edge-stem edge) 
		     (mapcar #'(lambda (rule-spec)
				 (concise-rule-name (pt-node-rule rule-spec)))
			     (morpho-stem-edge-partial-tree edge)))
	   ;;; WARNING - assume partial-tree is a list
	   (concise-edge-label edge)))
       (edge-leaves edge)
       (cond 
	;; ((token-edge-p edge) " T")
	(roots "*")
	(t ""))
       (or (loop for child in (edge-children edge) collect (edge-id child))
	   (loop for child in (edge-tchildren edge) collect (edge-id child))))
      (format
       t
       "~:[~2*~; ~:[+~;~]~d~]"
       (edge-frozen edge) (and (edge-frozen edge) (minusp (edge-frozen edge)))
       (edge-frozen edge))
      ;;
      ;; if applicable, print out compact summary of packings (9-aug-99  -  oe)
      ;;
      (when (or (edge-equivalent edge) (edge-packed edge))
	(let ((edge (first (or (edge-equivalent edge) (edge-packed edge)))))
	  (format 
	   stream 
	   " { [~d < ~{~d~^ ~}" 
	   (edge-id edge) 
	   (loop for child in (edge-children edge) collect (edge-id child))))
	(loop
	    for edge in (rest (edge-equivalent edge)) do
	      (format 
	       stream 
	       "; ~d < ~{~d~^ ~}"
	       (edge-id edge) 
	       (loop 
		   for child in (edge-children edge) 
		   collect (edge-id child))))
	(loop
	    for edge in (if (edge-equivalent edge)
			    (edge-packed edge)   
			  (rest (edge-packed edge))) do
	      (format 
	       stream 
	       "; ~d < ~{~d~^ ~}"
	       (edge-id edge) 
	       (loop 
		   for child in (edge-children edge) 
		   collect (edge-id child))))
	(format stream "]"))
      ;; print cfrom/cto
      (when (token-edge-p edge)
	(with-slots (cfrom cto) edge
	  (format stream "~:[~2*~; <~A c ~A> ~]"
		  (and cfrom cto) cfrom cto)))
      ;; print xfrom/xto
      (when (token-edge-p edge)
	(with-slots (xfrom xto) edge
	  (format stream "~:[~2*~; <~A x ~A> ~]"
		  (and xfrom xto) xfrom xto)))
      (format stream "~%"))))

(defun concise-edge-label (edge)
  (if (rule-p (edge-rule edge))
    (rule-id (edge-rule edge)) 
    (first (edge-lex-ids edge))))

;;; **************************************************************
;;;
;;; Parsing sentences from file (when the fine system is just too
;;; much ...)
;;;
;;; **************************************************************

(defun parse-sentences (&optional input-file (output-file 'unspec) &key rest)
   (unless input-file 
      (setq input-file (ask-user-for-existing-pathname "Sentence file?")))
   ;; if xml input assume SAF XML
   (when (file-xml-p input-file)
     (return-from parse-sentences
       (apply #'process-saf-file-sentences (cons input-file rest))))
   (when
      (and input-file
           (or (probe-file input-file)
               (progn
                 (show-message-window 
                  (format nil "Input file `~a' does not exist" input-file))
                 (return-from parse-sentences))))
      (with-open-file (istream input-file :direction :input)
         (if (eq output-file 'unspec)
           (setq output-file 
             (ask-user-for-new-pathname "Output file?" input-file))
           (if (equal input-file output-file)
               (progn
                 (show-message-window 
                  (format nil "Attempt to overwrite input file `~a'"input-file))
                 (return-from parse-sentences))))
         (unless output-file (return-from parse-sentences))
         (let ((line (read-line istream nil 'eof)))
            (if (and output-file (not (eq output-file t)))
               (with-open-file (ostream output-file :direction :output
                                :if-exists :supersede 
                                :if-does-not-exist :create)
		 (batch-parse-sentences istream ostream line 
					#'extract-fine-system-sentence))
	      (batch-parse-sentences istream t line
				     #'extract-fine-system-sentence))))))


(defparameter *do-something-with-parse* nil)

(defparameter *lex-ids-used* nil)

(defparameter *parse-input* nil)

(defparameter *ostream* nil)

(defun batch-parse-sentences (istream ostream raw-sentence &optional access-fn)
   (setf *lex-ids-used* nil)
   (clear-type-cache)
   (format t "~%;;; Parsing test file~%") (finish-output t)
   (let ((nsent 0)
         (edge-total 0)
         (parse-total 0)
         ;; ask for recycling of safe dags
         ;; NB lexical entries must not contain safe dags - so expand-psort-entry
         ;; and friends must rebind *safe-not-to-copy-p* to nil
         (*print-right-margin* 300)
         (start-time (get-internal-run-time)))
     (unwind-protect
       (loop
         (when (eql raw-sentence 'eof)
           (format t "~%;;; Finished test file")
           (unless (fboundp *do-something-with-parse*)
             (when ostream
               (format ostream "~%;;; Total CPU time: ~A msecs~%" 
                       (- (get-internal-run-time) start-time))
               (format ostream "~%;;; Mean edges: ~,2F~%" 
                       (/ edge-total nsent))
               (format ostream "~%;;; Mean parses: ~,2F~%" 
                       (/ parse-total nsent))))
           (lkb-beep)
           (return))
#|
         (when (eql (rem nsent 50) 49)
            (clear-expanded-lex))      ; try and avoid image increasing
					; at some speed cost
					;
					; duh - this now also clears
					; generator indices - could fix
					; but not important enough to
					; worry about
					|#
         (let ((interim-sentence (if access-fn (apply access-fn (list raw-sentence))
                                     raw-sentence)))
            (let ((sentence (string-trim '(#\Space #\Tab) interim-sentence)))
              (unless (or (equal sentence "") (char= (elt sentence 0) #\;))
                (incf nsent)
                (unless (fboundp *do-something-with-parse*)
                  ;; if we're doing something else, 
                  ;; let that function control the output
                  (when ostream
                    (format ostream "~A ~A " nsent interim-sentence)
                    (finish-output ostream)))
                 (let* ((munged-string 
                         (if (fboundp 'preprocess-sentence-string)
                             (preprocess-sentence-string sentence)
                           sentence))
                        (user-input (split-into-words munged-string))
                        (*dag-recycling-p* t))
                   (#-:gdebug 
                    handler-case
                    #+:gdebug
                    progn
                       (progn
                         #+:pooling
                         (reset-pools #+:gdebug t)
                         (parse user-input nil)
                         (setf *sentence* sentence)
                         (setf *parse-input* user-input)
                         (setf *ostream* ostream)
                         (when (fboundp *do-something-with-parse*)
                           (funcall *do-something-with-parse*)))
                       #-:gdebug
                       (storage-condition (condition)
					  (format t "Memory allocation problem: ~A caused by ~A~%" condition raw-sentence))
                       #+:(and (not :gdebug) :allegro)
		       (EXCL:INTERRUPT-SIGNAL () (error "interrupt-signal"))
                       #-:gdebug
                       (error (condition)
                              (format t  "Error: ~A caused by ~A~%" condition raw-sentence)))
                   (unless (fboundp *do-something-with-parse*)
                     ;; if we're doing something else, 
                     ;; let that function control the output
                     (when ostream
                       (let ((n (length *parse-record*)))
                         (setf parse-total (+ parse-total n))
                         (setf edge-total (+ edge-total *edge-id*))
                         (format ostream "~A ~A~%" n 
            ;;                     (edge-count)
                                 *edge-id*))
                       (finish-output ostream))))))
            (loop for lex-id in (collect-expanded-lex-ids *lexicon*)
               do
               (pushnew lex-id *lex-ids-used*))
            (setq raw-sentence (read-line istream nil 'eof))))
       (clear-chart)))) ; prevent any recycled dags from hanging around

(defun edge-count nil
  (let ((distinct-parse-edges nil))
    (dolist (p *parse-record*)
      (setq distinct-parse-edges 
        (local-parse-tsdb-distinct-edges p distinct-parse-edges)))
    (length distinct-parse-edges)))
      

(defun local-parse-tsdb-distinct-edges (edge found)
  ;; same as parse-tsdb-distinct-edges but don't want 
  ;; to necessarily load tsdb
   (pushnew edge found :test #'eq)
   (when (and (edge-children edge)
            (not (lexical-rule-p (edge-rule edge))))
      (dolist (c (edge-children edge))
         (setq found (local-parse-tsdb-distinct-edges c found))))
   found)


;;; *************************************************************
;;;
;;; generator utilities and structures 
;;;
;;; ************************************************************


;;; extracting a list of lexical entries used in a parse
;;; used for testing the generation lexical lookup algorithm

(defun retrieve-lex-from-parses nil
  (loop for edge in *parse-record*
       collect
       (edge-lex-ids edge)))

; (collect-parse-base (car *parse-record*))

(defun collect-parse-base (edge-rec)
  ;;; takes a top edge, returns a list of 
  ;;; lexical identifiers, unary-rule-list pairs
  (if (or (cdr (edge-lex-ids edge-rec))
          (and (rule-p (edge-rule edge-rec))
               (not (lexical-rule-p (edge-rule edge-rec)))))
      (loop for child in (edge-children edge-rec)
           append
           (collect-parse-base child))
    (list (cons (car (edge-lex-ids edge-rec))
          (nreverse (collect-unary-rule-names edge-rec))))))

(defun collect-unary-rule-names (edge-rec)
  (when (cdr (edge-children edge-rec))
    (error "~%Should be unary edge ~A" edge-rec))
  (if (edge-children edge-rec)
    (cons (rule-id (edge-rule edge-rec))
          (collect-unary-rule-names (car (edge-children edge-rec))))))


(defstruct (dotted-edge (:include edge))
   res ; feature name of mother in rule dag in active edges
   needed ; ordered list of names of daughter features still to be found
   )


(defstruct (g-edge (:include dotted-edge))
   ;; category, id, rule, leaves: filled in, not used in generation algorithm
   ;; itself, but needed for debug output, chart display etc
   rels-covered ; set of relations generated so far
   index
   lexemes ; non-ordered set of found-lex structures
   mod-index ; 0-based index to modifier under an intersective rule instantiation
   accessible ; indices accessible in this edge
   )

;;; This is now only used by the generator
;;; but - FIX - is oe calling this somewhere unknown?  What is the
;; optional daughter for?

(defun apply-morph-rule (rule fs fs-restricted new-orth &optional daughter)
  #-:debug 
  (declare (ignore daughter))
  ;;
  ;; _fix_me_
  ;; 
  (let* ((qc (restrictors-compatible-p 
              (car (rule-daughters-restricted rule)) fs-restricted))
         (result (and qc (evaluate-unifications rule (list fs) new-orth))))
    #+:debug
    (when qc
      (format
       t
       "apply-morph-rule(): ~a + ~a: ~:[nil~;t~] ~:[nil~;t~]~%"
       (rule-id rule) 
       (if (rule-p daughter) (rule-id daughter) daughter)
       qc result))
    result))
