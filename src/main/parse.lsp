;;; Copyright (c) 1991-2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.


(in-package :lkb)

;;; This file implements the chart parser.
;;; A rule is applied in a standard fashion (see below). In case a mother node
;;; can be constructed, the dags of the daughter nodes (which are already) 
;;; recorded in the chart) are unified, following the constraints specified 
;;; by the unification patterns on the rule. The process of  unification 
;;; either blocks the actual application of the rule (due to  failure to meet
;;; constraints), or triggers a success (in which case the new dag - the 
;;; result of the unification) is associated with the node constructed for the
;;; mother. This information is recorded in the chart, and the process 
;;; continues.
;;;      The chart itself is kept and can be displayed by the top level
;;; commands.

;;; The chart structure
;;;
;;; The chart is an array indexed by the vertex number 
;;; This is slightly inelegant in that the size of the array is set.
;;; Vertices are integers, not atoms.

(defvar *executed-tasks* 0)
(defvar *successful-tasks* 0)
(defvar *contemplated-tasks* 0)
(defvar *filtered-tasks* 0)
(declaim (type fixnum *executed-tasks* *successful-tasks* 
	       *contemplated-tasks* *filtered-tasks*))

(defvar *parser-rules* nil)
(defvar *parser-lexical-rules* nil)

;;; *chart-limit* is defined in globals.lsp

(defvar *chart-generation-counter* 0
  "a counter used by the user interface to make sure parse tree windows 
   etc have the current chart")

(defvar *chart* (make-array (list *chart-limit* 2))) 

(defvar *parse-record* nil)

;;; *chart* is a vector of chart-entry structures - one for each end vertex

(defstruct (chart-entry) configurations)

;;; chart-entry-configurations is a list of chart-configurations

(defstruct (chart-configuration) 
    begin edge roots end)

;;; begin is a vertex - edge is an edge 

;;; an edge is a structure
;;; it has the following properties
;;; category - eg S VP etc - now a type
;;; rule - either the word (storms etc) or the grammar rule itself which has
;;; been applied to create that edge  
;;; dag - the dag associated with the constituent
;;; leaves - orthography of whatever this edge has been formed from
;;; lex-ids - like leaves, but identifiers of whole structures
;;; children - daughter edges
;;;
;;; `foo,' `bar', and `baz' are junk slots for various pieces of code (e.g. the
;;; Redwoods annotation tool and PCFG estimation) to use at their discretion.
;;;                                                           (28-oct-02; oe)
(defstruct
   (edge
      (:constructor make-edge-x
                    (&key id score category rule dag odag 
                          (dag-restricted (when dag
                                            (restrict-fs (tdfs-indef dag))))
                          leaves lex-ids parents children morph-history 
                          spelling-change orth-tdfs
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
                          mrs
                          foo bar baz
                          packed equivalent frozen adjuncts)))
   id score category rule dag odag dag-restricted leaves lex-ids
   parents children morph-history spelling-change orth-tdfs from to label head
   cfrom cto mrs foo bar baz
   packed equivalent frozen adjuncts)

(defparameter *characterize-p* nil)

(defun make-edge (&rest rest)
  (let ((new-edge
	 (apply #'make-edge-x rest)))
    (when *characterize-p*
      (set-characterization (edge-dag new-edge)
			    (edge-cfrom new-edge)
			    (edge-cto new-edge)))
    new-edge))


(defmethod print-object ((instance edge) stream)
  (format 
   stream 
   "#[Edge # ~d: `~(~a~)' <~{~a~^ ~}>]"
   (edge-id instance)
   (let ((rule (edge-rule instance)))
     (if rule 
       (typecase rule
         (string rule)
         (rule (rule-id rule)))
       (edge-category instance)))
   (loop 
       for child in (edge-children instance)
       collect (if (edge-p child) (edge-id child) "_"))))

(defstruct
   (mrecord
      (:constructor make-mrecord
                    (&key fs (fs-restricted (restrict-fs (tdfs-indef fs)))
                          lex-ids rules history)))
   fs fs-restricted lex-ids rules history)

(defstruct 
  (mhistory)
  rule
  fs
  new-spelling orth-tdfs)

(defvar *edge-id* 0)
(declaim (type fixnum *edge-id*))

(defparameter %edge-allowance% 0)

(defun next-edge (&optional type)
  (when (eq type :unpack)
    ;;
    ;; _fix_me_
    ;; better generalize all of this, maybe encapsulate state variables.
    ;;                                                       (16-dec-03; oe)
    (when (> (incf %edge-allowance%) 50000)
      (error "edge allowance overrun (~a)" *edge-id*)))
  (when (> *edge-id* (+ *maximum-number-of-edges* %edge-allowance%))
    (error "~%Probable runaway rule: parse/generate aborted 
             (see documentation of *maximum-number-of-edges*)"))
  (incf *edge-id*))

(defvar *morphs* (make-array (list *chart-limit*) :initial-element nil))

;;; *morphs* is added, paralleling *chart* to allow for
;;; multi-word entries (including non-decomposable idioms).
;;; multi-word entries may have affixation on any member
;;; but they span several vertices.  It's therefore
;;; necessary to treat them as individual words with respect to
;;; the orthographemic component, and to record the results of that
;;; on a structure, so that putative multiword entries can be checked
;;; when we get to the rightmost element.

(defstruct (morph-edge)
   id word morph-results cfrom cto)

;;;

(defvar *morph-records* nil)

;;;
;;; keep track of lexical items used in each parse: whenever we use the same
;;; lexical entry the second time, we better copy it first to avoid spurious
;;; reentrancies.  this mechanism should suffice to remove a test in the parser
;;; that currently checks for duplicates among the edges feeding into a rule.
;;; --- come to think of it, this might also be useful in resetting temporary
;;; pointers in feature structures that are part of the grammar ...
;;;
(defvar *lexical-entries-used* nil)

;;; Agenda stuff - the agenda is represented as a heap (as in of Cormen,
;;; Leiserson, and Rivest).  The heap is a tree stored in an array: the car of
;;; each array element is its key and the cdr is the value.

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
    (error "~%Too many pending tasks, probable runaway rule: parse/generate aborted 
             (see documentation of *maximum-number-of-tasks*)"))
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
    (error "This shouldn't happen!  Something's wrong with the parser."))
  (let ((max (shiftf (fast-aref a 1) (fast-aref a (heap-size a)))))
    (decf (heap-size a))
    (heapify a 1)
    (let ((entry (rest max)))
      (setf (rest max) nil)
      entry)))

(defun heap-extract-max-full (a)
  (when (< (heap-size a) 1)
    (error "This shouldn't happen!  Something's wrong with the parser."))
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
;;;
(defparameter *active-parsing-p* t)


;;; *morph-records* is just so that the morphological history
;;; (i.e. inflection, derivation rules and any zero-morpheme rules
;;; interspersed among them) can be displayed

(defun clear-chart nil
   (incf *chart-generation-counter*)
   (setf *parse-record* nil) 
   (loop for i from 0 upto (1- *chart-limit*)
       do (setf (aref *chart* i 0) nil)
	  (setf (aref *chart* i 1) nil))
   (fill *morphs* nil)
   (setf *morph-records* nil)
   (setf *edge-id* 0)
   (setf %edge-allowance% 0)
   (when *active-parsing-p* (clear-achart)))

;;; Entry point to this group of functions is parse which is passed the
;;; sentence as a list of strings and is called from the top level

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

(defun parse (bracketed-input &optional 
                              (show-parse-p *show-parse-p*) 
                              (first-only-p *first-only-p*))
  ;;
  ;; keep track of mutual dependencies between various configurations:
  ;;   - input bracketing is only available in passive mode;
  ;;   - passive best-first restricted to unary and binary rules.
  ;;
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
                         first-only-p)))
    (multiple-value-bind (user-input brackets-list)
        (if *bracketing-p*
          (initialise-bracket-list bracketed-input)
          (values bracketed-input nil))
      
      (when (> (length user-input) *chart-limit*)
        (error "Sentence `~a' too long - ~A words maximum (*chart-limit*)" 
               user-input *chart-limit*))

      (let ((*brackets-list* brackets-list)
            (*executed-tasks* 0) (*successful-tasks* 0)
            (*contemplated-tasks* 0) (*filtered-tasks* 0)
            (*parser-rules* (get-matching-rules nil nil))
            (*parser-lexical-rules* (get-matching-lex-rules nil))
            (*lexical-entries-used* nil)
            (*minimal-vertex* 0)
            (*maximal-vertex* (length user-input))
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
          (let ((*safe-not-to-copy-p* #-:cle t #+:cle nil))
            (add-morphs-to-morphs user-input)
            (catch :best-first
              (add-words-to-chart (and first-only-p (null *active-parsing-p*)
                                       (cons 0 (length user-input))))
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
                (find-spanning-edges 0 (length user-input)))))
          (push (get-internal-run-time) *parse-times*))
        (when show-parse-p (show-parse))
        (values *executed-tasks* *successful-tasks* 
                *contemplated-tasks* *filtered-tasks*)))))

(defstruct chared-word
  word
  cfrom
  cto)

(defun set-characterization (dag cfrom cto)
  (declare (ignore dag cfrom cto)))

(defun add-morphs-to-morphs (preprocessed-input)
  (if (consp (first preprocessed-input))
      (sppp-setup-morphs preprocessed-input)
    (let ((current 0)
	  (xml-p (chared-word-p (first preprocessed-input))))
      (dolist (token preprocessed-input)
        (let* ((base-word 
		(if xml-p 
		    (chared-word-word token)
		  token))
	       (word (string-upcase base-word))
	       (cfrom (if xml-p
			  (chared-word-cfrom token)
			-1))
	       (cto (if xml-p
			  (chared-word-cto token)
			-1))
               (new (+ current 1))
                (morph-poss 
                 (union
                  (filter-for-irregs
                   (remove-duplicates
                    (morph-analyse word)
                    :test #'equalp))
                  ;; filter is in rules.lsp
                  (find-irregular-morphs word) :test #'equalp)))
          (unless #+:ltemplates (template-p word) #-:ltemplates nil
            (unless morph-poss 
              (format t "~%Word `~A' is not in lexicon." word)
              (when *unknown-word-types* 
                (format t " Using unknown word mechanism."))))
          ;;
          ;; this test seems unnecessary, as later on there is a test for
          ;; coverage over the full string of tokens (`to-be-accounted-for');
          ;; furthermore, failure to create (zero-inflection) morph edges here
          ;; prevents us from efficient retrieval of lexical entries: moving 
          ;; to the PSQL lexical database, we want to be able to pull out 
          ;; multi-word lexical entries by just one of their tokens (i.e. the
          ;; inflected one, by default the last token for English).  hence, 
          ;; for input like `ad hoc' it must be possible for look-up to fail
          ;; on `ad' and later retrieve the multi-word entry when we process
          ;; `hoc'.  this seems to work okay, though some further inspection 
          ;; of related code, particularly the *unknown-word-types* mechanics,
          ;; (and more testing :-) would seem in order.  (22-oct-02; oe & dan)
          ;; 
          (when #-:null t #+:null (or morph-poss *unknown-word-types*)
            (setf (aref *morphs* current)
              (make-morph-edge :id current :word base-word 
                               :morph-results 
                               (or morph-poss (list (list word)))
			       :cfrom cfrom
			       :cto cto))
            (setf current new)))))))

(defun add-words-to-chart (f)
  (let ((current 0)
        (to-be-accounted-for (make-array (list *chart-limit*) 
                                          :initial-element nil)))
     ;; to-be-accounted for is needed because we cannot tell that a word is
     ;; impossible until after the whole sentence has been processed because
     ;; it may be part of a multi-word
     (loop
       (let ((morph-poss (aref *morphs* current)))
         (when (null morph-poss)
           (return nil))
         (incf current)
         (multiple-value-bind (ind-results multi-strings)
	     (add-word (morph-edge-word morph-poss)
		       (morph-edge-morph-results morph-poss) current
		       f (morph-edge-cfrom morph-poss)
		       (morph-edge-cto morph-poss))
           (unless (or ind-results multi-strings)
             (setf (aref to-be-accounted-for current)
	       (morph-edge-word morph-poss)))
	   ;; record the fact we haven't analysed this word
           (dolist (mstr multi-strings)
	     ;; wipe the record for multi-words which allow for it
	     (let ((words (split-into-words mstr)))
	       (dotimes (x (length words))
		 (setf (aref to-be-accounted-for (- current x)) 
		   nil)))))))
     (dotimes (y current)
       (when (aref to-be-accounted-for y)
         (format t "~%No sign can be constructed for `~(~a~)'" 
                 (aref to-be-accounted-for y))))))


(defun add-word (local-word morph-poss right-vertex f cfrom cto)
  ;; get-senses returns a list of conses of ids and dags corresponding to the
  ;; word senses - the type of the dag is used to do the indexing
  (let* ((multi-results (add-multi-words morph-poss right-vertex f cfrom cto))
         (word-senses 
          (if #+:ltemplates (template-p local-word) #-:ltemplates nil
            #+:ltemplates
            (let* ((template (retrieve-template local-word))
                   (surface (or (get-template-surface template) local-word))
                   (tdfs (when template (instantiate-template template))))
              (when tdfs
                (setf local-word surface)
                (list (make-mrecord :lex-ids (list (intern local-word))
                                    :fs tdfs
                                    :rules nil))))
            #-:ltemplates nil
            (loop for morph-res in morph-poss
                append
                  (loop for sense in (get-senses (car morph-res))
                      append
                        (if (cdr morph-res)
                            (apply-all-lexical-and-morph-rules 
                             (list (make-mrecord :lex-ids (list (car sense))
                                                 :fs (cdr sense) 
                                                 :rules (cdr morph-res)))
                             f)
                          (list (make-mrecord :lex-ids (list (car sense))
                                              :fs (cdr sense) 
                                              :rules nil))))))))
    (dolist (mrec word-senses)
      (let* ((lex-ids (mrecord-lex-ids mrec))
             (sense (mrecord-fs mrec))
             (history (mrecord-history mrec))
             (edge (construct-lex-edge sense history local-word lex-ids
                                       (- right-vertex 1) right-vertex
				       cfrom cto)))
        (if *active-parsing-p*
          (let ((configuration (make-chart-configuration
                                :begin (- right-vertex 1) :end right-vertex
                                :edge edge)))
            (lexical-task (lex-priority mrec) configuration))
          (with-agenda (when f (lex-priority mrec))
            (activate-context (- right-vertex 1) edge right-vertex f)))))
    ;; add-multi-words is mostly for side effects, but want to check if we've
    ;; found something, and produce correct error messages, so we return the
    ;; strings found
    (values word-senses multi-results)))

(defun construct-lex-edge (sense history word lex-ids from to cfrom cto)
  #+ignore (format t "~%Construct word ~A" word)
  (make-edge :id (next-edge) 
             :category (indef-type-of-tdfs sense) 
             :rule (if history
                      (mhistory-rule (car history))
		      word)
             :dag sense
             :leaves (list word)
             :lex-ids lex-ids
             :morph-history (construct-morph-history word lex-ids 
                                                     history from to cfrom cto)
             :spelling-change (when history 
				(mhistory-new-spelling (car history)))
             :orth-tdfs (when history (mhistory-orth-tdfs (car history)))
             :from from
             :to to
	     :cfrom cfrom
	     :cto cto))


(defun get-senses (stem-string)
  #+:arboretum
  (declare (special *mal-active-p*))
  (let ((entries
         (loop for entry in (get-unexpanded-lex-entry stem-string)
             nconc
               (when (not (cdr (lex-entry-orth entry)))
                 ;; exclude multi-words
                 (let* ((id (lex-entry-id entry))
                        (expanded-entry (get-lex-entry-from-id id))
                        (tdfs (when expanded-entry 
                                (lex-entry-full-fs expanded-entry))))
                   (when (and tdfs
                              #+:arboretum
                              (or *mal-active-p* 
                                  (not (mal-lex-entry-p expanded-entry))))
                     (list
                      (cons id
                            (cond
                             ((or *characterize-p*
				  (smember tdfs *lexical-entries-used*))
                              (copy-tdfs-completely tdfs))
                             (t 
                              (push tdfs *lexical-entries-used*)
                              tdfs))))))))))
    (or entries
        (generate-unknown-word-entries stem-string))))

;;; get-multi-senses has to return a structure

(defstruct (sense-record)
  word-string
  cfrom
  left-vertex
  lex-ids
  fs
  morph-res
  mrecs)
  
(defun add-multi-words (morph-poss right-vertex f cfrom cto)
  (declare (ignore cfrom))
  (let* ((multi-strings nil)
	 (word-senses 
          (loop for stem in (remove-duplicates 
                        (loop for analysis in morph-poss
                             collect (car analysis)) :test #'equal)
	       ;; make sure we have all the possible stems in case inflection
	       ;; is going to be allowed on rightmost element but otherwise
	       ;; the variable morph-poss is not used
               append
               (loop for sense-record in (get-multi-senses stem right-vertex)
                    nconc
                    (let* ((sense (sense-record-fs sense-record))
                           (lex-ids (sense-record-lex-ids sense-record))
                           (new-morph-res 
                            (sense-record-morph-res sense-record))
                           (mrecs 
                            (if (cdr new-morph-res)
				(apply-all-lexical-and-morph-rules 
				 (list (make-mrecord :fs sense 
						     :lex-ids lex-ids
						     :rules 
						     (cdr new-morph-res)))
				 f)
                              (list (make-mrecord :fs sense 
                                                  :lex-ids lex-ids      
                                                  :rules nil)))))
                      (if mrecs
			  (progn
			    (setf (sense-record-mrecs sense-record) mrecs)
			    (list sense-record))))))))
    (dolist (sense-record word-senses)
      (let ((word (sense-record-word-string sense-record))
	    (left-vertex (sense-record-left-vertex sense-record))
	    (cfrom (sense-record-cfrom sense-record)))
	(push word multi-strings)
	(dolist (mrec (sense-record-mrecs sense-record))
	  (let* ((sense (mrecord-fs mrec))
                 (lex-ids (mrecord-lex-ids mrec))
                 (history (mrecord-history mrec))
                 (edge (construct-lex-edge sense history word lex-ids
                                           left-vertex right-vertex 
					   cfrom cto)))
            (if *active-parsing-p*
              (let ((configuration (make-chart-configuration
                                    :begin left-vertex :end right-vertex
                                    :edge edge)))
                (lexical-task (lex-priority mrec) configuration))
              (with-agenda (when f (lex-priority mrec))
                (activate-context left-vertex edge right-vertex f)))))))
    ;; return multi-strings, so we know what's been found
    multi-strings))


(defun get-multi-senses (stem-string right-vertex)
  (let* ((entries (get-unexpanded-lex-entry stem-string)))
    (loop for entry in (sort entries #'(lambda (x y) 
                                    (> (length (lex-entry-orth x))
                                       (length (lex-entry-orth y)))))
         append
         (if (cdr (lex-entry-orth entry))
	     (check-multi-word stem-string entry right-vertex
			       (lex-entry-id entry))))))


(defun check-multi-word (stem unexpanded-entry right-vertex id)
  (let ((entry-orth (lex-entry-orth unexpanded-entry))
        (ok t)
        (rules nil)
	(cfrom nil)
        (amalgamated-stems nil)
        (amalgamated-words nil)
        (inflection-position (lex-entry-infl-pos unexpanded-entry))) 
    (when (< right-vertex (length entry-orth))
      (return-from check-multi-word nil)) ; too near start of sentence
    (when (string-equal (car (last entry-orth)) stem)
      ;; only check multi-words when we have the rightmost
      (let ((current-vertex (- right-vertex (length entry-orth)))
	    (current-position 1))
	(dolist (word-stem entry-orth)
	  (let* ((morph-entry (aref *morphs* current-vertex))
		 (existing-word (morph-edge-word morph-entry)))
	    (unless cfrom
	      (setf cfrom (morph-edge-cfrom morph-entry)))
	    (if (eql current-position inflection-position)
		;; inflection allowed here
		(let ((current-morph-res 
		       (morph-edge-morph-results morph-entry)))
		  (unless
                      (let ((some-ok nil))
                        (loop for res in current-morph-res
                             do
                             (if (string-equal word-stem (car res))
                                 (progn (push (cdr res) rules)
                                        (setf some-ok t))))
                        some-ok)
		    (setf ok nil)
		    (return)))
	      ;; else cannot be inflected        
	      (if (string-equal word-stem existing-word)
                  (let ((current-morph-res 
                         (morph-edge-morph-results morph-entry)))
                    (unless
                      (dolist (res current-morph-res)
                        (when
                            (and (string-equal word-stem (car res))
                                 (null (cdr res)))
                          (return t)))
                      ;; this assumes there are no null affixes 
                      ;; found by the morphology program
                      (setf ok nil)
                      (return)))
                (progn
                  (setf ok nil)
                  (return))))
	    (push word-stem amalgamated-stems)
	    (push " " amalgamated-stems)
	    (push existing-word amalgamated-words)
	    (push " " amalgamated-words)
	    (incf current-vertex)
	    (incf current-position)))
	(when ok
	  (let ((expanded-entry (get-lex-entry-from-id id)))
	    (when expanded-entry
	      (let* ((full-stem-string 
		      (apply #'concatenate 'string 
			     (nreverse (cdr amalgamated-stems))))
		     (full-word-string 
		      (apply #'concatenate 'string 
			     (nreverse (cdr amalgamated-words)))))
                (loop for rule-set in (or rules (list nil))
                     collect
                     (let ((left-vertex (- right-vertex (length entry-orth))))
                       (make-sense-record :word-string full-word-string
                                          :left-vertex left-vertex
					  :cfrom cfrom
                                          :morph-res 
                                          (if rule-set
                                              (cons full-stem-string 
                                                    (loop for rule-rec in rule-set
                                                         collect
                                                         (cons (car rule-rec)
                                                               (list full-word-string))))
;;; this isn't quite right - if there are multiple affixes,
;;; the effect will be to put the fully inflected form on all of
;;; them.  FIX sometime.                                            
                                            (list full-stem-string))
                                          :lex-ids (list (lex-entry-id
                                                          expanded-entry))
                                          :fs (lex-entry-full-fs 
					       expanded-entry))))))))))))


(defun construct-morph-history (word lex-ids history from to cfrom cto)
  ;; the rule on an edge refers `back' i.e. to the way it was constructed, so
  ;; when this is called, the rule and the new spelling (if any) of the
  ;; current-record have already been put into an edge
  (when history
    (let* ((current-record (car history))
	   (fs (mhistory-fs current-record))
	   (new-edge (construct-lex-edge fs (cdr history) word lex-ids
                                         from to cfrom cto)))
      (push new-edge *morph-records*)
      new-edge)))
    


(defun apply-all-lexical-and-morph-rules (entries f)
  ;; This function applies morphological rules, possibly interleaved with
  ;; lexical rules, but terminating when the last morphologically significant
  ;; rule has been applied, since the parser will take care of the rest
  ;;
  ;; entries is a list of mrecords - current-fs, morph-rule-ids, history (the
  ;; history is nil initially, then a list of mhistory structures)
  ;;
  ;; the function returns a list of such mrecords, though the second element
  ;; will be nil in each case
  ;;
  (let ((transformed-entries 
	 (loop for entry in entries
	      append
	      (let* ((fs (mrecord-fs entry))
                     (fs-restricted (mrecord-fs-restricted entry))
                     (lex-ids (mrecord-lex-ids entry))
                     (morph-rules (mrecord-rules entry))
                     (history (mrecord-history entry))
                     (daughter (if history
                                 (mhistory-rule (first history))
                                 (first lex-ids))))
		(if (>=  (length history) *maximal-lex-rule-applications*)
		    (progn (format t 
				   "~%Warning - probable circular lexical rule") 
			   nil)
		  (append
		   (loop for rule in *parser-lexical-rules*
			nconc
			(let ((result (apply-morph-rule 
				       rule fs fs-restricted nil daughter)))
			  (if result 
			      (list (make-mrecord :fs result
					    :lex-ids lex-ids
					    :rules morph-rules 
					    :history (cons
						      (make-mhistory 
						       :rule rule
						       :fs fs
						       :new-spelling nil)
						      history))))))
		   (when morph-rules
		     (let* ((morph-rule-info (car morph-rules))
			    (new-orth (cadr morph-rule-info))
                            (orth-tdfs (when new-orth 
                                         (make-orth-tdfs new-orth)))
			    (rule-id (car morph-rule-info))
			    (rule-entry (get-lex-rule-entry rule-id))
			    (result
			     (if rule-entry
				 (apply-morph-rule
				  rule-entry fs fs-restricted orth-tdfs
                                  daughter))))
		       (unless rule-entry
			 (format t 
				 "~%Warning: rule ~A specified by ~
                                       morphology was not found"
				 rule-id))
		       (when result 
			 (list (make-mrecord 
                                :fs result 
                                :lex-ids lex-ids
                                :rules (cdr morph-rules)
                                :history (cons (make-mhistory 
                                                :rule rule-entry
                                                :fs fs
                                                :new-spelling new-orth
                                                :orth-tdfs orth-tdfs)
                                               history))))))))))))
    (if transformed-entries
	(append (remove-if #'mrecord-rules transformed-entries)
		(apply-all-lexical-and-morph-rules 
		 (remove-if-not #'mrecord-rules transformed-entries) f)))))


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


(defun activate-context (left-vertex edge right-vertex f)
  #+:pdebug
  (format 
   t 
   "~&activate-context(): edge # ~d: [~d -- ~d];~%"
   (edge-id edge) left-vertex right-vertex)

  (add-to-chart left-vertex edge right-vertex f)
  (dolist (rule *parser-rules*)
    ;; grammar rule application is attempted when we've got all the bits
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
                              f 0))))


(defun add-to-chart (left edge right f)
  ;; Find an existing chart-entry structure if there is one and add a
  ;; new chart-configuration to it, otherwise build up a new
  ;; chart-entry
  (let ((item (aref *chart* right 0))
	(config (make-chart-configuration :begin left :edge edge :end right)))
    (if item 
	(push config (chart-entry-configurations item))
      (setf (aref *chart* right 0)
	(make-chart-entry :configurations (list config))))
    ;; When doing a best-first parse, we need to index chart edges
    ;; by both the end vertex and the start vertex
    (when f
      (let ((item2 (aref *chart* left 1)))
	(if item2
	    (push config (chart-entry-configurations item2))
	  (setf (aref *chart* left 1)
	    (make-chart-entry :configurations (list config))))))
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
  ;; a check is made to see whether its addition triggers a rule application.
  ;; (That is whether it has a category corresponding to the rightmost
  ;; daughter of a grammar rule - checked before calling apply-grammar-rule
  ;; and whether edges corresponding to the other daughter categories are
  ;; already on the chart.)  If yes collect the dags associated with the
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
  (incf *contemplated-tasks*)
  (if (and (check-rule-filter rule (edge-rule (car child-edge-list)) n)
	   (restrictors-compatible-p (car rule-restricted-list) 
				     (edge-dag-restricted 
				      (car child-edge-list))))
      (if (cdr rule-restricted-list)
	  (let ((entry (aref (the (simple-array t (* *)) *chart*) 
			     left-vertex 0)))
	    (if entry
	      (dolist (config (chart-entry-configurations entry) t)
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
                  #+:vanilla t))
              t)) ;; must return t, because we don't want an outer
                  ;; loop to throw 
	;; we've got all the bits
	(with-agenda (when f (rule-priority rule))
	  (apply-immediate-grammar-rule rule left-vertex right-vertex
					child-edge-list f t)))
    (progn (incf *filtered-tasks*) t)))

(defun try-grammar-rule-right (rule rule-restricted-list left-vertex 
			       right-vertex child-edge-list f n)
  (declare (type fixnum n))
  (incf *contemplated-tasks*)
  (if (and (check-rule-filter
              rule (edge-rule (car child-edge-list)) n)
	   (restrictors-compatible-p (car rule-restricted-list)
				     (edge-dag-restricted 
                                      (car child-edge-list))))
      (if (cdr rule-restricted-list)
	  (let ((entry (aref (the (simple-array t (* *)) *chart*)
                             right-vertex 1)))
	    (if entry
	      (dolist (config (chart-entry-configurations entry) t)
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
                  #+:vanilla t))
                t))
	;; we've got all the bits
	(with-agenda (when f (rule-priority rule))
	  (apply-immediate-grammar-rule rule left-vertex right-vertex 
					child-edge-list f nil)))
    (progn (incf *filtered-tasks*) t)))


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
                                 nil child-edge-list-reversed backwardp)
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
			       :from left-vertex
			       :to right-vertex)))
              #+pdebug (format t " ... success.~%")
              (activate-context left-vertex new-edge right-vertex f)
              t)
          (progn
            #+pdebug (format t " ... ~:[fail~;throw~].~%" first-failed-p)
            (if first-failed-p nil t))))))))


(defun evaluate-unifications (rule child-fs-list 
			      &optional nu-orth child-edge-list backwardp)
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
       ;;
       ;; _fix_me_
       ;; the entire `nu-orth' mechanism is, hmm, a little baroque: results of
       ;; make-orth-tdfs() are cached (presumably so they can be retrieved from
       ;; within a unification context, apparently in tree reconstruction) and
       ;; cause grief when attempting to construct a (display) tree for an edge
       ;; after the orthography TDFS cache has been cleared (e.g. by parsing).
       ;; personally, i believe that the cache should go and everybody in their
       ;; right minds will cache those TDFSs within the edge.  however, since a
       ;; number of other functions rely on evaluate-unifications(), allow both
       ;; a string of TDFS as the value for .nu-orth. here :-{.  (7-aug-03; oe)
       ;; 
       (new-orth-fs (when nu-orth
                      (if (tdfs-p nu-orth) nu-orth (make-orth-tdfs nu-orth)))))
    ;; shouldn't strictly do this here because we may not need it but
    ;; otherwise we get a nested unification context error - cache the values
    ;; for a word, so it's not reconstructed only wasted if the morphology is
    ;; wrong
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
	   (t (incf *filtered-tasks*)
	      (return-from evaluate-unifications nil))))
	(incf *executed-tasks*)
	(let ((child (pop child-fs-list)))
	  ;; If two daughters are eq, the unifier's subgraph sharing code may
	  ;; cause spurious coreferences in the result
	  (when (member child child-fs-list :test #'eq)
	    (setq child (copy-tdfs-completely child)))
	  (if (setq current-tdfs
		(yadu current-tdfs
		      (create-temp-parsing-tdfs child rule-feat)))
	      (incf *successful-tasks*)
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
		      (copy-tdfs-elements result))))
		(or res
		    ;; charge copy failure to last successful unification
		    (progn (decf *successful-tasks*) nil))))))))))


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

;;; evaluate-unifications-with-fail-messages - temporarily removed

(defun find-spanning-edges (start-vertex end-vertex)
  ;; Returns all edges between two vertices and checks for root conditions -
  ;; used to see if a parse has been found.
  (let ((start-symbols (if (listp *start-symbol*)
			   *start-symbol*
			 (list *start-symbol*)))
	(chart-index (aref *chart* end-vertex 0)))
    (when chart-index
      (loop for item in (chart-entry-configurations chart-index)
	   append
	   (when (eql (chart-configuration-begin item) start-vertex)
	     ;; root may be a list of (td)fs with the interpretation that
	     ;; if any of them match the parse is OK
	     (if (null start-symbols)
		 (list (chart-configuration-edge item))
	       (if *substantive-roots-p*
		   (create-new-root-edges item start-symbols
					  start-vertex end-vertex)
		 (filter-root-edges item start-symbols))))))))

;; Decide if a single edge is a successful parse (when looking for the
;; first parse only).

(defun find-spanning-edge (item start-vertex end-vertex)
  (when (eql (chart-configuration-begin item) start-vertex)
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


;;; TTY printout of chart
;;; chart edges are ordered on: right vertex, left vertex, edge id

(defun print-chart (&key frozen concise (stream t))
  (format stream "~% > chart dump:~%")
  (loop
      for i from 1 to *chart-limit*
      while (print-chart-entry i (aref *chart* i 0) 
                               :frozen frozen :concise concise :stream stream))
  (terpri))

(defun print-chart-entry (vertex item &key frozen concise (stream t))
  (if item 
    (progn
      (terpri)
      (dolist
         (configuration
            (sort (copy-list (chart-entry-configurations item))
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
                          :frozen frozen :concise concise :stream stream))
      t)
    (aref *morphs* vertex)))      ; return t if we might be in the middle
                                  ; of a multi word

(defun print-chart-item (item 
                         &optional end 
                         &key (frozen nil frozenp)
                              concise (stream t))
  (let ((edge (if (edge-p item) item (chart-configuration-edge item)))
        (begin (unless (edge-p item) (chart-configuration-begin item)))
        (roots (unless (edge-p item) (chart-configuration-roots item))))
    (when (or (null frozenp) (eq (edge-frozen edge) frozen))
      (format 
       stream 
       "~&~:[~2*~;~A-~A ~][~A] ~A => ~A~A  [~{~A~^ ~}]"
       (and begin end) begin end
       (edge-id edge)
       (if concise (concise-edge-label edge) (edge-category edge))
       (edge-leaves edge)
       (if roots "*" "")
       (loop for child in (edge-children edge) collect (edge-id child)))
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
      (format stream "~%"))))
      
(defun concise-edge-label (edge)
  (if (rule-p (edge-rule edge))
    (rule-id (edge-rule edge)) 
    (first (edge-lex-ids edge))))

;;; Parsing sentences from file

(defun parse-sentences (&optional input-file (output-file 'unspec))
   (unless input-file 
      (setq input-file (ask-user-for-existing-pathname "Sentence file?")))
   (when
      (and input-file
         (or (probe-file input-file)
            (error "Input file ~A does not exist" input-file)))
      (with-open-file (istream input-file :direction :input)
         (when (eq output-file 'unspec)
            (setq output-file (ask-user-for-new-pathname "Output file?"))
            (when (equal input-file output-file)
              (error "Attempt to overwrite input file `~a'" input-file))
            (unless output-file (return-from parse-sentences)))
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

(defparameter *sentence* nil)

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
         (when (eql (rem nsent 50) 49)
            (clear-expanded-lex))      ; try and avoid image increasing
                                       ; at some speed cost
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
                       (format t "~%Memory allocation problem: ~A caused by ~A~%" condition user-input)) 
                       #-:gdebug
                       (error (condition)
                              (format t  "~%Error: ~A caused by ~A~%" condition user-input)))
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
          (collect-unary-rule-names (car (edge-children edge-rec))))
    (if (edge-morph-history edge-rec)
        (cons (rule-id (edge-rule edge-rec))
              (collect-morph-history-rule-names 
               (edge-morph-history edge-rec))))))

(defun collect-morph-history-rule-names (edge-rec)
  (if (edge-morph-history edge-rec)
      (cons (rule-id (edge-rule edge-rec))
            (collect-morph-history-rule-names 
             (edge-morph-history edge-rec)))))


;;; generator structures 

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
   )

