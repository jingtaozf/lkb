;; Copyright Ann Copestake 1992-1998
;;; All Rights Reserved.
;;; No use or redistribution without permission.
;;; 

(in-package :cl-user)

;;; Port to MCL - moved dialect specific display stuff to parseout.lsp

;;; Modified 03/93 to incorporate LKB morphology system - BEMJ
;;;
;;; mods to: add-word
;;;          evaluate-unifications
;;; new fns: process-morph-possibilities
;;;          apply-morph-rule
;;; AAC 1995 - junked most of Bernie's mods - call morph-analyse and
;;; proceed from there ...

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
;;; Control strategy is purely bottom up, left corner and not the world's most
;;; efficient.

;;; The chart structure
;;;
;;; The chart is an array indexed by the vertex number 
;;; This is slightly inelegant in that the size of the array is set.
;;; Vertices are integers, not atoms.

(defvar *executed-tasks* 0)
(defvar *successful-tasks* 0)
(defvar *contemplated-tasks* 0)
(defvar *filtered-tasks* 0)

(defvar *cached-orth-str-list* nil)

;;; *chart-limit* is defined in globals.lsp

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
;;; rule-number - either the word (storms etc) or the number of the grammar
;;; rule which has been applied to create that edge  
;;; dag - the dag associated with the constituent
;;; leaves - orthography of whatever this edge has been formed from
;;; lex-ids - like leaves, but identifiers of whole structures
;;; children - daughter edges
 
(defstruct
   (edge
      (:constructor make-edge
                    (&key id category rule-number dag 
                          (dag-restricted (restrict-fs (tdfs-indef dag)))
                          leaves lex-ids children morph-history 
                          spelling-change)))
   id category rule-number dag dag-restricted leaves lex-ids
   children morph-history spelling-change)

(defstruct
   (mrecord
      (:constructor make-mrecord
                    (&key fs (fs-restricted (restrict-fs (tdfs-indef fs)))
                          lex-ids rules history)))
   fs fs-restricted lex-ids rules history)

(defstruct 
  (mhistory)
  rule-id
  fs
  new-spelling)

(defvar *edge-id* 0)

(defun next-edge nil
  (when (> *edge-id* *maximum-number-of-edges*)
    (error "~%Probable runaway rule: parse/generate aborted 
             (see documentation of *maximum-number-of-edges*)"))
  (incf *edge-id*)
  *edge-id*)

(defvar *morphs* (make-array (list *chart-limit*) :initial-element nil))

;;; *morphs* is added, paralleling *chart* to allow for
;;; multi-word entries (and eventually idioms perhaps).
;;; multi-word entries may have affixation on any member
;;; but they span several vertices.  It's therefore
;;; necessary to treat them as individual words with respect to
;;; the orthographemic component, and to record the results of that
;;; on a structure, so that putative multiword entries can be checked
;;; when we get to the rightmost element.

(defstruct (morph-edge)
   id word morph-results)

;;;

(defvar *morph-records* nil)

;;; Agenda stuff - the agenda is represented as a heap (as in of Cormen,
;;; Leiserson, and Rivest).  The heap is a tree stored in an array: the car of
;;; each array element is its key and the cdr is the value.

(defmacro parent (i)
  `(the fixnum (floor (the fixnum ,i) 2)))

(defmacro left (i)
  `(the fixnum (* (the fixnum ,i) 2)))

(defmacro right (i)
  `(the fixnum (1+ (* (the fixnum ,i) 2))))

(defmacro heap-size (a)
  `(the fixnum (aref ,a 0)))

(defun heapify (a i)
  (let* ((l (left i))
	 (r (right i))
	 (largest (if (and (<= l (heap-size a))
			   (> (car (aref a l)) (car (aref a r))))
		      l
		    i)))
    (when (and (<= r (heap-size a))
	       (> (car (aref a r)) (car (aref a largest))))
      (setq largest r))
    (unless (eql largest i)
      (rotatef (aref a i) (aref a largest))
      (heapify a largest))))

(defun heap-insert (a key value)
  (incf (heap-size a))
  (when (>= (heap-size a)  (array-dimension a 0))
    (error "~%Too many pending tasks, probable runaway rule: parse/generate aborted 
             (see documentation of *maximum-number-of-tasks*)"))
  (loop 
      with i = (heap-size a)
      while (and (> i 1)
		 (< (car (aref a (parent i))) key))
      do (setf (aref a i) (aref a (parent i)))
	 (setf i (parent i))
      finally (setf (aref a i) (cons key value)))
  value)

(defun heap-extract-max (a)
  (when (< (heap-size a) 1)
    (error "This shouldn't happen!  Something's wrong with the parser."))
  (let ((max (shiftf (aref a 1) (aref a (heap-size a)))))
    (decf (heap-size a))
    (heapify a 1)
    (cdr max)))

(defun make-heap ()
  (let ((heap (make-array (list *maximum-number-of-tasks*))))
    (setf (aref heap 0) 0)
    heap))

(defun empty-heap (a)
  (zerop (aref a 0)))

(defun flush-heap (a)
  (setf (heap-size a) 0))

(defvar *agenda* (make-heap))

(defmacro with-agenda ((f priority) &body body)
  `(if ,f
       (heap-insert *agenda* ,priority #'(lambda () ,@body))
     (progn
       ,@body)))

;;; *morph-records* is just so that the morphological history
;;; (i.e. inflection, derivation rules and any zero-morpheme rules
;;; interspersed among them) can be displayed

(defun clear-chart nil
   (setf *cached-orth-str-list* nil)
   (setf *parse-record* nil) 
   (loop for i from 0 upto (1- *chart-limit*)
       do (setf (aref *chart* i 0) nil)
	  (setf (aref *chart* i 1) nil))
   (fill *morphs* nil)
   (setf *morph-records* nil)
   (setf *edge-id* 0))

;;; Entry point to this group of functions is parse which is passed the
;;; sentence as a list of strings and is called from the top level

(defvar *cached-category-abbs* nil
  "variable used in output to avoid recomputation of tree nodes")

(defun parse (user-input &optional (show-parse-p t) 
				   (first-only-p *first-only-p*))
  (if (> (length user-input) *chart-limit*)
      (error "Sentence ~A too long" user-input)
    (let ((*safe-not-to-copy-p* t)
	  (*executed-tasks* 0) (*successful-tasks* 0)
	  (*contemplated-tasks* 0) (*filtered-tasks* 0))
      (flush-heap *agenda*)
      (clear-chart)
      (setf *cached-category-abbs* nil)
      #+powerpc(setq aa 0 bb 0 cc 0 dd 0 ee 0 ff 0 gg 0 hh 0 ii 0 jj 0)
      (add-morphs-to-morphs user-input)
      (unless 
	  (catch 'first
	    (add-words-to-chart (when first-only-p 
				  (cons 0 (length user-input))))
	    (loop 
		until (empty-heap *agenda*)
		do (funcall (heap-extract-max *agenda*))))
	(setf *parse-record*
	  (find-spanning-edges 0 (length user-input))))
      (when show-parse-p (show-parse))
      (values *executed-tasks* *successful-tasks* *contemplated-tasks*
	      *filtered-tasks*))))

(defun add-morphs-to-morphs (user-input)
   (let ((current 0))
      (dolist (word user-input)
         (let* ((new (+ current 1))
                (morph-poss 
                 (progn #+:powerpc(decf gg (CCL::%HEAP-BYTES-ALLOCATED))
                        (prog1
                            (append 
                             (filter-for-irregs (morph-analyse word))
                             ;; filter is in rules.lsp
                             (find-irregular-morphs word))
                          #+:powerpc(incf gg (CCL::%HEAP-BYTES-ALLOCATED))))))
           (unless morph-poss (format t "~%Word ~A is not in lexicon" word)
                   (return))
           (setf (aref *morphs* current)
                 (make-morph-edge :id current 
                                  :word word 
                                  :morph-results morph-poss))
           (setf current new)))))

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
		       f)
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
         (format t "~%No sign can be constructed for ~A" 
                 (aref to-be-accounted-for y))))))


(defun add-word (local-word morph-poss right-vertex f)
  ;; get-senses returns a list of conses of ids and dags corresponding to the
  ;; word senses - the type of the dag is used to do the indexing
  (let* ((multi-results (add-multi-words morph-poss right-vertex f))
         (word-senses 
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
					    :fs (cdr sense) :rules nil)))))))
    (dolist (mrec word-senses)
      (let ((lex-ids (mrecord-lex-ids mrec))
	    (sense (mrecord-fs mrec))
	    (history (mrecord-history mrec)))
	(with-agenda (f (when f (lex-priority mrec)))
	  (activate-context (- right-vertex 1) 
			    (construct-lex-edge sense history local-word
						lex-ids)
			    right-vertex
			    f))))
    ;; add-multi-words is mostly for side effects, but want to check if we've
    ;; found something, and produce correct error messages, so we return the
    ;; strings found
    (values word-senses multi-results)))

(defun construct-lex-edge (sense history word lex-ids)
  #+ignore (format t "~%Construct word ~A" word)
  (make-edge :id (next-edge) 
             :category (indef-type-of-tdfs sense) 
             :rule-number (if history (mhistory-rule-id 
				       (car history))
			    word)
             :dag sense
             :leaves (list word)
             :lex-ids lex-ids
             :morph-history (construct-morph-history lex-ids history)
             :spelling-change (when history 
				(mhistory-new-spelling (car history)))))

;; RPM (18-Aug-1998) This originally just marked lexical entries as unsafe, so
;; as soon as they successfully unified with something they'd get copied.  That
;; doesn't work if you have the same lexical entry appearing twice as daughters
;; of a single rule (as in "Kim gave Sandy Sandy") Our solution is to copy all
;; lexical entries no matter what, but this runs the risk of copying lexical
;; entries which never wind up being part of an edge in the chart.  

;(defmacro protect (fs)
;  `(copy-tdfs-completely ,fs))

(defmacro protect (fs)
  fs)

(defun get-senses (stem-string)
  (let* (;;(*safe-not-to-copy-p* nil)
         (entries (get-unexpanded-lex-entry 
                   (string-upcase stem-string))))
    (for entry in entries
         filter
         (if (not (cdr (lex-or-psort-orth entry)))
             ;; exclude multi-words
             (let* ((id (lex-or-psort-id entry))
                    (expanded-entry
                     (get-psort-entry id)))
               (when expanded-entry
                 (cons id 
		       (protect (lex-or-psort-full-fs expanded-entry)))))))))

;;; get-multi-senses has to return a structure

(defstruct (sense-record)
  word-string
  left-vertex
  lex-ids
  fs
  morph-res
  mrecs)
  
(defun add-multi-words (morph-poss right-vertex f)
  (let* ((multi-strings nil)
	 (word-senses 
          (for stem in (remove-duplicates 
                        (for analysis in morph-poss
                             collect (car analysis)) :test #'string-equal)
	       ;; make sure we have all the possible stems in case inflection
	       ;; is going to be allowed on rightmost element but otherwise
	       ;; the variable morph-poss is not used
               append
               (for sense-record in (get-multi-senses stem right-vertex)
                    filter
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
			    sense-record)))))))                      
    (dolist (sense-record word-senses)
      (let ((word (sense-record-word-string sense-record))
	    (left-vertex (sense-record-left-vertex sense-record)))
	(push word multi-strings)
	(dolist (mrec (sense-record-mrecs sense-record))
	  (let ((sense (mrecord-fs mrec))
		(lex-ids (mrecord-lex-ids mrec))
		(history (mrecord-history mrec)))
	    (with-agenda (f (when f (lex-priority mrec)))
	      (activate-context left-vertex 
				(construct-lex-edge sense history word
						    lex-ids)      
				right-vertex f))))))
    ;; return multi-strings, so we know what's been found
    multi-strings))


(defun get-multi-senses (stem-string right-vertex)
  (let* (;; (*safe-not-to-copy-p* nil)
	 (entries (get-unexpanded-lex-entry (string-upcase stem-string))))
    (for entry in (sort entries #'(lambda (x y) 
                                    (> (length (lex-or-psort-orth x))
                                       (length (lex-or-psort-orth y)))))
         append
         (if (cdr (lex-or-psort-orth entry))
	     (check-multi-word stem-string entry right-vertex
			       (lex-or-psort-id entry))))))


(defun check-multi-word (stem unexpanded-entry right-vertex id)
  (let ((entry-orth (lex-or-psort-orth unexpanded-entry))
        (ok t)
        (new-morph-res nil)
        (amalgamated-stems nil)
        (amalgamated-words nil)
        (inflection-position (lex-or-psort-infl-pos unexpanded-entry))) 
    (when (< right-vertex (length entry-orth))
      (return-from check-multi-word nil)) ; too near start of sentence
    (when (string-equal (car (last entry-orth)) stem)
      ;; only check multi-words when we have the rightmost
      (let ((current-vertex (- right-vertex (length entry-orth)))
	    (current-position 1))
	(dolist (word-stem entry-orth)
	  (let* ((morph-entry (aref *morphs* current-vertex))
		 (existing-word (morph-edge-word morph-entry)))
	    (if (eql current-position inflection-position)
		;; inflection allowed here
		(let ((current-morph-res 
		       (morph-edge-morph-results morph-entry)))
		  (setf new-morph-res
		    (for res in current-morph-res
			 filter
			 (if (string-equal word-stem (car res))
			     res)))
		  (unless new-morph-res
		    (setf ok nil)
		    (return)))
	      ;; else cannot be inflected        
	      (unless (string-equal word-stem existing-word)
		(setf ok nil)
		(return)))
	    (push word-stem amalgamated-stems)
	    (push " " amalgamated-stems)
	    (push existing-word amalgamated-words)
	    (push " " amalgamated-words)
	    (incf current-vertex)
	    (incf current-position)))
	(when ok
	  (let ((expanded-entry (get-psort-entry id)))
	    (when expanded-entry
	      (let* ((full-stem-string 
		      (apply #'concatenate 'string 
			     (nreverse (cdr amalgamated-stems))))
		     (full-word-string 
		      (apply #'concatenate 'string 
			     (nreverse (cdr amalgamated-words)))))
		(cons 
		 (make-sense-record :word-string full-word-string
				    :left-vertex (- right-vertex 
						    (length entry-orth))
				    :morph-res (list full-stem-string)
				    :lex-ids (list (lex-or-psort-id
						    expanded-entry))
				    :fs (protect (lex-or-psort-full-fs 
						  expanded-entry)))
		 (for rule in (for res in new-morph-res
				   filter
				   (caadr res))
		      collect
		      (make-sense-record :word-string full-word-string
					 :left-vertex (- right-vertex 
							 (length entry-orth))
					 :morph-res 
					 (list full-stem-string 
					       (list rule 
						     full-word-string))
					 :lex-ids (list (lex-or-psort-id
							 expanded-entry))
					 :fs (protect 
					      (lex-or-psort-full-fs 
					       expanded-entry)))))))))))))


(defun construct-morph-history (lex-ids history)
  ;;; the rule on an edge refers `back' i.e. to the way it was
  ;;; constructed, so when this is called, the rule-id and 
  ;;; the new spelling (if any) of the current-record have
  ;;; already been put into an edge
  (if history
      (let* ((current-record (car history))
             (fs (mhistory-fs current-record))
             (new-edge (construct-lex-edge fs (cdr history) nil lex-ids)))
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
	 (for entry in entries
	      append
	      (let ((fs (mrecord-fs entry))
		    (fs-restricted (mrecord-fs-restricted entry))
		    (lex-ids (mrecord-lex-ids entry))
		    (morph-rules (mrecord-rules entry))
		    (history (mrecord-history entry)))
		(if (>=  (length history) *maximal-lex-rule-applications*)
		    (progn (format t 
				   "~%Warning - probable circular lexical rule") 
			   nil)
		  (append
		   (for rule in (get-matching-lex-rules fs)
			filter
			(let ((result (apply-morph-rule 
				       rule fs fs-restricted nil)))
			  (if result 
			      (make-mrecord :fs result
					    :lex-ids lex-ids
					    :rules morph-rules 
					    :history (cons
						      (make-mhistory 
						       :rule-id (rule-id rule)
						       :fs fs
						       :new-spelling nil)
						      history)))))
		   (when morph-rules
		     (let* ((morph-rule-info (car morph-rules))
			    (new-orth (cadr morph-rule-info))
			    (rule-id (car morph-rule-info))
			    (rule-entry (get-lex-rule-entry rule-id))
			    (result
			     (if rule-entry
				 (apply-morph-rule
				  rule-entry fs fs-restricted new-orth))))
		       (unless rule-entry
			 (format t 
				 "~%Warning: rule ~A specified by ~
                                       morphology was not found"
				 rule-id))
		       (when result 
			 (list (make-mrecord :fs result 
					     :lex-ids lex-ids
					     :rules (cdr morph-rules)
					     :history (cons (make-mhistory 
							     :rule-id rule-id
							     :fs fs
							     :new-spelling new-orth)
							    history))))))))))))
    (if transformed-entries
	(append (remove-if #'mrecord-rules transformed-entries)
		(apply-all-lexical-and-morph-rules 
		 (remove-if-not #'mrecord-rules transformed-entries) f)))))


(defun apply-morph-rule (rule fs fs-restricted new-orth)
  (and
   (restrictors-compatible-p (car (rule-daughters-restricted rule))
			     fs-restricted)
   (evaluate-unifications rule (list fs) new-orth)))


(defun activate-context (left-vertex edge right-vertex f)
  (let ((no-unary nil))
    (add-to-chart left-vertex edge right-vertex f)
    (dolist (rule (get-matching-rules (edge-dag edge) no-unary))
      ;; grammar rule application is attempted when we've got all the bits
      (try-grammar-rule-left rule
			     (reverse (rule-daughters-restricted rule))
			     left-vertex
			     right-vertex
			     (list edge)
			     f)
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
				f)))))


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
	       (eql right (cdr f))
	       (setf *parse-record* (find-spanning-edge config (car f) 
							(cdr f))))
      (throw 'first t))))

(defun try-grammar-rule-left (rule rule-restricted-list left-vertex 
			      right-vertex child-fs-list f &optional n)
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
  (unless n
    (setq n (1- (length (rule-daughters-apply-order rule)))))
  (incf *contemplated-tasks*)
  (if (and (check-filter (rule-id rule) 
			 (edge-rule-number (car child-fs-list))
			 n)
	   (restrictors-compatible-p (car rule-restricted-list) 
				     (edge-dag-restricted (car child-fs-list))))
      (if (cdr rule-restricted-list)
	  (let ((entry (aref *chart* left-vertex 0)))
	    (when entry
	      (dolist (config (chart-entry-configurations entry))
		(try-grammar-rule-left rule
				       (cdr rule-restricted-list)
				       (chart-configuration-begin config)
				       right-vertex
				       (cons (chart-configuration-edge config) 
					     child-fs-list)
				       f
				       (1- n)))))
	;; we've got all the bits
	(with-agenda (f (when f (rule-priority rule)))
	  (apply-immediate-grammar-rule rule left-vertex right-vertex
					child-fs-list f)))
    (incf *filtered-tasks*)))

(defun try-grammar-rule-right (rule rule-restricted-list left-vertex 
			       right-vertex child-fs-list f &optional (n 0))
  (incf *contemplated-tasks*)
  (if (and (check-filter (rule-id rule) 
			 (edge-rule-number (car child-fs-list))
			 n)
	   (restrictors-compatible-p (car rule-restricted-list)
				     (edge-dag-restricted (car child-fs-list))))
      (if (cdr rule-restricted-list)
	  (let ((entry (aref *chart* right-vertex 1)))
	    (when entry
	      (dolist (config (chart-entry-configurations entry))
		(try-grammar-rule-right rule
					(cdr rule-restricted-list)
					left-vertex
					(chart-configuration-end config)
					(cons (chart-configuration-edge config)
					      child-fs-list)
					f (1+ n)))))
	;; we've got all the bits
	(with-agenda (f (when f (rule-priority rule)))
	  (apply-immediate-grammar-rule rule left-vertex right-vertex 
					(reverse child-fs-list) f)))
    (incf *filtered-tasks*)))


(defparameter *debugging* nil)

(defun apply-immediate-grammar-rule (rule left-vertex right-vertex 
				     child-fs-list f)
  ;; attempt to apply a grammar rule when we have all the parts which match
  ;; its daughter categories
  #+ignore
  (format t "~%Try Rule id ~A left ~A right ~A dtrs ~A" (rule-id rule)  
	  left-vertex right-vertex (mapcar #'edge-id child-fs-list))
  (let ((unification-result
	 (evaluate-unifications rule (mapcar #'edge-dag child-fs-list)
				nil child-fs-list)))
    (if unification-result
	(let ((new-edge (make-edge :id (next-edge)
                                   :category (indef-type-of-tdfs 
                                              unification-result)
                                   :rule-number (rule-id rule)
                                   :children child-fs-list
                                   :dag unification-result
                                   :lex-ids 
                                   (mapcan
                                    #'(lambda (child)
                                        (copy-list (edge-lex-ids child)))
                                    child-fs-list)
                                   :leaves
                                   (mapcan
                                    #'(lambda (child)
                                        (copy-list (edge-leaves child)))
                                    child-fs-list))))
	  #+ignore (format t " Succeed.")
	  (activate-context left-vertex new-edge right-vertex f))
      (progn
	#+ignore (format t " Fail.")
	(when *debugging*
	  #+ignore (format t "~%~A" *filtered-tasks*)
	  (format t "~%Unification failure on rule ~A and edges ~:A" 
		  (rule-id rule) (mapcar #'edge-id child-fs-list)))))))


(defun evaluate-unifications (rule child-fs-list &optional nu-orth child-edges)
  ;; An additional optional argument is given. This is the new orthography if
  ;; the unification relates to a morphological process. If it is present, it
  ;; is inserted in the resulting fs
  (let*
      ((current-tdfs (rule-full-fs rule))
       (rule-daughter-order (cdr (rule-order rule)))
       (rule-apply-order (rule-daughters-apply-order rule))
       (n 0)
       (new-orth-fs (if nu-orth (get-orth-tdfs nu-orth))))
    ;; shouldn't strictly do this here because we may not need it but
    ;; otherwise we get a nested unification context error - cache the values
    ;; for a word, so it's not reconstructed only wasted if the morphology is
    ;; wrong
    (with-unification-context (ignore)
      (dolist (rule-feat rule-apply-order)
	(let ((dtr (position rule-feat rule-daughter-order :test #'eq)))
	  (cond
	   ((eql (incf n) 1))
	   ((x-restrict-and-compatible-p
	     (if (listp rule-feat)
		 (x-existing-dag-at-end-of 
		  (tdfs-indef current-tdfs) rule-feat)
	       (x-get-dag-value (tdfs-indef current-tdfs) rule-feat))
	     (edge-dag-restricted (nth dtr child-edges))))
	   (t (incf *filtered-tasks*)
	      (return-from evaluate-unifications nil)))
	  (incf *executed-tasks*)
	  (if (setq current-tdfs
		(yadu current-tdfs
		      (create-temp-parsing-tdfs (nth dtr child-fs-list)
						rule-feat)))
	      (incf *successful-tasks*)
	    (return-from evaluate-unifications nil))))
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
          (for tail-element in (tdfs-tail tdfs)
               do
               (push (add-path-to-tail path tail-element) tail))))
      (make-tdfs :indef indef-dag :tail tail))))

(defun get-orth-tdfs (str)
  (or (cdr (assoc str *cached-orth-str-list* :test #'equal))
      (let ((new-orth-tdfs (make-orth-tdfs str)))
        (push (cons str new-orth-tdfs) *cached-orth-str-list*)
        new-orth-tdfs)))

;;; evaluate-unifications-with-fail-messages - temporarily removed

(defun find-spanning-edges (start-vertex end-vertex)
  ;; Returns all edges between two vertices and checks for root conditions -
  ;; used to see if a parse has been found.
  (let ((start-symbols (if (listp *start-symbol*)
			   *start-symbol*
			 (list *start-symbol*)))
	(chart-index (aref *chart* end-vertex 0)))
    (when chart-index
      (for item in (chart-entry-configurations chart-index)
	   append
	   (when (eql (chart-configuration-begin item) start-vertex)
	     ;; root may be a list of (td)fs with the interpretation that
	     ;; if any of them match the parse is OK
	     (if (null *start-symbol*)
		 (list (chart-configuration-edge item))
	       (if *substantive-roots-p*
		   (create-new-root-edges item start-symbols
					  start-vertex end-vertex)
		 (filter-root-edges item start-symbols))))))))

;; Decide if a single edge is a successful parse (when looking for the
;; first parse only).

(defun find-spanning-edge (item start-vertex end-vertex)
  (when (eql (chart-configuration-begin item) start-vertex)
    (if (null *start-symbol*)
	(list (chart-configuration-edge item))
      (if *substantive-roots-p*
	  (create-new-root-edges item *start-symbol* start-vertex end-vertex)
	(filter-root-edges item *start-symbol*)))))

(defun filter-root-edges (item start-symbols)
  (dolist (start-symbol start-symbols)
    (let ((root-spec (get-tdfs-given-id start-symbol)))
         (when root-spec
             (when (yadu root-spec
                         (edge-dag 
                          (chart-configuration-edge item)))
               (return (list (chart-configuration-edge item))))))))

(defun create-new-root-edges (item start-symbols start-vertex end-vertex)
  (for start-symbol in start-symbols        
       filter
       (let ((tdfs (get-tdfs-given-id 
                    start-symbol)))
         (if tdfs
            (let ((unif
                    (yadu tdfs
                          (edge-dag 
                            (chart-configuration-edge item)))))
               (if unif
                   (let ((new-edge
                          (make-edge :dag (copy-tdfs-elements unif)
                                     :id (next-edge)
                                     :category
                                     (indef-type-of-tdfs unif)
                                     :rule-number start-symbol
                                     :children 
                                     (list (chart-configuration-edge item))
                                     :lex-ids (edge-lex-ids
                                               (chart-configuration-edge item))
                                     :leaves
                                     (edge-leaves 
                                      (chart-configuration-edge item)))))
                     (add-to-chart start-vertex
                                   new-edge
                                   end-vertex
				   ;; Don't (recursively) check for success
				   nil)
                     new-edge)))))))


;;; TTY printout of chart
;;; chart edges are ordered on: right vertex, left vertex, edge id

(defun print-chart nil 
   (format t "~% > chart dump:~%")
   (dotimes (vertex (- *chart-limit* 1))
      (unless 
         (print-chart-entry (+ 1 vertex) (aref *chart* (+ 1 vertex) 0))
         (return nil)))
   (terpri))

(defun print-chart-entry (vertex item)
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
         (print-chart-configuration configuration vertex))
      t)
    (aref *morphs* vertex)))      ; return t if we might be in the middle
                                  ; of a multi word

(defun print-chart-configuration (span right-vertex)
   (let ((e (chart-configuration-edge span)))
      (format t "~A-~A [~A] ~A => ~A~A  [~{~A~^ ~}]~%"
         (chart-configuration-begin span)
         right-vertex
         (edge-id e)
         (edge-category e)
         (edge-leaves e)
         (if (chart-configuration-roots span) "*" "")
         (mapcar #'edge-id (edge-children e)))))


;;; Parsing sentences from file

(defun parse-sentences (&optional input-file parse-file run-file result-file)
  (declare (ignore run-file result-file))
   (unless input-file 
      (setq input-file (ask-user-for-existing-pathname "Sentence file?")))
   (when
      (and input-file
         (or (probe-file input-file)
            (error "Input file ~A does not exist" input-file)))
      (with-open-file (istream input-file :direction :input)
         (let ((line (read-line istream nil 'eof)))
            (cond
               ((eq line 'eof))
;;               ((eql (count #\@ line) 11)
;;                                        ; must be 12 fields in tsdb input
;;                (if (fboundp 'get-test-run-information)
;;                    (parse-tsdb-sentences1
;;                     istream line parse-file result-file run-file)
;;                 (batch-parse-sentences istream line parse-file
;;                                         #'get-tsdb-sentence)))
               (t
                  (batch-parse-sentences istream line parse-file)))))))


(defparameter *do-something-with-parse* nil)

(defparameter *lex-ids-used* nil)

(defparameter *sentence* nil)

(defparameter *ostream* nil)

(defun batch-parse-sentences (istream raw-sentence parse-file &optional access-fn)
  (setf *lex-ids-used* nil)
  (let* ((output-file 
            (or parse-file (ask-user-for-new-pathname "Output file?")))
         (start-time (get-internal-run-time)))
     (unless output-file (return-from batch-parse-sentences nil))
     (with-open-file (ostream output-file :direction :output
                      :if-exists :supersede :if-does-not-exist :create)
       (format t "~%Parsing test file")
        (loop
           (when (eql raw-sentence 'eof) (return))
           (let ((interim-sentence (if access-fn (apply access-fn (list raw-sentence))
                                     raw-sentence)))
             (unless (fboundp *do-something-with-parse*)
                     ;;; if we're doing something else, let that function
                     ;;; control the output
               (format ostream "~A~%" interim-sentence)
               (finish-output ostream))
             (let ((sentence (string-trim '(#\Space #\Tab) interim-sentence)))
               (unless (equal sentence "")
                 (let* ((munged-string 
                         (if (fboundp 'preprocess-sentence-string)
                             (preprocess-sentence-string sentence)
                           sentence))
                        (user-input (split-into-words munged-string)))
                   (handler-case
                       (progn (parse user-input nil)
                              (setf *sentence* user-input)
                              (setf *ostream* ostream)
                              (when (fboundp *do-something-with-parse*)
                                (funcall *do-something-with-parse*)))
                       (error (condition)
                              (format t  "~%Error: ~A caused by ~A~%" condition user-input)))                  
                   (unless (fboundp *do-something-with-parse*)
                     ;;; if we're doing something else, let that function
                     ;;; control the output
                     (let ((n (length *parse-record*)))
                       (format ostream "  ~R parse~:[s~;~] found~%" n (= n 1))
                       (finish-output ostream))))))
             (for lex-id in (collect-expanded-lex-ids *lexicon*)
                  do
                  (pushnew lex-id *lex-ids-used*))
             (clear-expanded-lex)       ; try and avoid image increasing
                                        ; at some speed cost
             (setq raw-sentence (read-line istream nil 'eof))))
        (format ostream ";;; Total elapsed time: ~A msecs~%" 
                (- (get-internal-run-time) start-time))
        (format t "~%Finished test file")
        (lkb-beep))))


;;; extracting a list of lexical entries used in a parse
;;; used for testing the generation lexical lookup algorithm

(defun retrieve-lex-from-parses nil
  (for edge in *parse-record*
       collect
       (edge-lex-ids edge)))

; (collect-parse-base (car *parse-record*))

(defun collect-parse-base (edge-rec)
  ;;; takes a top edge, returns a list of 
  ;;; lexical identifiers, unary-rule-list pairs
  (if (or (cdr (edge-lex-ids edge-rec))
          (not (get-lex-rule-entry (edge-rule-number edge-rec))))
      (for child in (edge-children edge-rec)
           append
           (collect-parse-base child))
    (list (cons (car (edge-lex-ids edge-rec))
          (nreverse (collect-unary-rule-names edge-rec))))))

(defun collect-unary-rule-names (edge-rec)
  (when (cdr (edge-children edge-rec))
    (error "~%Should be unary edge ~A" edge-rec))
  (if (edge-children edge-rec)
    (cons (edge-rule-number edge-rec)
              (collect-unary-rule-names (car (edge-children edge-rec))))
    (if (edge-morph-history edge-rec)
        (cons (edge-rule-number edge-rec)
              (collect-morph-history-rule-names 
               (edge-morph-history edge-rec))))))

(defun collect-morph-history-rule-names (edge-rec)
  (if (edge-morph-history edge-rec)
      (cons (edge-rule-number edge-rec)
            (collect-morph-history-rule-names 
             (edge-morph-history edge-rec)))))


;;; DISCO-style rule filter

(defparameter *rule-filter* nil)

(defun init-filter nil
  (let ((names (make-hash-table :test #'eq))
	(max-arity 0)
	(rule-no 0)
	(rule-list NIL))
    (flet ((process-rule (name rule)
	     (setq max-arity (max max-arity (1- (length (rule-order rule)))))
	     (push (list rule-no (rule-full-fs rule) (cdr (rule-order rule)))
		   rule-list)
	     (setf (gethash name names) rule-no)
	     (incf rule-no)))
      (maphash #'process-rule *rules*)
      (maphash #'process-rule *lexical-rules*))
    (values
     (setf *rule-filter*
       (cons names
	     (make-array (list rule-no rule-no max-arity)
			 :initial-element nil)))
     rule-list)))

(defun build-filter nil
  (multiple-value-bind (filter rule-list)
      (init-filter)
    (loop for (rule-index rule-tdfs rule-dtrs) in rule-list
	do
	  (loop for (test-index test-tdfs test-dtrs) in rule-list
	      do
		(loop for arg from 0 to (1- (length rule-dtrs))
		    do
		      (with-unification-context (ignore)
			(if (yadu rule-tdfs
				  (create-temp-parsing-tdfs
				   (if (eq test-tdfs rule-tdfs)
				       (copy-tdfs-completely test-tdfs)
				     test-tdfs)
				   (nth arg rule-dtrs)))
			    (setf (aref (cdr filter)
					rule-index test-index arg) t)
			  nil)))))
    t))

(defun check-filter (rule test arg)
  (if (and *rule-filter*
	   (symbolp test))
      (aref (cdr *rule-filter*)
	    (gethash rule (car *rule-filter*))
	    (gethash test (car *rule-filter*))
	    arg)
    t))
