;;; Still to do: 
;;; Generalize switch to allow classification of mal-rules,
;;; perhaps by giving ROBUST a wider range of (typed) values.

(in-package :lkb)

;;; eventually move to globals

(defparameter *mal-active-p* nil)
(defparameter *gen-mal-active-p* nil)

(defparameter *mal-marking-path* '(ROBUST))

(defparameter *found-configs* nil)

;;; Note to EB: the cascaded let* statement serves as error checking.
;;; (ERB 2003-10-16) mal-rule-p and find-robust-rules and perhaps others
;;; need to be generalized to cover mal lexical entries.

(defun mal-rule-p (rule)

  (let* ((tdfs (when (rule-p rule)
		 (rule-full-fs rule)))
	 (dag (when (tdfs-p tdfs)
		(tdfs-indef tdfs)))
	 (robustp (when (dag-p dag)
		    (existing-dag-at-end-of dag *mal-marking-path*))))
  
    (when (dag-p robustp)
      (bool-value-true robustp))))

; For switch in parser and generator, this function
; will be passed a lexical entry.  In diagnosis, we
; (apparently) no longer have access to the lexical entries
; so it will be passed a tdfs instead.

(defun mal-lex-entry-p (lex)

  (let* ((tdfs (cond ((lex-entry-p lex)
		      (lex-entry-full-fs lex))
		     ((tdfs-p lex) lex)
		     ((mrs::base-lex-p lex) (lex-entry-full-fs (get-lex-entry-from-id (mrs::base-lex-lex-id lex))))))
	 (dag (when (tdfs-p tdfs)
		(tdfs-indef tdfs)))
	 (robustp (when (dag-p dag)
		    (existing-dag-at-end-of dag *mal-marking-path*))))
  
    (when (dag-p robustp)
      (bool-value-true robustp))))



;;; (ERB 2003-10-07) Functions for setting rule and lex priorities
;;; for the generator for aligned generation.  Give high scores to
;;; tasks that correspond to edges in the parse returned by the
;;; parser.  Relying on global variable *parse-record* to store results of
;;; parser, but I'd rather not.

;;; (ERB 2003-10-08) Rather than transversing the tree each time we
;;; need a rule priority, make a table first for quicker look up.
;;; Store in *found-configs* and populate the table each time generate-from-mrs
;;; is called.


(defun populate-found-configs ()

  (let* ((symbol (when (find-package :tsdb)
                   (find-symbol "%GRAFT-ALIGNED-GENERATION-HACK%" :tsdb)))
         (ag-parse-result (if (and symbol (symbol-value symbol))
                           (symbol-value symbol)
			   (first *parse-record*))))

    (when ag-parse-result
      (setf *found-configs* (make-hash-table :test #'equal))
      (setf (gethash 'lex-ids *found-configs*) (edge-lex-ids ag-parse-result))
      (put-subtrees-in-found-configs ag-parse-result)
      (put-lex-yields-in-found-configs ag-parse-result))))

;;; (ERB 2003-10-20) This also puts in configurations for active edges 
;;; (with at least one instantiated daughter).

(defun put-subtrees-in-found-configs (edge)
  
  (if (rule-p (edge-rule edge))
      (let* ((rule (edge-rule edge))
	     (id (rule-id rule))
	     (dtr-ids (loop for dtr in (edge-children edge)
			  collect (if (rule-p (edge-rule dtr))
				      (rule-id (edge-rule dtr))
				    (first (edge-lex-ids dtr))))))
		      
	; passive edges
	(setf (gethash (cons id dtr-ids) *found-configs*) t)
	
	;active edges
	(let* ((length (length dtr-ids))
	       (active-dtrs (loop for i from 1 to length
				collect nil))
	       (active (cons id active-dtrs))
	       (apply-order (rule-daughters-apply-order rule))
	       (order (rule-order rule)))
	  (loop for dtr in apply-order
			   
	      ;configs for active edges should have at least one missing
	      ;dtr (i.e., a NIL on the list active)	   
			   
	      with n = (length (loop for dtr in active-dtrs
				   if (null dtr)
				   collect dtr))
	      while (> n 1)
		    
	      ;Take the dtrs in the order they would be applied (looping through
	      ;apply-order) and get the rule id from the parse results,
              ;and stick it in the relevant position in this mocked-up active
	      ;edge configuration.  Then put the config into *found-configs*
		    
	      do (let ((pos (position dtr order)))
		   (setf active (gen-copy-list-and-insert active (nth (1- pos) dtr-ids) pos))
		   (setf (gethash active *found-configs*) t)
		   (setf n (- n 1)))))))
		       
  ; recurse over daughters	
  (if (edge-children edge)
      (loop for dtr in (edge-children edge)
	  do (put-subtrees-in-found-configs dtr))))

(defun put-lex-yields-in-found-configs (edge)

  (when (rule-p (edge-rule edge))
  
    (let* ((id (rule-id (edge-rule edge)))
	   (yield (edge-lex-ids edge))
	   (existing-yields (gethash id *found-configs*)))
      (if existing-yields
	  (setf (gethash id *found-configs*) (cons yield existing-yields))
	  (setf (gethash id *found-configs*) (list yield))))
	     
    (when (edge-children edge)
      (loop for dtr in (edge-children edge)
	  do (put-lex-yields-in-found-configs dtr)))))
      
(defun lex-found-in-parse-p (lex-id)
  
  (let ((parse-lex-ids (gethash 'lex-ids *found-configs*)))
    (if (member lex-id parse-lex-ids) t)))

;;; With active edges stored in found-configs, we can now get
;;; away with one function for looking up potential rule+passive
;;; and active+passive tasks.

(defun config-found-in-parse-p (dtr mother pointer)
  
  (if (null dtr)

; If we're considering an inactive edge, just extract the configuration,
; and look it up in *found-configs*
; Not currently using this case.
      
      (let* ((id (rule-id (g-edge-rule mother)))
	     (dtr-ids (loop for dtr in (g-edge-children mother)
			  collect (get-dtr-id-from-generator-edge dtr)))
	     (config (cons id dtr-ids)))
	(gethash config *found-configs*))
	

; If we're considering an active edge,
; construct mock-up of edge that would be produced.  
; [This code may be tyring to handle cases that don't in
; fact come up -- things that are handled at the inactive  
; edge stage instead.]  [Or not].
  
  
    (let* ((edge (if (g-edge-p mother)
		     mother
		   nil))
	   (rule (if (rule-p mother)
		     mother
		   (g-edge-rule mother)))
	   (dtr-id (get-dtr-id-from-generator-edge dtr))
	   (order (rule-order rule))
	   (active-base (cons (rule-id rule)
			      (loop for i from 2 to (length order)
				  collect nil)))
	   (edge-with-new-dtr (gen-copy-list-and-insert active-base dtr-id (+ pointer 1)))
	   (new-edge (if (null edge)
			 edge-with-new-dtr
		       (loop for child in (g-edge-children edge)
			   if child
			   do (let ((child-id (get-dtr-id-from-generator-edge child)))
				(setf edge-with-new-dtr 
				  (gen-copy-list-and-insert edge-with-new-dtr 
							    child-id
							    (1+ (position child (g-edge-children edge))))))
			   finally (return edge-with-new-dtr)))))

;      (format t "Looking for config: ~a~%" new-edge)
      (gethash new-edge *found-configs*))))

;;; "Lexical" generator edges are the result of applying lexical
;;; rules to lexical items.  For purposes of assigning priority to
;;; further tasks involving those edges, we need to know what the
;;; rule-id of the last (highest) lexical rule was.  
;;; get-dtr-id-from-generator-edge gets the rule-id of the grammar
;;; rule or lexical rule used, or if neither applies, returns
;;; the lex-id of the edge.
;;;
;;; For grammars with a richer collection of lexical rules, it
;;; might be desirable to prioritize the application of lexical
;;; rules as well, but this will require greater changes to the 
;;; current generation scheme.
;;;
;;; I'm assuming in writing this that there aren't any cases
;;; where g-edge-lexemes returns more than one lexeme, despite
;;; the name.  Worry about such cases later.

(defun get-dtr-id-from-generator-edge (edge)
  
  (if (rule-p (g-edge-rule edge)) 
      (rule-id (g-edge-rule edge))
    (let* ((lexeme (first (g-edge-lexemes edge)))
	   (lex-rules (mrs::found-lex-rule-list lexeme))
	   (lex-rule (first lex-rules)))
      (if lex-rule
	  lex-rule
	(first (g-edge-lex-ids edge))))))
 

;;; Second strategy: Did we use that rule with 
;;; the same lexical yield?

;;; Utlity function -- surely we have this somewhere already?

(defun subbagp (bag1 bag2)
  (or (null bag1)
      (if (member (first bag1) bag2)
	  (subbagp (rest bag1) (remove (first bag1) bag2 :count 1)))))


(defun rule-found-with-yield-p (dtr mother)
  
  (let* ((id (cond ((rule-p mother) (rule-id mother))
			((g-edge-p mother) (rule-id (g-edge-rule mother)))
			(t (error "Non-rule non-edge passed to yield-found-in-parse-p~%"))))
	 (mother-yield (when (g-edge-p mother)
			 (loop for child in (g-edge-lex-ids mother)
			     if child
			     append child)))
	 (yield (if (rule-p mother)
		    (g-edge-lex-ids dtr)
		  (append mother-yield
			  (g-edge-lex-ids dtr)))))
;    (format t "Rule: ~a Yield: ~a~%"  id yield)
    (loop for lex-id-list in (gethash id *found-configs*)
	if (subbagp yield lex-id-list)
	return t)))
  
;;; Third strategy: did we use that rule at all?
;;; *found-configs* will contain entries for the
;;; yield of rules keyed of the rule id.

(defun rule-found-in-parse-p (rule)
  
  (let ((id (cond ((edge-p rule)
		   (rule-id (edge-rule rule)))
		  ((rule-p rule)
		   (rule-id rule))
		  (t (error "Non-edge, non-rule passed to rule-found-in-parse-p")))))
    
    (gethash id *found-configs*)))


(defun ag-gen-lex-priority (lex)
  
  (if (lex-found-in-parse-p lex)
      100
    1))


(defvar *ag-debug* nil)
(defvar *ag-rule-debug* nil)
(defvar *config-strategy* t)
(defvar *yield-strategy* t)
(defvar *rule-strategy* t)

(defun ag-gen-rule-priority (dtr mother pointer)

  (let* ((result (cond ((and *config-strategy* (config-found-in-parse-p dtr mother pointer)) 100)
                       ((and *yield-strategy* (rule-found-with-yield-p dtr mother) 80))
                       ((and *rule-strategy* (rule-found-in-parse-p mother) 60))
                       (t 1))))
    
    (when *ag-debug*
      (cond ((null dtr)
	     (format t "ag-gen-rule-priority for inactive edge ~a with
                     children ~a:  ~a~%" mother (g-edge-children mother) result))
	    ((g-edge-p mother)
	     (format t "ag-gen-rule-priority for active edge ~a with
                     instantiated children ~a and dtr ~a:   ~a~%"
		     mother
		     (g-edge-children mother)
		     dtr
		     result))
	    ((and (rule-p mother)  *ag-rule-debug*)
	     (format t "ag-gen-rule-priority for rule ~a and dtr ~a:   ~a~%"
		     (rule-id mother)
		     dtr
		     result))))
    result))
  

(defparameter *graft-debug-p* nil)

(defun ag-score-task (task)
  (let ((score
         (case (first task)
           (:lexicon
            (ag-gen-lex-priority (first (edge-lex-ids (second task)))))
           (:rule
            (let* ((rule (second task))
                   (passive (third task))
                   (index (position (first (rule-daughters-apply-order rule))
                                    (rest (rule-order rule))
                                    :test #'eq)))
              (ag-gen-rule-priority passive rule index)))
           (:active
            (let* ((active (second task))
                   (passive (third task))
                   (index (position 
                           (first (g-edge-needed active))
                           (rest (rule-order (g-edge-res active)))
                           :test #'eq)))
              (ag-gen-rule-priority passive active index))))))
    (when *graft-debug-p*
      (format
       t
       "~(~a~): ~a~@[ + ~a~] == ~a~%"
       (first task)
       (let ((second (second task)))
         (if (rule-p second) (rule-id second) second))
       (third task) score))
    score))
   

;; (ERB 2003-10-16) Wrapper function for grammar checking.  Takes
;; and input string, and either declares it grammatical, fails to 
;; parse it, or returns a diagnosis of the error and a corrected version.

(declaim (special *gen-scoring-hook* *bypass-equality-check* *gen-packing-p*))

(defun grammar-check (string)


  (let* ((new-string nil)
         (*first-only-p* t)
         (*gen-packing-p* nil)
         (*bypass-equality-check* nil)
         (*gen-first-only-p* t)
         (*mal-active-p* t)
         (*gen-mal-active-p* nil)
         (*gen-scoring-hook* #'ag-score-task))
    
     ;; Parse the string.
    (parse (split-into-words 
	    (preprocess-sentence-string 
	     (string-trim '(#\space #\tab #\newline) string))))
    
    (if *parse-record*
	(let ((parse (first *parse-record*)))
	  ;; Look for errors
	  (if (robust-parse-p parse)
	      (let ((errors (find-robust-rules parse)))
		(cond ((null errors)
		       (error "robust-p returned true, but I found no errors"))
		      
		      ;; One error -- tell the user about it.
		      ((eq 1 (length errors))
		       (let ((error (lookup-error-description (first errors)))
			     (string (first (generate-from-mrs (mrs::extract-mrs parse)))))
			 (format t "The sentence you typed appears to have an error in it:~%")
			 (format t "~a~%" error)
			 (format t "Perhaps you meant: ")
			 (setf new-string string)))
		      
		      ;; More than one error -- tell the user about it
		      ;; Assume for now that any given error will only require one mal-rule
		      ;; or one mal-entry in the tree.
		      (t (let ((string (first (generate-from-mrs (mrs::extract-mrs parse)))))
			   (format t "The sentence you typed appears to have errors in it:~%")
			   (loop for error in errors
			       do (let ((error-msg (lookup-error-description error)))
				    (format t "~a~%" error-msg)))
			   (format t "Perhaps you meant: ~%")
			   (setf new-string string)))))
	    (setf new-string "The sentence you typed was recognized as grammatical by the grammar."))))
    
    (if (stringp new-string)
	(format t "~a~%" new-string)
      new-string)))
      
;; Assumes that the grammar passes the value of ROBUST (or whatever else is used
;; to mark robustness) up the tree.

(defun robust-parse-p (parse)
  
  (let* ((tdfs (when (edge-p parse)
		 (edge-dag parse)))
	 (dag (when (tdfs-p tdfs)
		(tdfs-indef tdfs)))
	 (robustp (when (dag-p dag)
		    (existing-dag-at-end-of dag *mal-marking-path*))))
  
    (when (dag-p robustp)
      (bool-value-true robustp))))

;; find-robust-rules takes a parse (i.e., (first *parse-record*)) and
;; returns a list of mal-rules that were used in the parse.
;; find-rules will return a list of rule-ids (for rules) and tdfses
;; for lexical entries.

(defun find-robust-rules (edge)

  (let ((all (find-rules edge)))
    (loop for rule in all
	if (or (mal-rule-p rule) 
	       (mal-lex-entry-p rule))
	collect (cond ((rule-p rule) (rule-id rule))
		      ((tdfs-p rule) (dag-type (tdfs-indef rule)))
		      (t (error "Non-rule non-tdfs passed to find-robust-rules~%"))))))
	     
		      
;; Maybe this one already exists somewhere?
;; Find rules for all edges licensed by grammar rules
;; and tdfses for all edges licensed by lexical entries.
;; Error diagnosis for mal-lex-entries will have to be
;; keyed of of types, and not lex ids, but that's probably
;; a good thing, since we expect to have classes of 
;; verbs, saying, with similarly deviant subcat frames.

(defun find-rules (edge)
  
  (if (rule-p (edge-rule edge))
      (append (list (edge-rule edge))
	      (loop for dtr in (edge-children edge)
		  append (find-rules dtr)))
    (list (edge-dag edge))))


;; Table storing error descriptions, keyed on rule ids
;; of mal-rules.  Probably need different strategy for
;; (eventual) mal-lex-entries.
	 
(defparameter *error-descriptions* nil)
(setf *error-descriptions* (make-hash-table :test #'eq))
(setf (gethash 'mal_third_sg_fin_verb_infl_rule *error-descriptions*) "There is a verb which does not agree with its subject.")
(setf (gethash 'mal_non_third_sg_fin_verb_infl_rule *error-descriptions*) "There is a verb which does not agree with its subject.")
(setf (gethash 'mal_plur_noun_infl_rule *error-descriptions*) "There is a lack of number agreement with the singular noun.")
(setf (gethash 'bare_np_sg *error-descriptions*) "There is a noun which is missing a determiner.")
(setf (gethash 'mal_advadd *error-descriptions*) "There is a misplaced adverb.")
(setf (gethash 'mal_sailr *error-descriptions*) "Subject-verb inversion is only possible with auxiliary verbs.")
(setf (gethash 'mal_v_inf_cp_comp_le *error-descriptions*) 
  "There is a verb taking an infinitival complement that it is not subcategorized for.")
(setf (gethash 'mal_v_subj_equi_le *error-descriptions*)
  "There is a verb taking a base form VP complement that wants an infinitival complement.")
(setf (gethash 'mal_va_does_le *error-descriptions*) "There is a verb which does not agree with its subject.")
(setf (gethash 'mal_va_doesnt_neg_pres_le *error-descriptions*) "There is a verb which does not agree with its subject.")
(setf (gethash 'mal_va_dont_neg_pres_le *error-descriptions*) "There is a verb which does not agree with its subject.")
(setf (gethash 'mal_va_do_fin_le *error-descriptions*) "There is a verb which does not agree with its subject.")
(setf (gethash 'mal_det_part_div_le *error-descriptions*) "There is a noun which should not combine with the determiner 'a/an'.")

;; lookup-error-description takes a rule-id and looks for
;; an error description tied to that rule in *error-descriptions*

;; Things on the list passed to lookup-error-description are either
;; rule ids or lexical types for lexical edges.

(defun lookup-error-description (error)
  (gethash error *error-descriptions*))
  
;  (let ((error (cond ((symbolp error) error)
;		     ((tdfs-p error) (dag-type (tdfs-indef error)))
;		     (t (error "Non-symbol non-tdfs passed to lookup-error-description~%")))))
;  
;    (gethash error *error-descriptions*)))
		
