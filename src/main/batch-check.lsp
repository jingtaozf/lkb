;;; Copyright (c) 1991-2003 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Ben Waldron
;;; see LICENSE for conditions

;;; Functions for checking instances are well formed

(in-package :lkb)

(defvar *current-lex-id*)
(defvar *grammar-specific-batch-check-fn* nil)
(defvar *batch-check-diff-list-strict* nil)
(defvar *batch-check-diff-list*)

(defun get-diff-list-start-path nil
  (cond
   ((boundp '*batch-check-diff-list*)
    *batch-check-diff-list*)
   ((boundp 'mrs::*initial-semantics-path*)
    mrs::*initial-semantics-path*)
   (t
    (error "please set *batch-check-diff-list*"))))
   
(defun batch-check-lexicon (&optional (unexpandp t) &key (check-duplicates t) (lexicon *lexicon*))
  (format t "~&;;; Running batch check over lexicon ~a"
	  (or (name lexicon) ""))
  (let ((*batch-mode* t))
    #+:psql
    (when (typep lexicon 'psql-lex-database)
      (format t "~&;;; (caching all lexical records)")
      (cache-all-lex-records lexicon))
    (let ((ids (collect-psort-ids lexicon)))
      (format t "~&;;; (~a entries)" (length ids))
      (loop for id in ids
	  do
	    (check-lex-entry id lexicon
			     :unexpandp unexpandp
			     :ostream *lkb-background-stream*))))
  (when check-duplicates
    (display-tdl-duplicates lexicon))
;  (format t "~%(emptying cache)")
  (empty-cache lexicon)
  (format t "~&;;; lexicon checked"))

(defvar *x-recs-dup*)
(defun display-tdl-duplicates (lex)
  (format t "~&;;; CHECKING FOR DUPLICATE ENTRIES:~%")
  (let* (;; collect triples <lex,id,unifs>
	 (x-recs
          (mapcar #'(lambda (x) 
		      (let* ((x (read-psort lex x :cache nil))
			     (id (lex-entry-id x))
			     (x-lex (lexicon-for-id lex id))
			     (x-id (string id))
			     (x-unifs (lex-entry-unifs x)))
			(forget-psort lex x)
			(list x-lex x-id x-unifs)))
                  (collect-psort-ids lex)))
	 ;; sort on ID (string<)
         (x-recs-sorted 
          (sort x-recs #'string< :key #'second))
	 ;; check for duplicate UNIFS (equalp)
         (x-recs-dup
          (duplicates  
           x-recs-sorted 
           :key #'third
           :test #'equalp)))
    (setf *x-recs-dup* x-recs-dup) ;!!!
    (when x-recs-dup
      (loop
          for dup-set in x-recs-dup
          do
            (format t "~%~%")
            (loop
                for x in dup-set
		for x-lex = (first x)
		for x-id = (second x)
		for x-unifs = (third x)
                do
                  (format t "~&[~a] ~a := ~a"
			  (name x-lex) x-id (unifs-to-tdl-body x-unifs))))
      ))
  (format t "~&;;; END OF DUPLICATE ENTRIES~%"))

(defun list-duplicates-to-remove (dups-list &key (stream t))
  ;; output name of all but first duplicate in each duplicate-set
  (loop
      for dups in dups-list
      do
	(loop
	    for dup in (cdr dups)
	    for id = (second dup)
	    do
	      (princ (string-downcase id) stream)
	      (terpri stream))))

(defun lex-and-id-str (lexicon id)
  (let* ((in-lex (lexicon-for-id lexicon id))
	 (in-lex-name (and in-lex (name in-lex))))
    (if in-lex-name
	(format nil "[~a] ~a" in-lex-name (tdl-val-str id))
      (tdl-val-str id))))

(defun check-lex-entry (id lexicon &key unexpandp ostream)
  (let* ((entry (read-psort lexicon id :cache (not unexpandp)))
	 (output-stream (or ostream t))
	 (lex-id id)
	 (start-path (get-diff-list-start-path)))
    ;;; make behaviour of check-lex-entry consistent with respect
    ;;; to diff-list start path 
    (cond 
     ((null entry)
      (format t "~%WARNING: lexical entry '~a' not found in lexicon" lex-id)
      :unknown)
     (t
      (expand-psort-entry entry)
      (let ((new-fs (lex-entry-full-fs entry)))
	(if (eq :FAIL new-fs)
	    (setf new-fs nil))
	(unless new-fs
	  (format output-stream
		  "~%No feature structure for ~A~%" lex-id))
	(when (and new-fs
		   *grammar-specific-batch-check-fn*)
	  (funcall *grammar-specific-batch-check-fn* new-fs id))   
	(when new-fs
	  (sanitize (existing-dag-at-end-of (tdfs-indef new-fs) 
					    start-path)
		    lex-id
		    (reverse start-path)
		    output-stream)
          (when (and *lexicon-tokens-path*
                     (null (existing-dag-at-end-of
                            (tdfs-indef new-fs) *lexicon-tokens-path*)))
            (format
             output-stream "~%No *lexicon-tokens-path* (~{~a~^.~}) in ~a.~%"
             *lexicon-tokens-path* lex-id))
          (when (and *lexicon-last-token-path*
                     (null (existing-dag-at-end-of
                            (tdfs-indef new-fs) *lexicon-last-token-path*)))
            (format
             output-stream "~%No *lexicon-last-token-path* (~{~a~^.~}) in ~a.~%"
             *lexicon-last-token-path* lex-id)))
	new-fs)))))

(defun sanitize (dag-instance id path &optional (ostream t))
  ;;; walks over a fs, looking for things of type
  ;;; *diff-list-type* and checks that it is well-formed
  ;;; Outputs a warning message if this fails
  (when (dag-p dag-instance)
    (invalidate-visit-marks)  
    (sanitize-aux dag-instance id path ostream)))

(defun sanitize-aux (dag-instance id path-so-far ostream)
   (let* ((real-dag (follow-pointers dag-instance))
         (flag-value (dag-visit real-dag)))
     (unless (eql flag-value :sanitized)
       (setf (dag-visit real-dag) :sanitized)
       (unless (not (has-features real-dag))
         ; don't care about things without features
         (when 
	     (subtype-or-equal (type-of-fs real-dag) *diff-list-type*)
           (check-dag-diff-list dag-instance id path-so-far ostream))
         (loop for label in (top-level-features-of real-dag)
              do
	       (sanitize-aux (get-dag-value real-dag label) 
			     id 
			     (cons label path-so-far) 
			     ostream))))))

(defun check-dag-diff-list (dag id path &optional (ostream t))
  (let* ((list-dag (existing-dag-at-end-of dag (list *diff-list-list*)))
	 (last-dag (existing-dag-at-end-of dag (list *diff-list-last*))))
    (when
	(and
	 (null (top-level-features-of list-dag))
	 (null (top-level-features-of last-dag))
	 (subtype-or-equal (type-of-fs list-dag) *list-type*)
	 (subtype-or-equal (type-of-fs last-dag) *list-type*)
	 (not (eq list-dag last-dag)))
      (format ostream "~%WARNING: malformed empty difference list at ~a in ~a" (reverse path) id)
      (return-from check-dag-diff-list))
    (loop
	with rest-dag
	while (not (eq list-dag
		       last-dag))
	do
	  (setf rest-dag (existing-dag-at-end-of list-dag *list-tail*))
	  (when (null rest-dag)
	    (format ostream "~%WARNING: malformed difference list at ~a in ~a" (reverse path) id)
	    (return-from check-dag-diff-list))
	do
	  (setf list-dag rest-dag))
    t))

;;;; Morphology

(defun batch-check-morphology (&optional plus-ids)
  ;;; generates all morphological forms
  ;;; plus-ids is a boolean value: if set to t, it 
  ;;; also outputs the id of the base form and the rule 
  ;;; used to generate the new form
  (loop for lexid in (collect-psort-ids *lexicon*)
       do
       (gen-all-morphs lexid (get-lex-entry-from-id lexid) plus-ids)))   

(defun gen-all-morphs (id entry &optional plus-ids)
  (when entry
    (setf *number-of-applications* 0)
    (try-all-morph-rules (list (lex-entry-full-fs entry))
                         (if plus-ids id))
    (forget-psort *lexicon* id)))
    
(defun try-all-morph-rules (entries &optional id)
   (incf *number-of-applications*)
   (when (> *number-of-applications* *maximal-lex-rule-applications*)
      (error "~%Probable circular lexical rule"))
   (let ((transformed-entries 
            (loop for entry in entries
               append
               (loop for rule in 
                  (get-indexed-lrules entry)
                  nconc
                  (let* ((spelling-rule-p
                                        (spelling-change-rule-p rule))
                         (new-morph 
                          (if spelling-rule-p
                                    (car (mapcar #'car 
                                     (full-morph-generate
                                       (extract-orth-from-fs entry)
                                       (rule-id rule))))))
                         (result
                          (if (or new-morph (not spelling-rule-p))
                        ; allow morphographemics to block generation
                              (evaluate-unifications rule
                                                     (list entry)
                                                     new-morph))))
                    (when (and result new-morph) 
                      (format t "~%~A" new-morph)
                      (when id (format t " ~A ~A" id (rule-id rule))))
                    (if result
                        (list
                         result)))))))
      (if transformed-entries
          (try-all-morph-rules transformed-entries))))

