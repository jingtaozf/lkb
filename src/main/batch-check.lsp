;;; Copyright (c) 1991-2003 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Ben Waldron
;;; see licence.txt for conditions

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
   
(defun batch-check-lexicon (&optional (unexpandp t) &key (check-duplicates t))
  #-:psql
  (declare (ignore check-duplicates))
  (let ((*batch-mode* t)
	(start-path (get-diff-list-start-path)))
    #+:psql
    (when (typep *lexicon* 'psql-lex-database)
      (format t "~%(caching all lexical records)")
      (cache-all-lex-records *lexicon*))
    (format t "~%Checking lexicon")
    (format t "~%  - difference-list check starts at path ~a" start-path)
    (dolist (id (collect-psort-ids *lexicon*))
      ;; alternatively - for lexicon only
      ;; (collect-psort-ids *lexicon*) 
      (let* ((entry (read-psort *lexicon* id :cache (not unexpandp)))
             (lex-id (lex-entry-id entry)))
        (expand-psort-entry entry)
        (let ((new-fs (lex-entry-full-fs entry)))
          (unless new-fs
            (format lkb::*lkb-background-stream*
                    "~%No feature structure for ~A~%" lex-id))
          (when (and new-fs
                     *grammar-specific-batch-check-fn*)
            (funcall *grammar-specific-batch-check-fn* new-fs id))   
	  (when new-fs
	    (sanitize (existing-dag-at-end-of (tdfs-indef new-fs) start-path)
		      lex-id
		      (reverse start-path)))
	  ))))
  #+:psql
  (when check-duplicates
    (format t "~%CHECKING FOR DUPLICATE ENTRIES:~%")
    (display-tdl-duplicates *lexicon*)
    (format t "~%END OF DUPLICATE ENTRIES~%"))
  (format t "~%(emptying cache)")
  (empty-cache *lexicon*)
  (format t "~%Lexicon checked"))

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
         (when (eq-or-subtype real-dag *diff-list-type*)
           (check-dag-diff-list dag-instance id path-so-far ostream))
         (loop for label in (top-level-features-of real-dag)
              do
	       (sanitize-aux (get-dag-value real-dag label) 
			     id 
			     (cons label path-so-far) 
			     ostream))))))

(defun eq-or-subtype (dag type)
  (or (eq (type-of-fs dag) type)
      (subtype-p (type-of-fs dag) type)))
  
(defun check-dag-diff-list (dag id path &optional (ostream t))
  (let* ((list-dag (dag-path-val (list *diff-list-list*) dag))
	 (last-dag (dag-path-val (list *diff-list-last*) dag)))
    (when
	(and
	 (null (top-level-features-of list-dag))
	 (null (top-level-features-of last-dag))
	 (eq-or-subtype list-dag *list-type*)
	 (eq-or-subtype last-dag *list-type*))
      (format *batch-check-diff-list-strict* "~%WARNING: malformed but 'acceptable' difference list at ~a in ~a" (reverse path) id)
      (return-from check-dag-diff-list))
    (loop
	with rest-dag
	while (not (eq list-dag
		       last-dag))
	do
	  (setf rest-dag (dag-path-val '(rest) list-dag))
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
    (unexpand-psort *lexicon* id)))
    
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

;;;
;;; dag access
;;;

(defun dag-path-val (path dag)
  (existing-dag-at-end-of dag path))

;(defun dag-path-val (path dag)
;  (cond
;   ((null dag)
;    nil)
;   ((null path)
;    dag)
;   (t
;    (dag-path-val
;     (cdr path)
;     (cdr (assoc (car path) (dag-arcs dag)))))))

(defun dag-path-type (path dag)
  (let ((val (dag-path-val path dag)))
    (if (typep val 'dag)
        (dag-type val)
      nil)))

(defun dag-diff-list-2-list (dag)
  (let* ((last-dag (dag-path-val (list *diff-list-last*) dag))
	 (list-dag (dag-path-val (list *diff-list-list*) dag)))
    (loop
	with rest-dag
	while (not (eq list-dag
		       last-dag))
	do
	  (setf rest-dag (dag-path-val '(rest) list-dag))
	  (when (null rest-dag)
	    (format t "~%WARNING: invalid difference list ~a in ~a" out-list *current-lex-id*)
	    (loop-finish))
	collect (dag-path-val '(first) list-dag)
	into out-list
	do
	  (setf list-dag rest-dag)
	finally
	  (return out-list)
	  )))

(defun dag-list-2-list (dag)
  (let* ((list-dag dag))
    (loop
	with rest-dag
	while (not (equal (dag-type list-dag)
			  *empty-list-type*))
	do
	  (setf rest-dag (dag-path-val *list-tail* list-dag))
	  (when (null rest-dag)
	    (format t "~%WARNING: invalid list ~a in ~a" out-list dag)
	    (loop-finish))
	collect (dag-path-val *list-head* list-dag)
	into out-list
	do
	  (setf list-dag rest-dag)
	finally
	  (return out-list)
	  )))