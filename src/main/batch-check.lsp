;;; Functions for checking instances are well formed

(in-package :cl-user)

(defun batch-check-lexicon (&optional path-name)
  (let ((error-file (or path-name 
                        (ask-user-for-new-pathname "File for errors?"))))
    (when error-file
      (format t "~%Checking lexicon")
      (with-open-file (ostream error-file :direction :output
		       :if-exists :new-version)  
        (setf *batch-mode* t)
        (write-time-readably ostream)
        (dolist (id (collect-psort-ids *lexicon*))
	  ;; alternatively - for lexicon only
	  ;; (reverse *ordered-lex-list*) 
	  (format ostream "~%Checking ~A" id)
	  (let* ((entry (read-psort *lexicon* id))
		 (lex-id (lex-or-psort-id entry)))
	    (expand-psort-entry entry)
	    (let ((new-fs (lex-or-psort-full-fs entry)))
	      (unless new-fs
		(format ostream "~%No feature structure for ~A" lex-id))
	      ;; uncomment these lines with the LinGO ERG version of
	      ;; user-fns in order to check coindexation for
	      ;; inflection position
;;;	      (when new-fs
;;;		(unless (extract-infl-pos-from-fs (tdfs-indef new-fs))
;;;		  (format t "~%No position identified for ~A" id)))
;;;                     (when new-fs
;;;                       (sanitize (existing-dag-at-end-of 
;;;                                  (tdfs-indef new-fs) 
;;;                                  mrs::*initial-semantics-path*)
;;;                                 lex-id ostream))))
	      ))
	  (unexpand-psort *lexicon* id))))
    (format t "~%Lexicon checked")
    (setf *batch-mode* nil)))

#|
(sanitize (existing-dag-at-end-of (tdfs-indef (lex-or-psort-full-fs
                                   (get-psort-entry 'SCHEYTT_N1)))
'(synsem local cont))
 'SCHEYTT_N1)
|#

(defun sanitize (dag-instance id &optional (ostream t))
  ;;; walks over a fs, looking for things of type
  ;;; *diff-list-type* and checks that the
  ;;; LAST is indeed equal to the end of the LIST
  ;;; Outputs a warning message if this fails
  (when (dag-p dag-instance)
    (invalidate-visit-marks)  
    (sanitize-aux dag-instance id nil ostream)))


(defun sanitize-aux (dag-instance id path-so-far ostream)
   (let* ((real-dag (follow-pointers dag-instance))
         (flag-value (dag-visit real-dag)))
     (unless (eql flag-value :sanitized)
       (setf (dag-visit real-dag) :sanitized)
       (unless (is-atomic real-dag)
         ; don't care about atomic things - they can't be relevant
         (when (or (eq (type-of-fs real-dag) *diff-list-type*)
                   (subtype-p (type-of-fs real-dag) *diff-list-type*))
           (check-diff-list dag-instance id path-so-far ostream))
         (for label in (top-level-features-of real-dag)
              do
              (sanitize-aux (get-dag-value real-dag label)
                            id (cons label path-so-far) ostream))))))

#|
(check-diff-list (mrs::path-value (tdfs-indef (lex-or-psort-full-fs (get-psort-entry 'dial_v1))) 
                                  '(SYNSEM NON-LOCAL SLASH)) 
                 'dial_v1 '(SYNSEM NON-LOCAL SLASH))
|#

(defun check-diff-list (fs id path &optional (ostream t))
  ;;; all-diff-lists should have 
  ;;; *diff-list-list* and *diff-list-last*
  (let ((labels (top-level-features-of fs)))
    (if (and (member *diff-list-list* labels)
             (member *diff-list-last* labels))
        (let* ((last-fs (get-dag-value fs *diff-list-last*))
               (list-fs (get-dag-value fs *diff-list-list*))
               (last-of-list (find-end-of-list list-fs id path ostream)))
          (unless (eq last-of-list last-fs)
            (format ostream "~%~A has unterminated diff-list at ~A" id (reverse path))))
      (format ostream "~%~A has missing list/last on diff-list at ~A" id (reverse path)))))


(defun find-end-of-list (input-fs id path &optional (ostream t))
  (let* ((fs (follow-pointers input-fs)))
    (if (is-atomic fs)
        (if (eq (type-of-fs fs) *empty-list-type*)
            fs
          (progn
            (format ostream "~%~A :list not terminated with ~A at ~A" 
                    id *empty-list-type* (reverse path))
            nil))
      (let ((head-part (get-dag-value fs (car *list-head*)))
            (rest-part (get-dag-value fs (car *list-tail*))))             
        (if head-part
            (if rest-part
                (find-end-of-list rest-part id path ostream)
              (progn
                (format ostream "~%~A has an incorrect structure in its ~A list" 
                        id (reverse path))
                nil))
          (if rest-part
              (progn
                (format ostream "~%~A has a gap in its ~A list" id (reverse path))
                nil)
            fs))))))

;;;; Morphology


(defun batch-check-morphology nil
  ;;; generates all morphological forms
  (for psort in *ordered-lex-list*
       do
       (gen-all-morphs psort (get-psort-entry psort))))   

(defun gen-all-morphs (id entry)
  (when entry
    (setf *number-of-applications* 0)
    (try-all-morph-rules (list (lex-or-psort-full-fs entry)))
    (unexpand-psort *lexicon* id)))
    

(defun try-all-morph-rules (entries)
   (incf *number-of-applications*)
   (when (> *number-of-applications* *maximal-lex-rule-applications*)
      (error "~%Probable circular lexical rule"))
   (let ((transformed-entries 
            (for entry in entries
               append
               (for rule in 
                  (get-indexed-lrules entry)
                  filter
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
                    (when (and result new-morph) (format t "~%~A" new-morph))
                    result)))))
      (if transformed-entries
          (try-all-morph-rules transformed-entries))))




    