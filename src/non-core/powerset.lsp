;;; ERB 2004-08-25
;;; Utilities for creating all possible disjuntive types given
;;; a root and a set of leaf nodes.  Useful for subtypes of 
;;; head in the Matrix.

(in-package :lkb)

;;; Define construct

(defstruct type-spec
  id members card parents)

;;; Set subtraction

(defun set-subtraction (set1 set2)
  (cond ((null set1) nil)
	((member (first set1) set2)
	 (set-subtraction (rest set1) set2))
	(t (cons (first set1) (set-subtraction (rest set1) set2)))))

;;; Define powerset

(defun powerset (elements)
  (if (null elements)
      (list nil)
    (let ((p (powerset (cdr elements))))
      (append p
	      (loop for set in p
		    collect (cons (car elements) set))))))

(defun no-nil-powerset (elements)
  (remove nil (powerset elements)))


;;; Make power set
;;; Define type-specs for each element of power set, 
;;; except the one with all the elements.
;;; :id, :members, :card(inality)
;;; Collect them in a list.
;;; Go through list and add :parents for
;;; each type-spec:
;;;  if cardinality = N-1, sole :parents is root
;;;  otherwise, for each type-spec in the list
;;;  if :card(inality) is one more than current type-spec
;;;  and current spec's :members is a subset of :members,
;;;  add to :parents
;;; print to stdout the tdl definition of each type.

(defun power-set-types (root leaves &optional (glosses nil))

  (unless (stringp root)
    (error "Usage is (power-set-types root leaves &optional glosses).~%Value of root must be a string.~%"))

  (unless (string-list-p leaves)

    (error "Usage is (power-set-types root leaves &optional glosses).~%Value of leaves must be a list of strings.~%"))
    
  (unless (and glosses
	       (check-glosses leaves glosses))
    (error "Usage is (power-set-types root leaves &optional glosses).~%Glosses must be an alist associating each element of leaves with a string (the gloss)."))



;;; Takes a root type in the hierarchy, a set of leaf types,
;;; and creates a powerset of disjunctive types representing
;;; all possible disjunctions of the leaf types, rooted in root.
;;; (power-set-types "root" '("leaf1" "leaf2" "leaf3"))

  (let* ((powerset (no-nil-powerset leaves))
	 (n (length leaves))
	 (specs (loop for element in powerset
		      unless (eq (length element) n)
		      collect (make-type-spec :members element
					      :card (length element)))))

;    (format t "~a" specs)
    ;;; Set id for each type specification
    (loop for spec in specs 
	  do (let ((id (make-plus-string (type-spec-members spec))))
	       (setf (type-spec-id spec) id)))

    ;;; Set parents for each type specification.  Those that have
    ;;; just one fewer elements than leaves get the root as their 
    ;;; sole parent.  Others get all parents which are supersets with
    ;;; exactly one more parent.
    (loop for spec in specs
	  if (eq (type-spec-card spec) (- n 1))
	  do (setf (type-spec-parents spec) (list root))
	  else do (let ((parents (loop for parent in specs
				       if (and 
					   (eq (type-spec-card parent) 
					       (+ (type-spec-card spec) 1))
					   (string-subset-p (type-spec-members spec) 
							    (type-spec-members parent)))
				       collect (type-spec-id parent))))
		    (setf (type-spec-parents spec) parents)))
  
    ;;; Print in tdl formalism (to stdout, for now), starting
    ;;; with the largest sets.
    (loop for i from (- n 1) downto 1
	  do (loop for spec in specs
		   if (eq (type-spec-card spec) i)
		   do (print-tdl spec glosses)))))

;;; Print function to output tdl given a type-spec.

(defun print-tdl (spec glosses)
  (format t "~a := " (type-spec-id spec))
  (loop for parent on (type-spec-parents spec)
	if (> (length parent) 1)
	do (format t "~a & " (car parent))
	else do (format t "~a" (car parent)))
  (if glosses

      (let ((members (type-spec-members spec)))
	
	(if (> (length members) 1)
	    
	    (progn 
	      (format t " & ~%\"Disjuntive type for ")
	      (loop for elements on (type-spec-members spec)
		    do (let ((gloss (cdr (assoc (car elements) glosses :test #'equal))))
			 (cond ((> (length elements) 2)
				(format t "~a, " gloss))
			       ((= (length elements) 2)
				(format t "~a " gloss))
			       (t (format t "and ~a.\".~%" gloss))))))
	  (format t ".~%")))

    (format t ".~%")))


;;; id function to create type id string, given members.
;;; If there's just one member, then id is the same as the
;;; member.  If there's more than one, concatenate all, with
;;; a leading + to indicate disjunction.

(defun make-plus-string (strings)
  (if (eq (length strings) 1)
      (car strings)
    (concatenate 'string "+" (make-plus-string-aux strings))))

(defun make-plus-string-aux (strings)
  (if strings 
      (concatenate 'string (car strings) (make-plus-string-aux (cdr strings)))
    ""))

;;; built in subsetp doesn't seem to work for lists of strings.
;;; utility functions

(defun string-member-p (member set)
  (cond ((null set) nil)
	((equal member (car set)) t)
	(t (string-member-p member (cdr set)))))

(defun string-subset-p (set1 set2)
  (cond ((null set1) t)
	((string-member-p (car set1) set2) 
	 (string-subset-p (cdr set1) set2))
	(t nil)))


(defun string-list-p (list)

  (and (listp list)
       (or (null list)
	   (and (stringp (car list))
		(string-list-p (cdr list))))))

;;; Check that the glosses a-list is properly formed and
;;; corresponds to the leaves.

(defun check-glosses (leaves glosses)
  
  (let ((gloss-keys (mapcar #'car glosses)))
    (and (string-subset-p leaves gloss-keys)
	 (string-subset-p gloss-keys leaves))))


;;; For development purposes

(defun test-pst ()
  (power-set-types "root" '("a" "b" "c")))

(defun test-pst-2 ()
  (power-set-types "root" '("a" "b" "c") '(("a" . "a-type") ("b" . "b-type") ("c" . "c-type"))))

(defun test-pst-3 ()
  (power-set-types "root" 
		   '("a" "b" "c" "d") 
		   '(("a" . "a-type") ("b" . "b-type") ("c" . "c-type") ("d" . "d-type"))))


