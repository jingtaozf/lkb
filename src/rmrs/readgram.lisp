(in-package :mrs)

(defparameter *rule-instructions* nil)

(defparameter *valid-hook-elements* '("INDEX"))

(defparameter *unknown-rules* nil
  "record unknown rules to help extend the grammar")

(defun show-unknown-rules nil
  (show-unknown *unknown-rules*))
  
(defun show-unknown (lst)  
  (let ((counted-list nil))
    (dolist (thing lst)
      (let ((existing (assoc thing counted-list :test #'equal)))
        (if existing
            (incf (cdr existing))
          (push (cons thing 1) counted-list))))
    (pprint
     (sort counted-list #'(lambda (x y) (> (cdr x) (cdr y)))))))

;;; Reading

(defun read-rmrs-grammar (file-name)
  (setf *rule-instructions* nil)
  (setf *unknown-rules* nil)
  (with-open-file (istream file-name :direction :input)
    (loop
      (let ((next-rule (read-rmrs-rule istream)))
	(when (null next-rule) (return))
	(add-rmrs-rule next-rule)))))

(defun add-rmrs-rule (rule)
  (push rule *rule-instructions*))

(defun lookup-instruction (rule-name)
  (or 
   (find rule-name *rule-instructions*
         :test #'equal :key #'rmrs-rule-name)
   (progn (push rule-name *unknown-rules*)
          nil)))
  
(defun read-rmrs-rule (istream)
  #|
<rule>
<name>S -> NP VP</name>
<dtrs> NP VP </dtrs>
<head> VP
<semstruct>
 <hook> <index> e 
 <ep> <realpred> p-agt <arg> e <arg> x2 </ep>
</semstruct>
<eq> VP.index e </eq>
<eq> NP.index x </eq>
</rule>
|#
  (let ((tag (read-next-tag istream)))
    (if (eq tag 'RULE)
	(let ((name nil) (dtrs nil) (head nil) (head-pos nil)
	      (semstruct nil) (eqs nil))
	  (loop
	    (let  
		((next-tag (read-next-tag istream)))
	      (case next-tag
		(NAME (setf name (read-rmrs-rule-name istream)))
		(DTRS (setf dtrs (read-rmrs-rule-dtrs istream)))
		;;; dtrs must appears before eqs 
		(HEAD (setf head (read-rmrs-rule-head istream)))
		(SEMSTRUCT (setf semstruct (read-rmrs-semstruct istream)))
		(EQ (push (read-rmrs-rule-eq istream dtrs) eqs))
		(t (return)))))
	  (when head (setf head-pos (position head dtrs))
		(unless head-pos
		  (error "~%Head ~S is not a member of dtrs ~S in ~A"
			 head dtrs name)))
	  (make-rmrs-rule :name name
			  :dtrs dtrs
			  :arity (length dtrs)
			  :head head-pos
			  :semstruct semstruct
			  :eqs eqs)))))

(defun read-rmrs-rule-name (istream)
;;;  <name>S -> NP VP</name>
  (let ((name 
	 (read-string-to-tag istream)))
    (check-for-end-tag 'NAME istream)
    name))

(defun read-rmrs-rule-dtrs (istream)
;;;  <dtrs> NP VP </dtrs>
  (let ((dtrs nil))
    (loop (let ((symbol (read istream)))
	    (when (is-tag symbol)
	      (return))
	    (push symbol dtrs)))
    (nreverse dtrs)))

(defun read-rmrs-rule-head (istream)
;;;  <head> VP
  (let ((head 
	 (read istream)))
    head))

(defun read-rmrs-semstruct (istream)
;;; <semstruct>
;;;  <hook> <index> e 
;;;  <ep> <realpred> p-agt <arg> e <arg> x2 </ep>
;;;  </semstruct>
;;;
;;; one hook, any number of eps
  (let ((hook nil) (eps nil))
    (loop 
	 (let ((next-tag (read-next-tag istream)))
	      (case next-tag
		(HOOK (setf hook (read-rmrs-semstruct-hook istream)))
		(EP (push (read-rmrs-semstruct-ep istream)
			  eps))
		(t (return)))))
    (make-semstruct :hook hook :eps eps
		    ;; binding-list is constructed when the
		    ;; real semstruct is created
		    :binding-list nil)))

(defun read-rmrs-semstruct-hook (istream)
  ;;;  <hook> <index> e 
  (let ((next-tag (read-next-tag istream))
	(symbol nil))
    (case next-tag
      (INDEX (setf symbol (read istream)))
      (t nil))
    (if symbol
	(make-indices :index
		      (string symbol)))))

(defun read-rmrs-semstruct-ep (istream)
  ;;;  <ep> <realpred> p-agt <arg> e <arg> x2 </ep>
  (let ((pred nil) (args nil))
    (loop 
      (let ((next-tag (read-next-tag istream)))
	(case next-tag
	  (REALPRED (setf pred (read istream)))
	  (PRED (setf pred (make-dummy-pred)))
	  (ARG (push (read-rmrs-semstruct-ep-arg istream)
		     args))
	  (t (return)))))
    (make-ep :sort pred :flist (nreverse args))))

(defun read-rmrs-semstruct-ep-arg (istream)
  ;;;  <arg> e <arg> x2 </ep>
  (string (read istream)))

(defun read-rmrs-rule-eq (istream dtrs)
;;; <eq> VP.index e </eq>
  (let ((equalities nil))
    (loop (let ((symbol (read istream)))
	    (when (is-tag symbol)
	      (return))
	    (let* ((symbol-string (string symbol))
		   (char-pos (position #\. symbol-string)))
	      (push (if char-pos
			(split-hook-info 
			 symbol-string char-pos dtrs)
			symbol-string)
			equalities))))
    ;; want to end up with a list of either integer+index (representing
    ;; daughter hooks) or rule variables
    (make-equality :eq-els equalities)))

(defun split-hook-info (symbol-string char-pos dtrs)
  ;;; utility fn - given X.y and a list of dtrs containing X
  ;;; this returns (integer y) where integer is the position of
  ;;; X on the dtrs
  (let* 
      ((dtr (subseq symbol-string 0 char-pos))
       (hook-element (subseq symbol-string (+ 1 char-pos)))
       (dtr-number (position (intern dtr) dtrs)))
    (if (and dtr-number
	     (member hook-element *valid-hook-elements* :test #'equal))
	(make-pointer :dtrnum dtr-number 
		      :hook-el hook-element)
      (error "~%Invalid args to split-hook-info ~S ~S ~S"
	  symbol-string char-pos dtrs))))   


