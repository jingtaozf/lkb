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
  (let ((*readtable* (make-xml-break-table)))
    (with-open-file (istream file-name :direction :input)
      (loop
        (let ((next-rule (read-rmrs-rule istream)))
          (when (null next-rule) (return))
          (add-rmrs-rule next-rule)))))
  (setf *rule-instructions*
    (nreverse *rule-instructions*))
  nil)

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
<name>S - NP VP</name>
<dtrs>NP VP</dtrs>
<head>VP</head>
<semstruct>
 <hook><index>e</index></hook> 
 <ep><realpred>p-agt</realpred><arg>e</arg><arg>x2</arg></ep>
</semstruct>
<eq>VP.index e</eq>
<eq>NP.index x</eq>
</rule>
|#
  (let ((tag (read-next-tag istream)))
    (if (eq tag 'RULE)
	(let ((name nil) (dtrs nil) (head nil) (head-pos nil)
	      (semstruct nil) (eqs nil))
	  (loop
	    (let  
		((next-tag (read-next-tag istream)))
	      (ecase next-tag
		(NAME (setf name (read-rmrs-rule-name istream)))
		(DTRS (setf dtrs (read-rmrs-rule-dtrs istream)))
		;;; dtrs must appears before eqs 
		(HEAD (setf head (read-rmrs-rule-head istream)))
		(SEMSTRUCT (setf semstruct (read-rmrs-semstruct istream)))
		(EQ (push (read-rmrs-rule-eq istream dtrs) eqs))
		(/RULE (return)))))
	  (when head (setf head-pos (position head dtrs))
		(unless head-pos
		  (error "~%Head ~S is not a member of dtrs ~S in ~A"
			 head dtrs name)))
	  (make-rmrs-rule :name name
			  :dtrs dtrs
			  :arity (length dtrs)
			  :head head-pos
			  :semstruct semstruct
			  :eqs (nreverse eqs))))))

(defun read-rmrs-rule-name (istream)
;;;  <name>S - NP VP</name>
  (let ((name 
	 (read-string-to-tag istream)))
    (check-for-end-tag 'NAME istream)
    name))

(defun read-rmrs-rule-dtrs (istream)
;;;  <dtrs>NP VP</dtrs>
  (let ((dtrs (read-symbols-to-tag istream)))
    (check-for-end-tag 'dtrs istream)
    dtrs))

(defun read-rmrs-rule-head (istream)
;;;  <head>VP</head>
  (let ((head 
	 (read istream)))
    (check-for-end-tag 'head istream)
    head))

(defun read-rmrs-semstruct (istream)
;;; <semstruct>
;;;  <hook><index>e</index></hook>
;;;  <ep><realpred>p-agt</realpred><arg>e</arg><arg>x2</arg></ep>
;;;  </semstruct>
;;;
;;; one hook, any number of eps
  (let ((hook nil) (eps nil))
    (loop 
	 (let ((next-tag (read-next-tag istream)))
	      (ecase next-tag
		(HOOK (setf hook (read-rmrs-semstruct-hook istream)))
		(EP (push (read-rmrs-semstruct-ep istream)
			  eps))
		(/SEMSTRUCT (return)))))
    (make-semstruct :hook hook :eps eps
		    ;; binding-list is constructed when the
		    ;; real semstruct is created
		    :binding-list nil)))

(defun read-rmrs-semstruct-hook (istream)
  ;;;  <hook><index>e</index></hook>
  (let ((next-tag (read-next-tag istream))
	(symbol nil))
    (ecase next-tag
      (INDEX (setf symbol (read istream))))
    (check-for-end-tag 'index istream)
    (check-for-end-tag 'hook istream)
    (if symbol
        (make-indices :index
		      (string symbol)))))

(defun read-rmrs-semstruct-ep (istream)
  ;;;  <ep><realpred>p-agt</realpred><arg>e</arg><arg>x2</arg></ep>
  (let ((pred nil) (args nil))
    (loop 
      (let ((next-tag (read-next-tag istream)))
	(ecase next-tag
	  (REALPRED (setf pred (read istream))
                    (check-for-end-tag 'realpred istream))
	  (PRED (setf pred (make-dummy-pred))
                (check-for-end-tag 'pred istream))
	  (ARG (push (read-rmrs-semstruct-ep-arg istream)
		     args)
               (check-for-end-tag 'arg istream))
	  (/EP (return)))))
    (make-ep :sort pred :flist (nreverse args))))

(defun read-rmrs-semstruct-ep-arg (istream)
  (string (read istream)))

(defun read-rmrs-rule-eq (istream dtrs)
;;; <eq> VP.index e </eq>
  (let* ((equalities (read-symbols-to-tag istream))
         (processed-eqs
          (loop for symbol in equalities
              collect
                (let* ((symbol-string (string symbol))
                       (char-pos (position #\. symbol-string)))
                  (if char-pos
                      (split-hook-info 
                       symbol-string char-pos dtrs)
                    symbol-string)))))
    ;; want to end up with a list of either integer+index (representing
    ;; daughter hooks) or rule variables
    (check-for-end-tag 'eq istream)
    (make-equality :eq-els processed-eqs)))

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

;;; Outputting rules

(defun write-rmrs-rules (filename)
  (with-open-file (ostream filename :direction :output
                   :if-exists :supersede)
    (loop for rule in *rule-instructions*
        do
          (output-rmrs-rule rule ostream))))

(defun output-rmrs-rule (rule ostream)
  (let ((dtrs (rmrs-rule-dtrs rule))
        (semstruct (rmrs-rule-semstruct rule))
        (head (rmrs-rule-head rule)))
    (format ostream "~%<rule>")
    (format ostream "~%<name>~A</name>" (rmrs-rule-name rule))
    (format ostream "~%<dtrs>~A~{ ~A~}</dtrs>" (car dtrs) (cdr dtrs))
    (when head (format ostream "~%<head>~A</head>" (elt dtrs head)))
    (when semstruct
      (output-rmrs-semstruct semstruct ostream))
    (loop for eq in (rmrs-rule-eqs rule)
        do
          (output-rmrs-eq eq dtrs ostream))
    (format ostream "~%</rule>~%")))

(defun output-rmrs-semstruct (semstruct ostream)
  (format ostream "~%<semstruct>")
  (when (semstruct-hook semstruct)
    (format ostream "~%<hook><index>~A</index></hook>" 
            (indices-index (semstruct-hook semstruct))))
  (loop for ep in (semstruct-eps semstruct)
      do
        (output-rmrs-ep ep ostream))
  (format ostream "~%</semstruct>"))

(defun output-rmrs-eq (eq dtrs ostream)
  (format ostream "~%<eq>")
  (let ((first t))
    (loop for eq-el in (equality-eq-els eq)
        do
          (unless first (format ostream " "))
          (setf first nil)
          (if (pointer-p eq-el)
              (format ostream "~A.~A" (elt dtrs (pointer-dtrnum eq-el))
                      (pointer-hook-el eq-el))
            (format ostream "~A" eq-el))))
  (format ostream "</eq>"))

                     
(defun output-rmrs-ep (ep ostream)
  (format ostream "~%<ep>")
  (let ((pred (ep-sort ep)))
    (if (dummy-pred-p pred) 
        (format ostream "<pred></pred>")
      (format ostream "<realpred>~A</realpred>" pred)))
  (loop for arg in (ep-flist ep)
      do
        (format ostream "<arg>~A</arg>" arg))             
  (format ostream "</ep>"))
