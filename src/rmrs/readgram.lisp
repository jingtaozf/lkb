;;; Copyright (c) 2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.


(in-package :mrs)

(defparameter *valid-hook-elements* '("INDEX" "LABEL"))

;;; Reading

(defun read-rmrs-grammar (file-name)
  ;;; <!ELEMENT gram (rule)*>
  (setf *rule-instructions* nil)
  (clear-rule-record)
  (with-open-file (istream file-name :direction :input)
    (let ((rules (parse-xml-removing-junk istream)))
      (unless (equal (car rules) '|gram|)
        (error "~A is not a valid rules file" file-name))
      (loop for rule in (cdr rules)
          do
           (unless (xml-whitespace-string-p rule)
             (let ((next-rule (read-rmrs-rule rule)))
               (when next-rule
                 (add-rmrs-rule next-rule)))))))
  (setf *rule-instructions*
    (nreverse *rule-instructions*))
  nil)

(defun add-rmrs-rule (rule)
  (push rule *rule-instructions*))
                      
(defun read-rmrs-rule (rule)
;;;  <!ELEMENT rule (name, (comment)*, dtrs, head, (semstruct)*, (equalities)*)>
  (let ((tag (car rule)))
    (if (eq tag '|rule|)
	(let ((name nil) (dtrs nil) (head nil) (head-pos nil)
	      (semstruct nil) (eqs nil))
	  (loop for next-el in (cdr rule)
              do
                (unless (xml-whitespace-string-p next-el)
                  (let*  
                      ((next-tag (car next-el))
                       (tag-content (cdr next-el)))
                    (ecase next-tag
                      (|comment| tag-content)
                      (|name| (setf name (car tag-content)))
                      (|dtrs| (setf dtrs (read-rmrs-rule-dtrs tag-content)))
		;;; dtrs must appears before equalitiess 
                      (|head| (setf head (car tag-content)))
                      (|semstruct| (setf semstruct (read-rmrs-semstruct tag-content)))
                      (|equalities| (push (read-rmrs-rule-eq tag-content dtrs) eqs)))
                    (when head
		      (if (string-equal head "RULE")
			  (setf head-pos -1)
			(progn
			  (setf head-pos (position head dtrs :test #'string-equal))
                          (unless head-pos
                            (error 
                             "~%Head ~S is not a member of dtrs ~S in ~A"
                             head dtrs name))))))))
	  (make-rmrs-rule :name name
			  :dtrs dtrs
			  :arity (length dtrs)
			  :head head-pos
			  :semstruct semstruct
			  :eqs (nreverse eqs))))))

(defun read-rmrs-rule-dtrs (content)
  (loop for dtr in content
      when (eql (car dtr) '|dtr|)
      unless (string-equal (cadr dtr) "OPT")
      collect (cadr dtr)))
      
(defun read-rmrs-semstruct (content)
;;; <!ELEMENT semstruct (hook,(ep|rarg|ing|hcons)*)> 
  (let ((hook nil) (eps nil) (rargs nil) (ings nil)
        (h-cons nil))
    (loop for next-el in content
          do
          (unless (xml-whitespace-string-p next-el)
            (let*  
                ((next-tag (car next-el))
                 (tag-content (cdr next-el)))
	      (if (and (listp next-tag)
		       (eql (car next-tag) '|hcons|)
		       (string-equal (third next-tag) "qeq"))
	      	  (push (read-rmrs-semstruct-qeq tag-content)
                            h-cons)
	      (ecase next-tag
		(|hook| (setf hook (read-rmrs-semstruct-hook tag-content)))
		(|ep| (push (read-rmrs-semstruct-ep tag-content)
			  eps))
                (|rarg| (push (read-rmrs-semstruct-rarg tag-content)
                            rargs))
                (|ing| (push (read-rmrs-semstruct-in-g tag-content)
			     ings)))))))
    (make-semstruct :hook (or hook (make-default-hook))
                    :liszt eps
		    ;; binding-list is constructed when the
                    ;; real semstruct is created
                    :h-cons h-cons
                    :rmrs-args (nreverse rargs)
                    :in-groups ings
		    :bindings nil)))

(defun read-rmrs-semstruct-hook (content)
;;; <!ELEMENT hook (index,label)>  
  (let ((index nil) (label nil))
    (setf index (construct-grammar-var 
                 (read-rmrs-simple '|index| (car content))))
    (setf label (construct-grammar-var
                 (read-rmrs-simple '|label| (cadr content))))
    (make-indices 
         :label label
         :index index)))


(defun read-rmrs-semstruct-ep (content)
  ;;; <!ELEMENT ep ((gpred|pred|realpred),label,var)>
  (let ((pred nil) (arg nil) (label nil))
    (setf pred (read-rmrs-pred (car content)))
    (setf label (read-rmrs-simple '|label| (cadr content)))
    (setf arg (read-rmrs-simple '|var| (caddr content)))
    ;;; vars are just letters in grammar file
    (make-rel :sort pred 
              :handel (construct-grammar-var label)
              :flist (list (construct-grammar-var arg)))))


;;; read-rmrs-pred is in input.lisp

(defun read-rmrs-semstruct-rarg (content)
  ;;; <!ELEMENT rarg (rargname, label, (var|constant|lemma)) >
  (let ((name nil) (label nil) (arg nil))
    (setf name (read-rmrs-simple '|rargname| (car content)))
    (setf label (read-rmrs-simple '|label| (cadr content)))
    (let* ((argval (caddr content))
           (argvaltag (car argval)))
      (setf arg 
        (ecase argvaltag 
          (|lemma| (make-dummy-constant))
          (|constant|  
           (read-rmrs-simple '|constant| argval))
          (|var|
           (construct-grammar-var 
            (read-rmrs-simple '|var| argval))))))
    (make-rmrs-arg :arg-type name :label (construct-grammar-var label) 
                   :val arg)))
  
(defun read-rmrs-semstruct-in-g (content)
;;; <!ELEMENT ing (ing-a,ing-b) >
  (let ((ing-a nil) (ing-b nil))
    (setf ing-a (read-rmrs-simple '|ing-a| (car content)))
    (setf ing-b (read-rmrs-simple '|ing-b| (cadr content)))
    (make-in-group :labels (list (construct-grammar-var ing-a) 
                                 (construct-grammar-var ing-b)))))

(defun read-rmrs-semstruct-qeq (content)
;;; <!ELEMENT hcons (hi, lo)>
  (let ((hi nil) (lo nil))
    (setf hi (read-rmrs-simple '|hi| (car content)))
    (setf lo (read-rmrs-simple '|lo| (cadr content)))
    (make-hcons :relation "qeq"
		:scarg (construct-grammar-var hi) 
                :outscpd (construct-grammar-var lo))))

(defun read-rmrs-rule-eq (content dtrs)
;;; <!ELEMENT equalities (rv|dh)* >
;;; <!ELEMENT rv (#PCDATA) >
  (let ((processed-eqs
         (loop for next-el in content
               unless (xml-whitespace-string-p next-el)
               collect                
            (let*  
                ((next-tag (car next-el))
                 (tag-content (cdr next-el)))
	      (ecase next-tag
		(|rv| (construct-grammar-var (car tag-content)))
		(|dh| (read-daughter-hook tag-content dtrs)))))))       
    ;; want to end up with a list of either integer+index (representing
    ;; daughter hooks) or rule variables
    (make-equality :eq-els processed-eqs)))

(defun read-daughter-hook (dh dtrs)
  ;;; <!ELEMENT dh (dtr,he) >
  ;;; given a dh containing a dtr and a hook-element 
  ;;; and a list of dtrs containing dtr
  ;;; this returns (integer hook-element) where integer is the position of
  ;;; dtr on the dtrs
  (let* 
      ((dtr (read-rmrs-simple '|dtr| (car dh)))
       (hook-element (read-rmrs-simple '|he| (cadr dh)))
       (dtr-number (position dtr dtrs :test #'string-equal)))
    (if (and dtr-number
	     (member hook-element *valid-hook-elements* :test #'string-equal))
	(make-pointer :dtrnum dtr-number 
		      :hook-el hook-element)
      (error "~%Invalid args to convert-hook-info ~S ~S"
	  dh dtrs))))   

;;; Outputting rules

(defun write-rmrs-rules (filename)
  (with-open-file (ostream filename :direction :output
                   :if-exists :supersede)
    (format ostream "~%<gram>")
    (loop for rule in *rule-instructions*
        do
          (output-rmrs-rule rule ostream))
    (format ostream "~%</gram>~%")))

(defun output-rmrs-rule (rule ostream)
  (let ((dtrs (rmrs-rule-dtrs rule))
        (semstruct (rmrs-rule-semstruct rule))
        (head (rmrs-rule-head rule)))
    (format ostream "~%<rule>")
    (format ostream "~%<name>~A</name>" (rmrs-rule-name rule))
    (format ostream "~%<dtrs>~{<dtr>~A</dtr>~}</dtrs>" dtrs)
    (when head (format ostream "~%<head>~A</head>" (elt dtrs head)))
    (when semstruct
      (output-rmrs-semstruct semstruct ostream))
    (loop for eq in (rmrs-rule-eqs rule)
        do
          (output-rmrs-eq eq dtrs ostream))
    (format ostream "~%</rule>~%")))

(defun output-rmrs-semstruct (semstruct ostream)
  ;;; <!ELEMENT hook (index,label)>
  (format ostream "~%<semstruct>")
  (internal-output-rmrs semstruct 'gramxml ostream)
  (format ostream "~%</semstruct>"))

(defun output-rmrs-eq (eq dtrs ostream)
  ;;; <!ELEMENT equalities (rv|dh)* >
  ;;; <!ELEMENT rv (#PCDATA) >
  ;;; <!ELEMENT dh (dtr,he) >
  ;;; <!ELEMENT he (#PCDATA) >
  (format ostream "~%<equalities>")
  (loop for eq-el in (equality-eq-els eq)
        do
        (if (pointer-p eq-el)
            (format ostream "<dh><dtr>~A</dtr><he>~A</he></dh>" 
                    (elt dtrs (pointer-dtrnum eq-el))
                    (pointer-hook-el eq-el))
          (format ostream "<rv>~A</rv>" (var-id eq-el))))
  (format ostream "</equalities>"))

                     
