(in-package :mrs)

(defparameter *rule-instructions* nil)

(defparameter *valid-hook-elements* '("INDEX" "LABEL"))

;;; Reading

(defun read-rmrs-grammar (file-name)
  (setf *rule-instructions* nil)
  (clear-rule-record)
  (let ((*readtable* (make-xml-break-table)))
    (with-open-file (istream file-name :direction :input)
      (loop
        (let ((next-char (peek-char t istream nil 'eof)))
         (when (eql next-char 'eof) (return))
         (if (eql next-char #\;) 
             (read-line istream)
           (let ((next-rule (read-rmrs-rule istream)))
             (when (null next-rule) (return))
             (add-rmrs-rule next-rule)))))))
  (setf *rule-instructions*
    (nreverse *rule-instructions*))
  nil)

(defun add-rmrs-rule (rule)
  (push rule *rule-instructions*))
                      
(defun read-rmrs-rule (istream)
#| 
<rule>
<name>S/np_vp</name>
<dtrs>NP VP</dtrs>
<head>VP</head>
<semstruct>
<hook><index>E</index><label>H</label></hook>
<rarg><rargname>ARG1</rargname><label>H</label><arg>X</arg></ep>
</semstruct>
<eq>X NP.INDEX</eq>
<eq>H VP.LABEL</eq>
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
                (COMMENT (read-rmrs-rule-comment istream))
		(NAME (setf name (read-rmrs-rule-name istream)))
		(DTRS (setf dtrs (read-rmrs-rule-dtrs istream)))
		;;; dtrs must appears before eqs 
		(HEAD (setf head (read-rmrs-rule-head istream)))
		(SEMSTRUCT (setf semstruct (read-rmrs-semstruct istream)))
		(EQ (push (read-rmrs-rule-eq istream dtrs) eqs))
		(/RULE (return)))))
	  (when head (setf head-pos (position head dtrs))
		(unless head-pos
		  (error 
                   "~%Head ~S is not a member of dtrs ~S in ~A at position ~A"
			 head dtrs name (file-position istream))))
	  (make-rmrs-rule :name name
			  :dtrs dtrs
			  :arity (length dtrs)
			  :head head-pos
			  :semstruct semstruct
			  :eqs (nreverse eqs))))))

(defun read-rmrs-rule-comment (istream)
;;; <comment>S/np_vp</comment>
  (let ((comment 
	 (read-string-to-tag istream)))
    (check-for-end-tag 'COMMENT istream)
    comment))

(defun read-rmrs-rule-name (istream)
;;; <name>S/np_vp</name>
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
;;; <hook><index>E</index><label>H</label></hook>
;;; <rarg><rargname>ARG1</rargname><label>H</label><arg>X</arg></ep>
;;; </semstruct>
;;;
;;; one hook, any number of eps, rargs, in-gs, h-cons
  (let ((hook nil) (eps nil) (rargs nil) (ings nil)
        (h-cons nil))
    (loop 
	 (let ((next-tag (read-next-tag istream)))
	      (ecase next-tag
		(HOOK (setf hook (read-rmrs-semstruct-hook istream)))
		(EP (push (read-rmrs-semstruct-ep istream)
			  eps))
                (RARG (push (read-rmrs-semstruct-rarg istream)
                            rargs))
                (ING (push (read-rmrs-semstruct-in-g istream)
                            ings))
                (HCONS (push (read-rmrs-semstruct-hcons istream)
                            h-cons))
		(/SEMSTRUCT (return)))))
    (make-semstruct :hook (or hook (make-default-hook))
                    :liszt eps
		    ;; binding-list is constructed when the
                    ;; real semstruct is created
                    :h-cons h-cons
                    :rmrs-args rargs
                    :in-groups ings
		    :bindings nil)))

(defun read-rmrs-semstruct-hook (istream)
  ;;;  <hook><index>e</index><label>h</label></hook>
  (let ((index nil) (label nil))
    (loop 
      (let 
          ((next-tag (read-next-tag istream)))
        (ecase next-tag
          (INDEX (when index (error "index already set to ~A at position ~A" 
                                    index (file-position istream)))
                 (setf index (read istream))
                 (check-for-end-tag 'index istream))
          (LABEL (when label (error "label already set to ~A at position ~A" 
                                    label (file-position istream)))
                 (setf label (read istream))
                 (check-for-end-tag 'label istream))
          (/HOOK (return)))))
    (if (or label index)
        (make-indices 
         :label (string label)
         :index (string index)))))


(defun read-rmrs-semstruct-ep (istream)
  ;;;  <ep><realpred>unspec</realpred><label>h</label><arg>x2</arg></ep>
  (let ((pred nil) (args nil) (label nil))
    (loop 
      (let ((next-tag (read-next-tag istream)))
	(ecase next-tag
	  (REALPRED (setf pred (read istream))
                    (check-for-end-tag 'realpred istream))
	  (PRED (setf pred (make-dummy-pred))
                (check-for-end-tag 'pred istream))
          (LABEL (setf label (read istream))
                 (check-for-end-tag 'label istream))
	  (ARG (push (read-rmrs-semstruct-ep-arg istream)
		     args)
               (check-for-end-tag 'arg istream))
	  (/EP (return)))))
    (make-rel :sort pred 
              :handel (string label)
              :flist (nreverse args))))

(defun read-rmrs-semstruct-rarg (istream)
  ;;; <rarg><rargname>ARG1</rargname><label>H</label><arg>X</arg></rarg>
  ;;; all elements obligatory and ordered
  ;;; FIX
  ;;; value might not be an arg but a constant?
  (let ((next-tag (read-next-tag istream))
        (name nil) (label nil) (arg nil))
    (unless (eql next-tag 'RARGNAME)
      (error "tag ~A expected and not found at position ~A" 
             'RARGNAME (file-position istream)))
    (setf name (read istream))
    (check-for-end-tag 'rargname istream)
    (setf next-tag (read-next-tag istream))
    (unless (eql next-tag 'LABEL)
      (error "tag ~A expected and not found at position ~A" 
             'LABEL (file-position istream)))
    (setf label (read-rmrs-semstruct-ep-arg istream))
    (check-for-end-tag 'label istream)
    (setf next-tag (read-next-tag istream))
    (unless (eql next-tag 'ARG)
      (error "tag ~A expected and not found at position ~A" 
             'ARG (file-position istream)))
    (setf arg (read-rmrs-semstruct-ep-arg istream))
    (check-for-end-tag 'arg istream)
    (check-for-end-tag 'rarg istream)
    (make-rmrs-arg :arg-type name :label label :val arg))) 
  
(defun read-rmrs-semstruct-in-g (istream)
  ;;;<ing><ing-a>H1</ing-a><ing-b>H2</ing-b></ing>
  ;;; all elements obligatory and ordered
  (let ((next-tag (read-next-tag istream))
        (ing-a nil) (ing-b nil))
    (unless (eql next-tag 'ING-A)
      (error "tag ~A expected and not found at position ~A" 
             'ING-A (file-position istream)))
    (setf ing-a (read-rmrs-semstruct-ep-arg istream))
    (check-for-end-tag 'ing-a istream)
    (setf next-tag (read-next-tag istream))
    (unless (eql next-tag 'ING-B)
      (error "tag ~A expected and not found at position ~A" 
             'ING-B (file-position istream)))
    (setf ing-b (read-rmrs-semstruct-ep-arg istream))
    (check-for-end-tag 'ing-b istream)
    (check-for-end-tag 'ing istream)
    (make-in-group :labels (list ing-a ing-b))))

(defun read-rmrs-semstruct-hcons (istream)
  ;;;<hcons><hreln>qeq</hreln><hi>H1</hi><lo>H2</lo></hcons>"
  ;;; all elements obligatory and ordered
    (let ((next-tag (read-next-tag istream))
        (name nil) (hi nil) (lo nil))
    (unless (eql next-tag 'hreln)
      (error "tag ~A expected and not found at position ~A" 
             'hreln (file-position istream)))
    (setf name (read istream))
    (check-for-end-tag 'hreln istream)
    (setf next-tag (read-next-tag istream))
    (unless (eql next-tag 'hi)
      (error "tag ~A expected and not found at position ~A" 
             'hi (file-position istream)))
    (setf hi (read-rmrs-semstruct-ep-arg istream))
    (check-for-end-tag 'hi istream)
    (setf next-tag (read-next-tag istream))
    (unless (eql next-tag 'lo)
      (error "tag ~A expected and not found at position ~A" 
             'lo (file-position istream)))
    (setf lo (read-rmrs-semstruct-ep-arg istream))
    (check-for-end-tag 'lo istream)
    (check-for-end-tag 'hcons istream)
    (make-hcons :relation name :scarg hi :outscpd lo)))
  
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
    (format ostream "~%<hook>")
    (when (indices-index (semstruct-hook semstruct))
      (format ostream "<index>~A</index>" (indices-index (semstruct-hook semstruct))))
    (when (indices-label (semstruct-hook semstruct))
      (format ostream "<label>~A</label>" 
              (indices-label (semstruct-hook semstruct))))
    (format ostream "</hook>"))
  (internal-output-rmrs semstruct 'xml ostream)
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

                     
