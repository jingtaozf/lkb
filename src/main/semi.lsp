;;; Copyright (c) 2003
;;;   Ben Waldron;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

(defvar *current-lex-id*)

(defun dag-path-val (path dag)
  (cond
   ((null dag)
    nil)
   ((null path)
    dag)
   (t
    (dag-path-val
     (cdr path)
     (cdr (assoc (car path) (dag-arcs dag)))))))

(defun dag-path-type (path dag)
  (let ((val (dag-path-val path dag)))
    (if (typep val 'dag)
        (dag-type val)
      nil)))

(defun get-arg-info (stream lex-id)
  (let* ((entry (get-lex-entry-from-id lex-id))
	 (dag (and
	       entry
	       (dag-path-val '(synsem lkeys keyrel) (tdfs-indef (lex-entry-full-fs entry)))))
	 (n 0)
	 (argN 'arg0)
	 (dagN))
    
    (loop
	while (setf dagN (dag-path-val (list argN) dag))
	do
	  (format stream "~a~%"
		  (tsv-line 
		   (list
		    lex-id ;;lex-id
		    n ;;arg
		    (dag-path-type (list) dagN) ;;type
		    (dag-path-type (list 'e 'tense) dagN) ;;tense
		    (dag-path-type (list 'e 'aspect 'perf) dagN) ;;aspect-perf
		    (dag-path-type (list 'e 'aspect 'progr) dagN) ;;aspect-progr
		    (dag-path-type (list 'e 'mood) dagN) ;;mood
		    (dag-path-type (list 'png 'pn) dagN) ;;pn
		    (dag-path-type (list 'png 'gen) dagN) ;;gen
		    )))
	  
	  (setf n (1+ n))
	  (setf argN (str-2-symb (format nil "arg~a" n))))
    (unexpand-psort *lexicon* entry)
    ))
    

(defun get-secondaries-info (stream lex-id)
  (let* ((entry (get-lex-entry-from-id lex-id))
	 (dag (and
	       entry
	       (tdfs-indef (lex-entry-full-fs entry))))
	 (dag-keyrel (dag-path-val '(synsem lkeys keyrel) dag))
	 (*current-lex-id* lex-id)
	 (cont-rels (dag-diff-list-2-list 
		     (dag-path-val '(synsem local cont rels) dag)))
	 )
    (loop
	for x in (remove dag-keyrel cont-rels)
	do
	  (format stream "~a~a~a~%" 
		  (string-downcase lex-id)
		  #\tab
		  (string-downcase (dag-path-type '(pred) x))))
  (unexpand-psort *lexicon* entry)))
    

(defun tsv-line (str-list)
  (str-list-2-str
   (mapcar #'symb-2-tsv-val str-list)
   (format nil "~a" #\tab)))

(defun tsv-escape (str &optional (sep-char #\tab))
  (let ((l))
    (do ((i (1- (length str)) (1- i)))
	((< i 0))
      (push (aref str i) l)
      (if (eq (aref str i) sep-char)
	  (push #\\ l)))
    (concatenate 'string l)))

(defun symb-2-tsv-val (symb)
  (cond
   ((null symb)
    "\\N")
   (t
    (tsv-escape (string-downcase (format nil "~a"  symb))))))

(defmethod dump-obj-semi-main-aux ((lexicon psql-lex-database) filename)  
  (setf filename (namestring (pathname filename)))
  (fn-get-records lexicon ''dump-obj-semi-main filename))

(defun dump-obj-semi-main (filename)
  (get-postgres-temp-filename)
  (setf filename (namestring (pathname filename)))
  (dump-obj-semi-main-aux *psql-lexicon* *postgres-temp-filename*)
  (common-lisp-user::run-shell-command (format nil "cp ~a ~a"
					       *postgres-temp-filename*
					       filename)))

(defmethod dump-obj-semi-args-aux ((lexicon lex-database) filename)
  (setf filename (namestring (pathname filename)))
  (with-open-file 
      (ostream filename :direction :output :if-exists :supersede)
    (mapc 
     #'(lambda (x) (get-arg-info ostream x))
     (collect-psort-ids lexicon))))

(defun dump-obj-semi-args (filename)
  (dump-obj-semi-args-aux *psql-lexicon* filename))

(defmethod dump-obj-semi-secondaries-aux-file ((lexicon lex-database) filename)
  (setf filename (namestring (pathname filename)))
  (with-open-file 
      (ostream filename :direction :output :if-exists :supersede)
    (dump-obj-semi-secondaries-aux-stream lexicon ostream)))

(defmethod dump-obj-semi-secondaries-aux-stream ((lexicon lex-database) ostream)
    (mapc 
     #'(lambda (x) (get-secondaries-info ostream x))
     (collect-psort-ids lexicon)))

(defun dump-obj-semi-secondaries (filename)
  (dump-obj-semi-secondaries-aux-file *psql-lexicon* filename))

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
	  )))
	  
  
