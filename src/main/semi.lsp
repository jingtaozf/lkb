;;; Copyright (c) 2002 
;;;   Ben Waldron;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

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

(defun get-arg-info (lex-id)
  (let* ((dag (and
	       (get-lex-entry-from-id lex-id)
	       (dag-path-val '(synsem lkeys keyrel) (tdfs-indef (lex-entry-full-fs (read-psort *lexicon* lex-id))))))
	 (n 0)
	 (argN 'arg0)
	 (dagN))
    
    (loop
	while (setf dagN (dag-path-val (list argN) dag))
	collect (list
		 (cons :lex-id lex-id)
		 (cons :arg n)
		 (cons :type (dag-path-type (list) dagN))
		 (cons :tense (dag-path-type (list 'e 'tense) dagN))
		 (cons :aspect-perf (dag-path-type (list 'e 'aspect 'perf) dagN))
		 (cons :aspect-progr (dag-path-type (list 'e 'aspect 'progr) dagN))
		 (cons :mood (dag-path-type (list 'e 'mood) dagN))
		 (cons :pn (dag-path-type (list 'png 'pn) dagN))
		 (cons :gen (dag-path-type (list 'png 'gen) dagN))
		 )
	do
	  (setf n (1+ n))
	  (setf argN (str-2-symb (format nil "arg~a" n))))))
    

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

(defun print-arg-info (stream lex-id)
  (let ((arg-info (get-arg-info lex-id)))
    (loop
	for x in arg-info
	do
	  (format stream "~a~%"
		  (tsv-line 
			  (mapcar #'cdr x))))))
		    
(defmethod export-semi-args-to-file ((lexicon lex-database) filename)
  (setf filename (namestring (pathname filename)))
  ;;(format t "~%Exporting semi args to file ~a" filename)
  (with-open-file 
      (ostream filename :direction :output :if-exists :supersede)
    (export-semi-args lexicon ostream)))

(defmethod export-semi-args ((lexicon lex-database) stream)
    (mapc 
     #'(lambda (x) (print-arg-info stream x))
     (collect-psort-ids lexicon)))

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
(defun dump-obj-semi-args (filename)
  (export-semi-args-to-file *psql-lexicon* filename))
