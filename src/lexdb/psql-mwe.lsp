;;; Copyright (c) 2003 
;;;   Ben Waldron;
;;;   see `licence.txt' for conditions.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Postgres database: MWE extensions
;;;

(in-package :lkb)

(defmethod mwe-initialize-lex ((lexicon psql-database))
  (mwe-initialize-userschema lexicon))

(defmethod mwe-initialize-userschema ((lexicon psql-database))
  (format *postgres-debug-stream* "~%(mwe initializing private schema)")
  (fn-get-val lexicon ''mwe-initialize-schema))

;;;
;;; postgres interface
;;;

(defmethod mwe-retrieve-id-set ((lexicon psql-lex-database))
  (mapcar
   #'(lambda (x) (str-2-symb (cdar x))) 
   (fn-get-records lexicon ''mwe-retrieve-id-set)))

(defmethod mwe-retrieve-type ((lexicon psql-lex-database) mwe-id)
  (str-2-symb (fn-get-val lexicon ''mwe-retrieve-type (symb-2-str mwe-id))))

(defmethod mwe-retrieve-keyrels ((lexicon psql-lex-database) mwe-id)
  (let* ((raw-results (fn-get-records lexicon ''mwe-retrieve-keyrels (symb-2-str mwe-id)))
	 (s (make-sequence 'vector (length raw-results))))
    (mapcar #'(lambda (x) 
		(setf (aref s (1- (str-2-num (get-assoc-val :slot x))))
		  (str-to-mixed (get-assoc-val :keyrel x))))
	    raw-results
	    )))

(defmethod dump-multi-db ((lexicon psql-lex-database) filename)  
  (setf filename (namestring (pathname filename)))
  (fn-get-records lexicon ''dump-multi-db filename))

(defmethod merge-multi-into-db ((lexicon psql-lex-database) filename)  
  (setf filename (namestring (pathname filename)))
  (fn-get-records lexicon ''merge-multi-into-db filename))

#+:null
(defun dump-multi-psql-lexicon (filename)
  (get-postgres-temp-filename)
  (setf filename (namestring (pathname filename)))
  (dump-multi-db *psql-lexicon* *postgres-temp-filename*)
  (common-lisp-user::run-shell-command (format nil "cp ~a ~a"
					       *postgres-temp-filename*
					       filename)))

#+:null
(defun merge-multi-into-psql-lexicon (filename)
  (setf filename (namestring (pathname filename)))
  (unless
      (and *psql-lexicon* (connection *psql-lexicon*))
    (initialize-psql-lexicon))
  (merge-multi-into-db *psql-lexicon* filename)
  (initialize-psql-lexicon))

;;;
;;; misc
;;;

(defun get-assoc-val (x assoc-list)
  (cdr (assoc x assoc-list)))


;;;
;;; generate TDL code for MWE entries
;;;

(defmethod mwe-to-tdl ((lexicon psql-lex-database) mwe-id)
  (format 
   nil "~%~a := ~a.~%"
   (tdl-val-str mwe-id)
   (p-2-tdl (mwe-build-P-list (mwe-retrieve-type lexicon mwe-id)
			      (mapcar #'(lambda (x) (cons 'PRED (list (list x))))
				      (mwe-retrieve-keyrels lexicon mwe-id))))))
	     
(defun mwe-build-P-list (type keyrel-list)
  (append
   (list (list type))
   (list (cons 'SEM 
	       (list (cons 'IDRELS
			   (build-PD-list keyrel-list 1)))))))

(defun build-PD-list (d-list coindex)
  (append
   (list (cons 'LAST (build-PD-list-aux-LIST nil coindex)))
   (list (cons 'LIST (build-PD-list-aux-LIST d-list coindex)))))

(defun build-PD-list-aux-LIST (d-list coindex)
  (cond
   ((null d-list)
    (list (list (str-2-symb (get-coindex-symb coindex)))))
   (t
    (append
     (list (cons 'FIRST (list (car d-list))))
     (list (cons 'REST (build-PD-list-aux-LIST (cdr d-list) coindex)))))))

(defun get-coindex-symb (i)
  (format nil "#~a" i))

;;;
;;; script file fn
;;;

(defmethod mwe-read-roots ((lexicon psql-lex-database))
  ;;(initialise-psort-file file-name :root)
  (format  t "~%Loading MWE roots from lexical database ~a" (dbname lexicon))
  (let ((*readtable* (make-tdl-break-table)))
    (mapcar 
     #'(lambda (x)
	 (with-input-from-string (istream (mwe-to-tdl lexicon x))
	   (read-tdl-psort-stream istream :root)))
     (mwe-retrieve-id-set lexicon)))
  (finalize-psort-file :root))

(defmethod mwe-read-root-entry ((lexicon psql-lex-database) mwe-id)
  (let ((*readtable* (make-tdl-break-table)))
    (with-input-from-string (istream (mwe-to-tdl lexicon mwe-id))
      (read-tdl-psort-stream istream :root))))

(defmethod reload-roots-mwe ((lexicon psql-lex-database))
    (mwe-read-roots lexicon)
    (format  t "~%MWE roots reloaded from lexical database ~a" (dbname lexicon)))

