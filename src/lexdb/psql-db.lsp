;;; Copyright (c) 2002-2004
;;;   Ann Copestake, Fabre Lambeau, Stephan Oepen, Benjamin Waldron;
;;;   see `licence.txt' for conditions.


(in-package :lkb)

;;;
;;; Postgres interface
;;;

;;;
;;; moved here from `src/main/initializations.lsp'.           (20-may-04; oe)
;;;

(defun psql-initialize ()
  ;;
  ;; make sure we `mark' the current universe as PSQL-enabled.
  ;;
  (pushnew :psql *features*)
  (handler-case (load "libpq.so") 
    (file-error () 
      ;; some feedback to user
      (format t ";   Warning: cannot load libpq.so")
      (format t "~%;            (PSQL lexicon functionality will be unavailable)")
      (format t "~%;            (hint: are the PostgreSQL libraries installed on your machine?)")
      ;; need this for backward compatibility with ERG script
      ;; (also a good idea anyway)
      (setf *features* (remove :psql *features*)))))


(defun compat-version (lexdb-version)
  (when (stringp lexdb-version)
    (subseq lexdb-version 0 3)))  
    

(defun dump-psql-lexicon (filename)
  (when
      (catch 'pg:sql-error
	(progn
	  (get-postgres-temp-filename)
	  (let* ((revision-filename 
		 (namestring (pathname (format nil "~a.csv" filename))))
		(defn-filename 
		    (namestring (pathname (format nil "~a.dfn" filename))))
		 (pg-files 
		  (string-2-str-list-on-spc
		   (dump-db *psql-lexicon*)
		   :esc nil))
		 (pg-rev (first pg-files))
		 (pg-dfn (second pg-files)))
	    (common-lisp-user::run-shell-command (format nil "cp ~a ~a"
							 pg-rev
							 revision-filename))
	    (common-lisp-user::run-shell-command (format nil "cp ~a ~a"
							 pg-dfn
							 defn-filename))
	    nil)))
    (format t "~%Dump aborted...")))

(defun dump-scratch (filename)
  (get-postgres-temp-filename)
  (setf filename (namestring (pathname filename)))
  (dump-scratch-db *psql-lexicon* *postgres-temp-filename*)
  (common-lisp-user::run-shell-command (format nil "cp ~a ~a"
					       *postgres-temp-filename*
					       filename)))

(defun absolute-namestring (format str)
  (namestring (pathname (format nil format str))))

(defun merge-into-psql-lexicon (lexicon filename)
  "reconnect as db owner and merge new data into lexdb"
  (with-slots (dbname host port) lexicon
    (let ((conn-db-owner (make-instance 'psql-database
			   :dbname dbname
			   :host host
			   :port port
			   :user (raw-get-val lexicon "SELECT db_owner()")))
	  (count-new-dfn 0)
	  (count-new 0))
      (connect conn-db-owner)
      (retrieve-fn-defns conn-db-owner)
      (when
	  (catch 'pg:sql-error
	    (progn
	      (get-postgres-temp-filename)
	      (let* ((rev-filename 
		      (absolute-namestring "~a.csv" 
					   filename))
		     (dfn-filename 
		      (absolute-namestring "~a.dfn" 
					   filename)))
		(setf count-new 
		  (merge-into-db conn-db-owner 
				 rev-filename))
		(if (probe-file dfn-filename)
		    (setf count-new-dfn
		      (merge-defn conn-db-owner 
				  dfn-filename)))
		nil
		)))
	(format t "Merge new entries aborted..."))
      (if (and 
	   (equal count-new 0)
	   (equal count-new-dfn 0))
	  nil
	(initialize-psql-lexicon)))))

(defun make-db-access-fn (str-fn-name-in str type-list)
  (let* ((fn-name (new-fn-name (concatenate 'string "sql-query-string-" (string str-fn-name-in))))
	 (tmp (prepare-db-access-fn str type-list str-fn-name-in))
	 (format-cmd (append '(format nil) (car tmp)))
	 (args (cdr tmp))
	 (fn-defn (list 'defun fn-name args format-cmd)))
    (eval fn-defn)))

(defun new-fn-name (str)
  (loop
      with i = 0
      with fn-name
      do
	(setf fn-name (str-2-symb (concatenate 'string str (num-2-str i))))
	(unless (fboundp fn-name)
	  (return fn-name))
	(setf i (1+ i))))
	
(defun prepare-db-access-fn (str type-list str-fn-name)
  (let ((stream (make-string-output-stream))
	(args)
	(arg-vars '(a b c d e f g h i j))
	(arity (length type-list)))
  (loop
      with max = (1- (length str))
      and c
      for i from 0 to max
      with max-arg = -1
      and arg
      and type
      and explicit-type-str
      do
	(setf c (aref str i))
	(cond 
	 ((eq c #\~)
	  (format stream "~~~~"))
	 ((eq c #\\)
	  (if (= i max)
	      (error "invalid string ('\\' cannot be string final)"))
	  (format stream "~a" (aref str (1+ i)))
	  (setf i (1+ i)))
	 ((eq c #\$)
	  (if (= i max)
	      (error "invalid string ('$' cannot be string final)"))
;	  (unless (numberp (char-2-symb (aref str (1+ i))))
	  (setf arg (char-2-num (aref str (1+ i))))
	  (unless arg
	    (error "invalid string ('$' can only preceed a digit)"))
	  (if (> arg (1- arity))
	      (error "whilst compiling embedded SQL function ~a(~a). Argument $~a is not valid in 
~%~a~a~a" 
		     str-fn-name 
		     (str-list-2-str-by-str (get-$-args arity) ",") 
		     arg 
		     (if (> (- i 20) 0) "..." "")
		     (subseq str 
			     (max 0 (- i 20)) 
			     (min (length str) (+ i 20)))
		     (if (< (+ i 20) (length str)) "..." "")
		     ))
	  (setf max-arg (max max-arg arg))
	  (setf type (cdr (assoc arg type-list)))
	  (setf explicit-type-str (get-explicit-type str (1+ i)))
	  (when explicit-type-str
	    (setf type (str-2-symb explicit-type-str))
	    (setf i (+ i 1 (length explicit-type-str))))
	  (cond
	   ((equal type 'text)
	    (push (list 'sql-embedded-text (nth arg arg-vars)) args))
	   ((equal type 'like-text)
	    (push (list 'sql-like-text (nth arg arg-vars)) args))
	   ((equal type 'select-list)
	    (push (nth arg arg-vars) args))
	   ((equal type 'value-list)
	    (push (nth arg arg-vars) args))
	   ((equal type 'where-subcls)
	    (push (nth arg arg-vars) args))
	   
	   ((equal type 'e-text)
	    (push (list 'sql-embedded-text (list 'sql-embedded-text (nth arg arg-vars))) args))
	   ((equal type 'e-like-text)
	    (push (list 'sql-embedded-text (list 'sql-like-text (nth arg arg-vars))) args))
	   ((equal type 'e-select-list)
	    (push (list 'sql-embedded-text (nth arg arg-vars)) args))
	   ((equal type 'e-value-list)
	    (push (list 'sql-embedded-text (nth arg arg-vars)) args))
	   ((equal type 'e-where-subcls)
	    (push (list 'sql-embedded-text (nth arg arg-vars)) args))
	   (t
	    (error "unknown type: ~A" type)))
	  (format stream "~~a")

	  (setf i (1+ i)))
	 (t
	  (format stream "~a" (aref str i)))))
  (cons (cons (get-output-stream-string stream) (reverse args)) 
	(subseq arg-vars 0 arity))))

(defun get-$-args (arity)
    (loop
	for i from 1 to arity
	collect (format nil "$~a" (1- i))))

(defun get-explicit-type (str i)
  (let* ((j (1+ i))
	 (end-char-set '(#\Space #\Newline #\Return))
	 (type-str
	  (and (< (1+ j) (length str))
	       (eq (aref str j) #\:)
	       (not 
		 (member (aref str (1+ j)) end-char-set))
	       (subseq str (1+ j) (position-char-set end-char-set str :start j)))))
    type-str))

(defun position-char-set (char-set string &key (start 0))
  (loop
      for i from start to (1- (length string))
      do
	
      (if 
	  (member (aref string i) char-set)
	  (return-from position-char-set i)))
      nil)
      
(defun get-postgres-temp-filename nil
  (setf *postgres-temp-filename*
    (format nil "~a.~a" "/tmp/postgres-temp" (sys:user-name))))

(defun read-pgpass nil
  (with-open-file 
      (fstream
       "~/.pgpass"
       :direction :input
       :if-does-not-exist nil)
    (when fstream
      (loop
	  for line = (read-line fstream nil :eof)
	  while (not (eq line :eof))
	  collect line))))
