;;; Copyright (c) 2002-2003 
;;;   Ann Copestake, Fabre Lambeau, Stephan Oepen, Benjamin Waldron;
;;;   see `licence.txt' for conditions.


(in-package :lkb)

;;;
;;; Miscellaneous functions
;;;

;;;
;;; format conversion
;;;

(defun str-2-symb (str)
  (unless (stringp str)
    (error "string exected"))
  (intern (string-upcase str) :lkb))

(defun str-2-keyword (str)
  (unless (stringp str)
    (error "string exected"))
  (intern (string-upcase str) :keyword))

;; currently 'str-2-lisp-object' ...
(defun str-2-list (str)
  (with-package (:lkb)
    (let ((item (read-from-string str)))
      (cond
       ((listp item)
	item)
       (t
	(error "list expected"))))))

;; use (parse-integer X :junk-allowed t) for integers
(defun str-2-num (str &optional default)
  (with-package (:lkb)
    (let ((item (read-from-string str)))
      (cond
       ((numberp item)
	item)
       ((eq default t)
	(error "number expected"))
       (t default)))))

(defun str-2-numstr (str &optional default)
  (let ((num (str-2-num str)))
    (cond
     ((numberp num) 
      (format nil "~a" num))
     ((eq default t)
      (error "number expected"))
     (t default))))

(defun str-list-2-str (str-list &optional (separator " "))
  (unless (listp str-list)
    (error "list expected"))
  (cond
   ((null str-list) "")
   (t (apply 'concatenate
	     (cons
	      'string
	      (cons
	       (pop str-list)
	       (mapcan #'(lambda (x) (list separator x)) str-list)))))))
  
(defun symb-2-str (symb)
  (unless (symbolp symb)
    (error "symbol expected"))
  (cond
   ((null symb) "")
   (t (string-downcase (string symb)))))

(defun num-2-str (num)
  (if (null num)
      (return-from num-2-str))
  (unless (numberp num)
    (error "number expected"))
  (format nil "~a" num))
  
(defun char-2-symb (c)
  (unless (characterp c)
    (error "character expected"))
  (str-2-symb (string c)))

(defun char-2-num (c)
  (unless (characterp c)
    (error "character expected"))
  (str-2-num (string c)))

(defun 2-symb (x)
  (cond
   ((symbolp x) x)
   ((stringp x) (str-2-symb x))
   (t (error "unhandled type"))))

(defun 2-str (x)
  (cond
   ((stringp x) x)
   ((symbolp x) (symb-2-str x))
   ((numberp x) (num-2-str x))
   (t (error "unhandled type"))))

(defun get-val (field record)
  (cdr (assoc field record :test #'equal)))

(defun sql-escape-string (string)
  (if (and string (stringp string))
      (loop
          with padding = 128
          with length = (+ (length string) padding)
          with result = (make-array length
                                    :element-type 'character
                                    :adjustable nil :fill-pointer 0)
          for c across string
          when (char= c #\') do
            (vector-push #\\ result)
            (vector-push c result)
            (when (zerop (decf padding))              (setf padding 42)
              (incf length padding)
              (setf result (adjust-array result length)))
          else do
            (vector-push c result)
          finally
            (return result))
    string))

;;; prepare field list for SQL INSERT INTO query
(defun sql-field-list-str (symb-list)
  (concatenate 'string "(" (sql-select-list-str symb-list) ")"))
  
;;; prepare select list for SQL query
(defun sql-select-list-str (symb-list)
  (if (null symb-list) (error (format nil "non-null list expected")))
  (let ((stream (make-string-output-stream)))
    (format stream "~a" (symb-2-str (pop symb-list)))
    (loop 
	while symb-list
	do 
	  (format stream ",~a" (symb-2-str (pop symb-list))))
    (get-output-stream-string stream)))

;;; prepare val list for SQL INSERT INTO query
(defun sql-val-list-str (symb-list psql-le)
  (if (null symb-list) (error (format nil "non-null list expected")))
  (let ((stream (make-string-output-stream)))
    (format stream "~a" (make-sql-val-str 
			 (retr-val psql-le (pop symb-list))))
    (loop 
	while symb-list
	do 
	  (format stream ",~a" (make-sql-val-str 
				(retr-val psql-le (pop symb-list)))))
    (get-output-stream-string stream)))

;;; create val string for SQL query
(defun make-sql-val-str (x)
  (cond 
   ((null x)
    "")
   ((listp x)
    (format nil "'~a'" (sql-escape-string (str-list-2-str x))))
   ((stringp x)
    (format nil "'~a'" (sql-escape-string x)))
   ((numberp x)
    (format nil "~a" x))
   ((symbolp x)
    (format nil "~a" x))
   (t
    (error (format nil "unhandled data type")))))

(defun record-id (record)
  (str-2-symb (cdr (assoc :name record))))

(defun record-orth (record)
  (cdr (assoc :orthography record)))

(defun orth-string-to-str-list (string)
  ;;
  ;; break orthography string returned from DB at (one or more) spaces
  ;;
  (unless (stringp string)
    (error "string exected"))

  (loop
      with result = nil
      with word = (make-array 42
                              :element-type 'character
                              :adjustable t :fill-pointer 0)
      with stream = (make-string-input-stream string)
      for c = (read-char stream nil nil)
      while c
      when (and (eql c #\space) (not (zerop (length word)))) do
        (push (copy-seq word) result)
        (setf (fill-pointer word) 0)
      when (not (eql c #\space)) do
        (vector-push-extend c word)
      finally
        (when (not (zerop (length word))) (push word result))
        (return (nreverse result))))

;;; returns _list_ of values of appropriate type
(defun work-out-value (type value &key path)
  (cond ((equal type "symbol") 
	 (unless (equal value "")
	   (list (str-2-symb value))))
	((equal type "string")
	 (unless (equal value "")
	   (list (str-to-string value))))
	((equal type "mixed")
	 (unless (equal value "")
	   (list (str-to-mixed value))))
	((equal type "string-list")
	 (list (orth-string-to-str-list value)))
	((equal type "string-fs")
	 (expand-string-list-to-fs-list (orth-string-to-str-list value)))
	((equal type "string-diff-fs")
	 (expand-string-list-to-fs-diff-list (orth-string-to-str-list value) :path path))
	((equal type "list") (unless (equal value "")
			       (str-2-list value) ))
	(t (error "unhandled type during database access"))))

(defun str-to-mixed (val-str)
  (let ((len (length val-str)))
    (cond 
     ((eq (aref val-str 0) #\")
      (unless (eq (aref val-str (1- len)) #\")
	(error "STRING val must be of form \\\"STR\\\""))
      (subseq val-str 1 (1- len)))
     ((and (eq (aref val-str 0) #\\)
	  (eq (aref val-str 1) #\"))
      (str-2-symb (format nil "\"~a" (subseq val-str 2 len))))
     (t
      (str-2-symb val-str)))))

(defun str-to-string (val-str)
  (let ((len (length val-str)))
    (cond 
     ((eq (aref val-str 0) #\")
      (unless (eq (aref val-str (1- len)) #\")
	(error "STRING val must be of form \\\"STR\\\""))
      (subseq val-str 1 (1- len)))
     (t
      (error "bad format")))))

;;; eg. ("w1" "w2") -> ((FIRST "w1") (REST FIRST "w2") (REST REST *NULL*)) 
(defun expand-string-list-to-fs-list (string-list)
  (cond
   ((equal string-list nil) 
    (list (list '*NULL*)))
   (t
    (cons (list 'FIRST (first string-list)) 
	  (mapcar #'(lambda (x) (cons 'REST x))
	  (expand-string-list-to-fs-list (cdr string-list)))))))   

;;; eg. ("w1" "w2") path -> ((LIST FIRST "w1") (LIST REST FIRST "w2") (LIST REST REST path)) 
(defun expand-string-list-to-fs-diff-list (string-list &key path)
   (mapcar #'(lambda (x) (cons 'LIST x))
	   (expand-string-list-to-fs-diff-list-aux string-list :path path)))

;;; eg. ("w1" "w2") path -> ((FIRST "w1") (REST FIRST "w2") (REST REST path)) 
(defun expand-string-list-to-fs-diff-list-aux (string-list &key path)
  (cond
   ((equal string-list nil) 
    (list 
     (list 
      (append (work-out-value "list" path) 
	      (list 'LAST)))))
   (t
    (cons (list 'FIRST (first string-list)) 
	  (mapcar #'(lambda (x) (cons 'REST x))
		  (expand-string-list-to-fs-diff-list-aux (cdr string-list) :path path))))))   

(defun sql-embedded-text (str)
  (cond
   ((equal str "")
    "")
   ((eq (char str 0) #\')
    (format nil "''~a" (sql-embedded-text (subseq str 1))))
   (t
    (format nil "~a~a" (char str 0) (sql-embedded-text (subseq str 1))))))

(defun sql-like-text (str)
  (format nil "'~a'" (sql-like-text-aux str))
  )

(defun sql-like-text-aux (str)
  (cond
   ((equal str "")
    "")
   ((eq (char str 0) #\')
    (format nil "''~a" (sql-like-text-aux (subseq str 1))))
   ((eq (char str 0) #\_)
    (format nil "\\\\_~a" (sql-like-text-aux (subseq str 1))))
   ((eq (char str 0) #\%)
    (format nil "\\\\%~a" (sql-like-text-aux (subseq str 1))))
   ((eq (char str 0) #\\)
    (format nil "\\\\\\\\~a" (sql-like-text-aux (subseq str 1))))
   (t
    (format nil "~a~a" (char str 0) (sql-like-text-aux (subseq str 1))))))

;;;
;;; misc
;;;

(defun extract-param (param param-list)
  (second (assoc param param-list)))

(defun kwl2alist (l)
  (loop
      while l
      collect (let ((kw (pop l))
		    (v (pop l)))
		(unless (keywordp kw)
		  (error "kwl2alist input format"))
		(cons kw v))))

(defun un-keyword (keyword-symb)
  (str-2-symb (symb-2-str keyword-symb)))

(defun recomp (x)
  (compile-file x)
  (load x))

(defun split-on-char (string &optional (char #\Space))
  (loop for i = 0 then (1+ j)
      as j = (position char string :start i)
      collect (subseq string i j)
      while j))

;;;
;;; lexport
;;;

(defun subseq-from-end (seq rev-end &optional (rev-start 0))
  (let* ((len (length seq))
	 (start (- len rev-end))
	 (end (- len rev-start)))
    (subseq seq start end)))

;;;
;;; temp
;;;

(defun time-parse (str)
  (time
   (parse
    (split-into-words 
     (preprocess-sentence-string str)))))

