;;; Copyright (c) 2002-2003 
;;;   Ann Copestake, Fabre Lambeau, Stephan Oepen, Benjamin Waldron;
;;;   see `licence.txt' for conditions.


(in-package :lkb)

;;;
;;; Miscellaneous functions
;;;

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

(defun string-2-str-list-on-spc (string)
  (loop
      with res
      with flag
      with word-chars
      for c in (explode-to-chars string)
      if flag do
	(push c word-chars)
	(setf flag nil)
      else do
	   (cond
	     ((eq #\Space c)
	      (push (implode-from-chars (reverse word-chars)) res)
	      (setf word-chars nil))
	     ((eq #\\ c)
		 (setf flag t))
	     (T
	      (push c word-chars)))
      finally (return (reverse (push (implode-from-chars (reverse word-chars)) res)))
	      ))

;; see also extract-value-by-path
;;; returns _list_ of values of appropriate type
(defun work-out-value (type value &key path)
  (case type
    ('mixed
     (unless (equal value "")
       (list (str-to-mixed value))))
    ('string
     (unless (equal value "")
       (list (str-to-string value))))
    ('symbol 
     (unless (equal value "")
       (list (str-2-symb value))))
    ('string-list
     (list (string-2-str-list-on-spc value)))
    ('list 
     (unless (equal value "")
       (str-2-list value) ))
    ('string-fs
     (expand-string-list-to-fs-list 
      (string-2-str-list-on-spc value)))
    ('string-diff-fs
     (expand-string-list-to-fs-diff-list 
      (string-2-str-list-on-spc value) :path path))
    (T
     (typecase type
       (list
	(case (first type)
	  ('mixed-fs
	   (expand-string-list-to-fs-list-complex 
	    (string-2-str-list-on-spc value)
	    :elt-path (cdr type)))
	  ('mixed-diff-fs
	   (expand-string-list-to-fs-diff-list-complex 
	    (string-2-str-list-on-spc value)
	    :path path
	    :elt-path (cdr type)))
	  (t
	   (error "unhandled (list) type: ~a" (first type)))))
       (T3 
	(error "unhandled type"))))))


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
    (list (list *empty-list-type*)))
   (t
    (cons (append *list-head* (list (first string-list))) 
	  (mapcar #'(lambda (x) (append *list-tail* x))
		  (expand-string-list-to-fs-list (cdr string-list)))))))   

;;; eg. ("w1" "w2") (A B)-> ((FIRST A B "w1") (REST FIRST A B "w2") (REST REST *NULL*)) 
(defun expand-string-list-to-fs-list-complex (string-list &key elt-path)
  (cond
   ((equal string-list nil) 
    (list (list *empty-list-type*)))
   (t
    (cons (append *list-head* 
		  elt-path
		  (list (first string-list))) 
	  (mapcar #'(lambda (x) (append *list-tail* x))
		  (expand-string-list-to-fs-list-complex (cdr string-list)
							 :elt-path elt-path))))))   

;;; eg. ("w1" "w2") path -> ((LIST FIRST "w1") (LIST REST FIRST "w2") (LIST REST REST path)) 
(defun expand-string-list-to-fs-diff-list (string-list &key path)
   (mapcar #'(lambda (x) (cons *diff-list-list* x))
	   (expand-string-list-to-fs-diff-list-aux string-list :path path)))

;;; eg. ("w1" "w2") path -> ((FIRST "w1") (REST FIRST "w2") (REST REST path)) 
(defun expand-string-list-to-fs-diff-list-aux (string-list &key path)
  (cond
   ((equal string-list nil) 
    (list 
     (list 
      (append path 
	      (list *diff-list-last*)))))
   (t
    (cons (append *list-head* (list (first string-list))) 
	  (mapcar #'(lambda (x) (append *list-tail* x))
		  (expand-string-list-to-fs-diff-list-aux (cdr string-list) :path path))))))   

;;; eg. ("w1" "w2") path (A B)-> ((LIST FIRST A B "w1") (LIST REST FIRST A B "w2") (LIST REST REST path)) 
(defun expand-string-list-to-fs-diff-list-complex (string-list &key path elt-path)
   (mapcar #'(lambda (x) (cons *diff-list-list* x))
	   (expand-string-list-to-fs-diff-list-complex-aux string-list 
							   :path path
							   :elt-path elt-path)))

;;; eg. ("w1" "w2") path (A B) -> ((FIRST A B "w1") (REST FIRST A B "w2") (REST REST path)) 
(defun expand-string-list-to-fs-diff-list-complex-aux (string-list &key path elt-path)
  (cond
   ((equal string-list nil) 
    (list 
     (list 
      (append path 
	      (list *diff-list-last*)))))
   (t
    (cons 
     (append *list-head*
	     elt-path
	     (list (first string-list))) 
     (mapcar #'(lambda (x) (append *list-tail* x))
	     (expand-string-list-to-fs-diff-list-complex-aux (cdr string-list) 
							     :path path
							     :elt-path elt-path))))))   


(defun sql-embedded-text (str)
  (format nil "'~a'" (sql-embedded-text-aux str)))

(defun sql-embedded-text-aux (str)
  (cond
   ((equal str "")
    "")
   ((eq (char str 0) #\')
    (format nil "\\'~a" (sql-embedded-text-aux (subseq str 1))))
   ((eq (char str 0) #\\)
    (format nil "\\\\~a" (sql-embedded-text-aux (subseq str 1))))
   (t
    (format nil "~a~a" (char str 0) (sql-embedded-text-aux (subseq str 1))))))

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

(defun split-on-char (string &optional (char #\Space))
  (loop for i = 0 then (1+ j)
      as j = (position char string :start i)
      collect (subseq string i j)
      while j))

(defun mixed-list-2-str (str-list &optional (separator " "))
  (unless (listp str-list)
    (error "list expected"))
  (cond
   ((null str-list) "")
   (t (apply 'concatenate
	     (cons
	      'string
	      (cons
	       (escape-char #\Space (encode-mixed-as-str (pop str-list)))
	       (mapcan #'(lambda (x) 
			   (list separator 
				 (escape-char #\Space (encode-mixed-as-str x))))
		       str-list)))))))

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


;;;
;;;
;;;

(defun duplicates (l &key (test #'equal) (key #'identity))
  (let ((out))
    (loop
        for x in l
        with x-key
        with prev
        with prev-key
        with dup-set
        do
          (setf x-key (apply key (list x)))
          (setf prev-key (apply key (list prev)))
          (cond
           ((apply test (list x-key prev-key))
            (unless dup-set (setf dup-set (list prev)))
            (push x dup-set))
           (t
            (if dup-set
                (push dup-set out))
            (setf dup-set nil)))
          (setf prev x)
        finally
          (if dup-set
              (push dup-set out)))
    out))

(defun join-tdl (x &key (stream nil))
  (format stream "~a := ~a~%" (car x) (cdr x)))

(defun display-tdl-duplicates (lexicon)
  (let* ((tdl-lex
          (mapcar #'(lambda (x) 
		      (let* ((x (read-psort lexicon x :cache nil))
			     (val-dot-body
			      (cons (tdl-val-str (lex-entry-id x))
				    (to-tdl-body x))))
			(forget-psort lexicon x)
			val-dot-body))
                  (collect-psort-ids lexicon)))
         (tdl-lex-sort 
          (sort tdl-lex #'string< :key #'cdr))
         (tdl-lex-dup
          (duplicates  
           tdl-lex-sort 
           :key #'cdr 
           :test #'string=)))
    (loop
        for dup-set in tdl-lex-dup
        do
          (format t "~%")
          (loop
              for x in dup-set
              do
                (join-tdl x :stream t)
                ))))

(defvar *rc-file* nil)
(defun rc (&optional file)
  (if file
      (setf *rc-file* file))
  (lkb::recomp *rc-file*))
