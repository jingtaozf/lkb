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

(defun string-2-mxd-list-on-spc (&rest rest)
  (mapcar #'str-to-mixed
   (apply #'string-2-str-list-on-spc rest)))

(defun string-2-str-list-on-spc (string &key (esc t))
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
	     ((and (eq #\\ c) esc)
	      (setf flag t))
	     (T
	      (push c word-chars)))
      finally (return (reverse (push (implode-from-chars (reverse word-chars)) res)))
	      ))
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

(defvar *rc-file* nil)
(defun rc (&optional file)
  (if file
      (setf *rc-file* file))
  (lkb::recomp *rc-file*))

(defun 2-symb-or-list (x)
  (if (and (stringp x) (eq (aref x 0) #\())
      (work-out-value 'list x)
    (2-symb x)))

(defun to-multi-csv-line (&key name base-name particle type keyrel)
  (let ((separator (string *postgres-export-separator*)))
    (format *postgres-export-multi-stream* "~a~%"
	    (concatenate 'string
	      name
	      separator base-name
	      separator particle
	      separator type
	      separator keyrel))
    ""))

(defun csv-line (&rest str-list)
  (str-list-2-str str-list
		  :sep-c *postgres-export-separator*
		  :null-str "?"))

