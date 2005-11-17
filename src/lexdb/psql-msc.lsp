;;; Copyright (c) 2002 - 2005
;;;   Ben Waldron, Ann Copestake, Fabre Lambeau, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

;;;
;;; Miscellaneous functions
;;;

(defun absolute-namestring (format str)
  (namestring (pathname (format nil format str))))

(defun normalize-orthkey (x)
  (string-downcase x))

(defun normalize-orthkey! (x)
  (nstring-downcase x))

(defun get-val (field raw-record cols)
  (let ((position (position field cols)))
    (unless position
      (error "internal error: ~a not found in ~a" field cols))
    (nth position raw-record)))

(defun string-2-mxd-list-on-spc (&rest rest)
  (mapcar #'str-to-mixed
   (apply #'string-2-str-list-on-spc rest)))

(defun string-2-str-list-on-spc (string &key (esc t))
  (string-2-str-list string :sep #\Space :esc esc))

(defun string-2-str-list (string &key (sep #\Space) (esc t))
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
	     ((eq sep c)
	      (push (implode-from-chars (reverse word-chars)) res)
	      (setf word-chars nil))
	     ((and (eq #\\ c) esc)
	      (setf flag t))
	     (T
	      (push c word-chars)))
      finally (return (reverse (push (implode-from-chars (reverse word-chars)) res)))))

;;;
;;; misc
;;;

(defun extract-param (param param-list)
  (second (assoc param param-list)))

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

#+:bmw20
(defun time-parse (str)
  (time
   (parse
    (split-into-words 
     (preprocess-sentence-string str)))))


;;;
;;;
;;;
#+:bmw20
(defvar *rc-file* nil)
#+:bmw20
(defun rc (&optional file)
  (if file
      (setf *rc-file* file))
  (lkb::recomp *rc-file*))

(defun 2-symb-or-list (x)
  (if (and (stringp x) (eq (aref x 0) #\())
      (work-out-rawlst x)
    (2-symb x)))

;;;
;;; misc
;;;

(defun get-assoc-val (x assoc-list)
  (cdr (assoc x assoc-list)))


;;;
;;; generate TDL code for MWE entries
;;;

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

;;
;; misc
;;

(defun ordered-symb-val-list (ordered-symb-list symb-val-list)
  (if (null ordered-symb-list) (error (format nil "non-null list expected")))
  (loop 
      while ordered-symb-list
      collect (assoc (pop ordered-symb-list) symb-val-list)))

(defun dot (a b)
  (unless (or (null b) (= (length a) (length b)))
    (error "unequal input list lengths"))
  (loop
      while a
      collect (cons (pop a) (pop b))))

(defun myremdup (list pred< pred=)
  (when list
    (loop
	with slist = (sort (copy-list list) pred<)
	with last = (not (car slist))
	for x in slist
	unless (funcall pred= x last)
	collect x
	do
	  (setf last x))))

(defun add-w-empty (val list-w-empty)
  (if (or (null list-w-empty) 
	  (equal :empty list-w-empty))
      (list val)
    (cons val list-w-empty)))