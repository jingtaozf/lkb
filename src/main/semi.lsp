;;; Copyright (c) 2003
;;;   Ben Waldron;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

(defvar *current-lex-id*) 
(defvar *obj-semi-lex-pred-n* 0) ;; counter must be reset at start of dump

(defmethod dump-obj-semi ((lexicon psql-lex-database))
  (with-open-file 
      (arg-stream
       (format nil "~asemi.obj.arg" *postgres-user-temp-dir*)
       :direction :output :if-exists :supersede)
    (with-open-file 
	(carg-stream
	 (format nil "~asemi.obj.carg" *postgres-user-temp-dir*)
	 :direction :output :if-exists :supersede)
      (with-open-file 
	  (docu-stream
	   (format nil "~asemi.obj.docu" *postgres-user-temp-dir*)
	   :direction :output :if-exists :supersede)
	(setf *obj-semi-lex-pred-n* 0)
	(mapc 
	 #'(lambda (x) (get-obj-semi-info x 
					  :arg-stream arg-stream
					  :carg-stream carg-stream
					  :docu-stream docu-stream))
	 (collect-psort-ids lexicon))))))
    

(defun get-obj-semi-info (lex-id &key (arg-stream t) (carg-stream t) (docu-stream t))
  (declare (ignore docu-stream)) ;;do later!!!
  (let* ((entry (get-lex-entry-from-id lex-id))
	 (dag (and
	       entry
	       (tdfs-indef (lex-entry-full-fs entry))))
	 (*current-lex-id* lex-id)
	 (cont-rels (dag-diff-list-2-list 
		     (dag-path-val '(synsem local cont rels) dag))))
    (loop
	for x in cont-rels
	do
	  (get-obj-semi-pred-info lex-id x 
				  :arg-stream arg-stream
				  :carg-stream carg-stream
				  )) 
    (unexpand-psort *lexicon* entry)))
    
(defun get-obj-semi-pred-info (lex-id dag &key (arg-stream t) (carg-stream t))
   (let* ((pred-str (2-str (dag-path-type '(pred) dag)))
	  (pred-fields (get-lex-pred-fields pred-str))
	  (carg (dag-path-type '(carg) dag))
	  (n 0)
	  (argN 'arg0)
	  (dagN))
     (setf *obj-semi-lex-pred-n* (1+ *obj-semi-lex-pred-n*))
     (loop
       ;; for arg0, arg1, etc.
	 while (setf dagN (dag-path-val (list argN) dag))
	 do
	   ;; predicate arg info table
	   (format arg-stream "~a~%"
		   (tsv-line 
		    (append
		     (list
		      *obj-semi-lex-pred-n* ;; counter
		      lex-id ;; lex-id
		      pred-str ;; pred
		      (nth 0 pred-fields) ;; lexeme
		      (nth 1 pred-fields) ;; pos
		      (nth 2 pred-fields) ;; sense
		      n ;;arg
		      (dag-path-type (list) dagN) ;;type
		      )
		     (dag-2-txts (dag-path-val '(e) dagN)) ;; move to separate table!!!
		     (dag-2-txts (dag-path-val '(png) dagN)))
		    ))	
	   (when carg
	     ;; carg table
	     (format carg-stream "~a~%"
		     (tsv-line 
		      (list
		       *obj-semi-lex-pred-n* ;; counter
		       lex-id ;; lex-id
		       pred-str ;; pred
		       (dag-path-type '(carg) dag) ;; carg
		       ))))
	   (setf n (1+ n))
	   (setf argN (str-2-symb (format nil "arg~a" n))))))

(defun get-lex-pred-fields (pred-str)
  (when (eq (aref pred-str 0) 
	    #\_)
    (let ((split-pred (split-on-char pred-str #\_)))
      (cond
       ((= (length split-pred) 4)
	(subseq split-pred 1 3))
       (t
	(subseq split-pred 1 4))))))

;;;
;;; dag access
;;;

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

;;;
;;; fields from dag leaves
;;;

(defun dag-2-txts (dag)
  (fields-2-txts (dag-2-fields dag)))

(defun fields-2-txts (fields)
  (mapcar #'(lambda (x)
	      (format nil "~a=~a" (car x) (cdr x)))
	  fields))

(defun dag-2-fields (dag &optional path)
  (if (null dag)
      (return-from dag-2-fields))
  (let ((feats (top-level-features-of dag)))
    (cond
     (feats
      (mapcan
       #'(lambda (x) (dag-2-fields (dag-path-val (list x) dag) (cons x path)))
       feats))
     (t
      (list (cons (str-2-symb (str-list-2-str (mapcar #'symb-2-str (reverse path)) "."))
		  (dag-path-type nil dag)))))))
      
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
	finally
	  (return out-list)
	  )))

;;;
;;; tsv text format
;;;

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
