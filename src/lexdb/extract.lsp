;;; Copyright (c) 2001 -- 2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Ben Waldron;
;;;   see `licence.txt' for conditions.

;;;
;;; extract field values from lexical entries
;;;

(in-package :lkb)

;;;
;;; encode atoms
;;;

(defun encode-mixed-as-str (val)
  (cond
   ((null val)
    "")
   ((symbolp val)
    (let ((val-str (symb-2-str val)))
      (if (and (> (length val-str) 0)
	       (eq (aref val-str 0) #\"))
	  (format nil "\\~a" val-str)
	val-str)))
   ((stringp val)
    (format nil "\"~a\"" val))
   (t
    (error "unhandled type: ~a" val))))

(defun encode-string-as-str (val)
  (cond
   ((null val)
    "")
   ((stringp val)
    (format nil "\"~a\"" val))
   (t
    (error "unhandled type: ~a" val))))
;;;
;;; extract grammatical fields
;;;

(defun extract-key-from-unification (unification)
  (when (unification-p unification)
    (let ((lhs (unification-lhs unification)))
      (when (path-p lhs)
        (path-typed-feature-list lhs)))))

(defun extract-value-from-unification (unification)
  (when (unification-p unification)
    (let ((rhs (unification-rhs unification)))
      (when (u-value-p rhs)
        (u-value-type rhs)))))

(defun extract-field (x field-kw &optional fields-map)
  (unless *psql-lexicon*
    (error "*psql-lexicon* is unset"))
  (if (stringp field-kw)
      (setf field-kw (str-2-keyword field-kw)))
  (let* ((fields-map (or fields-map (fields-map *psql-lexicon*)))
	 (mapping (find field-kw fields-map :key #'second :test 'equal)))
    (if mapping
        (extract-field2 x (first mapping) (third mapping) (fourth mapping))
      "")))
	 
(defun extract-field2 (x key path type)
  (extract-value-by-path x key (get-path path) type))

;;;

(defun extract-rawlst-by-path (x key path)
  (mixed-list-2-str (extract-raw-list x key path)))

(defun extract-list-by-path (x key path)
  (extract-raw-list x key path))

(defun extract-mixed-by-path (x key path)
  (encode-mixed-as-str (extract-atom-by-path x key path)))

(defun extract-str-by-path (x key path)
  (encode-string-as-str (extract-atom-by-path x key path)))

(defun extract-sym-by-path (x key path)
  (symb-2-str (extract-atom-by-path x key path)))

(defun extract-mixed-rawlst-by-path (x key path)
  (mixed-list-2-str (extract-raw-list x key path)))

(defun extract-str-rawlst-by-path (x key path)
  (str-list-2-str (extract-raw-list x key path)))

(defun extract-str-lst-by-path (x key path)
  (str-list-2-str (extract-fs-list x key path)))

(defun extract-str-dlst-by-path (x key path)
  (str-list-2-str (extract-fs-diff-list x key path)))

(defun extract-lst-t-by-path (x key path &key e-path top)
  (mixed-list-2-str
   (extract-fs-list-complex x key path :e-path e-path :top top)))

(defun extract-lst-by-path (x key path &key e-path)
  (mixed-list-2-str
   (extract-fs-list-complex x key path :e-path e-path)))
   
(defun extract-dlst-t-by-path (x key path &key e-path top)
  (mixed-list-2-str 
   (extract-fs-diff-list-complex x key path :e-path e-path :top top)))

(defun extract-dlst-by-path (x key path &key e-path)
  (mixed-list-2-str 
   (extract-fs-diff-list-complex x key path :e-path e-path)))

;; see also work-out-value
(defun extract-value-by-path (x key path typel)
  (case (first typel)
    (rawlst 
     (extract-rawlst-by-path x key path))
    (list
     (extract-list-by-path x key path))
    (mixed
     (extract-mixed-by-path x key path))
    (str
     (extract-str-by-path x key path))
    (sym
     (extract-sym-by-path x key path))
    (mixed-rawlst
     (extract-mixed-rawlst-by-path x key path))   
    (str-rawlst
     (extract-str-rawlst-by-path x key path))
    (str-lst
     (extract-str-lst-by-path x key path))
    (str-dlst
     (extract-str-dlst-by-path x key path))
    (lst-t
     (extract-lst-t-by-path x key path :e-path (cddr typel) :top (second typel)))
    (lst 
     (extract-lst-by-path x key path :e-path (cdr typel)))	  
    (dlst-t
     (extract-dlst-t-by-path x key path :e-path (cddr typel) :top (second typel)))
    (dlst
     (extract-dlst-by-path x key path :e-path (cdr typel)))
    (t
     (error "~%unhandled field-map type: ~a" (first typel)))))

(defun extract-raw-list (x key path)
  (if (eq key :unifs) (error "Cannot extract raw list from unifs. Use fs-list instead."))
  (if path (error "path must be null"))
  (let ((val (cdr (assoc key x))))
    (unless (listp val)
      (error "Raw list expected"))
    val))
    
(defun extract-atom-by-path (x key path)
  (case key
    (:unifs
     (extract-atom-by-path-from-unifs x path))
    (t
     (if path
	 (error "path must be null")
       (cdr (assoc key x))))))
     
(defun extract-atom-by-path-from-unifs (x path)
  (let* ((constraint (cdr (assoc :unifs x)))
	 (unification (find path constraint
                            :key #'extract-key-from-unification 
                            :test #'equal)))
    (when (unification-p unification)
       (setf (cdr (assoc :unifs x))
	(remove unification constraint))
     (extract-value-from-unification unification))))

(defun extract-fs-list (x key path)
  (extract-fs-list-complex x key path))

(defun extract-fs-list-complex (x key path &key e-path (top '*))
  (let ((res (extract-fs-list-complex-aux (copy-list (cdr (assoc key x))) 
					  path
					  nil
					  :e-path e-path
					  :top top)))
    (cond
     ((listp res)
      (setf (cdr (assoc key x)) (cdr res))
      (car res))
     (t
      nil))))

(defun extract-fs-list-complex-aux (unifs path o-list &key e-path (top '*))
  (let* ((end-match (find path unifs
			  :key #'extract-key-from-unification
			  :test #'equal))
	 (first-match (find (append path (list 'first) e-path)
			    unifs
			    :key #'extract-key-from-unification
			    :test #'equal))
	 (top-match)
	 (val (extract-value-from-unification first-match)))
    (cond
     ((and end-match 
	   (eq (extract-value-from-unification end-match)
	       *empty-list-type*))
      (setf unifs (remove end-match unifs))
      (cons (reverse o-list) unifs))
     ((eq *toptype*
	  (extract-value-from-unification
	   (setf top-match
	     (find (append path (list 'first))
		   unifs
		   :key #'extract-key-from-unification
		   :test #'equal))))
      (setf unifs 
	(remove top-match
		unifs))
      (extract-fs-list-complex-aux 
       unifs 
       (append path (list 'REST))
       (cons top o-list)
       :e-path e-path))
     ((null val)
      :fail)
     (t
      (setf unifs (remove first-match unifs))
      (extract-fs-list-complex-aux 
       unifs 
       (append path (list 'REST))
       (cons val o-list)
       :e-path e-path)))))

(defun extract-fs-diff-list (x key path)
  (extract-fs-diff-list-complex x key path))
  
(defun extract-fs-diff-list-complex (x key path &key e-path (top '*))
  (let* ((unifs (copy-list (cdr (assoc key x))))
	 (last-match 
	  (find (append path (list 'LAST))
		 unifs
		 :key #'extract-key-from-unification
		 :test #'equal))
	 (last-path
	  (and last-match
	       (path-typed-feature-list
		(unification-rhs last-match))))
	 (res 
	  (and last-path
	       (extract-fs-diff-list-complex-aux 
		(remove last-match unifs)
		(append path (list 'LIST))
		nil
		:last last-path
		:e-path e-path
		:top top))))
    (cond
     ((null last-path)
      nil)
     ((not (listp res))
      nil)
     ((listp res)
      (setf (cdr (assoc key x)) (cdr res))
      (car res)))))

(defun extract-fs-diff-list-complex-aux (unifs path o-list &key last e-path (top '*))
  (let* ((first-match (find (append path (list 'first) e-path)
			    unifs
			    :key #'extract-key-from-unification
			    :test #'equal))
	 (top-match)
	 (val (extract-value-from-unification first-match)))
   (cond
    ((equal path
	    last)
     (cons (reverse o-list) unifs))
    ((eq *toptype*
	 (extract-value-from-unification
	  (setf top-match
	    (find (append path (list 'first))
		  unifs
		  :key #'extract-key-from-unification
		  :test #'equal))))
     (setf unifs 
       (remove top-match
	       unifs))
     (extract-fs-diff-list-complex-aux 
      unifs 
      (append path (list 'REST))
      (cons top o-list)
      :last last
      :e-path e-path))    
    ((null val)
     :fail)
    (t
     (setf unifs (remove first-match unifs))
     (extract-fs-diff-list-complex-aux 
      unifs 
      (append path (list 'REST))
      (cons val o-list)
      :last last
      :e-path e-path)))))

(defun get-path (path-str)
  (cond
   ((null path-str)
    nil)
   ((listp path-str)
    path-str)
   ((equal "" path-str)
    nil)
   ((stringp path-str)
    (work-out-rawlst path-str))
   (t
    (error "unhandled value: ~a" path-str))))

;; unused?
(defun get-orthkey (orth-list)
  (string-downcase 
   (or (car (last orth-list))
       "")))

;;;
;;; MWE stuff
;;;

#+:mwe
(defmethod multi-p (&key name type)
  (cond
   ((equal (subseq type 0 10) "idiomatic-")
    (multi-idiom-base-name name))
   ((equal (subseq-from-end type 12) "_particle_le")
    (multi-vpc-base-name name))
   (t
    nil)))

#+:mwe
(defun remove-sense-id-substr (name)
  (if (and (find #\_ name)
	   (numberp
	    (2-symb 
	     (subseq name (1+ (position #\_ name :from-end t))))))
      (subseq name 0 (position #\_ name :from-end t))
    name))

#+:mwe
(defun multi-idiom-base-name (name-full)
  (let (( name (remove-sense-id-substr name-full)))
    (cond
     ((equal (subseq name 0 2) "i_")
      (subseq name 2))
     (t
      (format t "WARNING: cannot generate base name for idiom ~a~%" name-full)
      (format nil "UNKNOWN_BASE_~a" name)))))

#+:mwe
(defun multi-vpc-base-name (name-full)
  (let ((name (remove-sense-id-substr name-full)))
    (cond
     ((and
       (not (equal (subseq name 0 1) "_"))
       (position #\_ name))
    (subseq name 0 (position #\_ name)))
     (t
      (format t "WARNING: cannot generate base name for vpc ~a~%" name-full)
      (format nil "UNKNOWN_BASE_~a" name)))))

;;;

(defun work-out-mixed (val)
  (unless (equal val "")
    (list (str-to-mixed val))))

(defun work-out-str (val)
  (unless (equal val "")
    (list (str-to-string val))))

(defun work-out-sym (val)
  (unless (equal val "")
    (list (str-2-symb val))))

(defun work-out-mixed-rawlst (val)
  (list (string-2-mxd-list-on-spc val)))

(defun work-out-str-rawlst (val)
  (list (string-2-str-list-on-spc val)))

(defun work-out-rawlst (val)
  (unless (equal val "")
    (str-2-list val)))

(defun work-out-str-lst (val)
  (unless (equal val "")
    (expand-string-list-to-fs-list
     (string-2-str-list-on-spc val))))

(defun work-out-str-dlst (val &key path)
  (unless (equal val "")
    (expand-string-list-to-fs-diff-list (string-2-str-list-on-spc val)
					:path path)))

(defun work-out-lst-t (val &key elt-path top)
  (unless (equal val "")
    (expand-string-list-to-fs-list-complex (string-2-mxd-list-on-spc val)
					   :top top
					   :elt-path elt-path))) 

(defun work-out-lst(val &key elt-path)
  (unless (equal val "")
    (expand-string-list-to-fs-list-complex (string-2-mxd-list-on-spc val)
					   :elt-path elt-path))) 

(defun work-out-dlst-t (val &key elt-path top path)
  (unless (equal val "")
    (expand-string-list-to-fs-diff-list-complex (string-2-mxd-list-on-spc val)
						:top top
						:elt-path elt-path
						:path path))) 

(defun work-out-dlst (val &key elt-path path)
  (unless (equal val "")
    (expand-string-list-to-fs-diff-list-complex (string-2-mxd-list-on-spc val)
						:elt-path elt-path
						:path path))) 

;; see also extract-value-by-path
;;; returns _list_ of values of appropriate type
(defun work-out-value (typel val &key path)
  (case (first typel)
    (mixed
     (work-out-mixed val))
    (str
     (work-out-str val))
    (sym
     (work-out-sym val))
    (mixed-rawlst
     (work-out-mixed-rawlst val))
    (str-rawlst
     (work-out-str-rawlst val))
    (rawlst
     (work-out-rawlst val))
    (str-lst
     (work-out-str-lst val))
    (str-dlst
     (work-out-str-dlst val :path path))     
    (lst-t
     (work-out-lst-t val :top (second typel) :elt-path (cddr typel)))
    (lst
     (work-out-lst val :elt-path (cdr typel)))
    (dlst-t
     (work-out-dlst-t val :top (second typel) :elt-path (cddr typel) :path path))
    (dlst
     (work-out-dlst val :elt-path (cdr typel) :path path))
    (t
     (error "~%unhandled field-map type: ~a" (first typel)))))

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
      (error "invalid format")))))

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
(defun expand-string-list-to-fs-list-complex (string-list &key elt-path (top '*))
  (cond
   ((equal string-list nil) 
    (list (list *empty-list-type*)))
   ((equal (first string-list)
	   top)
    (cons (append *list-head* 
		  elt-path
		  (list *toptype*)) 
	  (mapcar #'(lambda (x) (append *list-tail* x))
		  (expand-string-list-to-fs-list-complex (cdr string-list)
							 :elt-path elt-path))))
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
(defun expand-string-list-to-fs-diff-list-complex-aux (string-list &key path elt-path (top '*))
  (cond
   ((equal string-list nil) 
    (list 
     (list 
      (append path 
	      (list *diff-list-last*)))))
   ((equal (first string-list)
	top)
    (cons 
     (append *list-head*
	     (list *toptype*)) 
     (mapcar #'(lambda (x) (append *list-tail* x))
	     (expand-string-list-to-fs-diff-list-complex-aux (cdr string-list) 
							     :path path
							     :elt-path elt-path))))
   (t
    (cons 
     (append *list-head*
	     elt-path
	     (list (first string-list))) 
     (mapcar #'(lambda (x) (append *list-tail* x))
	     (expand-string-list-to-fs-diff-list-complex-aux (cdr string-list) 
							     :path path
							     :elt-path elt-path))))))   

