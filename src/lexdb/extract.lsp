;;; Copyright (c) 2001 -- 2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Benjamin Waldron;
;;   see `licence.txt' for conditions.

;;;
;;; extract field values from lexical entries
;;;

(in-package :lkb)

(defmethod copy-slots ((x lex-entry) fields-map)
  "copy slots for use in destructive operations"
  (let* ((slot-names
	  (remove-duplicates
	   (mapcar 
	    #'first
	    fields-map)))
	 (slots
	  (mapcar 
	   #'(lambda (s)
	       (cons
		s
		(copy-tree 
		 (slot-value 
		  x
		  (un-keyword s)))))
	   slot-names)))
    slots))

;;;
;;; MWE stuff
;;;

(defmethod multi-p (&key name type)
  (cond
   ((equal (subseq type 0 10) "idiomatic-")
    (multi-idiom-base-name name))
   ((equal (subseq-from-end type 12) "_particle_le")
    (multi-vpc-base-name name))
   (t
    nil)))

(defun remove-sense-id-substr (name)
  (if (and (find #\_ name)
	   (numberp
	    (2-symb 
	     (subseq name (1+ (position #\_ name :from-end t))))))
      (subseq name 0 (position #\_ name :from-end t))
    name))

(defun multi-idiom-base-name (name-full)
  (let (( name (remove-sense-id-substr name-full)))
    (cond
     ((equal (subseq name 0 2) "i_")
      (subseq name 2))
     (t
      (format t "WARNING: cannot generate base name for idiom ~a~%" name-full)
      (format nil "UNKNOWN_BASE_~a" name)))))

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
  (if (stringp field-kw)
      (setf field-kw (str-2-keyword field-kw)))
  (let* ((fields-map (or fields-map (fields-map *psql-lexicon*)))
	 (mapping (find field-kw fields-map :key #'second :test 'equal)))
    (if mapping
        (extract-field2 x (first mapping) (third mapping) (fourth mapping))
      "")))
	 
(defun extract-field2 (x key path2 type)
  (let* ((path (get-path path2)))
    (extract-value-by-path x key path type)))

;; see also work-out-value
(defun extract-value-by-path (x key path type)
  (case type
    ;; todo: move LIST to separate fn
    ('list
     (extract-raw-list x key path))
    
    (nil
     (encode-mixed-as-str 
      (extract-atom-by-path x key path)))
    ('mixed
     (encode-mixed-as-str 
      (extract-atom-by-path x key path)))
    
    ('str
     (encode-string-as-str
      (extract-atom-by-path x key path)))
    ('string
     (encode-string-as-str
      (extract-atom-by-path x key path)))
    
    ('sym
     (symb-2-str
      (extract-atom-by-path x key path)))
    ('symbol
     (symb-2-str
      (extract-atom-by-path x key path)))
    
    ('str-rawlst
     (str-list-2-str
      (extract-raw-list x key path)))
    ('string-list
     (str-list-2-str
      (extract-raw-list x key path)))
    
    ('rawlst
     (mixed-list-2-str
      (extract-raw-list x key path)))
    
    ('str-lst
     (str-list-2-str (extract-fs-list x key path)))
    ('string-fs
     (str-list-2-str (extract-fs-list x key path)))
    
    ('str-dlst
     (str-list-2-str (extract-fs-diff-list x key path)))
    ('string-diff-fs
     (str-list-2-str (extract-fs-diff-list x key path)))
    
    (T
     (typecase type
       (list
	(case (first type)
	  ('lst
	   (mixed-list-2-str 
	    (extract-fs-list-complex x key path 
				     :e-path (cdr type))))
	  ('mixed-fs
	   (mixed-list-2-str 
	    (extract-fs-list-complex x key path 
				     :e-path (cdr type))))
	  
	  ('dlst
	   (mixed-list-2-str 
	    (extract-fs-diff-list-complex x key path 
					  :e-path (cdr type))))
	  ('mixed-diff-fs
	   (mixed-list-2-str 
	    (extract-fs-diff-list-complex x key path 
					  :e-path (cdr type))))
	  
	  (t
	   (error "unhandled complex type: ~a" (first type)))))
       (T 
	(error "unhandled type"))))))
    
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
     (extract-value-from-unification unification)
      )))

(defun extract-fs-list (x key path)
  (extract-fs-list-complex x key path))

(defun extract-fs-list-complex (x key path &key e-path)
  (let ((res (extract-fs-list-complex-aux 
	       (copy-list (cdr (assoc key x))) 
	       path
	       nil
	       :e-path e-path)))
    (cond
     ((listp res)
      (setf (cdr (assoc key x)) (cdr res))
      (car res))
     (t
      nil))))

(defun extract-fs-list-complex-aux (unifs path o-list &key e-path)
  (let* (
	 (end-match (find path
			  unifs
			  :key #'extract-key-from-unification
			  :test #'equal))
	 (first-match (find (append path (list 'first) e-path)
			    unifs
			    :key #'extract-key-from-unification
			    :test #'equal))
	 (val (extract-value-from-unification first-match)))
    (cond
     ((and end-match 
	   (eq (extract-value-from-unification end-match)
	       *empty-list-type*))
      (setf unifs (remove end-match unifs))
      (cons (reverse o-list) unifs))
     ((null val)
      :fail)
     (t
      (setf unifs (remove first-match unifs))
      (extract-fs-list-complex-aux unifs 
			   (append path (list 'REST))
			   (cons val o-list)
			   :e-path e-path)))))

(defun extract-fs-diff-list (x key path)
  (extract-fs-diff-list-complex x key path))
  
(defun extract-fs-diff-list-complex (x key path &key e-path)
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
		:e-path e-path))))
    (cond
     ((null last-path)
      nil)
     ((not (listp res))
      nil)
     ((listp res)
      (setf (cdr (assoc key x)) (cdr res))
      (car res)))))

(defun extract-fs-diff-list-complex-aux (unifs path o-list &key last e-path)
  (let* ((first-match (find (append path (list 'first) e-path)
			    unifs
			    :key #'extract-key-from-unification
			    :test #'equal))
	 (val (extract-value-from-unification first-match)))
   (cond
    ((equal path
	    last)
     (cons (reverse o-list) unifs))
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
    (work-out-value 'list path-str))
   (t
    (error "unhandled value: ~a" path-str))))

(defun get-orthkey (orth-list)
  (string-downcase 
   (or (car (last orth-list))
       "")))

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

