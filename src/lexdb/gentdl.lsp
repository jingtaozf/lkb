;;; Copyright (c) 2001 -- 2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Ben Waldron;
;;;   see `licence.txt' for conditions.

;;;
;;; generate tdl code for lexical entry
;;;

(in-package :lkb)

;;;
;;; tdl export (packed)
;;;

(defun tdl-val-str (symb)
  (cond
   ((null symb) "")
   ((numberp symb) (num-2-str symb))
   ((stringp symb) (format nil "~S" symb))
   (t (string-downcase (string symb)))))
    
(defun pack-unifs (unifs)
  (pack (unifs-2-list unifs)))

(defun unifs-2-list (unifs)
  (let ((c 0)
	(coindex nil)
	(coindex-map)
	(match))
    (mapcan 
     #'(lambda (unif)
	 (with-slots (rhs lhs) unif
	   (cond
	    ((typep rhs 'U-VALUE)
	     (list (append (path-typed-feature-list lhs)
			   (u-value-type rhs))))
	    ((typep rhs 'PATH)
	     (setf match (assoc lhs coindex-map :test #'equalp))
	     (cond 
	      (match
	       (setf coindex (str-2-symb (format nil "#~a" (cdr match))))
	       (list (append (path-typed-feature-list rhs)
			     coindex)))
	      (t
	       (incf c)
	       (push (cons lhs c) coindex-map)
	       (setf coindex (str-2-symb (format nil "#~a" c)))
	       (list
		(append (path-typed-feature-list lhs)
			coindex)
		(append (path-typed-feature-list rhs)
			coindex))))))))
     unifs)))

(defun unif-2-lists (unif)
  (with-slots (rhs lhs) unif
    (cond
     ((typep rhs 'U-VALUE)
      (list (append (path-typed-feature-list lhs)
		    (u-value-type rhs))))
     ((typep rhs 'PATH)
      (list
       (append (path-typed-feature-list lhs)
	       '\#1)
       (append (path-typed-feature-list rhs)
	       '\#1))))))

(defun packed-extract-nonterminal (path packed)
  (packed-extract-aux path packed :terminal nil))

(defun packed-extract-terminal (path packed)
  (packed-extract-aux path packed :terminal t))

(defun packed-extract-aux (path packed &key terminal)
  (cond
   (path
    (packed-extract-nonterminal
     (cdr path)
     (cdr (car (member (car path) packed :key #'(lambda (x) (and (car x))))))))
   (terminal
    (mapcan #'(lambda (x) (and (not (cdr x)) (list x))) packed))
   (t
    (mapcan #'(lambda (x) (and (cdr x) (list x))) packed))))

(defun pack (l2)
  (loop
      for x in l2
      with p
      do
	(if (atom x)
	    (push (list x) p)
	  (if (assoc (car x) p)
	      (push (cdr x) (cdr (assoc (car x) p)))
	    (push (cons (car x) (list (cdr x))) p)))
      finally 
	(return
	  (mapcar
	   (lambda (x)
	     (if (atom x)
		 x
	       (cons (car x) (pack (cdr x)))))
	     (sort p #'pack-order)))))

;; list components ordered according to their printed representation
;; non-list components come first (non-deterministic ordering)
(defun pack-order (x y)
  (let ((a (pack-order-str x))
	(b (pack-order-str y)))
      (string< a b)))

(defun pack-order-str (x)
  (cond
   ((cdr x)
    (string (car x)))
   (t
    "")))

;; copy of p-2-tdl-2 w/o root
(defun p-2-tdl (branches)
  (unless branches
    (error "non-null value expected"))
  (let* ((a-branch-flag (not (cdr (first branches))))
	 (a-branches)
	 (len)
	 (i 0))
    (when a-branch-flag
      (do ()
	  ((or (null branches) (cdr (first branches))))
	(push (pop branches) a-branches)))
    (setf len (length branches))
    
     (cond
      ((and a-branch-flag (= len 0))
       (str-list-2-str-by-str (mapcar #'(lambda (x) (tdl-val-str (car x)))
			       a-branches)
		       " & "))
      (a-branch-flag
       (format nil "~a &~%~a ~a"  
	       (str-list-2-str-by-str (mapcar #'(lambda (x) (tdl-val-str (car x)))
				       a-branches)
			       " & ")
	       (make-string i :initial-element #\ )
	       (p-2-tdl-aux (+ i 3) branches)))
      ((= len 1)
       (format nil "~a" (p-2-tdl-2 i (first branches))))
      (t
       (format nil "~a" 
	       (p-2-tdl-aux i branches))))))

(defun p-2-tdl-2 (i p)
  (unless p
    (error "internal"))
  (let* ((root (car p))
	 (branches (cdr p))
	 (a-branch-flag (not (cdr (first branches))))
	 (a-branches)
	 (len))
  (setf i (+ i 3 (length (string root))))
    (when a-branch-flag
      (do ()
	  ((or (null branches) (cdr (first branches))))
	(push (pop branches) a-branches)))
    (setf len (length branches))
    
    (setf len (length branches))
     (cond
      ((and a-branch-flag (= len 0))
       (format nil "~a ~a" (string root)
	       (str-list-2-str-by-str (mapcar #'(lambda (x) (tdl-val-str (car x)))
				       a-branches)
			       " & ")))
      (a-branch-flag
       (format nil "~a ~a & ~a" 
	       (string root) 
	       (str-list-2-str-by-str (mapcar #'(lambda (x) (tdl-val-str (car x)))
				       a-branches)
			       " & ")	       
	       (p-2-tdl-aux i branches)))
      ((= len 1)
       (format nil "~a.~a" (string root) (p-2-tdl-2 i (first branches))))
      (t
       (format nil "~a ~a" 
	       (string root) 
	       (p-2-tdl-aux i branches))))))

(defun p-2-tdl-aux (i branches)
  (let ((res))
    (cond
     ((and (setf res (get-tdl-list branches))
	   (every #'(lambda (x) (= (length x) 1)) res))
      (format nil "< ~a >"
	      (str-list-2-str-by-str
	       (mapcar (lambda (x) (p-2-tdl-2-in-list i (car x))) res)
	       ", ")))
     ((and (setf res (get-tdl-diff-list branches))
	   (every #'(lambda (x) (= (length x) 1)) res))
      (format nil "<! ~a !>"
	      (str-list-2-str-by-str
	       (mapcar (lambda (x) (p-2-tdl-2-in-list i (car x))) res)
	       ", ")))
     (t
      (format nil "[ ~a ]"
	      (str-list-2-str-by-str
	       (mapcar (lambda (x) (p-2-tdl-2 i x)) branches)
	       (format nil ",~%~a" (make-string i :initial-element #\ ))))))))

(defun p-2-tdl-2-in-list (i x)
  (if (> (length x) 1)
      (format nil "[ ~a ]" (p-2-tdl-2 i x))
    (tdl-val-str (car x))))


(defun tdl-list-start-p (branches)
    (and
     (= (length branches) 2)
     (find (CAR *LIST-HEAD*) branches :key 'car)
     (find (CAR *LIST-TAIL*) branches :key 'car)
     *empty-list-type*))

(defun tdl-diff-list-start-p (branches)
  (let ((blast))
    (and
     (= (length branches) 2)
     (find *diff-list-list* branches :key 'car)
     (setf blast (find *diff-list-last* branches :key 'car))
     (= (length blast) 2)
     (coindex-p (car (second blast)))
     (car (second blast)))))

(defun get-tdl-list (branches)
  (let* ((bfirst (find (CAR *LIST-HEAD*) branches :key 'car))
	 (brest (find (CAR *LIST-TAIL*) branches :key 'car))
	 (res))
    (when (tdl-list-start-p branches)
      (setf res (get-tdl-list-aux *empty-list-type* (cdr brest)))
      (when (car res)
	(cons (cdr bfirst)
	      (cdr res))))))

(defun get-tdl-diff-list (branches)
  (let* ((blist (find *diff-list-list* branches :key 'car))
	 (end-symb (tdl-diff-list-start-p branches))
	 (res))
    (when end-symb
      (setf res (get-tdl-list-aux end-symb (cdr blist)))
      (when (car res)
	(cdr res)))))

(defun get-tdl-list-aux (end-symb branches)
  (let* ((vfirst (cdr (find (CAR *LIST-HEAD*) branches :key 'car)))
	 (vrest (cdr (find (CAR *LIST-TAIL*) branches :key 'car)))
	 (res))
    (cond
     ((eq (caar branches) end-symb)
      (cons end-symb nil))
     ((null vrest)
      nil)
     ((eq (caar vrest) end-symb)
      (cons end-symb (cons vfirst nil)))
     ((car (setf res (get-tdl-list-aux end-symb vrest)))
      (cons end-symb (cons vfirst (cdr res)))))))

(defun coindex-p (x)
  (and
   (symbolp x)
   (eq (char (symb-2-str x) 0) #\#)))

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
    (dlst-t
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

(defun sql-like-text (id)
  (format nil "~a" (sql-like-text-aux (2-str id))))

(defun sql-like-text-aux (str)
  (cond
   ((equal str "")
    "")
   ((eq (char str 0) #\_)
    (format nil "\\_~a" (sql-like-text-aux (subseq str 1))))
   ((eq (char str 0) #\%)
    (format nil "\\%~a" (sql-like-text-aux (subseq str 1))))
   (t
    (format nil "~a~a" (char str 0) (sql-like-text-aux (subseq str 1))))))
