;;; Copyright (c) 2007
;;; Ben Waldron
;;;   see `LICENSE' for conditions.

(in-package :lkb)

(defvar *compound-constituent-supertype* nil) ;; set in grammar's globals.lsp
(defvar *compound-join-strings* nil) ;; set in grammar's globals.lsp
(defvar *compound-sem-path* nil) ;; set in grammar's globals.lsp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  lexicon wrapper: generates compounds when necessary 
;;;

(defclass compound-lex-database (lex-database)
  ((lex :initarg :lex)))

(defmethod close-lex ((lexicon compound-lex-database) &key in-isolation delete)
  (with-slots (lex) lexicon
    (close-lex lex :in-isolation in-isolation
	       :delete delete)))

(defmethod empty-cache ((lexicon compound-lex-database) &key recurse)
  (when recurse
    (with-slots (lex) lexicon
      (empty-cache lex))))

;;
;; the compound-lexicon wrapper returns a compound analysis
;; (constructed online) if the wrapped lexicon fails to return an analysis
;;
(defmethod lookup-word ((lexicon compound-lex-database) orth &key (cache *lexicon-lexical-entries-cache-p*))
  (with-slots (lex lexical-entries psorts) lexicon
    (let ((entries
	   (or
	    (lookup-word lex orth :cache cache) ;; look in wrapped lexicon first
	    (gethash orth lexical-entries) ;; look for cached value second
	    (let ;; look for compound analysis third
		((compound-analyses (analyse-compound (string-downcase orth) :lexdb lex)))
	      (cond
	       ((null compound-analyses) ;; returned no analysis
		(setf (gethash orth lexical-entries) ;; cache :empty orth
		  :empty))
	       (t
		(loop ;; returned one or more analyses
		    for compound-entry in compound-analyses
		    for id = (lex-entry-id compound-entry)
		    do
		      (setf (gethash orth lexical-entries) ;; cache orth
			(pushnew id (gethash orth lexical-entries)))
		      (setf (gethash id psorts) ;; cache psorts
			compound-entry))
		(gethash orth lexical-entries)))))))
       (if (eq :empty entries) ;; cache value :empty
	nil 
	entries))))

;;
;; the compound-lexicon wrapper is transparent to lex-words method
;;
(defmethod lex-words ((lexicon compound-lex-database))
  (with-slots (lex) lexicon
    (lex-words lex)))

;;
;; compound-lexicon wrapper is transparent to collect-psort-ids method
;;
(defmethod collect-psort-ids ((lexicon compound-lex-database) &key (cache t) (recurse t))
  (with-slots (lex) lexicon
    (collect-psort-ids lex :cache cache :recurse recurse)))

;;
;; any relevant compound analysis will already have been cached
;; if no cached value, recurse into wrapped lexicon
;;
(defmethod read-psort ((lexicon compound-lex-database) id &key (cache t) (recurse t) (new-instance nil))
  (with-slots (lex psorts) lexicon
    (or (gethash id psorts)
	(read-psort lex id :cache cache
		    :recurse recurse
		    :new-instance new-instance))))

;;
;; analyse a compound word into constituents
;; and return corresponding lex entry(s)
;;

;; format = :le :tdl :basic
(defun analyse-compound (word &key (lexdb *lexicon*) (format :le))
  ;; ensure there is no infinite recursion on the compound lexicon
  (when (typep lexdb 'compound-lex-database)
    (setf lexdb (slot-value lexdb 'lex)))
  (let* ((matrix (analyse-compound2 word :lexdb lexdb)) ;; matrix of constituents
	 (paths (analyse-compound-getpaths matrix)) ;; extract paths (reverse points)
	 (strings (analyse-compound-getstrings paths word)) ;; extract paths (strings)
	 ;; heuristic to prune alternatives
	 (path (longest strings :filter #'valid-compound-string)))
    (case format
      (:basic path)
      (:tdl 
       (loop
	   for le in (compound-le path word :lex lexdb)
	   collect (to-tdl le)))
      (:le (compound-le path word :lex lexdb))
      (t
       (error "unknown argument to :format keyword")))))

;; compound must not end in join string
(defun valid-compound-string (x)
  (not
   (member (car (last x)) ;; last element of x
	   *compound-join-strings* 
	   :test #'string=)))
  
;; extract matrix of constituents
(defun analyse-compound2 (word &key (lexdb *lexicon*) matrix (offset 0) prev-cat)
  ;; create new matrix when necessary
  (unless matrix
    (setf matrix (make-array (list (1+ (length word))))))
  (loop
    ;; walk through word from current offset
    ;; attempting to find valid constituent
      for i from 1 to (length word)
      for substr-cat = (analyse-compound-substr (subseq word 0 i) lexdb 
						:prev-cat prev-cat)
      when substr-cat
      do
	;; recurse from new offset
	(pushnew offset (aref matrix (+ offset i)))
	(analyse-compound2 (subseq word i) 
			  :lexdb lexdb
			  :matrix matrix 
			  :offset (+ offset i)
			  :prev-cat substr-cat))
  matrix)

;; a valid constituent is
;;  - joining element in *compound-join-strings*
;;  - lex entry with supertype *compound-constituent-supertype*
(defun analyse-compound-substr (word lexdb &key prev-cat)
  (cond
   ((and (eq prev-cat :lex)
	 (member word *compound-join-strings* :test #'string=))
    :join)
   ((and
     (>= (length word) 3)
     (loop
	 for id in (lookup-word lexdb (string-upcase word))
	 thereis 
	   (subtype-or-equal 
	    (dag-type 
	     (tdfs-indef 
	      (lex-entry-full-fs 
	       (get-lex-entry-from-id id)))) 
	    *compound-constituent-supertype*)))
     :lex)))

;; extract set of paths from matrix
;;
(defun analyse-compound-getpaths (matrix &key i)
  ;; start from end and work backwards
  (unless i
    (setf i (1- (length matrix))))
  (loop
      with paths
      for j in (aref matrix i) ;; there is path-component j->i
      do
	(cond 
	 ((zerop j)
	  ;; we have reached the start
	  (push (list j) paths))
	 (t
	  ;; recurse from new point
	  (loop
	    ;; add j to each path ending in j
	      for path2 in (analyse-compound-getpaths matrix :i j)
	      do
		(push (cons j path2) paths))))
      finally
	(return paths)))

;; split word into components specified by path (list of points)
(defun analyse-compound-getstrings (paths word)
  (loop
      for path in paths
      collect
	(reverse
	 (loop
	     with prev = (length word)
	     for node in path
	     collect (subseq word node prev)
	     do (setf prev node)))))

;; batch process a file
(defun analyse-compound-batch (filename &key (lexdb *lexicon*))
  ;; ensure no infinite recusrion on compound lexicon
  (when (typep lexdb 'compound-lex-database)
    (setf lexdb (slot-value lexdb 'lex)))
  (with-open-file (s filename :direction :input)
    (loop
	with item0
	while (setf item0 (read s nil nil))
	for item = (string-downcase item0)
	for analyses = (analyse-compound item :lexdb lexdb)
	do
	  (format t "~&~a : ~a" item analyses))))

;; return a longest input passing filter
;; (this is used as a heuristic above)
(defun longest (x &key (filter #'(lambda (y) 
				   (declare (ignore y))
				   t)))
  (loop
      with longest
      with len = 0
      for item in x
      for len2 = (length item)
      when (and
	    (funcall filter item)
	    (> len2 len))
      do 
	(setf longest item)
	(setf len len2)
      finally
	(return longest)))

;; split word into path (strings) and convert to lex entry
;; this lex entry is formed by:
;;  - head (=final component) provides template
;;  - into which orth(=word)
;;  - and new sem pred (=replace head in old sem by word)
;;    are substituted
(defun compound-le (path word &key (lex *lexicon*))
  (when path
    (let ((head (car (last path)))) ;; head is final component
      (loop
	  with le
	  for i from 0
	  for id in (lookup-word lex (string-upcase head)) ;; retrieve template lex entry
	  for tdfs = (tdfs-indef 
		      (lex-entry-full-fs 
		       (get-lex-entry-from-id id)))
	  for type = (dag-type tdfs)
	  when (subtype-or-equal type *compound-constituent-supertype*) 
	       ;; ensure template has required supertype
	  do 
	    (let* (;; extract unifs
		   (unifs (tdl-to-unifs 
			   (to-tdl-body (read-psort lex id))))
		   ;; extract orth
		   (orth (find (append *orth-path*
				       *list-head*) 
			       unifs
			       :key #'(lambda (x) 
					(path-typed-feature-list 
					 (unification-lhs x))) 
			       :test #'equalp))
		   ;; extract sem-val
		   (sem (find *compound-sem-path* unifs
			      :key #'(lambda (x) 
				       (path-typed-feature-list 
					(unification-lhs x)))
			     :test #'equalp))
		   (sem-val (u-value-type (unification-rhs sem)))
		   ;; create new lex id
		   (lex-id (intern 
			    (string-upcase 
			     (format nil "compound_~a-~a_~a"
				     word
				     i
				     (str-list-2-str-by-str path "_")
				     )))))
	      ;; insert replacement orth
	      (setf (u-value-type (unification-rhs orth)) word)
	      ;; insert replacement sem
	      (setf (u-value-type (unification-rhs sem)) 
		(cl-ppcre:regex-replace head sem-val word))
	      ;; put the pieces together into a lex entry
	      (setf le
		(make-lex-entry
		 :UNIFS unifs
		 :ID lex-id
		 :ORTH (list word))))
	  and collect le))))
