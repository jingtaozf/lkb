;;; Copyright (c) 1998-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see LICENSE for conditions

(in-package :lkb)

;;; *check-paths* is the parameter set in user file. This is used to construct
;;; value of *check-paths-optimised* which is what is used internally

;;; (defparameter *check-paths* nil)
;;; defined in globals

(defparameter *check-paths-optimised* nil)


;;; Macro for wrapping around some call to parse a sentence or a set of
;;; sentences - collects stats on all feature paths that fail in unification,
;;; and computes best set for checking values of before unifications. Writes
;;; out set to specified file
;;;
;;; NB existing set of checking paths is left untouched

#|
(with-check-path-list-collection "Macintosh HD:mylkb:checkpaths1.lsp"
   (parse-sentences "Macintosh HD:mylkb:checkpaths-sample.txt" "Macintosh HD:mylkb:checkpaths-out.txt"))

(with-check-path-list-collection "Macintosh HD:mylkb:checkpaths1.lsp"
   (parse '("Devito" "manages" "a" "programmer" "Abrams" "interviewed" "and" "Browne" "hired") nil))

(with-check-path-list-collection "Macintosh HD:lkb99-expt:big:grammar:lkb:checkpaths1.lsp"
  (chart-generate input-sem lex-entry-alts))

(with-check-path-list-collection "~aac/checkpaths.lsp"
  (tsdb::tsdb-do-process "csli3"))

(with-check-path-list-collection "data/textbook/lkb/checkpaths2.lsp"
  (parse-sentences "data/textbook/well-formed.txt" "data/textbook/test.out"))
|#

;;; see variant at end of file for interactive version

(defmacro with-check-path-list-collection (output-file &body forms)
  `(let ((.saved-names-and-fns. (install-unify-check-paths-functions))
	 (.completedp. nil))
     (declare (special *fail-path-list*))
     (unwind-protect
         (prog1
	     ;; disable any path checking in force
	     (let ((*check-paths-optimised* nil)) 
                ,@forms)
	   (setq .completedp. t))
       ;; Restore original function definitions
       (dolist (name-and-fn .saved-names-and-fns.)
	 (setf (symbol-function (car name-and-fn)) (cdr name-and-fn)))
       (when .completedp.
	 (with-open-file (.str. ,output-file :direction :output 
			  :if-exists :supersede
			  :if-does-not-exist :create)
	   (let ((*print-pretty* nil))
	     (format .str. "#|~%Check paths created from execution of~%  ~S~%|#~%"
                '(with-check-path-list-collection ,output-file ,@forms)))
	   (format t "~%Extracting paths...")
	   (write
	    `(defparameter *check-paths* 
		 ',(check-path-convert 
		    (extract-check-paths *fail-path-list*)))
	    :stream .str. :escape t :pretty t :length nil :level nil)
	   (terpri .str.)
	   (format t "~%Wrote file ~A" (truename ,output-file)))))))



(defun extract-check-paths (fail-path-list)
  (when fail-path-list
    (let ((max 0) 
	  (max-item nil))
      ;; Find the path that caused the greatest number of unification failures
      (dolist (item fail-path-list)
	(when (> (hash-table-count (cdr item)) max)
	  (setq max (hash-table-count (cdr item)))
	  (setq max-item item)))
      ;; Remove all the failure paths that are superceded by the one we just
      ;; found (i.e., which were contributed by one of the unifications that
      ;; contributed the best path), and repeat the process
      (cons (cons (car max-item) max)
            (extract-check-paths
	     (mapcan
	      #'(lambda (item)
		  (let ((item-table (cdr item)))
		    (maphash
		     #'(lambda (key val)
			 (declare (ignore val))
			 (remhash key item-table))
		     (cdr max-item))
		    (when (> (hash-table-count item-table) 0)
		      (list (cons (car item) item-table)))))
	      (remove max-item fail-path-list :test #'eq)))))))

#|
(defun extract-check-paths (fail-path-list)
  ;; simple-minded version which just returns paths in order of decreasing
  ;; number of fails
  (sort
     (mapcar #'(lambda (item) (cons (car item) (hash-table-count (cdr item))))
        fail-path-list)
     #'> :key #'cdr))

(with-check-path-list-collection "Macintosh HD:mylkb:checkpaths-naive.lsp"
   (parse-sentences "Macintosh HD:mylkb:checkpaths-sample.txt" nil))
|#


;;; find ALL failing paths, not just first
;;;
;;; performs similar function to reordering features so most common failing
;;; features come first - here, the most commonly failing paths are appended to
;;; front of feature structures, in order of most commonly failing first
;;;
;;; Only collects when actually parsing - as long as other bits of code don't
;;; call unify-dags, but unifiable-dags-p etc instead
;;;
;;; NB If unify-dags or unify2 change, then the code below had better be
;;; updated accordingly!!!

(defun install-unify-check-paths-functions nil
  (declare (special *collecting-check-paths-p* *unify-dags-fail-count* 
		    *fail-path-list*))
  (prog1
      (mapcar #'(lambda (name) (cons name (symbol-function name))) 
	      '(unify-dags unify2))
    (setq *collecting-check-paths-p* nil)
    (setq *unify-dags-fail-count* 0)
    (setq *fail-path-list* nil)
    ;;
    (setf (symbol-function 'unify-dags)
      #'(lambda (dag1 dag2)
	  (if *within-unification-context-p*
	      (let ((*collecting-check-paths-p* t)
		    (*unify-dags-failed-p* nil))
		(declare
		 (special *collecting-check-paths-p* *unify-dags-failed-p*
			  *unify-dags-fail-count*))
		(incf *unify-dags-fail-count*)
		(catch '*fail*
		  (unify1 dag1 dag2 nil)
		  (if *unify-dags-failed-p* nil dag1)))
	    (with-unification-context (dag1) 
	      (when (unify-dags dag1 dag2) 
		(copy-dag dag1))))))
    ;;
    (setf (symbol-function 'unify2)
      #'(lambda (dag1 dag2 path)
	  (declare
	   (special *collecting-check-paths-p* *unify-dags-failed-p* 
		    *unify-dags-fail-count* *fail-path-list*))
	  (multiple-value-bind (new-type constraintp)
	      (greatest-common-subtype (unify-get-type dag1) (unify-get-type dag2))
	    ;; --- new bit start
	    (if (or new-type *collecting-check-paths-p*)
		(progn
		  (unless new-type
		    ;; Unification failed, so we want to record the path
		    (setq *unify-dags-failed-p* t)
		    (let* ((p (reverse path))
			   (item (assoc p *fail-path-list* :test #'equal)))
		      (unless item
			(setq item (cons p (make-hash-table)))
			(push item *fail-path-list*))
		      ;; an adjustable bit-vector might be more suitable than
		      ;; a hash table
		      (setf (gethash *unify-dags-fail-count* (cdr item)) t)))
		  ;; --- new bit end
		  (setf (dag-new-type dag1) new-type)
		    (progn
		      ;; unify in constraints if necessary - may have to copy
		      ;; them to prevent separate uses of same constraint in
		      ;; same unification becoming reentrant
		      (when (and constraintp *unify-wffs*)
			(let ((constraint (may-copy-constraint-of new-type)))
			  (if *unify-debug*
			      (let ((res
				     (catch '*fail* (unify1 dag1 constraint path))))
				(unless res
				  (if (eq *unify-debug* :return)
                                    (setf %failure% 
                                      (list :constraints 
                                            (reverse path) new-type))
                                    (format 
                                     t 
                                     "~%Unification with constraint ~
                                      of type ~A failed ~
                                      at path < ~{~A ~^: ~}>" 
                                     new-type (reverse path)))
				  (throw '*fail* nil)))
			    (unify1 dag1 constraint path)))
			;; dag1 might just have been forwarded so dereference
			;; it again
			(setq dag1 (deref-dag dag1)))
		      (setf (dag-copy dag1) :inside)
		      ;; cases for each of dag1 and dag2 where they have no
		      ;; arcs just considering straightforward use of unify1:
		      ;; if we've previously visited a node with no arcs then
		      ;; it must have got forwarded then so we won't ever
		      ;; visit it again - so no need to test for presence of
		      ;; any comp-arcs BUT: unify-paths-dag-at-end-of1 adds to
		      ;; comp-arcs independently so we do need the additional
		      ;; tests
		      (cond
		       ((and (null (dag-arcs dag1)) (null (dag-comp-arcs dag1)))
                        (setf (dag-new-type dag2) new-type)
                        (setf (dag-forward dag1) dag2))
		       ((and (null (dag-arcs dag2)) (null (dag-comp-arcs dag2)))
                        (setf (dag-forward dag2) dag1))
		       (t
                        (setf (dag-forward dag2) dag1)
                        (unify-arcs dag1 dag2 path)))
		      (setf (dag-copy dag1) nil)))
	      (progn
		;; Unification failed, and we aren't collecting failure stats
		(when *unify-debug*
		  (if (eq *unify-debug* :return)
                    (setf %failure% (list :clash 
                                          (reverse path) 
                                          (unify-get-type dag1)
                                          (unify-get-type dag2)))
                    (format 
                     t 
                     "~%Unification of ~A and ~A failed at path < ~{~A ~^: ~}>"
                     (unify-get-type dag1) (unify-get-type dag2) 
                     (reverse path))))
		(throw '*fail* nil)))))
      )
					;
    ))


;;; called from check-type-table, once constraints have been expanded. Needs
;;; to be kept in synch with type hierarchy and constraints
;;;
;;; daughters-restricted field of rules must be kept in synch with optimised
;;; paths.  This is done when a rule is read in

(defun optimise-check-unif-paths nil
  (when (or (null *check-paths*)
            (find :vanilla *features*))
    (setq *check-paths-optimised* nil)
    (return-from optimise-check-unif-paths nil))
  (let ((nseen 0))
    ;; there's scope here for experimenting with the criteria under which lower
    ;; frequency paths should be kept
    (setq *check-paths-optimised*
      (mapcan
       #'(lambda (path-and-freq)
	   (incf nseen)
	   (cond
	    ((not (and (listp (car path-and-freq)) (integerp (cdr path-and-freq))))
	     (error "Incorrect format for check path list"))
            ;; ((and (< (cdr path-and-freq) 1000) (> nseen 40))
            ;;  ;; keep all paths whose freq is within a factor of 1000 of most frequent
            ;;  nil)
            ((> nseen *check-path-count*)
             ;; AAC - for ACL/Linux at least, 45 seems 
             ;; suboptimal
             nil)
            (t
	     (list
	      (optimise-check-unif-path
	       (car path-and-freq) (cdr path-and-freq))))))
       *check-paths*))
    t))

(defun optimise-check-unif-path (path freq)
  (cons path
	(if path
	    (let* ((feat (car (last path)))
		   (fs
		    (constraint-of
		     (or (maximal-type-of feat)
			 (error "Inconsistency - *check-paths* uses feature ~A ~
                                 which is not in grammar" feat))))
		   (type (type-of-fs (get-dag-value fs feat))))
	      (let* ((types (cons (get-type-entry type) 
				  (retrieve-descendants type)))
		     (len (length types)))
		;; (format t "~%Feature ~A, number of possible types ~A" feat len)
		(if (and (<= len (integer-length most-positive-fixnum)) ; restrict to fixnum
                       (or (null *string-type*)
                           (not (member *string-type* types :key #'ltype-name))))
		    (mapcar
		     #'(lambda (d)
			 (cons (ltype-name d)
			       (let ((val 0))
				 (dolist (x (cons d (ltype-descendants d)) val)
				   (setq val
				     ;; set bit corresponding to pos of x in
				     ;; types list
				     (dpb 1 (byte 1 (position x types)) val))))))
		     types)
		  freq)))
	  freq)))


(defmacro type-bit-representation-p (x)
  ;; mcl produces inline code for ccl:fixnump, but not integerp - and we know
  ;; that the bit representation is < most-positive-fixnum
  #+:mcl `(ccl:fixnump ,x)
  #+:allegro `(excl:fixnump ,x)
  #-(or :mcl :allegro) `(integerp ,x))


#|
(optimise-check-unif-paths)

;;; update rules in situ with unif paths

(dolist (table (list *rules* *lexical-rules*))
   (maphash
      #'(lambda (id rule)
         (declare (ignore id))
         (let* ((fs (rule-full-fs rule))
                (f-list (rule-order rule)))
          (setf (rule-daughters-restricted rule)
            (mapcar
               #'(lambda (path)
                   (restrict-fs (existing-dag-at-end-of (tdfs-indef fs) path)))
               (cdr f-list)))))
      table))
|#

;;; Statically compute set of restrictor values for a tdfs or dag, and check
;;; two sets of values for compatibility
;;;


(defun restrict-fs (fs)
  (loop for path-spec in *check-paths-optimised*
      collect
	(let ((v (existing-dag-at-end-of fs (car path-spec))))
	  (when v
	    (let ((type (type-of-fs v)))
	      (when type
		(if (consp (cdr path-spec))
		    ;; there is a bit-vector encoding for the possible values
		    ;; of this path, so use it instead of the type name
		    (or
		     (cdr (assoc type (cdr path-spec) :test #'eq))
		     (error "Inconsistency - ~A could not find restrictor ~
                              bit vector for type ~A at path ~A" 
			    'restrict-fs type (car path-spec)))
		  type)))))))

(defun restrictors-compatible-p (daughter-restricted child-restricted)
  (loop for dt in daughter-restricted
      for ct in child-restricted
      do
	(cond
	 ;; eq possibly avoids a function call
	 ((or (eq dt ct) (null dt) (null ct))) 
	 ((not (type-bit-representation-p dt)) 
	  ;; a type - i.e. a symbol or disjunction (list)
	  ;; not bit vector encoding, so do type unification the hard way
          (unless (greatest-common-subtype dt ct)
	    (return-from restrictors-compatible-p nil)))
	 ;; a bit vector
	 ((zerop (logand (the fixnum dt) (the fixnum ct)))
	  (return-from restrictors-compatible-p nil))))
  t)


;;; Versions called dynamically inside the scope of a set of unifications

;;;
;;; note that x-existing-dag-at-end-of() now assumes the input argument has
;;; been dereferenced already; otherwise, x-restrict-fs() would amount in one
;;; deref-dag() call per quick check path.                 (23-jun-99  -  oe)
;;;

(defun x-restrict-fs (fs)
  (loop 
      with fs = (deref-dag fs)
      for path-spec in *check-paths-optimised*
      collect
        (let ((v (x-existing-dag-at-end-of fs (car path-spec))))
          (when v
            (let ((type (or (dag-new-type v) (dag-type v))))
              (when type
                (when (consp type) (setq type (car type)))
                (if (consp (cdr path-spec))
                  (or (cdr (assoc type (cdr path-spec) :test #'eq))
                      (error "Inconsistency - ~A ~
                              could not find restrictor bit vector ~
                              for type ~A at path ~A" 'x-restrict-fs
                              type (car path-spec)))
                  type)))))))

(defun x-restrict-and-compatible-p (fs child-restricted)
  (loop
      with fs = (deref-dag fs)
      for path-spec in *check-paths-optimised*
      for dt = (let ((v (x-existing-dag-at-end-of fs (car path-spec))))
                 (when v
                   (let ((type (or (dag-new-type v) (dag-type v))))
                     (when type
                       (when (consp type) (setq type (car type)))
                       (if (consp (cdr path-spec))
                           (or (cdr (assoc type (cdr path-spec) :test #'eq))
                               (error "Inconsistency - ~A ~
                                       could not find restrictor bit vector ~
                                       for type ~A at path ~A" 'x-restrict-fs
                                       type (car path-spec)))
                         type)))))
      for ct in child-restricted
      do
        (cond
         ((or (eq dt ct) (null dt) (null ct))) ; eq possibly avoids a function call
         ((not (type-bit-representation-p dt))
	  (unless (greatest-common-subtype dt ct) 
	    (return-from x-restrict-and-compatible-p nil)))
         ((zerop (logand (the fixnum dt) (the fixnum ct)))
	  (return-from x-restrict-and-compatible-p nil))))
  t)

(defun x-existing-dag-at-end-of (dag labels-chain)
  (cond 
   ((null labels-chain) dag)
   (t
    (let ((one-step-down (x-get-dag-value dag (car labels-chain))))
      (when one-step-down 
        (x-existing-dag-at-end-of 
         (deref-dag one-step-down) (cdr labels-chain)))))))

(defun x-get-dag-value (dag attribute)
  (dolist (arc (dag-comp-arcs dag) nil)
    (when (eq attribute (dag-arc-attribute arc))
      (return-from x-get-dag-value (dag-arc-value arc))))
  (dolist (arc (dag-arcs dag))
    (when (eq attribute (dag-arc-attribute arc))
      (return-from x-get-dag-value (dag-arc-value arc)))))



#|
(defun treeify (paths)
  (let ((tree nil))
    (dolist (x paths)
      (setq tree (add-path (car x) (cdr x) tree)))
    tree))

(defun add-path (path value tree)
  (if (null path)
      (push (cons :value value) tree)
    (let ((next (assoc (car path) tree)))
      (unless next
	(setq next (cons (car path) nil))
	(push next tree))
      (setf (cdr next) (add-path (cdr path) value (cdr next)))
      tree)))
|#

(defun interactive-create-check-paths nil
  (let* ((test-file (ask-user-for-existing-pathname "Checkpaths sample file?"))
         (output-file (ask-user-for-new-pathname "Checkpaths output file?")))
    (when (and test-file output-file)
        (with-check-path-list-collection output-file
	  (parse-sentences test-file t)))
    (format t "~%Script should contain:~
               ~%(lkb-load-lisp (this-directory) <your-checkpaths-file> t)")))





