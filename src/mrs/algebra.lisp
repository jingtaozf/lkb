;;; Copyright (c) 2004--2005
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

;;; Checking that a grammar obeys the algebra for composition
;;; and extracting the components so that algebra MRSs can be displayed
;;; on parse trees.

;;; sement structures and print code are in basemrs.lisp

(in-package :mrs)

;;; FIX
;;; variable naming - we would like this to be constant over a parse
;;; this implies calling the algebra construction code in such a way
;;; that variables are recognised as equivalent between phrases.
;;; The best way to do this is probably to do something INSTLOC-like
;;; but ignore for now.
;;; There's also an issue about the interaction between this and the standard
;;; MRS extraction code, again ignored for now.

(defun extract-sement (fs)
  (initialise-algebra)
  (extract-algebra-from-fs (lkb::tdfs-indef fs)))

(defun initialise-algebra nil
  (unless (and *hook-path*
	       *psoa-liszt-path*
	       *psoa-rh-cons-path*)
    (error "Grammar parameters not set for algebra use"))
  (setf *variable-generator* (init-variable-generator))
  (setf *named-nodes* nil))

(defun extract-algebra-from-fs (fs)
  ;;; components are
  ;;; a) hook
  ;;; b) slots
  ;;; c) rels list
  ;;; d) hcons
  (let ((sem-fs (path-value fs *initial-semantics-path*)))
    ;; *fragment-p* controls whether the scoping code is run
    (if (is-valid-fs sem-fs)
        (construct-algebra-sement sem-fs fs))))


(defun construct-algebra-sement (sem-fs full-fs)
  (let* ((hook-fs (path-value sem-fs *hook-path*))
         (liszt-fs (path-value sem-fs *psoa-liszt-path*))
         (h-cons-fs (path-value sem-fs *psoa-rh-cons-path*)))
    (unless (and hook-fs liszt-fs h-cons-fs)
      (error "Missing algebra components"))
    (let ((hook (construct-algebra-hook hook-fs))
	  (slots (construct-algebra-slots full-fs))
	  (rels (nreverse (construct-liszt 
			   liszt-fs nil *variable-generator*)))
	  ;;; construct-liszt in mrsoutput
          (hcons (nreverse (construct-h-cons 
			    h-cons-fs nil *variable-generator*))))
      	  ;;; construct-h-cons in mrsoutput
      (make-sement :hook hook :slots slots
		   :liszt rels :h-cons hcons))))

(defun construct-algebra-hook (fs)
  (let ((index-fs
	 (path-value fs *index-path*))
	(ltop-fs 
	 (path-value fs *ltop-path*))
	(xarg-fs
	 (path-value fs *xarg-path*)))
    (unless (and index-fs (is-valid-fs index-fs)
		 ltop-fs (is-valid-fs ltop-fs))
      (error "Defective hook feature structure"))
    (make-hook
     :index (create-variable index-fs *variable-generator*)
     :ltop (create-variable ltop-fs *variable-generator*)
     :xarg (if (and xarg-fs (is-valid-fs xarg-fs))
	       (create-variable xarg-fs *variable-generator*)))))


(defun construct-algebra-slots (fs)
  ;;; the code for the generator and the SEM-I that do fairly
  ;;; similar things don't quite do what's wanted.  
  ;;; This code walks over a feature structure, finding all
  ;;; paths in the syntax part of the FS that lead to a HOOK
  ;;; feature.  The HOOK is taken to be a slot, the path name
  ;;; is used to generate the slot name.
  ;;; 1. reentrancy - if we've seen the HOOK before, store 
  ;;; all paths that lead to it and decide on the best name
  ;;; when we're done (e.g., ignore -- paths)
  ;;; 2. don't worry about OPT etc.  The slot will be registered
  ;;; as a slot at the location where it exists.  The algebra
  ;;; checking code will have to ensure legitimacy of slot removal.
  ;;;
  ;;; C-CONT is an issue
  (let ((slot-alist
	 (lkb::collect-subdags-for-feature fs (car *hook-path*) nil
					   '(lkb::sem lkb::args))))
    (loop for slot in slot-alist
	collect
	  (make-slot :hook (construct-algebra-hook (car slot))
		     :name (create-slot-name (cdr slot))))))

(defparameter *tmp-slot-paths* nil)

(defun create-slot-name (paths)
  ;;; slot naming
  ;;; a slot gets named after the list of features on the trimmed path
  ;;; LIST features are interpreted so that we get
  ;;; COMPS1 etc
  ;;;
  ;;; in the case of multiple paths leading to the same 
  ;;; HOOK, we distinguish between uninteresting cases (e.g. features beginning
  ;;; with `--') and interesting cases, such as control where
  ;;; e.g. try shares its SPR hook with the COMP1 SPR1 hook (because
  ;;; the whole SPR is shared)
  ;;; given ((SPR FIRST SEM HOOK)(COMPS REST FIRST SPR FIRST SEM HOOK))
  ;;; we return SPR1/COMPS2.SPR1
  (let* ((first-feat (car lkb::*list-head*))
	 (rest-feat (car lkb::*list-tail*))
	 (names 
	  (loop for path in paths
	      collect
		(let* ((trimmed-path
			(reverse (cddr (reverse path)))) ;; get rid of SEM HOOK
		       ;; FIX - do this properly
		       (path-with-counts
			(reverse
			 (trimmed-path-name trimmed-path 0 nil first-feat rest-feat))))
		  (apply #'concatenate 'string path-with-counts)))))
    (if (cdr names)
	(let ((sorted-names (sort names #'(lambda (x y)
					    (< (length x) (length y))))))
	  (format nil "~A/~{~A~}" (car sorted-names) (cdr sorted-names)))
      (car names))))
	    
(defun trimmed-path-name (path count name-so-far first-feat rest-feat)
  (if (null path) name-so-far
    (progn 
      (cond ((eql (car path) first-feat)
	     (push (format nil "~A" (+ 1 count)) name-so-far)
	     (setf count 0))
	    ((eql (car path) rest-feat)
	     (setf count (+ 1 count)))
	    (t (let ((name (format nil ".~A" (car path))))
		 (push name name-so-far))))
      (trimmed-path-name (cdr path) count name-so-far first-feat rest-feat))))


	  
