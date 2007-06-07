;;; -*- Mode: Lisp; Package: COMMON-LISP-CONTROLLER -*-
;;;
;;; Copyright (C) 2000,2004  Peter Van Eynde and Kevin M. Rosenberg
;;; Licensed under the LLGPL, see debian/copyright file

(in-package #:common-lisp-controller)

(eval-when (:load-toplevel :execute :compile-toplevel)
  (unless (find-package :asdf)
    (error "You need to load the asdf system before loading or compiling this file!")))

(defun get-homedir ()
  #-cmu
  (user-homedir-pathname)
  #+cmu
  (let ((dirs (extensions:search-list "home:")))
    (when dirs
      (first dirs))))

;;; ECL has the asdf::build-op operation
#+ecl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :osi))

#+clisp (defun get-uid () (posix:user-info-uid (posix:user-info (ext:getenv "USER"))))
#+sbcl (defun get-uid () (sb-unix:unix-getuid))
#+cmu (defun get-uid () (unix:unix-getuid))
#+allegro (defun get-uid () (excl.osi:getuid))
#+clisp (defun world-writable? (mode) (or (member :RWXO mode) (member :WOTH mode)))
#+clisp (defun group-writable? (mode) (or (member :RWXG mode) (member :WGRP mode)))
#-clisp (defun world-writable? (mode) (/= 0 (logand mode #o002)))
#-clisp (defun group-writable? (mode) (/= 0 (logand mode #o020)))

#-(or cmu sbcl clisp allegro)
(defun get-uid ()
  (labels ((mktemp ()
	     (let ((temp-name (format nil "/tmp/clc-~A" (random 50000))))
	       (with-open-file (file temp-name :direction :input :if-does-not-exist nil)
		 (if file (mktemp) temp-name)))))

    (let ((file (mktemp)))
      (unwind-protect
	   (progn (asdf:run-shell-command "umask 077 && echo $UID > ~A" file)
		  (with-open-file (uid file :direction :input)
		    (handler-case (parse-integer (read-line uid))
		      (error () (error "Unable to find out user ID")))))
	(asdf:run-shell-command "rm ~A" file)))))

#+clisp
(defun get-owner-and-mode (directory)
  (when (ignore-errors (ext:probe-directory directory))
    (let* ((stat (posix:file-stat directory))
	   (mode (posix:file-stat-mode stat))
	   (owner (posix:file-stat-uid stat)))
      (values owner mode))))

#+sbcl
(defun get-owner-and-mode (directory)
  (when (eq :directory
	    (sb-unix:unix-file-kind (namestring directory)))
    ;; check who owns it
    (multiple-value-bind (res dev ino mode nlink uid gid rdev size atime mtime)
	(sb-unix:unix-stat (namestring directory))

      (declare (ignore res dev ino nlink gid rdev size atime mtime))
      (values uid mode))))

#+cmu
(defun get-owner-and-mode (directory)
  (when (eq :directory
	    (unix:unix-file-kind (namestring directory)))
    ;; check who owns it
    (multiple-value-bind (res dev ino mode nlink uid gid rdev size atime mtime)
	(unix:unix-stat (namestring directory))

      (declare (ignore res dev ino nlink gid rdev size atime mtime))
      (values uid mode))))

#+allegro
(defun get-owner-and-mode (directory)
  (when (excl:probe-directory directory)
    ;; check who owns it
    (let* ((stat (excl.osi:stat (namestring directory)))
	   (mode (excl.osi:stat-mode stat))
	   (uid  (excl.osi:stat-uid stat)))
      (values uid mode uid))))

#+sbcl
(defmacro with-secure-umask (&body forms)
  (let ((old-umask (gensym)))
    `(let ((,old-umask (sb-posix:umask #o0077)))
      (unwind-protect (progn ,@forms)
	(sb-posix:umask ,old-umask)))))

#+clisp
(defmacro with-secure-umask (&body forms)
  (let ((old-umask (gensym)))
    `(let ((,old-umask (posix:umask #o077)))
      (unwind-protect ,@forms
	(posix:umask ,old-umask)))))

#+cmu
(defmacro with-secure-umask (&body forms)
  (let ((old-umask (gensym)))
    `(let ((,old-umask (unix::unix-umask #o077)))
      (unwind-protect ,@forms
	(unix::unix-umask ,old-umask)))))

#+allegro
(defmacro with-secure-umask (&body forms)
  (let ((old-umask (gensym)))
    `(let ((,old-umask (excl.osi:umask #o077)))
      (unwind-protect ,@forms
	(excl.osi:umask ,old-umask)))))

#+(or clisp sbcl cmu allegro)
(defun check-spooldir-security (target)
  ;; does target exist?
  (multiple-value-bind (uid mode)
      (get-owner-and-mode (namestring target))
    (if uid
	(let ((my-uid (get-uid)))
	  (unless (= uid my-uid)
	    (error "Security problem: The owner of ~S is not ~S as I wanted"
		   target
		   my-uid))
	  (when (world-writable? mode)
	    (error "Security problem: the cache directory ~S is writable for other users"
		   target))
	  (when (group-writable? mode)
	    (error "Security problem: the cache directory ~S is writable for a group, for better security we do not allow this"
		   target)))
	(with-secure-umask (ensure-directories-exist target))))
  (values))

;; sucks but is portable ;-(
#-(or cmu sbcl clisp allegro)
(defun check-spooldir-security (target)
  #+(or)
  (cerror "I have checked this"
	  "The security of the directory ~A cannot be checked.
Please check if you are the owner of that directory and
that the permissions are such that nobody can write to it."
	  target)
  (let ((result
	 (asdf:run-shell-command "perl -W -e 'use File::stat; use User::pwent;
umask 077;
if (! -d \"~A~:*\") {
  mkdir \"~A~:*\";
}~
my $stat=stat(\"~A~:*\") || exit 42;
if (($stat->mode & 0040000) == 0) {
   exit 43;
}
if (! -O \"~A~:*\" ) {
   exit 44;
}
if (($stat->mode & 002) != 0) {
   exit 45;
}
if (($stat->mode & 020) != 0) {
   exit 46;
}
exit 0;' 2>&1 3>&1"
				     target)))
    (case result
      (0 nil)
      (42 (error "Security problem: Could not stat ~A" target))
      (43 (error "Security problem: ~A is not a directory" target))
      (44 (error "Security problem: ~A is not a owned by you" target))
      (45 (error "Security problem: ~A is world writable" target))
      (46 (error "Security problem: ~A is writable by a group, we do not allow this" target)))))


(defun calculate-fasl-root  ()
  "Inits common-lisp controller for this user"
  (or *fasl-root*
    (setf *fasl-root*
	  (let ((target-root (merge-pathnames
			      (make-pathname
			       :directory (list :relative (format nil "~A" (get-uid))))
			      #p"/var/cache/common-lisp-controller/")))
	    (check-spooldir-security target-root)
	    (merge-pathnames (make-pathname
			      :directory (list :relative *implementation-name*))
			     target-root)))))

(defun asdf-system-compiled-p (system)
  "Returns T is an ASDF system is already compiled" 
  (notany #'(lambda (op) (and (typep (car op) 'asdf:compile-op)
			      (not (asdf:operation-done-p (car op) (cdr op)))))
	  (asdf::traverse (make-instance 'asdf:compile-op) system)))

(defun beneath-source-root? (c)
  "Returns T if component's directory below *source-root*"
  (when c
    (let ((root-dir (pathname-directory (asdf::resolve-symlinks *source-root*)))
	  (comp-dir (pathname-directory (asdf:component-pathname c))))
      (and (>= (length comp-dir)
	       (length root-dir))
	   (equalp root-dir (subseq comp-dir 0 (length root-dir)))))))

(defvar *warned-for-broken-enough-namestring* nil)

(defun source-root-path-to-fasl-path (source)
  "Converts a path in the source root into the equivalent path in the fasl root"
  (let* ((source-root (asdf::resolve-symlinks *source-root*))
	 (relative-source (enough-namestring source source-root)))

    #-(or clisp sbcl allergo cmu)
    (when (equalp source
		  (pathname relative-source))
      (unless *warned-for-broken-enough-namestring*
	(warn "your enough-namestring implementation is not reducting a pathname like it should, correcting for this")
	(setf *warned-for-broken-enough-namestring* t))

      (let ((source-root-path (pathname-directory source-root))
	    (source-path (pathname-directory source)))
	(setf relative-source
	      (make-pathname
	       :directory (cons :RELATIVE
				(loop :for tail :on source-path
				      :for root :in source-root-path
				      :while root
				      :unless (equal root (first tail)) :do
				      (error "Path ~S not beneath ~S? ~S /= ~S"
					  source source-root root (first tail))
				      :finally (return tail)))
	       :defaults source))))
  (merge-pathnames  relative-source *fasl-root*)))

(defun alternative-root-path-to-fasl-path (source)
  "Converts a path in anywhere to a path beneath the fasl root"
  (let ((source-path (pathname-directory source
					 :case :local)))
    (cond
      ;; it could already be beneath /var/cache/common-lisp
      ((and (eq (first source-path) :ABSOLUTE)
	    (string= (second source-path) "var")
	    (string= (third source-path) "cache")
	    (string= (fourth source-path) "common-lisp"))
       ;; just let it be
       source)
      ;; it could be a library package
      ((and (eq (first source-path) :ABSOLUTE)
	    (string= (second source-path) "usr")
	    (string= (third source-path) "lib"))
       ;; just let it be
       source)
      (t
       (merge-pathnames
	(make-pathname :directory
		       (append (list :RELATIVE "local")
			       (rest source-path))
		       :case :local
		       :defaults source)
	*fasl-root*)))))

(defmethod asdf:output-files :around ((op asdf:operation) (c asdf:component))
  "Method to rewrite output files to fasl-root"
  (let ((orig (call-next-method)))
    (if (not *redirect-fasl-files-to-cache*)
	orig
      (progn
	(calculate-fasl-root)
	(cond ((beneath-source-root? c)
	       (mapcar #'source-root-path-to-fasl-path orig))
	      (t
	       (mapcar #'alternative-root-path-to-fasl-path orig)))))))

(defun system-in-source-root? (c)
  "Returns T if component's directory is the same as *source-root* + component's name"
  ;; asdf::resolve-symlinks gives an error for non-existent pathnames
  ;; on lispworks
  (ignore-errors
    (when c
      (equalp (pathname-directory (asdf:component-pathname c))
	      (pathname-directory
	       (asdf::resolve-symlinks
		(merge-pathnames
		 (make-pathname
		  :directory (list :relative (asdf:component-name c)))
		 *source-root*)))))))

(defun find-system-def (module-name)
  "Looks for name of system. Returns :asdf if found asdf file."
  (when (asdf:find-system module-name nil) :asdf))

(defun require-asdf (module-name)
  (let ((system (asdf:find-system module-name)))
    (when system
      (if (asdf-system-compiled-p system)
		  (asdf:oos 'asdf:load-op module-name)
		(progn
		  (unless *clc-quiet*
		    (format t "~&;;; Please wait, recompiling library..."))
		  (asdf:oos 'asdf:compile-op module-name)
		  (terpri)
		  (asdf:oos 'asdf:load-op module-name)))
      t)))

(defun clc-require (module-name &optional (pathname 'clc::unspecified))
  (let ((*redirect-fasl-files-to-cache* t))
    (if (not (eq pathname 'clc::unspecified))
	(common-lisp:require module-name pathname)
      (let ((system-type (find-system-def module-name)))
	(case system-type
	  (:asdf
	   (require-asdf module-name))
	  ;; Don't call original-require with SBCL since we are called by that function
	  #-sbcl 
	  (otherwise
	   (common-lisp:require module-name)))))))


(defun clc-build-all-packages (&optional (ignore-errors nil))
  "Tries to build all known packages.
Looks in /usr/share/commmon-lisp/systems/ for .asd files
If IGNORE-ERRORS is true ignores all errors while rebuilding"
  (loop :for registry-object :in asdf:*central-registry*
	:for registry-location = (eval registry-object)
	:with failed-packages = ()
	:finally (when failed-packages
		   (format t "~&~%Failed the following packages failed: ~{~A~^, ~}"
			   failed-packages))
	:do
	(loop :for pathname :in (directory
			         (merge-pathnames
			      	  (make-pathname :name :wild :type "asd")
				  registry-location))
	      :for package-name = (pathname-name pathname) :do
	      (restart-case
	       (handler-case 
		(let ((*redirect-fasl-files-to-cache* t))
		  (asdf:oos 'asdf:compile-op package-name))
		(error (error)
		       (cond (ignore-errors
			       (format t "~&Ignoring error: ~A~%" error)
			       nil)
			     (t (error error)))))
	       (skip-package ()
			     (push package-name failed-packages)
			     nil)))))

(defun list-systems ()
  (let ((systems (make-hash-table :test #'equal)))
    (loop :for item :in asdf:*central-registry*
	  :for location = (eval item)
	  :for files = (when location
			 (directory
			  (merge-pathnames
			   (make-pathname  :version :newest :name :wild
					   :type "asd" :case :local)
			   location)))
	  :when files :do
	  (loop :for filename :in files
		:for system = (when filename (pathname-name filename))
		:do
		(setf (gethash system systems) system)))
    (format t
	    "~&Known systems:~%~@<~;:~A~_ ~;~:>~%"
	    (sort 
	     (loop :for system :being :the :hash-keys :of systems
		   :collect system)
	     #'string<))
    (values)))

#-ecl
(defun load-component (system)
  (asdf:operate 'asdf:compile-op system)
  (asdf:operate 'asdf:load-op system)
  nil)

#+ecl
(defun load-component (system)
  (asdf:operate 'asdf:compile-op system)
  (asdf:operate 'asdf:load-op system)
  (asdf::get-object-files system))

(defun load-user-image-components ()
  (with-open-file (components (merge-pathnames
			       *image-preferences*
			       *implementation-name*)
			      :direction :input :if-does-not-exist nil)
    (when components
      (let ((asdf:*central-registry*
	     (append asdf:*central-registry* (list *systems-root*))))
	(loop for component = (read-line components nil)
	      while component nconc
	      (let ((system (asdf:find-system component nil)))
		(if system (load-component system)
		    (warn "System ~S not found, not loading it into implementation image" component))))))))
