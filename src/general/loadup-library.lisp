;;; -*- Mode: LISP; Package: MAKE -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: loadup-library.lisp
;;;      module: DISCO loadup environment, local library functions
;;;     version: 2.1 -- 26-jul-1994
;;;  written by: bernd kiefer, dfki saarbruecken
;;; last update: 26-jul-1994
;;;  updated by: bk, dfki saarbruecken
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAKE")

;;;
;;; chances are we have a modern eval-when() ... i doubt it (11-jul-94 -- oe)
;;;

(eval-when (:execute :load-toplevel :compile-toplevel)
  (pushnew :ansi-eval-when *features*))

;;;
;;; the change in functionp() behaviour is most annoying
;;;

(unless (functionp :foo)
  (pushnew :ansi-functionp *features*))

;;;
;;; set up the rest of the DISCO loadup environment
;;;
 
(eval-when #+:ansi-eval-when (:load-toplevel :compile-toplevel :execute)
	   #-:ansi-eval-when (load eval compile)
  (import
    '(common-lisp-user::*page-version*
      common-lisp-user::%athome%
      common-lisp-user::sys-home
      common-lisp-user::general-dir)
    "MAKE"))

(defparameter *modstat-file-name* "modstat")
(defparameter *version-file-name* "versions")

;;;
;;; some handy utility functions manipulating structured directories
;;;

;;; a function combining a directory description and a file name and
;;; (optional) a file type

(defun dir-and-name (dir name &optional type)
  (if (pathnamep dir)
      (let ((name (pathname name)))
	(make-pathname
	 :host (or (pathname-host dir) (pathname-host name))
	 :device (or (pathname-device dir) (pathname-device name))
	 :directory (pathname-directory dir)
	 :name (pathname-name name)
	 :type (or type (pathname-type dir) (pathname-type name))))
    (progn
      (unless (listp dir) (setq dir (pathname-directory (pathname dir))))
      (if (pathnamep name)
	  (make-pathname
	   :host (pathname-host name) :device (pathname-device name)
	   :directory dir
	   :name (pathname-name name)
	   :type (or type (pathname-type name)))
	(let ((name (pathname name)))
	  (make-pathname
	   :host (pathname-host name) :device (pathname-device name)
	   :directory dir
	   :name (pathname-name name)
	   :type (or type (pathname-type name))))))))


  
;;; a function appending the second directory path to the first. It
;;; assumes that pathname-directory returns a cltl2 compatible
;;; directory-list even if a pathname with a string argument as
;;; directory specification was given

(defun dir-append (pathname directory)
  (let ((dir1 (if (pathnamep pathname)
		  (pathname-directory pathname)
		  (pathname-directory (make-pathname :directory pathname))))
	(dir2 (if (pathnamep directory)
		  (pathname-directory directory)
		  (pathname-directory (make-pathname :directory directory)))))
    (cond
      ((pathnamep pathname)
       (make-pathname
	:host (pathname-host pathname) :device (pathname-device pathname)
	:directory (if (eq (first dir2) :relative)
		       (append dir1 (rest dir2))
		       (append dir1 dir2))
	:name (pathname-name pathname) :type (pathname-type pathname)
	:version (pathname-version pathname)))
      ((or (consp pathname) (stringp pathname))
       (make-pathname 
	:directory
	(cond
	 ((eq (first dir2) :relative) (append dir1 (rest dir2)))
	 ((eq (first dir2) :absolute)
	  (error "Second directory path starts with :ABSOLUTE"))
	 (T (append dir1 dir2))))))))

;;;
;;; extensions to defsystem() to allow for new load and compile operations
;;;

(defun gen-compile-and-load-operation (component force
				       &optional
				       (compile-fn #'compile-file)
				       (load-fn #'load))
  ;; FORCE was CHANGED. this caused defsystem during compilation to only
  ;; load files that it immediately compiled.
  (let ((changed
	 (gen-compile-file-operation component force compile-fn)))
    ;; Return T if the file had to be recompiled and reloaded.
    (if (and changed (component-compile-only component))
	;; For files which are :compile-only T, compiling the file
	;; satisfies the need to load. 
	changed
	;; If the file wasn't compiled, or :compile-only is nil,
	;; check to see if it needs to be loaded.
      (and (gen-load-file-operation component force compile-fn load-fn)
					; FORCE was CHANGED ???
	   changed))))

(defun gen-compile-file-operation (component force compile-fn)
  ;; Returns T if the file had to be compiled.
  (let ((must-compile
	 ;; For files which are :load-only T, loading the file
	 ;; satisfies the demand to recompile.
	 (and (null (component-load-only component)) ; not load-only
	      (or (find force '(:all :new-source-all t) :test #'eq) 
		  (and (find force '(:new-source :new-source-and-dependents)
			     :test #'eq)
		       (needs-compilation component))))))

    (cond ((and must-compile
		(probe-file (component-full-pathname component :source)))
	   (with-tell-user ("Compiling source" component :source)
	     (or *oos-test*
		 (funcall 
		  compile-fn
		  (component-full-pathname component :source)
		  :output-file (component-full-pathname component :binary)
		  )))
	   must-compile)
	  (must-compile
	   (tell-user "Source file not found. Not compiling"
		      component :source :no-dots :force)
	   nil)
	  (t nil))))




;;; Need to completely rework this function...
(defun gen-load-file-operation (component force
				&optional (compile-fn #'compile-file)
					  (load-fn #'load))
  ;; Returns T if the file had to be loaded
  (let* ((binary-pname (component-full-pathname component :binary))
	 (source-pname (component-full-pathname component :source))
	 (binary-exists (probe-file binary-pname))
	 (source-exists (probe-file source-pname))
	 (source-needs-loading (needs-loading component t nil))
	 (binary-needs-loading (needs-loading component nil t))
	 ;; needs-compilation has an implicit source-exists in it.
	 (needs-compilation (if (component-load-only component)
				source-needs-loading
				(needs-compilation component)))
	 (check-for-new-source 
	  ;; If force is :new-source*, we're checking for files
	  ;; whose source is newer than the compiled versions.
	  (find force '(:new-source :new-source-and-dependents :new-source-all)
		:test #'eq))
	 (load-binary (or (find force '(:all :new-source-all t) :test #'eq)
			  binary-needs-loading))
	 (load-source
	  (or *load-source-instead-of-binary*
	      (and load-binary (component-load-only component))
	      (and check-for-new-source needs-compilation)))
	 (compile-and-load
	  (and needs-compilation (or load-binary check-for-new-source)
	       (compile-and-load-source-if-no-binary component))))
    ;; When we're trying to minimize the files loaded to only those
    ;; that need be, restrict the values of load-source and load-binary
    ;; so that we only load the component if the files are newer than
    ;; the load-time.
    (when *minimal-load*
      (when load-source (setf load-source source-needs-loading))
      (when load-binary (setf load-binary binary-needs-loading)))
    (when (or load-source load-binary compile-and-load)
      (cond (compile-and-load
	     ;; If we're loading the binary and it is old or nonexistent,
	     ;; and the user says yes, compile and load the source.
	     (gen-compile-file-operation component t compile-fn)
	     (with-tell-user ("Loading binary"   component :binary)
	       (or *oos-test*
		   (progn
		     (funcall load-fn binary-pname)
		     (setf (component-load-time component)
			   (file-write-date binary-pname)))))
	     T)
	    ((and source-exists
		  (or (and load-source	; implicit needs-comp...
			   (or *load-source-instead-of-binary*
			       (component-load-only component)
			       (not *compile-during-load*)))
		      (and load-binary (not binary-exists)
			   (load-source-if-no-binary component))))
	     ;; Load the source if the source exists and:
	     ;;   o  we're loading binary and it doesn't exist
	     ;;   o  we're forcing it
	     ;;   o  we're loading new source and user wasn't asked to compile
	     (with-tell-user ("Loading source" component :source)
	       (or *oos-test*
		   (progn 
		     (funcall load-fn source-pname)
		     (setf (component-load-time component)
			   (file-write-date source-pname)))))
	     T)
	    ((and binary-exists load-binary)
	     (with-tell-user ("Loading binary"   component :binary)
	       (or *oos-test*
		   (progn
		     (funcall load-fn binary-pname)
		     (setf (component-load-time component)
			   (file-write-date binary-pname)))))
	     T)
	    ((and (not binary-exists) (not source-exists))
	     (tell-user-no-files component :force)
	     (when *files-missing-is-an-error*
	       (cerror "Continue, ignoring missing files."
		       "~&Source file ~S ~:[and binary file ~S ~;~]do not exist."
		       (namestring source-pname)
		       (or *load-source-if-no-binary* 
			   *load-source-instead-of-binary*)
		       (namestring binary-pname)))
	     nil)
	    (t 
	     nil)))))


;; another custom load or compile function that simply checks the availability
;; of a file
(defun file-present-p (c f)
  (declare (ignore f))
  (format T ";; searching ~a: ~a~%"
	  (component-full-pathname c :source)
	  (if (probe-file (component-full-pathname c :source))
	      "found" "MISSING"))
  NIL)


;;;
;;; the DISCO loadup environment used in the DFKI CL department (at home).
;;;
;;; in the ongoing DISCO development we often want to keep several version of
;;; some module (a defsystem() size unit) simultaneously; the assignment of
;;; status values (`:test', `:licensed', `:external' and `:local' constitute
;;; the core set) makes module mangement easier (thanx to bernd kiefer |:-).
;;;

;;;
;;; In an external installation there is no need for the status system.
;;;

(eval-when #+:ansi-eval-when (:load-toplevel :compile-toplevel :execute)
	   #-:ansi-eval-when (load eval compile)
  (setq
    *central-registry* 
    (if %ATHOME%
      (list
       (dir-append sys-home '(:relative "src" "test" "systems"))
       (dir-append sys-home '(:relative "src" "licensed" "systems"))
       (dir-append sys-home '(:relative "src" "external" "systems")))
      (list (dir-append sys-home '(:relative "src" "systems"))))))

(defparameter *directories-list* 
  (if %ATHOME%
      `((:external
	 (:relative "src" "external")
	 (:relative "src" "external" ,%BINARY-DIR-NAME%))
	(:licensed
	 (:relative "src" "licensed")
	 (:relative "src" "licensed" ,%BINARY-DIR-NAME%))
	(:test
	 (:relative "src" "test")
	 (:relative "src" "test" ,%BINARY-DIR-NAME%))
	(:demo
	 (:relative "src" "demo")
	 (:relative "src" "demo" ,%BINARY-DIR-NAME%))
	(:cosma
	 (:relative "src" "cosma")
	 (:relative "src" "cosma" ,%BINARY-DIR-NAME%))
	)
    `((:licensed
       (:relative "src")
       (:relative "src" ,%BINARY-DIR-NAME%)))))

(defparameter *module-status* NIL
  "This variable is an assoc-list that can be used to set the status
   of modules to :test or :local (or some other weird status). If 
   the module can not be found in this list, it is assumed to be 
   a licensed module")

(defun reset-module-status (&optional (deffile T))
  (setq *module-status* NIL)
  (when deffile
    (when (or (probe-file
	       (dir-and-name general-dir *modstat-file-name* "lisp"))
	      (probe-file
	       (dir-and-name general-dir *modstat-file-name* "fasl")))
      (load (dir-and-name general-dir *modstat-file-name*)))
    (when (or (probe-file
	       (dir-and-name general-dir *version-file-name* "lisp"))
	      (probe-file
	       (dir-and-name general-dir *version-file-name* "fasl")))
      (load (dir-and-name general-dir *version-file-name*)))))

(defmacro get-module-status (modulename)
  `(rest (assoc ,modulename *module-status* :test #'string=)))

(defun set-module-status (modulename status)
  (if (get-module-status modulename)
      (setf (get-module-status modulename) status)
    (push (cons modulename status) *module-status*)))

(defparameter *module-versions* NIL
  "This variable is an assoc-list that can be used to store the version number
of modules. This is very important if lisp images are used because otherwise
the module versions used in an image can not be identified. The list contains
lists of the form (<modulename> <version no.> <:active-or-NIL>)")

(defmacro get-module-version (modulename)
  `(rest (assoc ,modulename *module-versions* :test #'string=)))

(defun set-module-version (modulename version &optional active)
  (if (get-module-version modulename)
      (setf (get-module-version modulename) (list version active))
    (push (list modulename version active) *module-versions*)))

(defun get-active-versions ()
  (loop for version-list in *module-versions*
      when (eq (third version-list) :active)
      collect (cons (first version-list) (second version-list))))

(defun get-sys-dir (modulename which)
  (set-module-version modulename
		      (or (first (get-module-version modulename)) "0.0")
		      :active)
  (let ((dir (nth which
		  (or (assoc (or (get-module-status modulename) :licensed)
			     *directories-list* :test #'string=)
		      (first *directories-list*))
		  )))
    (if (eq (first (if (listp dir) dir
		     (pathname-directory (pathname dir))))
	    :relative)
	(dir-append sys-home dir)
      dir)))

(defun get-sources-dir (modulename) (get-sys-dir modulename 1))
(defun get-binaries-dir (modulename) (get-sys-dir modulename 2))


;;; these paths will be set by reset-system-paths()

(defparameter bin-dir NIL)
(defparameter tmp-dir NIL)
(defparameter grammar-dir NIL)
(defparameter *source-grammar* NIL)

(defun reset-system-paths ()
  (setq tmp-dir 
    #+(or (and :allegro :mswindows (version>= 5 0)) :mcl) 
      (dir-append sys-home '(:relative "tmp"))
    #-(or :mcl (and :allegro :mswindows (version>= 5 0)))
    (dir-append (user-homedir-pathname) '(:relative "tmp")))
  (setq bin-dir
	(dir-append sys-home (list :relative "bin" %system-binaries%)))
  (setq grammar-dir
	(dir-append (get-sources-dir "grammar") '(:relative "grammar"))))

(defparameter *modules-loaded* NIL)
(defparameter *no-module-greetings* NIL)

(defun module-loaded-p (module)
  (or *no-module-greetings*
      (find module *modules-loaded* :test #'equal)
      (not (push module *modules-loaded*))))

(defparameter *WARNING-LEVEL* 3
  "This variable should be used by all systems in order to decide which
warnings to print. 0: no warnings,
                   1: only severe warnings,
                   2: warnings about potential problems,
                   3: all warnings")

(defmacro level-warn-p (level)
  `(>= *warning-level* ,level))

(eval-when #+:ansi-eval-when (:load-toplevel :compile-toplevel :execute)
	   #-:ansi-eval-when (load eval compile)
  (use-package "MAKE" "COMMON-LISP-USER")
  (export
   '(dir-and-name
     dir-append
     get-sources-dir get-binaries-dir
     set-module-status get-module-status
     set-module-version get-module-version get-active-versions
     reset-module-status
     reset-system-paths
     *directories-list*
     module-loaded-p
     general-dir tmp-dir bin-dir *source-grammar*
     gen-compile-and-load-operation
     gen-load-file-operation
     run-process
     *warning-level*
     level-warn-p)
   "MAKE")
  (pushnew :disco-loadup *features*))

