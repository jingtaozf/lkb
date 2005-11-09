;;; -*- Mode: LISP; Package: MAKE -*-

(in-package "COMMON-LISP-USER")

#+(and :unix (or :i386 :i686 :pentium3 :pentium4 :athlon))
(pushnew :linux *features*)
(pushnew :cltl2 *features*)

;;;
;;; load the portable defsystem() from CMU
;;;
#-:mk-defsystem
(load
 (make-pathname :directory general-dir :name "defsystem"))

(in-package "MAKE")

;;;
;;; the latest defsystem() defaults the object extension to `.so', which seems
;;; to break our ecl-compile-file() and load() interactions.   (18-feb-05; oe)
;;;
(setf *filename-extensions* (cons "lsp" "fas"))

;;;
;;; to avoid (bogus, i think) redefinition warning for print-object() method
;;; _extensions_.                                              (21-feb-05; oe)
;;;
#+:ecl
(si::package-lock "CL" nil)

;;;
;;; we use ECL primarily for embedded Lisp in PET; add some support for library
;;; creation.
;;;

(defparameter %object-files% nil)

(defparameter %object-stubs% nil)

(defun ecl-compile-file (source &key output-file)
  ;;
  ;; this is all slightly involved (and feels brittle across ECL releases :-{):
  ;; we need to both compile into `.fas' files that we can load incrementally
  ;; into the running Lisp (so as to get through the entire system, i presume),
  ;; plus object files that can be combined into a C-style library.
  ;;
  (compile-file source :output-file output-file :system-p nil :c-file t)
  (let ((file (make-pathname :device (pathname-device output-file)
                             :directory (pathname-directory output-file)
                             :name (pathname-name output-file)
                             :type "o")))
    (compile-file source :output-file file :system-p t :c-file t)
    (setf %object-files% (cons (namestring file) %object-files%)))
  (let ((name (string-upcase (pathname-name output-file))))
    (setf %object-stubs% (cons (substitute #\_ #\- name) %object-stubs%))))

;(setf c::*debug-compiler* t)
;(pop si::*exit-hooks*)


(define-language :lisp
  :compiler #'ecl-compile-file
  :loader #'load
  :source-extension (first *filename-extensions*)
  :binary-extension (rest *filename-extensions*))

(defun ecl-initialize ()
  (setf %object-files% nil)
  (setf %object-stubs% nil))

(defun ecl-finalize (&key (module "mrs")
                          (header (format 
                                   nil 
                                   "~a~a.h" 
                                   (namestring include-dir)
                                   module))
                          (library (format 
                                    nil 
                                    "~alib~a.a" 
                                    (namestring lib-dir) module)))
  
  (setf %object-files% (nreverse %object-files%))
  (setf %object-stubs% (nreverse %object-stubs%))
  (when (probe-file library) (delete-file library))
  (si:system (format 
              nil 
              "ar cq \"~a\" ~{\"~a\"~^ ~}" 
              library %object-files%))
  (with-open-file (stream header :direction :output :if-exists :supersede)
    (format
     stream
     "void~%~
      initialize_~a() {~%~%"
     module)
    (loop
        for stub in %object-stubs%
        do
          (format 
           stream
           "  extern void init_~a(cl_object);~%"
           stub))
    (loop
        for stub in %object-stubs%
        do
          (format 
           stream
           "  read_VV(OBJNULL, init_~a);~%"
           stub))
    (format stream "~%} /* initialize_~a() */~%" module)))

(defvar %binary-dir-name% 
  (or
   #+(and (or :i386 :i686 :pentium3 :pentium4 :athlon) :linux)
   ".lecl"
   #+(and (or :i386 :i686 :pentium3 :pentium4 :athlon) (not :unix))
   ".wecl"
   ".ecl"))

;;;
;;; determine the system type (in terms of hardware and os) in order to set
;;; `bin-dir' (the location of external platform-specific executables)
;;; accordingly (6-feb-96 -- oe@csli)
;;;
(defvar %system-binaries%
  #+(and (or :i386 :i686 :pentium3 :pentium4 :athlon) :unix) "linux.x86.32"
  #+(and :x86_64 :unix) "linux.x86.64"
  #+(and (or :i386 :i686 :pentium3 :pentium4 :athlon) (not :unix)) "windows"
  #-(or (or :i386 :i686 :pentium3 :pentium4 :athlon) 
        (and (not (or :i386 :i686 :pentium3 :pentium4 :athlon)) :unix))
  (error "~&loadup: unable to determine system type; see file ~
          `ecl-patches.lisp'.~%"))

