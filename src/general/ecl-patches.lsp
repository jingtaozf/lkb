;;; -*- Mode: LISP; Package: MAKE -*-

(in-package "COMMON-LISP-USER")

#+(and :unix :i686) (pushnew :linux *features*)
(pushnew :cltl2 *features*)

;;;
;;; load the portable defsystem() from CMU
;;;
#-:mk-defsystem
(load
 (make-pathname :directory general-dir :name "defsystem"))

(in-package "MAKE")


;;;
;;; we use ECL primarily for embedded Lisp in PET; add some support for library
;;; creation.
;;;

(defparameter %object-files% nil)

(defparameter %object-stubs% nil)

(defun ecl-compile-file (source &key output-file)
  (compile-file source :output-file output-file :system-p nil :c-file t)
  (compile-file source :output-file output-file :system-p t :c-file t)
  (let ((file (make-pathname :device (pathname-device output-file)
                             :directory (pathname-directory output-file)
                             :name (pathname-name output-file)
                             :type "o")))
    (setf %object-files% (cons (namestring file) %object-files%)))
  (let ((name (string-upcase (pathname-name output-file))))
    (setf %object-stubs% (cons (substitute #\_ #\- name) %object-stubs%))))

(define-language :lisp
  :compiler #'ecl-compile-file
  :loader #'load
  :source-extension (car *filename-extensions*)
  :binary-extension (cdr *filename-extensions*))

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
   #+(and :i686 :linux) ".lecl"
   #+(and :i686 (not :unix)) ".wecl"
   ".ecl"))

;;;
;;; determine the system type (in terms of hardware and os) in order to set
;;; `bin-dir' (the location of external platform-specific executables)
;;; accordingly (6-feb-96 -- oe@csli)
;;;
(defvar %system-binaries%
  #+(and :i686 :unix) "linux"
  #+(and (not :i686) :unix) "ppc"
  #+(and :i686 (not :unix)) "windows"
  #-(or (and :unix :i686))
  (error "~&loadup: unable to determine system type; see file ~
          `ecl-patches.lisp'.~%"))
