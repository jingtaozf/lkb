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
(defparameter %libbuild% nil)

(defun ecl-compile-file (source &key output-file)
  (if %libbuild%
      ;; do it the ECL way: only produce the object file and load the lisp
      ;; file, which is even faster than the old version producing fasl an
      ;; object file and loading the fasl
      ;; Besides, it has the advantage that the *init-function-prefix* can be
      ;; used, which breaks the fasls, but allows us to have different modules
      ;; with equal file names -- bk March 17 2006
      (let ((file (compile-file-pathname output-file :type :object)))
        (compile-file source :output-file file :system-p t ;; :c-file t)
                      )
        ;; collect the object files that are later combined into a library by
        ;; ecl-finalize
        (push file %object-files%))
    ;; operate ECL in `normal' lisp/compiler mode
    (compile-file source :output-file output-file :system-p nil ;; :c-file t
                  )))

(define-language :lisp
  :compiler #'ecl-compile-file
  :loader #'load
  :source-extension (first *filename-extensions*)
  :binary-extension (rest *filename-extensions*))

(defun ecl-initialize-libbuild (&key module)
  (when module
    (setq si::*init-function-prefix* (string module)))
  (setf %libbuild% t)
  (setf %object-files% nil)
  ;; if we produce object files, we simply load the sources (see above)
  (setf mk::*load-source-if-no-binary* t))

(defun ecl-finalize-libbuild (&key (module "mrs")
                                   (lib-type :static-library)
                                   (library (compile-file-pathname 
                                             (dir-and-name lib-dir module)
                                             :type lib-type)))
  (setf %libbuild% nil)
  (when (probe-file library) (delete-file library))
  (funcall (if (eq lib-type :shared-library)
               #'c:build-shared-library
             #'c:build-static-library)
           library
           :lisp-files (reverse %object-files%)
           :init-name (format nil "initialize_ecl~a" module)))

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

