(in-package "COMMON-LISP-USER")

;;;
;;; because ann used the term `type' the way she does well before it became a
;;; bloody part of the bloody common-lisp language |:-}.
;;;
#+:allegro 
(setf excl:*enable-package-locked-errors* nil)

(defparameter *lkb-source-dir* (pathname-directory mk::tmp-dir))

(defparameter *psorts-temp-file* 
  (make-pathname :name "templex" 
                 :directory (pathname-directory mk::tmp-dir))
   "a temporary file for the lexicon")

(defparameter *psorts-temp-index-file* 
  (make-pathname :name "templex-index" 
                 :directory (pathname-directory mk::tmp-dir))
   "a file to index the lexicon")

(import '(enable-type-interactions disable-type-interactions))

#+(and allegro (not allegro-v5.0)) (setq tk-silica::*use-clim-gc-cursor* t)

(defpackage "MRS" (:use "COMMON-LISP" "MAKE"))
(defpackage "MAIN" (:use "COMMON-LISP" "MAKE"))

