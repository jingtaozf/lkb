(in-package "COMMON-LISP-USER")

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

