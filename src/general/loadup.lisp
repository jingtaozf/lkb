;;; -*- Mode: LISP; Package: COMMON-LISP-USER -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: loadup.lisp
;;;      module: unified LKB and PAGE loadup environment
;;;     version: 2.1 -- 26-jul-1994
;;;  written by: bernd kiefer, dfki saarbruecken
;;; last update: 14-aug-98
;;;  updated by: oe, csli stanford
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "COMMON-LISP-USER")

;;;
;;; set LKB version
;;;

(defvar *lkb-version* "1.0")

(defparameter %athome% nil)
(defparameter %sys-home%
  #-:mcl (rest (butlast (pathname-directory *load-truename*) 2))
  #+:mcl '("macintosh hd" "lkb"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; --------------- END SITE-SPECIFIC INSTALLATION PARAMETERS -----------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter sys-home (cons #-:lucid :absolute #+:lucid :root %sys-home%))

(defparameter general-dir (append sys-home '("src" "general")))

;;;
;;; load several patches together with the the cmu version of defpackage() for
;;; non-common lucid 4.x lisps (really widespread still in 1994 |:-{).
;;;

;;;
;;; the patches for ``fucking lucid incompatibilities'' (bernd kiefer |:-) have
;;; now moved into a file of their own ... rip (5-jun-1994 -- oe@csli)
;;;
;;; and the ones for allegro and mcl too... (4-jul-1994 -- bk@dfki)
;;;

#+:lucid
(load 
 (make-pathname :directory general-dir :name "lucid-patches"))

#+:allegro
(load
 (make-pathname :directory general-dir :name "allegro-patches"))

#+:mcl
(load
 (make-pathname :directory general-dir :name "mcl-patches"))

#+:clisp
(load
 (make-pathname :directory general-dir :name "clisp-patches"))

(load
 (make-pathname :directory general-dir :name "loadup-library"))

(in-package "MAKE")

(let ((dumping
       (make-pathname :directory general-dir :name "dump" :type "lisp")))
  (when (probe-file dumping) (load dumping)))

(reset-module-status)
(reset-system-paths)

(pushnew :lkb *features*)
