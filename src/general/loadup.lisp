;;; Copyright (c) 1991--2018
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `LICENSE' for conditions.

(in-package :common-lisp-user)

;;;
;;; set LKB version
;;;

(defvar *lkb-version* "5.5")

;;; see also below - *features*

(defparameter %athome% nil)
(defparameter %sys-home%
  (rest (butlast (pathname-directory *load-truename*) 2)))

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; --------------- END SITE-SPECIFIC INSTALLATION PARAMETERS -----------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter sys-home (make-pathname 
                        :host (pathname-host *load-truename*)
                        :device (pathname-device *load-truename*)
                        :directory (cons #-:lucid :absolute 
                                         #+:lucid :root %sys-home%)))

;;;
;;; the defsystem() :host and :device components are plain useless, since they
;;; are _not_ evaluated; to avoid having to patch `defsystem.lisp' (which comes
;;; dumped into at least CLISP and CMU-CL nowadays), use pathname defaults for
;;; :host and :device (certainly constant for all systems in an installation).
;;;
;;; _fix_me_
;;; unfortunately, for ACL 6.2, this is not enough (see `allegro-patches.lisp')
;;; --- need to test for (Open)MCL at least.                    (9-sep-03; oe)
;;;
(setf *default-pathname-defaults*
  (merge-pathnames (make-pathname :host (pathname-host *load-truename*)
                                  :device (pathname-device *load-truename*))))

(defparameter general-dir 
    (append 
     (cons #-:lucid :absolute #+:lucid :root %sys-home%) 
     '("src" "general")))

;;;
;;; load several patches together with the the cmu version of defpackage() for
;;; non-common lucid 4.x lisps (really widespread still in 1994 |:-{).

;;;
;;; analogously, we now have one patch file per Lisp environment that we know
;;; of; although these files are somewhat smaller these days, it seems we still
;;; require some small amount of vendor-specific tweaking.  (2-jun-00  -  oe)
;;;

#+:cmu
(load 
 (make-pathname :directory general-dir :name "cmucl-patches"))

#+:sbcl
(load 
 (make-pathname :directory general-dir :name "sbcl-patches"))

#+:lucid
(load 
 (make-pathname :directory general-dir :name "lucid-patches"))

#+:allegro
(load
 (make-pathname 
  :directory general-dir :name "allegro-patches"))

#+(and :mcl (not :openmcl))
(load
 (make-pathname :directory general-dir :name "mcl-patches"))

#+:openmcl
(load
 (make-pathname :directory general-dir :name "openmcl-patches"))

#+:clisp
(load
 (make-pathname :directory general-dir :name "clisp-patches"))

#+:ecl
(load
 (make-pathname :directory general-dir :name "ecl-patches"))

#+:lispworks
(load
 (make-pathname :directory general-dir :name "lispworks-patches"))

(load
 (make-pathname :directory general-dir :name "loadup-library"))

(in-package :make)

(reset-module-status)
(reset-system-paths)

