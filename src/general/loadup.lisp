;;; Copyright (c) 1991-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions


(in-package :common-lisp-user)

;;;
;;; set LKB version
;;;

(defvar *lkb-version* "5.4 (beta)")

;;; see also below - *features*

(defparameter %athome% nil)
(defparameter %sys-home%
  (rest (butlast (pathname-directory *load-truename*) 2)))

(defparameter %sys-host%
  (pathname-host *load-truename*))

(defparameter %sys-device%
  (pathname-device *load-truename*))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; --------------- END SITE-SPECIFIC INSTALLATION PARAMETERS -----------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter sys-home (make-pathname :host %sys-host%
                        :device %sys-device%
                        :directory (cons #-:lucid :absolute 
                                         #+:lucid :root %sys-home%)))

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
;;; require some small amount of vendor-specific tweaking (2-jun-00  -  oe)
;;;

#+:cmu
(load 
 (make-pathname :directory general-dir :name "cmucl-patches"))

#+:lucid
(load 
 (make-pathname :directory general-dir :name "lucid-patches"))

#+:allegro
(load
 (make-pathname :device %sys-device%
   :directory general-dir :name "allegro-patches"))

#+:mcl
(load
 (make-pathname :directory general-dir :name "mcl-patches"))

#+:clisp
(load
 (make-pathname :directory general-dir :name "clisp-patches"))

#+:lispworks
(load
 (make-pathname :host %sys-host%
                :device %sys-device%
                :directory general-dir :name "lispworks-patches"))

(load
 (make-pathname :host %sys-host% :device %sys-device%
   :directory general-dir :name "loadup-library"))

(in-package :make)

(reset-module-status)
(reset-system-paths)



