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

(defvar *lkb-version* "5.2")

;;; see also below - *features*

(defparameter %athome% nil)
(defparameter %sys-home%
  #-:mcl (rest (butlast (pathname-directory *load-truename*) 2))
  #+:mcl '("macintosh hd" "newlkb"))

;;; #+mcl (defpackage :common-lisp-user (:nicknames :user :cl-user))

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
;;; AAC - turned this into a pathname, so it can include the device

(defparameter general-dir (append 
                            (cons #-:lucid :absolute #+:lucid :root %sys-home%) 
                            '("src" "general")))

;;;
;;; load several patches together with the the cmu version of defpackage() for
;;; non-common lucid 4.x lisps (really widespread still in 1994 |:-{).
;;;

;;;
;;; the patches for ``belgianing lucid incompatibilities'' (bernd kiefer |:-) 
;;; (censored by aac@csli ...) have
;;; now moved into a file of their own ... rip (5-jun-1994 -- oe@csli)
;;;
;;; and the ones for allegro and mcl too... (4-jul-1994 -- bk@dfki)
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

(in-package "MAKE")

(let ((dumping
       (make-pathname :device cl-user::%sys-device% 
         :directory general-dir :name "dump" :type "lisp")))
  (when (probe-file dumping) (load dumping)))

(reset-module-status)
(reset-system-paths)

(pushnew :lkb *features*)
(pushnew :lkb-v5.2 *features*)
#+(or :cl-http (not (or :mcl :clim :common-graphics))) 
   (pushnew :tty *features*)

;;; graphics currently assumes mcl or clim,
;;; with common graphics under development
;;; do (pushnew :tty *features*) manually
;;; if you want to use the tty version from mcl or acl/clim
;;; for some reason



