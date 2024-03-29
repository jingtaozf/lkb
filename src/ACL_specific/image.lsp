;;; Copyright (c) 1998-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see LICENSE for conditions


;; Create a development image

(in-package :common-lisp-user)

;;(delete-directory-and-files (merge-pathnames #p"bin/lkb/" sys-home))

(let ((*record-source-file-info* t)
      (*load-source-file-info* t)
      (target (dir-append sys-home (list :relative (format nil "image/~a" mk::%system-binaries%)))))
  
  (excl:build-lisp-image
   (merge-pathnames "lkb.dxl" target)

   :lisp-files (list :srecord
		     :eli
		     (merge-pathnames #p"src/ACL_specific/build.lsp" 
				      sys-home))
   
   :opt-speed 3
   :newspace (* 1024 1024 256)
   :oldspace (* 1024 1024 128)
   :lisp-heap-size (* 1024 1024 #-:mswindows 768 #+:mswindows 512)
   ;;
   ;; change heap placement to allow immense newspace growth (21-may-00  -  oe)
   ;;
   #+:linux86 :lisp-heap-start 
   #+(and :linux86 (not (version>= 6 2))) "1040M" 
   #+(and :linux86 (version>= 6 2)) "1088M"
   #+(and :sparc :solaris2) :c-heap-start #+(and :sparc :solaris2) #xe0000000
 
   
   :runtime nil
   
   :include-clim t
   :include-compiler t
   
   :include-tpl t
   :include-debugger t
   :include-devel-env t
   :include-ide nil
   #-(or :mswindows (version>= 6 2)) :include-common-graphics
   #-(or :mswindows (version>= 6 2)) nil
   :include-composer t
   :include-xcw t

   :discard-compiler nil
   :discard-arglists nil
   :discard-local-name-info nil
   :discard-source-file-info nil
   :discard-xref-info nil
   :print-startup-message t
   
   :presto t
   :presto-lib (merge-pathnames "lkb.lib")
   ))

