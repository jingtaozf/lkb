;; Create a runtime image suitable for distribution

(in-package :common-lisp-user)

(let ((target (dir-append sys-home (list :relative mk::%system-binaries%))))
  (delete-directory-and-files target)

  (excl:generate-application 
   "lkb" 
   target
   (list :srecord
         :eli
         :sock
         (merge-pathnames #p"src/ACL_specific/build.lsp" sys-home))

   :opt-speed 3
   :newspace (* 1024 1024 3)
   :oldspace (* 1024 1024 10)
   :lisp-heap-size (* 1024 1024 #-:mswindows 1024 #+:mswindows 768)
   ;;
   ;; change heap placement to allow immense newspace growth (21-may-00  -  oe)
   ;;
   #+:linux86 :lisp-heap-start #+:linux86 "1040M"
   #+:linux86 :c-heap-start #+:linux86 "2816M"
   #+(and :sparc :solaris2) :c-heap-start #+(and :sparc :solaris2) #xe0000000

   :runtime :standard 
   #+(version>= 6 0) :runtime-bundle #+(version>= 6 0) t
   :include-clim t
   :include-compiler t

   :include-tpl t
   :include-debugger nil
   :include-devel-env nil
   :include-ide nil
   :include-common-graphics nil
   :include-composer nil
   :include-xcw nil

   :discard-compiler t
   :discard-arglists t
   :discard-local-name-info t
   :discard-source-file-info t

   :load-xref-info nil
   :record-xref-info nil
   :discard-xref-info nil

   :print-startup-message nil

   :presto t
   :presto-build-lib (dir-and-name target "lkb.lib")))




