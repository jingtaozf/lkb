;; Create a runtime image suitable for distribution

(in-package "COMMON-LISP-USER")

(delete-directory-and-files 
 (dir-append sys-home (list :relative mk::%system-binaries%)))

(excl:generate-application 
 "lkb" 
 (dir-append sys-home (list :relative mk::%system-binaries%))
 (list :srecord
       :eli
       :sock
       (merge-pathnames #p"src/ACL_specific/build.lsp" sys-home))
 
 :opt-speed 3
 :newspace (* 1024 1024 3)
 :oldspace (* 1024 1024 10)
 :lisp-heap-size (* 1024 1024 1024)
 ;;
 ;; change heap placement on Linux to allow newspace growth (21-may-00   -  oe)
 ;;
 #+:linux86 :lisp-heap-start #+:linux86 "1040M"
 #+:linux86 :c-heap-start #+:linux86 "2816M"
 :runtime :standard 
 
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
 :presto-build-lib (merge-pathnames #p"bin/lkb/lkb" sys-home))


