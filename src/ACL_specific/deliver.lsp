;; Create a runtime image suitable for distribution

(in-package "COMMON-LISP-USER")

(delete-directory-and-files (merge-pathnames #p"bin/lkb/" sys-home))

(excl:generate-application 
 "lkb" 
 (merge-pathnames #p"bin/lkb/" sys-home)
 (list :srecord
       :eli
       :sock
       (merge-pathnames #p"src/ACL_specific/build.lsp" sys-home))
 
 :opt-speed 3
 :newspace 3145728			; 3 meg newspace
 :oldspace 10485760			; 10 meg oldspace 
 :lisp-heap-size (* 1024 1024 1024) ; 1 gbyte maximal size
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


