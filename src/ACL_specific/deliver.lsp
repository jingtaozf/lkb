;; Create a runtime image suitable for distribution

(in-package "COMMON-LISP-USER")

(delete-directory-and-files
 (make-pathname :directory (append sys-home '("bin" "lkb"))))

(excl:generate-application 
 "lkb" 
 (make-pathname :directory (append sys-home '("bin" "lkb")))
 (list :srecord
       :eli
       :sock
       (make-pathname :directory (append sys-home '("src" "ACL_specific"))
		      :name "build.lsp"))
 
 :opt-speed 3
 :newspace 3145728			; 3 meg newspace
 :oldspace 10485760			; 10 meg oldspace 
 
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
 :presto-build-lib (make-pathname 
		    :directory (append sys-home '("bin" "lkb"))
		    :name "lkb"))

