;; Create a development image

(in-package "COMMON-LISP-USER")

(delete-directory-and-files
 (make-pathname :directory (append sys-home '("bin" "lkb"))))

(let ((*record-source-file-info* t)
      (*load-source-file-info* t))
  
  (excl:generate-application 
   "lkb" 
   (make-pathname :directory (append sys-home '("bin" "lkb")))
   (list :srecord
	 (make-pathname :directory (append sys-home '("src" "ACL_specific"))
			:name "build.lsp"))

   :opt-speed 3
   :newspace 83886080			; 80 meg newspace
   :oldspace 78643200			; 75 meg oldspace
   
   :runtime nil

   :include-clim t
   :include-compiler t

   :include-tpl t
   :include-debugger t
   :include-devel-env t
   :include-ide nil
   :include-common-graphics nil
   :include-composer t
   :include-xcw t

   :discard-compiler nil
   :discard-arglists nil
   :discard-local-name-info nil
   :discard-source-file-info nil
   :discard-xref-info nil
   :print-startup-message t
   
   ))

