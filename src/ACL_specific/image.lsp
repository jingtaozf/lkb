;;; Copyright (c) 1998-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions


;; Create a development image

(in-package :common-lisp-user)

;;(delete-directory-and-files (merge-pathnames #p"bin/lkb/" sys-home))

(let ((*record-source-file-info* t)
      (*load-source-file-info* t))
  
  (excl:build-lisp-image
   "/usr/groups/nltools/lingo/images/linux/lkb.dxl"

   :lisp-files (list :srecord
		     :eli
		     (merge-pathnames #p"src/ACL_specific/build.lsp" 
				      sys-home))
   
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
;   :include-common-graphics nil
   :include-composer t
   :include-xcw t

   :discard-compiler nil
   :discard-arglists nil
   :discard-local-name-info nil
   :discard-source-file-info nil
   :discard-xref-info nil
   :print-startup-message t
   
   :presto t
   :presto-lib "/usr/groups/nltools/lingo/images/linux/lkb.lib"
   ))

