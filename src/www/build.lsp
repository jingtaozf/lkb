(in-package :user)

(load (make-pathname 
       :directory (append (butlast (pathname-directory *load-truename*))
			  '("general"))
       :name "loadup"))

(setq http::*server-maintainer* "malouf@csli.stanford.edu") 
(setq http:*bug-http-server* "malouf@csli.stanford.edu") 
(setq http:*server-mail-address* "ergo-www@eoan.stanford.edu") 
(setq smtp:*network-mail-host* "mail-csli.stanford.edu")

(setq *building-image-p* t)
(setq make:*compile-during-load* t)
(load-system "www")
(setq *building-image-p* nil)

(defun lkb-tmp-dir nil 
  '#p"/usr/local/etc/www/htdocs/tmp/")

(setq excl:*restart-init-function* 
  #'(lambda ()
      (read-script-file-aux "/usr/local/etc/www/htdocs/grammar/lkb/script")
      (index-for-generator)
      (http:start)
      (mp:process-disable mp:*current-process*)))

(excl:gc t)

(dumplisp :name "/eo/e4/malouf/acl/wwwlkb.dxl")

