(in-package :user)

(load (make-pathname 
       :directory (append (butlast (pathname-directory *load-truename*))
			  '("general"))
       :name "loadup"))

(setq *building-image-p* t)
(setq make:*compile-during-load* t)
;;(sys:record-strings "/tmp/lkb.str" 
;;  (load-system "mrs"))
(compile-system "mrs" :force t)
(setq *building-image-p* nil)

(setq excl:*restart-init-function* 
  #'(lambda ()
      (let ((*package* (find-package "CLIM-USER")))
	(clim-user::set-up-lkb-interaction :core))))

;;(sys:write-codevectors "/tmp/lkb.cvr")

