(in-package :user)

(load (make-pathname 
       :directory (append (butlast (pathname-directory *load-truename*))
			  '("general"))
       :name "loadup"))

(setq *building-image-p* t)
(setq make:*compile-during-load* t)
(load-system "mrs")
(setq *building-image-p* nil)

(setq excl:*restart-init-function* 
  #'(lambda ()
      (let ((*package* (find-package "CLIM-USER")))        
        (clim-user::set-up-lkb-interaction :core))))


