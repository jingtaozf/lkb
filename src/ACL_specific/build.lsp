;;; Copyright (c) 1998-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions


(in-package :common-lisp-user)

(load (make-pathname 
       :device (pathname-device *load-truename*)
       :host (pathname-host *load-truename*)
       :directory (append (butlast (pathname-directory *load-truename*))
			  '("general"))
       :name "loadup"))

;;;
;;; these are copied from ACL 6.0 `develenv.cl', with those modules that cannot
;;; be included in a runtime image deleted.                   (26-sep-01; oe)
;;;
(require :list2)
(require :seq2)
#-mswindows (require :safeseq)
(require :regexp)
(require :streama)
(require :srecord)
(require :tpl-debug)
(require :tpl-proc)
(require :foreign)
(require :defftype)
(require :process)
#-mswindows (require :sigio)
#-mswindows (require :excl)
(require :eli)
(require :emacs)
(require :lze)
(require :lep)
(require :scm)
(require :walker)
(require :trace)
(require :inspect)
(require :sock)
(require :loop)
(require :regexp)
#-mswindows (require :constructor)
#-mswindows (require :mcombin)
#-mswindows (require :uri)

(setq make::*building-image-p* t)
(setq make:*compile-during-load* t)
(compile-system "mrs" :force t)
(setq make::*building-image-p* nil)

(setq excl:*restart-init-function* 
  #'(lambda ()
      (tpl:setq-default *package* (find-package :lkb))
      (let ((*package* (find-package "CLIM-USER")))
	(clim-user::set-up-lkb-interaction :core))))

;;(sys:write-codevectors "/tmp/lkb.cvr")

