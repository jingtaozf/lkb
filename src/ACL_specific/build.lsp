;;; Copyright (c) 1998--2002
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.


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
#+(version>= 6 0) (require :safeseq)
(require :regexp)
(require :streama)
(require :srecord)
(require :tpl-debug)
(require :tpl-proc)
(require :foreign)
(require :defftype)
(require :process)
#+(and (version>= 6 0) (not :mswindows)) (require :sigio)
#+(version>= 6 0) (require :excl)
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
#+(version>= 6 0) (require :constructor)
#+(version>= 6 0) (require :mcombin)
#+(version>= 6 0) (require :uri)
#+(version>= 6 2) (require :euc)
#+(version>= 6 2) (require :ffcompat)

;;;
;;; [spr27650] apparently the runtime bundle does not include (all) external
;;; formats; to work around that, for now, preload everything. (22-may-03; oe)
;;;
#+(version>= 6 2)
(loop
    for ef in '("1250" "1251" "1252" "1253" "1254" "1255" "1256" "1257" "1258"
                "874" "932" "936" "949" "950" "big5" "crcrlf" "crlf" "e-cr" 
                "e-crcrlf" "e-crlf" "emacs-mule" "euc" "fat" "gb2312" 
                "iso-2022-jp" "iso8859-1" "iso8859-14" "iso8859-15" "iso8859-2"
                "iso8859-3" "iso8859-4" "iso8859-5" "iso8859-6" "iso8859-7" 
                "iso8859-8" "iso8859-9" "jis" "koi8-r" "latin-14" "latin-15" 
                "latin-2" "latin-3" "latin-4" "latin-5" "latin-6" "latin-7" 
                "latin-8" "latin-9" "latin1" "latin14" "latin15" "latin2"
                "latin3" "latin4" "latin5" "latin6" "latin7" "latin8" "latin9"
                "shiftjis" "ujis" "unicode" "utf8" "void")
    do (excl::find-external-format (intern ef :keyword)))

(setq make::*building-image-p* t)
(setq make:*compile-during-load* t)
(pushnew :lkb *features*)
(pushnew :mrs *features*)
(when (system:getenv "MT")
  (pushnew :mt *features*))
(compile-system "mrs" :force t)
(setq make::*building-image-p* nil)

(setq excl:*restart-init-function* 
  #'(lambda ()
      (if (system:getenv "SSP")
        (tpl:setq-default *package* (find-package :ssp))
        (tpl:setq-default *package* (find-package :lkb)))
      (let ((*package* (find-package "CLIM-USER")))
	(clim-user::set-up-lkb-interaction :core))))
