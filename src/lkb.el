;;; Copyright (c) 1998-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions

;;; Add an LKB menu to the emacs menu bar

(if
    (and (boundp 'emacs-major-version)
	 (>= emacs-major-version 21))
    (load "lkb.21.el")
  (load "lkb.pre21.el"))
