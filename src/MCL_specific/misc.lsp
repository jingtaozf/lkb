;;; Copyright (c) 1991-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions

;;; misc functions for compatability

(in-package :lkb)


(defun lkb-beep nil
;; for Procyon (beep *screen*)
  (ed-beep))


