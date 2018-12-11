;;; Copyright (c) 1998-2017 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see LICENSE for conditions

;;; this is just for the Allegro/IDE project mechanism
;;; to replace the call in mrs.system

(in-package :lkb)

(defun after-mrs-project-load nil
   (progn
    (when lkb::*grammar-directory* 
       (load (merge-pathnames
              (pathname "mrsglobals-eng.lisp")
              lkb::*grammar-directory*)))
    (when (and lkb::*grammar-directory*
               (fboundp 'lkb::read-mrs-rule-file-aux))
       (funcall (symbol-function 'lkb::read-mrs-rule-file-aux)
         (merge-pathnames
          (make-pathname :directory 
             (append 
               (pathname-directory
                lkb::*grammar-directory*) 
               '("data"))
             :name "new-rules.mrs"
             :host (pathname-host
                lkb::*grammar-directory*)
            :device (pathname-device
                lkb::*grammar-directory*)))))))

