;;; this is just for the Allegro/IDE project mechanism
;;; to replace the call in mrs.system

(in-package :cl-user)

(defun after-mrs-project-load nil
   (progn
    (when COMMON-LISP-USER::*grammar-directory* 
       (load (merge-pathnames
              (make-pathname :name "mrsglobals-eng.lisp")
              COMMON-LISP-USER::*grammar-directory*)))
    (when (and COMMON-LISP-USER::*grammar-directory*
               (fboundp 'COMMON-LISP-USER::read-mrs-rule-file-aux))
       (COMMON-LISP-USER::read-mrs-rule-file-aux 
         (merge-pathnames
          (make-pathname :directory 
             (append 
               (pathname-directory
                COMMON-LISP-USER::*grammar-directory*) 
               '("data"))
            :name "new-rules.mrs"
            :device (pathname-device
                COMMON-LISP-USER::*grammar-directory*)))))))

