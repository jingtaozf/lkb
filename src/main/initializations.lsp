(in-package :lkb)

#+:mswindows
(unless (find-package "MAKE") 
  (make-package "MAKE" :nicknames '("MK") :use '("COMMON-LISP")))

#+:mswindows
(pushnew :lkb *features*)

;;; if for some reason, the tty mode is desirable, use the following
;;; (pushnew :tty *features*)

;;;
;;; because ann used the term `type' the way she does well before it became a
;;; *censored* part of the *censored* common-lisp language |:-}.
;;;
;;; censorship by ann ...
;;;
#+:allegro 
(setf excl:*enable-package-locked-errors* nil)

#+:lispworks
(setf hcl:*packages-for-warn-on-redefinition*
  (loop
     with key = (find-package :common-lisp)
     for name in hcl:*packages-for-warn-on-redefinition*
     for package = (find-package name)
     unless (eq key package) collect name))

(defparameter *grammar-directory* nil)

#-:allegro
(defparameter *lkb-background-stream* *terminal-io*)

#+:allegro
(defparameter *lkb-background-stream* excl::*initial-terminal-io*)

(import '(enable-type-interactions disable-type-interactions))

#+(and :allegro :clim (not :mswindows)) 
(setq tk-silica::*use-clim-gc-cursor* t)

(defmacro with-package ((package) &body body)
  `(let ((*package* (find-package ,package))
         (*read-eval* nil)) ; for safety
      ,@body))


(defun time-a-funcall (timed-function report-function)
   #+:allegro
   (excl::time-a-funcall timed-function report-function)
   #-:allegro
   (let* ((treal (get-internal-real-time))
          (tcpu (get-internal-run-time))
          #+:mcl (tgc (ccl:gctime))
          #+:mcl (others (ccl::total-bytes-allocated)))
      (multiple-value-prog1
         (funcall timed-function)
         (let (#+:mcl (others (- (ccl::total-bytes-allocated) others))
               )
            (funcall report-function
               ;; tgcu tgcs tu ts tr scons ssym sother
               #+:mcl (round (* (- (ccl:gctime) tgc) 1000)
                             internal-time-units-per-second)
               #-:mcl 0
               0
               (round (* (- (get-internal-run-time) tcpu) 1000)
                      internal-time-units-per-second)
               0
               (round (* (- (get-internal-real-time) treal) 1000)
                      internal-time-units-per-second)
               0 0
               #+:mcl others #-:mcl -1)))))


(defun start-lkb ()
  ;;; this is a no-op in tty mode
  #+(not :tty)
  ;; (or :clim :common-graphics :mcl)
  (let ((building-image-p (find-symbol "*BUILDING-IMAGE-P*" :make)))
    (unless (and building-image-p (boundp building-image-p)
                 (symbol-value building-image-p))
      #+:allegro
      (tpl:setq-default *package* (find-package :lkb))
      (let ((*package* (find-package #+:clim :clim-user #-:clim :lkb)))
        #+:clim
        (clim-user::set-up-lkb-interaction)
        #-:clim
        (lkb::set-up-lkb-interaction)))))







