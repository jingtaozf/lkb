;;; Copyright (c) 1999-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions

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

;;; not used in LKB code but required by some grammar loading stuff
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
   #+(and :allegro-version>= (not (version>= 6 1)))
   (excl::time-a-funcall timed-function report-function)
   #+(and :allegro-version>= (version>= 6 1))
   (excl::time-a-funcall report-function timed-function)
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


(defun current-time (&key long)
  (decode-time (get-universal-time) :long long))

(defun decode-time (time &key long)
  (multiple-value-bind (second minute hour day month year foo bar baz)
      (decode-universal-time time)
    (declare (ignore foo bar baz))
    (let ((months '("jan" "feb" "mar" "apr" "may" "jun" 
                    "jul" "aug" "sep" "oct" "nov" "dec")))
      (cond
       ((null long)
        (format nil "~a-~a-~a" day month year))
       ((member long '(:usa :us :reverse))
        (format nil "~2,'0d-~2,'0d-~2,'0d" (mod year 100) month day))
       ((member long '(:tsdb))
        (format
         nil "~a-~a-~a ~2,'0d:~2,'0d" 
         day (nth (- month 1) months) year hour minute))
       ((member long '(:pretty :readable))
        (format
         nil "~a-~a-~a (~2,'0d:~2,'0d h)" 
         day (nth (- month 1) months) year hour minute))
       ((eq long :short)
        (format nil "~2,'0d:~2,'0d:~2,'0d" hour minute second))
       (t
        (format
         nil "~a-~a-~a (~2,'0d:~2,'0d:~2,'0d)"
         day month year hour minute second))))))


(defun start-lkb ()

  (let* ((lkbrc (dir-and-name (user-homedir-pathname) ".lkbrc")))
    (with-package (:lkb) (when (probe-file lkbrc) (load lkbrc))))

  ;;; this is a no-op in tty mode
  #+(not :tty)
  ;; (or :clim :common-graphics :mcl)
  (let ((building-image-p (find-symbol "*BUILDING-IMAGE-P*" :make)))
    (unless (and building-image-p (boundp building-image-p)
                 (symbol-value building-image-p))
      #+:allegro
      (tpl:setq-default *package* (find-package :lkb))
      (let ((display (system:getenv "DISPLAY"))
            (*package* (find-package #+:clim :clim-user #-:clim :lkb)))
        (when (and (stringp display) (not (zerop (length display))))
          #+:clim
          (clim-user::set-up-lkb-interaction)
          #-:clim
          (lkb::set-up-lkb-interaction))))))







