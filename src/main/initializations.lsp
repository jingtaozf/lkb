;;; Copyright (c) 1999--2002
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `LICENSE' for conditions.

(in-package :lkb)

#+:mswindows
(unless (find-package "MAKE") 
  (make-package "MAKE" :nicknames '("MK") :use '("COMMON-LISP")))

#+:mswindows
(pushnew :lkb *features*)

(defparameter *lkb-package* 
  (or (find-package :lkb) (find-package :common-lisp-user)))

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

#+:clisp
(setf (ext:package-lock "LISP") nil)

#+:sbcl
(sb-ext:unlock-package :common-lisp)

#+:ecl
(si::package-lock "CL" nil)

;;; not used in LKB code but required by some grammar loading stuff
(defparameter *grammar-directory* nil)

(defparameter *lkb-background-stream*
  #-:allegro *terminal-io*
  #+:allegro excl::*initial-terminal-io*)

(import '(enable-type-interactions disable-type-interactions))

#+(and :allegro :clim (not :mswindows) (not :64bit))
(setq tk-silica::*use-clim-gc-cursor* t)

(defmacro with-package ((package) &body body)
  `(let ((*package* (find-package ,package))
         (*read-eval* nil)) ; for safety
      ,@body))


(defun time-a-funcall (timed-function report-function)
  ;;
  ;; mimicry of an /old/ version of time-a-funcall() in Allegro CL.  the report
  ;; function takes eight arguments: user and system gc() time, user and system
  ;; non-gc() time, wall-clock time, and allocation counts for cons() cells,
  ;; symbols, and other bytes.
  ;;
   #+(and :allegro-version>= (not (version>= 6 1)))
   (excl::time-a-funcall timed-function report-function)
   #+(and :allegro-version>= (version>= 6 1) (not (version>= 8 2)))
   (excl::time-a-funcall report-function timed-function)
   ;;
   ;; _fix_me_
   ;; as of Allegro CL 8.2, timing is now in microseconds (surely a good thing,
   ;; in principle), and arguments to excl::time-a-funcall() have changed.
   ;;                                                          (17-aug-11; oe)
   #+(and :allegro-version>= (version>= 8 2))
   (excl::time-a-funcall 
    #'(lambda (stream tgcu tgcs tu ts tr scons sother static &rest ignore)
        (declare (ignore stream ignore))
        (funcall 
         report-function 
         (round tgcu 1000) (round tgcs 1000) (round tu 1000) (round ts 1000)
         (round tr 1000) scons 0 (+ sother static)))
    *standard-output*
    timed-function)
   #-:allegro
   (let* ((treal (get-internal-real-time))
          (tcpu (get-internal-run-time))
          #+:mcl (tgc (ccl:gctime))
          #+:mcl (others (ccl::total-bytes-allocated)))
      (multiple-value-prog1
         (funcall timed-function)
         (let (#+:mcl (others (- (ccl::total-bytes-allocated) others)))
            (funcall report-function
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

(defun normalize-string (string)
  (if string
    (loop
        with string = (if (stringp string)
                        string 
                        (with-standard-io-syntax 
                          (let ((*package* (find-package :tsdb)))
                            (write-to-string string))))
        with padding = 128
        with length = (+ (length string) padding)
        with result = (make-array length
                                  :element-type 'character
                                  :adjustable nil :fill-pointer 0)
        with space = t
        for c across string
        when (member c '(#\Space #\Newline #\Tab)) do
          (when space (incf padding))
          (unless space
            (vector-push #\Space result)
            (setf space :space))
        else do
          (vector-push c result)
          (setf space nil)
        finally
          (when (and (eq space :space) (not (zerop (fill-pointer result))))
            (decf (fill-pointer result)))
          (return result))
    ""))
     

(defun start-lkb (&optional runtimep)

  ;;
  ;; unless we are creating a run-time image or operate in [incr tsdb()] slave
  ;; mode, initialize various subsystems and the GUI.
  ;;
  (let ((building-image-p (find-symbol "*BUILDING-IMAGE-P*" :make)))
    (unless (or (and building-image-p (boundp building-image-p)
                     (symbol-value building-image-p))
                (find :slave *features*))

      (let* ((home (getenv "DELPHINHOME"))
             (home (when home (namestring (parse-namestring home))))
             (home (make-pathname :directory home))
             (lkbrc (when home (dir-and-name home "dot.lkbrc"))))
        (when (and lkbrc (probe-file lkbrc))
          (with-package (:lkb) (load lkbrc))))

      ;;
      ;; for runtime binaries, reset mk::bin-dir et al. according to current
      ;; image location; assume standard runtime directory structure.
      ;;
      (when runtimep
        (let* ((sys (truename (translate-logical-pathname "sys:")))
               (home (make-pathname 
                      :directory (butlast (pathname-directory sys)))))
          (setf mk::sys-home (merge-pathnames home sys))
          (mk::reset-system-paths)))

      ;;
      ;; when part of the LOGON source tree, initialize appropriately
      ;;
      #+:logon
      (let* ((home (getenv "LOGONROOT"))
             (home (when home (namestring (parse-namestring home))))
             (home (make-pathname :directory home))
             (lkbrc (when home (dir-and-name home "dot.lkbrc"))))
        (when (and lkbrc (probe-file lkbrc))
          (with-package (:lkb) (load lkbrc))))
      
      ;;
      ;; and see whether there is a per-user `.lkbrc' initialization file
      ;;
      (let* ((lkbrc (dir-and-name (user-homedir-pathname) ".lkbrc")))
        (with-package (:lkb) (when (probe-file lkbrc) (load lkbrc))))
      
      #+:allegro
      (tpl:setq-default *package* 
        (find-package (if (system:getenv "SSP") :ssp :lkb)))
      
      #+:lui
      (let* ((lui (getenv "LUI"))
             (port (and (stringp lui) (parse-integer lui :junk-allowed t))))
        (when lui (lui-initialize :port port :runtimep runtimep :lui lui)))

      ;;
      ;; no graphics when in :tty mode
      ;;
      #+(not :tty)
      (let ((display #+:allegro (system:getenv "DISPLAY") #-:allegro nil)
            (*package* (find-package #+:clim :clim-user #-:clim :lkb)))
        (when #+:mswindows t
              #-:mswindows (and (stringp display) (not (string= display "")))
          #+:clim
          (clim-user::set-up-lkb-interaction)
          #-:clim
          (lkb::set-up-lkb-interaction))
        ;;
        ;; _fix_me_
        ;; attempt to work around [spr31047], i.e. what appears to be an 
        ;; invalid function pointer added by the top-level CLIM pane.
        ;;                                                    (23-jan-06; oe) 
        ;;
        #+(and :allegro :64bit :null)
        (setf (excl:gc-before-c-hooks) nil (excl:gc-after-c-hooks) nil)))))

;; 'define-constant' taken from SBCL manual
;; Under ANSI spec, application of defconstant multiple times is undefined
;; unless values are eql. SBCL treats this undefined behaviour as an error.
;; What's worse, in SBCL defconstant takes effect both at load time and at
;; compile time...
(defmacro define-constant (name value &optional doc)
       `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
	  ,@(when doc (list doc))))

