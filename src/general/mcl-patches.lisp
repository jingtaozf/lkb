;;; -*- Mode: LISP; Package: MAKE -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: mcl-patches.lisp
;;;      module: DISCO loadup environment
;;;     version: 2.0 -- 4-jun-1994
;;;  written by: bernd kiefer, dfki saarbruecken
;;; last update: 6-jun-94
;;;  updated by: oe, dfki saarbruecken
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;; John Carroll      | 18-may-04   | additions so MCL 5 understands unix line
;;;                   |             | termination conventions. Still also need
;;;                   |             | to set ccl::*linefeed-equals-newline* to
;;;                   |             | true before loading any file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "COMMON-LISP-USER")

;;;
;;; chances are we have a modern eval-when() ... i doubt it (11-jul-94 -- oe)
;;;

(eval-when (:execute :load-toplevel :compile-toplevel)
  (pushnew :ansi-eval-when *features*))


;;; Aliases for multiprocessing functions

(defpackage :mp (:use "COMMON-LISP")
   (:export "RUN-FUNCTION" "PROCESS-WAIT" "PROCESS-WAIT-WITH-TIMEOUT"
            "PROCESS-KILL" "WITH-PROCESS-LOCK"
            "MAKE-PROCESS-LOCK" "PROCESS-ADD-ARREST-REASON"
            "PROCESS-REVOKE-ARREST-REASON" "*CURRENT-PROCESS*")
   (:import-from "CCL" "*CURRENT-PROCESS*"))
(in-package :mp)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (setf (symbol-function 'run-function) 
        (symbol-function 'ccl:process-run-function))
  (setf (symbol-function 'process-wait) 
        (symbol-function 'ccl:process-wait))
  (setf (symbol-function 'process-wait-with-timeout) 
        (symbol-function 'ccl:process-wait-with-timeout))
  (setf (symbol-function 'process-kill) 
        (symbol-function 'ccl:process-kill))
  (defmacro with-process-lock ((lock) &body body) 
     `(ccl:with-lock-grabbed (,lock) ,@body))
  (setf (symbol-function 'make-process-lock) 
        (symbol-function 'ccl:make-lock))
  (setf (symbol-function 'process-add-arrest-reason) 
        (symbol-function 'ccl:process-enable-arrest-reason))
  (setf (symbol-function 'process-revoke-arrest-reason) 
        (symbol-function 'ccl:process-disable-arrest-reason)))

(in-package :cl-user)


;;;
;;; load the portable defsystem() from CMU
;;;

#-:mk-defsystem
(load (make-pathname :directory general-dir :name "defsystem"))

(in-package "MAKE")

(defparameter %binary-dir-name% 
  ".masl")

(defparameter %system-binaries%
  "mac")

;;; Unix system functions not in MCL -- simulate them

(eval-when (:execute :load-toplevel :compile-toplevel)
  (setf (symbol-function 'user-name) 
        (symbol-function 'ccl:false))
  (setf (symbol-function 'getenv)
        (symbol-function 'ccl:false)))


;;; User's home directory pathname (i.e. ~/) -- user-homedir-pathname
;;; doesn't return anything useful by default

(in-package :ccl)

(let ((*warn-if-redefine-kernel* nil))
(defun user-homedir-pathname (&optional host)
   (declare (ignore host)) 
   (ccl::findfolder #$kOnSystemDisk #$kCurrentUserFolderType))
)


;;; Set preferred memory allocation for saved images (400MB)

(ccl::set-preferred-size-resource (* 400 1024 1024))


;;; Fix so that linefeed is taken to end a comment, as well as newline
;;; (default for newline is CR on Mac, c.f. LF on unix).

(setq ccl::*linefeed-equals-newline* t)


;;; Modify the behaviour of read-line so that the #\linefeed character
;;; is also taken as ending a line

(in-package :ccl)

(let ((*warn-if-redefine-kernel* nil))

(eval-when (:execute :compile-toplevel)
  (defmacro signal-eof-error (stream)
    `(error 'end-of-file :stream ,stream))  
  )

; from Shannon Spires slightly modified - its a method now
(defmethod ccl::read-line-raw ((input-stream t) eof-error-p eof-value)
    "A faster way to do read-line-raw than that in streams.lisp. The speedup
     comes from avoiding with-output-to-string. Conses less too."
    (declare (optimize (speed 3) (safety 1)))
    (if (stream-eofp input-stream)
      (if eof-error-p
        (ccl::signal-eof-error input-stream)
        (values eof-value (or eof-value t)))
      (let ((char nil)
            (str (make-array 20
                             :element-type 'base-character
                             :adjustable t :fill-pointer 0)))
        (multiple-value-bind (reader reader-arg) (stream-reader input-stream)
          (while (and (setq char (funcall reader reader-arg))
                      (not (or (eql char #\newline) (eql char #\linefeed)))) ; jac - added linefeed
            (when (and (not (base-character-p char))(base-string-p str))
              (setq str (string-to-extended-string str)))
            (vector-push-extend char str)))
        (values str (null char)))))

)


;;; There is a problem with using the ~<newline> format command
;;; because of the difference between #\Linefeed and #\Return.  The
;;; following code allows either line terminator to work, by copying the
;;; #\Return function to the (normally empty) #\Linefeed entry.

(setf (aref ccl::*format-char-table* (char-code #\Linefeed))
      (aref ccl::*format-char-table* (char-code #\Return)))


;;; ccl::*do-unix-hack* fixes Fred to try to deduce whether a file
;;; uses #\linefeed as EOL and if Fred so decides, it will convert
;;; #\linefeed to #\return in the Fred buffer so a person can read it. If the file
;;; is written, the substitution will be reversed.
;;;
;;; (N.B. There is also a Fred command c-x c-f that changes the line endings
;;; for the buffer from #\Linefeed to #\Newline)

(setq ccl::*do-unix-hack* t)


;;; More OS X-like behaviour

; track scroll in Fred (text editor and listener) windows
(setq *fred-track-thumb-p* t)

; standard OSX application switch behaviour for windows
(setq ccl:*dont-bring-all-windows-front* t)

; execute all becomes command-option-E (was command-H)
(set-command-key ccl::*execute-item* '(:option #\E))

; command-H hides application
(ccl::set-hide-command-key :modifier nil)

; command-option-H does "Hide Others" like e.g. BBEdit
(ccl::set-hide-command-key :others t :modifier :option)

; keep window control buttons active on inactive windows (standard OSX)
(setq ccl:*disable-bubbles-on-inactive-windows* nil)


;;; Use more CPU than only around 50% when in background

(setq ccl:*background-sleep-ticks* nil) 

