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


;;;
;;; Such that (user-homedir-pathname) will return just the drive MCL is on.
;;;

(setf ccl::*user-homedir-pathname*
      (make-pathname
       :directory (subseq (pathname-directory (mac-default-directory)) 0 2)))

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


;;; Aliases for multiprocessing functions

(defpackage :mp (:use "COMMON-LISP")
   (:intern "RUN-FUNCTION" "PROCESS-WAIT" "PROCESS-KILL" "WITH-PROCESS-LOCK"
            "MAKE-PROCESS-LOCK"))
(in-package :mp)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export 'run-function)
  (setf (symbol-function 'run-function) 
        (symbol-function 'ccl:process-run-function))
  (export 'process-wait)
  (setf (symbol-function 'process-wait) 
        (symbol-function 'ccl:process-wait))
  (export 'process-kill)
  (setf (symbol-function 'process-kill) 
        (symbol-function 'ccl:process-kill))
  (export 'with-process-lock)
  (defmacro with-process-lock ((lock) &body body) 
     `(ccl:with-lock-grabbed (,lock) ,@body))
  (export 'make-process-lock)
  (setf (symbol-function 'make-process-lock) 
        (symbol-function 'ccl:make-lock)))


;;; Set preferred memory allocation for saved images (400MB)

(ccl::set-preferred-size-resource (* 400 1024 1024))


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


;;; ccl::*do-unix-hack* fixes the comment reader to also understand linefeeds
;;; as line terminators for opening a text file for editing in Fred.
;;;
;;; (N.B. There is also a Fred command c-x c-f that changes the line endings
;;; for the whole buffer from #\Linefeed to #\Newline. Thus a person using
;;; Fred can at least read Mac OS X files with unix line conventions)

(setq ccl::*do-unix-hack* t)
