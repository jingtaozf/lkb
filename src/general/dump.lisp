;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: MAKE -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: dump.lisp
;;;     version: 1.0 -- 1-aug-1994 (experimental)
;;;  written by: oe, dfki saarbruecken
;;; last update: 13-dec-96
;;;  updated by: oe, dfki saarbruecken
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File dump.lisp contains useful functions to overcome the DISCO software
;;; modularity ... |:-}.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "MAKE")

#+:allegro
(setf excl::*libfasl* nil)

#+(and :allegro :never)
(proclaim '(optimize (safety 0) (space 1) (speed 3) (debug 0)))
#+(and :allegro :never)
(setq compiler::generate-interrupt-checks-switch
  (compile nil
    '(lambda (safety space speed debug)
       (declare (ignore safety space speed debug))
       t)))

#+(and :lucid :never)
(proclaim '(optimize (safety 3) (space 0) (speed 3) (compilation-speed 0)))

(defparameter 
  *interesting-modules*
  '("fegramed"
    "udine"
    "tdl"
    "morphix"
    "english-morphology"
    "dtree"
    "ebl"
    "lexicon"
    "combinazione"
    "grammatik"
    "re2atn"
    "scan-interface"
    "smes-proto"
    "smes2html"
    "tsdb"
    "fst-grammar"))

(defparameter *print-module-versions* t)

(defun print-module-versions ()
  (format
   t
   "~&~%;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~%")
  (do* ((modules (reverse *interesting-modules*) (rest modules))
        (module (first modules) (first modules))
        (version (first (get-module-version module))
                 (first (get-module-version module)))
        (message (when (and version (member module *modules* :test #'equal))
                   (format nil " `~a' (~a);" module version))
                 (when (and version (member module *modules* :test #'equal))
                  (format nil " `~a' (~a);" module version)))
        (length (+ 3 (length message))
                (if (< (+ length (length message)) 80)
                  (+ length (length message))
                  (+ 3 (length message)))))
      ((null modules))
    (when message
      (if (= length (+ 3 (length message)))
        (format t "~&;;;~a" message)
        (format t "~a" message))))
  (format
   t
   "~&;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~%~%"))

(defun dump (&key file news shutdown restart)
  (let* ((configuration
          (cond
           ((member "fst-grammar" *modules* :test #'equal)
            :smes)
           ((and (member "tdl" *modules* :test #'equal)
                 (not (member "grammatik" *modules* :test #'equal))
                 (not (member "page" *modules* :test #'equal)))
            :tdl)
           ((and (member "grammatik" *modules* :test #'equal)
                 (not (member "page" *modules* :test #'equal)))
            :grammatik)
           ((and (member "page" *modules* :test #'equal)
                 (not (member "page+" *modules* :test #'equal)))
            :page)
           ((and (member "page+" *modules* :test #'equal)
                 (not (or (member "depage" *modules* :test #'equal)
                          (member "enpage" *modules* :test #'equal))))
            :page+)
           ((and (member "depage" *modules* :test #'equal)
                 (not (member "deutsch" *modules* :test #'equal)))
            :depage)
           ((member "deutsch" *modules* :test #'equal)
            :deutsch)
           ((and (member "enpage" *modules* :test #'equal)
                 (not (member "csli" *modules* :test #'equal)))
            :enpage)
           ((member "csli" *modules* :test #'equal)
            :csli)))
        (shutdown
	 (append
         '(("scan-interface" . "(scanning::close-scanner)")
           ("fegramed" . "(fegramed::close-fegramed)")
           ("parser" . "#+:chartdisplay (parsing::close-window-system)
                        #-:chartdisplay nil"))
	 shutdown))
	(dumper #+:allegro (system:getenv "USER")
		#+:lucid (lcl:environment-variable "USER")
		#+:clisp
		(let ((stream NIL))
		  (unwind-protect
		      (progn
			(setq stream
			  (run-shell-command "echo $USER" :output :stream))
			(when stream (read-line stream)))
		    (when stream (close stream))))
		#-(or :allegro :lucid :clisp) "unknown"))
    (if configuration
      (multiple-value-bind (sec min h day month year) (get-decoded-time)
	(declare (ignore sec))
        (dolist (action shutdown)
          (when (or (equal (first action) t)
                    (member (first action) *modules* :test #'string=))
            (eval (read-from-string (rest action)))))
        #+:allegro (excl::gc) #+:allegro (excl::gc t)
        (let* ((message
                (case configuration
                  (:smes
                   (format
                    nil
                    ";; SMES image (including fst and lexicon) ~
                        dumped ~a-~a-~a (~2,'0d:~2,'0d h) [~a].~%~
                     ;; Evaluate (smes)~a for IE core engine startup.~%"
                    day month year h min dumper
                    #+:allegro " (or `:smes')" #-:allegro ""))
                  (:tdl
                   (format
                    nil
                    ";; TDL image (including nothing) ~
                        dumped ~a-~a-~a (~2,'0d:~2,'0d h) [~a].~%~
                     ;; Evaluate (tdl)~a for plain TDL startup.~%"
                    day month year h min dumper
                    #+:allegro " (or `:tdl')" #-:allegro ""))(:grammatik
                   (format
                    nil
                    ";; GRAMMATIK image (including nothing) ~
                        dumped ~a-~a-~a (~2,'0d:~2,'0d h) [~a].~%~
                     ;; Evaluate (tdl)~a for plain TDL startup.~%"
                    day month year h min dumper
                    #+:allegro " (or `:tdl')" #-:allegro ""))
                  (:page
                   (format
                    nil
                    ";; PAGE (excluding grammar and lexicon) ~
                        dumped ~a-~a-~a (~2,'0d:~2,'0d h) [~a].~%~
                     ;; Evaluate `~a' for the PAGE core engine --- ~
                        `~a' for plain TDL.~%"
                    day month year h min dumper
                    #+:allegro ":page" #-:allegro "(page)"
                    #+:allegro ":tdl" #-:allegro "(tdl)"))
                  (:page+
                   (format
                    nil
                    ";; PAGE+ (excluding grammar and lexicon) ~
                        dumped ~a-~a-~a (~2,'0d:~2,'0d h) [~a].~%~
                     ;; Evaluate `~a' for the PAGE core engine --- ~
                        `~a' for plain TDL.~%"
                    day month year h min dumper
                    #+:allegro ":page" #-:allegro "(page)"
                    #+:allegro ":tdl" #-:allegro "(tdl)"))
                  (:depage
                   (format
                    nil
                    ";; DEPAGE (excluding grammar and lexicon) ~
                        dumped ~a-~a-~a (~2,'0d:~2,'0d h) [~a].~%~~
                     ;; Evaluate `~a' for the PAGE core engine --- ~
                        `~a' for plain TDL.~%"
                    day month year h min dumper
                    #+:allegro ":page" #-:allegro "(page)"
                    #+:allegro ":tdl" #-:allegro "(tdl)"))
                  (:deutsch
                   (format
                    nil
                    ";; DEPAGE (including grammatik and lexikon) ~
                        dumped ~a-~a-~a (~2,'0d:~2,'0d h) [~a].~%~~
                     ;; Evaluate `~a' for the PAGE core engine --- ~
                        `~a' for plain TDL.~%"
                    day month year h min dumper
                    #+:allegro ":page" #-:allegro "(page)"
                    #+:allegro ":tdl" #-:allegro "(tdl)"))
                  (:enpage
                   (format
                    nil
                    ";; ENPAGE (excluding grammar and lexicon) ~
                        dumped ~a-~a-~a (~2,'0d:~2,'0d h) [~a].~%~~
                     ;; Evaluate `~a' for the PAGE core engine --- ~
                        `~a' for plain TDL.~%"
                    day month year h min dumper
                    #+:allegro ":page" #-:allegro "(page)"
                    #+:allegro ":tdl" #-:allegro "(tdl)"))
                  (:csli
                   (format
                    nil
                    ";; CSLI (excluding grammar and lexicon) ~
                        dumped ~a-~a-~a (~2,'0d:~2,'0d h) [~a].~%~~
                     ;; Evaluate `~a' for the PAGE core engine --- ~
                        `~a' for plain TDL.~%"
                    day month year h min dumper
                    #+:allegro ":page" #-:allegro "(page)"
                    #+:allegro ":tdl" #-:allegro "(tdl)"))))
               (file (or file
                         (case configuration
                           (:smes
                            (if make::%athome%
                              "/usr/acl/bin/smes"
                              "/tmp/smes"))
                           (:tdl
                            (if make::%athome%
                              "/usr/acl/bin/tdl"
                              "/tmp/tdl"))
			   (:grammatik
                            (if make::%athome%
                              "/usr/acl/bin/grammatik"
                              "/tmp/grammatik"))
                           (:page
                            (if make::%athome%
                              "/usr/acl/bin/page"
                              "/tmp/page"))
                           (:page+
                            (if make::%athome%
                              "/usr/acl/bin/page+"
                              "/tmp/page+"))
                           (:depage
                            (if make::%athome%
                              "/usr/acl/bin/depage"
                              "/tmp/depage"))
                           (:deutsch
                            (if make::%athome%
                              "/usr/acl/bin/deutsch"
                              "/tmp/deutsch"))
                           (:enpage
                            (if make::%athome%
                              "/usr/acl/bin/enpage"
                              "/tmp/enpage"))
                           (:csli
                            (if make::%athome%
                              "/usr/acl/bin/csli"
                              "/tmp/csli")))))
               (restart-function
                #'(lambda ()
                    (let ((e-arg
                           #+:allegro
                           (member
                            "-e"
                            (system:command-line-arguments :application nil)
                            :test #'string=)
                           #-:allegro nil))
		      ;; (when e-arg
		      ;; (eval (with-standard-io-syntax
		      ;; (read-from-string (cadr e-arg)))))
                      (format
                       t
                       "~%~&;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~
                            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~%")
                      (format t message)
                      #-:lucid
                      (format
                       t
                       ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;~%")
                      #+:lucid
                      (format
                       t
                       ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;~%~%")
                      (when *print-module-versions* 
                        (print-module-versions))
                      (when news
                        (format
                         t
                         "~%~&;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~
                              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~%")
                        (format t news)
                        (format
                         t
                         "~&;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~
                          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~%~%"))
                      (when (find-package "MAIN")
                        (let ((prompt 
                               (concatenate 
                                'string
                                "page@"
                                #-:lucid (short-site-name)
                                #+:lucid (with-open-stream
                                             (foo (run-program
                                                   "hostname"
                                                   :output :stream
                                                   :wait nil))
                                           (read-line foo))
                                #+:mcl "mac"
                                ": ")))
                          (eval
                           `(setf
                             ,(read-from-string "main::*disco-prompt*")
                             ,prompt))))
                      (eval (read-from-string "(make::reset-system-paths)"))
                      (when restart
                        (eval restart))))))
	  (when (find-package "MAIN")
	    (eval (read-from-string "(setf main::*page-initialized-p* nil)")))
          #+(and :allegro (not (or :allegro-v4.3 :allegro-v4.3.1 :allegro-v5.0)))
          (excl::dumplisp
           :name file
           :read-init-file t
           :restart-function restart-function)
          #+(or :allegro-v4.3 :allegro-v4.3.1 :allegro-v5.0)
          (progn
            (setf excl:*restart-init-function* restart-function)
            (setf excl:*read-init-files* t)
            (excl::dumplisp :name file))
          #+:lucid
          (lcl::disksave file
           :restart-function restart-function
           :full-gc t
           :dynamic-free-segments 50)
	  #+:clisp
	  (lisp:saveinitmem file)
	  #+:mcl
	  (ccl:save-application file :init-file "init.lisp")
	  #+:mcl
	  (funcall restart-function)))
      (format
       t
       "~&ERROR: no modules worth dumping have been loaded.~%"))))

(eval-when #+:ansi-eval-when (:load-toplevel :compile-toplevel :execute)
	   #-:ansi-eval-when (load eval compile)
  (export
   '(*interesting-modules*
     *print-interesting-modules*
     print-interesting-modules
     dump)))

