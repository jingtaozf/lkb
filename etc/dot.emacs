;;; Hey, emacs, this file is -*- Emacs-Lisp -*- ... got that?

(defconst lingo-home "/afs/ir.stanford.edu/users/o/e/oepen/src/lkb")

(defmacro when (condition &rest body)
  (list 'and condition (cons 'progn body)))

(defmacro unless (condition &rest body)
  (list 'or condition (cons 'progn body)))

(defun lkb (&optional prefix)
  (interactive "P")

  ;;
  ;; set up .load-path., load and configure emacs -- lisp interface
  ;;
  (let ((eli (format "%s/eli" lingo-home))
        (lkb (format "%s/src" lingo-home)))
    (unless (member eli load-path)
      (setq load-path (cons eli load-path)))
    (unless (member lkb load-path)
      (setq load-path (cons lkb load-path))))
  (load "fi-site-init" nil t)
  (fset 'lisp-mode (symbol-function 'common-lisp-mode))
  (setq fi:common-lisp-image-name 
    (format
     "%s/%s/lkb"
     lingo-home
     (cond
      ((string-match "solaris" system-configuration) "solaris")
      ((string-match "linux" system-configuration) "linux")
      ((string-match "windows" system-configuration) "windows"))))

  (setq fi:lisp-evals-always-compile nil)
  (setq
    fi:lisp-mode-hook
    (function (lambda () 
                (modify-syntax-entry ?\[ "(]  " fi:lisp-mode-syntax-table)
                (modify-syntax-entry ?\] ")[  " fi:lisp-mode-syntax-table)
                (modify-syntax-entry ?\{ "(}  " fi:lisp-mode-syntax-table)
                (modify-syntax-entry ?\} "){  " fi:lisp-mode-syntax-table))))

  ;;
  ;; load LKB-specific parts of emacs -- lisp interface
  ;;
  (load "lkb" nil t)
  (load "tdl-mode" nil t)
  (setq auto-mode-alist
    (append 
      '(("\\.cl$" . common-lisp-mode)
        ("\\.lisp$" . common-lisp-mode)
        ("\\.tdl$" . tdl-mode)
        ("\\.mrs$" . tdl-mode)
        ("\\.system$" . common-lisp-mode))
      auto-mode-alist))

  ;;
  ;; start up inferior lisp process
  ;;
  (let ((process-connection-type nil))
    (fi:common-lisp)))

;;;
;;; [incr tsdb()] add-on system; assumes the precompiled binaries are installed
;;; into the same directory tree as the LKB.
;;;

(defun itsdb ()
  (interactive)
  (save-window-excursion
    (fi:eval-in-lisp
     (format 
      "(and
        (load \"%s/src/general/defsystem.lisp\")
        (load \"%s/src/general/loadup.lisp\")
        (load-system \"tsdb\")
        nil)"
      lingo-home lingo-home))))
