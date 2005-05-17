;;; Hey, emacs, this file is -*- Emacs-Lisp -*- ... got that?

;;;
;;; make sure this file is included from your personal `~/.emacs', e.g. put the
;;; following towards the end of `~/.emacs' (without the leading semikolons, of
;;; course):
;;;
;;;   (let ((root (getenv "DELPHINHOME")))
;;;     (if (file-exists-p (format "%s/lkb/etc/dot.emacs" root))
;;;       (load (format "%s/lkb/etc/dot.emacs" root) nil t t)))
;;;
;;; in other words, set the shell environment variable `DELPHINHOME' to the 
;;; root directory of your installation tree (which is `~/delphin' for most 
;;; people), then make sure to also export that variable (see the bash(1) or
;;; csh(1) on-line documentation), log out and back in (so that changes to the
;;; shell configuration take effect), and then launch emacs(1).
;;;
;;; please try not to make changes to _this_ file, so that on future upgrades
;;; you can safely move to a later version.
;;;

(defconst delphin-home 
  (or (getenv "DELPHINHOME") "/afs/ir.stanford.edu/users/o/e/oepen/src/lingo"))

(defconst allegro-home 
  (or (getenv "ACL_HOME") "/lingo/local/acl"))

(defconst allegro-locale
  (or (getenv "ACL_LOCALE") "en_US.UTF-8"))

(defmacro when (condition &rest body)
  (list 'and condition (cons 'progn body)))

(defmacro unless (condition &rest body)
  (list 'or condition (cons 'progn body)))

(defun system-binaries ()
  (cond
   ((string-match "solaris" system-configuration) "solaris")
   ((string-match "linux" system-configuration) 
    (if (string-match "x86_64" system-configuration)
      "linux.x86.64"
      "linux.x86.32"))
   ((or (string-match "windows" system-configuration)
        (string-match "mingw-nt" system-configuration)
        (string-match "msvc" system-configuration)) "windows")))

(defun allegro (&optional prefix)
  (interactive "P")

  ;;
  ;; set up .load-path., load and configure emacs -- lisp interface
  ;;
  (let ((eli (format "%s/eli" delphin-home))
        (lkb (format "%s/lkb/src" delphin-home)))
    (unless (member eli load-path)
      (setq load-path (cons eli load-path)))
    (unless (member lkb load-path)
      (setq load-path (cons lkb load-path))))
  (load "fi-site-init" nil t)
  (fset 'lisp-mode (symbol-function 'common-lisp-mode))
  (setq fi:common-lisp-directory delphin-home)
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
  ;; add LKB-specific library directory to LD_LIBRARY_PATH, in case the Motif
  ;; libraries are not installed locally.
  ;;
  (let ((old (getenv "LD_LIBRARY_PATH"))
        (new (format
              "LD_LIBRARY_PATH=%s/lkb/lib/%s"
              delphin-home (system-binaries))))
    (setq process-environment
      (cons
       (if old (format "%s:%s" new old) new)
       process-environment))))

(defun lkb (&optional prefix)
  (interactive "P")

  (allegro)
  ;;
  ;; use pre-built run-time binaries distributed from `lingo.stanford.edu'
  ;;
  (setq fi:common-lisp-image-name 
    (format
     "%s/lkb/%s/lkb%s"
     delphin-home
     (system-binaries)
     (if (or (string-match "windows" system-configuration)
             (string-match "mingw-nt" system-configuration)
             (string-match "msvc" system-configuration))
       ".exe"
       "")))

  (setq fi:common-lisp-image-file
    (format "%s/lkb/%s/lkb.dxl" delphin-home (system-binaries)))

  (setq fi:common-lisp-image-arguments (list "-locale" allegro-locale))

  ;;
  ;; start up inferior lisp process
  ;;
  (let ((process-connection-type nil))
    (fi:common-lisp)))


(defun japanese (&optional prefix)
  (interactive "P")

  (allegro)
  ;;
  ;; make sure that emacs(1) will use the appropriate encoding on all buffers
  ;; connected to Lisp (aka the LKB), which for latest versions include more 
  ;; than just the main `*common-lisp*' buffer.  also, we need to force the LKB
  ;; into using a locale with corresponding encoding (`japan.EUC' as we used to
  ;; call it appears to be an Allegro CL nickname for `ja_JP.EUC').
  ;;
  (let ((encoding 'euc-jp))
    (set-language-environment 'japanese)
    (setq default-buffer-file-coding-system encoding)
    (setq default-process-coding-system (cons encoding encoding)))

  (setq fi:common-lisp-image-name 
    (format
     "%s/lkb/%s/lkb%s"
     delphin-home
     (system-binaries)
     (if (or (string-match "windows" system-configuration)
             (string-match "mingw-nt" system-configuration)
             (string-match "msvc" system-configuration))
       ".exe"
       "")))

  (setq fi:common-lisp-image-file
    (format "%s/lkb/%s/lkb.dxl" delphin-home (system-binaries)))

  (setq fi:common-lisp-image-arguments (list "-locale" "ja_JP.EUC"))

  (let ((process-connection-type nil))
    (fi:common-lisp)))


(defun lisp (&optional prefix)
  (interactive "P")

  (allegro)

  (let ((image (getenv "ACL_IMAGE")))
    (setq fi:common-lisp-image-name 
      (format
       "%s/%s"
       allegro-home (or image "alisp")
       (if (or (string-match "windows" system-configuration)
               (string-match "mingw-nt" system-configuration)
               (string-match "msvc" system-configuration))
         ".exe"
         "")))

    (setq fi:common-lisp-image-file 
      (format "%s/%s.dxl" allegro-home (or image "clim"))))

  (setq fi:common-lisp-image-arguments (list "-locale" allegro-locale))

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
        (load \"%s/lkb/src/general/defsystem.lisp\")
        (load \"%s/lkb/src/general/loadup.lisp\")
        (load-system \"tsdb\")
        nil)"
      delphin-home delphin-home))))
