(in-package "MRS")

(defun vsym (str) 
  ;;; allow mrsglobals-eng file to be system independent
  (intern (string-upcase str) lex:*lex-package*))

;;
;; unfortunately, all TDL domains have a few symbols from the :tdl package; the
;; vsym() mechanism has no way to tell and, potentially, when we load() MRS
;; the :disco domain is not yet defined |:-{.             (25-aug-98  -  oe)
;;
(eval-when #+:ansi-eval-when (:load-toplevel :compile-toplevel :execute)
           #-:ansi-eval-when (load eval compile)
  (import 'tdl::*diff-list* lex:*lex-package*))

(defun get-parse-fs (parse)
  (lexicon::cfs-fs (car (main::typed-item-args parse))))

(defun get-last-sentence ()
  main::*last-sentence*)
