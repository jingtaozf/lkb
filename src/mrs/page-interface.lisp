(in-package "MRS")

(defun vsym (str) 
  ;;; allow mrsglobals-eng file to be system independent
  (intern (string-upcase str) lex:*lex-package*))

(defun get-parse-fs (parse)
  (lexicon::cfs-fs (car (main::typed-item-args parse))))

(defun get-last-sentence ()
  main::*last-sentence*)
