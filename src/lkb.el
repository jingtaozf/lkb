;;; Copyright (c) 1998-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions

;;; Add an LKB menu to the emacs menu bar

(defconst lkb-menu
    '("LKB"
      ("Load"
       ["Complete grammar..." read-script-file t]
       ["Reload grammar" reload-script-file t])
      ("View"
       ["Type hierarchy..." show-type-tree t]
       ["Type definition..." show-type-spec t]
       ["Expanded type..." show-type t]
       ["Lex entry..." show-lex t]
       ["Word entries..." show-words t]
       ["Grammar rule..." show-grammar-rule t]
       ["Lexical rule..." show-lex-rule t])
      ("Parse"
       ["Parse input..." clim-user::do-parse-batch t]
       ["Redisplay pars" show-parse t]
       ["Show chart" show-chart t]
       ["Print chart" print-chart t]
       ["Batch parse..." parse-sentences-batch t])
      ("Generate"
       ["Generate..." generate-from-edge t]
       ["Redisplay realization" show-gen-result t]
       ["Show chart" show-gen-chart t]
       ["Index" index-for-generator t])
      "---"
      ["Redefine type" redefine-type t]))
      
(add-hook 'fi:lisp-mode-hook 
	  (function (lambda ()
		      (fi::install-menubar lkb-menu))))

(add-hook 'tdl-mode-hook
	  (function (lambda ()
		      (fi::install-menubar lkb-menu))))

(defun define-lisp-commands (commands)
  (dolist (com commands)
    (eval `(defun ,com ()
	     (interactive ())
	     (eval-in-lisp ,(format "(%s)" com))))))

(define-lisp-commands 
    '(read-script-file reload-script-file show-type-tree show-type-spec 
      show-type show-lex show-words show-grammar-rule show-lex-rule 
      clim-user::do-parse-batch show-parse show-chart print-chart 
      parse-sentences-batch generate-from-edge show-gen-result show-gen-chart 
      index-for-generator))
			 
(defun redefine-type (arg)
  (interactive "P")
  (let ((beg 0)
        (end 0)
        (pos (point)))
    (setq beg (calc-begin-of-tdl-expression))
    (goto-char pos)
    (setq end (calc-end-of-tdl-expression))
    (eval-in-lisp (format "(lkb::redefine-type \"%s\")" 
			  (buffer-substring beg (min (1+ end) (point-max)))))
    (goto-char pos)))

(defun eval-in-lisp (expr)
  (fi::make-request
      (lep::evaluation-request
       :transaction-directory fi:emacs-to-lisp-transaction-directory
       :text expr
       :echo nil
       :pathname nil
       :compilep nil)
    ;; Normal continuation
    (() (result) nil)
    ;; Error continuation
    (() (error)
     (fi::show-error-text "Error: %s" error))))

(defun find-tdl-definition (thing file)
  (fi::ensure-buffer-visible (find-file file))
  (goto-char 0)
  (re-search-forward (format "%s\\W+:" (regexp-quote thing)))
  (goto-char (match-beginning 0)))
 
