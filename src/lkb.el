;;; Add an LKB menu to the emacs menu bar

(defconst lkb-menu
    '("LKB"
      ("View"
       ["Type hierarchy..." show-type-tree t]
       ["Type definition..." show-type-spec t]
       ["Expanded type..." show-type t]
       ["Lex definition..." show-lex-def t]
       ["Lex entry..." show-lex t]
       ["Word definition..." show-word-defs t]
       ["Word entries..." show-words t]
       ["Grammar rule..." show-grammar-rule t]
       ["Lexical rule..." show-lex-rule t])
      ("Parse"
       ["Parse input..." do-parse t]
       ["Show parse" show-parse t]
       ["Show edge..." show-parse-edge t]
       ["Show chart" show-chart t]
       ["Print chart" print-chart t]
       ["Batch parse" parse-sentences t]
       ["Compare..." compare-parses t])
      ("Generate"
       ["Generate..." generate-from-edge t]
       ["Show result" show-gen-result t]
       ["Show edge..." show-gen-edge t]
       ["Show chart" show-gen-chart t]
       ["Print chart" print-gen-chart t])
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

(define-lisp-commands '(show-type-tree show-type-spec show-type show-lex-def 
			show-lex show-word-defs show-words show-grammar-rule 
			show-lex-rule do-parse show-parse show-parse-edge 
			show-chart print-chart compare-parses 
			generate-from-edge show-gen-result show-gen-edge 
			show-gen-chart print-gen-chart))  
 
(defun redefine-type (arg)
  (interactive "P")
  (let ((beg 0)
        (end 0)
        (pos (point)))
    (setq beg (calc-begin-of-tdl-expression))
    (goto-char pos)
    (setq end (calc-end-of-tdl-expression))
    (eval-in-lisp (format "(redefine-type \"%s\")" 
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
 
