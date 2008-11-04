;;; -*- mode:emacs-lisp -*-
;;; Copyright (c) 1998-2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, 
;;;   Benjamin Waldron, Francis Bond; see `licence.txt' for conditions
;;;

;;; bmw (sep 03)
;;; - port to emacs 21 menus

;;; Add an LKB menu to the emacs menu bar

;;; 
;;; code for use with Emacs version = 21 (and above?)
;;;

(defun lkb-21 nil

(setq lkb-menu-installed nil)
(make-variable-buffer-local 'lkb-menu-installed)

(defvar *handled-types* '(list number string symbol))

;;;
;;; interface to common lisp
;;;

;; unusual return values cause system to hang, so only allow types
;; specified in *handled-types*
(defun eval-in-lisp (str)
	 (fi:eval-in-lisp "(let ((x %s)) (if (eval (cons 'or (mapcar #'(lambda (y) (typep x y)) '%s))) x '!!!unhandled-type!!!))" str *handled-types*))

			 
;;;
;;; menu construction
;;;

(defun install-lkb-menu (map)
  (unless lkb-menu-installed
    (install-lkb-menu-aux map)
    (setf lkb-menu-installed t)))

(defun install-lkb-menu-aux (map)
  (define-key map [menu-bar lkb] (name-keymap "LKB"))
  ;;
  ;; begin level 1
  (define-key map [menu-bar lkb redefine-type]
    (fi::menu "Redefine type"
	      'lkb::redefine-type))  
  (define-key map [menu-bar lkb break] (name-keymap "---"))
  (define-key map [menu-bar lkb lexicon] (name-keymap "Lexicon"))
  (define-key map [menu-bar lkb generate] (name-keymap "Generate"))
  (define-key map [menu-bar lkb parse] (name-keymap "Parse"))
  (define-key map [menu-bar lkb view] (name-keymap "View"))
  (define-key map [menu-bar lkb load] (name-keymap "Load"))
  ;;
  ;; begin level 2
  ;; (generate)
  (define-key map [menu-bar lkb lexicon batch_check]
    (fi::menu "Batch Check Lexicon"
	      'lkb::batch-check-lexicon))
  (define-key map [menu-bar lkb lexicon load_tdl]
    (fi::menu "Import TDL Entries to LexDB"
	      'lkb::command-load-tdl-to-scratch))
  ;; (generate)
  (define-key map [menu-bar lkb generate index]
    (fi::menu "Index"
	      'lkb::index-for-generator))
  (define-key map [menu-bar lkb generate print_chart]
    (fi::menu "Print chart input"
	      'lkb::print-gen-chart-input))
  (define-key map [menu-bar lkb generate print_chart]
    (fi::menu "Print chart"
	      'lkb::print-gen-chart))
  (define-key map [menu-bar lkb generate show_chart]
    (fi::menu "Show chart"
	      'lkb::show-gen-chart))
  (define-key map [menu-bar lkb generate redisplay]
    (fi::menu "Redisplay realization"
	      'lkb::show-gen-result))
  (define-key map [menu-bar lkb generate from_edge]
    (fi::menu "Generate..."
	      'lkb::generate-from-edge))
  ;; (parse)
  (define-key map [menu-bar lkb parse batch_parse]
    (fi::menu "Batch parse..."
	      'lkb::parse-sentences-batch))
  (define-key map [menu-bar lkb parse print_chart]
    (fi::menu "Print chart"
	      'lkb::print-chart))
  (define-key map [menu-bar lkb parse show_chart]
    (fi::menu "Show chart"
	      'lkb::show-chart))
  (define-key map [menu-bar lkb parse redisplay_parse]
    (fi::menu "Redisplay parse"
	      'lkb::show-parse))
  (define-key map [menu-bar lkb parse parse_input]
    (fi::menu "Parse input..."
	      'clim-user::do-parse-batch))
  ;; (view)
  (define-key map [menu-bar lkb view lexical_rule]
    (fi::menu "Lexical rule..."
	      'lkb::show-lex-rule))
  (define-key map [menu-bar lkb view grammar_rule]
    (fi::menu "Grammar rule..."
	      'lkb::show-grammar-rule))
  (define-key map [menu-bar lkb view word_entries]
    (fi::menu "Word entries..."
	      'lkb::show-words))
  (define-key map [menu-bar lkb view lex_entry]
    (fi::menu "Lex entry..."
	      'lkb::show-lex))
  (define-key map [menu-bar lkb view type_expanded]
    (fi::menu "Expanded type..."
	      'lkb::show-type))
  (define-key map [menu-bar lkb view type_definition]
    (fi::menu "Type definition..."
	      'lkb::show-type-spec))
  (define-key map [menu-bar lkb view type_hierarchy]
    (fi::menu "Type hierarchy..."
	      'lkb::show-type-tree))
  ;; (load)
  (define-key map [menu-bar lkb load reload]
    (fi::menu "Reload grammar"
	      'lkb::reload-script-file))
  (define-key map [menu-bar lkb load complete]
    (fi::menu "Complete grammar..."
	      'lkb::read-script-file)))

(defun name-keymap (str) 
  (cons str (make-sparse-keymap str)))

(add-hook 'fi:inferior-common-lisp-mode-hook 
	  (function (lambda ()
		      (install-lkb-menu fi:inferior-common-lisp-mode-map))))

(add-hook 'fi:common-lisp-mode-hook 
	  (function (lambda ()
		      (install-lkb-menu fi:common-lisp-mode-map))))

(add-hook 'fi:lisp-listener-mode-hook 
	  (function (lambda ()
		      (install-lkb-menu fi:lisp-listener-mode-map))))

(add-hook 'tdl-mode-hook 
	  (function (lambda ()
		      (install-lkb-menu tdl-mode-map))))

)
;;; end >= 21 mode

;;; 
;;; old code for use with Emacs version < 21
;;;

(defun lkb-pre-21 nil
  
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


(defconst lkb-menu
    '("LKB"
      ("Load"
       ["Complete grammar..." lkb::read-script-file t]
       ["Reload grammar" lkb::reload-script-file t])
      ("View"
       ["Type hierarchy..." lkb::show-type-tree t]
       ["Type definition..." lkb::show-type-spec t]
       ["Expanded type..." lkb::show-type t]
       ["Lex entry..." lkb::show-lex t]
       ["Word entries..." lkb::show-words t]
       ["Grammar rule..." lkb::show-grammar-rule t]
       ["Lexical rule..." lkb::show-lex-rule t])
      ("Parse"
       ["Parse input..." clim-user::do-parse-batch t]
       ["Redisplay pars" lkb::show-parse t]
       ["Show chart" lkb::show-chart t]
       ["Print chart" lkb::print-chart t]
       ["Batch parse..." lkb::parse-sentences-batch t])
      ("Generate"
       ["Generate..." lkb::generate-from-edge t]
       ["Redisplay realization" lkb::show-gen-result t]
       ["Show chart" lkb::show-gen-chart t]
       ["Print chart" lkb::print-gen-chart t]
       ["Print chart input" lkb::print-gen-chart-input t]
       ["Index" lkb::index-for-generator t])
      "---"
      ["Redefine type" lkb::redefine-type t]
))
      
(add-hook 'fi:lisp-mode-hook 
	  (function (lambda ()
		      (fi::install-menubar lkb-menu))))

(add-hook 'tdl-mode-hook
	  (function (lambda ()
		      (fi::install-menubar lkb-menu))))

)
;;; end pre-21 mode

;;; identical in both
(defun define-lisp-commands (commands)
  (dolist (com commands)
    (eval `(defun ,com ()
	     (interactive ())
	     (eval-in-lisp ,(format "(%s)" com))))))

(define-lisp-commands 
    '(lkb::read-script-file 
      lkb::reload-script-file 
      lkb::show-type-tree
      lkb::show-type-spec 
      lkb::show-type 
      lkb::show-lex lkb::show-words
      lkb::show-grammar-rule 
      lkb::show-lex-rule 
      clim-user::do-parse-batch 
      lkb::show-parse 
      lkb::show-chart 
      lkb::print-chart 
      lkb::parse-sentences-batch 
      lkb::generate-from-edge 
      ;; generator commands
      lkb::show-gen-result 
      lkb::show-gen-chart 
      lkb::print-gen-chart
      lkb::print-gen-chart-input
      lkb::index-for-generator
      ;; lexicon commands
      lkb::batch-check-lexicon
      lkb::command-load-tdl-to-scratch))


(if
    (and (boundp 'emacs-major-version)
	 (>= emacs-major-version 21))
    (lkb-21)
  (lkb-pre-21))

(defun redefine-type (arg)
  (interactive "P")
  (let ((beg 0)
        (end 0)
        (pos (point)))
    (setq beg (calc-begin-of-tdl-expression))
    (goto-char pos)
    (setq end (calc-end-of-tdl-expression))
    (eval-in-lisp (format "(lkb::redefine-type \"%s\")" 
			  (buffer-substring-no-properties beg (min (1+ end) (point-max)))))
    (goto-char pos)))


(defun find-tdl-definition (thing file)
  (fi::ensure-buffer-visible (find-file file))
  (goto-char 0)
  (re-search-forward (format "%s\\W+:" (regexp-quote thing)))
  (goto-char (match-beginning 0)))
 

;;;
;;; Some key bindings for those having trouble with encodings
;;; FCB 2003-12-25

(add-hook 'fi:inferior-common-lisp-mode-hook
	  (function (lambda ()
		      ;;; add parse key
		      (define-key fi:inferior-common-lisp-mode-map 
			  "\C-cp" 'lkb-do-parse)
		      (define-key fi:inferior-common-lisp-mode-map 
			  "\C-cl" 'lkb-show-words)
		      (define-key fi:inferior-common-lisp-mode-map 
			  "\C-cL" 'lkb-show-words-expanded)
		      (define-key fi:inferior-common-lisp-mode-map 
			  "\C-cu" 'lkb-tsdb-cpu)
		      (define-key fi:inferior-common-lisp-mode-map 
			  "\C-cr" 'lkb-mt-interactive)
		      (define-key fi:inferior-common-lisp-mode-map 
			"\C-cg" 'lkb-rsa)
		      )))

(defun lkb-do-parse ()
  "Prompt for sentence to parse"
  (interactive)
  (goto-char (point-max))
  (insert-string "(lkb::do-parse-tty \"\")")
  (backward-char 2))

(defun lkb-show-words ()
  "prompt for list of words to look up (unexpanded)"
  (interactive)
  (goto-char (point-max))
  (insert-string "(lkb::show-word-aux-tty \"\" nil)")
  (backward-char 6))

(defun lkb-show-words-expanded ()
    "prompt for list of words to look up (expanded)"
  (interactive)
  (goto-char (point-max))
  (insert-string "(lkb::show-word-aux-tty \"\" t)")
  (backward-char 4))

(defun lkb-tsdb-cpu ()
    "prompt for [incr tsdb()] cpu"
  (interactive)
  (goto-char (point-max))
  (insert-string "(tsdb::tsdb :cpu : :file t)")
  (backward-char 9))

(defun lkb-mt-interactive ()
    "prompt for [incr tsdb()] cpu"
  (interactive)
  (goto-char (point-max))
  (insert-string "(mt::parse-interactively \"\")")
  (backward-char 2))

(defun lkb-rsa ()
    "prompt for grammar"
  (interactive)
  (goto-char (point-max))
  (insert-string "(lkb::read-script-file-aux \"/lkb/script\")")
  (backward-char 13))


;;; RMRS display utility

(defun display-rmrs (arg)
  (interactive "P")
  (let ((beg 0)
        (end 0)
        (pos (point)))
    (setq beg (calc-begin-of-rmrs-expression))
    (goto-char pos)
    (setq end (calc-end-of-rmrs-expression))
    (eval-in-lisp (format "(lkb::display-rmrs-from-string \"%s\")" 
			  (buffer-substring-no-properties beg (min (1+ end) (point-max)))))
    (goto-char pos)))

(defun calc-begin-of-rmrs-expression ()
  "calculates begin of a rmrs expression in XML"
  (or (re-search-backward "<rmrs " nil t)
               (point-min)))

(defun calc-end-of-rmrs-expression ()
  "calculates end of an rmrs expression"
    (or (re-search-forward "</rmrs>" nil t)
                 (point-max)))

(defun select-rmrs (arg)
  (interactive "P")
  (let ((beg 0)
        (end 0)
        (pos (point)))
    (setq beg (calc-begin-of-rmrs-expression))
    (goto-char pos)
    (setq end (calc-end-of-rmrs-expression))
    (eval-in-lisp (format "(lkb::select-rmrs-from-emacs \"%s\")" 
			  (buffer-substring-no-properties beg (min (1+ end) (point-max)))))
    (goto-char pos)))

(defun generate-from-rmrs (arg)
  (interactive "P")
  (let ((beg 0)
        (end 0)
        (pos (point)))
    (setq beg (calc-begin-of-rmrs-expression))
    (goto-char pos)
    (setq end (calc-end-of-rmrs-expression))
    (eval-in-lisp (format "(lkb::generate-rmrs-from-emacs \"%s\")" 
			  (buffer-substring-no-properties beg (min (1+ end) (point-max)))))
    (goto-char pos)))

;;; By putting
;;; (add-to-list 'auto-mode-alist '("\\.mrs\\'" . sgml-mode))
;;; (add-to-list 'auto-mode-alist '("\\.rmrs\\'" . sgml-mode))
;;; in the .emacs, SGML mode will be invoked for .(r)mrs extensions

;;; The following makes these commands available via keystrokes
;;; in sgml mode (includes .xml files)

(add-hook 'sgml-mode-hook
	  (function (lambda ()
		      (define-key sgml-mode-map 
			  "\C-cr" 'display-rmrs)
		      (define-key sgml-mode-map 
			  "\C-cs" 'select-rmrs)
		      (define-key sgml-mode-map 
			  "\C-cg" 'generate-from-rmrs)
		      )))

;;; following would make them global but should remain commented
;;; out because of possible overlap with other commands

; (global-set-key "\C-cr" 'display-rmrs)
; (global-set-key "\C-cs" 'select-rmrs)
; (global-set-key "\C-cg" 'generate-from-rmrs)

;;; MRS display utility

(defun display-mrs (arg)
  (interactive "P")
  (let ((beg 0)
        (end 0)
        (pos (point)))
    (setq beg (calc-begin-of-mrs-expression))
    (goto-char pos)
    (setq end (calc-end-of-mrs-expression))
    (eval-in-lisp (format "(lkb::display-mrs-from-string \"%s\")" 
			  (buffer-substring-no-properties beg (min (1+ end) (point-max)))))
    (goto-char pos)))

(defun calc-begin-of-mrs-expression ()
  "calculates begin of a mrs expression in XML"
  (or (re-search-backward "<mrs>" nil t)
               (point-min)))

(defun calc-end-of-mrs-expression ()
  "calculates end of an mrs expression"
    (or (re-search-forward "</mrs>" nil t)
                 (point-max)))

; as above


(add-hook 'sgml-mode-hook
	  (function (lambda ()
		      (define-key sgml-mode-map 
			  "\C-cm" 'display-mrs))))

; (global-set-key "\C-cm" 'display-mrs)

;;;
;;; open a file in a new buffer, killing its existing buffer, if necessary
;;;
(defun rogue-find-file (file)
  (if (file-exists-p file)
    (let ((buffer (get-file-buffer file))
          (find-file-hooks nil))
      (if buffer (kill-buffer buffer))
      (find-file file))))
