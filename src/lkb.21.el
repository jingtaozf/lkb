;;; Copyright (c) 1998-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions

;;; bmw (sep 03)
;;; - port to emacs 21 menus

;;; Add an LKB menu to the emacs menu bar

(defvar lkb-menu-installed nil)
(defvar *handled-types* '(list number string symbol))

;;;
;;; interface to common lisp
;;;

;; unusual return values cause system to hang, so only allow types
;; specified in *handled-types*
(defun eval-in-lisp (str)
	 (fi:eval-in-lisp "(let ((x %s)) (if (eval (cons 'or (mapcar #'(lambda (y) (typep x y)) '%s))) x '!!!unhandled-type!!!))" str *handled-types*))

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
	      'redefine-type))  
  (define-key map [menu-bar lkb break] (name-keymap "---"))
  (define-key map [menu-bar lkb generate] (name-keymap "Generate"))
  (define-key map [menu-bar lkb parse] (name-keymap "Parse"))
  (define-key map [menu-bar lkb view] (name-keymap "View"))
  (define-key map [menu-bar lkb load] (name-keymap "Load"))
  ;;
  ;; begin level 2
  ;; (generate)
  (define-key map [menu-bar lkb generate index]
    (fi::menu "Index"
	      'index-for-generator))
  (define-key map [menu-bar lkb generate show_chart]
    (fi::menu "Show chart"
	      'show-gen-chart))
  (define-key map [menu-bar lkb generate redisplay]
    (fi::menu "Redisplay realization"
	      'show-gen-result))
  (define-key map [menu-bar lkb generate from_edge]
    (fi::menu "Generate..."
	      'generate-from-edge))
  ;; (parse)
  (define-key map [menu-bar lkb parse batch_parse]
    (fi::menu "Batch parse..."
	      'parse-sentences-batch))
  (define-key map [menu-bar lkb parse print_chart]
    (fi::menu "Print chart"
	      'print-chart))
  (define-key map [menu-bar lkb parse show_chart]
    (fi::menu "Show chart"
	      'show-chart))
  (define-key map [menu-bar lkb parse redisplay_parse]
    (fi::menu "Redisplay parse"
	      'show-parse))
  (define-key map [menu-bar lkb parse parse_input]
    (fi::menu "Parse input..."
	      'clim-user::do-parse-batch))
  ;; (view)
  (define-key map [menu-bar lkb view lexical_rule]
    (fi::menu "Lexical rule..."
	      'show-lex-rule))
  (define-key map [menu-bar lkb view grammar_rule]
    (fi::menu "Grammar rule..."
	      'show-grammar-rule))
  (define-key map [menu-bar lkb view word_entries]
    (fi::menu "Word entries..."
	      'show-words))
  (define-key map [menu-bar lkb view lex_entry]
    (fi::menu "Lex entry..."
	      'show-lex))
  (define-key map [menu-bar lkb view type_expanded]
    (fi::menu "Expanded type..."
	      'show-type))
  (define-key map [menu-bar lkb view type_definition]
    (fi::menu "Type definition..."
	      'show-type-spec))
  (define-key map [menu-bar lkb view type_hierarchy]
    (fi::menu "Type hierarchy..."
	      'show-type-tree))
  ;; (load)
  (define-key map [menu-bar lkb load reload]
    (fi::menu "Reload grammar"
	      'reload-script-file))
  (define-key map [menu-bar lkb load complete]
    (fi::menu "Complete grammar..."
	      'read-script-file)))

(defun name-keymap (str) 
  (cons str (make-sparse-keymap str)))

(add-hook 'fi:inferior-common-lisp-mode-hook 
	  (function (lambda ()
		      (install-lkb-menu fi:inferior-common-lisp-mode-map))))

