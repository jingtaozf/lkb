;;; Copyright (c) 1998-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions

;;; bmw (sep 03)
;;; - port to emacs 21 menus

;;; Add an LKB menu to the emacs menu bar

(defvar *handled-types* '(list number string))

(defun eval-in-lisp (str)
	 (fi:eval-in-lisp "(let ((x (eval %s))) (if (or (eq x t) (eval (cons 'or (mapcar #'(lambda (y) (typep x y)) '%s)))) x '!!!unhandled-type!!!))" str *handled-types*))

(defun name-keymap (str) 
  (cons str (make-sparse-keymap str)))

(defun initialize-lkb-menu-bar-map ()
    (let* (
	   (map fi:inferior-common-lisp-mode-map))
      (define-key map [menu-bar lkb] (name-keymap "LKB"))
      

      (define-key map [menu-bar lkb redefine-type]
	(fi::menu "Redefine type"
		  'redefine-type :enable '(eval-in-lisp "*current-grammar-load-file*")))

     (define-key map [menu-bar lkb break] (name-keymap "---"))

     (define-key map [menu-bar lkb generate] (name-keymap "Generate"))
      (define-key map [menu-bar lkb generate index]
	(fi::menu "Index"
		  'index-for-generator :enable '(eval-in-lisp "*current-grammar-load-file*")))
      (define-key map [menu-bar lkb generate show_chart]
	(fi::menu "Show chart"
		  'show-gen-chart :enable '(eval-in-lisp "*current-grammar-load-file*")))
      (define-key map [menu-bar lkb generate redisplay]
	(fi::menu "Redisplay realization"
		  'show-gen-result :enable '(eval-in-lisp "*current-grammar-load-file*")))
      (define-key map [menu-bar lkb generate from_edge]
	(fi::menu "Generate..."
		  'generate-from-edge :enable '(eval-in-lisp "*current-grammar-load-file*")))



      (define-key map [menu-bar lkb parse] (name-keymap "Parse"))
      (define-key map [menu-bar lkb parse batch_parse]
	(fi::menu "Batch parse..."
		  'parse-sentences-batch :enable '(eval-in-lisp "*current-grammar-load-file*")))
      (define-key map [menu-bar lkb parse print_chart]
	(fi::menu "Print chart"
		  'print-chart :enable '(eval-in-lisp "*current-grammar-load-file*")))
      (define-key map [menu-bar lkb parse show_chart]
	(fi::menu "Show chart"
		  'show-chart :enable '(eval-in-lisp "*current-grammar-load-file*")))
      (define-key map [menu-bar lkb parse redisplay_parse]
	(fi::menu "Redisplay parse"
		  'show-parse :enable '(eval-in-lisp "*current-grammar-load-file*")))
      (define-key map [menu-bar lkb parse parse_input]
	(fi::menu "Parse input..."
		  'clim-user::do-parse-batch :enable '(eval-in-lisp "*current-grammar-load-file*")))

       (define-key map [menu-bar lkb view] (name-keymap "View"))
      (define-key map [menu-bar lkb view lexical_rule]
	(fi::menu "Lexical rule..."
		  'show-lex-rule :enable '(eval-in-lisp "*current-grammar-load-file*")))
      (define-key map [menu-bar lkb view grammar_rule]
	(fi::menu "Grammar rule..."
		  'show-grammar-rule :enable '(eval-in-lisp "*current-grammar-load-file*")))
      (define-key map [menu-bar lkb view word_entries]
	(fi::menu "Word entries..."
		  'show-words :enable '(eval-in-lisp "*current-grammar-load-file*")))
      (define-key map [menu-bar lkb view lex_entry]
	(fi::menu "Lex entry..."
		  'show-lex :enable '(eval-in-lisp "*current-grammar-load-file*")))
      (define-key map [menu-bar lkb view type_expanded]
	(fi::menu "Expanded type..."
		  'show-type :enable '(eval-in-lisp "*current-grammar-load-file*")))
      (define-key map [menu-bar lkb view type_definition]
	(fi::menu "Type definition..."
		  'show-type-spec :enable '(eval-in-lisp "*current-grammar-load-file*")))
      (define-key map [menu-bar lkb view type_hierarchy]
	(fi::menu "Type hierarchy..."
		  'show-type-tree :enable '(eval-in-lisp "*current-grammar-load-file*")))

     (define-key map [menu-bar lkb load] (name-keymap "Load"))
      (define-key map [menu-bar lkb load reload]
	(fi::menu "Reload grammar"
		  'reload-script-file :enable '(eval-in-lisp "*current-grammar-load-file*")))
      (define-key map [menu-bar lkb load complete]
	(fi::menu "Complete grammar..."
		  'read-script-file :enable t))))


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

(initialize-lkb-menu-bar-map)
