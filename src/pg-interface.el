;;; Copyright (c) 2003 Benjamin Waldron
;;; see licence.txt for conditions

;;; Add a PG menu to the emacs menu bar

(require 'widget)

(eval-when-compile
  (require 'wid-edit))

;;;
;;; globals
;;;

(defvar *lexdb-completion-lists* nil)
(defvar *lexdb-record-features* '(:name :type :orthography :keyrel :keytag :altkey :altkeytag :alt2key :compkey :ocompkey :lang :country :dialect :domains :genres :register :confidence :comments :exemplars))
(defvar *lexdb-completion-features*  '(:name :type :orthography :keyrel :keytag :altkey :altkeytag :alt2key :compkey :ocompkey :lang :country :dialect :domains :genres :register :confidence :comments :exemplars))
(defvar *lexdb-minibuffer-max* 100)
(defvar *lexdb-active-id-ring* nil)

;;;
;;; buffer local vbles
;;;

(setq lexdb-fw-map nil)
(setq lexdb-fsize-map nil)
(setq lexdb-id nil)

(make-variable-buffer-local 'lexdb-fw-map)
(make-variable-buffer-local 'lexdb-fsize-map)
(make-variable-buffer-local 'lexdb-id)

;;;
;;; connection to common lisp process
;;;

(defvar *cle-handled-types* '(list number string symbol))

;; unusual return values cause system to hang...
(defun cle-eval (str)
	 (fi:eval-in-lisp "(let ((x %s)) (if (eval (cons 'or (mapcar #'(lambda (y) (typep x y)) '%s))) x '!!!unhandled-type!!!))" str *cle-handled-types*))

;;;
;;; menu items
;;;

(defun lexdb-make-name-keymap (str) 
  (cons str (make-sparse-keymap str)))

(defun lexdb-mode-initialize-menu-bar ()
  (let* ((map lexdb-mode-map))
    ;;
    ;; top level
    (define-key map [menu-bar LexDB] (lexdb-make-name-keymap "LexDB"))

    ;;
    ;; level 1
    (define-key map [menu-bar LexDB view-scratch]
      '(menu-item "View scratch" lexdb-view-scratch 
		  :keys "M-vs"
		  :enable (cle-eval-lexdb 'connection)))
    (define-key map [menu-bar LexDB next-id]
      '(menu-item "Next id" lexdb-advance-id 
		  :keys "M-n"
		  :enable (cle-eval-lexdb 'connection)))
    (define-key map [menu-bar LexDB search]
      '(menu-item "Search ids" lexdb-search-field-val 
		  :keys "M-s"
		  :enable (cle-eval-lexdb 'connection)))
    (define-key map [menu-bar LexDB cross-ref]
      '(menu-item "Cross reference ids" lexdb-lookup 
		  :keys "M-TAB"
		  :enable (cle-eval-lexdb 'connection)))
    (define-key map [menu-bar LexDB break] (lexdb-make-name-keymap "---"))
    (define-key map [menu-bar LexDB commit]
      '(menu-item "Commit" lexdb-commit-record 
		  :keys "C-c"
		  :enable (and lexdb-id
			       (cle-eval "(lexdb-fn 'connection)"))))
    (define-key map [menu-bar LexDB edit]
      '(menu-item "Edit" lexdb-load-record 
		  :keys "C-l"
		  :enable (cle-eval-lexdb 'connection)))))

(add-hook 'lexdb-mode-hook 
	  (function (lambda ()
		      (lexdb-mode-initialize-menu-bar))))
;;;
;;; major mode defn
;;;

(defun make-lexdb-keymap nil
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-vs" 'lexdb-view-scratch)
    (define-key map "\M-va" 'lexdb-view-merge-add)
    (define-key map "\C-l" 'lexdb-load-record)
    (define-key map "\C-c" 'lexdb-commit-record)
    (define-key map "\C-n" 'lexdb-normalize-buffer)
    (define-key map "\M-\t" 'lexdb-lookup)
    (define-key map "\M-n" 'lexdb-advance-id)
    (define-key map "\M-s" 'lexdb-search-field-val)
    (define-key map "\t" 'lexdb-complete-field)
    (setq lexdb-mode-map (append map widget-field-keymap))))

(defun lexdb-mode ()
  "Major mode for LexDB
Turning on lexdb-mode runs the hook `lexdb-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (use-local-map (make-lexdb-keymap))
  (setq local-abbrev-table text-mode-abbrev-table)
  (set-syntax-table text-mode-syntax-table)
  (setq mode-name "LexDB")
  (setq major-mode 'lexdb-mode)
  (run-hooks 'lexdb-mode-hook))    

;;;
;;; interactives
;;;

(defun lexdb (id)
  (interactive (list (read-from-minibuffer "Lex Id: " "")))
  (unless (cle-eval-lexdb 'connection)
    (if (not (cle-eval "(common-lisp-user::featurep :lkb)"))
	(error "Please load the the LKB"))
    (if (not (cle-eval "(common-lisp-user::featurep :psql)"))
	(error "The running version of LKB is not :psql enabled"))
    (princ "Please wait... (initializing LexDb) ")
    (cle-initialize-psql))
  (lexdb-load-record id))

(defun lexdb-load-record (id)
  (interactive (list (read-from-minibuffer "Lex Id: " "")))
  (unless (cle-eval-lexdb 'connection)
    (error "no connection to LexDb"))
  (lexdb-load-record-aux id)
  ;;(beep)
  )

(defun lexdb-normalize-buffer (buffer)
  (interactive (list (format "%s" (current-buffer))))
  (unless (cle-eval-lexdb 'connection)
    (error "no connection to LexDb"))
  (lexdb-normalize-buffer-aux buffer)
  ;;(beep)
  )

(defun lexdb-commit-record (buffer)
  (interactive (list (format "%s" (current-buffer))))
  (unless (cle-eval-lexdb 'connection)
    (error "no connection to LexDb"))
  (if (lexdb-commit-record-aux buffer)
      (beep)))

(defun lexdb-complete-field nil
  (interactive)
  (lexdb-complete-field-aux))

(defun lexdb-lookup ()
  (interactive)
  (lexdb-lookup-aux))

(defun lexdb-advance-id ()
  (interactive)
  (lexdb-advance-id-aux))

(defun lexdb-view-scratch ()
  (interactive)
  (lexdb-view-scratch-aux))

(defun lexdb-view-merge-add ()
  (interactive)
  (lexdb-view-merge-add-aux))

(defun lexdb-search-field-val (val-str)
  (interactive 
   (list 
    (read-from-minibuffer "Value: " 
			  (l:widget-val-normd (widget-field-find (point))))))
  (lexdb-search-field-val-aux val-str))

;;;
;;; main functions
;;;

(defun lexdb-search-field-val-aux (val-str)
  (let* ((widget (widget-field-find (point))))
    (if widget
	  (lexdb-lookup-aux2 (l:widget-to-field-kw widget)
		  (l:normalize val-str)
		  )
      (error "not in an editable field"))))

(defun lexdb-advance-id-aux nil
  (when *lexdb-active-id-ring*
    (lexdb-load-record-aux (car *lexdb-active-id-ring*))
    (setf *lexdb-active-id-ring*
	  (cdr *lexdb-active-id-ring*))
    (princ *lexdb-active-id-ring*)
    t))
    
(defun lexdb-view-scratch-aux nil
  (let ((filename "~/tmp/lexdb-scratch.csv"))
    (cle-dump-scratch filename)
    (find-file filename)))    

(defun lexdb-view-merge-add-aux nil
  (let ((filename "~/tmp/lexdb.new_entries"))
    (find-file filename)))    

(defun lexdb-commit-record-aux (buffer)
  (let* ((record (lexdb-read-record-buffer buffer)))
    (lexdb-display-record record buffer)
    (when (y-or-n-p "Confirm commit record: ")
      (lexdb-store-record (car record))
      (with-current-buffer buffer
	(rename-buffer (cdr (assoc :name (car record)))))
      t)))

(defun lexdb-complete-field-aux nil
  (let* ((widget (widget-field-find (point))))
    (if widget
	  (lexdb-complete-field-aux2
	   (l:widget-to-field-kw widget))
      (error "not in an editable field"))))

(defun lexdb-complete-field-aux2 (field-kw)
  (let* ((widget (cdr (assoc field-kw lexdb-fw-map)))
	 (value-str (cut-white-spc (widget-value widget)))
	 (start (- (point) (length value-str)))
	 (completion-list (cdr (assoc field-kw *lexdb-completion-lists*)))
	 (completion (try-completion value-str completion-list))
	 (alternatives (all-completions value-str completion-list)))
    (when (< (length alternatives) 10000)
      (princ (mapconcat 'identity alternatives " ")))
    (cond
     ((null completion)
      (beep)
      )
     ((stringp completion)
      (widget-value-set widget
			completion)
      (call-interactively 'lexdb-normalize-buffer)
      (set-window-point (selected-window) (+ start (length completion)))))))

(defun lexdb-lookup-aux nil
  (let* ((widget (widget-field-find (point))))
    (if widget
	  (lexdb-lookup-aux2 (l:widget-to-field-kw widget)
		  (l:widget-val-normd widget)
		  )
      (error "not in an editable field"))))

(defun lexdb-normalize-buffer-aux (buffer)
  (let ((record (lexdb-read-record-buffer buffer)))
    (lexdb-display-record record buffer)))

(defun lexdb-load-record-aux (id)
  (let* ((buffer (format "%s" id))
	 (record (lexdb-retrieve-record id)))
    (lexdb-display-record record buffer)))

(defun lexdb-display-record (record buffer)
  (unless (or (stringp buffer) (bufferp buffer))
    (setf buffer (cle-force-str buffer)))
  (save-current-buffer
    (if (get-buffer buffer)
	(kill-buffer buffer))
    (switch-to-buffer (get-buffer-create buffer))
    (lexdb-mode)
    (setf lexdb-fsize-map (cdr record))
    (setf lexdb-id (format "%s" buffer))
    (let ((inhibit-read-only t))
      (erase-buffer))
    (setf lexdb-fw-map (mapcar #'l:fv-pair-2-fw-pair (l:prepare-record (car record))))
    (widget-setup)
    lexdb-fw-map))
  
(defun lexdb-read-record-buffer (buffer)
  (with-current-buffer buffer
    (let* ((lines (split-string (buffer-string) "\n"))
	   (fv-pairs (remove nil 
			     (mapcar #'l:fw-pair-2-fv-pair lexdb-fw-map))))
      (cons fv-pairs lexdb-fsize-map))))

(defun lexdb-retrieve-completion-lists nil
  ;;(terpri)
  (princ "generating completion lists for ")
  (setf *lexdb-completion-lists*
	(mapcar #'(lambda (x)
		    (princ (format "%s " x))
		    ;;(terpri)
		    (cons x
			  (cle-get-completion-list x)))
		*lexdb-completion-features*))
  (beep))
	  
(defun lexdb-retrieve-record (id)
  (let ((fields (cle-retrieve-record-fields id))
	(sizes (cle-retrieve-record-sizes)))
    (unless (assoc :name fields)
      (princ (format "%s not found! " id))
      (setf fields (push 
		    (cons :name (l:val-str id))
		    fields)))
    (unless *lexdb-completion-lists*
      (lexdb-retrieve-completion-lists))
    (cons
     fields
     sizes)))

(defun lexdb-store-record (record-in)
  (push 
   (cons :orthkey (first (split-string (cdr (assoc :orthography record-in)))))
   record-in)
  (princ "please wait... ")
  ;;(terpri)
  (cle-store-record record-in)
  (lexdb-update-completion-lists record-in)
  (cle-empty-psql-cache)
  (princ (format "record saved to lexdb %s. " (cle-dbname))))

(defun lexdb-lookup-aux2 (field-kw val-str)
  (let ((ids (cle-lookup field-kw val-str)))
    (l:princ-list ids)
    (if ids
	(setf *lexdb-active-id-ring* (make-ring ids)))))

(defun lexdb-update-completion-lists (record)
  (mapcar #'(lambda (x) (lexdb-update-completion-list (car x)
						(cdr x)))
	  record))

(defun lexdb-update-completion-list (field-kw val-str)
  (let ((completion-list (assoc field-kw *lexdb-completion-lists*)))
    (when (and completion-list
	       (not (string= val-str "")))
      (setf completion-list
	    (pushnew (list val-str)
		     (cdr completion-list)
		     :test #'equal)))))


;;;
;;; lexdb util fns
;;;

(defun l:prepare-record (full-record)
  (mapcar #'(lambda (x) (cons x 
			      (l:field-val-str (assoc x full-record))))
			      *lexdb-record-features*))

(defun l:field-val-str (record-elt)
  (l:val-str (cdr record-elt)))

(defun l:val-str (val)
    (cond
     ((stringp val)
      val)
     ((numberp val)
      (format "%s" val))
     ((null val)
      "")
     (t
      (error "unhandled field value type"))))

(defun l:widget-val-normd (widget)
  (l:normalize 
   (widget-value widget)))

(defun l:normalize (str)
  (cut-white-spc str))

(defun l:widget-to-field-kw (widget)
  (car (find widget lexdb-fw-map :key 'cdr)))

(defun l:princ-list (l)
  (princ 
   (mapconcat #'cle-force-str 
	      (truncate-list l *lexdb-minibuffer-max*)
	      " ")))

(defun l:fv-pair-2-fw-pair (x)
  (cons 
   (car x)
   (progn 
     (widget-insert "\n"
		    (make-string (max 0 (- 15 (length (kw2str (car x))))) ? ) 
		    (upcase (kw2str (car x))) 
		    ": ")
     (widget-create 'editable-field
		    :size (min 50 (l:field-size (car x)))
		    :keymap nil
		    :value-face nil
		    (cdr x)))))

(defun l:fw-pair-2-fv-pair (x)
  (cons
   (car x)
   (l:widget-val-normd (cdr x))))

(defun l:empty-fv-pair (field-kw)
  (cons field-kw nil))

(defun l:field-size (kw)
  (third (assoc kw lexdb-fsize-map)))

;;;
;;; util fns
;;;

(defun kw2str (kw)
  (let ((name-str (symbol-name kw)))
    (if (= (aref name-str 0) 58)
	(substring name-str 1)
      (error "keyword expected"))))

(defun truncate-list (l n)
  (let ((out)
	(i 0))
    (while (and
	    l
	    (< i n))
      (push (pop l) out)
      (setf i (1+ i)))
    (reverse out)))    

(defun cut-white-spc (str)
  (mapconcat #'(lambda (x) x) (remove "" (split-string str)) " "))

(defun make-ring (l)
  (setf (cdr (last l)) l))


;;;
;;; allegro lisp interface fns
;;;

(defun cle-eval-lexdb (fn-name &rest fn-args)
  (cle-eval 
   (format "(and (fboundp 'lexdb-fn) (lkb::lexdb-fn '%s %s))"
			fn-name
			(mapconcat #'cle-force-str fn-args " "))))

(defun cle-force-str (x)
  (if (stringp x)
      x
    (format "%S" x)))

(defun cle-lisp-str (id)
  (format "\"%s\"" 
	  (mapconcat #'(lambda (x) 
			 (if (string= x "\"")
			     "\\\""
			   x))
		     (split-string (format "%s" id) "")
		     "")))

(defun cle-retrieve-record-fields (id)
  (cle-eval-lexdb 'retrieve-record-str (cle-lisp-str id)))

(defun cle-retrieve-record-sizes nil
   (cle-eval-lexdb 'get-field-size-map))

(defun cle-store-record (record-in)
  (cle-eval-lexdb 'set-lex-entry 
		  (format "(make-instance 'psql-lex-entry :fv-pairs '%S))" record-in)))

(defun cle-get-completion-list (field)
  (mapcar #'list
	  (cle-eval-lexdb 'get-value-set field)))

(defun cle-empty-psql-cache nil
  (cle-eval-lexdb 'empty-cache))

(defun cle-initialize-psql nil
  (cle-eval "(initialize-psql-lexicon)"))

(defun cle-dbname nil
  (cle-eval-lexdb 'dbname))

(defun cle-lookup (field-kw val-str)
  (if (or (string= val-str "") (null val-str))
      (setf val-str nil)
    (setf val-str (cle-lisp-str val-str)))
  (cle-eval-lexdb 'lookup
		 field-kw
		 val-str))
  
(defun cle-dump-scratch (filename)
  (cle-eval (format "(dump-scratch \"%s\")" (cle-force-str filename))))

