;;; Copyright (c) 2003-2004 Benjamin Waldron
;;; see licence.txt for conditions

;; Portions copyright (c) 1996, 1997, 1999, 2000, 2001 Free Software Foundation, Inc.

;; Free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Add a PG menu to the emacs menu bar

(require 'widget)

(eval-when-compile
  (require 'wid-edit))

;; default widgets misbehave 
;; due to markers defining :from and :to points
(define-widget 'editable-field-fixed-size 'default
  "An editable text field."
  :convert-widget 'widget-value-convert-widget
  :keymap widget-field-keymap
  :format "%v"
  :help-echo "M-TAB: complete field; RET: enter value"
  :value ""
  :prompt-internal 'widget-field-prompt-internal
  :prompt-history 'widget-field-history
  :prompt-value 'widget-field-prompt-value
  :action 'widget-field-action
  :validate 'widget-field-validate
  :valid-regexp ""
  :error "Field's value doesn't match allowed forms"
  :value-create 'widget-field-value-create
  :value-delete 'widget-field-value-delete
  :value-get 'widget-field-value-get
  :match 'widget-field-match
  :create 'l:widget-fixed-size-create
  :delete 'l:widget-fixed-size-delete)

;; delete-region based on :from + :size
;; instead of :to which is unreliable
(defun l:widget-fixed-size-delete (widget)
  "Remove widget from the buffer."
  (let ((from (widget-get widget :from))
	(to (widget-get widget :to))
	(inactive-overlay (widget-get widget :inactive))
	(button-overlay (widget-get widget :button-overlay))
	(sample-overlay (widget-get widget :sample-overlay))
	(doc-overlay (widget-get widget :doc-overlay))
	(inhibit-modification-hooks t)
	(inhibit-read-only t))
    (widget-apply widget :value-delete)
    (when inactive-overlay
      (delete-overlay inactive-overlay))
    (when button-overlay
      (delete-overlay button-overlay))
    (when sample-overlay
      (delete-overlay sample-overlay))
    (when doc-overlay
      (delete-overlay doc-overlay))
    (delete-region from (+ 1 from 
			   (widget-get widget :size)))
    (set-marker from nil)
    (set-marker to nil))
  (widget-clear-undo))

;; marker type of :from should not be 'before-insertion (t)
(defun l:widget-fixed-size-create (widget)
  "Create WIDGET at point in the current buffer."
  (widget-specify-insert
   (let ((from (point))
	 button-begin button-end
	 sample-begin sample-end
	 doc-begin doc-end
	 value-pos)
     (insert (widget-get widget :format))
     (goto-char from)
     ;; Parse escapes in format.
     (while (re-search-forward "%\\(.\\)" nil t)
       (let ((escape (char-after (match-beginning 1))))
	 (delete-backward-char 2)
	 (cond ((eq escape ?%)
		(insert ?%))
	       ((eq escape ?\[)
		(setq button-begin (point))
		(insert (widget-get-indirect widget :button-prefix)))
	       ((eq escape ?\])
		(insert (widget-get-indirect widget :button-suffix))
		(setq button-end (point)))
	       ((eq escape ?\{)
		(setq sample-begin (point)))
	       ((eq escape ?\})
		(setq sample-end (point)))
	       ((eq escape ?n)
		(when (widget-get widget :indent)
		  (insert ?\n)
		  (insert-char ?  (widget-get widget :indent))))
	       ((eq escape ?t)
		(let ((image (widget-get widget :tag-glyph))
		      (tag (widget-get widget :tag)))
		  (cond (image
			 (widget-image-insert widget (or tag "image") image))
			(tag
			 (insert tag))
			(t
			 (princ (widget-get widget :value)
				(current-buffer))))))
	       ((eq escape ?d)
		(let ((doc (widget-get widget :doc)))
		  (when doc
		    (setq doc-begin (point))
		    (insert doc)
		    (while (eq (preceding-char) ?\n)
		      (delete-backward-char 1))
		    (insert ?\n)
		    (setq doc-end (point)))))
	       ((eq escape ?v)
		(if (and button-begin (not button-end))
		    (widget-apply widget :value-create)
		  (setq value-pos (point))))
	       (t
		(widget-apply widget :format-handler escape)))))
     ;; Specify button, sample, and doc, and insert value.
     (and button-begin button-end
	  (widget-specify-button widget button-begin button-end))
     (and sample-begin sample-end
	  (widget-specify-sample widget sample-begin sample-end))
     (and doc-begin doc-end
	  (widget-specify-doc widget doc-begin doc-end))
     (when value-pos
       (goto-char value-pos)
       (widget-apply widget :value-create)))
   (let ((from (point-min-marker))
	 (to (point-max-marker)))
     (set-marker-insertion-type from nil)
     (set-marker-insertion-type to nil)
     (widget-put widget :from from)
     (widget-put widget :to to)))
  (widget-clear-undo))

;;;
;;; globals
;;;

(defvar *lexdb-read-only* '(:version :userid :modstamp))
(defvar *lexdb-record-features* '(:name :type :orthography :keyrel :keytag :altkey :altkeytag :alt2key :compkey :ocompkey :source :lang :country :dialect :domains :genres :register :confidence :comments :exemplars :flags :version :userid :modstamp))
(defvar *lexdb-minibuffer-max* 80)
(defvar *lexdb-active-id-ring* nil)
(defvar *new-entries-buffer* "*new-entries*")
;;;
;;; buffer local vbles
;;;

(setq lexdb-fw-map nil)
(setq lexdb-fsize-map nil)
(setq lexdb-id nil)
(setq lexdb-record nil)

(make-variable-buffer-local 'lexdb-fw-map)
(make-variable-buffer-local 'lexdb-fsize-map)
(make-variable-buffer-local 'lexdb-id)
(make-variable-buffer-local 'lexdb-record)

;;;
;;; connection to common lisp process
;;;

(defvar *cle-handled-types* '(list number string symbol))

;; unusual return values cause system to hang...
(defun cle-eval (str)
  (condition-case descr
	 (fi:eval-in-lisp "(let* ((x %s))
  (if (eval 
       (cons 'or 
	     (mapcar #'(lambda (y) (typep x y)) '%s)))
      x '!!!unhandled-type!!!))" 
			  str *cle-handled-types*)
      (error (princ (format "%s" descr))
	     (sit-for 4))))

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
  (interactive (list (l:completing-read-dyn "Lex Id: ")))
  (lexdb-load-record id))

(defun lexdb-load-record (id)
  (interactive (list (l:completing-read-dyn "Lex Id: ")))
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
  (let ((buffer *new-entries-buffer*))
    (if (get-buffer buffer)
	(kill-buffer buffer))
    (with-current-buffer (get-buffer-create buffer)
      (lexdb-mode)
      (apply
       #'insert 
       (mapcar 
	#'(lambda (x)
	     'bold
	     (format "%s:%s\t%s\n"
		     (nth 0 x)
		     (nth 1 x)
		     (nth 2 x)
	     ))
	(cdr 
	 (cle-new-entries)))))
    (switch-to-buffer buffer)))

(defun lexdb-commit-record-aux (buffer)
  (lexdb-update-record-from-buffer buffer)
  (lexdb-display-record buffer)
  (when (y-or-n-p "Confirm commit record: ")
    (lexdb-store-record (car lexdb-record))
    (lexdb-load-record-aux (cdr (assoc :name (car lexdb-record)))))
    (with-current-buffer buffer
    t))

(defun lexdb-complete-field-aux nil
  (let* ((widget (widget-field-find (point))))
    (if widget
	  (lexdb-complete-field-aux2
	   (l:widget-to-field-kw widget))
      (error "not in an editable field"))))

(defun lexdb-complete-field-aux2 (field-kw)
  (let* ((widget (cdr (assoc field-kw lexdb-fw-map)))
	 (value-str (cut-white-spc (widget-value widget)))
	 (alternatives (cle-complete field-kw value-str))
	 (completion (try-completion value-str 
				     (mapcar #'list
					     alternatives))))
    (l:princ-list alternatives)
    (cond
     ((null completion)
      (beep))
     ((stringp completion)
      (widget-value-set widget
			completion)
      (widget-setup)
      (set-window-point (selected-window) 
			(+ (widget-get widget :from) (length completion)))))))

(defun lexdb-lookup-aux nil
  (let* ((widget (widget-field-find (point))))
    (if widget
	  (lexdb-lookup-aux2 (l:widget-to-field-kw widget)
		  (l:widget-val-normd widget)
		  )
      (error "not in an editable field"))))

(defun lexdb-normalize-buffer-aux (buffer)
  (lexdb-update-record-from-buffer buffer)
  (let ((record lexdb-record)
	(pos (point)))
    (kill-buffer buffer)
    (with-current-buffer (get-buffer-create buffer)
      (lexdb-mode)
      (setf lexdb-record record)
      (lexdb-display-record buffer)
      (goto-char pos))))

(defun lexdb-load-record-aux (id)
  (let ((rec (lexdb-retrieve-record id)))
    (if (equal id "") 
	(setf id "?new?"))
    (setf buffer (format "%s" id))
    (if (get-buffer buffer)
	(kill-buffer buffer))
    (with-current-buffer (get-buffer-create buffer)
      (lexdb-mode)
      (setf lexdb-record rec)
      (lexdb-display-record buffer))))

(defun lexdb-display-record (buffer)
  (with-current-buffer buffer
    (switch-to-buffer buffer)
    (unless lexdb-record
      (error "buffer has no associated record"))
    (setf lexdb-fsize-map (cdr lexdb-record))
    (setf lexdb-id (format "%s" buffer))
    (let ((inhibit-read-only t))
      (erase-buffer))
    (setf lexdb-fw-map 
	  (remove-if-not #'cdr
			 (mapcar #'l:fv-pair-2-fw-pair 
				 (l:prepare-record (car lexdb-record)))))
    (widget-setup)
    lexdb-fw-map))
   
(defun lexdb-update-record-from-buffer (buffer)
  (with-current-buffer buffer
    (mapcar #'(lambda (x) (update-from-widget x (car lexdb-record)))
	    lexdb-fw-map)))

(defun update-from-widget (fw-pair record)
  (let* ((fv-pair (l:fw-pair-2-fv-pair fw-pair))
	 (feat (car fv-pair))
	 (val (cdr fv-pair))
	 (record-elt (assoc feat record)))
    (when val
      (if (null record-elt) 
	  (error "feature not found in record"))
      (setf (cdr record-elt) val))))
      
(defun lexdb-retrieve-record (id)
  (let ((fields (cle-retrieve-record-fields id))
	(sizes (cle-retrieve-record-sizes)))
    (unless fields
      (princ (format "%s not found! " id))
      (setf fields (l:make-empty-record id)))
    (setf lexdb-record
	  (cons
	   fields
	   sizes))))

(defun lexdb-store-record (record-in)
  (if (equal (cdr (assoc :name record-in))
	     "")
      (error "cannot commit record with no NAME"))
  (push 
   (cons :orthkey (first (split-string (cdr (assoc :orthography record-in)))))
   record-in)
  (princ "please wait... ")
  ;;(terpri)
  (cle-store-record record-in)
  (cle-empty-psql-cache)
  (princ (format " record saved to lexdb %s. " (cle-dbname))))

(defun lexdb-lookup-aux2 (field-kw val-str)
  (let ((ids (cle-lookup field-kw val-str)))
    (l:princ-list ids)
    (if ids
	(setf *lexdb-active-id-ring* (make-ring ids)))))

;;;
;;; lexdb util fns
;;;

(defun l:make-empty-record (id)
  (let* ((record
	  (mapcar #'(lambda (x) (cons x (make-string 0 ?x))) 
		  *lexdb-record-features*))
	 (name-elt (assoc :name record)))
  (when name-elt
    (setf (cdr name-elt)
	  (l:val-str id)))
  record))

(defun l:minibuffer-complete-dyn (&rest rest)
  (interactive)
  (unless (and
	   (cle-eval "(find-package :lkb)")
	   (cle-eval-lexdb 'connection))
    (if (not (cle-eval "(common-lisp-user::featurep :lkb)"))
	(error "Please load the the LKB"))
    (if (not (cle-eval "(common-lisp-user::featurep :psql)"))
	(error "Running version of LKB is not :psql enabled"))
    (princ "Initializing LexDB: please wait... ")
    (cle-initialize-psql))
  (let* ((val (buffer-substring (1+ (length prompt)) 
				(1+ (length (buffer-string)))))
	 (minibuffer-completion-table
	  (mapcar #'list
		  (cle-complete :name val))))
    (apply 'minibuffer-complete rest)))

(defun l:completing-read-dyn (prompt)
  (make-variable-buffer-local 'prompt)
  (let* ((map (copy-keymap minibuffer-local-completion-map))
	 (minibuffer-local-completion-map 
	  (and
	   (define-key map "\t" 'l:minibuffer-complete-dyn)
	   map)))
    (completing-read prompt 
		     '(("dog_n1") ("dogsled_n1")))))

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
  (let ((trunc-l
	 (if (< *lexdb-minibuffer-max*
		(length l))
	     (append (truncate-list l *lexdb-minibuffer-max*)
		     (list "..."))
	   l)))
  (princ 
   (mapconcat #'cle-force-str 
	      trunc-l
	      " "))))

(defun l:fv-pair-2-fw-pair (x)
  (let ((feat (car x))
	(val (cdr x)))
  (cons 
   feat
   (progn 
     (widget-insert "\n"
		    (make-string (max 0 (- 15 (length (kw2str feat)))) ? ) 
		    (upcase (kw2str feat)) 
		    ": ")
     (cond
      ((member feat *lexdb-read-only*)
       (widget-insert val))
      (t
       (widget-create 'editable-field-fixed-size
		      :size (min 50 (l:field-size feat))
		      :keymap nil
		      :value-face nil
		      val)))))))

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
  (let ((fi:package "lkb"))
    (cle-eval 
     (format "(and (fboundp 'lexdb-fn) (lexdb-fn '%s %s))"
	     fn-name
	     (mapconcat #'cle-force-str fn-args " ")))))

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
  (cle-eval-lexdb 'retrieve-head-record-str (cle-lisp-str id)))

(defun cle-retrieve-record-sizes nil
   (cle-eval-lexdb 'get-field-size-map))

(defun cle-store-record (record-in)
  (cle-eval-lexdb 'set-lex-entry 
		  (format "(make-instance 'psql-lex-entry :fv-pairs '%S))" record-in)))

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

(defun cle-complete (field-kw val-str)
  (if (or (string= val-str "") (null val-str))
      (setf val-str (cle-lisp-str ""))
    (setf val-str (cle-lisp-str val-str)))
  (cle-eval-lexdb 'complete
		 field-kw
		 val-str))

(defun cle-new-entries nil
  (cle-eval-lexdb 'new-entries))