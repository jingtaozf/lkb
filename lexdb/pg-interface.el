;;; Copyright (c) 2003 - 2007 Ben Waldron
;;; see licence.txt for conditions
;; Portions copyright (c) 1996, 1997, 1999, 2000, 2001 Free Software Foundation, Inc.

;; Free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Add a PG menu to the emacs menu bar

(defvar *lexdb-pg-interface-version* "2.21")

(require 'cl)      ; we use some common-lisp idioms
(require 'widget)

(eval-when-compile
  (require 'wid-edit))

;; default widgets misbehave 
;; due to markers defining :from and :to points
(define-widget 'editable-field-fixed-size 'default
  "An editable text field."
  :convert-widget 'widget-value-convert-widget
  :keymap 'widget-field-keymap
  :format "%v"
  :help-echo "M-TAB: complete field; : C-c C-c commit record"
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

(defvar *lexdb-record-features*)
(defvar *lexdb-read-only-fields*)
(defvar *lexdb-hidden*)
(defvar *lexdb-minibuffer-max*)
(defvar *lexdb-active-ium-size*)
(defvar *lexdb-active-ium-ring*)
(defvar *lexdb-new-entries-buffer*)
(defvar *lexdb-scratch-buffer*)
(defvar *lexdb-slot-len*)
(defvar *completable-fields*)
(defvar *lexdb-grammar-fields*)
(defvar *lexdb-orth-fiel*)
(defvar *lexdb-grammar-fields-propertize*)
(defvar *completion-field*)
(defvar *lexdb-dbname*)

(setf *lexdb-read-only-fields* '(:|userid| :|modstamp|))
(setf *lexdb-hidden* nil)
(setf *lexdb-minibuffer-max* 80)
(setf *lexdb-active-ium-size* 0)
(setf *lexdb-active-ium-ring* nil)
(setf *lexdb-new-entries-buffer* "*lexdb-merged*")
(setf *lexdb-scratch-buffer* "*lexdb-scratch*")
(setf *lexdb-slot-len* 30)
(setf *completable-fields* '("_text"))
(setf *lexdb-grammar-fields* nil)
(setf *lexdb-orth-field* nil)
(setf *lexdb-grammar-fields-propertize* (list 'face 'change-log-file-face))
(setf *completion-field* nil)
(setf *lexdb-dbname* nil)

;;;
;;; buffer local vbles
;;;

(setq lexdb-fw-map nil)
(setq lexdb-fsize-map nil)
(setq lexdb-id nil)
(setq lexdb-record nil)
(setq lexdb-tdl nil)
;(setq lexdb-completion-field nil)

(make-variable-buffer-local 'lexdb-fw-map)
(make-variable-buffer-local 'lexdb-fsize-map)
(make-variable-buffer-local 'lexdb-id)
(make-variable-buffer-local 'lexdb-tdl)
;(make-variable-buffer-local 'lexdb-completion-field)

;;;
;;; connection to common lisp process
;;;

;; fi:eval-in-lisp misbehaves:
;; "'(:a)" -> (:a)
;; ":A" -> "A"
;; "'A" -> "A"
;; "1" -> 1
;; "\"A\"" -> "A"

;; unusual return values cause system to hang...
(defun cle-eval (str)
  (condition-case descr
      (car (fi:eval-in-lisp "(list (lkb::eval-for-cle %s))" str))
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
    
    ;; level 2
    (define-key map [menu-bar LexDB dump/load] (lexdb-make-name-keymap "Dump/Load LexDB"))

    (define-key map [menu-bar LexDB dump/load lexdb-dump]
      '(menu-item "Dump LexDB" lexdb-dump
		  ;:keys "M-dump"
		  :enable (cle-connection)))
    (define-key map [menu-bar LexDB dump/load lexdb-load]
      '(menu-item "Load LexDB entries" lexdb-load
		  ;:keys "M-load"
		  :enable (cle-connection)))
    (define-key map [menu-bar LexDB dump/load lexdb-dump-tdl]
      '(menu-item "Export LexDB (tdl file)" lexdb-dump-tdl
		  ;:keys "M-dump-tdl"
		  :enable (cle-connection)))

    (define-key map [menu-bar LexDB manage-scratch] (lexdb-make-name-keymap "Manage private rev"))
    (define-key map [menu-bar LexDB manage-scratch view-scratch]
      '(menu-item "View private rev" lexdb-view-private-rev
		  :keys "M-vpr"
		  :enable (cle-connection)))
    (define-key map [menu-bar LexDB manage-scratch commit-scratch]
      '(menu-item "Commit private rev" lexdb-commit-private-rev
		  :keys "M-cpr"
		  :enable (cle-connection)))
    (define-key map [menu-bar LexDB manage-scratch clear-scratch]
      '(menu-item "Clear private rev" lexdb-clear-private-rev
		  :keys "M-cpr"
		  :enable (cle-connection)))

    (define-key map [menu-bar LexDB select-record] (lexdb-make-name-keymap "Select record"))

    (define-key map [menu-bar LexDB select-record next-id]
      '(menu-item "Next id" lexdb-advance-ium 
		  :keys "M-n"
		  :enable (cle-connection)))
    (define-key map [menu-bar LexDB select-record lexdb-search-orth-val]
      '(menu-item "Search orth" lexdb-search-orth
		  :keys "C-c C-s"
		  :enable (cle-connection)))
    (define-key map [menu-bar LexDB select-record search]
      '(menu-item "Search" lexdb-search-field-val 
		  :keys "M-s"
		  :enable (cle-connection)))
    (define-key map [menu-bar LexDB select-record cross-ref-lex]
      '(menu-item "Cross reference(lex)" lexdb-lookup 
		  :keys "M-TAB l"
		  :enable (cle-connection)))
    (define-key map [menu-bar LexDB select-record cross-ref-rev]
      '(menu-item "Cross reference(rev)" lexdb-lookup-rev-all 
		  :keys "M-TAB r"
		  :enable (cle-connection)))
    (define-key map [menu-bar LexDB select-record edit]
      '(menu-item "Load record" lexdb-load-record 
		  :keys "C-l"
		  :enable (cle-connection)))
    ;;
    ;; level 1

    (define-key map [menu-bar LexDB break] (lexdb-make-name-keymap "---"))
    (define-key map [menu-bar LexDB normalize]
      '(menu-item "Normalize buffer" lexdb-normalize-buffer 
		  :keys "C-n"
		  :enable (cle-connection)))
    (define-key map [menu-bar LexDB delete]
      '(menu-item "Delete record" lexdb-delete-record 
		  :keys "C-c C-d"
		  :enable (cle-connection)))
    (define-key map [menu-bar LexDB rename-record]
      '(menu-item "Rename record" lexdb-rename-record 
		  :keys "C-c C-r"
		  :enable (cle-connection)))
    (define-key map [menu-bar LexDB commit]
      '(menu-item "Commit record" lexdb-commit-record 
		  :keys "C-c C-c"
		  :enable (cle-connection)))


))

(add-hook 'lexdb-mode-hook 
	  (function (lambda ()
		      (lexdb-mode-initialize-menu-bar))))
;;;
;;; major mode defn
;;;

(defun make-lexdb-keymap nil
  (let ((map (make-sparse-keymap)))
;; no need for keystrokes for these commands
;    (define-key map "\M-load" 'lexdb-load)
;    (define-key map "\M-dump" 'lexdb-dump)
;    (define-key map "\M-dump-tdl" 'lexdb-dump-tdl)
    (define-key map "\M-vpr" 'lexdb-view-private-rev)
    (define-key map "\M-cpr" 'lexdb-commit-private-rev)
    (define-key map "\M-dpr" 'lexdb-clear-private-rev)
    (define-key map "\M-va" 'lexdb-view-merge-add)
    (define-key map "\C-l" 'lexdb-load-record)
    (define-key map "\C-c\C-r" 'lexdb-rename-record)
    (define-key map "\C-c\C-d" 'lexdb-delete-record)
    (define-key map "\C-c\C-c" 'lexdb-commit-record)
    (define-key map "\C-n" 'lexdb-normalize-buffer)
    (define-key map "\M-\tl" 'lexdb-lookup)
    (define-key map "\M-\tr" 'lexdb-lookup-rev-all)
    (define-key map "\M-n" 'lexdb-advance-ium)
    (define-key map "\C-c\C-s" 'lexdb-search-orth)
    (define-key map "\M-s" 'lexdb-search-field-val)
    (define-key map "\t" 'lexdb-complete-field)
    (define-key map "\C-m" 'widget-advance)
    (setq lexdb-mode-map (append map 'widget-field-keymap))))

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
  (setq truncate-lines t)
  (run-hooks 'lexdb-mode-hook)
  (cle-check-pg-interface-version))  

;;;
;;; interactives
;;;

(defun lexdb (id)
  (interactive (list (l:completing-read-dyn :|name|)))
  (lexdb-load-record id))

(defun lexdb-load-record (id)
  (interactive (list (l:completing-read-dyn :|name|)))
  (unless (cle-connection)
    (error "no connection to LexDb"))
  (lexdb-load-record-aux id)
  ;;(beep)
  )

(defun lexdb-normalize-buffer (buffer)
  (interactive (list (format "%s" (current-buffer))))
  (unless (cle-connection)
    (error "no connection to LexDb"))
  (lexdb-normalize-buffer-aux buffer)
  ;;(beep)
  )

(defun lexdb-rename-record (buffer)
  (interactive (list (format "%s" (current-buffer))))
  (unless (cle-connection)
    (error "no connection to LexDb"))
  (if (lexdb-rename-record-aux buffer)
      (beep)))

(defun lexdb-delete-record (buffer)
  (interactive (list (format "%s" (current-buffer))))
  (unless (cle-connection)
    (error "no connection to LexDb"))
  (if (lexdb-delete-record-aux buffer)
      (beep)))

(defun lexdb-commit-record (buffer)
  (interactive (list (format "%s" (current-buffer))))
  (unless (cle-connection)
    (error "no connection to LexDb"))
  (if (lexdb-commit-record-aux buffer)
      (beep)))

(defun lexdb-complete-field nil
  (interactive)
  (lexdb-complete-field-aux))

(defun lexdb-lookup ()
  (interactive)
  (lexdb-lookup-aux "lex"))

(defun lexdb-lookup-rev-all ()
  (interactive)
  (lexdb-lookup-aux "rev_all"))

(defun lexdb-advance-ium ()
  (interactive)
  (lexdb-advance-ium-aux))

(defun lexdb-view-private-rev()
  (interactive)
  (lexdb-view-private-rev-aux))

(defun lexdb-commit-private-rev()
  (interactive)
  (lexdb-commit-private-rev-aux))

(defun lexdb-clear-private-rev()
  (interactive)
  (lexdb-clear-private-rev-aux))

(defun lexdb-load (filename)
  (interactive (list (read-file-name "Load from .rev file: ")))
  (princ (format "Loading new entries into LexDB %s from file %s" (l:dbname) filename))
  (terpri)
  (cle-merge-lexdb filename)
  (terpri)
  (princ "DONE. See Lisp buffer for details.")
  (beep))

(defun lexdb-dump (filename)
  (interactive (list (read-file-name "Dump to .rev file: ")))
  (princ (format "Dumping LexDB %s to file %s" (l:dbname) filename))
  (terpri)
  (cle-dump-lexdb filename)
  (terpri)
  (princ "DONE. See Lisp buffer for details.")
  (beep))

(defun lexdb-dump-tdl (filename)
  (interactive (list (read-file-name "Dump to TDL file: ")))
  (princ (format "Dumping LexDB %s to TDL file %s" (l:dbname) filename))
  (terpri)
  (cle-dump-tdl-lexdb filename)
  (terpri)
  (princ "DONE. See Lisp buffer for details.")
  (beep))

(defun lexdb-view-merge-add ()
  (interactive)
  (lexdb-view-merge-add-aux))

(defun lexdb-search-orth (val-str)
  (interactive (list (l:completing-read-dyn (l:orth-field))))
  (lexdb-search-field-val-aux val-str (l:orth-field)))

(defun lexdb-search-field-val (val-str)
  (interactive (list (l:completing-read-dyn (l:widget-to-field-kw (widget-field-find (point))))))
  (let ((field-kw
	 (l:widget-to-field-kw (widget-field-find (point)))))
    (lexdb-search-field-val-aux val-str field-kw)))

;; advance cursor to start of next widget
;; or else to start of first widget
(defun widget-advance nil
  (interactive)
  (let* ((widget 
	  (widget-field-find (point)))
	 (widgets (mapcar #'cdr lexdb-fw-map))
	 (widget (second (member widget widgets))))
    (unless widget (setf widget (first widgets)))
    (if widget
	(set-window-point (selected-window) 
			  (widget-get widget :from)))))

;;;
;;; main functions
;;;

(defun lexdb-search-field-val-aux (val-str field-kw)
  (if field-kw
      (lexdb-lookup-aux2 field-kw
			 (l:normalize val-str)
			 "lex")))

(defun lexdb-advance-ium-aux nil
  (when *lexdb-active-ium-ring*
    (lexdb-load-record3-aux (car *lexdb-active-ium-ring*))
    (setf *lexdb-active-ium-ring*
	  (cdr *lexdb-active-ium-ring*))
    (l:princ-ring *lexdb-active-ium-ring* *lexdb-active-ium-size* #'car)
    t))

(defun lexdb-collect-field-lines (records fields)
  (let ((pos -1))
    (mapcar '(lambda (x)
	       (setf pos (1+ pos))
	       (lexdb-collect-field-line records x pos))
	    fields)))

(defun lexdb-string-slot (str len)
  (cond 
   ((> (length str) (- len 3))
    (format "%s.. " (substring str 0 (- len 4))))
   (t
    (format "%s%s" str (make-string (max 0 (- len (length str))) ? )))))

(defun lexdb-collect-field-line (records field pos)
  (format "%s%s" 
	  (lexdb-string-slot (symbol-name field) 20)
	  (mapconcat '(lambda (x) 
			(let ((val (nth pos x)))
			  (lexdb-string-slot val *lexdb-slot-len*) 
			  )) 
		     records
		     "")))

(defun lexdb-view-private-rev-aux nil
  (let* ((buffer *lexdb-scratch-buffer*)
	 (priv-recs (cle-get-private-revs))
	 (len (length priv-recs)))
    (case len
      (0
       (error "0 entries in private rev!")
       (beep))
      (t
       (if (get-buffer buffer)
	   (kill-buffer buffer))
       (with-current-buffer (get-buffer-create buffer)
	 (lexdb-mode)
	 (insert (mapconcat 'identity
			    (lexdb-collect-field-lines priv-recs *lexdb-record-features*)
			    "\n")))
       
       (switch-to-buffer buffer)))))

(defun lexdb-commit-private-rev-aux nil
  (let* ((scratch (cle-get-private-revs))
	(len (length scratch)))
    (case len 
     (0
      (error "0 entries in private rev!")
      (beep))
     (t
      (when (y-or-n-p (format
		       "Confirm commit privat rev (%s entries): "
		       (length scratch)))
	(cle-commit-private-rev)
	(if (get-buffer *lexdb-scratch-buffer*)
	    (kill-buffer *lexdb-scratch-buffer*))
	(beep))))))

(defun lexdb-clear-private-rev-aux nil
  (let* ((scratch (cle-get-private-revs))
	(len (length scratch)))
    (case len 
     (0
      (error "0 entries in private rev!")
      (beep))
     (t
      (when (y-or-n-p (format
		       "Confirm clear privat rev (%s entries): "
		       (length scratch)))
	(cle-clear-private-rev)
	(if (get-buffer *lexdb-scratch-buffer*)
	    (kill-buffer *lexdb-scratch-buffer*))
	(beep))))))

(defun lexdb-view-merge-add-aux nil
  (let ((buffer *lexdb-new-entries-buffer*))
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
		    (nth 2 x)))
	(cdr 
	 (cle-new-entries)))))
    (switch-to-buffer buffer)))

(defun lexdb-rename-record-aux (buffer)
  (lexdb-normalize-buffer buffer)
  (when (y-or-n-p "Confirm rename record: ")
    (let* ((fv-rec (car lexdb-record))
	  (name-fv (assoc :|name| fv-rec))
	  (new-name (l:completing-read-dyn :|name|)))
      (cle-delete-record fv-rec)
      (setf (cdr name-fv) new-name)
      (lexdb-store-record fv-rec)
      (lexdb-load-record-aux new-name)))
  (with-current-buffer buffer
    t))

(defun lexdb-delete-record-aux (buffer)
  (when (y-or-n-p "Confirm delete record: ")
    (cle-delete-record (car lexdb-record))
    (lexdb-load-record-aux (cdr (assoc :|name| (car lexdb-record)))))
  (with-current-buffer buffer
    t))

(defun lexdb-commit-record-aux (buffer)
  (lexdb-normalize-buffer buffer)
  (when (y-or-n-p "Confirm commit record: ")
    (lexdb-store-record (car lexdb-record))
    (lexdb-load-record-aux (cdr (assoc :|name| (car lexdb-record)))))
  (with-current-buffer buffer
    t))

(defun lexdb-complete-field-aux nil
  (let* ((widget (widget-field-find (point))))
    (if widget
	(lexdb-complete-field-aux2
	 (l:widget-to-field-kw widget))
      (error "not in an editable field"))))

(defun lexdb-complete-field-aux2 (field-kw)
  (unless (member (l:field-type field-kw) *completable-fields*)
    (error "field %s has non-completable type %s" field-kw (l:field-type field-kw)))
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

(defun lexdb-lookup-aux (from)
  (let* ((widget (widget-field-find (point))))
    (if widget
	(lexdb-lookup-aux2 (l:widget-to-field-kw widget)
			   (l:widget-val-normd widget)
			   from)
      (error "not in an editable field"))))

(defun lexdb-normalize-buffer-aux (buffer)
  (lexdb-update-record-from-buffer buffer)
  (let ((record lexdb-record)
	(tdl lexdb-tdl)
	(pos (point)))
    (kill-buffer buffer)
    (with-current-buffer (get-buffer-create buffer)
      (lexdb-mode)
      (setf lexdb-record record)
      (setf lexdb-tdl (or (cle-record-to-tdl (car record)) ""))
      (lexdb-display-record buffer)
      (goto-char pos))))

(defun lexdb-load-record-aux (id)
  (let* ((record (lexdb-retrieve-record id))
	 (name (cdr (assoc :|name| (car record))))
	 (tdl lexdb-tdl))
    (setf buffer (or name "?unknown record?"))
    (if (get-buffer buffer)
	(kill-buffer buffer))
    (with-current-buffer (get-buffer-create buffer)
      (lexdb-mode)
      (setf lexdb-record record)
      (setf lexdb-tdl tdl)
      (lexdb-display-record buffer))))

(defun lexdb-load-record3-aux (ium)
  (let ((record (lexdb-retrieve-record3 ium))
	(id (car ium))
	(tdl lexdb-tdl))
    (if (equal id "") 
	(setf id "?new?"))
    (setf buffer (format "%s" id))
    (if (get-buffer buffer)
	(kill-buffer buffer))
    (with-current-buffer (get-buffer-create buffer)
      (lexdb-mode)
      (setf lexdb-record record)
      (setf lexdb-tdl tdl)
      (lexdb-display-record buffer))))

(defun l:record-name nil
  (cdr (assoc :|name| (car lexdb-record))))

(defun lexdb-display-record (buffer)
  (with-current-buffer buffer
    (switch-to-buffer buffer)
    (unless lexdb-record
      (error "buffer has no associated record"))
    (setf lexdb-fsize-map (cdr lexdb-record))
    (setf lexdb-id (format "%s" buffer))
    (let* ((inhibit-read-only t))
      (erase-buffer))
    (let* ((source (if (cle-lookup :|name| (l:record-name) "lex")
		       ".lex"
		     ""))
	   (source-str (format "%s%s" (l:dbname) source)))
      (widget-insert (format (format "LexDB Entry (%s):\n" source-str))))
    (setf lexdb-fw-map 
	  (remove-if-not #'cdr
			 (mapcar #'l:fv-pair-2-fw-pair 
				 (l:prepare-record (car lexdb-record)))))
    (widget-insert "\n"
		   lexdb-tdl)
    (widget-setup)
    (goto-char 0)
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
	  (error "feature %s not found in record" feat))
      (setf (cdr record-elt) val))))


;; sets lexdb-tdl and lexdb-record
(defun lexdb-retrieve-record (id)
  (let ((fields (cle-retrieve-record-fields id))
	(sizes (cle-retrieve-record-sizes)))
    (setf *lexdb-record-features* (l:order-record-fields))
    (unless fields
      (princ (format "%s not found! " id))
      (setf fields (l:make-empty-record id)))
    (setf lexdb-tdl (or (cle-retrieve-tdl (cdr (assoc :|name| fields)))
			""))
    (setf lexdb-record (cons fields sizes))))

(defun l:order-record-fields ()
  (let ((fields (cle-retrieve-record-features))
	(ofields (list :|name|)))
    (setf ofields 
	  (append ofields
		  (set-difference (l:grammar-fields) ofields)))
    (setf ofields 
	  (append ofields
		  (set-difference fields ofields)))
    (setf ofields (reverse (set-difference ofields *lexdb-read-only-fields*)))
    (setf ofields (append ofields *lexdb-read-only-fields*))
    (set-difference ofields *lexdb-hidden*)))

(defun lexdb-retrieve-record3 (ium)
  (let ((fields (cle-retrieve-record-fields3 ium))
	(id (car ium))
	(sizes (cle-retrieve-record-sizes)))
    (setf *lexdb-record-features* (l:order-record-fields))
    (unless fields
      (princ (format "%s not found! " ium))
      (setf fields (l:make-empty-record id)))
    (setf lexdb-tdl (or (cle-retrieve-tdl (cdr (assoc :|name| fields)))
			""))
    (setf lexdb-record (cons fields sizes))))

(defun lexdb-store-record (record-in)
  (if (equal (cdr (assoc :|name| record-in)) "")
      (error "cannot commit record with no NAME field"))
  (if (equal (cdr (assoc :|dead| record-in)) "")
      (error "cannot commit record with no DEAD field"))
  (princ "please wait... ")
  ;;(terpri)
  (cle-store-record record-in)
;  (cle-empty-psql-cache)
  (princ (format " rev saved to LexDB %s " (cle-dbname))))

(defun lexdb-lookup-aux2 (field-kw val-str from)
  (let* ((iums (cle-lookup field-kw val-str "name,userid,modstamp" from)))
    (setf *lexdb-active-ium-size* (length iums))
    (setf *lexdb-active-ium-ring* (make-bmwring iums))
    (lexdb-advance-ium-aux)
    ))

;;;
;;; lexdb util fns
;;;

(defun l:make-empty-record (id)
  (let* ((record
	  (mapcar #'(lambda (x) (cons x (make-string 0 ?x)))
		  *lexdb-record-features*))
	 (name-elt (assoc :|name| record)))
    (when name-elt
      (setf (cdr name-elt)
	    (l:val-str id)))
    record))

(defun l:minibuffer-complete-dyn (&rest rest)
  (interactive)
  (unless (and
	   (cle-eval "(find-package :lkb)")
	   (cle-connection))
    (if (not (cle-eval "(common-lisp-user::featurep :lkb)"))
	(error "Pease load the the LKB"))
    (if (not (cle-eval "(common-lisp-user::featurep :psql)"))
	(error "Running version of LKB is not :psql enabled"))
    (princ "Initializing LexDB: please wait... ")
    (cle-initialize-psql))
  (if (cle-connection)
      (let* ((val (buffer-substring-no-properties (1+ (length prompt)) 
				    (1+ (length (buffer-string)))))
	     (minibuffer-completion-table
	      (mapcar #'list
		      (cle-complete *completion-field* val))))
	(apply 'minibuffer-complete rest))))

(defun l:completing-read-dyn (field-kw)
  (unless field-kw (error "invalid field: %s" field-kw))
  ;(make-variable-buffer-local 'field-kw) ;???
  (let* ((prompt (format "Enter %s: " (kw2str field-kw)))
	 (map (copy-keymap minibuffer-local-completion-map))
	 (minibuffer-local-completion-map 
	  (and
	   (setf *completion-field* field-kw)
	   (define-key map "\t" 'l:minibuffer-complete-dyn)
	   map)))
    (completing-read prompt      
		     '(("DUMMY")))))

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

(defun l:field-kw-to-widget (field-kw)
  (cdr (assoc field-kw lexdb-fw-map)))

(defun l:len (l)
  (cond
   ((null l) 0)
   (t (1+ (l:len-aux (cdr l) (car l))))))

(defun l:len-aux (l x)
  (let ((c 0))
    (while
	(not (or (null l)
		 (eq x (car l))))
      (incf c)
      (setf l (cdr l)))
    c))

(defun l:princ-ring (l &optional limit key)
  (l:princ-list l limit key))

(defun l:princ-list (l &optional limit key)
  (unless limit (setf limit *lexdb-minibuffer-max*))
  (unless key (setf key #'identity))
  (let* ((len (l:len l))
	 (trunc-l
	  (if (< limit len)
	      (append (truncate-list l len) (list "..."))
	    (truncate-list l len))))
    (princ "[")
    (princ len)
    (if (= len 1) (princ " item] ") (princ " items] "))
    (princ 
     (mapconcat #'(lambda (x) (cle-force-str (funcall key x))) 
		trunc-l
		" "))))

(defun l:dbname nil
 (or *lexdb-dbname*
     (setf *lexdb-dbname* (cle-dbname))))

(defun l:orth-field nil
 (or *lexdb-orth-field*
     (setf *lexdb-orth-field* (cle-orth-field))))

(defun l:grammar-fields nil
  (or *lexdb-grammar-fields*
      (setf *lexdb-grammar-fields* (cle-grammar-fields))))

(defun l:fv-pair-2-fw-pair (x)
  (let* ((feat (car x))
	 (feat-str (kw2str feat))
	 (val (cdr x)))
    (if (member feat (l:grammar-fields))
	(setf feat-str (apply 'propertize (cons feat-str *lexdb-grammar-fields-propertize*))))
    (cons 
     feat
     (progn 
       (widget-insert "\n"
		      (make-string (max 0 (- 15 (length feat-str))) ? ) 
		      feat-str
		      ": ")
       (cond
	((member feat *lexdb-read-only-fields*)
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

(defun l:field-type (kw)
  (second (assoc kw lexdb-fsize-map)))

;;;
;;; util fns
;;;

(defun kw2str (kw)
  (field-display-str kw))

(defun field-display-str (field-kw)
  (let ((str (symbol-name field-kw)))
    (cond
     ((string= ":|" (substring str 0 2))
      (substring str 2 -1))
     (t
      (substring str 1)))))

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

(defun make-bmwring (l)
  (and 
   (listp l)
   (setf (cdr (last l)) l)))

;(defun make-bmwring (l)
;  (make-bmwring-1 l))
;
;(defun make-bmwring-1 (l)
;  (and 
;   (make-bmwring-2 l)
;   (make-bmwring-3 l)))
;
;(defun make-bmwring-2 (l)
;  (listp l))
;
;(defun make-bmwring-3 (l)
;  (setf (cdr (last l)) l))


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
  (if (stringp x) x (format "%S" x)))

(defun cle-lisp-str (id)
  (format "\"%s\"" 
	  (mapconcat #'(lambda (x) 
			 (if (string= x "\"")
			     "\\\""
			   x))
		     (split-string (format "%s" id) "")
		     "")))

(defun cle-lisp-symb (id)
  (format "'%s" id))

(defun cle-lisp-list (list)
  (format "'%S" list))

(defun cle-retrieve-record-fields (id)
  (cle-eval-lexdb 'get-dot-lex-record (cle-lisp-str id)))

(defun cle-retrieve-record-fields3 (ium)
  (cle-eval-lexdb 'get-dot-rev-record
		  (cle-lisp-str (first ium))
		  (cle-lisp-str (second ium))
		  (cle-lisp-str (third ium))
		  ))

(defun cle-retrieve-tdl (id)
  (cle-eval-lexdb 'id-to-tdl-str (cle-lisp-str id)))

(defun cle-retrieve-record-sizes nil
  (cle-eval-lexdb 'get-field-size-map))

(defun cle-retrieve-record-features nil
  (cle-eval-lexdb 'fields))

(defun cle-delete-record (record-in)
  (cle-eval-lexdb 'delete-record (cle-lisp-list record-in)))

(defun cle-store-record (record-in)
  (cle-eval-lexdb 'set-lex-entry-from-record (cle-lisp-list record-in)))

(defun cle-record-to-tdl (record-in)
  (cle-eval-lexdb 'record-to-tdl (cle-lisp-list record-in)))

(defun cle-dbname nil
  (cle-eval-lexdb 'dbname))

(defun cle-lookup (field-kw val-str &optional ret-flds from)
  (unless ret-flds (setf ret-flds "*"))
  (unless from (setf from "lex"))
  (if (or (string= val-str "") (null val-str))
      (setf val-str nil)
    (setf val-str (cle-lisp-str val-str)))
  (setf ret-flds (cle-lisp-str ret-flds))
  (setf from (cle-lisp-str from))
  (cle-eval-lexdb 'lookup field-kw val-str :ret-flds ret-flds :from from))

(defun cle-lookup-rev-all (field-kw val-str)
  (if (or (string= val-str "") (null val-str))
      (setf val-str nil)
    (setf val-str (cle-lisp-str val-str)))
  (cle-eval-lexdb 'lookup-rev-all field-kw val-str))

(defun cle-get-private-revs ()
  (cle-eval-lexdb 'scratch-records))

(defun cle-commit-private-rev ()
  (cle-eval-lexdb 'commit-private-rev))

(defun cle-clear-private-rev ()
  (cle-eval-lexdb 'clear-private-rev))

(defun cle-complete (field-kw val-str)
  (if (or (string= val-str "") (null val-str))
      (setf val-str (cle-lisp-str ""))
    (setf val-str (cle-lisp-str val-str)))
  (cle-eval-lexdb 'complete field-kw val-str))

(defun cle-new-entries nil
  (cle-eval-lexdb 'new-entries))

(defun cle-connection nil
  (cle-eval-lexdb 'connection))

(defun cle-check-pg-interface-version nil
  (cle-eval-lexdb 'check-pg-interface-version *lexdb-pg-interface-version*))

(defun cle-initialize-psql nil
  (cle-eval "(initialize-lexdb)"))

(defun cle-grammar-fields nil
  (cle-eval-lexdb 'grammar-fields))

(defun cle-orth-field nil
 (cle-eval-lexdb 'orth-field-kw))

(defun cle-dump-lexdb (filename)
  (cle-eval-lexdb 'dump-lexdb2 (cle-lisp-str filename)))

(defun cle-dump-tdl-lexdb (filename)
  (cle-eval-lexdb 'dump-tdl-lexdb2 (cle-lisp-str filename)))

(defun cle-merge-lexdb (filename)
  (cle-eval-lexdb 'merge-lexdb2 (cle-lisp-str filename)))