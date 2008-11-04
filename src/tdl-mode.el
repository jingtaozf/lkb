;;; TDL-Mode --- Emacs commands for editing Type Definition Language files

;; Author: Ulrich Schaefer DFKI/DISCO; based on slant-mode.el by Jochen Bedersdorfer
;; Extended by Rob Malouf, Frederik Fouvry and Francis Bond
;; Maintainer: Francis Bond <bond@ieee.org>
;; Keywords: languages tdl modes

;; This code is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; tdl-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;;Commentary:

;; To enter tdl-mode automatically put it in your path, load it and
;; add something like:
;; (add-to-list 'auto-mode-alist '("\\.tdl\\'" . tdl-mode))
;; (add-to-list 'auto-mode-alist '("\\.set\\'" . tdl-mode))
;;
;; to add a menu of definitions try the following - 
;;
;; (add-hook 'tdl-mode-hook
;;          (function (lambda ()
;;                     (imenu-add-to-menubar "Index"))))

(eval-when-compile
  '(require 'font-lock))

(defvar tdl-mode nil)

(defvar tdl-mode-abbrev-table nil
  "Abbrev table in use in TDL-mode buffers.")
(define-abbrev-table 'tdl-mode-abbrev-table ())

(defvar tdl-mode-map ()
  "Keymap used in TDL mode.")
(if nil ;;tdl-mode-map
    ()
  (setq tdl-mode-map (make-sparse-keymap))
  (define-key tdl-mode-map "[" 'tdl-bracket-begin)
  (define-key tdl-mode-map "]" 'tdl-bracket-end)
  (define-key tdl-mode-map "\e\C-x" 'eval-tdl-expression)
  (define-key tdl-mode-map "\C-c\C-r" 'eval-tdl-region)
  (define-key tdl-mode-map "\C-cr" 'eval-tdl-region-and-go)
  (define-key tdl-mode-map "\C-c\C-s" 'eval-current-tdl-expression)
  (define-key tdl-mode-map "\C-c\C-e" 'goto-end-of-tdl-expression)
  (define-key tdl-mode-map "\C-c\C-a" 'goto-begin-of-tdl-expression)
  (define-key tdl-mode-map "\C-c\C-b" 'eval-tdl-file)
  (define-key tdl-mode-map "\177" 'backward-delete-char-untabify)
  (define-key tdl-mode-map "\C-c;" 'comment-region)
  (define-key tdl-mode-map "\e\034" 'tdl-indent-region)
  (define-key tdl-mode-map "\t" 'tdl-indent-command))
;  (define-key tdl-mode-map [double-down-mouse-1] 'tdl-show-type))

(defvar tdl-mode-syntax-table nil
  "Syntax table in use in TDL-mode buffers.")

(if tdl-mode-syntax-table
    ()
  (setq tdl-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "\\" tdl-mode-syntax-table)
  (modify-syntax-entry ?/ "_" tdl-mode-syntax-table)
  (modify-syntax-entry ?# "'" tdl-mode-syntax-table)
  (modify-syntax-entry ?~ "'" tdl-mode-syntax-table)
  (modify-syntax-entry ?@ "_" tdl-mode-syntax-table)
  (modify-syntax-entry ?_ "_" tdl-mode-syntax-table)
  (modify-syntax-entry ?+ "_" tdl-mode-syntax-table)
  (modify-syntax-entry ?- "_" tdl-mode-syntax-table)
  (modify-syntax-entry ?? "_" tdl-mode-syntax-table)
  (modify-syntax-entry ?% "'" tdl-mode-syntax-table)
  (modify-syntax-entry ?= "." tdl-mode-syntax-table)
  (modify-syntax-entry ?\( "()" tdl-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" tdl-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]" tdl-mode-syntax-table)
  (modify-syntax-entry ?< "(>" tdl-mode-syntax-table)
  (modify-syntax-entry ?> ")<" tdl-mode-syntax-table)
  (modify-syntax-entry ?& "." tdl-mode-syntax-table)
  (modify-syntax-entry ?| "." tdl-mode-syntax-table)
  (modify-syntax-entry ?, "." tdl-mode-syntax-table)
  (modify-syntax-entry ?. "." tdl-mode-syntax-table)
  (modify-syntax-entry ?\' "'" tdl-mode-syntax-table)
  (modify-syntax-entry ?\n ">   " tdl-mode-syntax-table)
  (modify-syntax-entry ?\f ">   " tdl-mode-syntax-table)
  (modify-syntax-entry ?\; "<   " tdl-mode-syntax-table)
)

(defconst tdl-eval-expression-string "(tdl::eval-tdl-string \"%s\")"
  "* Formatstring, containing tdl-function for evaluating tdl expression *")

(defconst tdl-eval-file-string "(tdl::include \"%s\")"
  "* Formatstring, containing tdl-function for evaluating tdl file *")

(defun tdl-mode ()
  "Major mode for editing TDL files.
\\{tdl-mode-map}

TDL mode supports:
- matching parentheses (,),[,],{,},<,>
- indentation (TAB key)
- connection to TDL/Common Lisp
- syntactic coloring (font-lock)
- comment/uncomment with comment-region
- double clicking on a type shows its hierarchy (via the lkb)

Known bugs: TDL mode may be confused by strange comment lines and strings.

Turning on TDL mode calls the value of the variable tdl-mode-hook 
with no args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map tdl-mode-map)
  (setq major-mode 'tdl-mode)
  (setq mode-name "TDL")
  (setq local-abbrev-table tdl-mode-abbrev-table)
  (set-syntax-table tdl-mode-syntax-table)
  (make-local-variable 'comment-start)
  (setq comment-start ";")
  (make-local-variable 'comment-style)
  (setq comment-style "plain")
  (make-local-variable 'basic-indent)
  (setq basic-indent 0)
  (make-local-variable 'bracks-left)
  (setq bracks-left 0)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'font-lock-keywords)
  (setq font-lock-defaults
	(list
	 '(":end" ":begin" ":instance" ":type" ":template" ":status" ":include")
	  nil t '(("+-*_!%~#". "w") ("<" . "(") (">" . ")")
		 (".&," . ".") (";" . "<") ("\n" . ">"))
	  nil))

  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression
	'((nil
	   "^:begin\\s-*:instance\\s-*:status\\s-*:\\([a-zA-Z0-9_-]+\\)\\s-*\\." 1)
	  ("Definitions" "^\\([a-zA-Z0-9_&!*+-]+\\)\\s-*:[=<+]" 1)))
  (run-hooks 'tdl-mode-hook))

(if (>= emacs-major-version 20)
      ;;; add keywords (only for modern emacs)
    (progn
      (font-lock-add-keywords
       'tdl-mode
       ;; real keywords are already in font-lock-defaults
       '(("\\*[a-zA-Z0-9---_+%]+\\*" . font-lock-type-face)             ; general types
	 ("\\(@[a-zA-Z0-9---_]+\\)[ \t\n]*(" (1 font-lock-keyword-face)) ; template calls
	 ("\\b[$][a-zA-Z0-9---_]+\\b" . font-lock-variable-name-face) ; template variables
	 ("[#][a-zA-Z0-9---_]+" . font-lock-variable-name-face)         ; re-entrancies
	 ("\\(\\[\\|,\\)[ \t\n]*\\([a-zA-Z0-9---_.]+\\)\\b" (2 font-lock-builtin-face)) ; features
	 ("^[ \t\n]*\\([a-zA-Z0-9---_]+\\)[ \t\n]*\\((\\|:[=<+]\\)" 
	  (1  font-lock-function-name-face))	; template names and types being defined
	 ("'[a-zA-Z0-9---_]+" . font-lock-string-face) ; single quoted predicates
	 ("\\(:[=<+]\\|&\\)"    . font-lock-constant-face) ; background syntax
	 ("#|\\(|[^#]\\|[^|]\\)*|#" . (0 font-lock-comment-face t)))) ; comments
;; If this is on font-lock-global-modes, then tdl-mode has been loaded ...
     (eval-after-load "font-lock"
       	'(if (boundp 'font-lock-global-modes)
	     (if (listp  font-lock-global-modes)
		 (add-to-list 'font-lock-global-modes 'tdl-mode))))))

(defun tdl-compute-ubound ()
  (beginning-of-line)
  (if (re-search-backward "[^\\.]\\.[ \t]*\\(;.*\\)*\n" nil t)
      (point)
    (point-min)))


(defun tdl-indent-command ()
  "Indent current line as TDL code"
  (interactive)
  (let ((indent-point (point))
        (column (current-column))
        (indent-me 0)
        (ubound (tdl-compute-ubound))
        (brack 1))
    (setq basic-indent 0) ;; (calculate-tdl-indent)
    (goto-char indent-point)
    (beginning-of-line)
    (skip-chars-forward " \t")
    (if (eq (following-char) ?\;)
        (goto-char indent-point)
      (progn
        (beginning-of-line)
        (catch 'out
          (while (and (not (bobp)) 
                      (not (= brack 0))
                      (re-search-backward "[][<>(){}=+]" ubound t))
            (if (and (eq (preceding-char) ?:)
                     (or (eq (following-char) ?=)
                         (eq (following-char) ?<)
			 (eq (following-char) ?+)))
                (throw 'out (setq brack 1))
              (if (memq (following-char) '(?\( ?{ ?< ?\[ )) ;; Closing backwards
                  (setq brack (1- brack))
                (if (memq (following-char) '(?\) ?} ?> ?\] )) ;; Opening backwards
                    (setq brack (1+ brack)))))))
        (setq indent-me 
              (if (and (= brack 1)
                       (eq (preceding-char) ?:)
                       (or (eq (following-char) ?=)
                           (eq (following-char) ?<)
			   (eq (following-char) ?+))
                       (not (tdl-one-line-expr-p indent-point)))
                  ;;;(if (tdl-empty-definition-line-p indent-point)
                  ;;;    (progn (beginning-of-line)
                  ;;;           (skip-chars-forward " \t")
                  ;;;           (forward-char tdl-indent-level)
                  ;;;           (current-column)))
                  (progn (forward-char 1)
                         (skip-chars-forward " \t")
                         (current-column))
                (if (or (bobp) (not (= brack 0)))
                    0
                  (progn (forward-char 1)
                         (when (and (eq (preceding-char) ?\[)
                                    (not (tdl-comma-at-end-p indent-point)))
                               (progn (skip-chars-forward " \t")
                                      (re-search-forward "[ \t]" nil t)))
                         (skip-chars-forward " \t")
                         (current-column))))) ;; + tdl-brace-offset
        (goto-char indent-point)
        (beginning-of-line)
        (setq brack (point))
        (skip-chars-forward " \t")
        (delete-region brack (point))
        (indent-to indent-me) ;; basic-indent
        (if (> column indent-me) (goto-char indent-point))))))


(defun tdl-indent-region (begin end &optional printflag)
  "indent a region of tdl expressions"
  (interactive "r\nP")
  (goto-char end)
  (setq end (point-marker))
  (goto-char begin)
  (while (< (point) end)
    (tdl-indent-command)
    (forward-line 1)
    (beginning-of-line))
  (end-of-line))


(defun tdl-comma-at-end-p (indent-point)
  (let* ((pos (point))
         (eol (progn (goto-char indent-point) 
                     (beginning-of-line)
                     (backward-char 1)
                     (point))))
    (beginning-of-line)
    (prog1
        (re-search-forward ",[ \t]*\\(;.*\\)*" eol t)
      (goto-char pos))))


(defun tdl-one-line-expr-p (indent-point)
  (let* ((pos (point))
         (eol (progn (goto-char indent-point) 
                     (beginning-of-line)
                     (backward-char 1)
                     (point))))
    (beginning-of-line)
    (prog1
      (re-search-forward "[^\\.]\\.[ \t]*\\(;.*\\)*" eol t)
      (goto-char pos))))


;;;(defun tdl-empty-definition-line-p (indent-point)
;;;  (let* ((pos (point))
;;;         (eol (progn (goto-char indent-point) 
;;;                     (beginning-of-line)
;;;                     (backward-char 1)
;;;                     (point))))
;;;    (goto-char pos)
;;;    (forward-char 1)
;;;    (skip-chars-forward " \t" eol)
;;;    (prog1
;;;        (or (= (point) eol)
;;;            (re-search-forward ";.*" eol t))
;;;      (goto-char pos))))        
    

;;;(defun calculate-tdl-indent ()
;;;  "Determines whether there is a basic indent or not"
;;;  (let* ((beg (progn (beginning-of-line) (point)))
;;;         (end (progn (end-of-line) (point)))
;;;         (s-string (buffer-substring beg end)))
;;;    (if (and (or (string-match ".*:=.*" s-string)
;;;                 (string-match ".*:<.*" s-string))
;;;             (not (or (string-match ".*:=.*\\.[ \t]*;*.*" s-string)
;;;                      (string-match ".*:<.*\\.[ \t]*;*.*" s-string))))
;;;        (progn
;;;          (beginning-of-line)
;;;          (search-forward ":" end t)
;;;          (forward-char 2)
;;;          (skip-chars-forward " \t")
;;;          (current-column))
;;;      0)))


(defun tdl-bracket-begin (arg)
  "Begin a bracket (not used yet)"
  (interactive "P")
  (setq bracks-left (1+ bracks-left))
  (self-insert-command (prefix-numeric-value arg)))


(defun tdl-bracket-end (arg)
  "End a bracket (not used yet)"
  (interactive "P")
  (setq bracks-left (1- bracks-left))
  (self-insert-command (prefix-numeric-value arg)))


(defun eval-tdl-file (arg)
  "Saves current buffer and tries to eval it with tdl-parser"
  (interactive "P")
  (if (and (buffer-modified-p)
           (y-or-n-p "Buffer modified! Saving before evaluating ? "))
      (save-buffer))
  (let ((name (buffer-file-name)))
    (if (not (save-excursion (get-buffer "*TDL-Listener*")))
        (fi:open-lisp-listener 0 "Tdl-Listener"))
    (save-excursion
      (save-window-excursion
        (set-buffer "*TDL-Listener*")
        (insert ?\n)
        (insert (format tdl-eval-file-string name))
        (message "TDL parsing ...")
        (fi:lisp-eval-last-sexp)
        (message "TDL parsing ...done.")))
    (pop-to-buffer (current-buffer))))


(defun eval-tdl-expression (arg)
  "Sends expression after or expression with point in it to lisp process"
  (interactive "P")
  (let ((beg 0)
        (end 0)
        (pos (point)))
    (setq beg (calc-begin-of-tdl-expression))
    (goto-char pos)
    (setq end (calc-end-of-tdl-expression))
    (tdl-send-region beg (min (1+ end) (point-max)))
    (goto-char pos)))


(defun tdl-send-region (begin end)
  "Sends buffer substring from begin to end to tdl lisp process"
  (let ((expr (buffer-substring begin end)))
    (save-window-excursion
      (if (save-excursion (get-buffer "*TDL-Listener*"))
          (send-tdl-expr-to-lisp expr)
        (progn (fi:open-lisp-listener 0 "TDL-Listener")
               (send-tdl-expr-to-lisp expr))))
    (pop-to-buffer (current-buffer))))


(defun eval-current-tdl-expression (arg)
  "Sends expression before current position to lisp process"
  (interactive "P")
  (eval-tdl-expression arg))


(defun calc-begin-of-tdl-expression ()
  "calculates begin of a tdl expression"
  (let ((pos (if (re-search-backward "[^\\.]\\.[ \t]*\\(;.*\\)*\n" nil t)
                 (point)
               (point-min))))
    (goto-char pos)
    (if pos           
        (progn (end-of-line)
               (skip-chars-forward " \t\n")
               (while (eq (following-char) ?\; )
                 (forward-line 1)
                 (beginning-of-line)
                 (skip-chars-forward " \t\n"))
               (point))
      (point-min))))


(defun calc-end-of-tdl-expression ()
  "calculates end of a tdl expression"
  (if (eq (preceding-char) ?.)
      (point)
    (let ((pos (if (re-search-forward "[^\\.]\\.[ \t]*\\(;.*\\)*\n" nil t)
                   (point)
                 (point-max))))
      (goto-char pos)
      (if pos
          (progn (search-backward "." (point-min) t)
                 (forward-char 1)
                 (point))
        (point-max)))))

  
(defun goto-begin-of-tdl-expression (arg)
  "go to begin of a tdl expression"
  (interactive "P")
  (goto-char (calc-begin-of-tdl-expression)))


(defun goto-end-of-tdl-expression (arg)
  "go to end of a tdl expression"
  (interactive "P")
  (goto-char (calc-end-of-tdl-expression)))


(defun eval-tdl-region (begin end &optional printflag)
  "eval a region of tdl expressions"
  (interactive "r\nP")
  (tdl-send-region begin end))


(defun eval-tdl-region-and-go (begin end &optional printflag)
  "eval a region of tdl expressions and go to inferior lisp"
  (interactive "r")
  (tdl-send-region begin end)
  ;;if (fboundp 'find-buffer-other-screen)
  ;;    (find-buffer-other-screen "*common-lisp*")
  (switch-to-buffer "*common-lisp*"))


(defun send-tdl-expr-to-lisp (expr)
  (save-excursion
    (set-buffer "*TDL-Listener*")
    ;;;(insert ?\n)
    (insert (format tdl-eval-expression-string 
                    (tdl-convert-delimiter expr)))
    (fi:lisp-eval-last-sexp)))


(defun tdl-convert-delimiter (string)
  "converts '' to \\''"
  (let* ((len (1- (length string)))
         (i 0)
         (char (aref string i))
         (newstring (make-string 0 ?\n)))
    (while (< i len)
      (if (char-equal char ?\")
          (setq newstring (concat newstring "\\" (char-to-string char)))
        (setq newstring (concat newstring (char-to-string char))))
      (setq i (1+ i))
      (setq char (aref string i)))
    newstring))

;;
;; show type in hierarchy on double click
;; assumes lkb and a grammar are loaded
;;

(defun tdl-show-type  ()
  "Show type of thing at point using lkb"
  (interactive)
  ;; assume that a type was clicked (is checked later)
  (setq tdl-type-at-point (thing-at-point 'symbol))
  ;; remove text properties
  (set-text-properties 0 (length tdl-type-at-point) nil tdl-type-at-point)
  ;; set up 
  (fi:eval-in-lisp 
   (format "(setq *last-type-name* '%s)"
	   tdl-type-at-point))
  ;; defined in lkb, shows *last-type-name*
  (show-type-tree))  


;;;STUFF FOR MINOR MODE (not used)
;;;(add-hook 'fi:lisp-mode-hook
;;;  (function (lambda ()  ...)))
;;;
;;;(or (assq 'tdl-mode minor-mode-alist)
;;;              (setq minor-mode-alist
;;;                    (cons '(tdl-mode " tdl") minor-mode-alist)))
;;;
;;;(defun tdl-mode (&optional arg)
;;;  (setq tdl-mode
;;;    (if (null arg) (not tdl-mode)
;;;      (> (prefix-numeric-value arg) 0))))
