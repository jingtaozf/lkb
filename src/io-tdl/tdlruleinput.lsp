;;; Copyright (c) 1998-2018 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see LICENSE for conditions


(in-package :lkb)

;;; Input from lexical rule and construction files
;;; in TDL format

;;; This uses many of the functions from tdltypeinput.lsp
;;; Syntax for TDL lexical entries, rules etc is effectively 
;;; the same as for type entries, so the functions in this file
;;; are basically concerned with handling the structures appropriately
;;; rather than reading them in

; *ordered-rule-list* is in main/rules.lsp

(defun read-tdl-grammar-file-aux (file-name &optional ovwr)
  (if ovwr 
    (setf *grammar-rule-file-list* (list file-name))
    (pushnew file-name *grammar-rule-file-list* :test #'equal))
  (when ovwr 
    (setf *ordered-rule-list* nil)
    (clear-grammar))
   (read-tdl-lex-or-grammar-rule-file file-name nil))

; *ordered-lrule-list* is in main/rules.lsp

(defun read-tdl-lex-rule-file-aux (file-name &optional ovwr)
  (unless (member file-name *morphology-rule-file-list* :test #'equal)
    (if ovwr 
        (setf *lexical-rule-file-list* (list file-name))
      (pushnew file-name *lexical-rule-file-list* :test #'equal)))
  (when ovwr 
    (setf *ordered-sprule-list* nil)
    (setf *ordered-lrule-list* nil))
  (when ovwr (clear-lex-rules) )    
  (read-tdl-lex-or-grammar-rule-file file-name t))   
      
(defun read-tdl-lex-or-grammar-rule-file (file-name lexical)
  (with-open-file (istream file-name :direction :input)
    (format t "~%Reading in ~Arules file ~A" 
            (if lexical "lexical " "")
            (pathname-name file-name))
    (read-tdl-rule-stream istream lexical)))


(defun read-tdl-rule-stream (istream lexical) 
  (loop
    (let ((next-char (peek-with-comments istream)))
      (cond
        ((eql next-char 'eof)
          (return))
        ((eql next-char #\%) ; Bernie morphology
	  (read-morphology-letter-set istream))
        (t 
          (catch 'syntax-error
            (read-tdl-rule-entry istream lexical)))))))


(defun read-tdl-rule-entry (istream lexical)
  (let* (#+(or :allegro :mcclim)
         (position nil) ; JAC - unused, was (1+ (file-position istream))
	 (id (lkb-read istream nil)))
    #+(or :allegro :mcclim) (record-source id istream position)
    (check-for-string ":=" istream id)
    (let ((orthographemicp
            (eql (peek-with-comments istream) #\%)))
      (when orthographemicp
        (read-morphology-affix id istream))
      (multiple-value-bind (non-def def)
          (read-tdl-lex-avm-def istream id)
        (check-for #\. istream id)
        (add-grammar-rule
          id non-def def *description-persistence* lexical orthographemicp)))))
