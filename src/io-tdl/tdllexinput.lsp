;;; Copyright (c) 1998-2018 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Benjamin Waldron
;;; see LICENSE for conditions


;;; modifications by bmw (dec-03)
;;; - internal reworking of cdb-lex-database + cdb-leaf-database classes 
;;;   and associated script functions

(in-package :lkb)

;;; Input from lexical entry and psort/template files
;;; in TDL format

;;; This uses many of the functions from tdltypeinput.lsp
;;; Syntax for TDL lexical entries, rules etc is effectively 
;;; the same as for type entries, so the functions in this file
;;; are basically concerned with handling the structures appropriately
;;; rather than reading them in


(defun read-tdl-lex-file-aux-internal (file-name)
  (with-open-file (istream file-name :direction :input)
    (format t "~%Reading in lexical entry file ~A" 
	    (pathname-name file-name))
    (read-tdl-lex-stream istream)))


(defun read-tdl-lex-stream (istream)
  (loop
    (if (eq (peek-with-comments istream) 'eof)
      (return)
      (catch 'syntax-error
        (read-tdl-lex-entry istream)))))


(defun read-tdl-lex-entry (istream)
;;; Lex-def -> Lexid Avm-def . 
;;; Lexid  -> identifier
;;; Avm-def -> := Conjunction (as in tdltypeinput.lsp)
  (let* (#+(or :allegro :mcclim)
         (position nil) ; JAC - unused, was (1+ (file-position istream))
	 (name (lkb-read istream nil)))
    #+(or :allegro :mcclim) (record-source name istream position)
    (check-for-string ":=" istream name)
    (multiple-value-bind (constraint default)
        (read-tdl-lex-avm-def istream name)
      (check-for #\. istream name)
      (unless (hash-table-p *ordered-lex-list*)
        ;; !!! note that this is not ordered, and it doesn't have to be
        (setq *ordered-lex-list* (make-hash-table :test #'eq)))
      (if (gethash name *ordered-lex-list*)
	  (format t
	    "~%WARNING: lexical entry `~a' redefined." name)
	  (setf (gethash name *ordered-lex-list*) t)
;	   (setf (cache-lex-list *lexicon-in*)
;	     (cons name (collect-psort-ids *lexicon-in*))) ;;fix_me properly
	  )
      (add-lex-from-file nil name constraint default))))


(defun read-tdl-lex-avm-def (istream name)
  ;; analogous to read-tdl-avm-def for type definitions
  (clrhash *tdl-coreference-table*)
  (clrhash *tdl-default-coreference-table*)
  (let ((comment nil) (constraint nil) (def-alist nil))
    (multiple-value-bind (top-conj c)
                         (read-tdl-top-conjunction istream name)
      (setq comment c)
      (dolist (unif top-conj)
        (cond ((unification-p unif) (push unif constraint))
              ((consp unif)
                (let ((entry (assoc (car unif) def-alist)))
                  (if entry
                      (push (cadr unif) (cdr entry))
                      (push unif def-alist))))
              (t (error "Inconsistency in read-tdl-lex-avm-def: unexpected unif in ~A"
                        name))))
      (dolist (coref
                (make-tdl-coreference-conditions istream *tdl-coreference-table* nil))
        (push coref constraint))
      (dolist (coref
                (make-tdl-coreference-conditions istream *tdl-default-coreference-table* t))
        (let ((entry (assoc (car coref) def-alist)))
          (if entry
              (push (cadr coref) (cdr entry))
              (push coref def-alist))))
      (values constraint def-alist comment))))


;;; Other varieties of files

(defun read-tdl-start-file-aux (file-name)
  (read-tdl-psort-file-aux file-name :root))

(defun read-tdl-parse-node-file-aux (file-name)
  (read-tdl-psort-file-aux file-name :nodes))

(defun read-tdl-idioms-file-aux (file-name)
  (read-tdl-psort-file-aux file-name :idioms))

(defun read-tdl-psort-file-aux (file-name &optional file-type)
  ;;; file-type shouldn't really be optional, but
  ;;; need this for backward compatibility with old grammar scripts
  (unless file-type
    (setf file-type :root))
  (initialise-psort-file file-name file-type)
  ;; in lexinput.lsp
  (with-open-file (istream file-name :direction :input)
    (format t "~%Reading in ~A file ~A"
	    (cond ((eql file-type :nodes) "parse node")
		  (file-type (string-downcase file-type))
		  (t "entry"))
	    (pathname-name file-name))
    (read-tdl-psort-stream istream file-type))
  (finalize-psort-file file-type))

(defun read-tdl-psort-stream (istream file-type)
  (loop
    (if (eq (peek-with-comments istream) 'eof)
      (return)
      (catch 'syntax-error
        (read-tdl-psort-entry istream file-type)))))

(defun read-tdl-psort-entry (istream file-type)
  (let ((name (lkb-read istream nil)))
    (check-for-string ":=" istream name)
    (multiple-value-bind (constraint default)
        (read-tdl-lex-avm-def istream name)
      (check-for #\. istream name)
      (add-psort-file-entry name constraint default file-type))))

