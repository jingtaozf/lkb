;;; Copyright (c) 1998-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions


(in-package :lkb)

;;; Input from lexical entry and psort/template files
;;; in TDL format

;;; This uses many of the functions from tdltypeinput.lsp
;;; Syntax for TDL lexical entries, rules etc is effectively 
;;; the same as for type entries, so the functions in this file
;;; are basically concerned with handling the structures appropriately
;;; rather than reading them in


(defun read-tdl-lex-file-aux-internal (file-name)
  (let ((*readtable* (make-tdl-break-table))) ;; this can be dangerous
    (with-open-file 
	(istream file-name :direction :input)
      (format t "~%Reading in lexical entry file ~A" 
	      (pathname-name file-name))
      (read-tdl-lex-stream istream))))


(defun read-tdl-lex-stream (istream) 
  (loop
      (let ((next-char (peek-char t istream nil 'eof)))
         (when (eql next-char 'eof) (return))
         (cond ((eql next-char #\;) 
                 (read-line istream))
               ; one line comments
               ((eql next-char #\:) 
                 (read-tdl-declaration istream))
               ; declarations like :begin :type
               ((eql next-char #\#) (read-tdl-comment istream))
               (t (catch 'syntax-error
                (read-tdl-lex-entry istream)))))))


(defun read-tdl-lex-entry (istream)
;;; Lex-def -> Lexid Avm-def . 
;;; Lexid  -> identifier
;;; Avm-def -> := Conjunction (as in tdltypeinput.lsp)
  (let* ((position  (1+ (file-position istream)))
	 (name (lkb-read istream nil))
	 (next-char (peek-char t istream nil 'eof)))
     (unless (eql next-char #\:)
       (lkb-read-cerror istream
                        "~%Incorrect syntax following lexicon name ~A" name)
       (ignore-rest-of-entry istream name))
     #+:allegro (record-source name istream position)
     (read-char istream)
     (let ((next-char2 (peek-char t istream nil 'eof)))
       (unless (eql next-char2 #\=)
         (lkb-read-cerror istream
                          "~%Incorrect syntax following lexicon name ~A" name)
         (ignore-rest-of-entry istream name))
       (read-char istream)
       (multiple-value-bind
           (constraint default)
           (read-tdl-lex-avm-def istream name)
         (check-for #\. istream name)
         (if (member name *ordered-lex-list* :test #'eq)
           (format
            t
            "~%WARNING: lexicon entry `~a' redefined." name)
           (push name *ordered-lex-list*))
	 (let ((*readtable* (copy-readtable nil))) ;;bmw
	   (add-lex-from-file nil name constraint default))))))



(defun read-tdl-lex-avm-def (istream name)
  ;;; Avm-def -> := Conjunction
  (clrhash *tdl-coreference-table*)     ; parameter defined in tdltypeinput
  (clrhash *tdl-default-coreference-table*)
  (let ((constraint nil)
        (def-alist nil))
      ;;; read-tdl-conjunction in tdltypeinput
    (loop for unif in (read-tdl-conjunction istream name nil nil)
         do
         (cond ((unification-p unif) (push unif constraint))
               ((consp unif)
                (let ((entry (assoc (car unif) def-alist)))
                  (if entry
                      (push (cadr unif) (cdr entry))
                    (push unif def-alist))))
               (t (error "~%Program error(?): Unexpected unif in ~A" name))))
    (dolist (coref (make-tdl-coreference-conditions istream
                    *tdl-coreference-table* nil))
      (push coref constraint))
    (dolist (coref (make-tdl-coreference-conditions istream 
                    *tdl-default-coreference-table* t))
      (let ((entry (assoc (car coref) def-alist)))
        (if entry
            (push (cadr coref) (cdr entry))
          (push coref def-alist))))
    (values constraint def-alist)))


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
  (let ((*readtable* (make-tdl-break-table)))
    (with-open-file 
	(istream file-name :direction :input)
      (format t "~%Reading in ~A file ~A"
	      (cond ((eql file-type :nodes) "parse node")
		    (file-type file-type)
		    (t "entry"))
	      (pathname-name file-name))
      (read-tdl-psort-stream istream file-type)))
  (finalize-psort-file file-type))

(defun read-tdl-psort-stream (istream file-type)
   (loop
      (let ((next-char (peek-char t istream nil 'eof)))
         (when (eql next-char 'eof) (return))
         (cond ((eql next-char #\;) 
                 (read-line istream))
               ; one line comments
               ((eql next-char #\:) 
                 (read-tdl-declaration istream))
               ; declarations like :begin :type
               ((eql next-char #\#) 
                (read-tdl-comment istream))
               (t 
                (catch 'syntax-error
                  (read-tdl-psort-entry istream file-type)))))))

(defun read-tdl-psort-entry (istream file-type)
  (let* ((name (lkb-read istream nil))
	 (next-char (peek-char t istream nil 'eof)))
    (unless (eql next-char #\:)
      (lkb-read-cerror 
       istream 
       "~%Incorrect syntax following ~A" name)
      (ignore-rest-of-entry istream name))
    (read-char istream)
    (let ((next-char2 (peek-char t istream nil 'eof)))
      (unless (eql next-char2 #\=)
	(lkb-read-cerror 
	 istream 
	 "~%Incorrect syntax following ~A" name)
	(ignore-rest-of-entry istream name))
      (read-char istream)
      (multiple-value-bind
	  (constraint default)
	  (read-tdl-lex-avm-def istream name)
	(check-for #\. istream name)
      (add-psort-file-entry name constraint default file-type)))))

 



