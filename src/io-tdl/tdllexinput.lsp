;;; Copyright Ann Copestake 1998 All Rights Reserved.
;;; No use or redistribution without permission.
;;;

(in-package :lkb)

;;; Input from lexical entry and psort/template files
;;; in TDL format

;;; This uses many of the functions from tdltypeinput.lsp
;;; Syntax for TDL lexical entries, rules etc is effectively 
;;; the same as for type entries, so the functions in this file
;;; are basically concerned with handling the structures appropriately
;;; rather than reading them in


;;; *category-display-templates* is in io-paths/lexinput

(defun read-tdl-lex-file-aux-internal (file-name)
  (let ((*readtable* (make-tdl-break-table)))
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
  (let* (#+:allegro
         (position  (1+ (file-position istream)))
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
         (add-lex-from-file nil name constraint default)))))



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

(defun read-tdl-parse-node-file-aux (file-name)
  (read-tdl-psort-file-aux file-name t))

(defun read-tdl-psort-file-aux (file-name &optional templates-p)
  (if templates-p
      (pushnew file-name *template-file-list* :test #'equal)
    (pushnew file-name *psort-file-list* :test #'equal))
  (when templates-p (setf *category-display-templates* nil))
  (let ((*readtable* (make-tdl-break-table)))
    (with-open-file 
	(istream file-name :direction :input)
      (format t "~%Reading in ~A file ~A"
	      (cond (templates-p "templates")
		    (t "psort"))
	      (pathname-name file-name))
      (read-tdl-psort-stream istream templates-p)))
  (when *simple-tree-display*
    (setf *category-display-templates* 
      (nreverse *category-display-templates*)))
  (if templates-p (split-up-templates)))

(defun read-tdl-psort-stream (istream &optional templates-p) 
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
                  (read-tdl-psort-entry istream templates-p)))))))

(defun read-tdl-psort-entry (istream &optional templates-p)
   (let* ((name (lkb-read istream nil))
          (next-char (peek-char t istream nil 'eof)))
     (unless (eql next-char #\:)
       (lkb-read-cerror 
        istream 
        "~%Incorrect syntax following template name ~A" name)
       (ignore-rest-of-entry istream name))
     (read-char istream)
     (let ((next-char2 (peek-char t istream nil 'eof)))
       (unless (eql next-char2 #\=)
         (lkb-read-cerror 
          istream 
          "~%Incorrect syntax following template name ~A" name)
         (ignore-rest-of-entry istream name))
       (read-char istream)
       (multiple-value-bind
           (constraint default)
           (read-tdl-lex-avm-def istream name)
         (check-for #\. istream name)
         (progn 
           (add-psort-from-file name constraint default)
           (if templates-p (pushnew name *category-display-templates*)))))))

 



