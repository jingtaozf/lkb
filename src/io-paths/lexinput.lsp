;;; Copyright (c) 1991--2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Benjamin Waldron;
;;;   see `licence.txt' for conditions.

;;; modifications by bmw (dec-03)
;;; - internal reworking of cdb-lex-database + cdb-leaf-database classes 
;;;   and associated script functions

;;; Drastically simplified for YADU
;;; old laurel input stuff is now in laurel/lrlinput

;;; June 1999 - to fit in with Rob's code, all lexicon files must 
;;; be read in by a single command.  Language must be specified within
;;; the FS.  Overwrite is no longer relevant.

(in-package :lkb)

;;; Input from lexical entry and psort files

;;; This uses many of the functions from typeinput.lsp

;;; Non-lexentry psorts are essentially identical except
;;; that there is a simple id rather than a string plus 
;;; sense id

;;; Lexical entries have the following syntax (in BNF):
;;; 
;;; Lexentry -> LexID Path_spec+ Default*
;;;             Comment
;;; Comment -> ; anything eol
;;; LexID -> orth sense-id
;;; Path_spec and Default are defined in the type file

;(defvar *lex-file-list* nil)

(defvar *template-file* nil)

(defvar *root-file* nil)

(defvar *idiom-file* nil)

;; entry fn
(defun read-cached-sublex-if-available (name filenames)
  ;; force filenames to list
  (unless (listp filenames) 
    (setf filenames (list filenames)))
  (cond
   ((null filenames)
    (error "no file names supplied"))
   ((not (check-load-names filenames 'lexical))
    (error "Lexicon file not found")
    )
   (t		
    (let ((lex (make-instance 'cdb-lex-database)))
      (unless
          (open-lex lex
                    :name name
                    :parameters (list (make-nice-temp-file-pathname 
                                       (format nil "~a-~a.lex" (get-grammar-version) name))
                                      (make-nice-temp-file-pathname 
                                       (format nil "~a-~a.idx" (get-grammar-version) name))))
        (error "Cannot open new lexicon instance"))
      (unless (read-cached-lex lex filenames)
	(let ((syntax (if (eql *lkb-system-version* :page) :tdl :path)))
	  (load-lex-from-files lex filenames syntax)))
      (link lex *lexicon*)
      ))))

(defun clear-lex-load-files nil
;  (setf *ordered-lex-list* nil)         ; adding this makes
                                        ; the fn name a bit of a misnomer
;  (setf *lex-file-list* nil)
  (setf *template-file* nil)
  (setf *idiom-file* nil)
  (setf *root-file* nil))

(defun read-lex-file nil
  (let* ((file-name 
          (ask-user-for-existing-pathname "Entry file?")))
    (when file-name
      (set-temporary-lexicon-filenames)
      (unless
          (open-lex *lexicon* 
                    :parameters (list *psorts-temp-file* *psorts-temp-index-file*))
        (return-from read-lex-file))
      (load-lex-from-files *lexicon* (list file-name) :path)))
  t)

(defun reload-lex-files (&key (allp t))
  (when (typep *lexicon* 'psql-lex-database)
    #-:tty(format t "~%Use Load Complete Grammar instead")
    #+:tty(format t "~%Use (read-script-file-aux file-name) instead")
    (return-from reload-lex-files))
  (let ((lex-source-files (source-files *lexicon*)))
    (setf *syntax-error* nil)
    ;;  (if (check-load-names *lex-file-list* 'lexical)
    (if (check-load-names lex-source-files 'lexical)
	(progn
	  (set-temporary-lexicon-filenames)
	  (unless
              (open-lex *lexicon* 
                        :parameters (list *psorts-temp-file* *psorts-temp-index-file*))
            (return-from reload-lex-files))
;;	  (load-lex-from-files *lexicon* (reverse *lex-file-list*)
	  (load-lex-from-files *lexicon* lex-source-files
			       (if (eql *lkb-system-version* :page) :tdl :path))
	  (format t "~%Lexicon reload complete")
	  (when (and allp *template-file*)
	    (reload-template-file))
	  (when (and allp *root-file*)
	    (reload-root-file))
	  #+:psql
	  (when (and allp 
		     (mwe-lexicon-enabled-p))
	    (reload-roots-mwe *lexicon*))
	  (when (and allp *idiom-file*)
	    (reload-idiom-file)))
      (progn
	#-:tty(format t "~%Use Load Complete Grammar instead")
	#+:tty(format t "~%Use (read-script-file-aux file-name) instead"))))
  t)

(defun reload-template-file nil
  (setf *syntax-error* nil)
  (when (check-load-name *template-file*)
    (if (eql *lkb-system-version* :page)
	(read-tdl-psort-file-aux *template-file* :nodes)
        (read-psort-file-aux *template-file* :nodes))
    (format t "~%Template reload complete")))

(defun reload-idiom-file nil
  (setf *syntax-error* nil)
  (when (check-load-name *idiom-file*)
    (if (eql *lkb-system-version* :page)
	(read-tdl-psort-file-aux *idiom-file* :idiom)
      (read-psort-file-aux *idiom-file* :idiom))
    (format t "~%Idiom file reload complete")))

(defun reload-root-file nil
  (setf *syntax-error* nil)
  (when (check-load-name *root-file*)
    (if (eql *lkb-system-version* :page)
	(read-tdl-psort-file-aux *root-file* :root)
      (read-psort-file-aux *root-file* :root))
    (format t "~%Root file reload complete")))

;; entry fn
(defun read-cached-lex-if-available (filenames)
  ;; force filenames to list
  (unless (listp filenames) 
    (setf filenames (list filenames)))
  (cond
   ((null filenames)
    (error "no file names supplied"))
   ((not (check-load-names filenames 'lexical))
    (error "Lexicon file not found"))
   (t		
    (set-temporary-lexicon-filenames)
    (unless
        (open-lex *lexicon*
                  :name "main_lexicon"
                  :parameters (list *psorts-temp-file* *psorts-temp-index-file*))
      (error "Operation aborted"))
    (unless (read-cached-lex *lexicon* filenames)
      (let (
	    ;(*syntax-error* nil)
	    (syntax (if (eql *lkb-system-version* :page)
			:tdl
		      :path)))
	(load-lex-from-files *lexicon* filenames syntax)))))
  t)
	
;; entry fn
(defun read-tdl-lex-file-aux (filenames &optional overwrite-p)
  ;;; this is the version that is called from scripts
  ;;; this version uses the cached-lex code but never reads
  ;;; an existing file
  ;;; filenames may be a list
  (declare (ignore overwrite-p))
  ;;; now always overwrites but retain optional arg for consistency
  ;;; with old scripts
  (unless (listp filenames) 
    (setf filenames (list filenames)))  
  (set-temporary-lexicon-filenames)
  (unless
      (open-lex *lexicon* 
                :parameters (list *psorts-temp-file* *psorts-temp-index-file*))
    (return-from read-tdl-lex-file-aux))
  (load-lex-from-files *lexicon* filenames :tdl)
  t)

;; entry fn
(defun read-lex-file-aux (filenames &optional overwrite-p)
  ;;; this is the version that is called from scripts
  ;;; this version uses the cached-lex code but never reads
  ;;; an existing file
  ;;; filenames may be a list
  (declare (ignore overwrite-p))
  ;;; now always overwrites
  ;;; but retain optional arg for consistency
  ;;; with old scripts
  (unless (listp filenames) 
    (setf filenames (list filenames)))  
  (set-temporary-lexicon-filenames)
  (unless
      (open-lex *lexicon* 
                :parameters (list *psorts-temp-file* *psorts-temp-index-file*))
    (return-from read-lex-file-aux))
  (load-lex-from-files *lexicon* filenames :path))

(defun read-lex-file-aux-internal (file-name)
  (let ((*readtable* (make-path-notation-break-table)))
    (with-open-file 
        (istream file-name :direction :input)
      (format t "~%Reading in lexical entry file ~A" 
              (pathname-name file-name))
      (loop
        (let ((next-char (peek-char t istream nil 'eof)))
          (when (eql next-char 'eof) (return))
          (if (eql next-char #\;) 
              (read-line istream)
            (read-lex-entry istream)))))))

(defun read-lex-entry (istream)
  (let* ((orth (lkb-read istream nil))
	 (id (lkb-read istream nil)))
;      (push (make-lex-id orth id) *ordered-lex-list*)
;    (setf (cache-lex-list *lexicon-in*)
;      (cons (make-lex-id orth id) (collect-psort-ids *lexicon-in*))) ;;fix_me properly
    (multiple-value-bind 
	(non-def defs)
	(read-psort-unifications id istream)
      (add-lex-from-file orth id non-def defs))))

;;; other sorts of entry

(defun read-parse-nodes-file nil  
   (let ((file-name 
            (ask-user-for-existing-pathname "Node name file?")))
      (when file-name
        (if (eql *lkb-system-version* :page)
         (read-tdl-psort-file-aux file-name :nodes)
         (read-psort-file-aux file-name :nodes)))))

;;; general functions for psorts files

(defun initialise-psort-file (file-name file-type)
  (ecase file-type
    (:nodes (setf *template-file* file-name)
	    (clear-category-display-templates))
    (:idioms
     (setf *idiom-file* file-name)
     (clear-idioms-entries))
    (:root 
     (setf *root-file* file-name)
     (clear-root-entries))))

(defun finalize-psort-file (file-type)  
  (when (eql file-type :idioms) 
    (expand-idioms-phrases))
  (when (eql file-type :nodes) 
    (split-up-templates)))

(defun add-psort-file-entry (name constraint default file-type)
  (ecase file-type
    (:idioms
     (add-idiom-entry name constraint default))
    (:nodes
     (add-category-display-template name constraint default))
    (:root 
     (add-root-entry name constraint default))))

;;; actual reading of path notation

(defun read-psort-file-aux (file-name file-type)
  (initialise-psort-file file-name file-type)
  (let ((*readtable* (make-path-notation-break-table)))
    (with-open-file 
	(istream file-name :direction :input)
      (format t "~%Reading in ~A file ~A"
	      (cond ((eql file-type :nodes) "parse node")
		    (file-type file-type)
		    (t "entry"))
	      (pathname-name file-name))
      (loop
	(let ((next-char (peek-char t istream nil 'eof)))
	  (when (eql next-char 'eof) (return))
	  (if (eql next-char #\;) 
	      (read-line istream)
	    (read-psort-entry istream file-type)))))))

(defun read-psort-entry (istream file-type)
   (let ((id (lkb-read istream nil)))
      (multiple-value-bind (non-def defs)
         (read-psort-unifications id istream)
         (add-psort-file-entry id non-def defs file-type))))

(defun read-psort-unifications (orth istream &optional type)
   (let ((next-char (peek-char t istream nil 'eof))
         (type-unifs (if type (list 
                        (make-unification :lhs 
                           (make-path :typed-feature-list nil)
                           :rhs (make-u-value :type type))))))
      (cond 
         ((eql next-char 'eof) (error "~%Unexpected end of file"))
         ((char= next-char #\.)
          (read-char istream)
          (if type
             (values type-unifs nil)
            (progn (cerror "~%Ignore entry"
               "~%No lex specification for ~A" 
               orth) nil))) 
          ((or (char= next-char #\;) (char= next-char #\%))           
           (loop (read-line istream)
                 (let ((next-char2 (peek-char t istream nil 'eof)))
                   (unless 
                    (or (char= next-char2 #\;) (char= next-char2 #\%))
                    (return nil))))
           (read-psort-unifications orth istream type))
         ((char= next-char #\/)
            (let ((def-fss
                     (read-defaults istream orth nil)))
                 (check-for #\. istream orth)
                 (values type-unifs def-fss)))
         ((char= next-char #\<)
          (let* ((fs (read-path-spec-list istream orth))
                 (next-char (peek-char t istream nil 'eof)))
            (if (char= next-char #\/)
                (let ((def-fss
                        (read-defaults istream orth nil)))
                  (check-for #\. istream orth)
                  (values (append type-unifs fs) def-fss))
              (progn
                (check-for #\. istream orth)
                (values (append type-unifs fs) nil)))))
         ((not type) ; may have a type by itself 
          (let ((itype (lkb-read istream nil)))
            (if (and itype (is-valid-type itype))
                (read-psort-unifications orth istream itype))))
         (t
            (error "~%Badly formed lexical specification for ~A" orth)))))



(defun load-new-lex-entries (filename)
  (read-tdl-lex-file-aux (list filename)))

