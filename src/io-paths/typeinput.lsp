;;; Copyright Ann Copestake 1991-1997. All Rights Reserved.
;;; No use or redistribution without permission.
;;; modified for defaults 1995
;;; 
;;; removed read-checked-type-file
;;; display-setting stuff is currently commented out

;;; Input from type files

;;; New - anything following a / is default

;;; Type specifications have the following syntax (in BNF):
;;; 
;;; Type specification -> typename Parents Constraint DefConstraint* . | 
;;;                       typename Parents DefConstraint*  . |
;;;                       Comment
;;; Comment -> ; anything eol
;;; Parents -> ( Typename_list ) | ( )
;;; Typename_list -> typename | typename Typename_list
;;; DefConstraint -> / persistence Constraint
;;; Constraint -> Path_spec_list | Enumeration
;;; Path_spec_list -> Path_spec | Path_spec Path_spec_list
;;; Path_spec -> EPath = Typevalue | Path = Path
;;; Typevalue -> typename | ( typename+ )
;;; Epath -> Path | <>
;;; Path -> < Type_F_pair_list >
;;; Type_F_pair_list -> Type_Feature_pair | 
;;;                           Type_Feature_pair : Type_F_pair_list 
;;; Type_Feature_pair -> typename feature | feature
;;;
;;; Enumeration -> (OR Typename_list)
;;; 
;;; typename and feature are terminal symbols (Lisp atoms)
;;; so is persistence
;;; other symbols are < > ( ) = : . /
;;; 
;;; Enumeration is syntactic sugar for enumerated atomic types.  For example:
;;; 
;;; state (T) (OR solid liquid gas).
;;; 
;;; expands out into the equivalent of:
;;; 
;;; state (T).
;;; solid (state).
;;; liquid (state).
;;; gas (state).
;;; 
;;; Syntactic sugar for lists is still to be defined

(defun read-type-file nil  
   (let* ((file-name 
            (ask-user-for-existing-pathname "Type file?"))
          (settings-file nil))
;         (if *settings-options* ; in globals.lsp
;            (ask-user-for-existing-pathname 
;               "Display settings file? (Cancel if none)"))
      (when file-name
         (read-type-file-aux file-name settings-file))))

(defun read-type-file-aux (file-name &optional settings-file)
   (setf *ordered-type-list* nil)
   (clear-types)
   (let ((*readtable*
            (define-break-characters 
               '(#\; #\< #\> #\= #\: #\. #\/))))
      (with-open-file 
         (istream file-name :direction :input)
         (format t "~%Reading in type file")
         (read-type-stream istream))) 
   ;; check-type-table is in checktypes.lsp           
   (when (check-type-table) 
      (canonicalise-feature-order)
      (when settings-file
         (set-up-display-settings settings-file))           
      (set-up-type-interactions)
      (lkb-beep)))

(defun read-type-files nil  
   (let* ((file-names 
            (ask-user-for-existing-pathnames 
               "Type file? (Cancel to finish)"))
            (settings-file nil))
;            (ask-user-for-existing-pathname 
;               "Display settings file? (Cancel if none)"))
      (when file-names
         (read-type-files-aux file-names settings-file))))

(defun read-type-files-aux (file-names &optional settings-file)
   (setf *ordered-type-list* nil)
   (clear-types)
   (let ((*readtable*
            (define-break-characters 
               '(#\; #\< #\> #\= #\: #\. #\/))))
      (for file-name in file-names
         do
         (format t "~%Reading in type file ~A" file-name)
         (with-open-file 
            (istream file-name :direction :input)
            (read-type-stream istream)))) 
   ;; check-type-table is in checktypes.lsp           
   (when (check-type-table) 
      (canonicalise-feature-order)
      (when settings-file
         (set-up-display-settings settings-file))           
      (set-up-type-interactions)
      (lkb-beep)))


             
(defun read-type-stream (istream)
   (loop
      (let ((next-char (peek-char t istream nil 'eof)))
         (when (eql next-char 'eof) (return))
         (if (eql next-char #\;) 
            (read-line istream)
            (read-type-entry istream)))))

(defun read-type-entry (istream)
   (let* ((name (read istream))
         (parents (read istream))
         (comment nil)
         (next-char (peek-char t istream nil 'eof)))
      (when (and (symbolp name) (eql (schar (symbol-name name) 0) #\%))
         (error "Illegal type name ~A - names starting with '%' are reserved ~
                 for instance types" name))
      (push name *ordered-type-list*)
      (when (eql next-char #\")
         (setf comment (read istream))
         (setf next-char (peek-char t istream nil 'eof)))
      (cond ((eql next-char 'eof) 
            (cerror "Continue"
               "~%Unexpected end of file at type ~A
               Possibly a missing ." name))
         ((char= next-char #\.) 
            (read-char istream)
            (add-type-from-file name parents nil nil comment))
         ((char= next-char #\( )
            (let ((daughters (read-disjunction istream name)))
               (add-enumerated-types name daughters)
               (add-type-from-file name parents nil nil comment daughters))
            (check-for #\. istream name))
         ((char= next-char #\/)
            (let ((def-constraint 
                     (read-defaults istream name nil)))
               (add-type-from-file name parents nil 
                  def-constraint comment))
            (check-for #\. istream name))
         ((char= next-char #\<)
            (let* ((constraint (read-path-spec-list istream name))
                  (next-char (peek-char t istream nil 'eof)))
               (if (char= next-char #\/)
                  (let ((def-constraint 
                           (read-defaults istream name nil)))
                     (add-type-from-file name parents constraint
                        def-constraint comment))                  
                  (add-type-from-file name parents constraint 
                     nil comment)))
            (check-for #\. istream name))
         (t (if (eql name '>)
               (error "~%Badly formed type specification 
                  - probably a period occurred before the
                  end of a unification specialisation")
            (error "~%Badly formed type specification for ~A" name))))))


(defun read-defaults (istream name defaults-so-far)
   ;;; keeps reading while next stuff begins with a slash
   ;;; returns an a-list of persistence atom cons
   ;;; with the results from read-path-spec-list
   (check-for #\/ istream name)  
   (let ((next-char (peek-char t istream nil 'eof)))
      (when (char= next-char #\<)
         (error "Missing persistence label in ~A" name))
      (let ((persistence (read istream)))
         (push (cons persistence 
               (read-path-spec-list istream name))
            defaults-so-far)
         (setf next-char (peek-char t istream nil 'eof))
         (if (char= next-char #\/)
            (read-defaults istream name defaults-so-far)
            defaults-so-far))))
                                      

(defun add-enumerated-types (name daughter-list)
   (for daughter in daughter-list
      do
      (add-type-from-file daughter (list name) nil nil nil)))

(defun read-disjunction (istream name)
   (let ((daughter-list (read istream)))
      (unless (and (listp daughter-list)
            (eql (car daughter-list) 'or))
         (error "Badly formed disjuction in ~A" name))
      (cdr daughter-list)))


(defun read-path-spec-list (istream name)
   ;;; Path_spec_list -> Path_spec | Path_spec Path_spec_list
   ;;; Path_spec -> Path = typename | Path = Path
   ;;; Path -> < Type_F_pair_list >
   ;;; Type_F_pair_list -> Type_Feature_pair | 
   ;;;                           Type_Feature_pair : Type_F_pair_list 
   ;;; Type_Feature_pair -> typename feature | feature
   ;;;
   ;;; read a list of path specs
   ;;; finishes when . or / is found
   (let ((path-spec-list nil))
      (loop
         (let ((next-char (peek-char t istream nil 'eof)))
            (when (eql next-char 'eof)
               (error "~%Unexpected eof when reading ~A" name))
            (when (or (char= next-char #\.) (char= next-char #\/)
                  (char= next-char #\-))
               (return))
            (if (or (eql next-char #\;) (eql next-char #\%))
                 ; % is for Bernie's stuff in morphology files
               (read-line istream)
               (let
                  ((item (read-path-spec istream name)))
                  (push item path-spec-list)))))
      (reverse path-spec-list)))

(defun read-path-spec (istream name)
   (let ((lhs (read-real-path istream name)))
      (check-for #\= istream name)
      (make-unification :lhs lhs :rhs (read-path istream name))))

(defun read-real-path (istream name)
   (check-for #\< istream name)
   (read-typed-path istream name))

(defun read-path (istream name)
   (let ((next-char (peek-char t istream nil 'eof)))
      (cond ((eq next-char #\<)
            (read-char istream)
            (read-typed-path istream name))
         (t (let ((value (read istream)))
               (make-u-value :types
                  (if (atom value)
                     (list value) value)))))))

(defun read-typed-path (istream name)
   (let ((typed-feature-list nil))
   (loop
      (let ((next-char (peek-char t istream nil 'eof)))
         (when (eql next-char 'eof)
            (error "~%Unexpected eof when reading ~A" name))
         (when (char= next-char #\>)
            (read-char istream)
            (return))
         (push (read-type-feature-pair istream name) 
            typed-feature-list)))
  (make-path :typed-feature-list (nreverse typed-feature-list))))

(defun read-type-feature-pair (istream name)
   ; the seperating : has to be delimited by spaces at the moment
   (unless *toptype* (error "No top type has been defined"))
   (let* ((token (read istream))
         (next-char (peek-char t istream nil 'eof)))    
      (cond 
         ((eql next-char 'eof)
            (error "~%Unexpected eof when reading ~A" name)) 
         ((char= next-char #\>) 
            (make-type-feature-pair :type *toptype* :feature token))
         ((char= next-char #\:) 
            (read-char istream)
            (make-type-feature-pair :type *toptype* :feature token))
         (t 
            (let* ((feature (read istream))
                  (end-char (peek-char t istream nil 'eof)))
               (cond 
                  ((eql end-char 'eof)
                     (error "~%Unexpected eof when reading ~A" name)) 
                  ((char= end-char #\>) 
                     (make-type-feature-pair :type token :feature feature))
                  ((char= end-char #\:) 
                     (read-char istream)
                     (make-type-feature-pair :type token :feature feature))
                  (t (error "~%Incorrect path spec in ~A" name))))))))

