;;; Copyright Ann Copestake 1996 All Rights Reserved.
;;; No use or redistribution without permission.

(in-package :lkb)

;;; 1997 - added status stuff so that we can read files without
;;; modification to get rid of this


;;; Input from type files in (a subset of) TDL format
;;; the subset is determined by a) what should be implementable in
;;; the LKB system b) what is actually used currently

;;; Type specifications have the following syntax (in BNF):
;;; 
;;; Type-def -> Type { Avm-def | Subtype-def} . | 
;;;                  Type { Avm-def | Subtype-def} Status .
;;; Type  -> identifier
;;; Subtype-def ->  :< type 
;;; Avm-def -> := Conjunction
;;; Conjunction -> Term { & Term } *
;;; Term -> Type | Feature-term | Diff-list | List | Coreference | Templ-call
;;; Feature-term -> [] | [ Attr-val {, Attr-val}* ]
;;; Attr-val -> attribute {.attribute}* Conjunction
;;;   (I am fairly convinced that the TDL manual BNF form is wrong
;;;    wrt Attr-val)
;;; Diff-list -> <! !> | <! Conjunction {, Conjunction}* !>
;;; List -> < > | < Conjunction {, Conjunction}* > |
;;;                < Conjunction {, Conjunction}* , ...> |
;;;                 < Conjunction {, Conjunction}* . Conjunction> 
;;; Coreference -> #corefname
;;; Templ-call -> @templ-name ( ) | @templ-name (Templ-par {, Templ-par}*)
;;; Templ-par -> $templ-var | $templ-var = conjunction
;;;
;;; Status -> status: status-name
;;;
;;; special characters are
;;; . : < = & , # [ ] @ $ ( ) > ! ^
;;; ^ - added - indicates `expanded syntax'
;;; / - added - indicates default
;;; also note that % indicates an instance type

;;; Modification for defaults
;;; Basically we change the definition of Conjunction and add a new
;;; entity DefTerm, which may be an ordinary Term or contain a default
;;; Conjunction -> DefTerm { & DefTerm } *
;;; DefTerm -> Term | Term / Term | / Term
;;; Note that the / may occur inside a FS
;;; There is a wrinkle, which is that once we're inside a default,
;;; we can't slash again.  Showing this in BNF is messy, so I won't.

;;; the type file extra.tdl is necessary for the basic type definitions
;;; to get the templates, evaluate the file templates.lsp

(defparameter *tdl-expanded-syntax-function* nil)

(defparameter *tdl-status-info* nil)

(defun make-tdl-break-table nil 
  (define-break-characters '(#\< #\> #\! #\= #\: #\. #\# #\&
                             #\, #\[ #\] #\; #\@ #\$ #\( #\) #\^ #\/)))


(defun read-tdl-type-file-aux (file-name &optional settings-file)
  (read-tdl-type-files-aux (list file-name) settings-file))

(defun read-tdl-type-files-aux (file-names &optional settings-file)
  (unless (listp file-names)
    (setf file-names (list file-names)))
  (when settings-file
    (setf *display-settings-file* settings-file))
   (setf *type-file-list* file-names)
   (clear-types)
   (setf *tdl-status-info* nil)
   (setf *toptype* '*top*)
   (add-type-from-file '*top* nil nil nil nil)
   (let ((*readtable* (make-tdl-break-table)))
      (loop for file-name in file-names
         do
         (format t "~%Reading in type file ~A" (pathname-name file-name))
         (force-output t)
         (with-open-file 
            (istream file-name :direction :input)
            (read-tdl-type-stream istream)))) 
   ;; check-type-table is in checktypes.lsp           
   (if *syntax-error*
       (progn (setf *syntax-error* nil)
              (cerror "Cancel load" "Syntax error(s) in type file")
              nil)
     (let ((ok 
            (when (check-type-table) 
              (canonicalise-feature-order)
              (when settings-file
                (set-up-display-settings settings-file))
              (set-up-type-interactions)
              t)))
       (unless ok (cerror "Continue loading script anyway" 
        "Problems in type file")))))

(defun read-tdl-leaf-type-file-aux (file-name)
  (pushnew file-name *leaf-type-file-list* :test #'equal)
  (let ((*readtable* (make-tdl-break-table))
        (*leaf-type-addition* t))
      (with-open-file 
         (istream file-name :direction :input)
        (format t "~%Reading in leaf type file ~A" 
                (pathname-name file-name))
         (read-tdl-type-stream istream))))

(defun read-tdl-patch-files-aux (file-names)
    (let ((*readtable* (make-tdl-break-table)))
      (loop for file-name in file-names
         do
         (format t "~%Reading in type file ~A" 
                 (pathname-name file-name))
         (with-open-file 
            (istream file-name :direction :input)
            (read-tdl-type-stream istream t)))) 
    ;; check-type-table is in checktypes.lsp 
    (if *syntax-error*
      (progn (setf *syntax-error* nil)
             (cerror "Cancel load" "Syntax error(s) in type file")
             nil)
      (if *amend-error*
          (setf *amend-error* nil)
        (when (patch-type-table) 
          (canonicalise-feature-order)           
          (set-up-type-interactions)
          t)) ))   


;;; main functions

(defun read-tdl-type-stream (istream &optional augment) 
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
                    (read-tdl-type-entry istream augment)))))))

(defun read-tdl-comment (istream)
  (let ((start-position (file-position istream)))
    ;; record this in case the comment isn't closed
    (read-char istream)
    (check-for #\| istream "no context")
    (loop 
      (let ((new-char (peek-char #\| istream nil 'eof)))
        (when (eql new-char 'eof)
          (lkb-read-cerror 
           istream 
           "File ended in middle of comment (comment start at ~A)" 
           start-position)
          (return))
        (read-char istream)
        (setf new-char (peek-char t istream nil 'eof))
        (when (eql new-char 'eof)
          (lkb-read-cerror 
           istream 
           "File ended in middle of comment (comment start at ~A)" 
           start-position)
          (return))
        (when (eql new-char #\#)
          (read-char istream)
          (return))
        (read-char istream)))))

(defun read-tdl-declaration (istream)
  (read-char istream)
  (let* ((begin-or-end (read istream))
         (break-char (read istream))
         (decl-type (read istream)))
    (declare (ignore break-char))
    (if (and (eql begin-or-end 'begin)
             (eql decl-type 'lisp))
      (loop (read-line istream)
            (let ((next-char (peek-char t istream nil 'eof)))
              (when (eql next-char #\:)
                (read-char istream)
                (let* ((new-begin-or-end (read istream))
                       (new-break-char (read istream))
                       (new-decl-type (read istream)))
                  (declare (ignore new-break-char))
                  (when (and (eql new-begin-or-end 'end)
                             (eql new-decl-type 'lisp))
                    (read-line istream)
                    (return))))))
      (read-line istream))))

(defun read-tdl-type-entry (istream &optional augment)
;;; Type-def -> Type { Avm-def | Subtype-def} . |
;;;             Type { Avm-def | Subtype-def} Status .
;;; Type  -> identifier
;;; Subtype-def ->  :< type 
;;; Avm-def -> := Conjunction
  (let* (#+:allegro
         (position (1+ (file-position istream)))
	 (name (lkb-read istream nil))
	 (next-char (peek-char t istream nil 'eof)))
    (when (and (symbolp name) (eql (schar (symbol-name name) 0) #\%))
      (lkb-read-cerror istream 
       "Illegal type name ~A - names starting with '%' are reserved ~
                 for instance types" name)
      (ignore-rest-of-entry istream name))
    (pushnew name *ordered-type-list*)    
    (unless (eql next-char #\:)
      (lkb-read-cerror istream
                       "~%Incorrect syntax following type name ~A" name)
      (ignore-rest-of-entry istream name))
    #+allegro (record-source name istream position)
    (read-char istream)
    (let ((next-char2 (peek-char t istream nil 'eof)))
      (cond 
       ((eql next-char2 #\=) 
         (read-char istream)
         (read-tdl-avm-def istream name augment)
         (check-for #\. istream name))
        ((eql next-char2 #\<) 
         (read-char istream)
         (read-tdl-subtype-def istream name augment)
         (check-for #\. istream name))
        (t (lkb-read-cerror istream
            "~%Syntax error following type name ~A" name)
           (ignore-rest-of-entry istream name))))))


(defun read-tdl-subtype-def (istream name &optional augment)
;;; Subtype-def ->  :< type 
     (let* ((parent (lkb-read istream nil)))
       (if augment
         (unless
           (amend-type-from-file name (list parent) nil nil nil)
           (setf *amend-error* t))
         (add-type-from-file name (list parent) nil nil nil))
       (let ((next-char (peek-char t istream nil 'eof)))
         (when (eql next-char #\,)
           (read-tdl-status-info istream name)))))

(defparameter *tdl-coreference-table* (make-hash-table))
(defparameter *tdl-default-coreference-table* (make-hash-table))

(defun read-tdl-avm-def (istream name &optional augment)
  ;;; Avm-def -> := Conjunction
  ;;; for the lkb type files we need to distinguish between
  ;;; the list of parents i.e. single types
  ;;; which are listed in the conjunction, and a constraint,
  ;;; expressed as a list of unifications
  (clrhash *tdl-coreference-table*)
  (clrhash *tdl-default-coreference-table*)
  (let ((parents nil)
        (constraint nil)
        (def-alist nil))
      ;;; read-tdl-conjunction returns a list of path constraints.
      ;;; In some cases the element may be nil.  If there is a
      ;;; path constraint with an empty path, for the special case of
      ;;; types we want to extract the type and put it on the parents
      ;;; list
      (loop for unif in (read-tdl-conjunction istream name nil nil)
           do
           (cond ((unification-p unif)
                  (if (null (path-typed-feature-list (unification-lhs unif)))
                      (push (u-value-type (unification-rhs unif)) parents)
                    (push unif constraint)))
                 ((consp unif)
                  (let ((entry (assoc (car unif) def-alist)))
                    (if entry
                        (push (cadr unif) (cdr entry))
                      (push unif def-alist))))
                 (t (error "~%Program error: unexpected unif in ~A" name))))
      (dolist (coref (make-tdl-coreference-conditions istream
                      *tdl-coreference-table* nil))
        (push coref constraint))
      (dolist (coref (make-tdl-coreference-conditions istream 
                      *tdl-default-coreference-table* t))
        (let ((entry (assoc (car coref) def-alist)))
          (if entry
              (push (cadr coref) (cdr entry))
            (push coref def-alist))))
      (if augment
        (unless
          (amend-type-from-file name parents constraint def-alist nil)
          (setf *amend-error* t))
        (add-type-from-file name parents constraint def-alist nil))
      (when (eql (peek-char t istream nil 'eof) #\,)
          (read-tdl-status-info istream name))))

(defun read-tdl-status-info (istream name)
  ;;; 
  ;;; Status -> status: status-name
  ;;;
  (read-char istream)
  (let* ((status-indicator (read istream))
         (break-char (read istream))
         (status-type (read istream)))
    (unless (and (eql status-indicator 'status)
             (eql  break-char #\:))
      (lkb-read-cerror istream "Unrecognised symbol ~A when reading ~A" status-indicator name)
      (ignore-rest-of-entry istream name))
    (push (cons name
                (format nil "~A~A ~A" 
                        status-indicator break-char status-type))
          *tdl-status-info*)))

(defun make-tdl-coreference-conditions (istream coref-table in-default-p)
  ;;; the coref table is a list of paths, indexed by
  ;;; a coreference atom.  
  ;;; If there happens to be only one path 
  ;;;     we make a boring unification of the form path = top,
  ;;;    just in case this is the only indication that 
  ;;;    there's a non-atomic type at some point along the path
  ;;;    and output a warning message
  ;;;  Otherwise
  ;;;     we make a series of path-path unifications with the first
  ;;;     path on the list, pairwise with any others.  For example
  ;;;     if the list is (< F G >, < H >, < I J > ),
  ;;;     we output < F G > = < H >
  ;;;               < F G > = < I J >
  (let ((unifs nil))
    (maphash #'(lambda (index value)
                 (let ((path1 (if in-default-p 
                                  (cdar value)
                                (car value)))
                       (persist (if in-default-p 
                                  (caar value)))
                       (rest (cdr value)))
                   ;;; this assumes that we can keep
                   ;;; the persistance the same on all the bits
                   (if rest
                     (loop for path2 in rest
                          do
                          (push (make-tdl-path-path-unif 
                                 path1 
                                 (if in-default-p (cdr path2) path2) 
                                 persist) 
                                unifs))
                     (progn
                       (lkb-read-cerror istream "Coreference ~A only used once"
                                        index)
                     (push (make-tdl-path-value-unif path1 *toptype* 
                                                     persist)
                           unifs)))))
             coref-table)
    unifs))



;;; name is carried around in functions from this point on
;;; only for the purposes of giving more helpful error messages

(defun read-tdl-conjunction (istream name path-so-far in-default-p)
  ;;; was
  ;;; Conjunction -> Term { & Term } *
  ;;; now 
  ;;; Conjunction -> DefTerm { & DefTerm } *
  (let* ((constraint nil))
    (loop
      (let* ((term (read-tdl-defterm istream name path-so-far in-default-p))
             (next-char (peek-char t istream nil 'eof)))
        (setf constraint 
              (nconc term constraint))
        (unless (eql next-char #\&) (return))
        (read-char istream)))
    constraint))

(defun read-tdl-defterm (istream name path-so-far in-default-p)
;;; DefTerm -> Term | Term / Term | / Term  
  (let ((next-char (peek-char t istream nil 'eof)))
    (cond ((eql next-char 'eof) 
           (lkb-read-cerror istream 
                            "Unexpected eof when reading ~A" name)
           (ignore-rest-of-entry istream name))
          ((eql next-char #\.) 
           (lkb-read-cerror istream "Missing term when reading ~A" name)
           (ignore-rest-of-entry istream name))
          ((eql next-char #\/)
           (when in-default-p
             (lkb-read-cerror istream
                              "Double defaults when reading ~A" name)
             (ignore-rest-of-entry istream name))
           (check-for #\/ istream name)
           (let ((persist (lkb-read istream t)))
             (if path-so-far
                 (cons
                  (make-tdl-path-value-unif (reverse path-so-far) *toptype* nil)
                  ;; need to add non-default path too
                  (read-tdl-term istream name path-so-far persist))
              (read-tdl-term istream name path-so-far persist)))) 
          (t  
           (let ((res1 (read-tdl-term istream name path-so-far in-default-p))
                 (next-char2 (peek-char t istream nil 'eof)))
               (cond ((eql next-char2 'eof) 
                      (lkb-read-cerror istream
                                       "Unexpected eof when reading ~A" name)
                      (ignore-rest-of-entry istream name))
                     ((eql next-char2 #\/)
                      (when in-default-p
                        (lkb-read-cerror 
                         istream 
                         "Double defaults when reading ~A" 
                         name)
                        (ignore-rest-of-entry istream name))
                      (check-for #\/ istream name) 
                      (let ((persist (lkb-read istream t)))
                        (append res1
                                (read-tdl-term istream 
                                               name path-so-far persist))))
                     (t res1)))))))

(defun read-tdl-term (istream name path-so-far in-default-p)
  ;;; Term -> Type | Feature-term | Diff-list | List | Coreference | Templ-call
  ;;; We can distinguish between these types of term
  ;;; by their initial characters:
  ;;; Feature-term       - [ - returns a list of path specs
  ;;; Diff-list and list - < - ditto
  ;;; Templ-call         - @ - ditto
  ;;; Coreference        - # - sets up global coref and returns nil
  ;;; Symbol-value       - ' - returns a list of one path=string unif
  ;;; Expanded-syntax    - ^ - not valid in type files (see below)
  ;;; Type               - anything else - returns a list of one path=type unif
  (let ((next-char (peek-char t istream nil 'eof)))
    (cond ((eql next-char 'eof) 
           (lkb-read-cerror istream
                            "Unexpected eof when reading ~A" name)
           (ignore-rest-of-entry istream name))
          ((eql next-char #\.) 
           (lkb-read-cerror istream 
                            "Missing term when reading ~A" name)
           (ignore-rest-of-entry istream name))
          ((eql next-char #\[) 
           (read-tdl-feature-term istream name path-so-far in-default-p))
          ((eql next-char #\<) 
           (read-tdl-list istream name path-so-far in-default-p))
          ((eql next-char #\#) 
           (read-tdl-coreference istream name path-so-far in-default-p))
          ((eql next-char #\@) 
           (read-tdl-templ-call istream name path-so-far in-default-p))
          ((eql next-char #\() 
           (read-tdl-lkb-disj istream name path-so-far in-default-p))
          ((eql next-char #\') 
           (read-tdl-symbol istream name path-so-far in-default-p))
          ((eql next-char #\^) 
           (if *tdl-expanded-syntax-function*
               (apply *tdl-expanded-syntax-function*
                      (list istream name path-so-far in-default-p))
             (progn (lkb-read-cerror 
                     istream 
                     "^ syntax used without expanded-syntax-function")
                    (format t "~%Treating as type")
                    (read-tdl-type istream name path-so-far in-default-p))))
  ;;; AAC - April 1998
  ;;; The idea is to allow the TDL reading code to be specialized
  ;;; for different applications by allowing the appropriate function to
  ;;; be called
          (t (read-tdl-type istream name path-so-far in-default-p)))))
          
(defun read-tdl-expanded-syntax (istream name path-so-far in-default-p)
  ;;; AAC - April 1998
  ;;; The idea is to allow the TDL reading code to be specialized
  ;;; for different applications by allowing this function to
  ;;; be redefined
  (lkb-read-cerror istream "^ syntax not allowed in type files")
  (format t "~%Treating as type")
  (read-tdl-type istream name path-so-far in-default-p))


(defun read-tdl-feature-term (istream name path-so-far in-default-p)
;;; Feature-term -> [] | [ Attr-val {, Attr-val}* ]
  (check-for #\[ istream name)
  (let ((next-char (peek-char t istream nil 'eof)))
    (cond ((eql next-char 'eof) 
           (lkb-read-cerror istream "Unexpected eof when reading ~A" name)
           (ignore-rest-of-entry istream name))
          ((eql next-char #\.) 
           (lkb-read-cerror istream 
                            "Missing attribute when reading ~A" name)
           (ignore-rest-of-entry istream name))
          ((eql next-char #\]) 
           (let ((res
                  (list (make-tdl-path-value-unif 
                         (reverse  path-so-far) *toptype* in-default-p))))
             (check-for #\] istream name)
             res))
          (t (let ((res 
                    (read-tdl-attr-vals istream name path-so-far in-default-p)))
               (check-for #\] istream name)
               res)))))

(defun read-tdl-attr-vals (istream name path-so-far in-default-p)
  (let* ((constraint nil))
    (loop
      (let* ((path-specs (read-tdl-attr-val istream name path-so-far in-default-p))
             (next-char (peek-char t istream nil 'eof)))
        (loop for path-spec in path-specs
             do
             (push path-spec constraint))
        (unless (eql next-char #\,) (return))
        (read-char istream)))
    constraint))

(defun read-tdl-attr-val (istream name path-so-far in-default-p)
  ;;; Attr-val -> attribute{.attribute}* Conjunction
  (loop
      (let* ((attribute (lkb-read istream nil))
             (next-char (peek-char t istream nil 'eof)))
        (when (char-equal (elt (string attribute) 0)
                          #\#)
          (lkb-read-cerror istream "Misplaced coreference in ~A" name)
          (ignore-rest-of-entry istream name))                         
        (push attribute path-so-far)
        (unless (eql next-char #\.) (return))
        (read-char istream)))
  (read-tdl-conjunction istream name path-so-far in-default-p))
    
  


(defun read-tdl-list (istream name path-so-far in-default-p)
;;; Diff-list -> <! !> | <! Conjunction {, Conjunction}* !>
;;; List -> < > | < Conjunction {, Conjunction}* > |
;;;                < Conjunction {, Conjunction}* , ...> |
;;;                 < Conjunction {, Conjunction}* . Conjunction> 
  (check-for #\< istream name)
  (let ((next-char (peek-char t istream nil 'eof))
        (constraints nil))
    (cond ((eql next-char 'eof) 
           (lkb-read-cerror istream "Unexpected eof when reading ~A" name)
           (ignore-rest-of-entry istream name))
          ((eql next-char #\!) 
           (read-char istream)
           (setf constraints 
             (read-tdl-diff-list istream name path-so-far in-default-p))
           (check-for #\! istream name)
           (check-for #\> istream name))
          (t (setf constraints  
               (read-tdl-non-diff-list istream name path-so-far in-default-p))
             (check-for #\> istream name)))
    constraints))

(defun read-tdl-non-diff-list (istream name path-so-far in-default-p)
;;; List -> < > | < Conjunction {, Conjunction}* > | < ... > |
;;;                < Conjunction {, Conjunction}* , ...> |
;;;                 < Conjunction {, Conjunction}* . Conjunction> 
;;; For example, the list
;;; < a, b >
;;; should give
;;; < FIRST > = a
;;; < REST : FIRST > = b
;;; < REST : REST > = *null*
;;;
;;; < >
;;; should give
;;; < > = *null*
;;;
;;; < ... >
;;; should give
;;; < > = *list*
;;;
;;; < a, b , ...>
;;; < FIRST > = a
;;; < REST : FIRST > = b
;;;
;;; < a, b . #coref >
;;; < FIRST > = a
;;; < REST : FIRST > = b
;;; < REST : REST > = #coref
;;;
  (let ((next-char (peek-char t istream nil 'eof))
        (constraints nil)
        (new-path (copy-list path-so-far)))
    (unless (or (eql next-char #\>) 
                 (eql next-char #\.))
      (loop 
        (setf constraints 
              (nconc 
               (read-tdl-conjunction 
                istream name (append *list-head* new-path) in-default-p)
               constraints))
        (setf next-char (peek-char t istream nil 'eof))
        (setf new-path
          (append *list-tail* new-path))
        (unless (eql next-char #\,) (return))
        (read-char istream)
        (setf next-char (peek-char t istream nil 'eof))
        (if (eql next-char #\.)
          (return))))
    (if (eql next-char #\.) 
      (progn (read-char istream)
             (setf next-char (peek-char t istream nil 'eof))
             (cond ((eql next-char #\.)                
                    (check-for #\. istream name)
                    (check-for #\. istream name)
                    (unless (eql (peek-char t istream nil 'eof) #\>)
                      (lkb-read-cerror 
                       istream
                       "Invalid syntax following ... in ~A~%" name)
                      (ignore-rest-of-entry istream name))
                    (push (make-tdl-path-value-unif (reverse new-path)
                                                    *list-type* in-default-p)
                          constraints))
               ;;; the single dot syntax only really makes sense
               ;;; if the next item is a coreference
                   (t (setf constraints 
                        (nconc 
                         (read-tdl-conjunction istream name new-path in-default-p)
                                   constraints)))))
      (push (make-tdl-path-value-unif (reverse new-path)
                                       *empty-list-type* in-default-p)
            constraints))
    constraints))

(defun read-tdl-diff-list (istream name path-so-far in-default-p)
;;; Diff-list -> <! !> | <! Conjunction {, Conjunction}* !>
;;; The interpretation of this structure is that the
;;; value of the attribute LIST is set to the list enclosed in
;;; the <! !> and the value of LAST is set to the end of this structure
;;; Thus when we read this stuff into the LKB we basically convert this
;;; to the usual path notation by adding LIST and the appropriate
;;; RESTs and FIRSTs to the path-so-far, and then add a coreference
;;; between the end of the LIST and the attribute LAST
;;; For example, the difference list
;;; <! a, b !>
;;; should give
;;; < LIST : FIRST > = a
;;; < LIST : REST : FIRST > = b
;;; < LIST : REST : REST > = < LAST >
;;; If the diff-list is empty (i.e. <! !>) then the 
;;; constraints should be simply
;;; < LIST > = < LAST >
  (let ((next-char (peek-char t istream nil 'eof))
        (constraints nil)
        (new-path (cons *diff-list-list* path-so-far)))
    (unless (eql next-char #\!) 
      (loop  
        (setf constraints 
              (nconc 
               (read-tdl-conjunction istream name 
                                     (append *list-head* new-path) 
                                     in-default-p)
               constraints))
        (setf next-char (peek-char t istream nil 'eof))
        (setf new-path
          (append *list-tail* new-path))
        (unless (eql next-char #\,) (return))
        (read-char istream)))
    (push (make-tdl-path-path-unif (reverse (cons *diff-list-last* path-so-far))
                                   (reverse new-path) in-default-p)
          constraints)
    constraints))


(defun read-tdl-coreference (istream name path-so-far in-default-p)
;;; Coreference -> #corefname
;;; because the LKB assumes a path = path encoding of coreference
;;; we have to store the coreference markers in a hash table
;;; and create path=path unifications when we have finished reading the
;;; structure
  (read-char istream) ; get rid of the hash
  (let ((corefindex (read istream nil 'end-of-file)))
    (when (eql corefindex 'end-of-file) 
      (lkb-read-cerror 
       istream 
       "Unexpected end of file when processing ~A" name)
      (ignore-rest-of-entry istream name))
    (unless (symbolp corefindex)
      (setf corefindex (convert-to-lkb-symbol corefindex)))
    (let ((true-path (reverse path-so-far)))
      (if in-default-p
          (push (cons in-default-p true-path)
                (gethash corefindex *tdl-default-coreference-table*))
        (push true-path
              (gethash corefindex *tdl-coreference-table*))))
    nil))



(defun read-tdl-symbol (istream name path-so-far in-default-p)
  (declare (ignore name))
  (read-char istream) ; get rid of the '
  (let* ((symbol (lkb-read istream t)))
      (list (make-tdl-path-value-unif (reverse path-so-far) 
                                      (format nil "~A" symbol) in-default-p))))


(defun read-tdl-type (istream name path-so-far in-default-p)
  (declare (ignore name))
  (let ((type (lkb-read istream t)))
    (list (make-tdl-path-value-unif (reverse path-so-far) type in-default-p))))


(defun read-tdl-lkb-disj (istream name path-so-far in-default-p)
  (declare (ignore name))
  (let ((types nil))
    (read-char istream)
    (loop
      (let ((next-char (peek-char t istream nil 'eof)))
        (when (char= next-char #\))
          (read-char istream)
          (return))
        (push (lkb-read istream t) types)))
    (list (make-tdl-path-value-unif (reverse path-so-far) types in-default-p))))


;;; Because there are a limited number of templates, instead of 
;;; creating a new file type etc so that they can be created as
;;; needed, they are all specified here.  
;;; A template has three parts:
;;; 1. name
;;; 2. constraint
;;; 3. list of parameter/path pairs
;;;
;;; the constraint is stored as a list of path/value pairs
;;; with the path specified as a simple list and the values being types
;;;
;;; the parameter/path pairs allow reference to be made to
;;; specific parts of the template without having to quote the full path name.
;;; Thus their effect is simply to add features to the value
;;; of path-so-far which are then utilised as normal when reading in 
;;; the conjunction

(defstruct (tdl-templ)
  name constraint parameters)

(defstruct (tdl-templ-pv)
  path value)

(defstruct (tdl-templ-parameter)
  name path)


(defparameter *tdl-templates* nil)

(defun clear-tdl-templates nil
  (setf *tdl-templates* nil))

(defun make-tdl-template (name pars cons)
  (push (make-tdl-templ :name name
                       :parameters (loop for par in pars 
                                  collect
                                  (make-tdl-templ-parameter :name (car par)
                                                           :path (cadr par)))
                       :constraint 
                       (loop for pv in cons
                            collect
                            (make-tdl-templ-pv :path (car pv)
                                               :value (cadr pv))))
        *tdl-templates*))


(defun read-tdl-templ-call (istream name path-so-far in-default-p)
;;; Templ-call -> @templ-name ( ) | @templ-name (Templ-par {, Templ-par}*)
;;; Templ-par -> $templ-var | $templ-var = conjunction
  (read-char istream) ; get rid of the @
  (let ((templ-name (read istream nil 'end-of-file)))
    (when (eql templ-name 'end-of-file)
      (lkb-read-cerror istream
       "Unexpected end of file when processing ~A" name)
      (ignore-rest-of-entry istream name))
    (unless (symbolp templ-name) 
      (setf templ-name (convert-to-lkb-symbol templ-name))) 
    (let ((template-entry (find templ-name *tdl-templates*
                                :key #'tdl-templ-name)))
      (unless template-entry 
        (lkb-read-cerror istream 
         "Unknown template ~A" templ-name)
        (ignore-rest-of-entry istream name))        
      (check-for #\( istream name)
      (let ((next-char (peek-char t istream nil 'eof))
            (constraints (get-tdl-template-constraints 
                          (tdl-templ-constraint template-entry)
                          path-so-far in-default-p)))
        (unless (eql next-char #\)) 
          (loop  
            (setf constraints 
                  (nconc 
                   (read-tdl-templ-par istream name path-so-far
                                       template-entry in-default-p)
                   constraints))
            (setf next-char (peek-char t istream nil 'eof))
            (unless (eql next-char #\,) (return))
            (read-char istream)))
        (check-for #\) istream name) 
        constraints))))

(defun get-tdl-template-constraints (template-constraint path-so-far in-default-p)
  (let ((rev-p (reverse path-so-far)))
    (loop for pv in template-constraint
         collect
         (make-tdl-path-value-unif
          (append rev-p (tdl-templ-pv-path pv))
          (tdl-templ-pv-value pv)
          in-default-p))))

(defun read-tdl-templ-par (istream name path-so-far template-entry in-default-p)
  ;;; Templ-par -> $templ-var | $templ-var = conjunction
  ;;; (actually I assume we always have
  ;;; Templ-par -> $templ-var = conjunction 
  ;;; because I'm not sure what the point of having the parameter
  ;;; without a value would be)
  (check-for #\$ istream name)
  (let* ((par-name (read istream nil 'end-of-file))
         (par-value 
          (find (if (symbolp par-name) 
                    par-name 
                  (convert-to-lkb-symbol par-name)) 
                (tdl-templ-parameters template-entry)
                :key #'tdl-templ-parameter-name)))
    (unless par-value 
      (lkb-read-cerror istream 
       "Unknown parameter ~A" par-name)
      (ignore-rest-of-entry istream name))
    (check-for #\= istream name)
    (read-tdl-conjunction 
     istream name 
     (append (reverse (tdl-templ-parameter-path par-value))
             path-so-far)
     in-default-p)))
     
             

(defun make-tdl-path-path-unif (path1 path2 def-p)
  ;;; def-p indicates the persistance of the default
  (let ((ps1 (create-path-from-feature-list path1))
        (ps2 (create-path-from-feature-list path2)))
    (if def-p
        (list def-p
              (make-unification :lhs ps1 :rhs ps2))
      (make-unification :lhs ps1 :rhs ps2))))

(defun make-tdl-path-value-unif (path1 type def-p)
  (let ((ps1 (create-path-from-feature-list path1))
        (val (make-u-value :type type)))
    (if def-p
        (list def-p
              (make-unification :lhs ps1 :rhs val))
      (make-unification :lhs ps1 :rhs val))))

