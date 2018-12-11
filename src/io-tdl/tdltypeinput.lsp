;;; Copyright (c) 1996-2018 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Benjamin Waldron
;;; see LICENSE for conditions

(in-package :lkb)

;;; 1997 - added status stuff so that we can read files without
;;; modification to get rid of this
;;; AAC 2007 - removed status stuff! and a load of other unused
;;; bits and pieces.  Allow ; comments within type definitions
;;; although they have to come after parents.

;;; Input from type files in (a subset of) TDL format
;;; the subset is determined by a) what should be implementable in
;;; the LKB system b) what is actually used currently

;;; Type specifications have the following syntax (in BNF):
;;; 
;;; Type-def -> Type { Avm-def | Subtype-def} . | 
;;;                  Type { Avm-def | Subtype-def}.
;;; Type  -> identifier
;;; Subtype-def ->  :< type 
;;; Avm-def -> := Conjunction | Comment Conjunction
;;; Conjunction -> Term { & Term } *
;;; Term -> Type | Feature-term | Diff-list | List | Coreference 
;;; Feature-term -> [] | [ Attr-val {, Attr-val}* ]
;;; Attr-val -> attribute {.attribute}* Conjunction
;;;   (I am fairly convinced that the TDL manual BNF form is wrong
;;;    wrt Attr-val)
;;; Diff-list -> <! !> | <! Conjunction {, Conjunction}* !>
;;; List -> < > | < Conjunction {, Conjunction}* > |
;;;                < Conjunction {, Conjunction}* , ...> |
;;;                 < Conjunction {, Conjunction}* . Conjunction> 
;;; Coreference -> #corefname
;;;
;;; Type-addendum -> Type Avm-addendum .
;;; Avm-addendum -> :+ Parents Conjunction | Parents Comment Conjunction |
;;;                    Parents | Parents Comment | Comment Conjunction |
;;;                    Comment | Conjunction
;;;
;;; special characters are
;;; . : < = & , # [ ] $ ( ) > ! ^
;;; ^ - added - indicates `expanded syntax'
;;; / - added - indicates default
;;; + - added - indicates type addendum; not a break character (ERB 2004-08-10)
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


(defparameter *tdl-expanded-syntax-function* nil)

(defun clear-tdl-contexts ()
  ;; JAC 28-Oct-2018 - used to set *tdl-context* and *tdl-all-contexts* to nil (set
  ;; in read-semicolon-comment and picked up in add-type-from-file), but that
  ;; facility has now been subsumed by docstrings
  )

(defun make-tdl-break-table nil 
  ;; JAC 28-Oct-2018 - these break characters are no longer necessary for TDL reading, but
  ;; left for now since they might be necessary for reading other kinds of definitions 
  (define-break-characters '(#\< #\> #\! #\= #\: #\. #\# #\&
                             #\, #\[ #\] #\; #\$ #\( #\) #\^ #\/)))


(defun read-tdl-type-file-aux (file-name &optional settings-file)
  (read-tdl-type-files-aux (list file-name) settings-file))

(defun read-tdl-type-files-aux (file-names &optional settings-file)
  (unless (listp file-names)
    (setf file-names (list file-names)))
  (when settings-file
    (setf *display-settings-file* settings-file))
  (setf *type-file-list* file-names)
  (clear-types)
  (add-type-from-file *toptype* nil nil nil nil)
  (dolist (file-name file-names)
    (format t "~%Reading in type file ~A" (pathname-name file-name))
    (force-output)
    (with-open-file (istream file-name :direction :input)
      (read-tdl-type-stream istream))) 
  (if *syntax-error*
      (progn
        (setf *syntax-error* nil)
        (cerror "Continue loading script with incomplete type information"
                "Syntax error(s) in type file")
        nil)
      (let ((ok 
              (when (check-type-table) 
                (canonicalise-feature-order)
                (when settings-file
                  (set-up-display-settings settings-file))
                (set-up-type-interactions)
	        (when (eql *morph-option* :distinct-mphon)
		  (extract-affixation-specifications))
                t)))
        (unless ok
          (cerror "Continue loading script with incompletely processed types"
                  "Problems in type file")))))

(defun read-tdl-leaf-type-file-aux (filenames)
  (read-general-leaf-type-files-aux filenames))
  
(defmethod read-tdl-leaf-type-file-aux-internal (filename)
  (read-general-leaf-type-file-aux-internal filename))


(let ((last-hash-position nil))
(defun peek-with-comments (istream)
  ;; An extension of (peek-char t istream nil 'eof), skipping over not only whitespace
  ;; but also LineComments (;).
  ;; In addition, on encountering a # character, consume a BlockComment (#| |#) if
  ;; it is there. This involves reading the # and peeking the following character.
  ;; If that character is not |, then return the # character and ensure that subsequent
  ;; calls to this function without any intervening read also return #.
  ;; !!! This means that if this function returns #, the character has already been
  ;; read, so callers must allow for the fact that in this case the function has done
  ;; more than a peek.
  (when (and last-hash-position
             (= last-hash-position (file-position istream)))
    (return-from peek-with-comments #\#))
  (setq last-hash-position nil)
  (loop
    (let ((next-char (peek-char t istream nil 'eof)))
      (cond
        ((eql next-char #\#)
          (read-char istream) ; consume the #
          (if (eql (peek-char nil istream nil 'eof) #\|)
            (read-tdl-comment istream)
            (progn
              (setq last-hash-position (file-position istream))
              (return #\#))))
        ((eql next-char #\;)
          (peek-char #\newline istream nil)) ; skip rest of line
        (t
          (return next-char))))))
)

(defun read-tdl-comment (istream)
  ;; consume a block (multiline) comment, having just read a # and peeked a |
  ;; does not allow block comments to be nested, so first |# terminates
  (let ((start-position (1- (file-position istream))))
    (read-char istream) ; consume the |
    (loop 
      for c = (read-char istream nil 'eof)
      do
      (case c
        (eof
          (lkb-read-cerror istream 
            "Terminating |# not found for block comment starting on line ~A"
            (prog2 (file-position istream start-position)
              (lkb-read-line/char-numbers istream)
              (file-position istream :end)))
          (return))
        (#\\
          ;; prevent end of commment interpretation of an immediately following |#
          (read-char istream nil))
        (#\|
          (when (eql (read-char istream nil) #\#) (return)))))))


;;; Main entry points for TDL reading

(defun read-tdl-type-stream (istream &optional augment) 
  (loop
    (if (eq (peek-with-comments istream) 'eof)
      (return)
      (catch 'syntax-error
        (read-tdl-type-entry istream augment)))))


(defun read-tdl-type-entry (istream &optional augment)
  ;; TypeDef -> Type { DefOp | AddOp } TypedDefBody Dot
  ;; Type -> Identifier Spacing
  ;; DefOp -> ":=" Spacing
  ;; AddOp -> ":+" Spacing
  (let* (#+(or :allegro :mcclim)
         (position nil) ; JAC - unused, was (1+ (file-position istream))
         (name (lkb-read istream nil))
         (existingp (gethash name *types*)))
    ;; JAC 2018-10-15: removed check for name starting with %
    #+(or :allegro :mcclim) (record-source name istream position)
    (unless (eql (peek-with-comments istream) #\:)
      (lkb-read-cerror
        istream ":= or :+ expected but not found following type name ~A" name)
      (ignore-rest-of-entry istream name))
    (read-char istream)
    (let ((next (read-char istream nil 'eof)))
      (case next
        ((#\= #\<)
          ;; in november 2008, following an email discussion on the `developers' 
          ;; list, we decided to treat `:<' (originally a sub-type definition
          ;; without local constraints) as a syntactic variant of `:='.  i apply
          ;; the corresponding change to PET today.               (29-nov-08; oe)
          ;; JAC 2018-09-07: on 'developers' list, we decided to remove the :< operator
          ;; (or if accepted as a variant of :=, throw a warning).
          (when (eql next #\<)
            (format t "~%WARNING: After type name ~A, deprecated operator :< treated as :="
              name)
            (force-output))
          (read-tdl-avm-def istream name augment)
          (check-for #\. istream name))
        (#\+
          ;; ERB 2004-08-10 New case to allow for addition of information to
          ;; existing types, dubbed a `type addendum'.
          (read-tdl-avm-def istream name 'add-info)
          (check-for #\. istream name))
        (t
          (lkb-read-cerror
            istream ":= or :+ expected but not found following type name ~A" name)
          (ignore-rest-of-entry istream name))))
    (unless existingp
      (push name *ordered-type-list*))))

(defparameter *tdl-coreference-table* (make-hash-table))
(defparameter *tdl-default-coreference-table* (make-hash-table))

;;; ERB (2004-08-10) Allow users to add information to a type
;;; defined previously.  Must be monotonic addition of information.
;;; Parents, conjunctions, comments can all be added.
;;; In this case the "augment" parameter has value add-info.

;;; JAC Sep 2018: rewritten to reflect TdlRfc cleanup, although note that for
;;; convenience some of the BNF clauses have been restructured here.

(defun read-tdl-avm-def (istream name &optional augment)
  ;; TypedDefBody -> TopConj - under the constraint that unless augment = add-info
  ;; (for AddOp :+) there must be at least one Term and at least one of them
  ;; must be a Type (i.e. at least one parent has been specified)
  (clrhash *tdl-coreference-table*)
  (clrhash *tdl-default-coreference-table*)
  (let ((comment nil) (constraint nil) (parents nil) (def-alist nil))
    (multiple-value-bind (top-conj c)
                         (read-tdl-top-conjunction istream name)
      (setq comment c)
      (dolist (unif top-conj)
        (cond ((unification-p unif)                   
                ;; for the special case of a path constraint with an empty path, we
                ;; extract the type and instead put it on the parents list
                (if (path-typed-feature-list (unification-lhs unif))
                    (push unif constraint)
                    (push (u-value-type (unification-rhs unif)) parents)))
              ((consp unif)
                (let ((entry (assoc (car unif) def-alist)))
                  (if entry
                      (push (cadr unif) (cdr entry))
                      (push unif def-alist))))
              (t (error "Inconsistency in read-tdl-avm-def: unexpected unif in ~A" name))))
      (dolist (coref
                (make-tdl-coreference-conditions istream *tdl-coreference-table* nil))
        (push coref constraint))
      (dolist (coref
                (make-tdl-coreference-conditions istream *tdl-default-coreference-table* t))
        (let ((entry (assoc (car coref) def-alist)))
          (if entry
              (push (cadr coref) (cdr entry))
              (push coref def-alist)))))
    (cond
      ((eql augment 'add-info)
        (add-info-to-type-from-file name parents constraint def-alist comment))
      (augment
        (amend-type-from-file name parents constraint def-alist comment)
        (setf *amend-error* t))
      (parents
        (add-type-from-file name parents constraint def-alist comment))
      (t
        (lkb-read-cerror istream "No parents found in type definition ~A" name)
        (ignore-rest-of-entry istream name)))))


(defun read-tdl-top-conjunction (istream name)
  ;; TopConj -> DocString | { DocString? DefTerm { And DocString? DefTerm }* DocString? } 
  ;; returns a list of path constraints and a (possibly merged) type comment docstring
  (let ((comment nil))
    (when (eql (peek-with-comments istream) #\")
      ;; NB this could in principle be an attempt at a DQString Term - but that would
      ;; make no sense at the top level of a type definition so assume it's intended
      ;; to be a DocString
      (setq comment (read-tdl-type-comment istream name nil)))
    (if (member (peek-with-comments istream) '(#\. eof))
      (progn
        (unless comment
          ;; the body must at least contain a DocString
          (lkb-read-cerror istream "Definition of ~A is empty" name))
        (values nil comment))
      (let ((constraint nil))
        (loop
          (setq constraint 
            (nconc (read-tdl-defterm istream name nil nil) constraint))
          (if (eql (peek-with-comments istream) #\&)
              (progn
                (read-char istream)
                (when (eql (peek-with-comments istream) #\") ; same ambiguity as above
                  (setq comment
                    (read-tdl-type-comment istream name comment))))
              (return)))
        (when (eql (peek-with-comments istream) #\")
          (setq comment (read-tdl-type-comment istream name comment)))
        (values constraint comment)))))    


(defun read-tdl-type-comment (istream name &optional comment)
  ;; Read a DocString (i.e. """...""" as in Python) - called when we've just
  ;; peeked a "
  (let ((start-position (file-position istream)))
    ;; first read the peeked double quote, and then the only legal following characters
    ;; are two more double quotes
    (read-char istream)
    (if (and (eql (peek-char nil istream nil nil) #\")
             (read-char istream)
             (eql (peek-char nil istream nil nil) #\")
             (read-char istream))
      ;; accumulate characters until encountering 3 consecutive non-escaped double quotes
      ;; NB this is not a general Python string reader: only \<newline>, \\ and \" are
      ;; recognised as escape sequences
      (loop 
        for c = (read-char istream nil 'eof)
        with ndouble = 0
        with chars = nil
        do
        (block do
          (case c
            (eof
              (lkb-read-cerror istream 
                "Non-terminated documentation string for ~A (starting on line ~A)"
                name
                (prog2 (file-position istream start-position)
                  (lkb-read-line/char-numbers istream)
                  (file-position istream :end)))
              (return ""))
            (#\\
              (setq ndouble 0)
              (case (peek-char nil istream nil 'eof)
                (#\newline (read-char istream nil 'eof) (return-from do))
                ((#\\ #\") (setq c (read-char istream nil 'eof)))))
            (#\"
              (incf ndouble)
              (when (= ndouble 3) (loop-finish)))
            (t
              (setq ndouble 0)))
          (push c chars))
        finally
        (return
          (let ((new (coerce (nreverse (cddr chars)) 'string))) ; last 2 chars were pre-final "s
            (if comment
                (concatenate 'string comment (string #\newline) new)
                new))))
      (progn
        (lkb-read-cerror istream 
          "In ~A, expected a documentation string after double quote character, but did not ~
find two further double quotes" 
          name)
        (ignore-rest-of-entry istream name)))))

(push :triple-quoted-typedoc *features*)


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
             (next-char (peek-with-comments istream)))
        (setf constraint 
              (nconc term constraint))
        (unless (eql next-char #\&) (return))
        (read-char istream)))
    constraint))

(defun read-tdl-defterm (istream name path-so-far in-default-p)
;;; DefTerm -> Term | Term / Term | / Term  
  (let ((next-char (peek-with-comments istream)))
    (cond ((eql next-char 'eof) 
           (lkb-read-cerror istream 
                            "File ended in middle of definition of ~A" name)
           (ignore-rest-of-entry istream name))
          ((eql next-char #\.) 
           (lkb-read-cerror istream "Missing term when reading ~A" name)
           (ignore-rest-of-entry istream name))
          ((eql next-char #\/)
           (when in-default-p
             (lkb-read-cerror istream
                              "Double defaults when reading ~A" name)
             (ignore-rest-of-entry istream name))
           (read-char istream) ; consume the /
           (let ((persist (lkb-read istream t)))
             (if path-so-far
                 (cons
                  (make-tdl-path-value-unif (reverse path-so-far) *toptype* nil)
                  ;; need to add non-default path too
                  (read-tdl-term istream name path-so-far persist))
              (read-tdl-term istream name path-so-far persist)))) 
          (t  
           (let* ((res1 (read-tdl-term istream name path-so-far in-default-p))
		  (next-char2 (peek-with-comments istream)))
               (cond ((eql next-char2 'eof) 
                      (lkb-read-cerror istream
                                       "File ended in middle of definition of ~A" name)
                      (ignore-rest-of-entry istream name))
                     ((eql next-char2 #\/)
                      (when in-default-p
                        (lkb-read-cerror 
                         istream 
                         "Double defaults when reading ~A" 
                         name)
                        (ignore-rest-of-entry istream name))
                      (read-char istream) ; consume the /
                      (let ((persist (lkb-read istream t)))
                        (append res1
                                (read-tdl-term istream 
                                               name path-so-far persist))))
                     (t res1)))))))

(defun read-tdl-term (istream name path-so-far in-default-p)
  ;; Term -> Type | Feature-term | Diff-list | List | Coreference 
  ;; We can distinguish between these types of term
  ;; by their initial characters:
  ;; Feature-term       - [ - returns a list of path specs
  ;; Diff-list and list - < - ditto
  ;; Coreference        - # - sets up global coref and returns nil
  ;; Symbol-value       - ' - not legal syntax any more
  ;; Expanded-syntax    - ^ - not valid in type files (see below)
  ;; Type               - anything else - returns a list of one path=type unif
  (let ((next-char (peek-with-comments istream)))
    (cond ((eql next-char 'eof) 
           (lkb-read-cerror istream
                            "File ended unexpectedly when reading ~A" name)
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
          ((eql next-char #\') 
           (lkb-read-cerror istream
"In ~A, found a single quote, which is no longer a valid notation for strings"
             name)
           (ignore-rest-of-entry istream name))                         
          ((eql next-char #\^) 
           (if *tdl-expanded-syntax-function*
               (apply *tdl-expanded-syntax-function*
                      (list istream name path-so-far in-default-p))
             ;; AAC - April 1998
             ;; The idea is to allow the TDL reading code to be specialized
             ;; for different applications by allowing the appropriate function to
             ;; be called
             (progn (lkb-read-cerror 
                     istream 
                     "^ syntax used without expanded-syntax-function")
                    (format t "~%Proceeding ignoring ^~%")
                    (read-char istream)
                    (read-tdl-type istream name path-so-far in-default-p))))
          (t
            (read-tdl-type istream name path-so-far in-default-p)))))
          
(defun read-tdl-expanded-syntax (istream name path-so-far in-default-p)
  ;;; AAC - April 1998
  ;;; The idea is to allow the TDL reading code to be specialized
  ;;; for different applications by allowing this function to
  ;;; be redefined
  (lkb-read-cerror istream "^ syntax not allowed in type files")
  (format t "~%Proceeding ignoring ^~%")
  (read-char istream)
  (read-tdl-type istream name path-so-far in-default-p))


(defun read-tdl-feature-term (istream name path-so-far in-default-p)
;;; Feature-term -> [] | [ Attr-val {, Attr-val}* ]
  (check-for #\[ istream name)
  (let ((next-char (peek-with-comments istream)))
    (cond ((eql next-char 'eof) 
           (lkb-read-cerror istream "File ended unexpectedly when reading ~A" name)
           (ignore-rest-of-entry istream name))
          ((eql next-char #\.) 
           (lkb-read-cerror istream 
                            "Missing attribute when reading ~A" name)
           (ignore-rest-of-entry istream name))
          ((eql next-char #\]) 
           (read-char istream) ; consume the ]
           (list
             (make-tdl-path-value-unif
               (reverse path-so-far) *toptype* in-default-p)))
          (t (let ((res 
                    (read-tdl-attr-vals istream name path-so-far in-default-p)))
               (check-for #\] istream name)
               res)))))

(defun read-tdl-attr-vals (istream name path-so-far in-default-p)
  ;; AttrVals := AttrVal ( COMMA AttrVal )*
  (let ((constraint nil))
    (loop
      (setq constraint
        (nconc (read-tdl-attr-val istream name path-so-far in-default-p) constraint))
      (if (eql (peek-with-comments istream) #\,)
        (read-char istream)
        (return)))
    constraint))

(defun read-tdl-attr-val (istream name path-so-far in-default-p)
  ;; AttrVal := AttrPath SPACE Conjunction
  ;; AttrPath := Attribute ( DOT Attribute )*
  ;; Attribute := Identifier Spacing
  (peek-with-comments istream)
  (loop
    (let ((attribute (lkb-read istream nil)))
      (when (char-equal (char (string attribute) 0) #\#)
        (lkb-read-cerror istream "Misplaced coreference or block comment in ~A" name)
        (ignore-rest-of-entry istream name))                         
      (push attribute path-so-far)
      (if (eql (peek-with-comments istream) #\.)
        (read-char istream)
        (return))))
  (read-tdl-conjunction istream name path-so-far in-default-p))  


(defun read-tdl-list (istream name path-so-far in-default-p)
;;; Diff-list -> <! !> | <! Conjunction {, Conjunction}* !>
;;; List -> < > | < Conjunction {, Conjunction}* > |
;;;                < Conjunction {, Conjunction}* , ...> |
;;;                 < Conjunction {, Conjunction}* . Conjunction> 
  (check-for #\< istream name)
  (let ((next-char (peek-char nil istream nil 'eof))
        (constraints nil))
    (cond ((eql next-char 'eof) 
           (lkb-read-cerror istream "File ended unexpectedly when reading ~A" name)
           (ignore-rest-of-entry istream name))
          ((eql next-char #\!) 
           (read-char istream)
           (setf constraints 
             (read-tdl-diff-list istream name path-so-far in-default-p))
           (check-for-string "!>" istream name))
          (t
           (setf constraints  
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
  (let ((next-char (peek-with-comments istream))
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
        (setf next-char (peek-with-comments istream))
        (setf new-path
          (append *list-tail* new-path))
        (unless (eql next-char #\,) (return))
        (read-char istream)
        (setf next-char (peek-with-comments istream))
        (if (eql next-char #\.)
          (return))))
    (if (eql next-char #\.) 
      (progn (read-char istream)
             (cond ((eql (peek-char nil istream nil 'eof) #\.)                
                    (check-for-string ".." istream name) ; remainder of ellipsis
                    (unless (eql (peek-with-comments istream) #\>)
                      (lkb-read-cerror 
                        istream "> expected but not found after ... in ~A" name)
                      (ignore-rest-of-entry istream name))
                    (push (make-tdl-path-value-unif (reverse new-path)
                                                    *list-type* in-default-p)
                          constraints))
               ;;; the single dot syntax only really makes sense
               ;;; if the next item is a coreference
                   (t (peek-with-comments istream)
                      (setf constraints 
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
  (let ((next-char (peek-with-comments istream))
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
        (setf next-char (peek-with-comments istream))
        (setf new-path
          (append *list-tail* new-path))
        (unless (eql next-char #\,) (return))
        (read-char istream)))
    (push (make-tdl-path-path-unif (reverse (cons *diff-list-last* path-so-far))
                                   (reverse new-path) in-default-p)
          constraints)
    constraints))


(defun read-tdl-coreference (istream name path-so-far in-default-p)
  ;; Coreference -> #corefname
  ;; because the LKB assumes a path = path encoding of coreference
  ;; we have to store the coreference markers in a hash table
  ;; and create path=path unifications when we have finished reading the
  ;; structure
  ;; !!! In contrast to the surrounding functions, the introducing hash character
  ;; has already been consumed (by peek-with-comments, in order to check whether the
  ;; character was in fact the start of a block comment) so don't try to read it now.
  (let ((corefindex (lkb-read istream nil))
        (true-path (reverse path-so-far)))
    (if in-default-p
        (push (cons in-default-p true-path)
              (gethash corefindex *tdl-default-coreference-table*))
        (push true-path
              (gethash corefindex *tdl-coreference-table*)))
    nil))


(defun read-tdl-type (istream name path-so-far in-default-p)
  (declare (ignore name))
  (let ((type (lkb-read istream t)))
    (list (make-tdl-path-value-unif (reverse path-so-far) type in-default-p))))


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

