(in-package :cl-user)

#|
to convert an LKB grammar
load in the grammar as usual

check the feature names in this file are set correctly

(output-types :lilfes "~aac/lilfes/lingo/types.lil" t)
(output-lex-and-derived :lilfes "~aac/lilfes/lingo/lex.lil")
or 
(output-lex-and-derived :lilfes "~aac/lilfes/lingo/lex.lil" *lex-ids-used*)

this outputs a full form lexicon, with all lexical rules applied
so morphology and lexical rules can be ignored

optional argument is a list of lexical ids - this is set by 
batch parsing

(output-grules :lilfes "~aac/lilfes/lingo/grules.lil")

(output-root :lilfes "~aac/lilfes/lingo/root.lil")

to run lilfes (in the lilfes directory)

setenv LD_LIBRARY_PATH /eo/e5/danf/nishiken/bin

lilfes defs.lil types.lil lex.lil grules.lil root.lil test.lil -

This doesn't deal with constraints on types which don't
end up in the full form lexicon / grammar rules.

Parse nodes need to be added so we can understand the display.


|#

;;; very preliminary

;;; types can be in '' in the LilFeS syntax
;;; in LilFeS 0.71, so can features

(defun convert-lilfes-type (type)
  (string-downcase
   (cond ((eq type *list-type*) 'list)
         ((eq type *empty-list-type*) nil) ; the type is called nil
         ((eq type 'ne-list) 'cons)     ; grammar specific
         ((eq type '*cons*) 'cons)
         ((eq type *toptype*) 'bot)
         ((eq type *string-type*) 'string)
         ((eq type 'true) 'tru)
         ((eq type 'list-of-synsem-structs) 'list)
         ((eq type 'list-ssm-strs) 'list)        
;         ((eq type 'list-of-predications) '*diff-list*)
;         ((eq type 'list-of-orths) '*diff-list*)
         ((eq type 'ne-list-of-synsem-structs) 'cons)
         ((eq type 'ne-lst-ssm-strs) 'cons)         
         ((eq type 'ne-list-of-anything) 'cons)
         ((eq type 'ne-list-any) 'cons)
         ;;; following are for the textbook grammar
         ((eq type '-) 'minus)
         ((eq type '+) 'plus)
         ((eq type 'symbol) 'string)
         ;;; these are for LinGO
         ((eq type 'follow) 'lingo_follow)
         ((eq type 'integer) 'lingo_integer)
         (t type))))

(defun convert-lilfes-feature (feat)
  (cond ((eq feat (car *list-head*)) "hd")
        ((eq feat (car *list-tail*)) "tl")
        (t (convert-iffy-characters feat))))
        

(defun convert-iffy-characters (val)
  #|
  ;;; for LilFeS 0.71 just escape the feature
  (format nil "'~A'" val))
  |#
  (let ((str (string val))
          (char-bag nil))
      (for char in (coerce str 'list)
           do
           (cond ((char= char #\-)
                  (setf char-bag 
                        (append (nreverse (coerce "HYPHEN" 'list))
                                char-bag)))
                 ((char= char #\*)
                  (setf char-bag 
                        (append (nreverse (coerce "ASTERIX" 'list))
                                char-bag)))
                 ((digit-char-p char) 
                  (setf char-bag 
                       (append 
                        (nreverse 
                         (coerce 
                          (string-upcase
                          (format nil "~R" 
                                         (- (char-code char) 48))) 'list))
                        char-bag)))
                 ;;; alas, cannot use ~:@R here, because 0
                 ;;; hadn't been invented then ...
                 (t (push char char-bag))))
      (coerce (nreverse char-bag) 'string)))

  
(defparameter *lilfes-builtins*
  '("list" "nil" "cons" "bot" "string"))
    
(defun output-type-as-lilfes (name type-struct stream sig-only-p)
  (let* ((def (type-local-constraint type-struct))
         (parents (type-parents type-struct))
         (lilfes-name (convert-lilfes-type name)))
    (unless 
      (member lilfes-name *lilfes-builtins* :test #'equal)
      ;; don't redefine LiLFeS built in types
      (format stream "~%'~A' <- " lilfes-name)
      (format stream "['~A'" (convert-lilfes-type (car parents)))
      (for parent in (cdr parents)
           do
           (format stream ", '~A'" (convert-lilfes-type parent)))
      (format stream "]")
      (when def
        (if sig-only-p
          (display-lilfes-signature name def stream)
          ;;; non-sig-only-p version isn't correct yet
          ;;; not sure what to do with constraints
          (display-dag1 def 'lilfes stream nil t)))
      (format stream "."))))

(defun display-lilfes-signature (type def stream)
  (let ((feats (for feat in (top-level-features-of def)
                    filter
                    (if (eq (maximal-type-of feat) type)
                        feat))))
    (when feats
      (format stream " +~%[")
      (output-lilfes-fv-pair (car feats) 
                             (type-of-fs (get-dag-value def (car feats)))
                             stream)
      (for feat in (cdr feats) 
           do
           (format stream ",~%")
           (output-lilfes-fv-pair feat
                                  (type-of-fs (get-dag-value def feat))
                                  stream))
      (format stream "]"))))

(defun output-lilfes-fv-pair (feat value stream)
    (format stream "~A\\'~A'" (convert-lilfes-feature feat)
            (convert-lilfes-type (if (listp value) (car value)
                                        value))))

(defun output-instance-as-lilfes (name entry stream &optional class)
  (let ((def (tdfs-indef (lex-or-psort-full-fs entry))))
    ;; assume no defaults
    ;; assume either a grammar rule, in which case
    ;; we want to know unary or binary
    ;; or a lexical entry
    (if
      (rule-p entry)
      (let ((order-length (length (rule-order entry))))
        (cond ((eql order-length 2)
               (format stream "~%id_schema(\"~A\", " name)
               (output-lilfes-spec stream entry 1))
              ((eql order-length 3)
               (format stream "~%binary_rule(\"~A\", " name)
               (output-lilfes-spec stream entry 2))
              (t (error "Rule order in ~A is ~A: only unary or binary expected"
                        name order-length))))
      (if (eql class :root)
          (format stream "~%root(")
        (format stream "~%lex(\"~A\", " name)))
    (display-dag1 def 'lilfes stream)
    (format stream ").~%")))

(defun output-lilfes-spec (stream entry order)
  (if (eql order 1)
      (format stream "unary_rule \& ARC_DTR\[HEAD-DTR\]~%")
    (progn 
      (format stream "binary_rule \& NH_DIR\\~A &~%" 
              (lilfes-rule-order entry))  
      (format stream  "ARC_DTR\[HEAD-DTR\] &~%INP_DTR\[NON-HEAD-DTR"))))


(defun output-derived-instance-as-lilfes (string fs stream id1 id2)
  (let ((def (tdfs-indef fs)))
    ;; assume no defaults
    (format stream "~%lexical_entry(\"string\", " string)
    (display-dag1 def 'lilfes stream)
    (format stream ").~%")))                 
