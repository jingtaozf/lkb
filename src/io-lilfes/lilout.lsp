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
batch parsing.  The idea is to parse a (small) set of sentences and
then only output the ids needed for those sentences

(output-grules :lilfes "~aac/lilfes/lingo/grules.lil")

(output-root :lilfes "~aac/lilfes/lingo/root.lil")

to run lilfes (in the lilfes directory)

setenv LD_LIBRARY_PATH /eo/e5/danf/nishiken/bin

lilfes defs.lil types.lil lex.lil grules.lil root.lil test.lil -

This doesn't deal with constraints on types which don't
end up in the full form lexicon / grammar rules.

Parse nodes need to be added so we can understand the display.


|#

(defun get-directory-name (dir)
  (if (eq #\/ (car (last (coerce dir 'list))))
      dir
      (concatenate 'string dir "/")))

(defun output-all-for-lilfes (&optional dir)
  (setf dir (cond (dir (get-directory-name dir))
		  ((sys:getenv "LKB_OUTPUT_DIR")
		   (get-directory-name (sys:getenv "LKB_OUTPUT_DIR")))
		  (t "./")))
  (output-types :lilfes (concatenate 'string dir "types.lil") t)
  (output-lex-and-derived :lilfes (concatenate 'string dir "lex.lil"))
  (output-grules :lilfes (concatenate 'string dir "grules.lil"))
  (output-root :lilfes (concatenate 'string dir "root.lil")))

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
         ;;; following are for the textbook grammar
         ((eq type '-) 'minus)
         ((eq type '+) 'plus)
         ((eq type 'symbol) 'string)
         ;;; these are for LinGO
         ((eq type 'follow) 'lingo_follow)
         ((eq type 'integer) 'lingo_integer)
         ((eq type 'rule) 'lingo_rule)
         (t type))))

(defun convert-lilfes-feature (feat)
  (cond ((eq feat (car *list-head*)) "hd")
        ((eq feat (car *list-tail*)) "tl")
        ((eq feat 'ARG1) "'LINGO_ARG1'")
        ((eq feat 'ARG2) "'LINGO_ARG2'")
        ((eq feat 'ARG3) "'LINGO_ARG3'")
        (t (convert-iffy-characters feat))))
        

(defun convert-iffy-characters (val)
  ;;; for LilFeS 0.71 just escape the feature
  (format nil "'~A'" val))
  
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
      (format stream ".")
      (when (equal lilfes-name "0-1-list")
	(format stream "~%'0-1-list-nil' <- ['0-1-list', 'nil'].")))))

(defun display-lilfes-signature (type def stream)
;;; FIX - this needs to be altered for the new version
;;; of lilfes
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
  (when entry
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
               (output-lilfes-spec stream def 1 name))
              ((eql order-length 3)
               (format stream "~%id_schema(\"~A\", " name)
               (output-lilfes-spec stream def 2 name))
              (t (error "Rule order in ~A is ~A: only unary or binary expected"
                        name order-length)))
	(format stream ",~%")
	(display-dag1 def 'lilfes stream)
	(format stream ",true, true).~%"))
      (progn (if (eql class :root)
		 (format stream "~%root(")
	       (format stream "~%lex(\"~A\", " name))
	     (display-dag1 def 'lilfes stream)
	     (format stream ").~%"))))))


(defun output-lilfes-spec (stream def order name)
  (if (eql order 1)
      (format stream "~%unary_rule \& ARC_DTR\\['HEAD-DTR'\\]")
    (if (eql order 2)          
	(progn 
	  (format stream "~%binary_rule \& NH_DIR\\~A &~%" 
		  (lilfes-rule-order def name))  
	  (format stream "ARC_DTR\\['HEAD-DTR'\\] &~%INP_DTR\\['NON-HEAD-DTR'\\]"))
      (error "Unrecognised order ~A in ~A" order name))))

(defun lilfes-rule-order (rule-fs name)
  (let ((nh-dtr (existing-dag-at-end-of rule-fs '(NON-HEAD-DTR)))
	(h-dtr (existing-dag-at-end-of rule-fs '(HEAD-DTR))))
    (when (and nh-dtr (not (eql nh-dtr 'no-way-through))
	       h-dtr (not (eql h-dtr 'no-way-through)))
	  (let ((first-dtr (existing-dag-at-end-of rule-fs '(ARGS FIRST))))
	    (unless first-dtr
		    (error "No first dtr in ~A" name))
	    (cond ((and (eq first-dtr nh-dtr) 
			(not (eq first-dtr h-dtr))) "left")
		  ((and (eq first-dtr h-dtr)
			(not (eq first-dtr nh-dtr))) "right")
		  (t (error "~A has problems" name)))))))
  

(defun output-derived-instance-as-lilfes (string fs stream)
  (let ((def (tdfs-indef fs)))
    ;; assume no defaults
    (format stream "~%lexical_entry(\"~A\", " string)
    (display-dag1 def 'lilfes stream)
    (format stream ").~%")))                 


