;;; Copyright (c) 1998-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions


(in-package :lkb)

#|
to convert an LKB grammar
load in the grammar as usual (if outputting an entire lexicon, note that
the lexicon must not be cached)

check the feature names in this file are set correctly

parse a test suite so that the leaf types are loaded
and *lex-ids-used* is set.  

(output-types :lilfes "~aac/lilfes/lingo/types.lil")
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




Parse nodes need to be added so we can understand the display.

|#

(defun get-directory-name (dir)
  (if (eq #\/ (car (last (coerce dir 'list))))
      dir
    (concatenate 'string dir "/")))

(defun parse-test-suite-for-lilfes (&optional (input "~/tmp/test-sentences")
                                              output)
  ;;; start off with a cleanly loaded grammar, then
  (setf *unify-debug-cycles* t)
  (setf *first-only-p* nil)
  (if (and (find-package "TSDB") (find-symbol "TSDB" :tsdb)
           (fboundp (find-symbol "TSDB" :tsdb)))
    (funcall (fboundp (find-symbol "TSDB" :tsdb)) :process input)
    (parse-sentences input output)))  

(defun output-all-for-lilfes (&optional dir)
  (setf dir (cond (dir (get-directory-name dir))
#+:allegro        ((sys:getenv "LKB_OUTPUT_DIR")
		   (get-directory-name (sys:getenv "LKB_OUTPUT_DIR")))
		  (t "./")))
  (output-types :lilfes (concatenate 'string dir "types.lil"))
  (output-lex-and-derived :lilfes (concatenate 'string dir "lex.lil")
                          *lex-ids-used*)
  (output-grules :lilfes (concatenate 'string dir "grules.lil"))
  (output-root :lilfes (concatenate 'string dir "root.lil")))

;;; very preliminary

;;
;; _hack_
;; since the LiLFeS encoding for lexical entries only has slots for the surface
;; form (spelling) and the feature structure itself, the output code has to add
;; unique identifiers to the structure.  if the structure has this path, then
;; it will be enriched to point to a unique identifier.       (2-jun-99  -  oe)
;;

(defvar *lexical-id-path* '(--IDENTIFIER))

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
	 ((eq type 'atom) 'string)
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
        ((eq feat 'ARG4) "'LINGO_ARG4'")
        (t (convert-iffy-characters feat))))
        

(defun convert-iffy-characters (val)
  ;;; for LilFeS 0.71 just escape the feature
  (format nil "'~A'" val))
  
(defparameter *lilfes-builtins*
  '("list" "nil" "cons" "bot" "string"))
    
(defun output-type-as-lilfes (name type-struct stream)
  (let* ((def (type-local-constraint type-struct))
         (parents (type-parents type-struct))
         (lilfes-name (convert-lilfes-type name)))
    (unless 
      (member lilfes-name *lilfes-builtins* :test #'equal)
      ;; don't redefine LiLFeS built in types
;      (format t "'~A'~%" lilfes-name)
      (format stream "~%'~A' <- " lilfes-name)
      (format stream "['~A'" (convert-lilfes-type (car parents)))
      (loop for parent in (cdr parents)
           do
           (format stream ", '~A'" (convert-lilfes-type parent)))
      (format stream "]")
      (when def
         (display-lilfes-signature-and-constraint name def stream))
      (format stream ".")
;; deleted by yusuke  May. 21
;      (when (equal lilfes-name "0-1-list")
;	(format stream "~%'0-1-list-nil' <- ['0-1-list', 'nil']."))))))
)))  ;; by yusuke  May. 21

(defun display-lilfes-signature-and-constraint (type def stream)
  (let ((feats (loop for feat in (top-level-features-of def)
                    when (eq (maximal-type-of feat) type)
                    collect feat)))
    (when feats
      (format stream " +~%[")
      (output-lilfes-fv-pair (car feats) 
                             (type-of-fs (get-dag-value def (car feats)))
                             stream)
      (loop for feat in (cdr feats) 
           do
           (format stream ",~%")
           (output-lilfes-fv-pair feat
                                  (type-of-fs (get-dag-value def feat))
                                  stream))
      (format stream "]"))
    (unless (signature-only-p def feats)
      (output-constraint-as-lilfes def stream))))

(defun signature-only-p (fs feats)
  ;;; we do not want to output a complex constraint if
  ;;; the information it contains is in the signature anyway
  (let ((result t))
    (invalidate-visit-marks)
    (mark-dag-for-output fs)
    ;; function from outputfs.lsp
    ;; that allows us to detect reentrancy
    (dolist (f (top-level-features-of fs))
      (unless (member f feats)
        (setf result nil)
        (return nil))
      (let ((value (get-dag-value fs f)))
        (unless (and (not (top-level-features-of value))
                     (not (eql (dag-visit value) 'double)))
          (setf result nil)
          (return nil))))
    result))

(defun output-lilfes-fv-pair (feat value stream)
    (format stream "~A\\'~A'" (convert-lilfes-feature feat)
            (convert-lilfes-type value)))

(defun output-constraint-as-lilfes (fs stream)
  (format stream "~%/ constr\\")
  (display-dag1 fs 'lilfes stream nil t))


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


;; changed by yusuke  May 25
;(defun output-lilfes-spec (stream def order name)
;  (if (eql order 1)
;      (format stream "~%unary_rule \& ARC_DTR\\['HEAD-DTR'\\]")
;    (if (eql order 2)          
;	(progn 
;	  (format stream "~%binary_rule \& NH_DIR\\~A &~%" 
;		  (lilfes-rule-order def name))  
;	  (format stream "ARC_DTR\\['HEAD-DTR'\\] &~%INP_DTR\\['NON-HEAD-DTR'\\]"))
;      (error "Unrecognised order ~A in ~A" order name))))
(defun output-lilfes-spec (stream def order name)
  (if (eql order 1)
      (format stream "~%unary_rule \& UNARY_DTR_PATH\\['ARGS'\\, hd\\]")
    (if (eql order 2)          
	(let ((rule-order (lilfes-rule-order def name)))
	  (if (eq rule-order nil)
	      (setq rule-order (lilfes-rule-order-nonheaded def name)))
	  (format stream "~%binary_rule \&~%")
	  (format stream "LEFT_DTR_PATH\\['ARGS'\\, hd\\] &~%RIGHT_DTR_PATH\\['ARGS'\\, tl\\, hd\\] &~%")
	  (if (string= rule-order "right")
	      (format stream "TNT_ARC_PATH\\['ARGS'\\, hd\\]")
	      (format stream "TNT_ARC_PATH\\['ARGS'\\, tl\\, hd\\]")))
	(error "Unrecognised order ~A in ~A" order name))))

(defun lilfes-rule-order (rule-fs name)
  (let ((nh-dtr (existing-dag-at-end-of rule-fs '(NON-HEAD-DTR)))
	(h-dtr (existing-dag-at-end-of rule-fs '(HEAD-DTR))))
    (when (and nh-dtr 
	       h-dtr)
	  (let ((first-dtr (existing-dag-at-end-of rule-fs '(ARGS FIRST))))
	    (unless first-dtr
		    (error "No first dtr in ~A" name))
	    (cond ((and (eq first-dtr nh-dtr) 
			(not (eq first-dtr h-dtr))) "left")
		  ((and (eq first-dtr h-dtr)
			(not (eq first-dtr nh-dtr))) "right")
		  (t (error "~A has problems" name)))))))

;; by yusuke  May 25
;; modified by aac
(defun lilfes-rule-order-nonheaded (rule-fs name)
  (declare (ignore name))
  (let ((key-arg-left (existing-dag-at-end-of rule-fs '(ARGS FIRST KEY-ARG)))
	(key-arg-right (existing-dag-at-end-of rule-fs '(ARGS REST FIRST KEY-ARG))))
    (cond ((bool-value-true key-arg-left) "right")
          ((bool-value-true key-arg-right) "left")
	  (t "error"))))
  
;(defun lilfes-rule-order-nonheaded (rule-fs name)
;  "right")

(defun enhance-tdfs (tdfs path value)
  (if (and path (existing-dag-at-end-of (tdfs-indef tdfs) path))
    (with-unification-context (ignore)
      (let ((result (yadu tdfs (create-temp-parsing-tdfs value path))))
        (when result (copy-tdfs-elements result))))
    tdfs))

(defun output-derived-instance-as-lilfes (form fs stream 
                                          &optional id stem derivation)
  (let* ((identifier (and id (make-tdfs :indef (make-dag :type (list id)))))
         (enhanced 
          (if identifier (enhance-tdfs fs *lexical-id-path* identifier) fs))
         (dag (tdfs-indef enhanced)))
    ;; assume no defaults
    (if (and id stem derivation)
      (format 
       stream 
       "~&~%compiled_lexical_entry(~
        \"~(~a~)\", \"~(~a~)\", [~{~(~s~)~^, ~}], \"~{~(~a~)~^ ~}\", "
       id form stem derivation)
      (format stream "~%lexical_entry(\"~A\", " (string-downcase form)))
    (display-dag1 dag 'lilfes stream)
    (format stream ").~%")))


