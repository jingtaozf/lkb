;;; Strings

(defparameter *toptype* 'top)

(defparameter *string-type* 'string
   "a special type name - any lisp strings are subtypes of it")

;;; Lexical files

(defparameter *sense-unif-fn* nil)

(defparameter *orth-path* '(orth lst))

(defparameter *list-tail* '(tl))

(defparameter *list-head* '(hd))

(defparameter *lex-rule-suffix* nil)
;;; create the lexical rule name from the info in irregs.tab
;;; for TDL - value should be an upcase string - NIL otherwise

;;; Parsing

(defparameter *chart-limit* 100)

(defparameter *sign-type* 'category
   "a special type wrt parsing - rule indexing is checked for its
   descendants")
 
(defparameter *mother-feature* 0
   "The feature giving the mother in a grammar rule")

(defun establish-linear-precedence (rule-fs)
   ;;;    A function which will order the features of a rule
   ;;;    to give (mother daughter1 ... daughtern)
   ;;;    
   ;;;  Modification - this must always give a feature
   ;;;  position for the mother - it can be NIL if
   ;;; necessary
   (sort (remove 'needs-affix (top-level-features-of rule-fs))
         #'(lambda (x y)
             (if (and (numberp x) (numberp y))
                 (< x y)
               (not (numberp x))))))
  
(defparameter *start-symbol* 'root-cat
   "a type which specifies the type of any valid parse")

(defparameter *morph-rule-type* 'lrule
   "rules of this type may have associated orthographic effects
    (but don't have to)")

(defun spelling-change-rule-p (rule)
;;; a function which is used to prevent the parser 
;;; trying to apply a rule which affects spelling and
;;; which should therefore only be applied by the morphology
;;; system.  
;;; Old test was for something which was a subtype of
;;; *morph-rule-type* - this tests for 
;;; < NEEDS-AFFIX > = true
;;; in the rule
  (let* ((fs (rule-full-fs rule))
         (affix (get-dag-value (if (tdfs-p fs)
                                   (tdfs-indef fs)
                                                fs)
                               'needs-affix)))
    (and affix (equal (type-of-fs affix) '(true)))))

(defun redundancy-rule-p (rule)
;;; a function which is used to prevent the parser 
;;; trying to apply a rule which is only used
;;; as a redundancy rule 
;;; this version tests for 
;;; < PRODUCTIVE > = false
;;; in the rule
  (let* ((fs (rule-full-fs rule))
         (affix (get-dag-value (if (tdfs-p fs)
                                   (tdfs-indef fs)
                                   fs) 
                               'productive)))
    (and affix (equal (type-of-fs affix) '(false)))))

(defparameter *maximal-lex-rule-applications* 7
   "The number of lexical rule applications which may be made
   before it is assumed that some rules are applying circularly")

;;; Display 

(defparameter *feature-abbreviations* 
   '(("-first" . "H")
     ("-last" . "T"))
   "a list of pairs of strings - if the end of a feature name 
   matches the first string it is displayed as the 
   second string in windows.  Used to make lists more readable")
   

(defparameter *active-fs-page-width* 700
   "Page width for displaying feature structures.
   Should be increased if there are problems with
   feature structures wrapping round")
   
(defparameter *dont-show-morphology* nil
  "if set, the morphological structures are not shown in parse trees")


(defparameter *possible-languages* nil
"Specifies the possible languages for interactions
   where a language has to be selected or specified -
 nil if irrelevant")


(defparameter *settings-options* nil
  "controls whether user is asked for type display options file")

(defparameter *feature-ordering*
 '(arg-str orth syn sem))