(in-package "MRS")

#|
;;; currently defined in mrs-package.lisp because it
;;; needs to be read in before any globals

(defun vsym (str) 
  ;;; allow mrsglobals and mrsglobals-eng to be system independent
  (intern (string-upcase str) "DISCO"))
|#  
     
;;; called from mrsoutput
                         
(defun is-disjunctive-fs (fs)
  (and fs (unify::disjunction-node-p fs)))

(defun is-valid-fs (fs)
  (and fs (not (eq fs 'unify::*fail*))))
    
;; Changed create-type to convert string values to symbols, since VIT checker 
;; does not like string value for attribute CONST-VALUE in numerals etc.

(defun base-create-type (type)
  (if (and (consp type) (eq (first type) :atom))
      (if (stringp (second type))
	  (read-from-string (second type))
	(second type))
    type))

(defun no-path-to (path-value-res)
  (eql path-value-res 'unify::*fail*))


(defun equal-or-subtype (type1 type2)
  (or (equal type1 type2)
      (if (and (symbolp type1) (symbolp type2))
          (tdl:extends type1 type2)
        (tdl::complex-subsumes-p type2 type1))))

;;; called from mrs-to-vit

(defun is-top-type (val)
  (or (eq 'tdl::*top* val)
      (eq 'cl-user::*top* val)))

(defun last-path-feature (path)
  path)

(defun tdl-show-current-domain nil
  (tdl::show-current-domain))

;;; called from mrsfns.lisp

(defun get-parse-fs (parse)
  (if (eq (type-of parse) 'csli-unify::fs) 
      parse
    (if (string-equal "1" (subseq user::*page-version* 0 1))
	(lexicon::cfs-fs (pg::u-item-cfs parse))
      (if (or (eq (type-of parse) 'typed-item)
	      (eq (type-of parse) 'main::typed-item))
	  (lexicon::cfs-fs (car (main::typed-item-args parse)))
	(cfs-fs (pg::combo-item-cfs parse))))))

(defun get-parse-fs-alt (parse)
  (if (string-equal "1" (subseq cl-user::*page-version* 0 1))
      (lexicon::cfs-fs (pg::u-item-cfs parse))
    (lexicon::cfs-fs (pg::combo-item-cfs parse))))

(defun output-parse-tree (tree ostream)
  (trees::kh-parse-tree tree :stream ostream))

(defun get-last-sentence nil
  main::*last-sentence*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What used to be in tdl-utilities.lisp

(defun tdl-precedes (type1 type2 &optional (domain (tdl::show-current-domain)))
  (DECLARE (TYPE SYMBOL  type1)
           (TYPE SYMBOL  type2)
           (TYPE SIMPLE-STRING  domain))
  (if (and (tdl::type-exists-p type1 domain)
           (tdl::type-exists-p type2 domain))
      (tdl::precedes type1 type2)))

;;; get disjunctions
(defun get-first-alter (big-fs)
  (let ((fs (deref big-fs)))
    (if (unify::disjunction-node-p fs)
	(first (unify::get-alternative-list fs))
      fs)))

(defun get-alter-list  (big-fs)
  (let ((fs (deref big-fs)))
    (if (unify::disjunction-node-p fs)
	(unify::get-alternative-list fs)
      fs)))

(defun get-real-alter-list (big-fs)
  (let* ((fs (if (listp big-fs)
                 big-fs
               (deref big-fs)))
         (altlist (if (unify::disjunction-node-p fs)
                      (unify::get-alternative-list fs)
                    fs))
         (newlist nil))
    (if (listp altlist)
        (dolist (ele altlist (reverse newlist))
          (if (not (eq ele 'unify::*fail*))
              (push ele newlist)))
      fs)))

;;; Probleme mit eingebetteten Disjunktionen
(defun get-first-real-alter (big-fs)
  (let* ((fs (if (listp big-fs)
                 big-fs
               (deref big-fs)))
         (altlist (if (unify::disjunction-node-p fs)
                      (unify::get-alternative-list fs)
                    fs)))
    (if (listp altlist)
        (dolist (ele altlist (first altlist))
          (if (not (eq ele 'unify::*fail*))
              (return ele)))
      fs)))

#|
Other useful functions from UDINE/TDL:

;;; get path-value from fs (returns unify::*fail* if path does not exist):

(unify::sub-fs fs path)

;;; (unify::coref-find node) is equivalent with (unify::deref-fs node)

|#

;;; required by munger

(defun compatible-types (type1 type2)
  (let ((type1name (fetch-and-expand-type type1))
	(type2name (fetch-and-expand-type type2)))
    (when (and type1name type2name)
      (not (eq (csli-unify::unify-types type1name type2name)
	       'CSLI-UNIFY::*FAIL*)))))

(defun is-valid-type (type)
  (tdl::get-infon (intern type lex::*lex-package*) lex::*lex-package* :avms))

(defun fetch-and-expand-type (name &optional (domain lex::*lex-package*))
  (let* ((domain (string domain))
         (name (intern name domain))
         (infon (tdl::get-infon name domain :avms)))
    (when infon
      (tdl::expand-type name :domain domain)
      (lex::convert 
       (tdl::feature-structure-term (tdl::get-prototype name domain :avms))
       user::*unifier*))))
