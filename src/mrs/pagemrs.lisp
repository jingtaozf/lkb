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

(defun create-type (type)
  (if (and (consp type) (eq (first type) :atom))
      (if (stringp (second type))
	  (read-from-string (second type))
	(second type))
    type))

(defun no-path-to (path-value-res)
  (eql path-value-res 'unify::*fail*))


(defun equal-or-subtype (type1 type2)
  (or (eql type1 type2)
      (tdl:extends type1 type2)))


;;; called from mrs-to-vit

(defun is-top-type (val)
  (or (eq 'tdl::*top* val)
      (eq 'user::*top* val)))

(defun last-path-feature (path)
  path)

(defun tdl-show-current-domain nil
  (tdl::show-current-domain))

;;; called from mrsfns.lisp

(defun get-parse-fs (parse)
  (if (string-equal "1" (subseq user::*page-version* 0 1))
      (lexicon::cfs-fs (pg::u-item-cfs parse))
  (lexicon::cfs-fs (car (main::typed-item-args parse)))))

(defun output-parse-tree (tree ostream)
  (trees::kh-parse-tree tree :stream ostream))

(defun get-last-sentence nil
  main::*last-sentence*)
