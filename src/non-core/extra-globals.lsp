;;; parameters and functions not needed by the core LKB

;;; indexing

;;; qualia type
;;; qualia psort

(defparameter *type-indices* 
   `(((qualia) . ,(make-hash-table))
      (() . ,(make-hash-table))))

(defparameter *parent-indices* 
   `(((qualia) . ,(make-hash-table))))



;;; Linking

(defparameter *bc96lrules* nil)

(defparameter *linking-type* 'linking-type
   "Type from which linking types all inherit")



(defparameter *do-really-well-formed-check* nil
   "If set, check lexical entries for real wel-formedness when indexing the 
   lexicon")

;; tlinks

(defparameter *tlink-syntactic-sugar* t
   "if set the slashed notation is assumed")


;;; for constraint solving and MT

(defparameter *bag-types* '(word))

;;; (defparameter *orth-path* '(orth hd))

(defparameter *lang-path* '(lang))

(defparameter *semantics-path* 
   (create-path-from-feature-list '(sem lizst lst hd)))

(defparameter *dummy-type* 'dummy)

(defparameter *type-of-isign* 's-sign)
(defparameter *type-of-osign* 's-sign)



(defun construct-generate-all (language)
   ;;; < > = s-sign
   ;;; < LANG > = language
   (process-unifications 
      (list 
         (make-unification :lhs
                        (create-path-from-feature-list '(lang))
                        :rhs (make-u-value :types (list language)))
         (make-unification :lhs
            (create-path-from-feature-list nil)
            :rhs (make-u-value :types (list *type-of-osign*))))))


(defun construct-orth-fs (word-list language)
   ;;; < > = s-sign
   ;;; < ORTH : HD > = "MARY"
   ;;; < ORTH : TL : HD > = "SEES" 
   ;;; < ORTH : TL : TL : HD > = "JOHN"
   ;;; < ORTH : TL : TL : TL > = e-list.
   (let ((tl-orths nil))
      (let
         ((unifications
               (when word-list
                  (cons (make-unification :lhs
                        (create-path-from-feature-list '(lang))
                        :rhs (make-u-value :types (list language)))
                     (append
                        (cons
                           (make-unification :lhs
                              (create-path-from-feature-list nil)
                              :rhs (make-u-value :types (list *type-of-isign*)))                   
                           (for poss-word in word-list
                              filter
                              (let ((word (string-upcase (string poss-word))))
                                 (push 'tl tl-orths)
                                 (make-unification :lhs
                                    (create-path-from-feature-list 
                                       (cons 'orth (append (cdr tl-orths) (list 'hd))))
                                    :rhs (make-u-value :types (list word))))))
                        (list (make-unification :lhs
                              (create-path-from-feature-list 
                                 (cons 'orth tl-orths))
                              :rhs (make-u-value :types (list 'e-list)))))))))
         (when unifications (process-unifications unifications)))))

(defparameter *top-variable-type* 'entity)
;;; need to know what types variables are in order to do "SKolemisation"

(defparameter *top-lex-sign-type* 'word)

(defparameter *feature-ordering*
   '(dtrs synsem orth head-dtr comp-dtrs hcs-synsems hcs-orth hd-dtr
      tl-dtr synsem-list))
