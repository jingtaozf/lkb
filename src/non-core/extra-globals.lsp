;;; parameters and functions not needed by the core LKB

;;; indexing

;;; qualia type
;;; qualia psort

(def-lkb-parameter *type-indices* 
   `(((qualia) . ,(make-hash-table))
      (() . ,(make-hash-table))))

(def-lkb-parameter *parent-indices* 
   `(((qualia) . ,(make-hash-table))))



;;; Linking

(def-lkb-parameter *bc96lrules* nil)

(def-lkb-parameter *linking-type* 'linking-type
   "Type from which linking types all inherit")



(def-lkb-parameter *do-really-well-formed-check* nil
   "If set, check lexical entries for real wel-formedness when indexing the 
   lexicon")

;; tlinks

(def-lkb-parameter *tlink-syntactic-sugar* t
   "if set the slashed notation is assumed")


;;; for constraint solving and MT

(def-lkb-parameter *bag-types* '(word))

;;; (def-lkb-parameter *orth-path* '(orth hd))

(def-lkb-parameter *lang-path* '(lang))

(def-lkb-parameter *semantics-path* 
   (create-path-from-feature-list '(sem lizst lst hd)))

(def-lkb-parameter *dummy-type* 'dummy)

(def-lkb-parameter *type-of-isign* 's-sign)
(def-lkb-parameter *type-of-osign* 's-sign)



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

(def-lkb-parameter *top-variable-type* 'entity)
;;; need to know what types variables are in order to do "SKolemisation"

(def-lkb-parameter *top-lex-sign-type* 'word)

