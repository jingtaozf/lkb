;;; Copyright (c) 1991-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions


(in-package :lkb)

;;; Feb 1998 - removed fairly useless indexing scheme
;;;            currently stubs for a better one, which will have to be implemented by
;;;            defining get-indexed-lrules and get-indexed-rules sensibly

;;; April 1997 - YADUized 
;;;            - I/O stuff moved to new file 
;;;                    io-general/ruleinput.lsp

;;; Lexical rules and grammar rules

;;; Modified 03/93 to incorporate LKB morphology system - BEMJ
;;;


;;; All rules are feature structures which are special in that they used
;;; differently from lexical entries, but they are constructed 
;;; by specifying types (and possibly psorts) in exactly the same way
;;; Rule entries thus consist of an identifier plus a series of
;;; unifications etc (which we will assume are non-default for the time
;;; being).  All rules have a mother and one or more daughters; lexical
;;; rules are special in that they are unary, and possibly in other respects
;;; but this is set up in the types.


(defvar *rules* (make-hash-table :test #'eq))

(defvar *lexical-rules* (make-hash-table :test #'eq))

(defvar *ordered-rule-list* nil)
(defvar *ordered-lrule-list* nil)

(defun clear-grammar nil
  (setf *ordered-rule-list* nil)
  (when (fboundp 'clear-generator-grules)
    (funcall 'clear-generator-grules))
  (when (fboundp 'clear-generator-grules)
    (funcall 'clear-generator-grules))
  (clrhash *rules*))

(defun clear-lex-rules nil
  (setf *ordered-lrule-list* nil)
  (when (fboundp 'reset-cached-lex-entries)
    (funcall 'reset-cached-lex-entries))
  (when (fboundp 'clear-generator-lrules)
    (funcall 'clear-generator-lrules))
  (clrhash *lexical-rules*))

(defstruct (rule (:include psort))
  ;;; NOTE - any changes to slots here have to be mirrored
  ;;; in mrs/lexlookup.lsp make-new-found-rule
  rtdfs ;; restricted feature structure for improved packing
  daughters-restricted
  daughters-restricted-reversed
  daughters-apply-order
  order
  rhs ;; list of indices into `order' --- for key-driven parsing
  daughters-order-reversed
  apply-filter
  apply-index
  head)

;;; order is an ordered list of the paths that get at mother and daughters
;;; e.g. 0 1 2
;;; nil (ARGS HD) (ARGS TL HD)

(defun get-lex-rule-entry (name)
   (gethash name *lexical-rules*))

(defun get-grammar-rule-entry (name)
   (gethash name *rules*))

(defun get-indexed-lrules (tdfs &optional test-fn)
  ;; don't return any rules with satisfy test-fn (if specified)
  (declare (ignore tdfs))
  (let ((result nil))
    (maphash #'(lambda (key value)
                 (declare (ignore key))
                 (unless
                    (or (redundancy-rule-p value)
                        (and test-fn (funcall test-fn value))
                        (and *current-language*
                             (not (eq *current-language* 
                                      (rule-language value)))))
                    (push value result)))
           *lexical-rules*)
    result))

(defun get-indexed-rules (tdfs &optional test-fn)
  ;; don't return any rules with satisfy test-fn (if specified)
  (declare (ignore tdfs))
  (let ((result nil))
    (maphash #'(lambda (key value)
                 (declare (ignore key))
                 (unless
                    (or (redundancy-rule-p value)
                        (and test-fn (funcall test-fn value))
                        (and *current-language*
                             (not (eq *current-language* 
                                      (rule-language value)))))
                    (push value result)))
           *rules*)
    result))


(defun greater-than-binary-p nil
  (maphash #'(lambda (k v) 
               (declare (ignore k))
               (if (> (length (rule-order v)) 3) 
                   (return-from greater-than-binary-p t)))
           *rules*))

(defun lexical-rule-p (x)
  (when (and (rule-p x) 
	     (get-lex-rule-entry (rule-id x)))
    t))


(defun apply-lexical-rule (rule-name fs)
   (let ((lex-rule (get-lex-rule-entry rule-name)))
      (unless lex-rule
         (error "Unknown lexical rule ~A" rule-name))
      (evaluate-unifications lex-rule
         (list fs))))

(defparameter *number-of-applications* 0)

(defun try-all-lexical-rules (entries &optional ignore-list)
   ;;; entries are pairs with list of rules applied plus result
   (incf *number-of-applications*)
   (when (> *number-of-applications* *maximal-lex-rule-applications*)
      (error "~%Probable circular lexical rule"))
   (let ((transformed-entries 
            (loop for entry in entries
               append
               (loop for rule in 
                  (get-indexed-lrules (cdr entry)
                     #'(lambda (rule) (member (rule-id rule) ignore-list)))
                  nconc
                  (let* ((spelling-rule-p (spelling-change-rule-p rule))
                         (new-morph 
                              (if spelling-rule-p
                                  (construct-new-morph entry rule)))
                         (result
                          (if (or (not spelling-rule-p) new-morph)
                              ; allow morphographemics to block generation
                              (evaluate-unifications rule
                                                     (list (cdr entry))
                                                     new-morph))))
                     (if result 
                         (list
                          (cons 
                           (cons (rule-id rule) (car entry))
                              result))))))))
      (if transformed-entries
         (append transformed-entries
            (try-all-lexical-rules transformed-entries ignore-list)))))


(defun construct-new-morph (entry rule)
  (let ((new-morph
         (full-morph-generate 
          (extract-orth-from-fs 
           (cdr entry))
          (rule-id rule))))
    (if new-morph
        (car (mapcar #'car new-morph)))))
    

(defun get-matching-rules (rhd-fs &optional no-unary)
  #+:arboretum
  (declare (special *mal-active-p*))
  ;;; the test which stops the parser applying a rule
  ;;; with orthographic effects is now
  ;;; spelling-change-rule-p which is defined in the
  ;;; globals file
  ;;; AAC Feb 1996
  (let ((all (if no-unary
               (get-indexed-rules
                rhd-fs
                #'(lambda (rule) (<= (length (rule-order rule)) 2)))
               (union (get-indexed-lrules rhd-fs #'spelling-change-rule-p)
                      (get-indexed-rules rhd-fs #'spelling-change-rule-p)
                      :test #'eq))))
    ;;
    ;; _fix_me_
    ;; incorporate support for grammar checking application, using an
    ;; additional set of `mal' rules that detect and correct certain errors
    ;; types, hence want to be available in parsing at times but probably never
    ;; in generation.  this would seem to call for a better generalization,
    ;; allowing grammars to tag rules according to various contexts for use,
    ;; e.g. in a phased parsing set-up, if we were to move that direction.
    ;;                                                    (23-apr-04; erb & oe)
    #+:arboretum
    (loop
        for rule in all 
        for mal-rule-p = (mal-rule-p rule)
        when (or (null mal-rule-p) *mal-active-p*)
        collect rule)
    #-:arboretum
    all))

(defun get-matching-lex-rules (rhd-fs)
   (get-indexed-lrules rhd-fs #'spelling-change-rule-p))


(defun apply-lex-interactive (lex lex-entry-fs lex-rule)
  (declare (ignore lex))
  (if 
      ;; modification to check whether a particular 
      ;; lexical rule is morphological - if so, then the 
      ;; unification function is called with an extra 
      ;; option value which describes the new 
      ;; orthography of the result.
      (spelling-change-rule-p lex-rule)
      ;; need to reimplement evaluate-unifications-with-fail-messages
      (evaluate-unifications 
       lex-rule 
       (list lex-entry-fs) 
       (car (mapcar #'car 
                    (full-morph-generate 
                     (extract-orth-from-fs lex-entry-fs)
                     (rule-id lex-rule)))))
    (evaluate-unifications lex-rule
                           (list lex-entry-fs))))


;;; adding rules - function called from tdlruleinput and ruleinput

(defun add-grammar-rule (id non-def def rule-persistence lexical-p)
  (let ((entry (make-rule :id id)))
    (setf (rule-unifs entry) non-def)
    (setf (rule-def-unifs entry) def)
    (when (gethash id (if lexical-p *lexical-rules* *rules*))
      (format t "~%WARNING: Rule `~A' redefined." id))
    (expand-rule id entry non-def def rule-persistence lexical-p)))

;;; expanding rules - also called from type redefinition functions

(defun expand-rule (id rule non-def def rule-persistence lexical-p)
  (process-unif-list id non-def def rule rule-persistence)
  (let ((fs (rule-full-fs rule)))  
    (when fs
      (setf (rule-rtdfs rule) (copy-tdfs-partially fs))
      (if lexical-p 
        (pushnew id *ordered-lrule-list*)
        (pushnew id *ordered-rule-list*))
      (let ((f-list
             (mapcar #'(lambda (x) (if (listp x) x (list x)))
                     (establish-linear-precedence (tdfs-indef fs)))))
        (setf (rule-order rule) f-list)
        ;; note that generator requires all slots related to rule-order
        ;; to contain paths that are eq when they are equal
        (setf (rule-daughters-order-reversed rule) (reverse (cdr f-list)))
        (setf (rule-daughters-restricted rule)
          (mapcar
           #'(lambda (path)
               (restrict-fs
                (existing-dag-at-end-of (tdfs-indef fs) path)))
           (cdr f-list)))
        (setf (rule-daughters-restricted-reversed rule)
          (reverse (rule-daughters-restricted rule)))
        (let ((key-path
               (some
                #'(lambda (path)
                    (let ((dag
                           (existing-dag-at-end-of 
                            (tdfs-indef fs)
                            (append path *key-daughter-path*))))
                      (and dag
                           (bool-value-true dag)
                           path)))
                (cdr f-list))))
          ;; if there is a key daughter, remaining daughters ordered to be
          ;; processed r->l before key-path, then l->r after - generator
          ;; assumes this
          (setf (rule-daughters-apply-order rule)
            (if key-path
                (let ((tail (member key-path (cdr f-list) :test #'eq)))
                  (cons key-path
                        (nconc (nreverse (ldiff (cdr f-list) tail)) 
                               (cdr tail))))
              (cdr f-list))))
        ;;
        ;; make several attempts to identify the (linguistic) head daughter
        ;; (for use in Redwoods tree lexicalization, but potentially useful for
        ;; other processing aspects).                         (29-nov-02; oe)
        ;;
        (let* ((daughters (rest (rule-order rule)))
               (dag (tdfs-indef fs))
               (daughter (existing-dag-at-end-of dag *head-daugher-path*))
               (head (existing-dag-at-end-of dag *head-path*)))
          (setf (rule-head rule)
            (or (when daughter
                  (loop
                      for path in daughters
                      for i from 0
                      for foo = (existing-dag-at-end-of dag path)
                      when (eq foo daughter) return i))
                (when head
                  (loop
                      for path in daughters
                      for i from 0
                      for foo = (existing-dag-at-end-of dag path)
                      for bar = (when foo
                                  (existing-dag-at-end-of foo *head-path*))
                      when (eq bar head) return i))
                0)))
        ;;
        ;; compute list of indices into `order' slot; these are used in
        ;; key-driven parsing (using the new (hyper-)active parser); there
        ;; seems to be some overlap with `daughters-apply-order' used in
        ;; the generator.  however, the parser really needs the numerical
        ;; encoding to decide wheter an edge wants to extend forwards or
        ;; backwards.  brief inspection of the generation code suggests
        ;; that `daughters-apply-order' currently is only used to compute
        ;; a numerical index of the (linguistic) head daughter.  --- so, 
        ;; maybe the two mechanisms could be joined.     (19-jul-99  -  oe)
        ;;
        #-:head-first
        (let* ((daughters (rest (rule-order rule)))
               (arity (length daughters))
               (dag (tdfs-indef fs))
               (key (or
                     (let ((key (rest (assoc id *rule-keys*))))
                       (and (integerp key) (<= key arity) (- key 1)))
                     (loop
                         for path in daughters
                         for i from 0
                         for daughter = (existing-dag-at-end-of dag path)
                         when (key-daughter-p daughter)
                         return i)
                     0)))
          (setf (rule-rhs rule)
            (cons
             key
             (nconc
              (loop for i from (- key 1) downto 0 collect i)
              (loop for i from (+ key 1) to (- arity 1) collect i)))))
        #+:head-first
        (let ((head (rule-head rule))
              (arity (length (rest (rule-order rule)))))
          (setf (rule-rhs rule)
            (cons
             head
             (nconc
              (loop for i from 0 to (- head 1) collect i)
              (loop for i from (+ head 1) to (- arity 1) collect i)))))
        (setf (gethash id (if lexical-p *lexical-rules* *rules*)) rule)))))


(defun key-daughter-p (dag)
  ;;; AAC - moved from user-fns because it's not really likely
  ;;; anyone will want to change this given the global variables
  (let* ((key (existing-dag-at-end-of dag *key-daughter-path*))
         (type (and (dag-p key) (dag-type key))))
    (when type
      (or (eq type *key-daughter-type*) 
          (and (consp type) (eq (first type) *key-daughter-type*))))))

;;; The following is called from the code which redefines types

(defun expand-rules nil
  (maphash #'(lambda (id rule)
               (reexpand-rule id rule nil))
	   *rules*)
  (maphash #'(lambda (id rule)
               (reexpand-rule id rule t))
	   *lexical-rules*))

(defun reexpand-rule (id rule lexical-p)
  (let ((non-def (rule-unifs rule))
        (def (rule-def-unifs rule)))
    (expand-rule id rule non-def def *description-persistence* lexical-p)))


;;; Irregular morphology

(defvar *irregular-forms* (make-hash-table :test #'equal))

(defvar *irregular-forms-gen* (make-hash-table :test #'equal))

(defun find-irregular-morphs (word)
  (gethash word *irregular-forms*))

(defun gen-irreg-morphs (stem rule)
  ;;; assumes only one answer which is clearly wrong, but until we
  ;;; have a mechanism for alternate spellings in
  ;;; lexical entries it'll have to do
  (let ((irreg
         (reverse ; so order matches textual order
          (gethash stem *irregular-forms-gen*))))
    (cdr (assoc rule irreg)))) 

(defun full-morph-generate (stem rule)
  (setf stem (string-upcase stem))
  (let ((irreg-form (gen-irreg-morphs stem rule)))
    (if irreg-form
        (list (list irreg-form (list rule stem)))
        (morph-generate stem rule))))


(defun filter-for-irregs (reg-list)
  ;;; called from parse.lsp
;;; remove anything from the regular morphology results which corresponds
;;; to the application of a rule to a stem corresponding to one of 
;;; the irregular forms
  (if *irregular-forms-only-p* 
      (loop for reg in reg-list
           nconc
           (let* ((stem (car reg))
                  (first-rule (caadr reg))
                  (irreg-stems 
                   (gethash stem *irregular-forms-gen*))
                  (irreg-rules (mapcar #'car irreg-stems)))
             (if (and irreg-stems first-rule
                      (member first-rule irreg-rules))
                 nil
               (list reg))))
    reg-list))


#|
(load-irregular-spellings "Macintosh HD:newlkb:src:data:Dikran:irregs.tab")

(load-irregular-spellings "~aac/grammar/irregs.tab")
 
(find-irregular-morphs "has")
|#

(defun read-irreg-form-strings (strings)
  ;;; caller has checked these are actual strings
  (clrhash *irregular-forms*)
  (clrhash *irregular-forms-gen*) 
  (dolist (string strings)
    (with-input-from-string (stream string)
         (loop for irreg = (read-line stream nil nil)
             while irreg
             unless (or (zerop (length irreg)) (eq (elt irreg 0) #\;))
             do
               (let* ((irreg-right (position '#\  irreg))                         
                      (spelling 
                       (if irreg-right
                           (string-upcase (subseq irreg 0 irreg-right))))
                      (aff-right 
                       (if irreg-right
                           (position '#\  irreg :start (+ 1 irreg-right))))
                      (affixname 
                       (if (and irreg-right aff-right)
                           (string-upcase
                            (subseq irreg (+ 1 irreg-right) aff-right))))
                      (stem-right 
                       (if aff-right
                           (position '#\  irreg :start (+ 1 aff-right))))
                      (stem 
                       (if aff-right
                           (string-upcase
                            (subseq irreg (+ 1 aff-right) stem-right)))))
                 (if (and spelling affixname stem)
                     (add-to-irregulars spelling (create-lex-rule-name affixname) 
                                        stem)))))))

(defun add-to-irregulars (irreg-form rule stem)
  (push (list stem (list rule irreg-form))
        (gethash irreg-form *irregular-forms*))
  (push (cons rule irreg-form)
        (gethash stem *irregular-forms-gen*)))

(defun create-lex-rule-name (rule-name)
  (if *lex-rule-suffix*
    (intern (concatenate 'string (string rule-name) *lex-rule-suffix*))
    (intern rule-name)))


;;; DISCO-style rule filter (build-rule-filter)

(defun build-rule-filter nil
  (unless (find :vanilla *features*)
    (let ((max-arity 0)
          (nrules 0)
          (rule-list nil))
      (flet ((process-rule (name rule)
               (declare (ignore name))
               (setq max-arity (max max-arity (1- (length (rule-order rule)))))
               (push rule rule-list)
               (setf (rule-apply-index rule) nrules)
               (incf nrules)))
        (maphash #'process-rule *rules*)
        (maphash #'process-rule *lexical-rules*)
        (dolist (rule rule-list)
          (let ((filter
                 (make-array (list nrules max-arity) :initial-element nil)))
            (setf (rule-apply-filter rule)
              (fill-rule-filter rule filter rule-list))))
        t))))


(defun fill-rule-filter (rule filter test-list)
  (let ((rule-tdfs (rule-rtdfs rule))
        (rule-daughters (cdr (rule-order rule))))
    (loop for test in test-list
        do
          (let ((test-tdfs (rule-rtdfs test))
                (test-index (rule-apply-index test)))
            (loop for arg from 0 to (1- (length rule-daughters))
                for dtr in rule-daughters
                do
                  (with-unification-context (ignore)
                    (when
                        (yadu rule-tdfs
                              (create-temp-parsing-tdfs
                               (if (eq test-tdfs rule-tdfs)
                                 (copy-tdfs-completely test-tdfs)
                                 test-tdfs)
                               dtr))
                      (setf (aref filter test-index arg) t))))))
    filter))


(defun check-rule-filter (rule test arg)
  ;; can test fill argth daughter of rule?
  (let ((filter (rule-apply-filter rule)))
    (if (and filter (not (stringp test)))
	(aref (the (simple-array t (* *)) filter) 
	      (rule-apply-index test)
	      arg)
      t)))


(defun rules-to-xml  (&key (stream t) file)
  (let ((stream (if file
                  (open file
                        :direction :output :if-exists :supersede
                        :if-does-not-exist :create)
                  stream)))
    (loop
        for rule being each hash-value in *rules*
        for id = (rule-id rule)
        for tdfs = (rule-full-fs rule)
        for type = (and tdfs (indef-type-of-tdfs tdfs))
        for key = (first (rule-rhs rule))
        for head = (let* ((daughters (rest (rule-order rule)))
                          (dag (tdfs-indef tdfs))
                          (head (existing-dag-at-end-of dag '(head-dtr))))
                     (when head
                       (loop
                           for path in daughters
                           for i from 0
                           for daughter = (existing-dag-at-end-of dag path)
                           when (eq daughter head)
                           return i)))
        when (and id type key) do
          (format
           stream
           "<instance name=\"~(~a~)\" type=\"~(~a~)\" ~
             ~@[head=\"~d\" ~] key=\"~d\" status=\"rule\"/>~%"
           id type head key))
    (loop
        for rule being each hash-value in *lexical-rules*
        for id = (rule-id rule)
        for tdfs = (rule-full-fs rule)
        for type = (and tdfs (indef-type-of-tdfs tdfs))
        when (and id type) do
          (format
           stream
           "<instance name=\"~(~a~)\" type=\"~(~a~)\" status=\"~a\"/>~%"
           id type (if (inflectional-rule-p id) "irule" "lrule")))
    (when file (close stream))))
