;;; Copyright Ann Copestake and Bernard Jones 1992-1997. 
;;; All Rights Reserved.
;;; No use or redistribution without permission.
;;; 

(in-package :cl-user)

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
  (when (fboundp 'clear-generator-grules)
    (funcall 'clear-generator-grules))
  (clrhash *rules*))

(defun clear-lex-rules nil
  (when (fboundp 'reset-cached-lex-entries)
    (funcall 'reset-cached-lex-entries))
  (when (fboundp 'clear-generator-lrules)
    (funcall 'clear-generator-lrules))
  (clrhash *lexical-rules*))

(defstruct (rule (:include lex-or-psort))
  ;;; NOTE - any changes to slots here have to be mirrored
  ;;; in mrs/lexlookup.lsp make-new-found-rule
   daughters-restricted
   daughters-restricted-reversed
   daughters-apply-order
   order
   daughters-order-reversed
   apply-filter
   apply-index)

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


(defun lexical-rule-p (x)
   (and (rule-p x) (get-lex-rule-entry (rule-id x))))


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
            (for entry in entries
               append
               (for rule in 
                  (get-indexed-lrules (cdr entry)
                     #'(lambda (rule) (member (rule-id rule) ignore-list)))
                  filter
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
                        (cons 
                           (cons (rule-id rule) (car entry))
                              result)))))))
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
  ;;; the test which stops the parser applying a rule
  ;;; with orthographic effects is now
  ;;; spelling-change-rule-p which is defined in the
  ;;; globals file
  ;;; AAC Feb 1996
  (if no-unary
    (get-indexed-rules rhd-fs
       #'(lambda (rule) (<= (length (rule-order rule)) 2)))
    (union (get-indexed-lrules rhd-fs #'spelling-change-rule-p)
           (get-indexed-rules rhd-fs #'spelling-change-rule-p)
           :test #'eq)))

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
      (format t "~%Rule ~A redefined" id))
    (expand-rule id entry non-def def rule-persistence lexical-p)))

;;; expanding rules - also called from type redefinition functions

(defun expand-rule (id rule non-def def rule-persistence lexical-p)
  (process-unif-list id non-def def rule rule-persistence)
  (let ((fs (rule-full-fs rule)))  
    (when fs
      (if lexical-p 
          (pushnew id *ordered-lrule-list*)
        (pushnew id *ordered-rule-list*))
      (let ((f-list (establish-linear-precedence (tdfs-indef fs))))
        (setf (rule-order rule) f-list)
        (setf (rule-daughters-order-reversed rule) (reverse (cdr f-list)))
        (setf (rule-daughters-restricted rule)
          (mapcar
           #'(lambda (path)
               (restrict-fs
                (existing-dag-at-end-of 
                 (tdfs-indef fs)
                 (if (listp path) path (list path)))))
           (cdr f-list)))
        (setf (rule-daughters-restricted-reversed rule)
           (reverse (rule-daughters-restricted rule)))
        (flet ((listify (x) (if (listp x) x (list x))))
          (let*
              ((mother-value
                (existing-dag-at-end-of 
                 (tdfs-indef fs)
                 (append (listify (car f-list)) *head-marking-path*)))
               (head-path
                (some
                 #'(lambda (path)
                     (and (eq
                           (existing-dag-at-end-of 
                            (tdfs-indef fs)
                            (append (listify path) *head-marking-path*))
                           mother-value)
                          path))
                 (cdr f-list))))
            ;; if there is a head, remaining daughters 
            ;; must stay in same left-to-right order - 
            ;; parser assumes this
            (setf (rule-daughters-apply-order rule)
              (if head-path
                  (cons head-path 
                        (remove head-path (cdr f-list) :count 1 :test #'eq))
                (cdr f-list)))))
        (setf (gethash id (if lexical-p *lexical-rules* *rules*)) rule)))))


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
      (for reg in reg-list
           filter
           (let* ((stem (car reg))
                  (first-rule (caadr reg))
                  (irreg-stems 
                   (gethash stem *irregular-forms-gen*))
                  (irreg-rules (mapcar #'car irreg-stems)))
             (if (and irreg-stems first-rule
                      (member first-rule irreg-rules))
                 nil
               reg)))
    reg-list))


#|
(load-irregular-spellings "Macintosh HD:newlkb:src:data:Dikran:irregs.tab")

(load-irregular-spellings "~aac/grammar/irregs.tab")
 
(find-irregular-morphs "has")
|#

(defun read-irreg-form-string (string)
  (when (and string (stringp string))
    (clrhash *irregular-forms*)
    (clrhash *irregular-forms-gen*) 
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
         t)))


(defun fill-rule-filter (rule filter test-list)
   (let ((rule-tdfs (rule-full-fs rule))
         (rule-daughters (cdr (rule-order rule))))
      (loop for test in test-list
         do
         (let ((test-tdfs (rule-full-fs test))
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
