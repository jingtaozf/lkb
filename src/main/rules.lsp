;;; Copyright Ann Copestake and Bernard Jones 1992-1997. 
;;; All Rights Reserved.
;;; No use or redistribution without permission.
;;; 

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


(defvar *rules* (make-hash-table))

(defvar *lexical-rules* (make-hash-table))

(defun clear-grammar nil
   (clrhash *rules*))

(defun clear-lex-rules nil
  (when (fboundp 'reset-cached-lex-entries)
   (reset-cached-lex-entries))
   (clrhash *lexical-rules*))



(defstruct (rule (:include lex-or-psort))
   daughters-restricted
   head-first-p
   order)

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
                       (and test-fn (funcall test-fn value)))
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
                       (and test-fn (funcall test-fn value)))
                    (push value result)))
           *rules*)
    result))

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
                  (let ((result
                           (evaluate-unifications rule
                              (list (cdr entry))
                              (if 
                                (spelling-change-rule-p rule)
                                ;;; test changed AAC Feb 1996
                                (car (mapcar #'car
                                             (morph-generate 
                                              (extract-orth-from-fs (cdr entry))
                                              (rule-id rule))))))))
                     (if result 
                        (cons 
                           (cons (rule-id rule) (car entry))
                              result)))))))
      (if transformed-entries
         (append transformed-entries
            (try-all-lexical-rules transformed-entries ignore-list)))))
         
(defun apply-all-lexical-rules (entries)
   (incf *number-of-applications*)
   (when (> *number-of-applications* *maximal-lex-rule-applications*)
      (error "~%Probable circular lexical rule"))
   (let ((transformed-entries 
            (for entry in entries
               append
               (for rule in (get-indexed-lrules entry)
                  filter
                  (evaluate-unifications rule
                     (list entry)
                     (if 
                       (spelling-change-rule-p rule)
                       (car (mapcar #'car
                                    (morph-generate 
                                     (extract-orth-from-fs entry)
                                     (rule-id rule))))))))))
      (if transformed-entries 
         (append transformed-entries
            (apply-all-lexical-rules transformed-entries)))))
      


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


;;; rule format - is very like any other psort file

(defun add-grammar-rule (id rule)
   (add-lex-or-grammar-rule id rule nil))

(defun add-lexical-rule (id rule)
  (add-lex-or-grammar-rule id rule t))
   

(defun add-lex-or-grammar-rule (id rule lexp)
  ;;; YADU - need to get at the indef structure
   (let* ((fs (rule-full-fs rule))
          (f-list (establish-linear-precedence (tdfs-indef fs))))
      (setf (rule-order rule) f-list)
      (setf (rule-daughters-restricted rule)
         (mapcar
            #'(lambda (path)
                (restrict-fs
                   (existing-dag-at-end-of (tdfs-indef fs)
                      (if (listp path) path (list path)))))
            (cdr f-list)))
      (flet ((listify (x) (if (listp x) x (list x))))
         (setf (rule-head-first-p rule)
            (eq
               (existing-dag-at-end-of (tdfs-indef fs)
                  (append (listify (car f-list)) *head-marking-path*))
               (existing-dag-at-end-of (tdfs-indef fs)
                  (append (listify (cadr f-list)) *head-marking-path*)))))
      (setf (gethash id (if lexp *lexical-rules* *rules*)) rule)))


;;; Irregular morphology

(defparameter *irregular-forms* (make-hash-table :test #'equal))

(defun find-irregular-morphs (word)
  (gethash (string-upcase word) *irregular-forms*))

#|
(read-irreg-form-file "Macintosh HD:lkb99-expt:data:Dikran:irregs.tab")

(find-irregular-morphs "has")
|#

(defun read-irreg-form-file (file-name)
  (clrhash *irregular-forms*)
  (with-open-file
    (istream file-name :direction :input)
    (format t "~%Reading in irregular forms")
    (loop
      (let ((next-char (peek-char t istream nil 'eof)))
        (when (eql next-char 'eof) (return))
        (if (eql next-char #\;) 
          (read-line istream)
          (read-irreg-entry istream))))))

(defun read-irreg-entry (istream)
  (let* ((irreg-form (string (read istream)))
         (rule (create-lex-rule-name (read istream)))
         (stem (string (read istream))))
    (add-to-irregulars irreg-form rule stem)))

(defun add-to-irregulars (irreg-form rule stem)
  (push (list stem (list rule irreg-form))
        (gethash irreg-form *irregular-forms*)))

(defun create-lex-rule-name (rule-name)
  (if *lex-rule-suffix*
    (intern (concatenate 'string (string rule-name) *lex-rule-suffix*))
    rule-name))

