(in-package :cl-user)

;;; from lexindex

(defun index-for-generator nil
  (index-lexicon)
  (index-lexical-rules)
  (index-grammar-rules)
  (format t "~%Indexing complete")
  nil)
 

(defun index-lexicon nil
  (mrs::clear-semantic-indices)
   (setf *batch-mode* t)
   (let ((ids nil))
      ;; because of multiple lexical entries, an id may be indexed by
      ;; multiple orthographies
     (dolist (word (lex-words *lexicon*))
       (dolist (inst (lookup-word *lexicon* word))
	 (pushnew inst ids :test #'eq)))
     (dolist (id ids)
       (let* ((entry (read-psort *lexicon* id))
	      (lex-id (lex-or-psort-id entry)))
	 (expand-psort-entry entry)
	 (let ((new-fs (lex-or-psort-full-fs entry)))
	   (if new-fs
	       (mrs::extract-lexical-relations entry)
	     (format t "~%No feature structure for ~A" lex-id))))
       (unexpand-psort *lexicon* id))
     (setf *batch-mode* nil)))
  
(defun index-lexical-rules nil
  (mrs::clear-lrule-globals)
  (maphash #'(lambda (id entry)
               (let* ((tdfs (rule-full-fs entry))
                      (fs (if tdfs (tdfs-indef tdfs))))
                 (if fs
                     (mrs::extract-lex-rule-rels id fs entry))))
           *lexical-rules*))           
                           

(defun index-grammar-rules nil
  (mrs::clear-grule-globals)
  (maphash #'(lambda (id entry)
               (let* ((tdfs (rule-full-fs entry))
                      (fs (if tdfs (tdfs-indef tdfs))))
                 (if fs
                     (mrs::extract-grammar-rule-rels id fs entry))))
           *rules*))
                 
;;; actually used by lexlookup, but convenient to define in USER

(defun make-mrs-unifs (fvplist initial-features)
  (let ((initial-path (create-typed-path-from-feature-list initial-features)))
    (for fvp in fvplist
         filter
         (let ((value (mrs:fvpair-value fvp)))
           (if (listp value)
               nil
             (make-unification :lhs (make-typed-path 
                                     :typed-feature-list
                                     (append (path-typed-feature-list
                                              initial-path)
                                             (path-typed-feature-list
                                              (mrs:fvpair-feature fvp))))
                               :rhs (make-u-value :types
                                                  (list (deasterisk value)))))))))

(defun deasterisk (value)
  (or
   (and (not (stringp value))
        (let* ((ancestors (mapcar #'type-name (retrieve-ancestors value)))
               (strict-type
                (if ancestors
                    (find-strict-type (cons value ancestors)))))
          (if strict-type
              (find-gcsubtype strict-type value))))
   value))

(defparameter *strict-pairs*
    '((pernum . strict_sort)
      (gender . strict_sort)
      (mood . strict_sort)
      (tense . strict_sort)))
      
(defun find-strict-type (types)
  (dolist (type types)
    (let ((strict-type
           (dolist (strp *strict-pairs*)
             (when (eq type (car strp))
               (return (cdr strp))))))
      (when strict-type
        (return strict-type)))))
       



;;; from lexlookup

;;; Testing lookup code.  
;;; If the following is evaluated, then the parse-tsdb-sentences
;;; code will call the fn on each parse
#|
(defparameter *do-something-with-parse* 'check-lex-retrieval)
|#

(defun check-lex-retrieval nil
    (time
     (for parse-res in *parse-record*
          do
          (let* ((lrules-and-entries-used (collect-parse-base parse-res))
                 (mrs (car (mrs::extract-mrs (list parse-res) t))))
            (let
                ((identified-entry-sets
                  (mrs::collect-lex-entries-from-mrs mrs)))
              (mrs::output-mrs mrs 'mrs::simple)
              (let ((retrieved-ids
                     (for res in identified-entry-sets
                          collect
                          (mrs::found-lex-lex-id-fn (car res))))
                    (overgen nil)
                    (undergen nil))
                (for id in retrieved-ids
                     do
                     (unless
                         (member id lrules-and-entries-used :key #'car)
                       (push id overgen)))
                (for id-and-rules in lrules-and-entries-used
                     do
                     (unless
                         (member (car id-and-rules) retrieved-ids)
                       (push (car id-and-rules) undergen)))
                (when undergen
                  (format t "~%Entries not retrieved ~{~A ~}" undergen)) 
                (when overgen
                  (format t "~%Extra entries retrieved  ~{~A ~}" overgen))))))))

;;; needs to be made more sophisticated to deal with lex rules etc

(defun quick-check-lex-retrieval nil
     (for parse-res in *parse-record*
        do
        (let ((mrs (car (mrs::extract-mrs (list parse-res)))))
          (mrs::output-mrs mrs 'mrs::simple)
          (let
               ((identified-entry-sets
                (mrs::collect-lex-entries-from-mrs mrs)))
          (for res in identified-entry-sets
               do
               (for item in res
                    do
                    (format t "~A ~A " (mrs::found-lex-lex-id-fn item)
                                       (mrs::found-lex-rule-list-fn item))
                    (display-dag 
                     (existing-dag-at-end-of 
                      (tdfs-indef (mrs::found-lex-inst-fs-fn item)) 
                      mrs::*main-semantics-path*) 'simple)))))))


