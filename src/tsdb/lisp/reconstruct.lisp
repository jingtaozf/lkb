(in-package "PARSING")

(defmacro derivation-root (derivation)
  `(first ,derivation))

(defmacro derivation-start (derivation)
  `(second ,derivation))

(defmacro derivation-end (derivation)
  `(third ,derivation))

(defmacro derivation-daughters (derivation)
  `(rest (rest (rest ,derivation))))

(defun reconstruct (i-id derivation)
  (catch :fail
    (reconstruct-derivation derivation)))

(defun reconstruct-derivation (derivation)
  (let* ((root (derivation-root derivation))
         (daughters (derivation-daughters derivation)))
    (cond
     ((and (= (length daughters) 1)
           (null (derivation-daughters (first daughters))))
      (let* ((surface (derivation-root (first daughters)))
             (entry (find-lexical-entry surface root)))
        (if entry
          entry
          (throw :fail
            (list derivation
                  (format nil "no lexical entry `~a' (`~a')" root surface))))))
     (t
      (let* ((items
              (loop
                  for daughter in daughters
                  for item = (reconstruct-derivation daughter)
                  collect item))
             (rule (find-rule root)))
        (if (null rule)
          (throw :fail
            (list derivation
                  (format nil "no rule `~a'" root)))
          (let* ((csli-unify::*unify-debug* :return)
                 (csli-unify::%failure% nil)
                 (result (instantiate-rule rule items)))
            (if (item-p result)
              result
              (throw :fail
                (list derivation result csli-unify::%failure%))))))))))

(defun find-lexical-entry (form instance)
  (let ((items (main::lex-lookup form))
        (name (intern (if (stringp instance)
                        (string-upcase instance)
                        instance)
                      lex::*lex-package*)))
    (find name items :key #'pg::combo-item-index)))


(defun find-rule (instance)
  (let ((name (intern (if (stringp instance)
                        (string-upcase instance)
                        instance)
                      lex::*lex-package*)))
    (or
     (find name (combo-parser-lex-rules (get-parser :lexicon))
           :key #'combo-item-index)
     (find name (combo-parser-syn-rules (get-parser :syntax))
           :key #'combo-item-index))))

(defun instantiate-rule (rule items)
  (let* ((itype (combo-item-itype rule))
         (parser (get-parser (if (eq itype :lex-rule) :lexicon :syntax)))
         (result (cfs-fs (combo-item-cfs rule)))
         (status 0))
      
    (loop
        while result
        for item in items
        for i from 0
        do
          (setf status i)
          (setf result 
            (unify
             result
             (cfs-fs (combo-item-cfs item))
             (append *args-prefix* (fslist-nth-path i)))))

    (when (and result (combo-parser-result-restrictor parser))
      (setf result
        (restrict result
                  (rest (combo-parser-result-restrictor parser))
                  (first (combo-parser-result-restrictor parser)))))

    (if result
      (make-combo-item
       :start (item-start (first items))
       :end (item-end (first (last items)))
       :daughters (make-array 
                    (length (combo-item-rhs rule))
                   :initial-contents items)
       :cfs (make-cfs :fs result)
       :key (item-key rule)
       :label (item-label rule)
       :index (combo-item-index rule)
       :itype itype
       :ruletype (item-ruletype rule))
      status)))
