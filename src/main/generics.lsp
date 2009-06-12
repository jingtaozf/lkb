(in-package :lkb)

(defparameter *generic-lexical-entries* nil)

(defparameter *generics-carg* 'CARG)

(defparameter %generics-index% nil)

(defstruct gle
  id flags test le pred mrs)

(defun index-generics ()
  (setf %generics-index%
    (cons
     nil
     (loop
         for (id . flags) in *generic-lexical-entries*
         for test = (let ((predicate (second flags))
                          (*package* (find-package :lkb)))
                      (typecase predicate
                        (string (symbol-function
                                 (read-from-string predicate)))
                        (null nil)
                        (symbol (symbol-function predicate))
                        (function predicate)
                        (cons predicate)))
         for entry = (when (smember :generate flags)
                       (get-lex-entry-from-id id))
         for mrs = (and entry (lex-entry-full-fs entry)
                        (mrs::extract-mrs-from-fs 
                         (tdfs-indef (lex-entry-full-fs entry))))
         when (and entry (null mrs)) do
           (format 
            t 
            "index-generic(): ~
             ignoring entry `~(~a~)' for null semantics.~%"
            id)
         when (and mrs (rest (mrs:psoa-liszt mrs))) do
           (format 
            t 
            "index-generic(): ~
             ignoring entry `~(~a~)' for decomposed semantics.~%"
            id)
         else when mrs
         collect
           (make-gle
            :id id :flags flags :test test :le entry
            :pred (mrs::rel-pred (first (mrs:psoa-liszt mrs)))
            :mrs mrs)))))
            
(defun gen-instantiate-generics (ep)
  (loop
      with ids
      with pred = (mrs::rel-pred ep)
      with carg = (loop
                      for role in (mrs:rel-flist ep)
                      when (eq (mrs:fvpair-feature role) *generics-carg*)
                      return (mrs:fvpair-value role))
      with surface = (substitute #\space #\_ carg :test #'char=)
      with *package* = (find-package :lkb)
      for gle in (rest %generics-index%)
      for test
      = (when (gle-test gle) (ignore-errors (funcall (gle-test gle) ep)))
      when (or test (and carg (equal pred (gle-pred gle))))
      do
        (let* ((id (format nil "~@:(~a[~a]~)" (gle-id gle) (or test surface)))
               (id (intern id :lkb)))
          (if (get-lex-entry-from-id id)
            (push id ids)
            (multiple-value-bind (tdfs orth)
                (instantiate-generic-lexical-entry
                 gle (or test surface) pred carg)
              (when tdfs
                (let ((new
                       (make-lex-entry
                        :orth (list orth) :id id :full-fs tdfs)))
                  ;;
                  ;; _fix_me_
                  ;; we should encapsulate the write access on the lexicon as a
                  ;; method cache-psort() or the like.           (7-jun-09; oe)
                  ;;
                  (with-slots (psorts) *lexicon*
                    (setf (gethash id psorts) new))
                  (mrs::extract-lexical-relations new)
                  (push id ids))))))
      finally (return ids)))


(defun glep (le)
  (typecase le
    (lex-entry
     (let* ((id (string (lex-entry-id le)))
            (bracket (position #\[ id))
            (id (intern (subseq id 0 bracket) *lkb-package*)))
       (assoc id *generic-lexical-entries* :test #'eq)))))
