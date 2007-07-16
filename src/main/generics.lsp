(in-package :lkb)

(defparameter *generic-lexical-entries* nil)

(defparameter *generics-carg* 'CARG)

(defparameter %generics-index% nil)

(defstruct gle
  id flags le pred mrs)

(defun index-generics ()
  (setf %generics-index%
    (cons
     nil
     (loop
         for (id . flags) in *generic-lexical-entries*
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
            :id id :flags flags :le entry
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
      for gle in (rest %generics-index%)
      when (equal pred (gle-pred gle))
      do
        (let* ((id (format nil "~@:(~a[~a]~)" (gle-id gle) surface))
               (id (intern id :lkb)))
          (if (get-lex-entry-from-id id)
            (push id ids)
            (multiple-value-bind (tdfs orth)
                (instantiate-generic-lexical-entry gle surface carg)
              (when tdfs
                (let ((new
                       (make-lex-entry
                        :orth (list orth) :id id :full-fs tdfs)))
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
