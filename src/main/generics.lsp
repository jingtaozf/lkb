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
      for gle in (rest %generics-index%)
      when (eq pred (gle-pred gle))
      do
        (let ((id (intern (format nil "~@:(~a[~a]~)" (gle-id gle) carg) :lkb)))
          (if (get-lex-entry-from-id id)
            (push id ids)
            (let* ((orth (list carg))
                   (tdfs (instantiate-generic-lexical-entry gle carg))
                   (new (when tdfs
                          (make-lex-entry :orth orth :id id :full-fs tdfs))))
              (when new
                (with-slots (psorts) *lexicon*
                  (setf (gethash id psorts) new))
                (mrs::extract-lexical-relations new)
                (push id ids)))))
      finally (return ids)))
