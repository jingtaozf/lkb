(in-package :lkb)

;;;
;;; --- sql-database methods
;;;

(defmethod close-lex ((lexicon sql-database) &key in-isolation delete)
  (declare (ignore in-isolation delete))
  (with-slots (dbname host user password fns) lexicon
    (setf dbname nil)
    (setf host nil)
    (setf lexicon nil)
    (setf password nil)
    (setf fns nil))			;:todo: unbind functions
  )

