(in-package :lkb)

;;;
;;; --- sql-database methods
;;;

(defmethod close-lex ((lexicon sql-database) &key in-isolation delete)
  (declare (ignore in-isolation delete))
  (with-slots (dbname host user password port) lexicon
    ;(setf dbname nil)
    ;(setf host nil)
    ;(setf user nil)
    (setf password nil)
    ;(setf port nil)
    )                                   ;:todo: unbind functions
  (if (next-method-p) (call-next-method)))


