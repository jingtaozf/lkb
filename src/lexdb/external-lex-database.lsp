;;; Copyright (c) 2001 -- 2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Ben Waldron;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

;;;
;;; --- external-lex-database methods
;;;

(defmethod collect-expanded-lex-ids ((lexicon external-lex-database))
  ;(error "collect-expanded-lex-ids(): invalid method on PostGreSQL lexicon")
  )

(defmethod close-lex ((lexicon external-lex-database) &key in-isolation delete)
  (declare (ignore in-isolation delete))
  (with-slots 
      (fields-map fields-tb lex-tb) 
      lexicon
    (setf fields-map nil)
;    (setf fields-tb nil)
    )
  (if (next-method-p) (call-next-method)))

(defmethod empty-cache ((lexicon external-lex-database) &key recurse)
  (declare (ignore recurse))
  (with-slots (record-cache) lexicon
    (clrhash record-cache)))
