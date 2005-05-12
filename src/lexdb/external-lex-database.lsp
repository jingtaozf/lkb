;;; Copyright (c) 2001 -- 2005
;;;   Ben Waldron, John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

;;;
;;; --- external-lex-database methods
;;;

(defmethod close-lex ((lexicon external-lex-database) &key in-isolation delete)
  (declare (ignore in-isolation delete))
  (with-slots 
      (fields-map fields-tb lex-tb) 
      lexicon
    (setf fields-map nil))
  (if (next-method-p) (call-next-method)))

(defmethod empty-cache ((lexicon external-lex-database) &key recurse)
  (declare (ignore recurse))
  (with-slots (record-cache) lexicon
    (clrhash record-cache)))

(defmethod forget-psort ((lexicon external-lex-database) id)
  "remove cached entry (can be :empty)"
  (remhash id (slot-value lexicon 'record-cache))
  (and (next-method-p) 
	      (call-next-method)))
