;;; Copyright (c) 2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :mrs)

;;;
;;; a couple of interface functions (mostly) for [incr tsdb()]
;;;

(defun rasp-semantix-hook (derivation)
  (let* ((*package* (find-package :mrs))
         (derivation (read-from-string derivation nil nil)))
    (ignore-errors
     (with-output-to-string (stream)
       (construct-sem-for-tree derivation stream)))))

(defun read-rmrs-from-string (string)
  (let ((*package* (find-package :mrs)))
    (ignore-errors 
     (read-rmrs (first (xml:parse-xml string))))))

(defun browse-rmrs (rmrs &optional title)
  (ignore-errors
   (let ((browser (fboundp (find-symbol "SHOW-MRS-RMRS-WINDOW" :lkb))))
     (if (functionp browser)
       (apply browser (list nil :rmrs rmrs :title title))
       (output-rmrs rmrs 'compact)))))
