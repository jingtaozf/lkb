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

;;;
;;; interface function for RASP: given a RASP derivation tree, create (LKB)
;;; edge structures from it, so we can draw and compare trees. 
;;;
;;; _fix_me_
;;; it is beginning to look like a separate Redwoods package (with it own edge
;;; structure, tree drawing, and comparison routines) would be cleaner at some
;;; point; right now, LKB, [incr tsdb()], and Redwoods code mutually depend on
;;; each other.                                                (14-aug-03; oe)
;;;
(defun rasp-reconstruct (derivation &optional dagp &key (start 0) (id 0))
  (if (stringp derivation)
    (rasp-reconstruct (read-from-string derivation) dagp :start start :id id)
    (let* ((root (if (consp derivation) (first derivation) derivation))
           (children (when (consp derivation) (rest derivation))))
      (if (null children)
        (lkb::make-edge :id id :category root 
                        :from start :to (+ start 1)
                        :leaves (let* ((string (string root))
                                       (break (position 
                                               #\_ string 
                                               :from-end t :test #'char=)))
                                  (list (subseq string 0 break))))

        (let ((edges
               (loop
                   for i from (+ id 1)
                   for child in children
                   for edge = (rasp-reconstruct child dagp :start start :id i)
                   then (rasp-reconstruct child dagp 
                                          :start (lkb::edge-to edge) :id i)
                   collect edge)))
          (lkb::make-edge :id id :category root :children edges
                          :leaves (loop
                                      for edge in edges
                                      append (lkb::edge-leaves edge))))))))
