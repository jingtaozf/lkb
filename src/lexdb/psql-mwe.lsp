;;; Copyright (c) 2003 
;;;   Ben Waldron;
;;;   see `licence.txt' for conditions.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Postgres database: MWE extensions
;;;

(in-package :lkb)

;;;
;;; misc
;;;

(defun get-assoc-val (x assoc-list)
  (cdr (assoc x assoc-list)))


;;;
;;; generate TDL code for MWE entries
;;;

(defun mwe-build-P-list (type keyrel-list)
  (append
   (list (list type))
   (list (cons 'SEM 
	       (list (cons 'IDRELS
			   (build-PD-list keyrel-list 1)))))))

(defun build-PD-list (d-list coindex)
  (append
   (list (cons 'LAST (build-PD-list-aux-LIST nil coindex)))
   (list (cons 'LIST (build-PD-list-aux-LIST d-list coindex)))))

(defun build-PD-list-aux-LIST (d-list coindex)
  (cond
   ((null d-list)
    (list (list (str-2-symb (get-coindex-symb coindex)))))
   (t
    (append
     (list (cons 'FIRST (list (car d-list))))
     (list (cons 'REST (build-PD-list-aux-LIST (cdr d-list) coindex)))))))

(defun get-coindex-symb (i)
  (format nil "#~a" i))

