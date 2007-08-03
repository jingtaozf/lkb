;;; Copyright (c) 2002 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions

(in-package :lkb)

;;; need to uncomment stuff in lexinput.lsp

(defparameter *idiom-phrases* nil)

(defun add-idiom-entry (name constraint default)
  (push 
   (cons name (make-non-lex-psort-entry name constraint default))
   *idiom-phrases*))

(defparameter *idiom-phrases-expanded* nil)

(defun clear-idioms nil
  (setf *idiom-phrases-expanded* nil)
  (setf *idiom-phrases* nil))

(defstruct idiom-phrase
  id
  tdfs
  mrs)

(defun expand-idioms-phrases nil
  (setf *idiom-phrases-expanded* nil)
  (dolist (ip *idiom-phrases*)
    (let ((iptdfs (get-tdfs-given-id (car ip))))
      (when iptdfs
        (let ((ipmrs (mrs::extract-mrs-from-fs (tdfs-indef iptdfs))))
          (when ipmrs
            (let ((ipstruct 
                   (make-idiom-phrase :id ip
                                      :tdfs iptdfs
                                      :mrs ipmrs)))
              (push ipstruct *idiom-phrases-expanded*))))))))

#| checking idioms meet conditions
0) check ordinary root conditions
1) check whether it's a non-idiom, according to the non-idiom root
- if it is, return
2) else - for each idiom in *idiom-phrases*
a) extract MRS from the idiom-phrase (actually cached of course)
b) extract MRS from the phrase to be checked (ditto)
c) check whether the relations from the idiom-phrase are all in the
phrase to be checked with the correct coindexations (we require
that coindexations actually exist, since at this point we're assuming
that non-equality means different variables)
d) check that all idiomp things in the phrase-to-be-checked are accounted for
(so we don't allow any bits of idioms).  Optionally, check for complete 
idioms in another round, but leave this for now.
e) check whether the idiom-phrase minus the LISZT is yaduable with
the phrase to be checked minus the LISZT (leave this for now)
|#

(defun idiom-check (phrase-tdfs)
  ;;; this should only be called on a structure which has met 
  ;;; one of the `normal' root conditions. 
  #+:debug
  (setf foo phrase-tdfs)
  ;;
  ;; for backwards compatibility, allow null() *non-idiom-root* to make all
  ;; candidate edges pass the test.                           (5-apr-04; oe)
  ;;
  (let ((irtdfs (and *non-idiom-root* (get-tdfs-given-id *non-idiom-root*))))
    (or (null irtdfs)
        (yaduablep phrase-tdfs irtdfs)
        (let ((phrase-mrs (mrs::extract-mrs-from-fs (tdfs-indef phrase-tdfs))))
          (dolist (idiom *idiom-phrases-expanded*)
            (let ((idiom-mrs (idiom-phrase-mrs idiom)))
              (when (and 
                     (mrs::psoa-liszt idiom-mrs)
                     (idiom-phrase-match idiom-mrs phrase-mrs))
     #+:debug               (mrs::output-mrs1 idiom-mrs :simple t) 
       #+:debug           (format t "~%~A" (car (idiom-phrase-id idiom)))
                (return t))))))))

(eval-when #+:ansi-eval-when (:load-toplevel :execute)
	   #-:ansi-eval-when (load eval)
  (setf *additional-root-condition* #'idiom-check))

(defun idiom-phrase-match (ipmrs testmrs)
  (multiple-value-bind
      (ok unmatched-rels)
      (mrs::mrs-subset-p ipmrs testmrs nil)
    (and ok
      (loop for unmatched in unmatched-rels
          always
            (loop for set-el in unmatched
                  always               
                  (not (idiom-rel-p set-el)))))))

(defun idiom-rel-p (rel)
  ;;; FIX
  ;;; relation name ends with _i_rel - this won't quite do because
  ;;; we want to allow for different senses and anyway this should use the
  ;;; standard pred parsing code
  (let* ((relpred (mrs::rel-pred rel))
         (relname (when relpred (string relpred))))
    (and relname
         (equal "_i_rel" (subseq relname (max 0
					      (- (length relname) 6)))))))

#|
old definition was leading i_

(defun idiom-rel-p (rel)
  ;;; cheat for now
  (let ((relname (string (mrs::rel-pred rel))))
    (and (char-equal (elt relname 0) #\i)
         (char-equal (elt relname 1) #\_))))
|#

(defun get-idiom-entry (id)
  (cdr (assoc id *idiom-phrases*)))

; Added stub for function now called in lexinput.lsp
(defun clear-idioms-entries ()
  (setf *idiom-phrases* nil)
  (setf *idiom-phrases-expanded* nil))
