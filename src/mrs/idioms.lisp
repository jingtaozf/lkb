;;; Copyright (c) 2002 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions

(in-package :lkb)

;;; (defparameter *idiom-phrases* nil)
;;; set in io-paths/lexinput.lsp

(defparameter *idiom-phrases-expanded* nil)

(defstruct idiom-phrase
  id
  tdfs
  mrs)

(defun expand-idioms-phrases nil
  (setf *idiom-phrases-expanded* nil)
  (dolist (ip *idiom-phrases*)
    (let ((iptdfs (get-tdfs-given-id ip)))
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
  (let ((irtdfs (get-tdfs-given-id *non-idiom-root*)))
    (if irtdfs
        (or (yaduablep phrase-tdfs irtdfs)
            (let ((phrase-mrs (mrs::extract-mrs-from-fs 
                               (tdfs-indef phrase-tdfs))))
              (dolist (idiom *idiom-phrases-expanded*)
                (let ((idiom-mrs (idiom-phrase-mrs idiom)))
                  (when (idiom-phrase-match idiom-mrs phrase-mrs)
                    (return t)))))))))

(defparameter *additional-root-condition* #'idiom-check)

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
  ;;; cheat for now
  (let ((relname (string (mrs::rel-sort rel))))
    (and (char-equal (elt relname 0) #\i)
         (char-equal (elt relname 1) #\_))))

    

