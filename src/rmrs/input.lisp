;;; Copyright (c) 2003--2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :mrs)

;;; reading in an rmrs file as specced in rmrs.dtd
;;;
;;; some of the fns are similar to those in readgram
;;; but there are sufficient differences to make it preferable
;;; to handle them separately

#|
(read-rmrs-file "test.xml")
(read-rmrs-file "rmrs/annlt-test/test-select.rmrs" :rasp)
|#

(defun read-rmrs-file (file-name &optional origin)
  ;;; <!ELEMENT rmrs-list (rmrs)*>
  ;;; <!ATTLIST rmrs-list
  ;;;        origin (RASP|ERG) #REQUIRED >
  (let ((*package* (find-package :mrs)))
    (with-open-file (istream file-name :direction :input)
      (let ((rmrss (parse-xml-removing-junk istream)))
	(unless (equal (car rmrss) '|rmrs-list|)
	  (error "~A is not a valid rmrs file" file-name))
	(loop for rmrs in (cdr rmrss)
	    unless (xml-whitespace-string-p rmrs)
	    collect
	      (read-rmrs rmrs origin))))))

(defun read-single-rmrs-from-string (str)
  ;;; currently called from emacs interface - 
  ;;; lkb::display-rmrs-from-string in lkb-acl-rmrs.lisp
  ;;; and also generate etc
  ;;; this takes a string containing an rmrs (we hope) and
  ;;; reads it in
  (let ((*package* (find-package :mrs)))
    (with-input-from-string (istream str)
      (let ((rmrs (parse-xml-removing-junk istream)))
	(unless (xml-whitespace-string-p rmrs)
	  (read-rmrs rmrs :rasp))))))
  

(defun read-rmrs (content origin)
;;; <!ELEMENT rmrs (label, (ep|rarg|ing|hcons)*)>
;;; FIX - attributes not dealt with
;;; <!ATTLIST rmrs
;;;          cfrom CDATA #REQUIRED
;;;          cto   CDATA #REQUIRED 
;;;          surface   CDATA #IMPLIED 
;;;          ident     CDATA #IMPLIED >
  (let ((top-h nil) (eps nil) (rargs nil) (ings nil)
        (h-cons nil)
	(tag (car content)))
    (unless (eql (car tag) '|rmrs|)
      (error "~A is not an rmrs" content))
    (setf content (cdr content))
    (loop (let ((next-el (car content)))
	    (if (xml-whitespace-string-p next-el)
		(pop content)
	      (return))))
    (when content
      (setf top-h (read-rmrs-label (car content)))
      (loop for next-el in (cdr content)
	  do
	    (unless (xml-whitespace-string-p next-el)
	      (let*  
		  ((next-tag (car next-el))
		   (body (cdr next-el)))
		(cond 
		 ((eql next-tag '|rarg|)
		  (push (read-rmrs-rarg body)
			rargs))
		 ((eql next-tag '|ing|)
		  (push (read-rmrs-in-g body)
			ings))
		 ((atom next-tag)
		  (error "Unexpected element ~A" next-el))
		 ((eql (car next-tag) '|ep|)
		  (push (read-rmrs-ep next-el)
			eps))
		 ((eql (car next-tag) '|hcons|)
		  (push (read-rmrs-hcons next-el)
			h-cons))
		 (t (error "Unexpected element ~A" next-el))))))
      (make-rmrs :top-h top-h
		 :liszt (nreverse eps)
		 :h-cons (nreverse h-cons)
		 :rmrs-args (nreverse rargs)
		 :in-groups (nreverse ings)
		 :origin origin))))

(defun read-rmrs-ep (content)
;;; <!ELEMENT ep ((realpred|gpred), label, var)>
;;; <!ATTLIST ep
;;;          cfrom CDATA #REQUIRED
;;;          cto   CDATA #REQUIRED 
;;;          surface   CDATA #IMPLIED 
;;;          base      CDATA #IMPLIED > 
  (let ((tag (car content))
        (body (cdr content)))
    (unless (and 
             (eql (first tag) '|ep|)
             (eql (second tag) '|cfrom|)
             (eql (fourth tag) '|cto|)
             (or (not (sixth tag)) 
                 (eql (sixth tag) '|surface|))
	     (or (not (eighth tag)) 
                 (eql (eighth tag) '|base|)))
      ;;; base is allowed but ignored
      (error "Malformed ep ~A" content))
    (setf body (loop for x in body
		   unless (xml-whitespace-string-p x)
		   collect x))
    (make-char-rel 
     :pred (read-rmrs-pred (first body))
              :handel (read-rmrs-label (second body))
              :flist (list (read-rmrs-var (third body)))
              :cfrom (parse-integer (third tag))
              :cto (parse-integer (fifth tag))
              :str (seventh tag))))

(defun read-rmrs-pred (content)
  (let ((tag (car content))
        (body (cdr content)))
    (cond 
     ((eql tag '|pred|) (make-dummy-pred))
     ((eql tag '|gpred|) (string-downcase (car body)))
         ;;; <!ELEMENT gpred (#PCDATA)>
     ((atom tag)
      (error "Unexpected element ~A" content))
     ((eql (car tag) '|realpred|)
      (read-rmrs-real-pred tag))
     (t (error "Unexpected element ~A" content)))))

(defun read-rmrs-real-pred (tag)
;;; <!ELEMENT realpred EMPTY>
;;;
;;; <!ATTLIST realpred
;;;          lemma CDATA #REQUIRED
;;;          pos (v|n|j|r|p|q|c|x|u|a|s) #REQUIRED
;;;          sense CDATA #IMPLIED >
    (unless (and 
             (eql (first tag) '|realpred|)
             (eql (second tag) '|lemma|))
      (error "Malformed realpred ~A" tag))
    (let ((pos-rest (member '|pos| tag))
          (sense-rest (member '|sense| tag)))
      (make-realpred :lemma (string-downcase (third tag))
                      :pos (cadr pos-rest)
                      :sense (cadr sense-rest))))


(defun read-rmrs-label (content)
;;; <!ELEMENT label EMPTY>
;;;
;;; <!ATTLIST label 
;;;          vid CDATA #REQUIRED >
  (let ((tag (car content))
        (body (cdr content)))
    (unless (and 
             (eql (first tag) '|label|)
             (eql (second tag) '|vid|)
             (null body))
      (error "Malformed label ~A" content))
    (create-new-label-with-id (parse-integer (third tag)))))

(defun create-new-label-with-id (idnumber)
  (make-var 
   :type "h"
   :id idnumber))


(defun read-rmrs-var (content)
;;; <!ELEMENT var EMPTY>
;;; <!ATTLIST var
;;;          sort (x|e|h|u|l) #REQUIRED
;;;          vid  CDATA #REQUIRED 
;;;          see DTD for current attributes
  (let ((tag (car content))
        (body (cdr content)))
    (unless (and 
             (eql (first tag) '|var|)
             (eql (second tag) '|sort|)
             (eql (fourth tag) '|vid|)
             (null body))
      (error "Malformed variable ~A" content))
    (create-new-var-with-id (parse-integer (fifth tag)) 
                            (third tag)
			    (construct-rmrs-var-extras 
			     (nthcdr 5 tag)))))

(defun construct-rmrs-var-extras (extra-list)
  (if extra-list
      (let ((feat (car extra-list))
	    (val (cadr extra-list)))
	(unless (and feat val)
	  (error "Malformed variable extras ~A" extra-list))
	(cons 
	 (make-extrapair :feature feat :value val)
	 (construct-rmrs-var-extras (cddr extra-list))))))

(defun create-new-var-with-id (idnumber str &optional extras)
  (let* ((type (find-var-type str)))
        (make-var 
         :type type
         :id idnumber
	 :extra extras)))

(defun read-rmrs-constant (content)
;;;  <!ELEMENT constant (#PCDATA)>
  (read-rmrs-simple '|constant| content))
  
(defun read-rmrs-rarg (content)
  ;;; <!ELEMENT rarg (rargname, label, (var|constant)) >
  (let ((name nil) (label nil) (arg nil))
    (setf name (read-rmrs-simple '|rargname| (car content)))
    ;;; <!ELEMENT rargname (#PCDATA)>
    (setf label (read-rmrs-label (cadr content)))
    (let* ((argval (caddr content))
           (argvaltag (car argval)))
      (setf arg
        (if (eql argvaltag '|constant|)
            (read-rmrs-constant argval)
          (read-rmrs-var argval))))
    (make-rmrs-arg :arg-type name :label label 
                   :val arg)))
  
(defun read-rmrs-in-g (content)
;;; <!ELEMENT ing (ing-a,ing-b) >
;;; <!ELEMENT ing-a (var)>
;;; <!ELEMENT ing-b (var)>  
  (let ((ing-a-xml (car content))
        (ing-b-xml (cadr content)))
    (unless (and (eql (car ing-a-xml) '|ing-a|)
                 (eql (car ing-b-xml) '|ing-b|))
      (error "Malformed ing ~A" content))
    (make-in-group :label-a (read-rmrs-var (cadr ing-a-xml))
                   :label-b (read-rmrs-var (cadr ing-b-xml)))))
                                 

(defun read-rmrs-hcons (content)
;;; <!ELEMENT hcons (hi, lo)>
;;; <!ATTLIST hcons 
;;;          hreln (qeq|lheq|outscopes) #REQUIRED >
;;;
;;; <!ELEMENT hi (var)>
;;; <!ELEMENT lo (label|var)>
  (let ((tag (car content))
        (body (cdr content)))
    (unless (and (eql (first tag) '|hcons|)
                 (eql (second tag) '|hreln|)
                 (eql (first (first body)) '|hi|)
                 (eql (first (second body)) '|lo|))
      (error "Malformed hcons ~A" content))
    (make-hcons :relation (third tag)
                :scarg (read-rmrs-var (second (first body)))
                :outscpd (read-rmrs-label (second (second body))))))
                

