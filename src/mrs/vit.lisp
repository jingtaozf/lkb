;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   $RCSfile$
;;  $Revision$
;;      $Date$
;;     Author: Ann Copestake (CSLI)/Walter Kasper (DFKI)
;;    Purpose: Lisp structures for VITs and basic functions for reading and writing VITS
;;   Language: Allegro Common Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; $Log$
;; Revision 1.5  1999/05/14 02:25:29  aac
;; allowing generator index to be cleared, tidying up to avoid compiler warnings
;;
;; Revision 1.4  1999/05/07 00:34:08  aac
;; minor fixes, mostly problems revealed by MCL
;;
;; Revision 1.3  1999/04/09 23:20:56  danf
;; Merged WK's changes
;;
;; Revision 1.2  1998/10/28 13:50:23  kasper
;; structures for vit-variables
;;
;; Revision 1.1  1998/10/28 10:37:56  kasper
;; Initial revision
;; 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MRS")


(defparameter %vit-indent% ", ")
;; to do pretty printing, set it to ",~%    "

;;; ****** Structures ********
;;;
;;; This may need fleshing out later - currently parts of the
;;; VIT are being left relatively unanalysed

;;; new specification: vit/9 (without ambiguities)
(defstruct (vit)
  utterance-id
  semantics
  main-condition
  sorts
  discourse
  syntax
  tenseandaspect
  scope
  prosody)

(defstruct (p-term (:print-function print-p-term))
  predicate
  args)

(defstruct (p-struct)
  operator
  args)

;;; segment descriptions with defaults for standalone
(defstruct (sid (:print-function print-sid-term))
  (turnnumber 1)
  (channel 's)
  (sourcelanguage 'de)
  (begintime 0)
  (endtime 10)
  (reading 1)
  (currentlanguage 'de)
  (turnend 'y)
  (sender 'syntaxger))

(defun print-sid-term (sid stream level)
  (declare (ignore level))
  (format stream "sid(~A,~A,~A,~A,~A,~A,~A,~A,~A)"
          (sid-turnnumber sid)
          (sid-channel sid)
          (sid-sourcelanguage sid)
          (sid-begintime sid)
          (sid-endtime sid)
          (sid-reading sid)
          (sid-currentlanguage sid)
          (sid-turnend sid)
          (sid-sender sid)))

(defstruct (vit-special-form (:include p-term)
            (:print-function print-vit-special-form))
  instance)

(defstruct (vit_sort (:include vit-special-form (predicate 's_sort))))

(defstruct (vit_tense (:include vit-special-form (predicate 'ta_tense))))

(defstruct (vit_aspect (:include vit-special-form (predicate 'ta_aspect))))

(defstruct (vit_mood (:include vit-special-form (predicate 'ta_mood))))

(defstruct (vit_perf (:include vit-special-form (predicate 'ta_perf))))

(defstruct (vit_gender (:include vit-special-form (predicate 'gend))))

(defstruct (vit_number (:include vit-special-form (predicate 'num))))

(defstruct (vit_person (:include vit-special-form (predicate 'pers))))

(defstruct (vit_dir (:include vit-special-form (predicate 'dir))))

(defstruct (vit_prontype (:include vit-special-form (predicate 'prontype))))

(defstruct (vit_case (:include vit-special-form (predicate 'cas))))

(defstruct (vit_pcase (:include vit-special-form (predicate 'pcase))))

(defstruct (vit_accent (:include vit-special-form (predicate 'pros_accent))))

(defstruct (vit_pmood (:include vit-special-form (predicate 'pros_mood))))


(defstruct (vit-var)
  (id 0 :type integer))

(defstruct (vit-hole-var (:include vit-var)
            (:print-function print-vit-hole-var)))

(defstruct (vit-label-var (:include vit-var)
            (:print-function print-vit-label-var)))

(defstruct (vit-instance-var (:include vit-var)
            (:print-function print-vit-instance-var)))

;;;; printers


;;; predicates are printed verbatim when presented as string, otherwise
;;; they are printed in downcase. If this should be insufficient we can
;;; change to excessive quoting
(defun print-p-term (fun stream level)
  (declare (ignore level))
  (let ((pred (p-term-predicate fun))
        (args (p-term-args fun)))
    (if (stringp pred)
        (format stream "~A" pred)
      (format stream "~(~A~)" pred))
    (when args
      (format stream "(~(~A~@[~{,~A~}~]~))" (first args) (rest args)))))

(defun print-vit-special-form (form stream level)
  (declare (ignore level))
  (format stream "~(~A(~A~@[~{,~A~}~])~)" (vit-special-form-predicate form)
          (vit-special-form-instance form)
          (vit-special-form-args form)))


(defun print-vit-hole-var (var stream level)
  (declare (ignore level))
  (format stream "~(~a~a~)" *vit-hole-prefix* (vit-var-id var)))

(defun print-vit-label-var (var stream level)
  (declare (ignore level))
  (format stream "~(~a~a~)" *vit-label-prefix* (vit-var-id var)))

(defun print-vit-instance-var (var stream level)
  (declare (ignore level))
  (format stream "~(~a~a~)" *vit-instance-prefix* (vit-var-id var)))

(defun vit-vars-eq (v1 v2)
  (and (eq (type-of v1)
           (type-of v2))
       (eq (vit-var-id v1)
           (vit-var-id v2))))

;;; Not sure whether VIT is allowing all Prolog operators
;;; or what

(defparameter *vit-operator-list* '(#\;))

(defun convert-operator-to-vit (operator)
  (case operator
    (disjunction #\;)
    (t (format t "~%Warning: unexpected operator in lispified VIT ~A"
               operator)
       "XXX")))

(defun convert-operator-from-vit (operator)
  (case operator
    (#\; 'disjunction)
    (t (format t "~%Warning: unexpected operator in VIT ~A"
               operator)
       'XXX)))

;;; sym ist symbol/string, suffix ist string; return ist string
(defun remove-name-suffix (sym suffix)
  (let* ((str (cond ((symbolp sym) (string-downcase (symbol-name sym)))
                    (t sym)))
         (strl (length str))
         (sufl (length suffix))
         (sufstart (- strl sufl)))
    ;; a temporary hack until the 'underscore accident' is repaired
    (if (mrs-language '(german japanese))
        (setf str (substitute #\_ #\- str)))
    (if (< sufstart 1)
        str
      (if (string= str suffix :start1 sufstart)
          (subseq str 0 sufstart)
        str))))

(defun remove-name-prefix (sym prefix)
  (let* ((str (cond ((symbolp sym) (string-downcase (symbol-name sym)))
                    (t sym)))
         (strl (length str))
         (sufl (length prefix)))
    ;; a temporary hack until the 'underscore accident' is repaired
    (if (mrs-language '(german japanese))
        (setf str (substitute #\_ #\- str)))
    (if (< strl sufl)
        str
      (if (string= str prefix :end1 sufl)
          (subseq str sufl)
        str))))

;;; ******* reading VIT  **********
;;; all fairly boring and obvious

(defun define-break-characters (char-list)
   (let ((temporary-readtable (copy-readtable *readtable*)))
      (dolist (break-char char-list)
         (set-macro-character break-char
            #'(lambda (stream x) (declare (ignore stream)) x)
            nil
            temporary-readtable))
      temporary-readtable))

(defun check-for (character istream)
   (let ((next-char (peek-char t istream nil 'eof)))
      (unless (eql next-char character)
         (error "~%~A Expected and not found" character))
      (read-char istream)))

;;; (read-vit-file "Macintosh HD:lkb96:vit:july-ex")

(defun read-vit-file (filename)
  ;;; returns a list of VIT structures
   (let ((*readtable*
            (define-break-characters 
               '(#\[ #\] #\,))))
     (with-open-file (istream filename :direction :input)
       (do* ((vitstruct (read-vit istream) (read-vit istream))
             (results (if vitstruct (list vitstruct)) 
                      (if vitstruct (cons vitstruct results)))
             (next-char (peek-char t istream nil 'eof)))
            ((or (null vitstruct) (eql next-char 'eof)) (return results))))))

;;; DPF 21-Jul-99 - Added from WK
(defun read-vit-string (vitstring)
  (let ((*readtable*
            (define-break-characters 
                '(#\[ #\] #\,))))
    (read-vit (make-string-input-stream vitstring))))

(defun read-vit (istream)
  (let ((utterance-id nil)
        (semantics nil)
        (main-condition nil)
        (sorts nil)
        (discourse nil)
        (syntax nil)
        (tenseandaspect nil)
        (scope nil)
        (prosody nil)
;        (ambiguities nil)
        )
    (check-for #\v istream)
    (check-for #\i istream)
    (check-for #\t istream)
    (check-for #\( istream)
    (setf utterance-id (read-utterance-id istream))
    (check-for #\, istream)
    (read-p-comment istream)
    (setf main-condition (read-p-form istream))
    (check-for #\, istream)
    (read-p-comment istream)
    (setf semantics (read-p-list istream))
    (check-for #\, istream)
    (read-p-comment istream)
    (setf scope (read-p-list istream))
    (check-for #\, istream)
    (read-p-comment istream)
    (setf sorts (pterms2special-form (read-p-list istream)))
    (check-for #\, istream)
    (read-p-comment istream)
    (setf discourse (pterms2special-form (read-p-list istream)))
    (check-for #\, istream)
    (read-p-comment istream)
    (setf syntax (pterms2special-form (read-p-list istream)))
    (check-for #\, istream)
    (read-p-comment istream)
    (setf tenseandaspect (pterms2special-form (read-p-list istream)))
    (check-for #\, istream)
    (read-p-comment istream)
    (setf prosody (pterms2special-form (read-p-list istream)))
    (read-p-comment istream)
    (check-for #\) istream)
    (read-p-comment istream)
;    (setf ambiguities (read-p-list istream))
;    (read-p-comment istream)
    (check-for #\) istream)
    (read-p-comment istream)
    (make-vit   :utterance-id utterance-id
                :semantics semantics
                :main-condition main-condition
                :sorts sorts
                :discourse discourse
                :syntax syntax
                :tenseandaspect tenseandaspect
                :scope scope
                :prosody prosody)
    ))

(defun read-p-form (istream)
  (read-p-comment istream)
  (let ((next-char (peek-char t istream nil 'eof)))
    (cond  ((eql next-char 'eof) (error "~%Form terminated by eof"))
           ((eql next-char #\[) (read-p-list istream))
           ((eql next-char #\() (read-p-struct istream))
           ((eql next-char #\') (read-quoted-stuff istream))
           ((alphanumericp next-char) 
            (let* ((first-element (read-p-constant istream))
                   (next-char (peek-char t istream nil 'eof)))
              (cond ((eql next-char #\() (read-p-term istream first-element))
                    ((member next-char '(#\, #\) #\])) first-element)
                    (t (error "Unexpected character after constant")))))
	   ;;; DPF 21-Jul-99 - Added from WK
	   ((member next-char *vit-operator-list* :test #'char=)
            (let* ((first-element (make-string 1 :initial-element 
					       (read-char istream nil 'eof)))
                   (next-char (peek-char t istream nil 'eof)))
              (cond ((eql next-char #\() (read-p-term istream first-element))
                    ((member next-char '(#\, #\) #\])) first-element)
                    (t (error "Unexpected character after constant")))))
           (t (error "Unexpected character in term")))))
  

;;; DPF 21-Jul-99 - Added from WK
(defun read-p-constant (istream)
  (pterm2vit-var (read istream)))

(defun read-p-comment (istream)
  (let ((next-char (peek-char t istream nil 'eof)))
    (when (eql next-char #\%)
      (read-line istream)
      (read-p-comment istream))))

(defun read-quoted-stuff (istream)
  (check-for #\' istream)
  (let ((chars nil))
    (loop (let ((next-char (read-char istream nil 'eof)))
            (when (eql next-char #\') (return nil))
            (when (eql next-char 'eof) (error "No terminating '"))
            (push next-char chars)))
    (coerce (nreverse chars) 'string)))

(defun read-p-list (istream)
  (let ((reslist nil))
  (check-for #\[ istream)  
  (loop
    (read-p-comment istream)
    (let ((next-char (peek-char t istream nil 'eof)))
      (when (eql next-char 'eof) (error "~%List terminated by eof"))
      (when (char= next-char #\]) (return nil))      
      (when (char= next-char #\,) (read-char istream))
      (push (read-p-form istream) reslist)))
  (check-for #\] istream)
  (nreverse reslist)))

(defun read-p-term (istream predicate)
  (let ((args nil))
      (check-for #\( istream)  
  (loop
    (let ((next-char (peek-char t istream nil 'eof)))
      (when (eql next-char 'eof) (error "~%Args terminated by eof"))
      (when (char= next-char #\)) (return nil))
      (when (char= next-char #\,) (read-char istream))
      (push (read-p-form istream) args)))
  (check-for #\) istream)
    (make-p-term :predicate predicate :args (nreverse args))))

(defun read-p-struct (istream)
  (check-for #\( istream)
  (let ((reslist nil)
        (operator nil))
    (loop
      (let ((next-char (peek-char t istream nil 'eof)))
        (cond ((eql next-char 'eof) (error "~%Structure terminated by eof"))
              ((char= next-char #\)) (return nil))
              ((alphanumericp next-char)
               (push (read-p-form istream) reslist))
              ((member next-char *vit-operator-list* :test #'char=) 
               (read-char istream)
               (setf operator (convert-operator-from-vit next-char)))             
              (t (error "Unexpected character in struct")))))
    (check-for #\) istream)
    (make-p-struct :operator operator :args (nreverse reslist))))
  
;------------------------------------------------------------------------------
;;; DPF 21-Jul-99 - Added Following four functions from WK

;;; add some type checking for robustness?
(defun read-utterance-id (istream)
  (let* ((term (read-p-form istream))
         (id (p-term-args (first (p-term-args term))))
         (whgs (second (p-term-args term))))
    (setf (p-term-predicate term) "vitID"
          (p-term-args term)
           (list
            (make-sid :turnnumber (first id)
                      :channel (second id)
                      :sourcelanguage (third id)
                      :begintime (fourth id)
                      :endtime (fifth id)
                      :reading (sixth id)
                      :currentlanguage (seventh id)
                      :turnend (eighth id)
                      :sender (ninth id))
            (loop for whg in whgs
                collect
                  (make-whg-id :id (second (p-term-args whg))
                               :word (first (p-term-args whg))
                               :handel (loop for handel in 
                                             (third (p-term-args whg))
                                           collect
                                             (pterm2vit-var handel))))))
    term))


(defun pterm2vit-var (varsym)
        (if (and (symbolp varsym) 
                 (member (elt (symbol-name varsym) 0) 
                         '(#\L #\H #\I) :test #'char=))
            (multiple-value-bind (type number)
                (split-vit-var-symbol varsym)
              (case type
                (h (make-vit-hole-var :id number))
                (i (make-vit-instance-var :id number))
                (l (make-vit-label-var :id number))
                (otherwise varsym)))
          varsym))
      
;;; instead use regexp-library?
(defun split-vit-var-symbol (varsym)
  (let* ((varname (symbol-name varsym))
         ;;; assume that all variables have the same prefix length
         (prefixlength (length *vit-hole-prefix*))
         (prefix (when (> (length varname) prefixlength)
                   (subseq varname 0 prefixlength)))
         (numval (when (> (length varname) prefixlength)
                      (read-from-string (subseq varname prefixlength))))
         (type nil))
    (if prefix
        (cond ((equal prefix *vit-instance-prefix*)
               (setq type 'i))
              ((equal prefix *vit-label-prefix*)
               (setq type 'l))
              ((equal prefix *vit-hole-prefix*)
               (setq type 'h))
              (t nil)))
    (if (integerp numval)
        (values type numval))))
    
(defun pterms2special-form (pterms)
  (cond ((consp pterms) (mapcar #'pterms2special-form pterms))
        ((p-term-p pterms)
         (let ((special (case (p-term-predicate pterms)
                          (s_sort (make-vit_sort))
                          (ta_tense (make-vit_tense))
                          (ta_aspect (make-vit_aspect))
                          (ta_mood (make-vit_mood))
                          (ta_perf (make-vit_perf))
                          (gend (make-vit_gender))
                          (num (make-vit_number))
                          (pers (make-vit_person))
                          (dir (make-vit_dir))
                          (prontype (make-vit_prontype))
                          (case (make-vit_case))
                          (pcase (make-vit_pcase))
                          (pros_accent (make-vit_accent))
                          (pros_mood (make-vit_pmood))
                          (t nil))))
           (if special
               (setf (vit-special-form-instance special) (first (p-term-args pterms))
                     (vit-special-form-args special) (rest (p-term-args pterms))))
           (or special pterms)))
        (t pterms)))


;;; ********* Writing VIT ************

#|
(write-vits "Macintosh HD:lkb96:vit:july-ex-out6" 
            (read-vit-file "Macintosh HD:lkb96:vit:july-ex"))
|#

(defun write-vits (filename list-of-vits)
  ;;; does not output the comments
  (with-open-file (vit-out filename :direction :output)
    (dolist (vit list-of-vits)
      (write-vit vit-out vit))))

;;; adapted to 9-place stuff
(defun write-vit (vit-out vit &optional pretty)
  (let ((*print-circle* nil)
	(*print-pretty* pretty))
    (format vit-out "vit( ")
    (output-p-form vit-out (vit-utterance-id vit))
    (format vit-out %vit-indent%)
    (output-p-form vit-out (vit-main-condition vit))
    (format vit-out %vit-indent%)
    (output-p-list vit-out (vit-semantics vit))
    (format vit-out %vit-indent%)
    (output-p-list vit-out (vit-scope vit))
    (format vit-out %vit-indent%)
    (output-p-list vit-out (vit-sorts vit))
    (format vit-out %vit-indent%)
    (output-p-list vit-out (vit-discourse vit))
    (format vit-out %vit-indent%)
    (output-p-list vit-out (vit-syntax vit))
    (format vit-out %vit-indent%)
    (output-p-list vit-out (vit-tenseandaspect vit))
    (format vit-out %vit-indent%)
    (output-p-list vit-out (vit-prosody vit))
    (format vit-out ")")))


(defun write-vit-pretty (vit-out vit)
  (let ((%vit-indent% ",~%    "))
    (write-vit vit-out vit T)))

      
(defun output-p-form (vit-out form)
  ;;; the generic output function for something that could be 
  ;;; a list, a p-struct, a constant
  (cond ((listp form) 
         (output-p-list vit-out form))
        ((p-struct-p form)
         (output-p-struct vit-out form))
        ((p-term-p form)
         (output-p-term vit-out form))
        ((whg-id-p form)
         (format vit-out "~A" form))
        ((stringp form)
         (format vit-out "'~A'" form))
        ((symbolp form)
         (format vit-out "~(~A~)" form))
        (t (format vit-out "~(~S~)" form))))

(defun output-p-list (vit-out form)
  (format vit-out "[")
  (let ((need-comma nil)) ; messiness because of comma delimiters
    (dolist (current-form form)
      (when need-comma
        (format vit-out %vit-indent%))
      (setf need-comma t)
      (output-p-form vit-out current-form))
  (format vit-out "]")))

(defun output-p-constant (vit-out form)
  (format vit-out "~(~A~)" form))

(defun output-p-term (vit-out p-term)
  (let ((pred (p-term-predicate p-term)))
    (if (stringp pred)
      (format vit-out "~A(" (p-term-predicate p-term))
      (format vit-out "~(~A(~)" (p-term-predicate p-term)))
    (let ((args (p-term-args p-term))
          (need-comma nil))             ; messiness because of comma delimiters
      (if (vit-special-form-p p-term)
          (setf args (cons (vit-special-form-instance p-term) args)))
    (dolist (current-arg args)
      (when need-comma
        (format vit-out ","))
      (setf need-comma t)
      (output-p-form vit-out current-arg)))
   (format vit-out ")")))


(defun output-p-struct (vit-out p-struct)
  (format vit-out "(")
  (let ((args (p-struct-args p-struct))
        (need-comma nil))
    (dolist (current-arg args)
      (when need-comma
        (format vit-out "~A"
                (convert-operator-to-vit (p-struct-operator p-struct))))
      (setf need-comma t)
      (output-p-form vit-out current-arg))
   (format vit-out ")")))

;;;; equality predicates for VITs

(defun vit-terms-equiv-p (rel1 rel2)
  (cond ((and (p-term-p rel1)
              (p-term-p rel2))
              (and (equalp (p-term-predicate rel1)
                           (p-term-predicate rel2))
                                        ; ignore the first arg (fixed type by specification)
                   (loop for arg1 in (rest (p-term-args rel1))
                       for arg2 in (rest (p-term-args rel2))
                                        ; for vit-vars only type is relevant
                       always (if (and (vit-var-p arg1)
                                       (vit-var-p arg2))
                                  (eq (type-of arg1)
                                      (type-of arg2))
                                (equalp arg1 arg2)))))
        (t nil)))

(defun vit-rellists-equiv-p (rels1 rels2)
  (when (= (length rels1) (length rels2))
    (loop for rel1 in rels1
        for rel2 in rels2
        always (vit-terms-equiv-p rel1 rel2))))

(defun vit-id-equiv-p (id1 id2)
  (if (and (p-term-p id1)
	   (p-term-p id2))
      (let ((sid1 (first (p-term-args id1)))
	    (sid2 (first (p-term-args id2))))
	(and (sid-p sid1) (sid-p sid2)
	     (= (sid-begintime sid1) (sid-begintime sid2))
	     (= (sid-endtime sid1) (sid-endtime sid2))))
    (eq id1 id2)))

;;; a simple form of equivalence check
;;; 1. relations must occur in same order
;;; 2 coreference is ignored
;;; 3. not every slot is considered
;;; according to agreement with C.J. (2.2.99) only semantics and
;;; sid-starttime/endtime are compared
(defun vits-equiv-p (vit1 vit2)
  (and (vit-p vit1) (vit-p vit2)
       (vit-id-equiv-p (vit-utterance-id vit1) (vit-utterance-id vit2))
       (vit-rellists-equiv-p (vit-semantics vit1) (vit-semantics vit2))
;       (vit-rellists-equiv-p (vit-sorts vit1) (vit-sorts vit2))
;       (vit-rellists-equiv-p (vit-discourse vit1) (vit-discourse vit2))
;       (vit-rellists-equiv-p (vit-syntax vit1) (vit-syntax vit2))
;       (vit-rellists-equiv-p (vit-tenseandaspect vit1) (vit-tenseandaspect vit2))
;       (vit-rellists-equiv-p (vit-scope vit1) (vit-scope vit2))
;       (vit-rellists-equiv-p (vit-prosody vit1) (vit-prosody vit2))
;       (vit-terms-equiv-p (vit-utterance-id vit1) (vit-utterance-id vit2))
       ))

;;;;;;;;;;;;;;;;;; other general access and create functions


(defun vitAddSort (sort inst vit)
  (if (member sort *vm-ignored-sort-list*)
      vit
    (setf (vit-sorts vit) 
      (cons (make-vit_sort :args (list inst sort))
            (vit-sorts vit)))))

