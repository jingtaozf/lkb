;;; Lisp structures for VITs and basic functions for reading 
;;; and writing VITS

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
        (ambiguities nil))
    (check-for #\v istream)
    (check-for #\i istream)
    (check-for #\t istream)
    (check-for #\( istream)
    (setf utterance-id (read-p-form istream))
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
    (setf sorts (read-p-list istream))
    (check-for #\, istream)
    (read-p-comment istream)
    (setf discourse (read-p-list istream))
    (check-for #\, istream)
    (read-p-comment istream)
    (setf syntax (read-p-list istream))
    (check-for #\, istream)
    (read-p-comment istream)
    (setf tenseandaspect (read-p-list istream))
    (check-for #\, istream)
    (read-p-comment istream)
    (setf prosody (read-p-list istream))
    (check-for #\, istream)
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
            (let* ((first-element (read istream))
                   (next-char (peek-char t istream nil 'eof)))
              (cond ((eql next-char #\() (read-p-term istream first-element))
                    ((member next-char '(#\, #\) #\])) first-element)
                    (t (error "Unexpected character after constant")))))
           (t (error "Unexpected character in term")))))
  

(defun read-p-constant (istream)
  (read istream))

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



;;;;;;;;;;;;;;;;;; other general access and create functions


(defun vitAddSort (sort inst vit)
  (if (member sort *vm-ignored-sort-list*)
      vit
    (setf (vit-sorts vit) 
      (cons (make-vit_sort :args (list inst sort))
            (vit-sorts vit)))))

