;;; Reorganised MRS code -
;;; this is the basic MRS file, which defines the lisp structures
;;; used for internally encoding MRS.  The other files are
;;; 
;;; 1. mrsoutput.lsp - this will differ between the TDL and LKB versions
;;;    - it constructs an MRS structure from a FS
;;; 2. mrsresolve.lsp - takes an MRS structure and returns a set of
;;; scope resolved structures
;;; 3. mrscons.lsp - dealing with the hcons structures
;;;
;;; in general, for.lsp is required


(in-package "MRS")

;;; MRS structs
;;; because I want to be able to do other things with MRSs besides
;;; just printing them, this stuff defines a set of structures
;;; for MRS.  Note that these structures are simple, because
;;; reentrancies have been replaced by variables.
;;; For VITs we need the top-handel and wgliszt

(defstruct (psoa)
  extras
  handel
  top-h
  index
  liszt
  h-cons
  message
  wgliszt
  key-h)


(defstruct (rel)
  extra                                 ; WK: used for non-argument features
  type                                  ; type might differ from relation
  sort                                  ; relation name
  handel
  label
  flist)


(defstruct (fvpair)
  feature
  value)

;;; feature may either be a symbol, or, for the LKB version only,
;;; a path structure (as defined in struct)

;;; value is either a constant (simple atom) or
;;; a var structure which contains a string plus a number 
;;; (unique to this MRS) or a list of var structures

(defstruct (var)
  name
  type
  extra ; useful for e.g. agreement values
  id)

(defstruct (handle-var (:include var)))

(defstruct (group-var (:include handle-var)))

(defstruct (hcons)
  scarg
  cands
  outscpd)

(defstruct (leq-sc (:include hcons))
  relation)

;;; for VM-wordgraph identifiers
(defstruct (whg-id (:print-function print-whg-id))
  id
  word
  handel)

;;; some time move structures and printers to separate files
;(defun print-prolog-list (list stream level)
;  (declare (ignore level))
;  (let ((elements (prolog-list-members list)))
;    (if elements
;        (format stream "~([~A~@[~{,~A~}~]]~)" (first elements) (rest elements))
;      (format stream "[]"))))

(defun print-whg-id (whg stream level)
  (declare (ignore level))
  (let ((handels (whg-id-handel whg)))
    (format stream "word('~A',~A," 
            (whg-id-word whg)
            (whg-id-id whg))
    (if handels 
        (format stream "~([~A~@[~{,~A~}~]])~)" (first handels) 
                (rest handels))
      (format stream "[])"))))
          

         
;;; WK: with an ugly way to print out 'extra' infos (for German)
#-lkb
(defun get-print-name (var-struct)
  (if (var-p var-struct)
      (if (var-extra var-struct)
          (concatenate 'string (var-name var-struct) " " (format nil "~{ ~S~%~}" (var-extra var-struct)))
        (var-name var-struct))
  (format nil "u")))
  
#+lkb
(defun get-print-name (var-struct)
  (if (var-p var-struct)
    (format nil "~A ~{ ~A~}" (var-name var-struct) 
            (extra-value-strings (var-extra var-struct)))
    "u"))

(defun extra-value-strings (extra)
  (for fvp in extra
       filter
       (let* ((feature (last-path-feature (fvpair-feature fvp)))
              (abbrev (assoc feature *mrs-extra-display*)))
         (if abbrev
             (format nil "~A ~A"
                     (cdr abbrev)
                     (fvpair-value fvp))))))




;;; The MRS structure could be output either as simple ascii
;;; or as LaTeX and possibly in other ways
;;; So use the same trick as the LKB to avoid unnecessary work
;;; for different output types

(defparameter *mrs-display-structure* nil)

(defun def-print-operations (class indentation stream)
  (setf *mrs-display-structure* (make-instance class 
					   :indentation indentation
					   :stream stream)))


;;; 
;;; Generic output-type class
;;;

(defclass output-type ()
  ((indentation :initform 0 :initarg :indentation)
   (stream :initarg :stream)))

(defmethod mrs-output-error-fn ((mrsout output-type) mrs-instance)
  (with-slots (stream) mrsout
    (format stream "~%::: ~A is not a psoa struct~%" mrs-instance)))

(defmethod mrs-output-max-width-fn ((mrsout output-type))
  nil)

;;; 
;;; simple output-type class
;;;

(defclass simple (output-type) ())

(defmethod mrs-output-start-fn ((mrsout simple))
  (with-slots (stream) mrsout
    (format stream "~V%" 1)))

(defmethod mrs-output-end-fn ((mrsout simple))
  (with-slots (stream) mrsout
    (format stream "~V%" 1)))

(defmethod mrs-output-start-psoa ((mrsout simple) 
				  handel-val event-val)
  (with-slots (stream) mrsout
    (format stream "[ TOP: ~A~%" handel-val)
    (when event-val
      (format stream "  INDEX: ~A~%" event-val))))

(defmethod mrs-output-start-liszt ((mrsout simple))
  (with-slots (stream indentation) mrsout
    (format stream "  LISZT: <")
    (setf indentation (+ indentation 10))))

(defmethod mrs-output-atomic-fn ((mrsout simple) atomic-value)
  (with-slots (stream) mrsout
    (format stream "~A" atomic-value)))

(defmethod mrs-output-list-fn ((mrsout simple) list-value)
  (with-slots (stream) mrsout
    (format stream "(~{~A~})" list-value)))
  
(defmethod mrs-output-start-rel ((mrsout simple) sort handel)
  (with-slots (stream indentation) mrsout
    (format stream "~VT[ ~A" indentation (string-downcase sort))
    (format stream "~%~VT~A: ~A" (+ indentation 2) 'handel handel)))

(defmethod mrs-output-label-fn  ((mrsout simple) label)
  (with-slots (stream indentation) mrsout
    (format stream "~%~VT~A: " (+ indentation 2) label)))

(defmethod mrs-output-end-rel ((mrsout simple))
  (with-slots (stream) mrsout
    (format stream "]~%")))

(defmethod mrs-output-end-liszt ((mrsout simple))
  (with-slots (stream indentation) mrsout
    (format stream "~VT>" indentation)))

(defmethod mrs-output-start-h-cons ((mrsout simple))
  (with-slots (stream) mrsout
    (format stream "~%  H-CONS: <")))

(defmethod mrs-output-is-one-of ((mrsout simple) element candidates)
  (with-slots (stream indentation) mrsout
    (format stream "~VT" (+ indentation 2))
    (cond ((null candidates)
           (format stream "~A: {}" element))
          ((cdr candidates)
           (format stream "~A: {~A~{,~A~}}" element 
                   (car candidates) (cdr candidates)))
          (t (format stream "~A: {~A}" element 
                     (car candidates))))
    (format stream "~%")))

(defmethod mrs-output-outscopes ((mrsout simple) higher lower)
  (with-slots (stream indentation) mrsout
    (format stream "~VT~A >= ~A~%" 
            (+ indentation 2) higher lower)))

(defmethod mrs-output-leq ((mrsout simple) higher lower)
  (with-slots (stream indentation) mrsout
    (format stream "~VT~A =< ~A~%" 
            (+ indentation 2) higher lower)))

(defmethod mrs-output-end-h-cons ((mrsout simple))
  (with-slots (stream indentation) mrsout
    (format stream "~VT>" indentation)))

(defmethod mrs-output-end-psoa ((mrsout simple))
  (with-slots (stream indentation) mrsout
    (format stream " ]~%" indentation)))

;;; WK:
(defmethod mrs-output-var ((mrsout output-type) var)
  (with-slots (stream indentation) mrsout
    (mrs-output-atomic-fn *mrs-display-structure* (var-name var))
    (if (var-extra var)
        (let ((name (var-name var)))
          (loop for feat-val in (var-extra var)
              do
                (mrs-output-start-rel mrsout))))))

;;; 
;;; comment output-type class
;;;

(defclass comment (simple)
  ((comment-string :initform ";;; ")))

(defmethod mrs-output-start-psoa ((mrsout comment) 
				  handel-val event-val)
  (with-slots (stream comment-string) mrsout
    (format stream "~A  TOP: ~A~%" comment-string handel-val)
    (when event-val
      (format stream "~A  INDEX: ~A~%" comment-string event-val))))

(defmethod mrs-output-start-liszt ((mrsout comment))
  (with-slots (stream indentation comment-string) mrsout
    (format stream "~A  LISZT: <~%" comment-string)
    (setf indentation (+ indentation (+ 10 (length comment-string))))))

(defmethod mrs-output-start-rel ((mrsout comment) sort handel)
  (with-slots (stream indentation comment-string) mrsout
    (format stream "~A~VT[ ~A" comment-string 
	    indentation (string-downcase sort))
  (format stream "~%~A~VT~A: ~A" comment-string
	  (+ indentation 2) 'handel handel)))

(defmethod mrs-output-end-rel ((mrsout comment))
  (with-slots (stream) mrsout
    (format stream "]~%")))

(defmethod mrs-output-label-fn  ((mrsout comment) label)
  (with-slots (stream indentation comment-string) mrsout
    (format stream "~%~A~VT~A: " comment-string (+ indentation 2) label)))

(defmethod mrs-output-end-liszt ((mrsout comment))
  (with-slots (stream indentation comment-string) mrsout
    (format stream "~A~VT>" comment-string indentation)))

(defmethod mrs-output-start-h-cons ((mrsout comment))
  (with-slots (stream comment-string) mrsout
    (format stream "~%~A H-CONS: < " comment-string)))

(defmethod mrs-output-is-one-of ((mrsout comment) element candidates)
  (with-slots (stream indentation comment-string) mrsout
    (format stream "~VT" (+ indentation 2))
    (cond ((null candidates)
           (format stream "~A: {}" element))
          ((cdr candidates)
           (format stream "~A: {~A~{,~A~}}" element 
                   (car candidates) (cdr candidates)))
          (t (format stream "~A: {~A}" element 
                     (car candidates))))
    (format stream "~%~A" comment-string)) )         
        

(defmethod mrs-output-outscopes ((mrsout comment) higher lower)
  (with-slots (stream indentation comment-string) mrsout
    (format stream "~VT~A >= ~A~%~A" (+ indentation 2) higher lower
                             comment-string)))

(defmethod mrs-output-leq ((mrsout comment) higher lower)
  (with-slots (stream indentation comment-string) mrsout
    (format stream "~VT~A =< ~A~%~A" (+ indentation 2) higher lower
                             comment-string)))

(defmethod mrs-output-end-h-cons ((mrsout comment))
  (with-slots (stream indentation) mrsout
    (format stream "~VT>" indentation)))

(defmethod mrs-output-end-psoa ((mrsout comment))
  (with-slots (stream) mrsout
    (format stream " ]~%")))

;;; 
;;; indexed output-type class
;;;

(defclass indexed (output-type) 
  ((need-comma :initform nil)))

(defmethod mrs-output-start-fn ((mrsout indexed))
  (with-slots (stream) mrsout
    (format stream "~V%" 1)))

(defmethod mrs-output-end-fn ((mrsout indexed))
  (with-slots (stream) mrsout
    (format stream "~V%" 1)))

(defmethod mrs-output-start-psoa ((mrsout indexed) 
				  handel-val event-val)
  (declare (ignore event-val))
  (with-slots (stream) mrsout
    (format stream "TOP INDEX: ~A~%"
            handel-val)))

(defmethod mrs-output-start-liszt ((mrsout indexed))
  (with-slots (stream) mrsout
    (format stream "AND[ ")))

(defmethod mrs-output-atomic-fn ((mrsout indexed) atomic-value)
  (with-slots (stream) mrsout
    (format stream "~A" (remove-variable-junk atomic-value))))

(defmethod mrs-output-list-fn ((mrsout indexed) list-value)
  (with-slots (stream) mrsout
    (format stream "(~{~A~})" list-value)))
  
(defmethod mrs-output-start-rel ((mrsout indexed) sort handel)
  (with-slots (stream indentation) mrsout
    (format stream "~A:~A(" 
             handel (remove-right-sequence "_rel" (string-downcase sort)))))

(defmethod mrs-output-label-fn  ((mrsout indexed) label)
  (declare (ignore label)) 
  (with-slots (stream need-comma) mrsout
    (when need-comma (format stream ", "))
    (setf need-comma t)))

(defmethod mrs-output-end-rel ((mrsout indexed))
  (with-slots (stream need-comma) mrsout
    (format stream ") ") 
    (setf need-comma nil)))

(defmethod mrs-output-end-liszt ((mrsout indexed))
  (with-slots (stream) mrsout
    (format stream "]")))

(defmethod mrs-output-start-h-cons ((mrsout indexed))
  (with-slots (stream) mrsout
    (format stream "~%")))

(defmethod mrs-output-is-one-of ((mrsout indexed) element candidates)
  (with-slots (stream) mrsout
    (cond ((null candidates)
           (format stream "~A:{}" element))
          ((cdr candidates)
           (format stream "~A:{~A~{,~A~}}" element 
                   (car candidates) (cdr candidates)))
          (t (format stream "~A:{~A}" element 
                     (car candidates))))
    (format stream "  ")))

;;; ???
(defmethod mrs-output-outscopes ((mrsout indexed) higher lower)
  (with-slots (stream) mrsout
    (format stream "~A >= ~A~%" 
             higher lower)))

(defmethod mrs-output-leq ((mrsout indexed) higher lower)
  (with-slots (stream) mrsout
    (format stream "~A=<~A  " higher lower)))

(defmethod mrs-output-end-h-cons ((mrsout indexed))
  (with-slots (stream) mrsout
    (format stream " ")))

(defmethod mrs-output-end-psoa ((mrsout indexed))
  (with-slots (stream) mrsout
    (format stream "~%" )))

  
(defun remove-right-sequence (remove-seq existing-seq)
  ;;; if existing-seq terminates in the string given by remove-seq
  ;;; return the existing-sequence without it
  (let ((sl (length existing-seq))
        (rl (length remove-seq)))
    (if (and (> sl rl)
             (equal remove-seq (subseq existing-seq (- sl rl))))
      (subseq existing-seq 0 (- sl rl))
      existing-seq)))
    
(defun remove-variable-junk (var)
  (if (stringp var)
    (let ((space-pos (position #\space var)))
      (if space-pos (subseq var 0 space-pos)
          var))
    var))



(defun output-mrs (mrs-instance device &optional file-name)
     (if file-name
      (with-open-file (stream file-name :direction :output)
         (output-mrs1 mrs-instance device stream))
      (output-mrs1 mrs-instance device t)))
  
; Added option to print raw MRS structures along with pretty-printed ones.

(defun output-mrs1 (mrs-instance device stream)   
  (def-print-operations device 0 stream)
       (cond ((psoa-p mrs-instance)
              (mrs-output-start-fn *mrs-display-structure*)
              (print-psoa mrs-instance 0)
              (mrs-output-end-fn *mrs-display-structure*)
              (mrs-output-max-width-fn *mrs-display-structure*)
	      (when (and (boundp *raw-mrs-output-p*)
			 *raw-mrs-output-p*)
		(format stream "~%~S" mrs-instance)))
             (t (mrs-output-error-fn *mrs-display-structure* mrs-instance))))

(defun print-psoa (psoa indentation)
  (declare (ignore indentation))
  (mrs-output-start-psoa *mrs-display-structure*
           (get-print-name (psoa-handel psoa))
	   (get-print-name (psoa-index psoa)))
  (mrs-output-start-liszt *mrs-display-structure*)
  (loop for rel in (psoa-liszt psoa)
        do
        (mrs-output-start-rel *mrs-display-structure*
                 (rel-sort rel) (get-print-name (rel-handel rel)))
        (loop for feat-val in (rel-flist rel)
              do
             (mrs-output-label-fn *mrs-display-structure*
                      (fvpair-feature feat-val))
             (let ((value (fvpair-value feat-val)))
                      (if (var-p value)
                          (mrs-output-atomic-fn *mrs-display-structure*
                             (get-print-name value))
                          (if (and (listp value)
                                   (var-p (car value)))
                            (mrs-output-list-fn *mrs-display-structure*
                             (mapcar #'get-print-name value))
                            (mrs-output-atomic-fn *mrs-display-structure*
                                                  value)))))
        (mrs-output-end-rel *mrs-display-structure*))
  (mrs-output-end-liszt *mrs-display-structure*)
  (mrs-output-start-h-cons *mrs-display-structure*) 
  (loop for hcons in (psoa-h-cons psoa)
      do
        (cond ((leq-sc-p hcons)
               (mrs-output-leq *mrs-display-structure*
                               (get-print-name (leq-sc-scarg hcons)) 
                                (get-print-name (leq-sc-outscpd hcons))))
              ((hcons-cands hcons)
               (mrs-output-is-one-of *mrs-display-structure*
                                     (get-print-name (hcons-scarg hcons)) 
                                     (mapcar #'get-print-name (hcons-cands hcons))))
              (t (mrs-output-outscopes *mrs-display-structure*
		   (get-print-name (hcons-scarg hcons)) 
                   (get-print-name (hcons-outscpd hcons))))))
  (mrs-output-end-h-cons *mrs-display-structure*)
  (mrs-output-end-psoa *mrs-display-structure*))


;;; error messages

(defparameter *giving-demo-p* t)

(defun struggle-on-error (&rest rest)
  (unless *giving-demo-p*
    (apply #'cerror "Try and continue" rest)))







