(in-package "MRS")

;;; Reorganised MRS code -
;;; this is the basic MRS file, which defines the lisp structures
;;; used for internally encoding MRS.  

;;; MRS structs
;;; because I want to be able to do other things with MRSs besides
;;; just printing them, this stuff defines a set of structures
;;; for MRS.  Note that these structures are simple, because
;;; reentrancies have been replaced by variables.

(defstruct (psoa)
  top-h
  index
  liszt
  h-cons)

(defstruct (rel)
  sort  ; relation name
  handel
  flist
  extra)                                ; extra is a junk slot
                                        ; needed for the munging rules 



(defstruct (fvpair)
  feature
  value)

;;; feature is a symbol

;;; value is either a constant (simple atom) or
;;; a var structure which contains a string plus a number 
;;; (unique to this MRS)

(defstruct (var)
  name
  type
  extra ; useful for e.g. agreement values
  id)

(defstruct (extrapair)
  feature
  value)

(defstruct (handle-var (:include var)))

(defstruct (hcons)
  relation
  scarg
  outscpd)

;;; variable generator - moved from mrsoutput because it could
;;; potentially be called without that code having been read in

(defvar *variable-generator* nil)

(defun create-variable-generator (&optional start)
  (let ((number (or start 0)))
    #'(lambda nil
        (incf number)
        number)))

(defun init-variable-generator ()
  (setf *variable-generator* (create-variable-generator)))

(init-variable-generator)



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

(defmethod mrs-output-start-psoa ((mrsout simple))
  (with-slots (stream) mrsout
    (format stream "[")))

(defmethod mrs-output-top-h ((mrsout simple) handel-val)
  (with-slots (stream) mrsout
    (format stream " TOP: ~A" handel-val)))

(defmethod mrs-output-index ((mrsout simple) index-val)
  (with-slots (stream) mrsout
    (when index-val
      (format stream "~%  INDEX: ~A" index-val))))

(defmethod mrs-output-start-liszt ((mrsout simple))
  (with-slots (stream indentation) mrsout
    (format stream "~%  LISZT: <")
    (setf indentation (+ indentation 10))))

(defmethod mrs-output-var-fn ((mrsout simple) var-string)
  (with-slots (stream) mrsout
    (format stream "~A" var-string)))

(defmethod mrs-output-atomic-fn ((mrsout simple) atomic-value)
  (with-slots (stream) mrsout
    (format stream "~S" atomic-value)))

(defmethod mrs-output-start-rel ((mrsout simple) sort first-p)
  (with-slots (stream indentation) mrsout
    (unless first-p (format stream "~%"))
    (format stream "~VT[ ~A" indentation (string-downcase sort))))

(defmethod mrs-output-rel-handel ((mrsout simple) handel)
  (with-slots (stream indentation) mrsout
    (format stream "~%~VT~A: ~A" (+ indentation 2) 'handel handel)))

(defmethod mrs-output-label-fn  ((mrsout simple) label)
  (with-slots (stream indentation) mrsout
    (format stream "~%~VT~A: " (+ indentation 2) label)))

(defmethod mrs-output-start-extra ((mrsout simple) var-type)
  (with-slots (stream indentation) mrsout
    (format stream " [ ~A" var-type)))

(defmethod mrs-output-extra-feat  ((mrsout simple) feat)
  (with-slots (stream indentation) mrsout
    (format stream "~%~VT~A: " (+ indentation 15) feat)))

(defmethod mrs-output-extra-val  ((mrsout simple) val)
  (with-slots (stream) mrsout
    (format stream " ~A" val)))

(defmethod mrs-output-end-extra ((mrsout simple))
  (with-slots (stream) mrsout
    (format stream " ]")))

(defmethod mrs-output-end-rel ((mrsout simple))
  (with-slots (stream) mrsout
    (format stream " ]")))

(defmethod mrs-output-end-liszt ((mrsout simple))
  (with-slots (stream indentation) mrsout
    (format stream "~VT>" indentation)))

(defmethod mrs-output-start-h-cons ((mrsout simple))
  (with-slots (stream) mrsout
    (format stream "~%  HCONS: <")))

(defmethod mrs-output-outscopes ((mrsout simple) reln higher lower first-p)
  (with-slots (stream indentation) mrsout
    (unless first-p
      (format stream "~%"))
    (format stream "~VT~A ~A ~A" 
            (+ indentation 2) higher reln lower)))

(defmethod mrs-output-end-h-cons ((mrsout simple))
  (with-slots (stream indentation) mrsout
    (format stream "~VT>" indentation)))

(defmethod mrs-output-end-psoa ((mrsout simple))
  (with-slots (stream indentation) mrsout
    (format stream " ]~%" indentation)))


(defclass active-t (simple)
  ())

(defmethod mrs-output-start-rel ((mrsout active-t) sort first-p)
  (with-slots (stream indentation) mrsout
    (unless first-p (format stream "~%"))
    (format stream "~VT[ " indentation)
    (cl-user::add-mrs-type-region stream sort)))

;;; 
;;; indexed output-type class
;;;

(defclass indexed (output-type) 
  ((need-comma :initform nil)
   (temp-sort :initform nil)))

(defmethod mrs-output-start-fn ((mrsout indexed))
  (with-slots (stream) mrsout
    (format stream "~V%" 1)))

(defmethod mrs-output-end-fn ((mrsout indexed))
  (with-slots (stream) mrsout
    (format stream "~V%" 1)))

(defmethod mrs-output-start-psoa ((mrsout indexed)) 
  nil)
  
(defmethod mrs-output-top-h ((mrsout indexed) handel-val)
  (with-slots (stream) mrsout
    (format stream "TOP: ~A~%"
            handel-val)))

(defmethod mrs-output-index ((mrsout indexed) index-val)
  (declare (ignore index-val))
  nil)

(defmethod mrs-output-start-liszt ((mrsout indexed))
  (with-slots (stream) mrsout
    (format stream "AND[ ")))

(defmethod mrs-output-var-fn ((mrsout indexed) var-string)
  (with-slots (stream) mrsout
    (format stream "~A" (remove-variable-junk var-string))))

(defmethod mrs-output-atomic-fn ((mrsout indexed) atomic-value)
  (with-slots (stream) mrsout
    (format stream "~A" atomic-value)))

(defmethod mrs-output-start-rel ((mrsout indexed) sort first-p)
  (declare (ignore first-p))
  (with-slots (temp-sort) mrsout
    (setf temp-sort sort)))

(defmethod mrs-output-rel-handel ((mrsout indexed) handel)
  (with-slots (stream temp-sort) mrsout  
    (format stream "~A:~A(" 
            handel (remove-right-sequence 
                    *sem-relation-suffix* 
                    (string-downcase temp-sort)))))

(defmethod mrs-output-label-fn  ((mrsout indexed) label)
  (declare (ignore label)) 
  (with-slots (stream need-comma) mrsout
    (when need-comma (format stream ", "))
    (setf need-comma t)))

(defmethod mrs-output-start-extra ((mrsout indexed) var-type)
  (declare (ignore var-type))
  nil)

(defmethod mrs-output-extra-feat  ((mrsout indexed) feat)
  (declare (ignore feat))
  nil)

(defmethod mrs-output-extra-val  ((mrsout indexed) val)
  (declare (ignore val))
  nil)

(defmethod mrs-output-end-extra ((mrsout indexed))
  nil)

(defmethod mrs-output-end-rel ((mrsout indexed))
  (with-slots (stream need-comma) mrsout
    (format stream ")~%") 
    (setf need-comma nil)))

(defmethod mrs-output-end-liszt ((mrsout indexed))
  (with-slots (stream) mrsout
    (format stream "]")))

(defmethod mrs-output-start-h-cons ((mrsout indexed))
  (with-slots (stream) mrsout
    (format stream "~%")))

;;; ???
(defmethod mrs-output-outscopes ((mrsout indexed) reln higher lower first-p)
  (declare (ignore first-p))
  (with-slots (stream) mrsout
    (format stream "~A ~A ~A~%" 
            higher reln lower)))

(defmethod mrs-output-end-h-cons ((mrsout indexed))
  (with-slots (stream) mrsout
    (format stream " ")))

(defmethod mrs-output-end-psoa ((mrsout indexed))
  (with-slots (stream) mrsout
    (format stream "~%" )))



;;; 
;;; prolog output-type class
;;;

#|

assume the following structure

psoa(handel,index,liszt,hcons)

handel is a handle-variable
index is a variable

liszt is a list of rels

rel(relation,handel,attrvals)

relation is a string
handel is a handle-variable
attrvals is a list of attrvals

attrval(attribute,value)

attribute is a string
value is a string or a variable or a handle-variable

hcons is a list of qeqs

qeq(higher,lower)

higher and lower are handle-variables

|#


#|

(defclass prolog (output-type)
  ((need-rel-comma :initform nil)
   (need-comma :initform nil)))

;;; replace generic fn

(defmethod mrs-output-print-name ((mrsout prolog) value)
  (with-slots () mrsout
    (if (var-p value)
        (intern (var-name value))
      'u)))

(defmethod mrs-output-start-fn ((mrsout prolog))
  (with-slots (stream) mrsout
    (format stream "~V%" 1)))

(defmethod mrs-output-end-fn ((mrsout prolog))
  (with-slots (stream) mrsout
    (format stream "~V%" 1)))

(defmethod mrs-output-start-psoa ((mrsout prolog) 
				  handel-val event-val)
  (with-slots (stream) mrsout
    (format stream "psoa(~A,~A," handel-val event-val)))

(defmethod mrs-output-start-liszt ((mrsout prolog))
  (with-slots (stream) mrsout
    (format stream "[")))

(defmethod mrs-output-var-fn ((mrsout prolog) var-string)
  (with-slots (stream) mrsout
    (format stream "'~A')" var-string)))

(defmethod mrs-output-atomic-fn ((mrsout prolog) atomic-value)
  (with-slots (stream) mrsout
    (if (stringp atomic-value)
        (format stream "'~A')" atomic-value)
      (format stream "~A)" atomic-value))))

(defmethod mrs-output-start-rel ((mrsout prolog) sort handel)
  (with-slots (stream need-rel-comma) mrsout
    (when need-rel-comma (format stream ","))
    (setf need-rel-comma t)
    (format stream "rel('~A',~A,[" (string-downcase sort) handel)))

(defmethod mrs-output-label-fn  ((mrsout prolog) label)
  (with-slots (stream need-comma) mrsout
    (when need-comma (format stream ","))
    (setf need-comma t)
    (format stream "attrval('~A'," label)))

(defmethod mrs-output-end-rel ((mrsout prolog))
  (with-slots (stream need-comma) mrsout
    (setf need-comma nil)
    (format stream "])")))

(defmethod mrs-output-end-liszt ((mrsout prolog))
  (with-slots (stream need-rel-comma) mrsout
    (setf need-rel-comma nil)
    (format stream "]")))

(defmethod mrs-output-start-h-cons ((mrsout prolog))
  (with-slots (stream) mrsout
    (format stream ",hcons([")))

(defmethod mrs-output-outscopes ((mrsout prolog) reln higher lower)
  (with-slots (stream need-comma) mrsout  
    (when need-comma (format stream ","))
    (setf need-comma t)
    (format stream "~A(~A,~A)" reln higher lower)))

(defmethod mrs-output-end-h-cons ((mrsout prolog))
  (with-slots (stream need-comma) mrsout
    (setf need-comma nil)
    (format stream "])")))

(defmethod mrs-output-end-psoa ((mrsout prolog))
  (with-slots (stream) mrsout
    (format stream ")~%")))


|#

;;; Utility fns

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

;;; Actual output fns

(defun output-mrs (mrs-instance device &optional file-name)
     (if file-name
      (with-open-file (stream file-name :direction :output)
         (output-mrs1 mrs-instance device stream))
      (output-mrs1 mrs-instance device t)))
  
(defun output-mrs1 (mrs-instance device stream)
  (def-print-operations device 0 stream)
       (cond ((psoa-p mrs-instance)
              (mrs-output-start-fn *mrs-display-structure*)
              (print-psoa mrs-instance)
              (mrs-output-end-fn *mrs-display-structure*)
              (mrs-output-max-width-fn *mrs-display-structure*))
             (t (mrs-output-error-fn *mrs-display-structure* mrs-instance))))

(defparameter *already-seen-vars* nil)

(defun find-var-name (var connected-p)
  (if connected-p
      (get-bound-var-value var)
      (var-name var)))

(defun print-psoa (psoa &optional connected-p)
  (setf *already-seen-vars* nil)
  (mrs-output-start-psoa *mrs-display-structure*)
  (mrs-output-top-h *mrs-display-structure* 
                    (find-var-name (psoa-top-h psoa) connected-p))
  (print-mrs-extra (psoa-top-h psoa))
  (mrs-output-index *mrs-display-structure* 
                    (find-var-name (psoa-index psoa) connected-p))
  (print-mrs-extra (psoa-index psoa))
  (mrs-output-start-liszt *mrs-display-structure*)
  (let ((first-rel t))
    (loop for rel in (psoa-liszt psoa)
        do
          (mrs-output-start-rel *mrs-display-structure*
                                (rel-sort rel) first-rel)
          (mrs-output-rel-handel
           *mrs-display-structure* 
           (find-var-name (rel-handel rel) connected-p))
          (print-mrs-extra (rel-handel rel))
          (loop for feat-val in (rel-flist rel)
              do
                (mrs-output-label-fn *mrs-display-structure*
                                     (fvpair-feature feat-val))
                (let ((value (fvpair-value feat-val)))
                  (if (var-p value)
                      (progn
                        (mrs-output-var-fn 
                         *mrs-display-structure*
                         (find-var-name value connected-p))
                        (print-mrs-extra value))
                    (mrs-output-atomic-fn 
                     *mrs-display-structure*
                     value))))
          (mrs-output-end-rel *mrs-display-structure*)
          (setf first-rel nil)))
    (mrs-output-end-liszt *mrs-display-structure*)
    (mrs-output-start-h-cons *mrs-display-structure*)
    (let ((first-hcons t))
      (loop for hcons in (psoa-h-cons psoa)
          do
            (mrs-output-outscopes 
             *mrs-display-structure*
             (hcons-relation hcons)
             (find-var-name 
              (hcons-scarg hcons) connected-p) 
             (find-var-name 
              (hcons-outscpd hcons) connected-p)
             first-hcons)
            (setf first-hcons nil)))
    ;; extra info can be ignored here because all handels
    ;; will have appeared elsewhere
    (mrs-output-end-h-cons *mrs-display-structure*)
    (mrs-output-end-psoa *mrs-display-structure*))

(defun print-mrs-extra (var)
  (when (var-p var)
    (when (not (member var *already-seen-vars*
                                            :test #'eq))
      (mrs-output-start-extra *mrs-display-structure*
                              (var-type var))
      (loop for extrapair in (var-extra var)
          do
            (mrs-output-extra-feat *mrs-display-structure*
                                   (extrapair-feature extrapair))
            (mrs-output-extra-val *mrs-display-structure*
                                  (extrapair-value extrapair)))
      (mrs-output-end-extra *mrs-display-structure*)
      (push var *already-seen-vars*))))
                         

;;; error messages

(defun struggle-on-error (&rest rest)
  (unless *giving-demo-p*
    (apply #'cerror "Try and continue" rest)))


;;; Reading in an MRS that was written out in the `simple' format

#|

MRS -> [ LTOP INDEX LISZT HCONS ]

LTOP -> top: VAR

INDEX -> index: VAR

LISZT -> liszt: < REL* >

HCONS -> hcons: < QEQ* >

REL -> [ PREDNAME handel: VAR FEATPAIR* ]

FEATPAIR -> FEATNAME: VAR | FEAT: CONSTNAME

VAR -> VARNAME | VARNAME [ VARTYPE EXTRAPAIR* ]

EXTRAPAIR -> PATHNAME: CONSTNAME

QEQ -> VARNAME RELNNAME VARNAME


|#


(defun make-mrs-break-table nil 
  (cl-user::define-break-characters '(#\< #\> #\:
                                      #\[ #\])))

(defun mrs-check-for (character istream)
   (let ((next-char (peek-char t istream nil 'eof)))
     (if (char-equal next-char character)
         (read-char istream)
         (error
                 "~%Syntax error: ~A expected and not found at position ~A" 
                 character (file-position istream)))))



(defun read-mrs-files-aux (file-names)
  (for file-name in file-names
       append
       (format t "~%Reading in MRS file ~A" (pathname-name file-name))
       (force-output t)
       (with-open-file 
           (istream file-name :direction :input)
         (read-mrs-stream istream))))

(defun read-mrs-stream (istream) 
  (let ((psoas nil))
   (loop
      (let ((next-char (peek-char t istream nil 'eof)))
         (when (eql next-char 'eof) (return))
         (cond ((eql next-char #\;) 
                 (read-line istream))
               ; one line comments
               (t (push (read-mrs istream)
                        psoas)))))
   (nreverse psoas)))

(defparameter *already-read-vars* nil
  "temporary storage of variables read in in one MRS")

(defun read-mrs (istream)
  (let ((*readtable* (make-mrs-break-table)))
;;;  MRS -> [ LTOP INDEX LISZT HCONS ]
    (setf *already-read-vars* nil)
    (mrs-check-for #\[ istream)
    (let* ((ltop (read-mrs-ltop istream))
           (index (read-mrs-index istream))
           (liszt (read-mrs-liszt istream))
           (hcons (read-mrs-hcons istream))
           (psoa
            (make-psoa :top-h ltop
                       :index index
                       :liszt liszt
                       :h-cons hcons)))
      (mrs-check-for #\] istream)
      psoa)))

(defun read-mrs-ltop (istream)
;;;  LTOP -> top: VAR
  (mrs-check-for #\t istream)
  (mrs-check-for #\o istream)
  (mrs-check-for #\p istream)
  (mrs-check-for #\: istream)
  (read-mrs-var istream))

(defun read-mrs-index (istream)
;;; INDEX -> index: VAR
  (mrs-check-for #\i istream)
  (mrs-check-for #\n istream)
  (mrs-check-for #\d istream)
  (mrs-check-for #\e istream)
  (mrs-check-for #\x istream)
  (mrs-check-for #\: istream)
  (read-mrs-var istream))

(defun read-mrs-liszt (istream)
  ;;; LISZT -> liszt: < REL* >
  (let ((rels nil))
    (mrs-check-for #\l istream)
    (mrs-check-for #\i istream)
    (mrs-check-for #\s istream)
    (mrs-check-for #\z istream)
    (mrs-check-for #\t istream)
    (mrs-check-for #\: istream)
    (mrs-check-for #\< istream)
    (loop 
      (let ((next-char (peek-char t istream nil 'eof)))
        (when (eql next-char 'eof) (error "Unexpected eof"))
        (when (eql next-char #\>) (return))
        (push (read-mrs-rel istream)
              rels)))
    (mrs-check-for #\> istream)
    rels))

(defun read-mrs-hcons (istream)
  ;; HCONS -> hcons: < QEQ* >
  (let ((cons nil))
    (mrs-check-for #\h istream)
    (mrs-check-for #\c istream)
    (mrs-check-for #\o istream)
    (mrs-check-for #\n istream)
    (mrs-check-for #\s istream)
    (mrs-check-for #\: istream)
    (mrs-check-for #\< istream)
    (loop 
      (let ((next-char (peek-char t istream nil 'eof)))
        (when (eql next-char 'eof) (error "Unexpected eof"))
        (when (eql next-char #\>) (return))
        (push (read-mrs-qeq istream)
              cons)))
    (mrs-check-for #\> istream)
    cons))

(defun read-mrs-rel (istream)
;  REL -> [ PREDNAME handel: VAR FEATPAIR* ]
  (mrs-check-for #\[ istream)
  (let ((predname (read-mrs-atom istream)))
    (mrs-check-for #\h istream)
    (mrs-check-for #\a istream)
    (mrs-check-for #\n istream)
    (mrs-check-for #\d istream)
    (mrs-check-for #\e istream)
    (mrs-check-for #\l istream)
    (mrs-check-for #\: istream)
    (let ((hvar (read-mrs-var istream))
          (featpairs nil))
      (loop 
        (let ((next-char (peek-char t istream nil 'eof)))
          (when (eql next-char 'eof) (error "Unexpected eof"))
          (when (eql next-char #\]) 
            (read-char istream)
            (return))
          (push (read-mrs-featpair istream)
                featpairs)))
      (make-rel :sort predname
                :handel hvar
                :flist (sort featpairs #'feat-sort-func)))))
          
(defun read-mrs-featpair (istream)         
  ;; FEATPAIR -> FEATNAME: VAR | FEAT: CONSTNAME
  (let ((feature (read-mrs-atom istream)))
    (mrs-check-for #\: istream)
    (let ((val
           (if (member feature *value-feats*
                       :test #'eq)
               (read-mrs-atom istream)
             (read-mrs-var istream))))
      (make-fvpair :feature feature
                   :value val))))

(defun read-mrs-var (istream)
  ;;  VAR -> VARNAME | VARNAME [ VARTYPE EXTRAPAIR* ]
  ;; note that the type and extra values are assumed
  ;; to only occur once (or if repeated, to be consistent)
  (let* ((varname (read-mrs-atom istream))
         (existing (assoc varname *already-read-vars*))
         (var (or (cdr existing)
                  (make-var :name (string varname)
                            :id (funcall *variable-generator*)))))
    (unless existing 
      (push (cons varname var) *already-read-vars*))
    (let ((next-char (peek-char t istream nil 'eof)))
      (when (eql next-char 'eof) (error "Unexpected eof"))
      (when (eql next-char #\[)
        (read-char istream)
        (let ((extra nil)
              (var-type (read-mrs-atom istream)))
          (loop
            (let ((inner-next-char (peek-char t istream nil 'eof)))
              (when (eql inner-next-char 'eof) (error "Unexpected eof"))
              (when (eql inner-next-char #\]) (return))
              (push (read-mrs-extrapair istream)
                extra)))
        (setf (var-type var) var-type)
        (setf (var-extra var) extra)
        (mrs-check-for #\] istream)))
      var)))

(defun read-mrs-simple-var (istream)
  ;;  VAR -> VARNAME 
  ;; no type or extras
  (let* ((varname (read-mrs-atom istream))
         (existing (assoc varname *already-read-vars*))
         (var (or (cdr existing)
                  (make-var :name (string varname)
                            :id (gensym (string varname))))))
    (unless existing 
      (push (cons varname var) *already-read-vars*))
    var))

(defun read-mrs-extrapair (istream)
  ;; EXTRAPAIR -> PATHNAME: CONSTNAME
  (let ((pathname (read-mrs-atom istream)))
    (mrs-check-for #\: istream)
    (let ((val (read-mrs-atom istream)))
      (make-extrapair :feature pathname
                      :value val))))
    

(defun read-mrs-qeq (istream)
  ;; QEQ -> VARNAME RELNNAME VARNAME
  (let* ((var1 (read-mrs-simple-var istream))
         (reln (read-mrs-atom istream))
         (var2 (read-mrs-simple-var istream)))
    (make-hcons :relation reln
                :scarg var1
                :outscpd var2)))    
          
          
(defun read-mrs-atom (istream)
  (let ((atomsym (read istream nil 'eof)))
    (when (eq atomsym 'eof)
      (error "Unexpected eof"))
    atomsym))

