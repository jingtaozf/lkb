;;; Copyright (c) 1998--2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

;;; WARNING: the variable-generation function and some structures are 
;;; duplicated in rmrs/standalone.lisp

(in-package "MRS")

;;; Reorganised MRS code -
;;; this is the basic MRS file, which defines the lisp structures
;;; used for internally encoding MRS.  

;;; ideally handle and liszt should be renamed, but life's too short

(defstruct (basemrs)
  top-h
  liszt
  h-cons)

;;; rmrs is a substructure of this (see comp.lisp)

(defstruct (psoa (:include basemrs))
  index)

;;; psoa records an index - this really is the equivalent of the semstruct
;;; in RMRS - the two should possibly be amalgamated and index should be
;;; replaced by hook, when the code for displaying MRSs according to the
;;; algebra is finally written

;;; I have changed the old `sort' to `pred' - the old name was 
;;; seriously confusing

;;; ep removed again - was used for the `simple' RMRS
;;; but that's no longer supported

(defstruct (rel)
  handel                               
  pred					; relation name
  flist
  parameter-strings                     ; copy of the relations with constant
					; values, used in the generator
                                        ; candidate for removal!
  extra)                                ; extra is a junk slot
                                        ; needed for the munging rules 

;;; currently the cfrom and cto are just used in RMRS (plus in some
;;; experimental MRS code that was never checked in) but defined here
;;; since they will be needed

(defstruct (char-rel (:include rel))
  cfrom
  cto)

(defstruct (fvpair)
  feature
  value)

;;; feature is a symbol

;;; value is either a constant or
;;; a var structure which contains a string plus a number 
;;; (unique to this MRS)

(defstruct (var)
  type
  extra ; e.g. agreement values
  id)

(defstruct (extrapair)
  feature
  value)

;;;
;;; _fix_me_
;;; is `handle-var' still used, actually?  at least the MRS extracted from an
;;; LKB parse result no longer have any, it seems.              (9-sep-03; oe)
;;;
(defstruct (handle-var (:include var)))

(defstruct (grammar-var (:include var)))
;;; a sort of placeholder variable used in RMRS code

(defstruct (hcons)
  relation
  scarg
  outscpd)

;;; In an attempt to clean up a messy situation,
;;; var-types are now all lower-case strings.  
;;; Although I wouldn't generally use strings
;;; to represent enumerated values, it saves 
;;; considerable messing around to do so, which
;;; seems more important than the minor efficiency
;;; hit.  In effect, the inventory of var-types is
;;; part of the SEM-I.  For now we have the
;;; following (from the RMRS DTD)
;;; (x|e|h|u|l)
;;; the mapping from the ERG (currently in lkbmrs.lisp)
;;; adds d, t, v and c

(defun var-string (var)
  ;;; always constructed from the type and the id
  ;;; FIX - should be renamed to avoid confusion
  ;;; when dependencies code is changed
  (unless (var-p var)
    (error "var expected ~A found" var))
  (format nil "~(~A~)~A" (var-type var)
	  (var-id var)))

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
  (setf *mrs-display-structure* 
    (make-instance class 
      :indentation indentation :stream stream)))


;;; 
;;; Generic output-type class
;;;

(defclass output-type ()
  ((indentation :initform 0 :initarg :indentation)
   (stream :initarg :stream)))

(defmethod initialize-display-structure ((class output-type) mrs-instance)
  (declare (ignore mrs-instance)))

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
  (when (and handel-val *rel-handel-path*)
    (with-slots (stream) mrsout
      (format stream " LTOP: ~(~a~)" handel-val))))

(defmethod mrs-output-index ((mrsout simple) index-val)
  (with-slots (stream) mrsout
    (when index-val
      (format stream "~%  INDEX: ~(~a~)" index-val))))

(defmethod mrs-output-start-liszt ((mrsout simple))
  (with-slots (stream indentation) mrsout
    (format stream "~%  RELS: <")
    (setf indentation (+ indentation 10))))

(defmethod mrs-output-var-fn ((mrsout simple) var-string)
  (with-slots (stream) mrsout
    (format stream "~(~a~)" var-string)))

(defmethod mrs-output-atomic-fn ((mrsout simple) atomic-value)
  (with-slots (stream) mrsout
    (format stream "~S" atomic-value)))

(defmethod mrs-output-start-rel ((mrsout simple) pred first-p)
  (declare (ignore first-p))
  (with-slots (stream indentation) mrsout
    (format stream "~%")
    (if (stringp pred)
      (format stream "~VT[ ~s" indentation pred)
      (format stream "~VT[ ~(~a~)" indentation pred))))

(defmethod mrs-output-rel-handel ((mrsout simple) handel)
  (if handel
      (with-slots (stream indentation) mrsout
        (format stream "~%~VT~A: ~(~a~)" (+ indentation 2) 'lbl handel))))

(defmethod mrs-output-label-fn  ((mrsout simple) label)
  (with-slots (stream indentation) mrsout
    (format stream "~%~VT~a: " (+ indentation 2) label)))

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
    (format stream "~VT~(~a~) ~A ~(~a~)" 
            (+ indentation 2) higher reln lower)))

(defmethod mrs-output-end-h-cons ((mrsout simple))
  (with-slots (stream indentation) mrsout
    (format stream "~VT>" indentation)))


(defmethod mrs-output-end-psoa ((mrsout simple))
  (with-slots (stream indentation) mrsout
    (format stream " ]~%" indentation)))

;;;
;;; active output class (for on-line browsing in CLIM)
;;;

(defclass active-t (simple)
  ())

(defmethod mrs-output-start-rel ((mrsout active-t) pred first-p)
  (declare (ignore first-p))
  (with-slots (stream indentation) mrsout
    (format stream "~%")
    (format stream "~VT[ " indentation)
    (lkb::add-mrs-pred-region stream pred)))

;;; 
;;; indexed output-type class
;;;

(defclass indexed (output-type) 
  ((need-comma :initform nil)
   (temp-pred :initform nil)))

(defmethod mrs-output-start-fn ((mrsout indexed))
  (with-slots (stream) mrsout
    (format stream "~V%" 1)))

(defmethod mrs-output-end-fn ((mrsout indexed))
  (with-slots (stream) mrsout
    (format stream "~V%" 1)))

(defmethod mrs-output-start-psoa ((mrsout indexed)) 
  (with-slots (stream) mrsout
    (format stream "<")))
  
(defmethod mrs-output-top-h ((mrsout indexed) handel-val)
  (if handel-val
      (with-slots (stream) mrsout
        (format stream "~(~a~)," handel-val))))

(defmethod mrs-output-index ((mrsout indexed) index-val)
  (with-slots (stream) mrsout
    (format stream "~(~a~)" index-val)))

(defmethod mrs-output-start-liszt ((mrsout indexed))
  (with-slots (stream) mrsout
    (format stream ",~%{")))

(defmethod mrs-output-var-fn ((mrsout indexed) var-string)
  (with-slots (stream) mrsout
    (format stream "~(~a~)" (remove-variable-junk var-string))))

(defmethod mrs-output-atomic-fn ((mrsout indexed) atomic-value)
  (with-slots (stream) mrsout
    (format stream "~S" atomic-value)))

(defmethod mrs-output-start-rel ((mrsout indexed) pred first-p)
  (with-slots (stream temp-pred) mrsout
    (setf temp-pred pred)
    (unless first-p (format stream ",~%"))))

(defmethod mrs-output-rel-handel ((mrsout indexed) handel)
  (if handel
      (with-slots (stream temp-pred) mrsout  
        (format stream "~(~a~):~A(" 
                handel (remove-right-sequence 
                        *sem-relation-suffix* 
                        (string-downcase temp-pred))))
    (with-slots (stream temp-pred) mrsout  
        (format stream "~A(" 
                (remove-right-sequence 
                        *sem-relation-suffix* 
                        (string-downcase temp-pred))))))

(defmethod mrs-output-label-fn  ((mrsout indexed) label)
  (declare (ignore label)) 
  (with-slots (stream need-comma) mrsout
    (when need-comma (format stream ", "))
    (setf need-comma t)))

(defmethod mrs-output-start-extra ((mrsout indexed) var-type)
   (declare (ignore var-type))
   nil)
;  (with-slots (stream) mrsout
;    (format stream ":~A" var-type)))

(defmethod mrs-output-extra-feat  ((mrsout indexed) feat)
  (declare (ignore feat))
  nil)

(defmethod mrs-output-extra-val  ((mrsout indexed) val)
  (with-slots (stream) mrsout
    (format stream ":~A" val)))

(defmethod mrs-output-end-extra ((mrsout indexed))
  nil)

(defmethod mrs-output-end-rel ((mrsout indexed))
  (with-slots (stream need-comma) mrsout
    (format stream ")") 
    (setf need-comma nil)))

(defmethod mrs-output-end-liszt ((mrsout indexed))
  (with-slots (stream) mrsout
    (format stream "},")))

(defmethod mrs-output-start-h-cons ((mrsout indexed))
  (with-slots (stream) mrsout
    (format stream "~%{")))

;;; ???
(defmethod mrs-output-outscopes ((mrsout indexed) reln higher lower first-p)
  (with-slots (stream) mrsout
    (unless first-p
      (format stream ",~%"))
    (format stream "~(~a~) ~A ~(~a~)" 
            higher reln lower)))

(defmethod mrs-output-end-h-cons ((mrsout indexed))
  (with-slots (stream) mrsout
    (format stream "}")))

(defmethod mrs-output-end-psoa ((mrsout indexed))
  (with-slots (stream) mrsout
    (format stream ">~%" )))

;;; dropping the `extra' stuff

(defclass simple-indexed (indexed)
  ())

(defmethod mrs-output-extra-val  ((mrsout simple-indexed) val)
  (declare (ignore val))
  nil)

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

(defclass prolog (output-type)
  ((need-comma :initform nil)))

(defmethod mrs-output-start-fn ((mrsout prolog))
  (with-slots (stream) mrsout
    (format stream "~V%" 1)))

(defmethod mrs-output-end-fn ((mrsout prolog))
  (with-slots (stream) mrsout
    (format stream "~V%" 1)))

(defmethod mrs-output-start-psoa ((mrsout prolog)) 
  (with-slots (stream) mrsout
    (format stream "psoa(")))

(defmethod mrs-output-top-h ((mrsout prolog) handel-val)
  (with-slots (stream) mrsout
    (format stream "~(~a~)" handel-val)))

(defmethod mrs-output-index ((mrsout prolog) index-val)
  (with-slots (stream) mrsout
    (format stream ",~(~a~)" index-val)))

(defmethod mrs-output-start-liszt ((mrsout prolog))
  (with-slots (stream) mrsout
    (format stream ",[")))

(defmethod mrs-output-var-fn ((mrsout prolog) var-string)
  (with-slots (stream) mrsout
    (format stream "~(~a~))" (remove-variable-junk var-string))))

(defmethod mrs-output-atomic-fn ((mrsout prolog) atomic-value)
  (with-slots (stream) mrsout
    (if (stringp atomic-value)
        (format stream "'~A')" atomic-value)
      (format stream "~A)" atomic-value))))

(defmethod mrs-output-start-rel ((mrsout prolog) pred first-p)
  (with-slots (stream) mrsout
    (unless first-p (format stream ","))
    (format stream "rel('~A'," 
            (remove-right-sequence 
	     *sem-relation-suffix*
	     (string-downcase pred)))))

(defmethod mrs-output-rel-handel ((mrsout prolog) handel)
  (with-slots (stream temp-pred) mrsout  
    (format stream "~(~a~),[" handel)))


(defmethod mrs-output-label-fn  ((mrsout prolog) label)
  (with-slots (stream need-comma) mrsout
    (when need-comma (format stream ","))
    (setf need-comma t)
    (format stream "attrval('~A'," label)))

(defmethod mrs-output-start-extra ((mrsout prolog) var-type)
   (declare (ignore var-type))
   nil)

(defmethod mrs-output-extra-feat  ((mrsout prolog) feat)
  (declare (ignore feat))
  nil)

(defmethod mrs-output-extra-val  ((mrsout prolog) val)
  (declare (ignore val))
  nil)

(defmethod mrs-output-end-extra ((mrsout prolog))
  nil)

(defmethod mrs-output-end-rel ((mrsout prolog))
  (with-slots (stream need-comma) mrsout
    (setf need-comma nil)
    (format stream "])")))

(defmethod mrs-output-end-liszt ((mrsout prolog))
  (with-slots (stream) mrsout
    (format stream "]")))

(defmethod mrs-output-start-h-cons ((mrsout prolog))
  (with-slots (stream) mrsout
    (format stream ",hcons([")))

(defmethod mrs-output-outscopes ((mrsout prolog) reln higher lower first-p)
  (with-slots (stream) mrsout  
    (unless first-p (format stream ","))
    (format stream "~A(~(~a~),~(~a~))" (string-downcase reln) higher lower)))

(defmethod mrs-output-end-h-cons ((mrsout prolog))
  (with-slots (stream need-comma) mrsout
    (setf need-comma nil)
    (format stream "])")))

(defmethod mrs-output-end-psoa ((mrsout prolog))
  (with-slots (stream) mrsout
    (format stream ")~%")))

;;; 
;;; HTML output-type class
;;;

(defparameter *mrs-relations-per-row* 6)

(defclass html (output-type) 
  ((nrels :initform nil)
   (i :initform nil)
   (nrows :initform nil)))

(defmethod initialize-display-structure ((class html) mrs)
  (with-slots (nrels nrows i) class
    (setf nrels (length (psoa-liszt mrs)))
    (setf nrows (ceiling nrels *mrs-relations-per-row*))
    (setf i 0)))
    
(defmethod mrs-output-start-fn ((mrs html))
  (with-slots (stream) mrs
    (format stream "~V%" 1)))

(defmethod mrs-output-end-fn ((mrs html))
  (with-slots (stream) mrs
    (format stream "~V%" 1)))

(defmethod mrs-output-start-psoa ((mrs html))
  (with-slots (stream) mrs
    (format stream "<table class=mrsMrs>~%")))

(defmethod mrs-output-top-h ((mrs html) handle)
  (when (and handle *rel-handel-path*)
    (with-slots (stream) mrs
      (format 
       stream 
       "<tr><td class=mrsFeatureTop>TOP</td>~%~
        <td class=mrsValueTop>~%  ~
        <div class=\"mrsVariable~:(~a~)\"~%       ~
             onMouseOver=\"mrsVariableSelect('~:(~a~)')\"~%       ~
             onMouseOut=\"mrsVariableUnselect('~:(~a~)')\">~(~a~)</div>~%~
        </td>~%" 
       handle handle handle handle))))

(defmethod mrs-output-index ((mrs html) index)
  (when index
    (with-slots (stream) mrs
      (format 
       stream 
       "<tr>~%<td class=mrsFeatureIndex>INDEX</td>~%~
        <td class=mrsValueIndex>~%  ~
        <div class=\"mrsVariable~:(~a~)\"~%       ~
             onMouseOver=\"mrsVariableSelect('~:(~a~)')\"~%       ~
             onMouseOut=\"mrsVariableUnselect('~:(~a~)')\">~(~a~)</div>~%~
        </td>~%" 
       index index index index))))

(defmethod mrs-output-start-liszt ((mrs html))
  (with-slots (stream nrows) mrs
    (format
     stream 
     "<tr>~%<td class=mrsFeatureRels>RELS</td>~%~
      <td class=mrsValueRels>~%~
      <table class=mrsRelsContainer>~%<tr>~%~
      <td valign=middle><span class=mrsBracket>{</span></td>~%~
      <td><table class=mrsRelsContainer><tr>~%
      <td><table class=mrsRelsContainer><tr>~%"
     nrows)))

(defmethod mrs-output-var-fn ((mrs html) variable)
  (with-slots (stream) mrs
    (format 
     stream 
     "<td class=mrsValue>~%~
      <div class=\"mrsVariable~:(~a~)\"~
             onMouseOver=\"mrsVariableSelect('~:(~a~)')\"~
             onMouseOut=\"mrsVariableUnselect('~:(~a~)')\">~(~a~)</div>~
      </td>~%" 
     variable variable variable variable)))

(defmethod mrs-output-atomic-fn ((mrs html) value)
  (with-slots (stream) mrs
    (format stream "<td class=mrsValue>~(~a~)</td>~%" value)))

(defmethod mrs-output-start-rel ((mrs html) pred firstp)
  (declare (ignore firstp))
  (with-slots (stream i nrows) mrs
    (when (and (not (zerop i)) (zerop (mod i *mrs-relations-per-row*)))
      (format 
       stream 
       "</tr></table></td></tr>~%~
        <tr><td><table class=mrsRelsContainer><tr>~%"))
    (format 
     stream 
     "    <td><table class=mrsRelation>~%      ~
      <tr><td class=mrsPredicate colspan=2>~(~a~)</td>~%"
     pred)
    (incf i)))

(defmethod mrs-output-rel-handel ((mrs html) handle)
  (when handle
    (with-slots (stream) mrs
      (format 
       stream 
       "      <tr><td class=mrsLabel>LBL</td><td class=mrsValue>~%
        <div class=\"mrsVariable~:(~a~)\"~
             onMouseOver=\"mrsVariableSelect('~:(~a~)')\"~
             onMouseOut=\"mrsVariableUnselect('~:(~a~)')\">~(~a~)</div></td>~%"
       handle handle handle handle))))

(defmethod mrs-output-label-fn  ((mrs html) label)
  (with-slots (stream) mrs
    (format 
     stream 
     "      <tr><td class=mrsLabel>~a</td>"
     label)))

(defmethod mrs-output-start-extra ((mrs html) type)
  (declare (ignore type)))

(defmethod mrs-output-extra-feat  ((mrs html) feature)
  (declare (ignore feature)))

(defmethod mrs-output-extra-val  ((mrs html) value)
  (declare (ignore value)))

(defmethod mrs-output-end-extra ((mrs html)))

(defmethod mrs-output-end-rel ((mrs html))
  (with-slots (stream) mrs
    (format stream "    </table></td>~%")))

(defmethod mrs-output-end-liszt ((mrs html))
  (with-slots (stream) mrs
    (format 
     stream 
     "</tr></table></td></tr></table>~%    ~
      <td valign=middle><span class=mrsBracket>}</span></td>~%  ~
      </table>~%</td>~%")))

(defmethod mrs-output-start-h-cons ((mrs html))
  (with-slots (stream) mrs
    (format 
     stream 
     "<tr><td class=mrsFeatureHcons>HCONS</td>~
      <td class=mrsValueHcons>{ ")))

(defmethod mrs-output-outscopes ((mrs html) relation higher lower firstp)
  (with-slots (stream) mrs
    (unless firstp (format stream ", "))
    (format 
     stream 
     "<span class=\"mrsVariable~:(~a~)\"~
            onMouseOver=\"mrsVariableSelect('~:(~a~)')\"~
            onMouseOut=\"mrsVariableUnselect('~:(~a~)')\">~(~a~)</span> ~
      ~a ~
      <span class=\"mrsVariable~:(~a~)\"~
            onMouseOver=\"mrsVariableSelect('~:(~a~)')\"~
            onMouseOut=\"mrsVariableUnselect('~:(~a~)')\">~(~a~)</span>"
     higher higher higher higher 
     relation 
     lower lower lower lower)))

(defmethod mrs-output-end-h-cons ((mrs html))
  (with-slots (stream) mrs
    (format stream " }</td>~%")))

(defmethod mrs-output-end-psoa ((mrs html))
  (with-slots (stream) mrs
    (format stream "</table>")))


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

(let ((lock #+:allegro (mp:make-process-lock) #-:allegro nil))
  (defun output-mrs1 (mrs-instance device stream)
    (#+:allegro mp:with-process-lock #+:allegro (lock) #-:allegro progn
      (def-print-operations device 0 stream)
      (initialize-display-structure *mrs-display-structure* mrs-instance)
      (cond ((psoa-p mrs-instance)
             (mrs-output-start-fn *mrs-display-structure*)
             (print-psoa mrs-instance)
             (mrs-output-end-fn *mrs-display-structure*)
             (mrs-output-max-width-fn *mrs-display-structure*))
            (t (mrs-output-error-fn *mrs-display-structure* mrs-instance))))))

(defparameter *already-seen-vars* nil)

(defun find-var-name (var connected-p)
  (if var
      (if connected-p
          (get-bound-var-value var)
        (var-string var))))

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
                                (rel-pred rel) first-rel)
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
  (when *rel-handel-path*
    (print-mrs-hcons (psoa-h-cons psoa) connected-p *mrs-display-structure*))
  (mrs-output-end-psoa *mrs-display-structure*))

(defun print-mrs-hcons (hcons-list connected-p display)
    (mrs-output-start-h-cons display)
    (let ((first-hcons t))
      (loop for hcons in hcons-list
          do
            (mrs-output-outscopes 
             display
             (hcons-relation hcons)
             (find-var-name 
              (hcons-scarg hcons) connected-p) 
             (find-var-name 
              (hcons-outscpd hcons) connected-p)
             first-hcons)
            (setf first-hcons nil)))
    ;; extra info can be ignored here because all handels
    ;; will have appeared elsewhere
    (mrs-output-end-h-cons display))


(defun print-mrs-extra (var)
  (when (and (var-p var) (var-type var) (var-extra var))
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

FEATPAIR -> FEATNAME: VAR | CFEATNAME: CONSTNAME

VAR -> VARNAME | VARNAME [ VARTYPE EXTRAPAIR* ]

EXTRAPAIR -> PATHNAME: CONSTNAME

QEQ -> VARNAME RELNNAME VARNAME


|#

;;; the following is for the simplified MRS structures
;;; where there are no handels or hcons

#|

MRS -> [ INDEX LISZT ]

INDEX -> index: VAR

LISZT -> liszt: < REL* >

REL -> [ PREDNAME FEATPAIR* ]

FEATPAIR -> FEATNAME: VAR | CFEATNAME: CONSTNAME

VAR -> VARNAME | VARNAME [ VARTYPE EXTRAPAIR* ]

EXTRAPAIR -> PATHNAME: CONSTNAME

|#

(defun make-mrs-break-table nil 
  (lkb::define-break-characters '(#\< #\> #\:
                                      #\[ #\] #\, #\{ #\})))

(defun mrs-check-for (character istream)
   (let ((next-char (peek-char t istream nil 'eof)))
     (if (char-equal next-char character)
         (read-char istream)
         (error
                 "~%Syntax error: ~A expected and not found at position ~A" 
                 character (file-position istream)))))



(defun read-mrs-files-aux (file-names)
  (loop for file-name in file-names
      append
        (progn 
          (format t "~%Reading in MRS file ~A" (pathname-name file-name))
          (force-output t)
          (with-open-file 
              (istream file-name :direction :input)
            (read-mrs-stream istream)))))

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
;;; or
;;; MRS -> [ INDEX LISZT ]
    (setf *already-read-vars* nil)
    (mrs-check-for #\[ istream)
    (let* ((ltop (if *rel-handel-path* (read-mrs-ltop istream)))
           (index (read-mrs-index istream))
           (liszt (read-mrs-liszt istream))
           (hcons (if *rel-handel-path* (read-mrs-hcons istream)))
           (psoa
            (make-psoa :top-h ltop
                       :index index
                       :liszt liszt
                       :h-cons hcons)))
      (mrs-check-for #\] istream)
      psoa)))

(defun read-mrs-ltop (istream)
;;;  LTOP -> top: VAR
  (mrs-check-for #\l istream)
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
  ;;; LISZT -> rels: < REL* >
  (let ((rels nil))
    (mrs-check-for #\r istream)
    (mrs-check-for #\e istream)
    (mrs-check-for #\l istream)
    (mrs-check-for #\s istream)
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
;;;  REL -> [ PREDNAME handel: VAR FEATPAIR* ]
;;; or
;;; REL -> [ PREDNAME FEATPAIR* ]
  (mrs-check-for #\[ istream)
  (let* ((relpred (read-mrs-atom istream)))
    (when *rel-handel-path*
      (mrs-check-for #\l istream)
      (mrs-check-for #\b istream)
      (mrs-check-for #\l istream)
      (mrs-check-for #\: istream))
    (let ((hvar (if *rel-handel-path* (read-mrs-var istream)))
          (featpairs nil))
      (loop 
        (let ((next-char (peek-char t istream nil 'eof)))
          (when (eql next-char 'eof) (error "Unexpected eof"))
          (when (eql next-char #\]) 
            (read-char istream)
            (return))
          (push (read-mrs-featpair istream)
                featpairs)))
      (make-rel :pred relpred
                :handel hvar
                :flist (sort featpairs #'feat-sort-func)))))
          
(defun read-mrs-featpair (istream)         
  ;; FEATPAIR -> FEATNAME: VAR | CFEATNAME: CONSTNAME
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
                  (make-var :id (funcall *variable-generator*)))))
    (unless existing 
      (push (cons varname var) *already-read-vars*))
    (let ((next-char (peek-char t istream nil 'eof)))
      (when (eql next-char 'eof) (error "Unexpected eof"))
      (when (eql next-char #\[)
        (read-char istream)
        (let ((extra nil)
              (var-type (string-downcase 
			 (string (read-mrs-atom istream)))))
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
                  (make-var :id (gensym (string varname))))))
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

;;; Reading in an MRS that was written out in the `indexed' format

#|

MRS -> < LTOP, INDEX, LISZT, HCONS >

LTOP -> VAR

INDEX -> VAR

LISZT -> { } | { [REL,]* REL }

HCONS -> {} | { [QEQ,]* QEQ }

REL -> VARNAME:PREDNAME([ARG,]* ARG)

ARG -> VAR | STRING | CONSTNAME

VAR -> VARNAME[:CONSTNAME]*

QEQ -> VARNAME RELNNAME VARNAME


|#

;;; or in the simplified format

#|

MRS -> < INDEX, LISZT >

INDEX -> VAR

LISZT -> { } | { [REL,]* REL }

REL -> PREDNAME([ARG,]* ARG)

ARG -> VAR | STRING | CONSTNAME

VAR -> VARNAME[:CONSTNAME]*

|#

(defun read-indexed-mrs (istream)
  (let ((*readtable* (make-mrs-break-table)))
;;; MRS -> < LTOP, INDEX, LISZT, HCONS >
;;; or
;;; MRS -> < INDEX, LISZT >
    (setf *already-read-vars* nil)
    (mrs-check-for #\< istream)
    (let* ((ltop (if *rel-handel-path* 
                     (read-mrs-indexed-ltop istream)))
           (index (read-mrs-indexed-index istream))
           (liszt (read-mrs-indexed-liszt istream))
           (hcons (if *rel-handel-path*
                      (read-mrs-indexed-hcons istream)))
           (psoa
            (make-psoa :top-h ltop
                       :index index
                       :liszt liszt
                       :h-cons hcons)))
      (mrs-check-for #\> istream)
      psoa)))

(defun read-mrs-indexed-ltop (istream)
;;; LTOP -> VAR
  (let ((var
         (read-mrs-indexed-var istream)))
    (mrs-check-for #\, istream)
    var))
  
(defun read-mrs-indexed-index (istream)
;;; INDEX -> VAR
  (let ((var
         (read-mrs-indexed-var istream)))
    (mrs-check-for #\, istream)
    var))

(defun read-mrs-indexed-liszt (istream)
  ;;; LISZT -> { } | { [REL,]* REL }
  (let ((rels nil)
        (first-p t))
    (mrs-check-for #\{ istream)
    (loop 
      (let ((next-char (peek-char t istream nil 'eof)))
        (when (eql next-char 'eof) (error "Unexpected eof"))
        (when (eql next-char #\}) (return))
        (unless first-p
          (mrs-check-for #\, istream))
        (setf first-p nil)
        (push (read-mrs-indexed-rel istream)
              rels)))
    (mrs-check-for #\} istream)
    (mrs-check-for #\, istream)
    rels))

(defun read-mrs-indexed-hcons (istream)
;;; HCONS -> {} | { [QEQ,]* QEQ }
  (let ((cons nil)
        (first-p t))
    (mrs-check-for #\{ istream)
    (loop 
      (let ((next-char (peek-char t istream nil 'eof)))
        (when (eql next-char 'eof) (error "Unexpected eof"))
        (when (eql next-char #\}) (return))
        (unless first-p
          (mrs-check-for #\, istream))
        (setf first-p nil)       
        (push (read-mrs-qeq istream)
              cons)))
    (mrs-check-for #\} istream)
    cons))

(defun read-mrs-indexed-rel (istream)
;;; REL -> VARNAME:PREDNAME([ARG,]* ARG)  
;;; or
;;; REL -> PREDNAME([ARG,]* ARG)
  (let ((hvar (if *rel-handel-path* 
                  (read-mrs-simple-var istream))))
    (when *rel-handel-path*
      (mrs-check-for #\: istream))
    (let ((predname (read-mrs-atom istream)))
      (mrs-check-for #\( istream)
      (let ((featpairs nil)
            (first-p t)
            (pos 1))
        (loop 
          (let ((next-char (peek-char t istream nil 'eof)))
            (when (eql next-char 'eof) (error "Unexpected eof"))
            (when (eql next-char #\)) 
              (read-char istream)
              (return))
            (unless first-p
              (mrs-check-for #\, istream))
            (setf first-p nil)
            (push (read-mrs-indexed-featpair istream predname pos)
                  featpairs)
            (incf pos)))
        (make-rel :pred predname
                  :handel hvar
                  :flist (sort featpairs #'feat-sort-func))))))
          
(defun read-mrs-indexed-featpair (istream relname pos)        
;;; ARG -> VAR | STRING | CONSTNAME
  ;;; Note that in this notation, the feature has to be determined from
  ;;; a semdb
  (let* ((feature (determine-mrs-feature relname pos))
         (val
           (if (member feature *value-feats*
                       :test #'eq)
               (read-mrs-atom istream)
             (read-mrs-indexed-var istream))))
      (make-fvpair :feature feature
                   :value val)))

(defun read-mrs-indexed-var (istream)
  ;; VAR -> VARNAME[:CONSTNAME]*
  ;; note that the extra info is filled in from the semdb
  (let* ((varname (read-mrs-atom istream))
         (existing (assoc varname *already-read-vars*))
         (var (or (cdr existing)
                  (make-var :id (funcall *variable-generator*)))))
    (unless existing 
      (push (cons varname var) *already-read-vars*))
    (let ((extra nil))
      (loop
        (let ((next-char (peek-char t istream nil 'eof)))
          (when (eql next-char 'eof) (error "Unexpected eof"))
          (when (not (eql next-char #\:)) (return))
          (mrs-check-for #\: istream)
          (let* ((val (read-mrs-atom istream))
                (pathname (determine-mrs-pathname val)))
            (if pathname
                (push
                 (make-extrapair :feature pathname
                                 :value val)
                 extra)
              (progn 
                (when (var-type var)
                  (error "Multiple type specs"))
                (setf (var-type var) (string-downcase (string val))))))))
      (when extra
        (setf (var-extra var) extra)))
    var))

(defun determine-mrs-pathname (val)
  (declare (ignore val))
  'DUMMY)




