;;; Copyright (c) 1998--2004
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

;;; rmrs is a substructure of this (see basermrs.lisp)

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

(defmacro make-scratch () '(cons -1 nil))

(defstruct (rel-base)
  pred					; relation name
  flist)

(defstruct (rel (:include rel-base))
  handel                               
  parameter-strings			; the constant values
					; a junk slot used by the
                                        ; generator and comparison code
  extra                                 ; extra is a junk slot
                                        ; needed for the munging rules 
  (scratch (make-scratch)))             ; another junk slot (for MT transfer)

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

(defstruct (var-base)
  type
  extra) ; e.g. agreement values

(defstruct (var (:include var-base))
  id
  (scratch (make-scratch)))

(defstruct (extrapair)
  feature
  value)

(defstruct (grammar-var (:include var)))
;;; a sort of placeholder variable used in RMRS code

(defstruct (hcons)
  relation
  scarg
  outscpd)

;;; relation is one of "qeq", "lheq" or "outscopes" as in the rmrs.dtd
;;; the code for grammar read in must convert the type to one
;;; of these

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
;;; the mapping from the ERG (in mrsoutput.lisp)
;;; adds d and v (should be u??)

(defun var-string (var)
  (cond
   ((grammar-var-p var)
    (var-id var))
   ((and (var-p var) (not (eq (var-id var) :dummy)))
    (format nil 
	    "~@[~(~A~)~]~A" 
	    (when (var-id var) (or (var-type var) "u")) (var-id var)))
   ((var-base-p var)
    (format nil 
	    "~@[~(~A~)~]" 
	    (or (var-base-type var) "u")))
   (t
    (error "var expected ~A found" var))))

;;; macros moved from mrsresolve

(defmacro is-handel-var (var)
    ;;; test is whether the type is "h"
  `(and (var-p ,var)
       (equal (var-type ,var) "h")))

(defmacro nonquantified-var-p (var)
  ;;; true if the type is anything other than x
  `(and (var-p ,var)
	(not (equal (var-type ,var) "x"))))

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

;;; test for variable equality

(defun eql-var-id (var1 var2)
  ;;; can't be macroized cos used where fn is required
  ;;; has to be `equal' since
  ;;; used for grammar vars etc in RMRS composition code
  ;;; where the id is a string
  (equal (var-id var1) (var-id var2)))

;;;
;;; make debugging with MRSs a little easier: print very compact representation
;;; of object by default; set *mrs-raw-output-p* to see things in full glory.
;;;
(defparameter *mrs-raw-output-p* t)

(defmethod print-object ((object psoa) stream)
  (if *mrs-raw-output-p*
    (call-next-method)
    (output-mrs1 object 'debug stream)))

(defmethod print-object ((object rel) stream)
  (if *mrs-raw-output-p*
    (call-next-method)
    (format 
     stream
     "~@[~a:~]~(~s~)"
     (and (rel-handel object) (var-string (rel-handel object)))
     (rel-pred object))))

(defmethod print-object ((object var) stream)
  (if *mrs-raw-output-p*
    (call-next-method)
    (format stream "~a" (var-string object))))

;;; The MRS structure could be output either as simple ascii
;;; or as LaTeX and possibly in other ways
;;; So use the same trick as the LKB to avoid unnecessary work
;;; for different output types

(defparameter *mrs-display-structure* nil)

(defun def-print-operations (class indentation stream)
  (setf *mrs-display-structure* 
    (make-instance class 
      :indentation indentation :stream stream)))

;;; Sept 04 - now using indentation more systematically,
;;; allows for two column output for comparisons

;;; 
;;; Generic output-type class
;;;

(defclass output-type ()
  ((indentation :initform 0 :initarg :indentation)
   (stream :initform t :initarg :stream)))

(defmethod initialize-display-structure ((class output-type) mrs &optional id)
  (declare (ignore mrs id)))

(defmethod mrs-output-error-fn ((mrsout output-type) mrs-instance)
  (with-slots (stream) mrsout
    (format stream "~%::: ~A is not a psoa struct~%" mrs-instance)))

(defmethod mrs-output-max-width-fn ((mrsout output-type))
  nil)

(defmethod mrs-output-end-fvpair-fn ((mrsout output-type))
    nil)

;;; 
;;; simple output-type class
;;;

(defclass simple (output-type) 
  ((line-per-rel :initform nil :initarg line-per-rel)))

(defmethod mrs-output-start-fn ((mrsout simple))
  (with-slots (stream) mrsout
    (format stream "~V%" 1)))

(defmethod mrs-output-end-fn ((mrsout simple))
  (with-slots (stream) mrsout
    (format stream "~V%" 1)))

(defmethod mrs-output-start-psoa ((mrsout simple))
  (with-slots (stream indentation) mrsout
    (format stream "~VT[" indentation)))

(defmethod mrs-output-top-h ((mrsout simple) handel-val)
  (when (and handel-val *rel-handel-path*)
    (with-slots (stream) mrsout
      (format stream " LTOP: ~(~a~)" handel-val))))

(defmethod mrs-output-index ((mrsout simple) index-val &optional properties)
  (declare (ignore properties))
  (with-slots (stream indentation) mrsout
    (when index-val
      (format stream "~%~VT  INDEX: ~(~a~)" indentation index-val))))

(defmethod mrs-output-start-liszt ((mrsout simple))
  (with-slots (stream indentation) mrsout
    (format stream "~%~VT  RELS: <" indentation)
    (setf indentation (+ indentation 10))))

(defmethod mrs-output-var-fn ((mrsout simple) var-string &optional properties)
  (declare (ignore properties))
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
  (with-slots (stream indentation line-per-rel) mrsout
    (if line-per-rel
	(format stream " ~a: " (+ indentation 2) label)
      (format stream "~%~VT~a: " (+ indentation 2) label))))
  
(defmethod mrs-output-start-extra ((mrsout simple) var-type)
  (with-slots (stream indentation) mrsout
    (format stream " [ ~A" var-type)))

(defmethod mrs-output-extra-feat  ((mrsout simple) feat)
  (with-slots (stream indentation) mrsout
    (format stream " ~A: " feat)))

(defmethod mrs-output-extra-val  ((mrsout simple) val)
  (with-slots (stream) mrsout
    (format stream "~A" val)))

(defmethod mrs-output-end-extra ((mrsout simple))
  (with-slots (stream) mrsout
    (format stream " ]")))

(defmethod mrs-output-end-rel ((mrsout simple))
  (with-slots (stream) mrsout
    (format stream " ]")))

(defmethod mrs-output-end-liszt ((mrsout simple))
  (with-slots (stream indentation) mrsout
    (format stream " >")
    (setf indentation (- indentation 10))))

(defmethod mrs-output-start-h-cons ((mrsout simple))
  (with-slots (stream indentation) mrsout
    (format stream "~%~VT  HCONS: <" indentation)))

(defmethod mrs-output-outscopes ((mrsout simple) reln higher lower first-p)
  (declare (ignore first-p))
  (with-slots (stream indentation) mrsout
    (format stream " ~(~a~) ~A ~(~a~)" higher reln lower)))

(defmethod mrs-output-end-h-cons ((mrsout simple))
  (with-slots (stream) mrsout
    (format stream " >")))


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
;;; column-two output class (for displaying two MRSs side by side)
;;;

(defclass column-two (active-t)
  ())

(defmethod mrs-output-start-fn ((mrsout column-two))
  (with-slots (stream indentation) mrsout
    (setf indentation (+ indentation 60))
    (format stream "~V%" 1)))

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

(defmethod mrs-output-index ((mrsout indexed) index-val &optional properties)
  (declare (ignore properties))
  (with-slots (stream) mrsout
    (format stream "~(~a~)" index-val)))

(defmethod mrs-output-start-liszt ((mrsout indexed))
  (with-slots (stream) mrsout
    (format stream ",~%{")))

(defmethod mrs-output-var-fn ((mrsout indexed) var-string &optional properties)
  (declare (ignore properties))
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

(defmethod mrs-output-index ((mrsout prolog) index-val &optional properties)
  (declare (ignore properties))
  (with-slots (stream) mrsout
    (format stream ",~(~a~)" index-val)))

(defmethod mrs-output-start-liszt ((mrsout prolog))
  (with-slots (stream) mrsout
    (format stream ",[")))

(defmethod mrs-output-var-fn ((mrsout prolog) var-string &optional properties)
  (declare (ignore properties))
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

(defun mrs-variable-html (variable properties id class stream)
  (let ((string (make-string-output-stream)))
    (when properties
      (format string "<table class=mrsProperties>")
      (loop
          for property in properties
          do
            (format 
             string
             "<tr><td class=mrsPropertyFeature>~a~
                  <td class=mrsPropertyValue>~(~a~)</td></tr>"
             (extrapair-feature property) (extrapair-value property)))
      (format string "</table>"))
    (format 
     stream 
     "<td class=~a>~%  ~
      <div class=\"mrsVariable~a~:(~a~)\"~%       ~
           onMouseOver=\"mrsVariableSelect('~a~:(~a~)', '~a')\"~%       ~
           onMouseOut=\"mrsVariableUnselect('~a~:(~a~)')\">~(~a~)</div>~%~
      </td>~%" 
     class id variable id variable 
     (get-output-stream-string string)
     id variable variable)))
  
(defclass html (output-type) 
  ((id :initform 0)
   (nrels :initform nil)
   (i :initform nil)
   (nrows :initform nil)))

(defmethod initialize-display-structure ((class html) mrs &optional n)
  (with-slots (id nrels nrows i) class
    (setf id n)
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
    (with-slots (id stream) mrs
      (format stream "<tr><td class=mrsFeatureTop>TOP</td>~%")
      (mrs-variable-html handle nil id "mrsValueTop" stream))))

(defmethod mrs-output-index ((mrs html) index &optional properties)
  (when index
    (with-slots (id stream) mrs
      (format stream "<tr>~%<td class=mrsFeatureIndex>INDEX</td>~%")
      (mrs-variable-html index properties id "mrsFeatureIndex" stream))))

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

(defmethod mrs-output-var-fn ((mrs html) variable &optional properties)
  (with-slots (id stream) mrs
    (mrs-variable-html variable properties id "mrsValue" stream)))

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
    (with-slots (id stream) mrs
      (format stream "      <tr><td class=mrsLabel>LBL</td>")
      (mrs-variable-html handle nil id "mrsValue" stream))))

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
  (with-slots (id stream) mrs
    (unless firstp (format stream ", "))
    (format 
     stream 
     "<span class=\"mrsVariable~a~:(~a~)\"~
            onMouseOver=\"mrsVariableSelect('~a~:(~a~)', '')\"~
            onMouseOut=\"mrsVariableUnselect('~a~:(~a~)')\">~(~a~)</span> ~
      ~a ~
      <span class=\"mrsVariable~a~:(~a~)\"~
            onMouseOver=\"mrsVariableSelect('~a~:(~a~)', '')\"~
            onMouseOut=\"mrsVariableUnselect('~a~:(~a~)')\">~(~a~)</span>"
     id higher id higher id higher higher 
     relation 
     id lower id lower id lower lower)))

(defmethod mrs-output-end-h-cons ((mrs html))
  (with-slots (stream) mrs
    (format stream " }</td>~%")))

(defmethod mrs-output-end-psoa ((mrs html))
  (with-slots (stream) mrs
    (format stream "</table>")))

;;; 
;;; maximally compact debugging output-type class
;;;

(defclass debug (output-type) 
  ((memory :initform nil)))

(defmethod initialize-display-structure ((class debug) mrs &optional n)
  (declare (ignore mrs n)))
    
(defmethod mrs-output-start-fn ((mrs debug)))

(defmethod mrs-output-end-fn ((mrs debug)))

(defmethod mrs-output-start-psoa ((mrs debug)))

(defmethod mrs-output-top-h ((mrs debug) handle)
  (when (and handle *rel-handel-path*)
    (with-slots (id stream) mrs
      (format stream "~a:" handle))))

(defmethod mrs-output-index ((mrs debug) index &optional properties)
  (declare (ignore properties))
  (when index
    (with-slots (id stream) mrs
      (format stream "~a:" index))))

(defmethod mrs-output-start-liszt ((mrs debug))
  (with-slots (stream nrows) mrs
    (format stream "{")))

(defmethod mrs-output-var-fn ((mrs debug) variable &optional properties)
  (declare (ignore variable properties)))

(defmethod mrs-output-atomic-fn ((mrs debug) value)
  (declare (ignore value)))

(defmethod mrs-output-start-rel ((mrs debug) pred firstp)
  (declare (ignore firstp))
  (with-slots (stream memory) mrs
    (setf memory (when pred
                   (if (stringp pred) 
                     (format nil "~(~s~)" pred)
                     (format nil "~(~a~)" pred))))
    (format stream " ")))

(defmethod mrs-output-rel-handel ((mrs debug) handle)
  (when handle
    (with-slots (stream memory) mrs
      (format stream "~a:~@[~a~]" handle memory))))

(defmethod mrs-output-label-fn  ((mrs debug) label)
  (declare (ignore label)))

(defmethod mrs-output-start-extra ((mrs debug) type)
  (declare (ignore type)))

(defmethod mrs-output-extra-feat  ((mrs debug) feature)
  (declare (ignore feature)))

(defmethod mrs-output-extra-val  ((mrs debug) value)
  (declare (ignore value)))

(defmethod mrs-output-end-extra ((mrs debug)))

(defmethod mrs-output-end-rel ((mrs debug)))

(defmethod mrs-output-end-liszt ((mrs debug))
  (with-slots (stream) mrs
    (format stream " }")))

(defmethod mrs-output-start-h-cons ((mrs debug)))

(defmethod mrs-output-outscopes ((mrs debug) relation higher lower firstp)
  (declare (ignore relation higher lower firstp)))

(defmethod mrs-output-end-h-cons ((mrs debug)))

(defmethod mrs-output-end-psoa ((mrs debug)))

;;; XML output class (quite similar to RMRS)

(defclass mrs-xml (output-type) ())

;;; <!ELEMENT mrs (var, (ep|hcons)*)>

(defmethod mrs-output-start-fn ((mrsout mrs-xml))
  (with-slots (stream) mrsout
    (format stream "~%<mrs>~%")))

(defmethod mrs-output-end-fn ((mrsout mrs-xml))
  (with-slots (stream) mrsout
    (format stream "~%</mrs>~%")))

(defmethod mrs-output-start-psoa ((mrsout mrs-xml))
  nil)

(defmethod mrs-output-top-h ((mrsout mrs-xml) handel-val)
  (with-slots (stream) mrsout
    (format stream "<var vid='~A'/>" handel-val)))


(defmethod mrs-output-index ((mrsout mrs-xml) index-val &optional properties)
  (declare (ignore index-val properties))
  nil)

(defmethod mrs-output-start-liszt ((mrsout mrs-xml))
  nil)

#|
<!ELEMENT var EMPTY>
<!ATTLIST var
          vid  CDATA #REQUIRED >

extras have to be sorted out later          
|#

(defmethod mrs-output-var-fn ((mrsout mrs-xml) var-string &optional properties)
  (declare (ignore properties))
  (with-slots (stream) mrsout
    (format stream "<var vid='~A'/>" var-string)))

(defmethod mrs-output-atomic-fn ((mrsout mrs-xml) atomic-value)
  (with-slots (stream) mrsout
    (format stream "<constant>~A</constant>" atomic-value)))

#|
<!ELEMENT ep (pred, var, fvpair*)>
;;; var is the label
<!ELEMENT pred (#PCDATA)>
|#

(defmethod mrs-output-start-rel ((mrsout mrs-xml) pred first-p)
  (declare (ignore first-p))
  (with-slots (stream) mrsout
    (format stream "~%<ep>")
    (format stream "<pred>~(~a~)</pred>" pred)))

(defmethod mrs-output-rel-handel ((mrsout mrs-xml) handel)
  (with-slots (stream) mrsout
    (format stream "<var vid='~A'/>" handel)))

#|
<!ELEMENT fvpair (rargname, (var|constant))>

<!ELEMENT rargname (#PCDATA)>

<!ELEMENT constant (#PCDATA)>
|#

(defmethod mrs-output-label-fn  ((mrsout mrs-xml) label)
  (with-slots (stream) mrsout
    (format stream "~%<fvpair><rargname>~A</rargname>" label)))

(defmethod mrs-output-end-fvpair-fn ((mrsout mrs-xml))
  (with-slots (stream) mrsout
    (format stream "</fvpair>")))

  
(defmethod mrs-output-start-extra ((mrsout mrs-xml) var-type)
  (declare (ignore var-type))
  nil)

(defmethod mrs-output-extra-feat  ((mrsout mrs-xml) feat)
  (declare (ignore feat))
  nil)

(defmethod mrs-output-extra-val  ((mrsout mrs-xml) val)
    (declare (ignore val))
  nil)

(defmethod mrs-output-end-extra ((mrsout mrs-xml))
  nil)

(defmethod mrs-output-end-rel ((mrsout mrs-xml))
  (with-slots (stream) mrsout
    (format stream "</ep>")))

(defmethod mrs-output-end-liszt ((mrsout mrs-xml))
  nil)

#|
<!ELEMENT hcons (hi, lo)>
<!ATTLIST hcons 
          hreln (qeq|lheq|outscopes) #REQUIRED >

<!ELEMENT hi (var)>
<!ELEMENT lo (var)>
|#

(defmethod mrs-output-start-h-cons ((mrsout mrs-xml))
  nil)

(defmethod mrs-output-outscopes ((mrsout mrs-xml) reln higher lower first-p)
  (declare (ignore first-p))
  (with-slots (stream) mrsout
    (format stream "~%<hcons hreln='~A'><hi>"
            (string-downcase reln))
    ;;; same as var
    (format stream "<var vid='~A'/>" higher)
    (format stream "</hi><lo>")
    ;;; same as var
    (format stream "<var vid='~A'/>" lower)
    (format stream "</lo></hcons>")))

(defmethod mrs-output-end-h-cons ((mrsout mrs-xml))
  nil)

(defmethod mrs-output-end-psoa ((mrsout mrs-xml))
  nil)

;;; end of classes

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
  (defun output-mrs1 (mrs-instance device stream &optional (id 0))
    (#+:allegro mp:with-process-lock #+:allegro (lock) #-:allegro progn
      (def-print-operations device 0 stream)
      (initialize-display-structure *mrs-display-structure* mrs-instance id)
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
                    (find-var-name (psoa-index psoa) connected-p)
                    (and (psoa-index psoa) (var-extra (psoa-index psoa))))
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
                         (find-var-name value connected-p)
                         (var-extra value))
                        (print-mrs-extra value))
                    (mrs-output-atomic-fn 
                     *mrs-display-structure*
                     value)))
                (mrs-output-end-fvpair-fn *mrs-display-structure*))
          (mrs-output-end-rel *mrs-display-structure*)
	  ;;; ideally replace above do-clause with this:
	;  (print-rel :display-to *mrs-display-structure*
	;	     :first-rel first-rel
	;	     :connected-p connected-p)
	  (setf first-rel nil)))
  (mrs-output-end-liszt *mrs-display-structure*)
  (when *rel-handel-path*
    (print-mrs-hcons (psoa-h-cons psoa) connected-p *mrs-display-structure*))
  (mrs-output-end-psoa *mrs-display-structure*))

(defun print-rel (rel &key (display-to *mrs-display-structure*) first-rel connected-p)
  (unless (rel-base-p rel)
    (error "unexpected type"))
  ;; print pred name
  (mrs-output-start-rel display-to
			(rel-pred rel) first-rel)
  ;; print handel if valid
  (when (rel-p rel)
    (mrs-output-rel-handel
     display-to 
     (find-var-name (rel-handel rel) connected-p))
    (print-mrs-extra (rel-handel rel)
		     :display-to display-to))
  (loop for feat-val in (rel-flist rel)
      do
	;; print feature
	(mrs-output-label-fn display-to
			     (fvpair-feature feat-val))
	(let ((value (fvpair-value feat-val)))
	  ;; print value struct
	  (if (var-p value)
	      (progn
		(mrs-output-var-fn display-to
				   (find-var-name value connected-p))
		(print-mrs-extra value
				 :display-to display-to))
	    (mrs-output-atomic-fn display-to value))))
  (mrs-output-end-rel display-to))

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


(defun print-mrs-extra (var &key (display-to *mrs-display-structure*))
  (when (and (var-base-p var) (var-base-type var) (var-base-extra var))
    (when (not (member var *already-seen-vars*
		       :test #'eq))
      (mrs-output-start-extra display-to
                              (var-base-type var))
      (loop for extrapair in (var-base-extra var)
          do
            (mrs-output-extra-feat display-to
                                   (extrapair-feature extrapair))
            (mrs-output-extra-val display-to
                                  (extrapair-value extrapair)))
      (mrs-output-end-extra display-to)
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

(defun read-mrs-stream (istream &optional syntax) 
  (let ((psoas nil))
   (loop
      (let ((next-char (peek-char t istream nil 'eof)))
         (when (eql next-char 'eof) (return))
         (cond ((eql next-char #\;) 
                 (read-line istream))
               ; one line comments
               (t (push (if (eql syntax :indexed)
			    (read-indexed-mrs istream)
			    (read-mrs istream))
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
    ;;
    ;; first of all, skip over any initial line-oriented (`;') comments ...
    ;;
    (loop
        for c = (peek-char t istream nil nil)
        while (and c (char= c #\;)) do (read-line istream))
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
      (unfill-mrs psoa))))

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
  ;;
  ;; LFG-derived MRSs (and in principle any MRS not constructed by ourselves)
  ;; may contain things like [ ... ARG0 nil ... ]; make the code below treat
  ;; nil like an "u" variable, even though probably we should complain bitterly
  ;; to our colleagues.                                         (27-jan-04; oe)
  ;;
  (let* ((varname (read-mrs-atom istream))
         (name (and varname (string varname)))
         (type (and name (string-downcase (subseq name 0 1))))
         (id (and name (parse-integer name :start 1 :junk-allowed t)))
         (existing (and name (assoc varname *already-read-vars*)))
         (var (or (cdr existing)
                  (make-var :id (if varname 
                                  (or id (funcall *variable-generator*))
                                  (funcall *variable-generator*))
                            :type (or type "u")))))
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
                  (make-var :id nil))))
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
  (let* ((*package* (find-package *mrs-package*))
         (atomsym (read istream nil 'eof)))
    (when (eq atomsym 'eof) (error "Unexpected eof"))
    ;;
    ;; because LKB type names are either symbols or strings, make sure that
    ;; we return with either one.  convert to symbols, where necessary, since
    ;; TDL allows a specification like [ PERS 1 ], where internally we end up
    ;; using |1| as the type name.                            (8-jan-04; oe)
    ;;
    (if (or (symbolp atomsym) (stringp atomsym))
      atomsym
      (intern (format nil "~a" atomsym) *mrs-package*))))

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

;;; reading in XML version - to be added

;;;
;;; interim solution for MRS `unfilling' until we construct a proper SEMI; note
;;; that unfill-mrs() _destructively_ modifies its input.
;;;
(defparameter %mrs-extras-filter% nil)

(defun unfill-mrs (mrs &optional (filter %mrs-extras-filter%))
  (when filter
    (labels ((unfill-variable (variable)
               (when (var-p variable)
                 (setf (var-extra variable)
                   (loop
                       for extra in (var-extra variable)
                       for feature = (extrapair-feature extra)
                       for value = (extrapair-value extra)
                       unless (loop
                                  for (key . match) in filter
                                  thereis (and (eq feature key) 
                                               (eq value match)))
                       collect extra)))))
      (unfill-variable (psoa-index mrs))
      (loop
          for ep in (psoa-liszt mrs)
          do
            (loop
                for role in (rel-flist ep)
                for value = (fvpair-value role)
                do (unfill-variable value)))))
  mrs)

;;;
;;; another interim solution: fill in `default' values for a range of index
;;; features that are not specified in an input MRS to generation, e.g. assume
;;; that the lack of information about progessive does mean something.
;;;
;;; _fix_me_
;;; i believe one should probably use (generator input) munging rules for this
;;; purpose, but then i would rather build them on the new MRS transfer code,
;;; and for now i hesitate to deploy that in the generator.    (12-dec-03; oe)
;;;
(defparameter %mrs-extras-defaults% 
  (list
   #-:null
   (list (vsym "E") 
         (cons (vsym "E.ASPECT.PROGR") (vsym "-"))
         (cons (vsym "E.ASPECT.PERF") (vsym "-"))
         (cons (vsym "E.TENSE") (vsym "NO_TENSE")))))

(defun fill-mrs (mrs &optional (defaults %mrs-extras-defaults%))
  (when defaults
    (labels ((fill-variable (variable)
               (when (var-p variable)
                 (loop
                     with result = nil
                     with extras = (var-extra variable)
                     with type = (var-type variable)
                     with defaults = (loop
                                         for (key . defaults) in defaults
                                         when (string-equal type key) 
                                         return defaults)
                     for (feature . value) in defaults
                     unless (find feature extras :key #'extrapair-feature) do
                       (push 
                        (make-extrapair :feature feature :value value)
                        result)
                     finally
                       (when result
                         (setf (var-extra variable)
                           (append (var-extra variable) result)))))))
      (loop
          for ep in (psoa-liszt mrs)
          do
            (loop
                for role in (rel-flist ep)
                for value = (fvpair-value role)
                do (fill-variable value)))))
  mrs)
