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
  h-cons
  a-cons
  vcs)

;;; a-cons added to allow for constraints on attachment

;;; rmrs is a substructure of this (see basermrs.lisp)

(defstruct (psoa (:include basemrs))
  index)

;;; psoa records an index - this really is the equivalent of the semstruct
;;; in RMRS - the two should possibly be amalgamated and index should be
;;; replaced by hook, when the code for displaying MRSs according to the
;;; algebra is finally written

;;; for the algebra - rationalise this later 
;;; not just psoa but rmrs semstruct in comp.lisp

(defstruct (sement (:include basemrs))
  hook
  slots
  equalities)

(defstruct (hook)
  index
  ltop
  xarg)

(defstruct (slot)
  hook
  name)

;;; I have changed the old `sort' to `pred' - the old name was 
;;; seriously confusing

;;; ep removed again - was used for the `simple' RMRS
;;; but that's no longer supported

(defstruct (rel-base)
  pred					; relation name
  flist)

(defstruct (rel (:include rel-base))
  str 
  handel                               
  parameter-strings			; the constant values
					; a junk slot used by the
                                        ; generator and comparison code
  extra                                 ; extra is a junk slot
                                        ; needed for the munging rules 
  link)                                 ; link to surface element(s)

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
  id)

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
;;; of these.  although at the moment, only qeqs are supported.

;;; The following structures are for attachment constraints as used
;;; by Berthold.
;;; The phrase to be attached supplies a pair of index and label,
;;; and the target is a set of such pairs.
;;; These are stored on a-cons

(defstruct (disj-cons)
  index-lbl
  target)

(defstruct (index-lbl)
  index
  lbl)


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

(defun is-top-semantics-type (pred)
  (eq pred *top-semantics-type*))

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

;;;
;;; provide a way of suppressing select roles in output; useful when preparing
;;; the input to UTool, e.g. for classification or solving, so as to omit the
;;; pesky TPC and PSV pseudo-roles.                              (5-jul-06; oe)
;;;
(defparameter *output-ignored-roles* nil)

(defmethod print-object ((object psoa) stream)
  (if *mrs-raw-output-p*
    (call-next-method)
    (output-mrs1 object 'debug stream)))

(defmethod print-object ((object rel) stream)
  (if *mrs-raw-output-p*
    (call-next-method)
    (let ((pred (rel-pred object))
          (roles (rel-flist object)))
      (format 
       stream
       "~@[~a:~]~(~a~)("
       (and (rel-handel object) (var-string (rel-handel object)))
       (if (stringp pred)
         (format nil "~s" pred)
         (or pred "_")))
      (loop
          for role in roles
          for value = (fvpair-value role)
          do (format stream "~:[ ~;~]~a" (eq role (first roles)) value))
      (format stream ")"))))

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

(defmethod mrs-output-start-a-cons ((mrsout output-type))
  nil)

(defmethod mrs-output-disj-cons-spec ((mrsout output-type) first-p)
  (declare (ignore first-p))
  nil)

(defmethod mrs-output-disj-cons-start-target ((mrsout output-type))
  nil)

(defmethod mrs-output-disj-cons-end-target ((mrsout output-type))
  nil)

(defmethod mrs-output-end-a-cons ((mrsout output-type))
  nil)

(defmethod mrs-output-ilp ((mrsout output-type) var1 var2 first-p)
  (declare (ignore var1 var2 first-p))
  nil)

(defmethod mrs-output-vcs ((mrsout output-type) vcs)
  (declare (ignore vcs))
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

(defmethod mrs-output-top-h ((mrsout simple) handel-val
			     &optional properties type id)
  (declare (ignore properties type id))
  (when (and handel-val *rel-handel-path*)
    (with-slots (stream) mrsout
      (format stream " LTOP: ~(~a~)" handel-val))))

(defmethod mrs-output-index ((mrsout simple) index-val 
			     &optional properties type id)
  (declare (ignore properties type id))
  (with-slots (stream indentation) mrsout
    (when index-val
      (format stream "~%~VT  INDEX: ~(~a~)" indentation index-val))))

(defmethod mrs-output-start-liszt ((mrsout simple))
  (with-slots (stream indentation) mrsout
    (format stream "~%~VT  RELS: <" indentation)
    (setf indentation (+ indentation 10))))

(defmethod mrs-output-var-fn ((mrsout simple) var-string 
			      &optional properties type id)
  (declare (ignore properties type id))
  (with-slots (stream) mrsout
    (format stream "~(~a~)" var-string)))

(defmethod mrs-output-atomic-fn ((mrsout simple) atomic-value)
  (with-slots (stream) mrsout
    (format stream "~S" atomic-value)))

(defmethod mrs-output-start-rel ((mrsout simple) pred first-p class
				 &optional cfrom cto str)
  (declare (ignore first-p class cfrom cto str))
  (with-slots (stream indentation) mrsout
    (format stream "~%")
    (if (stringp pred)
      (format stream "~VT[ ~s" indentation pred)
      (format stream "~VT[ ~(~a~)" indentation pred))))

(defmethod mrs-output-rel-handel ((mrsout simple) handel 
				  &optional properties sort id)
  (declare (ignore properties sort id))
  (if handel
      (with-slots (stream indentation) mrsout
        (format stream "~%~VT~A: ~(~a~)" (+ indentation 2) 'lbl handel))))

(defmethod mrs-output-rel-link ((mrsout simple) link)
  (when link
    (with-slots (stream indentation) mrsout
      (format stream "~%~vtLNK: <~{~a~^ ~}>" (+ indentation 2) link))))

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

(defmethod mrs-output-outscopes ((mrsout simple) reln higher lower first-p
				 higher-id higher-sort lower-id lower-sort)
  (declare (ignore first-p higher-id higher-sort lower-id lower-sort))
  (with-slots (stream indentation) mrsout
    (format stream " ~(~a~) ~A ~(~a~)" higher reln lower)))

(defmethod mrs-output-end-h-cons ((mrsout simple))
  (with-slots (stream) mrsout
    (format stream " >")))

#| output is e.g.,
ACONS: <x1,h4> in <<x2,h3>,<x5,h6>>, <x11,h41> in <<x21,h31>,<x51,h61>>
|#

(defmethod mrs-output-start-a-cons ((mrsout simple))
  (with-slots (stream indentation) mrsout
    (format stream "~%~VT  ACONS: " indentation)))

(defmethod mrs-output-disj-cons-spec ((mrsout simple) first-acons)
  (with-slots (stream indentation) mrsout
    (format stream "~A " (if first-acons "" ",") indentation)))

(defmethod mrs-output-disj-cons-start-target ((mrsout simple))
  (with-slots (stream indentation) mrsout
    (format stream " in <" indentation)))

(defmethod mrs-output-disj-cons-end-target ((mrsout simple))
  (with-slots (stream indentation) mrsout
    (format stream ">" indentation)))

(defmethod mrs-output-ilp ((mrsout simple) var1 var2 position)
  ;;; position can be :spec, :first-target or :target
  (with-slots (stream indentation) mrsout
    (format stream "~A<~A,~A>" (if (eql position :target) "," "") var1 var2)))

(defmethod mrs-output-end-a-cons ((mrsout simple))
  (with-slots (stream) mrsout
    (format stream "")))

(defmethod mrs-output-vcs ((mrs simple) vcs)
  (with-slots (stream indentation) mrs
    (format stream "~%~vt  VCS: < " indentation)
    (loop
        for foo in vcs
        do (format stream "~(~a~) " foo))
    (format stream ">")))
           

(defmethod mrs-output-end-psoa ((mrsout simple))
  (with-slots (stream indentation) mrsout
    (format stream " ]~%" indentation)))

;;;
;;; active output class (for on-line browsing in CLIM)
;;;

(defclass active-t (simple)
  ())

(defmethod mrs-output-start-rel ((mrsout active-t) pred first-p class
				 &optional cfrom cto str)
  (declare (ignore first-p class cfrom cto str))
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
  
(defmethod mrs-output-top-h ((mrsout indexed) handel-val
			     &optional properties type id)
  (declare (ignore properties type id))
  (if handel-val
      (with-slots (stream) mrsout
        (format stream "~(~a~)," handel-val))))

(defmethod mrs-output-index ((mrsout indexed) index-val 
			     &optional properties type id)
  (declare (ignore properties type id))
  (with-slots (stream) mrsout
    (format stream "~(~a~)" index-val)))

(defmethod mrs-output-start-liszt ((mrsout indexed))
  (with-slots (stream) mrsout
    (format stream ",~%{")))

(defmethod mrs-output-var-fn ((mrsout indexed) var-string 
			      &optional properties type id)
  (declare (ignore properties type id))
  (with-slots (stream) mrsout
    (format stream "~(~a~)" (remove-variable-junk var-string))))

(defmethod mrs-output-atomic-fn ((mrsout indexed) atomic-value)
  (with-slots (stream) mrsout
    (format stream "~S" atomic-value)))

(defmethod mrs-output-start-rel ((mrsout indexed) pred first-p class 
				 &optional cfrom cto str)
  (declare (ignore class cfrom cto str))
  (with-slots (stream temp-pred) mrsout
    (setf temp-pred pred)
    (unless first-p (format stream ",~%"))))

(defmethod mrs-output-rel-handel ((mrsout indexed) handel
				  &optional properties sort id)
  (declare (ignore properties sort id))
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

(defmethod mrs-output-rel-link ((mrsout indexed) link)
  (declare (ignore link)))
  
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
(defmethod mrs-output-outscopes ((mrsout indexed) reln higher lower first-p
				 higher-id higher-sort lower-id lower-sort)
  (declare (ignore  higher-id higher-sort lower-id lower-sort))
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

(defmethod mrs-output-top-h ((mrsout prolog) handel-val
			     &optional properties type id)
  (declare (ignore properties type id))
  (with-slots (stream) mrsout
    (format stream "~(~a~)" handel-val)))

(defmethod mrs-output-index ((mrsout prolog) index-val 
			     &optional properties type id)
  (declare (ignore properties type id))
  (with-slots (stream) mrsout
    (format stream ",~(~a~)" index-val)))

(defmethod mrs-output-start-liszt ((mrsout prolog))
  (with-slots (stream) mrsout
    (format stream ",[")))

(defmethod mrs-output-var-fn ((mrsout prolog) var-string 
			      &optional properties type id)
  (declare (ignore properties type id))
  (with-slots (stream) mrsout
    (format stream "~(~a~))" (remove-variable-junk var-string))))

(defmethod mrs-output-atomic-fn ((mrsout prolog) atomic-value)
  (with-slots (stream) mrsout
    (if (stringp atomic-value)
        (format stream "'~A')" atomic-value)
      (format stream "~A)" atomic-value))))

(defmethod mrs-output-start-rel ((mrsout prolog) pred first-p class
				 &optional cfrom cto str)
  (declare (ignore class cfrom cto str))
  (with-slots (stream) mrsout
    (unless first-p (format stream ","))
    (format stream "rel('~A'," 
            (remove-right-sequence 
	     *sem-relation-suffix*
	     (string-downcase pred)))))

(defmethod mrs-output-rel-handel ((mrsout prolog) handel
				  &optional properties sort id)
  (declare (ignore properties sort id))
  (with-slots (stream temp-pred) mrsout  
    (format stream "~(~a~),[" handel)))

(defmethod mrs-output-rel-link ((mrsout prolog) link)
  (declare (ignore link)))
  
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

(defmethod mrs-output-outscopes ((mrsout prolog) reln higher lower first-p
				 higher-id higher-sort lower-id lower-sort)
  (declare (ignore higher-id higher-sort lower-id lower-sort))
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
   (class :initform nil)
   (nrels :initform nil)
   (i :initform nil)
   (nrows :initform nil)
   (hconss :initform nil)))

(defmethod initialize-display-structure ((class html) mrs &optional n)
  (with-slots (id class nrels nrows i) class
    (setf id n)
    (setf class (determine-mrs-class mrs))
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
  (with-slots (stream class) mrs
    (format stream "<table class=mrs~:[~*~;~:(~a~)~]Mrs>~%" class class)))

(defmethod mrs-output-top-h ((mrs html) handle
			     &optional properties type var-id)
  (declare (ignore properties type var-id))
  (when (and handle *rel-handel-path*)
    (with-slots (id stream) mrs
      (format stream "<tr><td class=mrsFeatureTop>TOP</td>~%")
      (mrs-variable-html handle nil id "mrsValueTop" stream))))

(defmethod mrs-output-index ((mrs html) index 
			     &optional properties type var-id)
  (declare (ignore type var-id))
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

(defmethod mrs-output-var-fn ((mrs html) variable 
			      &optional properties type var-id)
  (declare (ignore type var-id))
  (with-slots (id stream) mrs
    (mrs-variable-html variable properties id "mrsValue" stream)))

(defmethod mrs-output-atomic-fn ((mrs html) value)
  (with-slots (stream) mrs
    (format stream "<td class=mrsValue>~(~a~)</td>~%" value)))

(defmethod mrs-output-start-rel ((mrs html) pred firstp class 
				 &optional cfrom cto str)
  (declare (ignore firstp cfrom cto str))
  (with-slots (stream i nrows) mrs
    (when (and (not (zerop i)) (zerop (mod i *mrs-relations-per-row*)))
      (format 
       stream 
       "</tr></table></td></tr>~%~
        <tr><td><table class=mrsRelsContainer><tr>~%"))
    (format 
     stream 
     "    <td><table class=mrs~:[~*~;~:(~a~)~]Relation>~%      ~
      <tr><td class=mrsPredicate colspan=2>~(~a~)</td>~%"
     class class pred)
    (incf i)))

(defmethod mrs-output-rel-handel ((mrs html) handle 
				  &optional properties sort id)
  (declare (ignore properties sort id))
  (when handle
    (with-slots (id stream) mrs
      (format stream "      <tr><td class=mrsLabel>LBL</td>")
      (mrs-variable-html handle nil id "mrsValue" stream))))

(defmethod mrs-output-rel-link ((mrsout html) link)
  (declare (ignore link)))
  
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

(defmethod mrs-output-outscopes ((mrs html) relation higher lower firstp
				 higher-id higher-sort lower-id lower-sort)
  (declare (ignore higher-id higher-sort lower-id lower-sort))
  (with-slots (id hconss stream) mrs
    (unless firstp (format stream ", "))
    (format 
     stream 
     "<span class=\"mrsVariable~a~:(~a~)\" ~
            onMouseOver=\"mrsVariableSelect('~a~:(~a~)', '')\" ~
            onMouseOut=\"mrsVariableUnselect('~a~:(~a~)')\">~(~a~)</span>~%~
      ~a ~
      <span class=\"mrsVariable~a~:(~a~)\" ~
            onMouseOver=\"mrsVariableSelect('~a~:(~a~)', '')\" ~
            onMouseOut=\"mrsVariableUnselect('~a~:(~a~)')\">~(~a~)</span>~%"
     id higher id higher id higher higher 
     (if (string-equal (string relation) "qeq")
       "=q"
       relation)
     id lower id lower id lower lower)
    (push (cons higher lower) hconss)))

(defmethod mrs-output-end-h-cons ((mrs html))
  (with-slots (stream) mrs
    (format stream " }</td>~%")))

(defmethod mrs-output-end-psoa ((mrs html))
  (with-slots (id hconss stream) mrs
    (format stream "</table>")
    ;;
    ;; generate JavaScript arrays for bi-directional HCONS indexing
    ;;
    (loop
        initially (format stream "<script>~%")
        for (high . low) in hconss
        do
          (format
           stream
           "  mrsHCONSsForward['~a~:(~a~)'] = '~a~:(~a~)';~%  ~
              mrsHCONSsBackward['~a~:(~a~)'] = '~a~:(~a~)';~%"
           id high id low id low id high)
        finally (format stream "</script>~%"))))
     

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

(defmethod mrs-output-top-h ((mrs debug) handle
			     &optional properties type id)
  (declare (ignore properties type id))
  (when (and handle *rel-handel-path*)
    (with-slots (id stream) mrs
      (format stream "~a:" handle))))

(defmethod mrs-output-index ((mrs debug) index 
			     &optional properties type id)
  (declare (ignore properties type id))
  (when index
    (with-slots (id stream) mrs
      (format stream "~a:" index))))

(defmethod mrs-output-start-liszt ((mrs debug))
  (with-slots (stream) mrs
    (format stream "{")))

(defmethod mrs-output-var-fn ((mrs debug) variable 
			      &optional properties type id)
  (declare (ignore properties type id))
  (with-slots (stream memory) mrs
    (format stream "~:[ ~;~]~a" memory variable)
    (setf memory nil)))
           

(defmethod mrs-output-atomic-fn ((mrs debug) value)
  (with-slots (stream memory) mrs
    (format stream "~:[ ~;~]~a" memory value)
    (setf memory nil)))

(defmethod mrs-output-start-rel ((mrs debug) pred firstp class
				 &optional cfrom cto str)
  (declare (ignore firstp class cfrom cto str))
  (with-slots (stream memory) mrs
    (setf memory (if pred
                   (if (stringp pred) 
                     (format nil "~(~s~)" pred)
                     (format nil "~(~a~)" pred))
                   "_"))
    (format stream " ")))

(defmethod mrs-output-rel-handel ((mrs debug) handle 
				  &optional properties sort id)
  (declare (ignore properties sort id))
  (when handle
    (with-slots (stream memory) mrs
      (format stream "~a:~a(" handle memory memory))))

(defmethod mrs-output-rel-link ((mrsout debug) link)
  (declare (ignore link)))
  
(defmethod mrs-output-label-fn  ((mrs debug) label)
  (declare (ignore label)))

(defmethod mrs-output-start-extra ((mrs debug) type)
  (declare (ignore type)))

(defmethod mrs-output-extra-feat  ((mrs debug) feature)
  (declare (ignore feature)))

(defmethod mrs-output-extra-val  ((mrs debug) value)
  (declare (ignore value)))

(defmethod mrs-output-end-extra ((mrs debug)))

(defmethod mrs-output-end-rel ((mrs debug))
  (with-slots (stream) mrs
    (format stream ")")))

(defmethod mrs-output-end-liszt ((mrs debug))
  (with-slots (stream) mrs
    (format stream " }")))

(defmethod mrs-output-start-h-cons ((mrs debug)))

(defmethod mrs-output-outscopes ((mrs debug) relation higher lower firstp
				 higher-id higher-sort lower-id lower-sort)
  (declare (ignore relation higher lower firstp
		   higher-id higher-sort lower-id lower-sort)))

(defmethod mrs-output-end-h-cons ((mrs debug)))

(defmethod mrs-output-end-psoa ((mrs debug)))

;;; XML output class (quite similar to RMRS)

(defclass mrs-xml (output-type) ())

;;; <!ELEMENT mrs (label, (ep|hcons)*)>

;;;
;;; <!ATTLIST mrs
;;;          cfrom CDATA #IMPLIED
;;;          cto   CDATA #IMPLIED 
;;;          surface   CDATA #IMPLIED 
;;;          ident     CDATA #IMPLIED >

(defmethod mrs-output-start-fn ((mrsout mrs-xml))
  (with-slots (stream) mrsout
    (format stream "~%<mrs>~%")))

(defmethod mrs-output-end-fn ((mrsout mrs-xml))
  (with-slots (stream) mrsout
    (format stream "~%</mrs>~%")))

(defmethod mrs-output-start-psoa ((mrsout mrs-xml))
  nil)

(defmethod mrs-output-top-h ((mrsout mrs-xml) handel-val
			     &optional properties type id)
  (declare (ignore properties type handel-val))
  (with-slots (stream) mrsout
    (format stream "<label vid='~A'/>" id)))


(defmethod mrs-output-index ((mrsout mrs-xml) index-val 
			     &optional properties type id)
  (declare (ignore properties type index-val))
  (with-slots (stream) mrsout
    (format stream "<var vid='~A'/>" id)))

(defmethod mrs-output-start-liszt ((mrsout mrs-xml))
  nil)

#|
<!ELEMENT var (extrapair*)>
<!ATTLIST var
          vid  CDATA #REQUIRED 
	  sort (x|e|h|u|l) #IMPLIED >

<!ELEMENT extrapair (path,value)>

<!ELEMENT path (#PCDATA)>

<!ELEMENT value (#PCDATA)>

;;; AAC added sortal values May 2 2006
         
|#

(defmethod mrs-output-var-fn ((mrsout mrs-xml) var-string 
			      &optional properties type id)
  (declare (ignore var-string))
  (with-slots (stream) mrsout
    (format stream "<var vid='~A'" id)
    (when type
	(format stream " sort='~A'" type))
    (format stream ">")    
    (dolist (evp properties)
      (format stream 
	      "~%<extrapair><path>~A</path><value>~A</value></extrapair>" 
	      (extrapair-feature evp) (extrapair-value evp)))
    (format stream "</var>")))


(defmethod mrs-output-atomic-fn ((mrsout mrs-xml) atomic-value)
  (with-slots (stream) mrsout
    (format stream "<constant>~A</constant>" atomic-value)))

#|
<!ELEMENT ep ((pred|realpred), label, fvpair*)>
<!ATTLIST ep
          cfrom CDATA #IMPLIED
          cto   CDATA #IMPLIED 
          surface   CDATA #IMPLIED
	  base      CDATA #IMPLIED >

<!ELEMENT pred (#PCDATA)>

<!ELEMENT spred (#PCDATA)>

;;; realpred for later ease of compatibility - not yet supported
<!ELEMENT realpred EMPTY>

<!ATTLIST realpred
          lemma CDATA #REQUIRED
          pos (v|n|j|r|p|q|c|x|u|a|s) #REQUIRED
          sense CDATA #IMPLIED >
|#

(defmethod mrs-output-start-rel ((mrsout mrs-xml) pred first-p class
				 &optional cfrom cto str)
  (declare (ignore first-p class))
  (with-slots (stream) mrsout
    (format stream "~%<ep")
    (when cfrom 
      (format stream " cfrom='~A'" cfrom))
    (when cto 
      (format stream " cto='~A'" cto))
    (when str 
      (write-string " surface='" stream)
      (xml-escaped-output str stream)
      (write-char #\' stream))
    (write-char #\> stream)
    (if (stringp pred)
	(progn
	  (write-string "<spred>" stream)
	  (xml-escaped-output pred stream)
	  (write-string "</spred>" stream)) 
      (format stream "<pred>~a</pred>" pred))))

(defmethod mrs-output-rel-handel ((mrsout mrs-xml) handel
				  &optional properties sort id)
  (declare (ignore handel properties sort))
  (with-slots (stream) mrsout
    (format stream "<label vid='~A'/>" id)))

#|
<!ELEMENT fvpair (rargname, (var|constant))>

<!ELEMENT rargname (#PCDATA)>

<!ELEMENT constant (#PCDATA)>
|#

(defmethod mrs-output-rel-link   ((mrsout mrs-xml) link)
  (declare (ignore link))
  nil)

(defmethod mrs-output-label-fn  ((mrsout mrs-xml) label)
  (with-slots (stream) mrsout
    (format stream "~%<fvpair><rargname>~A</rargname>" label)))

(defmethod mrs-output-end-fvpair-fn ((mrsout mrs-xml))
  (with-slots (stream) mrsout
    (write-string "</fvpair>" stream)))

  
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
    (write-string "</ep>" stream)))

(defmethod mrs-output-end-liszt ((mrsout mrs-xml))
  nil)

#|
<!ELEMENT hcons (hi, lo)>
<!ATTLIST hcons 
          hreln (qeq|lheq|outscopes) #REQUIRED >

<!ELEMENT hi (var)>
<!ELEMENT lo (label|var)>
|#

(defmethod mrs-output-start-h-cons ((mrsout mrs-xml))
  nil)

(defmethod mrs-output-outscopes ((mrsout mrs-xml) reln 
				 higher lower first-p 
				 higher-id higher-sort lower-id lower-sort)
  (declare (ignore first-p))
  (with-slots (stream) mrsout
    (format stream "~%<hcons hreln='~A'><hi>"
            (string-downcase reln))
    ;;;
    (mrs-output-var-fn mrsout higher nil higher-sort higher-id)
    (format stream "</hi><lo>")
    ;;;
    (mrs-output-var-fn mrsout lower nil lower-sort lower-id)
    ;;;
    (format stream "</lo></hcons>")))

(defmethod mrs-output-end-h-cons ((mrsout mrs-xml))
  nil)

(defmethod mrs-output-end-psoa ((mrsout mrs-xml))
  nil)

;;; variant of above for the case where the output has eqs specified
;;; mrs.dtd is varied so that:
;;; <!ELEMENT mrs (label, var, (ep|hcons-set)*)>
;;; <!ELEMENT hcons-set (hcons)*>
;;; <!ATTLIST hcons hreln (eq) #REQUIRED >

;;; slight hackiness - we output a set of hcons based on the value of
;;; a global variable which needs to be set by the calling function.
#|
(let ((mrs (mrs::extract-mrs edge)))
  (setf mrs::*canonical-bindings* nil)
  (setf mrs::*binding-sets* 
    (mrs::make-scoped-mrs mrs))
  (mrs::output-mrs1 mrs 'mrs::mrs-xml-scoped ostream))
|#

(defparameter *binding-sets* nil)

(defclass mrs-xml-scoped (mrs-xml)
  ())

(defmethod mrs-output-start-h-cons ((mrsout mrs-xml-scoped))
  (with-slots (stream) mrsout
    (dolist (binding *binding-sets*)
      (setf *canonical-bindings* (canonical-bindings binding))
      (format stream "~%<hcons-set>")
      (dolist (pair *canonical-bindings*)
	(let ((first (car pair)) 
	      (second (cdr pair)))
	  (unless (eql first second)
	    (format stream "~%<hcons hreln='eq'><hi>")
    ;;;
	    (mrs-output-var-fn mrsout nil nil "h" first)
	    (format stream "</hi><lo>")
    ;;;
	    (mrs-output-var-fn mrsout nil nil "h" second)
    ;;;
	    (format stream "</lo></hcons>"))))
      (format stream "~%</hcons-set>"))))

(defmethod mrs-output-outscopes ((mrsout mrs-xml-scoped) reln 
				 higher lower first-p 
				 higher-id higher-sort lower-id lower-sort)
  (declare (ignore reln higher lower first-p 
		   higher-id higher-sort lower-id lower-sort))
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
  ;;; AAC - changed behaviour when called with file name
  ;;; so that it will append if an existing file is given
  (if file-name
      (with-open-file (stream file-name :direction :output
		       :if-exists :append
		       :if-does-not-exist :create)
      (output-mrs1 mrs-instance device stream))
    (output-mrs1 mrs-instance device t)))

(let ((lock #+:allegro (mp:make-process-lock) #-:allegro nil))
  #-:allegro
  (declare (ignore lock))
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
                    (find-var-name (psoa-top-h psoa) connected-p)
		    (and (psoa-top-h psoa)(var-extra (psoa-top-h psoa)))
		    (and (psoa-top-h psoa)(var-base-type (psoa-top-h psoa)))
		    (and (psoa-top-h psoa)(var-id (psoa-top-h psoa))))
  (print-mrs-extra (psoa-top-h psoa))
  (mrs-output-index *mrs-display-structure* 
                    (find-var-name (psoa-index psoa) connected-p)
                    (and (psoa-index psoa) (var-extra (psoa-index psoa)))
		    (and (psoa-index psoa) (var-base-type (psoa-index psoa)))
		    (and (psoa-index psoa) (var-id (psoa-index psoa))))
  (print-mrs-extra (psoa-index psoa))
  (mrs-output-start-liszt *mrs-display-structure*)
  (let ((first-rel t))
    (loop for rel in (psoa-liszt psoa)
        do 
          (print-rel rel first-rel connected-p *mrs-display-structure*)
	  (setf first-rel nil)))
  (mrs-output-end-liszt *mrs-display-structure*)
  (when *rel-handel-path*
    (print-mrs-hcons (psoa-h-cons psoa) connected-p *mrs-display-structure*))
  (when (psoa-a-cons psoa)
    (print-mrs-acons (psoa-a-cons psoa) connected-p *mrs-display-structure*))
  (when (psoa-vcs psoa)
    (print-mrs-vcs (psoa-vcs psoa) *mrs-display-structure*))
  (mrs-output-end-psoa *mrs-display-structure*))

(defun print-rel (rel first-rel connected-p display)
  (mrs-output-start-rel 
   display
   (rel-pred rel) first-rel
   (determine-ep-class rel)
   (if (char-rel-p rel) (char-rel-cfrom rel))
   (if (char-rel-p rel) (char-rel-cto rel))
   (rel-str rel))
  (mrs-output-rel-handel
   display
   (find-var-name (rel-handel rel) connected-p)
   (var-extra (rel-handel rel))
   (var-base-type (rel-handel rel))
   (var-id (rel-handel rel)))
  (mrs-output-rel-link display (rel-link rel))
  (print-mrs-extra (rel-handel rel))
  (loop
      for feat-val in (rel-flist rel)
      for role = (fvpair-feature feat-val)
      unless (member role *output-ignored-roles* :test #'eq)
      do
	(mrs-output-label-fn display role)
	(let ((value (fvpair-value feat-val)))
	  (if (var-p value)
	      (progn
		(mrs-output-var-fn 
		 display
		 (find-var-name value connected-p)
		 (var-extra value)
		 (var-base-type value)
		 (var-id value))
		(print-mrs-extra value))
	    (mrs-output-atomic-fn display value)))
	(mrs-output-end-fvpair-fn display))
  (mrs-output-end-rel display))

;;; FIX
;;; following version of print-rel is called from the SEM-I 
;;; but unfortunate use of key and not exactly the same as the `real' code
;;; so renamed - should be changed to be same as print-rel above
;;; or moved to semi.lsp

(defun print-semi-rel (rel &key (display-to *mrs-display-structure*) first-rel connected-p)
  (unless (rel-base-p rel)
    (error "unexpected type"))
  ;; print pred name
  (mrs-output-start-rel
   display-to
   (rel-pred rel) first-rel
   (determine-ep-class rel))
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
             first-hcons
	     (var-id (hcons-scarg hcons))
	     (var-type (hcons-scarg hcons))
	     (var-id (hcons-outscpd hcons))
	     (var-type (hcons-outscpd hcons)))
            (setf first-hcons nil)))
    ;; extra info can be ignored here because all handels
    ;; will have appeared elsewhere
    (mrs-output-end-h-cons display))

(defun print-mrs-acons (acons-list connected-p display)
  (mrs-output-start-a-cons display)
  (let ((first-acons t))
    (loop for acons in acons-list
	do
	  (mrs-output-disj-cons-spec display first-acons)
	  (print-mrs-ilp
	   (disj-cons-index-lbl acons)
	   connected-p :spec display)
	  (mrs-output-disj-cons-start-target display)
	  (let ((first-el :first-target))
	    (dolist (target (disj-cons-target acons))
	      (print-mrs-ilp target connected-p first-el display)
	      (setf first-el :target)))
	  (mrs-output-disj-cons-end-target display)
	  (setf first-acons nil))
  (mrs-output-end-a-cons display)))

(defun print-mrs-ilp (ilp connected-p first-p display)
  (mrs-output-ilp 
   display
   (find-var-name 
    (index-lbl-index ilp) connected-p) 
   (find-var-name 
    (index-lbl-lbl ilp) connected-p)
   first-p))
   
(defun print-mrs-vcs (vcs display)
  (mrs-output-vcs display vcs))
  

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
           (vcs (ignore-errors (read-mrs-vcs istream)))
           (psoa
            (make-psoa :top-h ltop
                       :index index
                       :liszt liszt
                       :h-cons hcons
                       :vcs vcs)))
      (mrs-check-for #\] istream)
      #-:logon (unfill-mrs psoa) #+:logon psoa)))

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
    (nreverse rels)))

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

(defun read-mrs-vcs (stream)
  (mrs-check-for #\V stream)
  (mrs-check-for #\C stream)
  (mrs-check-for #\S stream)
  (mrs-check-for #\: stream)
  (mrs-check-for #\< stream)
  (loop
      for c = (peek-char t stream nil nil)
      when (null c) do (error "Unexpected eof")
      when (char= c #\>) do (read-char stream) and return vcs
      collect (read-mrs-atom stream) into vcs))

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
          featpairs link)
      (loop 
        (let ((next-char (peek-char t istream nil 'eof)))
          (when (eql next-char 'eof) (error "Unexpected eof"))
          (when (eql next-char #\]) 
            (read-char istream)
            (return))
          (let ((fvp (read-mrs-featpair istream)))
            ;;
            ;; _fix_me_
            ;; this is hacky, presumably these should not have been in the EP
            ;; in the first place.                              (1-jul-04; oe)
            ;;
            (if (eq (fvpair-feature fvp) *rel-link-feature*)
              (setf link (fvpair-value fvp))
              (unless (member (fvpair-feature fvp) *ignored-sem-features*)
                (push fvp featpairs))))))
      (make-rel :pred relpred :handel hvar :link link
                :flist (sort featpairs #'feat-sort-func)))))
          
(defun read-mrs-featpair (istream)         
  ;; FEATPAIR -> FEATNAME: VAR | CFEATNAME: CONSTNAME
  (let ((feature (read-mrs-atom istream)))
    (mrs-check-for #\: istream)
    (let ((val (cond
                ((eq feature *rel-link-feature*)
                 (if (eql (peek-char t istream nil nil) #\<)
                   (when (read-char istream nil nil)
                     (read-delimited-list #\> istream t))
                   ;;
                   ;; _fix_me_
                   ;; for some transition phase, support old-syle XLE syntax,
                   ;; e.g. `LNK: |12|'; this should go one day. (12-aug-04; oe)
                   ;;
                   (let* ((foo (read-mrs-atom istream))
                          (link (if (numberp foo)
                                  foo
                                  (parse-integer
                                   (string foo) :junk-allowed t))))
                     (when link (list link)))))
                ((member feature *value-feats* :test #'eq)
                 (read-mrs-atom istream))
                (t (read-mrs-var istream)))))
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
              (let ((foo (read-mrs-extrapair istream)))
                (when foo (push foo extra)))))
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
      (unless (member pathname *ignored-extra-features* :test #'eq)
        (make-extrapair :feature pathname :value val)))))
    

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

;;; reading in XML MRS
;;; see mrs.dtd


(defun read-mrs-xml-file (file-name)
  ;;; <!ELEMENT mrs-list (mrs)*>
  (let ((*package* (find-package :mrs)))
    (with-open-file (istream file-name :direction :input)
      (let ((mrss (parse-xml-removing-junk istream)))
	(unless (equal (car mrss) '|mrs-list|)
	  (error "~A is not a valid mrs file" file-name))
	(loop for mrs in (cdr mrss)
	    unless (xml-whitespace-string-p mrs)
	    collect
	      (read-mrs-xml mrs))))))

(defun read-single-mrs-xml-file (file-name)
  (let ((*package* (find-package :mrs)))
    (with-open-file (istream file-name :direction :input)
      (let ((mrs (parse-xml-removing-junk istream)))
	(xml-whitespace-string-p mrs)
	(read-mrs-xml mrs)))))

#|
to test
(output-mrs (read-single-mrs-xml-file "~aac10/foo5") 'simple)
|#

(defun read-single-mrs-xml-from-string (str)
  ;;; currently called from emacs interface - takes
  ;;; a string containing an mrs (we hope) and
  ;;; reads it in
  (let ((*package* (find-package :mrs)))
    (with-input-from-string (istream str)
      (let ((mrs (parse-xml-removing-junk istream)))
	(unless (xml-whitespace-string-p mrs)
	  (read-mrs-xml mrs))))))
  

(defun read-mrs-xml (content)
;;; <!ELEMENT mrs (label, var, (ep|hcons)*)>
  (setf *already-read-vars* nil)
  (let ((top-h nil) (index nil) (eps nil) 
        (h-cons nil)
	(tag (car content)))
    (unless (eql tag '|mrs|)
      (error "~A is not an mrs" content))
    (setf content (cdr content))
    (loop (let ((next-el (car content)))
	    (if (xml-whitespace-string-p next-el)
		(pop content)
	      (return))))
    (when content
      (setf top-h (read-mrs-xml-label (first content)))
      (setf index (read-mrs-xml-var (second content)))
      (loop for next-el in (cddr content)
	  do
	    (unless (xml-whitespace-string-p next-el)
	      (let
		  ((next-tag (car next-el)))
		(cond 
		 ((atom next-tag)
		  (cond ((eql next-tag '|ep|)
			 (push (read-mrs-xml-ep next-el)
			       eps))
			(t (error "Unexpected element ~A" next-el))))
		 ((eql (car next-tag) '|ep|)
		  (push (read-mrs-xml-ep next-el)
			eps))
		 ((eql (car next-tag) '|hcons|)
		  (push (read-mrs-xml-hcons next-el)
			h-cons))
		 (t (error "Unexpected element ~A" next-el))))))
      (make-psoa :top-h top-h
		 :index index
		 :liszt (nreverse eps)
		 :h-cons (nreverse h-cons)))))


(defun read-mrs-xml-ep (content)
;;; <!ELEMENT ep ((pred|realpred), label, fvpair*)>
;;; <!ATTLIST ep
;;;          cfrom CDATA #IMPLIED
;;;          cto   CDATA #IMPLIED 
;;;          surface   CDATA #IMPLIED
;;;	     base      CDATA #IMPLIED >
  (let ((tag (car content))
        (body (cdr content)))
    (unless (or (eql tag '|ep|)
		(and 
		 (listp tag)
		 (eql (first tag) '|ep|)))
      (error "Malformed ep ~A" content))
    (setf body (loop for x in body
		   unless (xml-whitespace-string-p x)
		   collect x))
    (let* ((pred (read-mrs-xml-pred (first body)))
	   (label (read-mrs-xml-label (second body)))
	   (flist (read-mrs-xml-rargs (cddr body)))
	   (cfrom (robust-c-extract (extract-from-xml-tag tag '|cfrom|)))
	   (cto (robust-c-extract (extract-from-xml-tag tag '|cto|)))
	   (str (extract-from-xml-tag tag '|str|)))
      ;;; base is allowed but ignored
      (make-char-rel 
       :pred pred
       :handel label
       :flist flist
       :cfrom cfrom
       :cto cto
       :str str))))

(defun robust-c-extract (res)
  (or 
   (if (stringp res)
       (parse-integer res :junk-allowed t))
   -1))

(defun extract-from-xml-tag (tag attribute)
  (if (listp tag)
      (second (member attribute tag))))

(defun read-mrs-xml-pred (content)
  (let ((tag (car content))
        (body (cdr content)))
    (cond 
     ((eql tag '|pred|) (make-mrs-atom (car body)))
         ;;; <!ELEMENT pred (#PCDATA)>
     ((eql tag '|spred|) (car body))
     (t (error "Unexpected element ~A" content)))))

(defun read-mrs-xml-var (content)
  (read-mrs-xml-var-or-label content nil))

(defun read-mrs-xml-label (content)
  (read-mrs-xml-var-or-label content t))


(defun read-mrs-xml-var-or-label (content label-p)
;;; <!ELEMENT var (extrapair*)>
;;; <!ATTLIST var
;;;          vid  CDATA #REQUIRED 
;;;          sort (x|e|h|u|l) #IMPLIED >
  (let ((tag (car content))
        (body (cdr content)))
    (if label-p
	(unless (and (eql (first tag) '|label|)
		     (eql (second tag) '|vid|))
	  (error "Malformed label ~A" content))
      (unless (and (eql (first tag) '|var|)
		   (eql (second tag) '|vid|))
	(error "Malformed variable ~A" content)))
    (let* ((extras (read-mrs-xml-extras body))
	   (id (parse-integer (third tag)))
	   (sort (or (fifth tag) (if label-p "h")))
	   (existing (assoc id *already-read-vars*))
	   (var (if (cdr existing)
		    (progn
		      (when (and extras (not (var-extra (cdr existing))))
			(setf (var-extra (cdr existing)) extras))
		      (when (and sort (not (var-type (cdr existing))))
			(setf (var-type (cdr existing)) sort))
		      (cdr existing))
		    (make-var :id id :extra extras :type sort))))
      (unless existing 
	(push (cons id var) *already-read-vars*))
     var)))

(defun read-mrs-xml-extras (extras)
  (loop for content in extras
      unless (xml-whitespace-string-p content)
      collect
;;; <!ELEMENT extrapair (path,value)>
;;; <!ELEMENT path (#PCDATA)>
;;; <!ELEMENT value (#PCDATA)>
	(let ((path nil) (value nil))
	  (setf path (make-mrs-atom 
		      (read-rmrs-simple '|path| (second content))))
	  (setf value (make-mrs-atom
		       (read-rmrs-simple '|value| (third content))))
	  (make-extrapair :feature path :value value)))) 

(defun make-mrs-atom (str)
  (let ((*package* (find-package *mrs-package*)))
      (intern str *mrs-package*)))

(defun read-mrs-xml-constant (content)
;;;  <!ELEMENT constant (#PCDATA)>
  (read-rmrs-simple '|constant| content))
  
(defun read-mrs-xml-rargs (rargs)
  (loop for content in rargs
        unless (xml-whitespace-string-p content)
	collect
  ;;; <!ELEMENT fvpair (rargname, (var|constant))>
	(let ((name nil) (arg nil))
	  (setf name (make-mrs-atom
		      (read-rmrs-simple 
		       '|rargname| (second content))))
    ;;; <!ELEMENT rargname (#PCDATA)>
	  (let* ((argval (third content))
		 (argvaltag (car argval)))
	    (setf arg
	      (if (eql argvaltag '|constant|)
		  (read-mrs-xml-constant argval)
		(read-mrs-xml-var argval))))
	  (make-fvpair :feature name :value arg))))
  
(defun read-mrs-xml-hcons (content)
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
                :scarg (read-mrs-xml-var (second (first body)))
                :outscpd (read-mrs-xml-var (second (second body))))))
                
;;; end MRS XML

;;;
;;; interim solution for MRS `unfilling' until we construct a proper SEMI; note
;;; that unfill-mrs() _destructively_ modifies its input.
;;;
(defparameter %mrs-extras-filter% nil)

(defparameter %mrs-roles-filter% nil)

(defun unfill-mrs (mrs &optional (filter %mrs-extras-filter%)
                                 (roles %mrs-roles-filter%))
  (when (or filter roles)
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
            (setf (rel-flist ep)
              (loop
                  for role in (rel-flist ep)
                  for value = (fvpair-value role)
                  do (unfill-variable value)
                  unless (member (fvpair-feature role) roles)
                  collect role)))))
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

(defparameter %mrs-extras-defaults% nil)

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


(defun sort-mrs (mrs)
  (when (psoa-p mrs)
    (let ((copy (copy-psoa mrs)))
      (setf (mrs:psoa-liszt copy)
        (stable-sort 
         (copy-list (mrs:psoa-liszt mrs))
         #'string-lessp :key #'mrs:rel-pred))
      copy)))

;;; sement output: uses MRS indexed routines, plus a few extras,
;;; as here

(defmethod mrs-output-hook-fn ((mrsout indexed) ltop index xarg)
  (with-slots (stream) mrsout
    (format stream "[~(~a~),~(~a~)" ltop index)
    (when xarg
	(format stream ",~(~a~)" xarg))
    (format stream "]")))

(defmethod mrs-output-start-slots ((mrsout indexed))
  (with-slots (stream) mrsout
    (format stream ",~%{")))

(defmethod mrs-output-start-slot ((mrsout indexed) first-p)
  (with-slots (stream) mrsout
    (unless first-p
      (format stream ","))))

(defmethod mrs-output-slot-name ((mrsout indexed) name)
  (with-slots (stream) mrsout
    (format stream "~a" name)))

(defclass active-slot (simple-indexed)
  ())

(defmethod mrs-output-slot-name ((mrsout active-slot) name)
  (with-slots (stream) mrsout
    (lkb::slot-region stream name)))

(defmethod mrs-output-end-slots ((mrsout indexed))
  (with-slots (stream) mrsout
    (format stream "}")))


;;; output code for sements

(defun output-algebra-sement (sement device &optional file-name)
  (if file-name
      (with-open-file (stream file-name :direction :output)
	(output-algebra-sement1 sement device stream))
    (output-algebra-sement1 sement device t)))

(let ((lock #+:allegro (mp:make-process-lock) #-:allegro nil))
  #-:allegro
  (declare (ignore lock))
  (defun output-algebra-sement1 (sement device stream)
    (#+:allegro mp:with-process-lock #+:allegro (lock) #-:allegro progn
      (def-print-operations device 0 stream)
      (cond ((sement-p sement)
             (print-sement sement)
             (mrs-output-max-width-fn *mrs-display-structure*))
            (t (mrs-output-error-fn *mrs-display-structure* sement))))))

(defun print-sement (sement)
  (setf *already-seen-vars* nil)
  (print-hook (sement-hook sement) *mrs-display-structure*) 
  (print-slots (sement-slots sement) *mrs-display-structure*) 
  (mrs-output-start-liszt *mrs-display-structure*)
  (let ((first-rel t))
    (loop for rel in (psoa-liszt sement)
        do 
          (print-rel rel first-rel nil *mrs-display-structure*)
	  (setf first-rel nil)))
  (mrs-output-end-liszt *mrs-display-structure*)
  (print-mrs-hcons (psoa-h-cons sement) nil *mrs-display-structure*)
  (when (psoa-a-cons sement)
    (print-mrs-acons (psoa-a-cons sement) nil *mrs-display-structure*)))
  

(defun print-hook (hook display)
  (let ((index (hook-index hook))
	(ltop (hook-ltop hook))
	(xarg (hook-xarg hook)))
    (mrs-output-hook-fn 
     display
     (find-var-name ltop nil)
     (find-var-name index nil)
     (if xarg
	 (find-var-name xarg nil)))))

(defun print-slots (slots display)
    (mrs-output-start-slots display)
    (let ((first-slot t))
      (loop for slot in slots
          do
	    (mrs-output-start-slot display first-slot)
	    (print-hook  (slot-hook slot) display)
	    (mrs-output-slot-name display (slot-name slot)) 
            (setf first-slot nil)))
    (mrs-output-end-slots display))


;;; ****************************************************************
;;;
;;; Copying an MRS completely
;;;
;;; ****************************************************************

(defun copy-psoa-completely (psoa)
  ;;; this does not fully copy `extra', parameter strings etc
  ;;; since we assume that those will not be modified
  (unless (psoa-p psoa)
    (error "~%~A is not a psoa structure"))
  (let ((mostly-new (copy-psoa-liszt-completely psoa nil)))
    (setf (psoa-index mostly-new)
      (copy-var (psoa-index mostly-new)))
    (setf (psoa-top-h mostly-new)
      (copy-var (psoa-top-h mostly-new)))
    (setf (psoa-h-cons mostly-new)
      (copy-psoa-hcons (psoa-h-cons mostly-new)))
    mostly-new))

(defun copy-psoa-hcons (hcons)
  (loop for hcons-el in hcons
      collect
	(copy-hcons-element hcons-el)))

(defun copy-psoa-liszt-completely (psoa no-var-copy-p)
  ;;; Note - this takes a full mrs and just copies the liszt
  ;;; existing fns in mrsresolve don't want copied qeqs
  ;;; or copied variables
  (let ((copy-mrsstruct (copy-psoa psoa)))
    (setf (psoa-liszt copy-mrsstruct)
      (copy-liszt-completely (psoa-liszt copy-mrsstruct) 
			     no-var-copy-p))
    copy-mrsstruct))

(defun copy-liszt-completely (liszt no-var-copy-p)
  (loop for rel in liszt
      collect
	(let* ((new-rel (copy-rel rel))
	       (new-flist (loop for fvp in (rel-flist new-rel)
			      collect
				(if no-var-copy-p
				    (copy-fvpair fvp)
				  (copy-fvpair-completely fvp)))))
	  (unless no-var-copy-p
	    (setf (rel-handel new-rel)
	      (copy-var (rel-handel new-rel))))
	  (setf (rel-flist new-rel)
	    new-flist)
	  new-rel)))

(defun copy-hcons-element (hcons-el)
  (let ((copied-hcons
	 (copy-hcons hcons-el)))
    (setf (hcons-scarg copied-hcons)
      (copy-var (hcons-scarg copied-hcons)))
    (setf (hcons-outscpd copied-hcons)
      (copy-var (hcons-outscpd copied-hcons)))
    copied-hcons))

(defun copy-fvpair-completely (fvp)
  (let ((copy-fvp (copy-fvpair fvp)))
    (when (var-p (fvpair-value copy-fvp))
      (setf (fvpair-value copy-fvp)
	(copy-var (fvpair-value copy-fvp))))
    copy-fvp))
      
(defun copy-sement-hook (hook)
  (make-hook
  :index (copy-var (hook-index hook))
  :ltop (copy-var (hook-ltop hook))
  :xarg (if (hook-xarg hook) (copy-var (hook-xarg hook)))))

(defun copy-sement-slots (slots)
  (loop for slot in slots
      collect
	(make-slot :hook (copy-sement-hook (slot-hook slot))
		   :name (slot-name slot))))

;;; ****************************************************************
;;;
;;; Replacing variables in an MRS destructively
;;;
;;; ****************************************************************

;;; cf rmrs/comp.lisp 
;;; destructive

(defun canonicalise-sement-hook (hook bindings)
  (canonicalise-basemrs-variable (hook-index hook) bindings)
  (when (hook-xarg hook)			   
    (canonicalise-basemrs-variable (hook-xarg hook) bindings))
  (canonicalise-basemrs-variable (hook-ltop hook) bindings)
  hook)

(defun canonicalise-sement-slots (slots bindings)
  (dolist (slot slots)
    (canonicalise-sement-hook (slot-hook slot) bindings))
  slots)


(defun canonicalise-basemrs (mrs bindings)
  ;;; destructive
  (canonicalise-basemrs-variable (psoa-index mrs)
				 bindings)
  (canonicalise-basemrs-variable (psoa-top-h mrs)
				 bindings)
  (canonicalise-basemrs-liszt (basemrs-liszt mrs)
			      bindings)
  (canonicalise-basemrs-hcons-list (basemrs-h-cons mrs)
				   bindings)
  mrs)

  
;;; individial functions are 
;;; called from: reset-mrs-for-disj-cons
;;; make-sement-from-sements

(defun canonicalise-basemrs-liszt (liszt bindings)
  (dolist (ep liszt)
    (canonicalise-basemrs-variable (rel-handel ep) bindings)
    (dolist (fvp (rel-flist ep))
      (let ((value (fvpair-value fvp)))
	(when (var-p value) 
	  (canonicalise-basemrs-variable value bindings)))))
  liszt)


(defun canonicalise-basemrs-variable (var bindings)
  (let* ((var-id (var-id var))
	 (replace-value (cdr (assoc var-id bindings))))
    (when replace-value
	(setf (var-id var) replace-value))))

(defun canonicalise-basemrs-hcons-list (hcons-list bindings)
  (dolist (hcons hcons-list)
    (canonicalise-basemrs-variable
     (hcons-scarg hcons) bindings)
    (canonicalise-basemrs-variable
     (hcons-outscpd hcons) bindings))
  hcons-list)



