(in-package :mrs)

(defun def-rmrs-print-operations (class indentation stream)
  (make-instance class 
    :indentation indentation
    :stream stream))

(defclass rmrs-output-type ()
  ((indentation :initform 0 :initarg :indentation)
   (stream :initarg :stream)))

(defmethod rmrs-output-error-fn ((rmrsout rmrs-output-type) rmrs-instance)
  (with-slots (stream) rmrsout
    (format stream "~%::: ~A is not an rmrs structure~%" rmrs-instance)))

;;; The XML situation is complicated because we've actually got to support
;;; three dtds
;;;
;;; rmrs.dtd is the main one for real rmrs's
;;;
;;; gram.dtd is for grammar rules and tag.dtd for the `lexicon'
;;; for RASP->RMRS.  These have their own functions for the `outer' 
;;; layers - for the inner parts, the differences are
;;; a) variables don't have separate id and type so are elements
;;; with no attributes
;;; b) notion of a semstruct and a hook - not found in `main' dtd

;;; 
;;; xml rmrs-output-type class for rmrs.dtd
;;;

(defclass xml (rmrs-output-type) ())

;;; <!ELEMENT rmrs (label, (ep|rarg|ing|hcons)*)>
;;; <!ATTLIST rmrs
;;;          cfrom CDATA #REQUIRED
;;;          cto   CDATA #REQUIRED >

(defmethod rmrs-output-start-fn ((rmrsout xml) cfrom cto)
  (with-slots (stream) rmrsout
    (format stream "~%<rmrs cfrom='~A' cto='~A'>~%" (or cfrom -1)
	    (or cto -1))))

(defmethod rmrs-output-end-fn ((rmrsout xml))
  (with-slots (stream) rmrsout
    (format stream "~%</rmrs>~%")))

#|
<!ELEMENT ep ((realpred|gpred), label, var)>
<!ATTLIST ep
          cfrom CDATA #REQUIRED
          cto   CDATA #REQUIRED >
|#

(defmethod rmrs-output-start-ep ((rmrsout xml) cfrom cto)
  (with-slots (stream) rmrsout
    (format stream "~%<ep cfrom='~A' cto='~A'>" (or cfrom -1)
	    (or cto -1))))

#|
<!ELEMENT realpred EMPTY>

<!ATTLIST realpred
          lemma CDATA #REQUIRED
          pos (V|N|J|R|P) #IMPLIED
          sense CDATA #IMPLIED >

|#

(defmethod rmrs-output-realpred ((rmrsout xml) lemma pos sense)
  (with-slots (stream) rmrsout
    (format stream "<realpred lemma='~A'" lemma)
    (when pos (format stream " pos='~A'" pos))
    (when sense (format stream " sense='~A'" sense))      
    (format stream "/>")))

;;; <!ELEMENT gpred (#PCDATA)>

(defmethod rmrs-output-gpred ((rmrsout xml) pred)
  (with-slots (stream) rmrsout
    (if (dummy-pred-p pred)
        (format stream "<pred/>")
      (format stream "<gpred>~(~a~)</gpred>" pred))))

#|
<!ELEMENT var EMPTY>
<!ATTLIST var
          sort (x|e|h|u|l) #REQUIRED
          vid  CDATA #REQUIRED 
          num  CDATA #IMPLIED
          pers CDATA #IMPLIED
          gender CDATA #IMPLIED
          tense CDATA #IMPLIED
          aspect CDATA #IMPLIED >
|#
          
(defmethod rmrs-output-var-fn ((rmrsout xml) var-id var-type)
  (with-slots (stream) rmrsout
    (format stream "<var sort='~A' vid='~A'" var-type var-id)))

(defmethod rmrs-output-end-var ((rmrsout xml))
  (with-slots (stream) rmrsout
    (format stream "/>")))

(defmethod rmrs-output-start-extra ((rmrsout xml))
   nil)

(defmethod rmrs-output-extra-feat-val  ((rmrsout xml) feat val)
  (with-slots (stream) rmrsout
    (format stream " ~A='~A'" feat val)))

(defmethod rmrs-output-constant-fn ((rmrsout xml) constant)
  (with-slots (stream) rmrsout
    (format stream "<constant>~A</constant>" constant)))

(defmethod rmrs-output-end-ep ((rmrsout xml))
  (with-slots (stream) rmrsout
    (format stream "</ep>")))

#|
<!ELEMENT label EMPTY>

<!ATTLIST label 
          vid CDATA #REQUIRED >
          |#

(defmethod rmrs-output-label ((rmrsout xml) label-id)
  (with-slots (stream) rmrsout
    (format stream "<label vid='~A'/>" label-id)))

(defmethod rmrs-output-top-label ((rmrsout xml) label-id)
  (with-slots (stream) rmrsout
    (format stream "<label vid='~A'/>" label-id)))

;;; Parsonian arguments

;;; <!ELEMENT rarg (rargname, label, var)>

(defmethod rmrs-output-start-rmrs-arg ((rmrsout xml) predname with-ep-p)
  (declare (ignore with-ep-p))
  (with-slots (stream) rmrsout
    (format stream "~%<rarg><rargname>~A</rargname>" predname)))

(defmethod rmrs-output-end-rmrs-arg ((rmrsout xml))
  (with-slots (stream) rmrsout
    (format stream "</rarg>")))

;;; hcons

#|
<!ELEMENT hcons (hi, lo)>
<!ATTLIST hcons 
          hreln (qeq|lheq|outscopes) "qeq" >

<!ELEMENT hi (var)>
<!ELEMENT lo (label|var)>
|#

(defmethod rmrs-output-hcons-start ((rmrsout xml) reln)
  (with-slots (stream) rmrsout
    (format stream "~%<hcons hreln='~A'><hi>"
            (string-downcase reln))))

(defmethod rmrs-output-hcons-next ((rmrsout xml))
  (with-slots (stream) rmrsout
    (format stream "</hi><lo>")))

(defmethod rmrs-output-hcons-end ((rmrsout xml))
  (with-slots (stream) rmrsout
    (format stream "</lo></hcons>")))

;;; in-group

#|
<!ELEMENT ing (ing-a, ing-b)>
<!ELEMENT ing-a (var)>
<!ELEMENT ing-b (var)>
|#

(defmethod rmrs-output-ingroup-start ((rmrsout xml))
  (with-slots (stream) rmrsout
    (format stream "~%<ing><ing-a>")))

(defmethod rmrs-output-ingroup-next ((rmrsout xml))
  (with-slots (stream) rmrsout
    (format stream "</ing-a><ing-b>")))

(defmethod rmrs-output-end-ingroup ((rmrsout xml))
  (with-slots (stream) rmrsout
    (format stream "</ing-b></ing>")))

;;; methods for semstructs actually relevant for
;;; gram.dtd and tag.dtd only

#|
<!ELEMENT semstruct (hook,(ep|rarg|ing)*)>
<!ELEMENT hook (index,label)>
<!ELEMENT index (#PCDATA)>
<!ELEMENT label (#PCDATA)>
|#

(defmethod semstruct-output-start-hook ((rmrsout xml))
  (with-slots (stream) rmrsout
    (format stream "~%<hook>")))

(defmethod semstruct-output-hook-label ((rmrsout xml) label)
  (with-slots (stream) rmrsout
    (format stream "<label>~A</label>"
            label)))

(defmethod semstruct-output-hook-index ((rmrsout xml) label)
  (with-slots (stream) rmrsout
    (format stream "<index>~A</index>"
            label)))

(defmethod semstruct-output-end-hook ((rmrsout xml))
  (with-slots (stream) rmrsout
    (format stream "</hook>")))

;;; variants for gram and tag dtds

(defclass gramxml (xml) ())

;;; <!ELEMENT ep (gpred,label,var)>

(defmethod rmrs-output-start-ep ((rmrsout gramxml) cfrom cto)
  (declare (ignore cfrom cto))
  (with-slots (stream) rmrsout
    (format stream "~%<ep>")))

#|
for gram.dtd and tag.dtd
<!ELEMENT var (#PCDATA)>
|#

(defmethod rmrs-output-var-fn ((rmrsout gramxml) var-id var-type)
  (declare (ignore var-type))
  (with-slots (stream) rmrsout
    (format stream "<var>~A</var>" var-id)))

#|
<!ELEMENT label (#PCDATA)>
|#

(defmethod rmrs-output-label ((rmrsout gramxml) label-id)
  (with-slots (stream) rmrsout
    (format stream "<label>~A</label>" label-id)))

;;; compact representation for tracing etc

;;; 
;;; compact rmrs-output-type class
;;;

(defclass compact (rmrs-output-type) ())

(defmethod rmrs-output-start-fn ((rmrsout compact) cfrom cto)
  (declare (ignore cfrom cto))
  (with-slots (stream) rmrsout
    (format stream "~%")))

(defmethod rmrs-output-end-fn ((rmrsout compact))
  (with-slots (stream) rmrsout
    (format stream "")))

(defmethod rmrs-output-start-ep ((rmrsout compact) cfrom cto)
  (declare (ignore cfrom cto))
  (with-slots (stream) rmrsout
    (format stream "")))

(defmethod rmrs-output-realpred ((rmrsout compact) lemma pos sense)
  (with-slots (stream indentation) rmrsout
    (format stream "~VT_~(~a_~a~@[~a~]~)(" 
	    indentation lemma (or pos "U") sense)))

(defmethod rmrs-output-gpred ((rmrsout compact) predname)
  (with-slots (stream indentation) rmrsout
    (format stream "~VT~(~a~)(" indentation predname)))

(defmethod rmrs-output-var-fn ((rmrsout compact) var-id var-type)
  (with-slots (stream) rmrsout
    (format stream "~A~A" var-type var-id)))


(defmethod rmrs-output-end-var ((rmrsout compact))
  (with-slots (stream) rmrsout
      (lkb::current-position stream)))
;;; may need fixing for generality if current-position isn't
;;; safe - also needs fixing to remove lkb specificity


(defmethod rmrs-output-start-extra ((rmrsout compact))
  (with-slots (stream) rmrsout
    (format stream ":")))

(defmethod rmrs-output-extra-feat-val  ((rmrsout compact) feat val)
  (declare (ignore feat))
  (with-slots (stream) rmrsout
    (format stream "~A:" val)))


(defmethod rmrs-output-constant-fn ((rmrsout compact) constant)
  (with-slots (stream) rmrsout
    (format stream "~A" constant)))

(defmethod rmrs-output-end-ep ((rmrsout compact))
  (with-slots (stream) rmrsout
    (format stream ")~%")))

(defmethod rmrs-output-label ((rmrsout compact) label-id)
  (with-slots (stream) rmrsout
    (format stream "h~A," label-id)))

(defmethod rmrs-output-top-label ((rmrsout compact) label-id)
  (with-slots (stream indentation) rmrsout
    (format stream "~VTh~A~%" indentation label-id)))

;;; Parsonian arguments

(defmethod rmrs-output-start-rmrs-arg ((rmrsout compact) predname with-ep-p)
  (with-slots (stream indentation) rmrsout
    (if with-ep-p
	(format stream "~VT          ~A(" indentation predname)
      (format stream "~VT~A(" indentation predname))))

(defmethod rmrs-output-end-rmrs-arg ((rmrsout compact))
  (with-slots (stream) rmrsout
    (format stream ")~%")))

;;; hcons

(defmethod rmrs-output-hcons-start ((rmrsout compact) reln)
  (with-slots (stream indentation) rmrsout
    (format stream "~VT~A(" indentation
            reln)))

(defmethod rmrs-output-hcons-next ((rmrsout compact))
  (with-slots (stream) rmrsout
    (format stream ",")))

(defmethod rmrs-output-hcons-end ((rmrsout compact))
  (with-slots (stream) rmrsout
    (format stream ")~%")))

;;; in-group

(defmethod rmrs-output-ingroup-start ((rmrsout compact))
  (with-slots (stream indentation) rmrsout
    (format stream "~VTING(" indentation)))

(defmethod rmrs-output-ingroup-next ((rmrsout compact))
  (with-slots (stream) rmrsout
    (format stream ",")))

(defmethod rmrs-output-end-ingroup ((rmrsout compact))
  (with-slots (stream) rmrsout
    (format stream ")~%")))

;;; methods for semstructs

(defmethod semstruct-output-start-hook ((rmrsout compact))
  (with-slots (stream) rmrsout
    (format stream "~%[")))

(defmethod semstruct-output-hook-label ((rmrsout compact) label)
  (with-slots (stream) rmrsout
    (format stream "h~A,"
            label)))

(defmethod semstruct-output-hook-index ((rmrsout compact) index)
  (with-slots (stream) rmrsout
    (format stream ",~A" index)))

(defmethod semstruct-output-end-hook ((rmrsout compact))
  (with-slots (stream) rmrsout
    (format stream "]~%")))

;;; more compact representation for tracing etc
;;; oe helpfully put ~%s in compact, so define a version
;;; without ...

(defclass vcompact (compact) ())

(defmethod rmrs-output-end-ep ((rmrsout vcompact))
  (with-slots (stream) rmrsout
    (format stream "), ")))

(defmethod rmrs-output-top-label ((rmrsout vcompact) label-id)
  (with-slots (stream) rmrsout
    (format stream "[h~A] " label-id)))

(defmethod rmrs-output-end-rmrs-arg ((rmrsout vcompact))
  (with-slots (stream) rmrsout
    (format stream "), ")))

(defmethod rmrs-output-hcons-end ((rmrsout vcompact))
  (with-slots (stream) rmrsout
    (format stream "), ")))

(defmethod rmrs-output-end-ingroup ((rmrsout vcompact))
  (with-slots (stream) rmrsout
    (format stream "), ")))

(defmethod semstruct-output-end-hook ((rmrsout vcompact))
  (with-slots (stream) rmrsout
    (format stream "], ")))

;;;

(defclass compact-chars (compact)
  ())

(defmethod rmrs-output-start-ep ((rmrsout compact-chars) cfrom cto)
  (with-slots (stream indentation) rmrsout
    (format stream "~VT~A->~A:" indentation cfrom cto)))

(defmethod rmrs-output-realpred ((rmrsout compact-chars) lemma pos sense)
  (with-slots (stream) rmrsout
    (format stream "_~(~a_~a~@[~a~]~)(" 
	    lemma (or pos "U") sense)))

(defmethod rmrs-output-gpred ((rmrsout compact-chars) predname)
  (with-slots (stream) rmrsout
    (format stream "~(~a~)(" predname)))


(defmethod rmrs-output-start-extra ((rmrsout compact-chars))
  nil)

(defmethod rmrs-output-extra-feat-val  ((rmrsout compact-chars) feat val)
  (declare (ignore feat val))
  nil)



(defclass compact-two (compact-chars)
  ((xpos :initform 0 :initarg :xpos)))

(defmethod rmrs-output-start-fn ((rmrsout compact-two) cfrom cto)
  (declare (ignore cfrom cto))
  (with-slots (stream indentation xpos) rmrsout
    (setf indentation (+ indentation 70))
    (format stream "~%~VT" indentation)
    (setf xpos (lkb::current-position-x stream))))

(defmethod rmrs-output-end-var ((rmrsout compact-two))
  (with-slots (stream xpos) rmrsout
    (lkb::make-position-record xpos
      (lkb::current-position-y stream))))
;;; may need fixing for generality if current-position isn't
;;; safe - also needs fixing to remove lkb specificity



;;; Actual printing function for robust mrs
;;; version for real mrs uses same (or similar) defmethods but
;;; is a variant of output-mrs

(defun output-rmrs (rmrs-instance device &optional file-name)
     (if file-name
      (with-open-file (stream file-name :direction :output)
         (output-rmrs1 rmrs-instance device stream))
      (output-rmrs1 rmrs-instance device t)))

(defun output-rmrs1 (rmrs-instance device stream 
		     &optional grouping-p)
  ;;; changed to return a list of eps and their positions
  ;;; for calculation of comparison arrows 
  ;;; not used otherwise
  (let ((rmrs-display-structure
	 (def-rmrs-print-operations device 0 stream)))
    (cond ((rmrs-p rmrs-instance)         
	   (rmrs-output-start-fn rmrs-display-structure 
				 (rmrs-cfrom rmrs-instance)
				 (rmrs-cto rmrs-instance))
	   (let ((positions
		  (print-rmrs rmrs-instance grouping-p 
			      rmrs-display-structure)))
	     (rmrs-output-end-fn rmrs-display-structure)
	     positions))
	  (t (rmrs-output-error-fn rmrs-display-structure 
				   rmrs-instance)))))

(defun internal-output-rmrs (rmrs-instance device stream)
  ;;; for rule output
  (let ((rmrs-display-structure
	 (def-rmrs-print-operations device 0 stream)))
    (cond ((rmrs-p rmrs-instance)         
	   (print-rmrs rmrs-instance nil rmrs-display-structure))
	  (t (rmrs-output-error-fn rmrs-display-structure 
				   rmrs-instance)))))

(defparameter *already-seen-rmrs-vars* nil)

(defparameter *already-seen-rmrs-args* nil)

(defun print-rmrs (rmrs grouping-p rmrs-display-structure)
  (setf *already-seen-rmrs-args* nil)
  (setf *already-seen-rmrs-vars* nil)
  (let ((hook (if (semstruct-p rmrs) (semstruct-hook rmrs))) 
        (top-h (rmrs-top-h rmrs))
        (eps (rmrs-liszt rmrs))
        (rmrs-args (rmrs-rmrs-args rmrs))
        (rmrs-h-cons (rmrs-h-cons rmrs))
        (rmrs-in-groups (rmrs-in-groups rmrs))
        (bindings (if (semstruct-p rmrs)
                      (close-bindings (rmrs-bindings rmrs))
                       (rmrs-bindings rmrs))))
    (when (and hook (not (indices-default hook)))
      (print-semstruct-hook hook 
                            bindings rmrs-display-structure))
    (unless hook
      (rmrs-output-top-label rmrs-display-structure
			     (if top-h
				 (find-rmrs-var-id top-h bindings)
			       (funcall
				*rmrs-variable-generator*))))
    (let ((positions
	   (print-rmrs-eps eps bindings grouping-p 
			   rmrs-args rmrs-display-structure)))
      (loop for arg in rmrs-args
	  unless (member arg *already-seen-rmrs-args*)
	  do
	    (print-rmrs-arg arg bindings nil rmrs-display-structure))
      (print-rmrs-in-groups rmrs-in-groups bindings rmrs-display-structure)
      (print-rmrs-hcons rmrs-h-cons bindings rmrs-display-structure)
      positions)))

(defun print-rmrs-eps (eps bindings grouping-p rmrs-args 
		       rmrs-display-structure)
    (loop for ep in eps
        collect
	  (let ((pred (rel-pred ep)))
	    (rmrs-output-start-ep rmrs-display-structure
				  (if (char-rel-p ep)
				      (char-rel-cfrom ep)
				    -1)
				  (if (char-rel-p ep)
				      (char-rel-cto ep)
				    -1))
            (if (realpred-p pred)
                (rmrs-output-realpred rmrs-display-structure
                                      (realpred-lemma pred)
                                      (realpred-pos pred)
                                      (realpred-sense pred))
              (rmrs-output-gpred rmrs-display-structure pred))
	    (let ((label (rel-handel ep)))
	      (rmrs-output-label rmrs-display-structure
				 (find-rmrs-var-id label bindings)))
	    (let* ((value (car (rel-flist ep)))
		   ;; checking should happen elsewhere
		   ;; got to be a variable, not a constant
		   ;; but could be a grammar variable
		   (position 
		    (print-rmrs-var value bindings rmrs-display-structure)))
	      (rmrs-output-end-ep rmrs-display-structure)
	      (when grouping-p
		(loop for arg in rmrs-args
		    when (eql-var-id (rmrs-arg-label arg) (rel-handel ep))
		    do		
		      (print-rmrs-arg arg bindings t rmrs-display-structure)
		      (push arg *already-seen-rmrs-args*)))
	      (if position
		  (record-rmrs-position position ep))))))

(defun print-rmrs-arg (arg bindings with-ep-p rmrs-display-structure)
  (rmrs-output-start-rmrs-arg rmrs-display-structure 
			      (rmrs-arg-arg-type arg) with-ep-p)
  (let ((label (rmrs-arg-label arg)))
    (rmrs-output-label rmrs-display-structure 
		       (find-rmrs-var-id
			label
			bindings)))
  (let ((value (rmrs-arg-val arg)))
    (if (var-p value)
	(print-rmrs-var value bindings rmrs-display-structure)
      (rmrs-output-constant-fn 
       rmrs-display-structure
       value)))
  (rmrs-output-end-rmrs-arg rmrs-display-structure))


(defun find-rmrs-var-id (var bindings)
  (let ((canon-var (if bindings 
                       (lookup-canonical-var var bindings)
                     var)))
    (var-id canon-var)))

(defun print-rmrs-var (value bindings display)
  (unless (var-p value) (error "Unexpected value ~A" value))
  (rmrs-output-var-fn 
   display
   (find-rmrs-var-id value bindings)
   (var-type value))
  (unless (member value *already-seen-rmrs-vars* :test #'eq)
    (push value *already-seen-rmrs-vars*)
    (rmrs-output-start-extra display)
    (loop for extrapair in (var-extra value)
	do
	  (rmrs-output-extra-feat-val 
	   display
	   (extrapair-feature extrapair)
	   (extrapair-value extrapair))))
  (rmrs-output-end-var display))
      

(defun print-rmrs-hcons (hcons-list bindings display)
    (loop for hcons in hcons-list
        do
          (rmrs-output-hcons-start
           display (hcons-relation hcons))
	  (print-rmrs-var 
            (hcons-scarg hcons) bindings display)
          (rmrs-output-hcons-next 
           display)
	  (rmrs-output-label 
	   display
	   (find-rmrs-var-id (hcons-outscpd hcons)
			     bindings))
          (rmrs-output-hcons-end display)))


(defun print-rmrs-in-groups (ingroup-list bindings display)
    (loop for in-g in ingroup-list
        do
          (rmrs-output-ingroup-start
           display)
          (print-rmrs-var 
           (car (in-group-labels in-g))
           bindings display)
          (rmrs-output-ingroup-next
           display)
          (print-rmrs-var 
           (cadr (in-group-labels in-g))
           bindings display)
          (rmrs-output-end-ingroup display)))

(defun print-semstruct-hook (hook bindings display)
  (semstruct-output-start-hook display)
  (semstruct-output-hook-label display
   (find-rmrs-var-id 
    (indices-label hook) bindings))
  (print-rmrs-var 
   (indices-index hook)
   bindings display)
  (semstruct-output-end-hook display))



