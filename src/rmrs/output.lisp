(in-package :mrs)

(defparameter *rmrs-display-structure* nil)

(defun def-rmrs-print-operations (class stream)
  (setf *rmrs-display-structure* (make-instance class 
					   :stream stream)))

(defclass rmrs-output-type ()
  ((stream :initarg :stream)))

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
    (format stream "~%<rmrs cfrom='~A' cto='~A'>~%" cfrom cto)))

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
    (format stream "~%<ep cfrom='~A' cto='~A'>" cfrom cto)))

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
      (format stream "<gpred>~A</gpred>" pred))))

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
    (format stream "<var sort='~A' vid='~A'/>" var-type var-id)))

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

(defmethod rmrs-output-start-rmrs-arg ((rmrsout xml) predname)
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
  (with-slots (stream) rmrsout
    (format stream "_~A~A~A(" lemma 
            (if pos (format nil "_~A" pos) "") 
            (if sense (format nil "_~A" sense) ""))))

(defmethod rmrs-output-gpred ((rmrsout compact) predname)
  (with-slots (stream) rmrsout
    (format stream "~A(" predname)))

(defmethod rmrs-output-var-fn ((rmrsout compact) var-id var-type)
  (with-slots (stream) rmrsout
    (format stream "~A~A" var-type var-id)))

(defmethod rmrs-output-constant-fn ((rmrsout compact) constant)
  (with-slots (stream) rmrsout
    (format stream "~A" constant)))

(defmethod rmrs-output-end-ep ((rmrsout compact))
  (with-slots (stream) rmrsout
    (format stream ")~%")))

(defmethod rmrs-output-label ((rmrsout compact) label-id)
  (with-slots (stream) rmrsout
    (format stream "l~A," label-id)))

(defmethod rmrs-output-top-label ((rmrsout compact) label-id)
  (with-slots (stream) rmrsout
    (format stream "l~A~%" label-id)))

;;; Parsonian arguments

(defmethod rmrs-output-start-rmrs-arg ((rmrsout compact) predname)
  (with-slots (stream) rmrsout
    (format stream "~A(" predname)))

(defmethod rmrs-output-end-rmrs-arg ((rmrsout compact))
  (with-slots (stream) rmrsout
    (format stream ")~%")))

;;; hcons

(defmethod rmrs-output-hcons-start ((rmrsout compact) reln)
  (with-slots (stream) rmrsout
    (format stream "~A("
            reln)))

(defmethod rmrs-output-hcons-next ((rmrsout compact))
  (with-slots (stream) rmrsout
    (format stream ",")))

(defmethod rmrs-output-hcons-end ((rmrsout compact))
  (with-slots (stream) rmrsout
    (format stream ")~%")))

;;; in-group

(defmethod rmrs-output-ingroup-start ((rmrsout compact))
  (with-slots (stream) rmrsout
    (format stream "ING(")))

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
    (format stream "~A"
            label)))

(defmethod semstruct-output-hook-index ((rmrsout compact) index)
  (with-slots (stream) rmrsout
    (format stream ",~A" index)))

(defmethod semstruct-output-end-hook ((rmrsout compact))
  (with-slots (stream) rmrsout
    (format stream "] ")))



;;; Actual printing function for robust mrs
;;; version for real mrs uses same (or similar) defmethods but
;;; is a variant of output-mrs

(defun output-rmrs (rmrs-instance device &optional file-name)
     (if file-name
      (with-open-file (stream file-name :direction :output)
         (output-rmrs1 rmrs-instance device stream))
      (output-rmrs1 rmrs-instance device t)))

(defun output-rmrs1 (rmrs-instance device stream)
  (def-rmrs-print-operations device stream)
  (cond ((rmrs-p rmrs-instance)         
         (rmrs-output-start-fn *rmrs-display-structure* 0 0)
         ;;; to be fixed when we have characters
         (print-rmrs rmrs-instance)
         (rmrs-output-end-fn *rmrs-display-structure*))
        (t (rmrs-output-error-fn *rmrs-display-structure* 
                                 rmrs-instance))))

(defun internal-output-rmrs (rmrs-instance device stream)
  ;;; for rule output
  (def-rmrs-print-operations device stream)
  (cond ((rmrs-p rmrs-instance)         
         (print-rmrs rmrs-instance))
        (t (rmrs-output-error-fn *rmrs-display-structure* 
                                 rmrs-instance))))

(defun print-rmrs (rmrs)
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
                            bindings *rmrs-display-structure*))
    (rmrs-output-top-label *rmrs-display-structure*
                       (if top-h
                           (find-rmrs-var-id top-h bindings)
                         (funcall
                          *rmrs-variable-generator*)))
    (loop for ep in eps
        do
          (rmrs-output-start-ep *rmrs-display-structure*
                                (if (char-rel-p ep)
                                    (char-rel-cfrom ep)
                                  0)
                                (if (char-rel-p ep)
                                    (char-rel-cto ep)
                                  0))
          (let ((pred (rel-sort ep)))
            (if (realpred-p pred)
                (rmrs-output-realpred *rmrs-display-structure*
                                      (realpred-lemma pred)
                                      (realpred-pos pred)
                                      (realpred-sense pred))
              (rmrs-output-gpred *rmrs-display-structure* pred)))
          (let ((label (rel-handel ep)))
            (rmrs-output-label *rmrs-display-structure*
                                 (find-rmrs-var-id label bindings)))
          (loop for value in (rel-flist ep)
                             ;; got to be a variable, not a constant
                             ;; but could be a grammar variable
              do
                (print-rmrs-var value bindings *rmrs-display-structure*))
          (rmrs-output-end-ep *rmrs-display-structure*))
    (loop for arg in rmrs-args
        do
          (rmrs-output-start-rmrs-arg *rmrs-display-structure*
                                      (rmrs-arg-arg-type arg))
          (let ((label (rmrs-arg-label arg)))
            (rmrs-output-label *rmrs-display-structure* 
                                   (find-rmrs-var-id
                                    label
                                    bindings)))
          (let ((value (rmrs-arg-val arg)))
            (if (var-p value)
                (print-rmrs-var value bindings *rmrs-display-structure*)
                (rmrs-output-constant-fn 
                 *rmrs-display-structure*
                 value)))
          (rmrs-output-end-rmrs-arg *rmrs-display-structure*))
    (print-rmrs-in-groups rmrs-in-groups bindings *rmrs-display-structure*)
    (print-rmrs-hcons rmrs-h-cons bindings *rmrs-display-structure*)))

(defun find-rmrs-var-id (var bindings)
  (let ((canon-var (if bindings 
                       (lookup-canonical-var var bindings)
                     var)))
    (var-id canon-var)))

(defun print-rmrs-var (value bindings display)
    (if (var-p value)
        (rmrs-output-var-fn 
         display
         (find-rmrs-var-id value bindings)
         (find-var-letter (var-type value)))
      (error "Unexpected value ~A" value)))

(defun print-rmrs-hcons (hcons-list bindings display)
    (loop for hcons in hcons-list
        do
          (rmrs-output-hcons-start
           display (hcons-relation hcons))
          (print-rmrs-var 
           (hcons-scarg hcons) bindings display)
          (rmrs-output-hcons-next 
           display)
          (print-rmrs-var 
            (hcons-outscpd hcons) bindings display)
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
  (semstruct-output-hook-index display
   (find-rmrs-var-id 
    (indices-index hook) bindings))
  (semstruct-output-hook-label display
   (find-rmrs-var-id 
    (indices-label hook) bindings))
  (semstruct-output-end-hook display))
