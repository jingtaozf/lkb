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

;;; 
;;; xml rmrs-output-type class
;;;

(defclass xml (rmrs-output-type) ())

(defmethod rmrs-output-start-fn ((rmrsout xml))
  (with-slots (stream) rmrsout
    (format stream "~%<rmrs>")))

(defmethod rmrs-output-end-fn ((rmrsout xml))
  (with-slots (stream) rmrsout
    (format stream "~%</rmrs>~%")))

(defmethod rmrs-output-start-eps ((rmrsout xml))
  (with-slots (stream) rmrsout
    (format stream "")))

(defmethod rmrs-output-start-ep ((rmrsout xml) predname)
  (with-slots (stream) rmrsout
    (format stream "~%<ep><realpred>~A</realpred>" predname)))

(defmethod rmrs-output-arg-fn ((rmrsout xml))
  (with-slots (stream) rmrsout
    (format stream "<arg>")))

(defmethod rmrs-output-end-arg-fn ((rmrsout xml))
  (with-slots (stream) rmrsout
    (format stream "</arg>")))

(defmethod rmrs-output-var-fn ((rmrsout xml) var-string)
  (with-slots (stream) rmrsout
    (format stream "~A" var-string)))

(defmethod rmrs-output-constant-fn ((rmrsout xml) constant)
  (with-slots (stream) rmrsout
    (format stream "~A" constant)))

(defmethod rmrs-output-end-ep ((rmrsout xml))
  (with-slots (stream) rmrsout
    (format stream "</ep>")))

(defmethod rmrs-output-end-eps ((rmrsout xml))
  (with-slots (stream) rmrsout
    (format stream "")))

(defmethod rmrs-output-label ((rmrsout xml) label)
  (with-slots (stream) rmrsout
    (format stream "<label>~A</label>" label)))

;;; Parsonian arguments

(defmethod rmrs-output-start-rmrs-args ((rmrsout xml))
  (with-slots (stream) rmrsout
    (format stream "")))

(defmethod rmrs-output-start-rmrs-arg ((rmrsout xml) predname)
  (with-slots (stream) rmrsout
    (format stream "~%<rarg><rargname>~A</rargname>" predname)))

(defmethod rmrs-output-end-rmrs-arg ((rmrsout xml))
  (with-slots (stream) rmrsout
    (format stream "</rarg>")))

(defmethod rmrs-output-end-rmrs-args ((rmrsout xml))
  (with-slots (stream) rmrsout
    (format stream "")))

;;; hcons

(defmethod rmrs-output-start-h-cons ((rmrsout xml))
  (with-slots (stream) rmrsout
    (format stream "")))

(defmethod rmrs-output-outscopes ((rmrsout xml) reln higher lower)
  (with-slots (stream) rmrsout
    (format stream "~%<hcons><hreln>~A</hreln><hi>~A</hi><lo>~A</lo></hcons>" 
            reln higher lower)))

(defmethod rmrs-output-end-h-cons ((rmrsout xml))
  (with-slots (stream) rmrsout
    (format stream "")))


;;; in-group

(defmethod rmrs-output-start-ingroup ((rmrsout xml))
  (with-slots (stream) rmrsout
    (format stream "")))

(defmethod rmrs-output-ingroup ((rmrsout xml) a b)
  (with-slots (stream) rmrsout
    (format stream "~%<ing><ing-a>~A</ing-a><ing-b>~A</ing-b></ing>" 
            a b)))

(defmethod rmrs-output-end-ingroup ((rmrsout xml))
  (with-slots (stream) rmrsout
    (format stream "")))

;;; methods for semstructs

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

;;; compact representation for tracing etc

;;; 
;;; xml rmrs-output-type class
;;;

(defclass compact (rmrs-output-type) ())

(defmethod rmrs-output-start-fn ((rmrsout compact))
  (with-slots (stream) rmrsout
    (format stream "")))

(defmethod rmrs-output-end-fn ((rmrsout compact))
  (with-slots (stream) rmrsout
    (format stream "")))

(defmethod rmrs-output-start-eps ((rmrsout compact))
  (with-slots (stream) rmrsout
    (format stream "")))

(defmethod rmrs-output-start-ep ((rmrsout compact) predname)
  (with-slots (stream) rmrsout
    (format stream "~A(" predname)))

(defmethod rmrs-output-arg-fn ((rmrsout compact))
  (with-slots (stream) rmrsout
    (format stream "")))

(defmethod rmrs-output-end-arg-fn ((rmrsout compact))
  (with-slots (stream) rmrsout
    (format stream "")))

(defmethod rmrs-output-var-fn ((rmrsout compact) var-string)
  (with-slots (stream) rmrsout
    (format stream ",~A" var-string)))

(defmethod rmrs-output-constant-fn ((rmrsout compact) constant)
  (with-slots (stream) rmrsout
    (format stream ",~A" constant)))

(defmethod rmrs-output-end-ep ((rmrsout compact))
  (with-slots (stream) rmrsout
    (format stream ")~%")))

(defmethod rmrs-output-end-eps ((rmrsout compact))
  (with-slots (stream) rmrsout
    (format stream "")))

(defmethod rmrs-output-label ((rmrsout compact) label)
  (with-slots (stream) rmrsout
    (format stream "~A" label)))

;;; Parsonian arguments

(defmethod rmrs-output-start-rmrs-args ((rmrsout compact))
  (with-slots (stream) rmrsout
    (format stream "")))

(defmethod rmrs-output-start-rmrs-arg ((rmrsout compact) predname)
  (with-slots (stream) rmrsout
    (format stream "~A(" predname)))

(defmethod rmrs-output-end-rmrs-arg ((rmrsout compact))
  (with-slots (stream) rmrsout
    (format stream ")~%")))

(defmethod rmrs-output-end-rmrs-args ((rmrsout compact))
  (with-slots (stream) rmrsout
    (format stream "")))

;;; hcons

(defmethod rmrs-output-start-h-cons ((rmrsout compact))
  (with-slots (stream) rmrsout
    (format stream "")))

(defmethod rmrs-output-outscopes ((rmrsout compact) reln higher lower)
  (with-slots (stream) rmrsout
    (format stream "~A(~A,~A)~%" 
            reln higher lower)))

(defmethod rmrs-output-end-h-cons ((rmrsout compact))
  (with-slots (stream) rmrsout
    (format stream "")))


;;; in-group

(defmethod rmrs-output-start-ingroup ((rmrsout compact))
  (with-slots (stream) rmrsout
    (format stream "")))

(defmethod rmrs-output-ingroup ((rmrsout compact) a b)
  (with-slots (stream) rmrsout
    (format stream "ing(~A,~A) " a b)))

(defmethod rmrs-output-end-ingroup ((rmrsout compact))
  (with-slots (stream) rmrsout
    (format stream "")))

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
         (rmrs-output-start-fn *rmrs-display-structure*)
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
        (eps (rmrs-liszt rmrs))
        (rmrs-args (rmrs-rmrs-args rmrs))
        (rmrs-h-cons (rmrs-h-cons rmrs))
        (rmrs-in-groups (rmrs-in-groups rmrs))
        (bindings (if (semstruct-p rmrs)
                      (close-bindings (rmrs-bindings rmrs))
                       (rmrs-bindings rmrs))))
    (when hook
      (print-semstruct-hook hook 
                            bindings *rmrs-display-structure*))
    (rmrs-output-start-eps *rmrs-display-structure*)
    (loop for ep in eps
        do
          (rmrs-output-start-ep *rmrs-display-structure*
                                (rel-sort ep))
          (rmrs-output-label *rmrs-display-structure* 
                             (find-rmrs-var-name (rel-handel ep) bindings))
          (loop for value in (rel-flist ep)
              do
                (rmrs-output-arg-fn *rmrs-display-structure*)
                (if (var-p value)
                    (rmrs-output-var-fn 
                     *rmrs-display-structure*
                     (find-rmrs-var-name value bindings)))
                (rmrs-output-end-arg-fn *rmrs-display-structure*))
          (rmrs-output-end-ep *rmrs-display-structure*))
    (rmrs-output-end-eps *rmrs-display-structure*)
    (rmrs-output-start-rmrs-args *rmrs-display-structure*)
    (loop for arg in rmrs-args
        do
          (rmrs-output-start-rmrs-arg *rmrs-display-structure*
                                 (rmrs-arg-arg-type arg))
          (rmrs-output-label *rmrs-display-structure* 
                             (find-rmrs-var-name (rmrs-arg-label arg) bindings))
          (rmrs-output-arg-fn *rmrs-display-structure*)
          (let ((value (rmrs-arg-val arg)))
            (if (var-p value)
                (rmrs-output-var-fn 
                 *rmrs-display-structure*
                 (find-rmrs-var-name value bindings))
                (rmrs-output-constant-fn 
                 *rmrs-display-structure*
                 value))
            (rmrs-output-end-arg-fn *rmrs-display-structure*))
          (rmrs-output-end-rmrs-arg *rmrs-display-structure*))
    (rmrs-output-end-rmrs-args *rmrs-display-structure*)
    (print-rmrs-in-groups rmrs-in-groups bindings *rmrs-display-structure*)
    (print-rmrs-hcons rmrs-h-cons bindings *rmrs-display-structure*)))

(defun find-rmrs-var-name (var bindings)
  (let ((canon-var (if bindings 
                       (lookup-canonical-var var bindings)
                     var)))
    (if (var-p var)
        (format nil "~A~A" 
                (find-var-letter (var-type canon-var)) 
                (var-id  canon-var))
        ;;; for the rules, the variable will just be a string
      (format nil "~A" var))))

(defun print-rmrs-hcons (hcons-list bindings display)
    (rmrs-output-start-h-cons display)
    (loop for hcons in hcons-list
        do
          (rmrs-output-outscopes 
           display
           (hcons-relation hcons)
           (find-rmrs-var-name 
            (hcons-scarg hcons) bindings) 
           (find-rmrs-var-name 
            (hcons-outscpd hcons) bindings))
          (rmrs-output-end-h-cons display)))


(defun print-rmrs-in-groups (ingroup-list bindings display)
    (rmrs-output-start-ingroup display)
    (loop for in-g in ingroup-list
        do
          (rmrs-output-ingroup 
           display
           (find-rmrs-var-name 
            (car (in-group-labels in-g)) bindings) 
           (find-rmrs-var-name 
            (cadr (in-group-labels in-g)) bindings))
          (rmrs-output-end-ingroup display)))

(defun print-semstruct-hook (hook bindings display)
  (semstruct-output-start-hook display)
  (semstruct-output-hook-label display
   (find-rmrs-var-name 
    (indices-label hook) bindings))
  (semstruct-output-hook-index display
   (find-rmrs-var-name 
    (indices-index hook) bindings))
  (semstruct-output-end-hook display))
