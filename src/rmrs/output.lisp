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

(defmethod rmrs-output-end-ep ((rmrsout xml))
  (with-slots (stream) rmrsout
    (format stream "</ep>")))

(defmethod rmrs-output-end-eps ((rmrsout xml))
  (with-slots (stream) rmrsout
    (format stream "")))

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

(defun print-rmrs (rmrs)
  (let ((eps (rmrs-eps rmrs)) 
        (bindings (rmrs-bindings rmrs)))
    (rmrs-output-start-eps *rmrs-display-structure*)
    (loop for ep in eps
        do
          (rmrs-output-start-ep *rmrs-display-structure*
                                (ep-sort ep))
          (loop for value in (ep-flist ep)
              do
                (rmrs-output-arg-fn *rmrs-display-structure*)
                (if (var-p value)
                    (rmrs-output-var-fn 
                     *rmrs-display-structure*
                     (find-rmrs-var-name value bindings)))
                (rmrs-output-end-arg-fn *rmrs-display-structure*))
          (rmrs-output-end-ep *rmrs-display-structure*))
    (rmrs-output-end-eps *rmrs-display-structure*)))

(defun find-rmrs-var-name (var bindings)
  (let ((canon-var (if bindings 
                       (lookup-canonical-var var bindings)
                     var)))
    (format nil "~A~A" 
            (find-var-letter (var-type canon-var)) 
            (var-id  canon-var))))

