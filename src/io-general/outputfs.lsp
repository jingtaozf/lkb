;;; Copyright Ann Copestake 1992-1997. All Rights Reserved.
;;; No use or redistribution without permission.
;;; 

(in-package :cl-user)

;;; Displaying FSs in a variety of ways
;;; Window functions are in activefs.lsp which is not 
;;; Lisp-independent

;;; Jan 1997 - made lisp-independent again
;;;          - make compatible with YADU

;;; Display-dag etc are defined to keep the actual printing
;;; operations seperate from the process of walking over the dag
;;; structure.  This is done by setting *display-structure*
;;; to be a structure containing the functions which are appropriate
;;; for the particular output device.


(defvar *reentrancy-pointer* 0)

(defvar *display-structure* nil)

(defun def-print-operations (class indentation stream box)
  (setf *display-structure* (make-instance class 
					   :indentation indentation
					   :stream stream :box box)))

;;; 
;;; Generic output-type class
;;;

(defclass fs-output-type ()
  ((indentation :initform 0 :initarg :indentation)
   (stream :initarg :stream)
   (box :initform nil :initarg :box)
   (max-width :initform 0)))

(defmethod fs-output-error-fn ((fsout fs-output-type) dag-instance)
  (with-slots (stream) fsout
    (format stream
            "~%::: ~A is not a dag...~%"
            dag-instance)))

(defmethod fs-output-max-width-fn ((fsout fs-output-type))
    (with-slots (max-width) fsout
      max-width))

(defmethod fs-output-reentrant-value-endfn ((fsout fs-output-type))
   nil)

(defmethod fs-output-shrinks ((fsout fs-output-type))
   nil)

(defmethod fs-output-no-need-to-display ((fsout fs-output-type) rpath)
   (declare (ignore rpath))
   (values nil nil nil nil))

(defmethod fs-output-record-start 
    ((fsout fs-output-type)  rpath flag pointer)
   (declare (ignore flag pointer rpath))
   nil)

(defmethod fs-output-record-end ((fsout fs-output-type) rpath)
   (declare (ignore rpath))
   nil)

;;; ********** linear operations ***********
;;; - for cheap readable display of dags during tracing etc

(defclass linear (fs-output-type) ())

(defmethod fs-output-start-fn ((fsout linear))
    nil)

(defmethod fs-output-end-fn ((fsout linear))
    nil)

(defmethod fs-output-reentrant-value-fn ((fsout linear) reentrant-pointer)
  (with-slots (stream) fsout
    (format stream
            "<~A>= "
            reentrant-pointer)))

(defmethod fs-output-reentrant-fn ((fsout linear) reentrant-pointer)
  (with-slots (stream) fsout
    (format stream "<~A>" reentrant-pointer)))

(defmethod fs-output-atomic-fn ((fsout linear) atomic-value)
  (with-slots (stream) fsout
    (format stream "~(~A~)" (if (and (listp atomic-value) 
                                             (null (cdr atomic-value)))
                                      (car atomic-value) atomic-value))))

(defmethod fs-output-start-fs ((fsout linear) type depth labels)
  (declare (ignore depth labels))
  (with-slots (stream) fsout
    (format stream "#D(~(~A~)" type)))

(defmethod fs-output-label-fn ((fsout linear) label depth old-x old-y path)
   (declare (ignore depth old-x old-y path))  
   (with-slots (stream) fsout
    (format stream " ~A: " label)))

(defmethod fs-output-end-fs ((fsout linear) terminal)
  (declare (ignore terminal))
  (with-slots (stream) fsout
    (format stream ")")))

;;; linear print is used by the following

(defmethod print-object ((object dag) (stream t))
  ;; default dag structure output during lisp code tracing etc
  ;; effectively ignores *print-level* etc since control never passes out
  ;; to lisp printer again
  (if *print-readably*
      ;; print so object can be read back into lisp
      (call-next-method)
    ;; usual case
    (display-dag1 object 'linear stream)))


;;; ******  TDL printing operations  **********

(defclass tdl (fs-output-type) 
    ((new-fs-p :accessor tdl-new-fs-p
                      :initform t)
     (indentation-vector :accessor edit-indentation-vector
                      :initform (make-array '(3000)))))

(defmethod fs-output-start-fn ((fsout tdl))
    (with-slots (stream) fsout  
      (format stream "~V%" 1)))
   
(defmethod fs-output-end-fn ((fsout tdl))
    nil)

(defmethod fs-output-reentrant-value-fn ((fsout tdl) reentrant-pointer)
  (with-slots (stream indentation) fsout
    (format stream
      "~VT#~A & "
      indentation
      reentrant-pointer)
    (setf indentation
          (cond ((> indentation 53) (terpri stream) 3)
                (t (+ 12 indentation))))))
   
(defmethod fs-output-reentrant-fn ((fsout tdl) reentrant-pointer)
  (with-slots (stream indentation) fsout
    (format stream "~VT#~A" indentation reentrant-pointer)))

(defmethod fs-output-atomic-fn ((fsout tdl) atomic-value)
  (with-slots (stream indentation) fsout
    (if (or (stringp atomic-value) 
                 (and (listp atomic-value) (stringp (car atomic-value))))
           (format stream "~VT~S" indentation 
                   (if (listp atomic-value) (car atomic-value)
                                        atomic-value))
           (format stream "~VT~A" indentation 
                   (string-downcase (if (listp atomic-value) (car atomic-value)
                                        atomic-value))))))

(defmethod fs-output-start-fs ((fsout tdl) type depth labels)
  (declare (ignore labels))
  (with-slots (stream indentation indentation-vector new-fs-p) fsout
    (if (eql type *toptype*)
            (format stream "~VT[ " indentation)
            (if (stringp type)
              (format stream "~VT ~S & [ " indentation type)
              (format stream "~VT ~A & [ " indentation (string-downcase type))))
          (setf new-fs-p t)
         (setf (aref indentation-vector depth) (+ indentation 2))))

(defmethod fs-output-label-fn ((fsout tdl) label depth stored-x stored-y
                               rpath) 
   (declare (ignore stored-x stored-y rpath))
   (with-slots (stream indentation indentation-vector new-fs-p) fsout
         (setf indentation (aref indentation-vector depth))
         (unless new-fs-p 
           (format stream ",~%~VT" indentation))
         (setf new-fs-p nil)
         (format stream "~A " label)
         (setf indentation (+ indentation 7))
         (when (> indentation 65) (terpri stream) (setf indentation 1))))

(defmethod fs-output-end-fs ((fsout tdl) terminal)
  (declare (ignore terminal))
  (with-slots (stream indentation) fsout
    (format stream " ]")
         (incf indentation)))
   

;;; ******* LiLFeS printing operations ***********


(defclass lilfes (fs-output-type) 
    ((new-fs-p :accessor lilfes-new-fs-p
                      :initform t)))

(defmethod fs-output-start-fn ((fsout lilfes))
    nil)
   
(defmethod fs-output-end-fn ((fsout lilfes))
    nil)

(defmethod fs-output-reentrant-value-fn ((fsout lilfes) reentrant-pointer)
  (with-slots (stream) fsout
    (format stream
            "($~A & "
            reentrant-pointer)))
      ;; no terpris

(defmethod fs-output-reentrant-value-endfn ((fsout lilfes))
   (with-slots (stream) fsout
     (format stream ")")))
   
(defmethod fs-output-reentrant-fn ((fsout lilfes) reentrant-pointer)
  (with-slots (stream) fsout
    (format stream "$~A" reentrant-pointer)))

(defmethod fs-output-atomic-fn ((fsout lilfes) atomic-value)
  (with-slots (stream) fsout
    (if (or (stringp atomic-value) 
                 (and (listp atomic-value) (stringp (car atomic-value))))
           (format stream "~S" 
                   (if (listp atomic-value) (car atomic-value)
                                        atomic-value))
           (format stream "'~A'"
                   (string-downcase
                    (convert-lilfes-type
                    (if (listp atomic-value) (car atomic-value)
                                        atomic-value)))))))


(defmethod fs-output-start-fs ((fsout lilfes) type depth labels)
  (declare (ignore labels depth))
  (with-slots (stream new-fs-p) fsout
    (if (eql type *toptype*)
       (format stream "(")
       (if (stringp type)
          (format stream "(~S & " type)
          (format stream "('~A' & " 
            (string-downcase (convert-lilfes-type type)))))
    (setf new-fs-p t)))

(defmethod fs-output-label-fn ((fsout lilfes) label depth stored-x stored-y
                               rpath) 
   (declare (ignore depth stored-x stored-y rpath))
   (with-slots (stream new-fs-p) fsout
     (unless new-fs-p 
        (format stream " & "))
     (setf new-fs-p nil)
     (format stream "~A\\" (convert-lilfes-feature label))))
     
(defmethod fs-output-end-fs ((fsout lilfes) terminal)
  (declare (ignore terminal))
  (with-slots (stream) fsout
    (format stream ")")))

;;; ***** simple print operations ************
;;; used by tty display

(defclass simple (fs-output-type) 
    ((indentation-vector :accessor edit-indentation-vector
                      :initform (make-array '(3000)))))

(defmethod fs-output-start-fn ((fsout simple))
    (with-slots (stream) fsout  
      (format stream "~V%" 1)))
   
(defmethod fs-output-end-fn ((fsout simple))
    (with-slots (stream) fsout  
      (format stream "~V%" 1)))

(defmethod fs-output-reentrant-value-fn ((fsout simple) reentrant-pointer)
  (with-slots (stream indentation) fsout
         (format stream
            "~VT<~A> = "
            indentation
            reentrant-pointer)
         (setf indentation
         (cond ((> indentation 53) (terpri stream) 3)
            (t (+ 12 indentation))))))

(defmethod fs-output-reentrant-fn ((fsout simple) reentrant-pointer)
  (with-slots (stream indentation) fsout
    (format stream "~VT<~A>" indentation reentrant-pointer)))

(defmethod fs-output-atomic-fn ((fsout simple) atomic-value)
  (with-slots (stream indentation) fsout
    (format stream "~VT~A" indentation atomic-value)))

(defmethod fs-output-start-fs ((fsout simple) type depth labels)
  (declare (ignore labels))
  (with-slots (stream indentation indentation-vector) fsout
    (format stream "~VT[~A" indentation type)
    (setf (aref indentation-vector depth) (+ indentation 2))))

(defmethod fs-output-label-fn ((fsout simple) label depth stored-x stored-y
                               rpath) 
   (declare (ignore stored-x stored-y rpath))
   (with-slots (stream indentation indentation-vector) fsout
     (setf indentation (aref indentation-vector depth))
     (format stream "~%~VT~A: " indentation label)
     (setf indentation (+ indentation 7))
     (when (> indentation 65) (terpri stream) (setf indentation 1))))

(defmethod fs-output-end-fs ((fsout simple) terminal)
  (declare (ignore terminal))
  (with-slots (stream indentation) fsout
    (format stream "]")
    (incf indentation)))


;;; ***** `edit' print operations ************
;;; are used by graphical display

(defclass edit (fs-output-type) 
    ((type-label-list :accessor edit-type-label-list
                      :initform nil)
     (indentation-vector :accessor edit-indentation-vector
                      :initform (make-array '(3000)))))
 
(defmethod fs-output-error-fn ((fsout edit) dag-instance)
   (declare (ignore dag-instance))
   (with-slots (stream) fsout
     (format stream
       "~%No feature structure~%")))

                              
(defmethod fs-output-start-fn ((fsout edit))
    (with-slots (stream) fsout  
      (format stream "~V%" 1)))
   
(defmethod fs-output-end-fn ((fsout edit))
    (with-slots (stream) fsout  
      (format stream "~V%" 1)))

(defmethod fs-output-reentrant-value-fn ((fsout edit) reentrant-pointer)
  (with-slots (stream indentation max-width type-label-list) fsout
    (move-to-x-y stream indentation (current-position-y stream))
    (with-bold-output stream 
      (let ((start-pos (current-position stream)))
	(add-active-pointer stream start-pos reentrant-pointer 
			    type-label-list t))) 
    (setf indentation (current-position-x stream))
    (setf max-width (max indentation max-width))))

(defmethod fs-output-reentrant-fn ((fsout edit) reentrant-pointer)
  (with-slots (stream indentation type-label-list max-width) fsout
    (move-to-x-y stream indentation (current-position-y stream))
    (with-bold-output stream
      (let ((start-pos (current-position stream)))
	(add-active-pointer stream start-pos reentrant-pointer 
			    type-label-list nil))) 
    (setf max-width (max (current-position-x stream) max-width))
    (pop type-label-list)))

(defmethod fs-output-atomic-fn ((fsout edit) atomic-value)
  (with-slots (stream indentation type-label-list max-width) fsout
    (let ((val
           (if (cdr atomic-value) atomic-value 
              (car atomic-value)))
          (y-pos (current-position-y stream)))
       (move-to-x-y stream indentation y-pos) 
       ; make start-pos the actual place where the type label starts!!
       (let ((start-pos (current-position stream)))
	 (add-type-and-active-fs-region stream start-pos type-label-list 
					val nil t)
          (setf max-width (max (current-position-x stream) max-width))
          (pop type-label-list)))))

(defmethod fs-output-start-fs ((fsout edit) type depth labels)
  (declare (ignore labels))
  (with-slots (stream indentation indentation-vector type-label-list
                max-width) fsout
    (let ((y-pos (current-position-y stream)))
       (move-to-x-y stream indentation y-pos)
       (write-char #\[ stream)
       (let ((start-pos (current-position stream)))   
          (add-type-and-active-fs-region stream start-pos type-label-list 
           type nil nil)
          (setf max-width (max (current-position-x stream) max-width))
          (setf (aref indentation-vector depth) 
                (+ indentation (stream-string-width stream "[")))))))

(defmethod fs-output-shrunk-fn ((fsout edit) type) 
   (with-slots (stream indentation type-label-list max-width) fsout
     (let ((y-pos (current-position-y stream))
           (start-pos (current-position stream)))
        (move-to-x-y stream indentation y-pos)             
        (add-type-and-active-fs-region stream start-pos 
         type-label-list type t nil)
        (frame-text-box stream start-pos (current-position stream))
        (setf max-width (max (current-position-x stream) max-width))
        (pop type-label-list))))

(defmethod fs-output-shrinks ((fsout edit))
   t)
   
(defmethod fs-output-label-fn ((fsout edit) label depth stored-x stored-y
                               rpath) 
   (with-slots (stream indentation indentation-vector 
                 type-label-list max-width) fsout
     (push label type-label-list)
     (setf indentation (aref indentation-vector depth))         
     (terpri stream)
     ;; write-char #\newline goes wrong on PC
     (if (and stored-x stored-y)
        (move-to-x-y stream stored-x stored-y)
        (move-to-x-y stream indentation (current-position-y stream)))
     (store-fs-record-data-label stream rpath)
     (let ((output-label (concatenate 'string (symbol-name label)
				      ": ")))
        (write-string output-label stream))
     (setf max-width (max (current-position-x stream) max-width))
     (setf indentation (current-position-x stream))))

(defun make-output-label (real-name)
  ;;; removed feature abbreviation facility - probably never used
   (write-to-string real-name :case :upcase))


(defmethod fs-output-end-fs ((fsout edit) terminal)
  (declare (ignore terminal))
  (with-slots (stream type-label-list indentation max-width) fsout
    (setq type-label-list (cdr type-label-list))
    (write-char #\] stream)
    (setf indentation
          (+ indentation (stream-string-width stream "]")))
    (setf max-width (max (current-position-x stream) max-width))))

;; following are to speed up redisplay e.g. when scrolling

(defmethod fs-output-no-need-to-display ((fsout edit) rpath)
   (with-slots (stream box) fsout
     (store-fs-redisplay stream rpath box)))              
              
(defmethod fs-output-record-start 
    ((fsout edit) rpath flag pointer)
      (with-slots (stream) fsout
        (store-fs-record-data stream rpath flag pointer)))

(defmethod fs-output-record-end ((fsout edit) rpath)
   (with-slots (stream) fsout
        (store-fs-record-data-end stream rpath)))

;;; ********* outputting Antonio's TeX macros  ***********
;;; \ has to be inserted into the format as a character ~C

(defclass tex (fs-output-type) 
    ((indentation-vector :accessor edit-indentation-vector
                      :initform (make-array '(3000)))
     (bracket-stack :accessor tex-bracket-stack :initform nil)
     (unoutput-label :accessor tex-unoutput-label :initform nil)))

(defmethod fs-output-start-fn ((fsout tex))
   ;; $   
    (with-slots (stream) fsout  
      (format stream "$")))

(defmethod fs-output-end-fn ((fsout tex))
   ;; $   
   ;;  
    (with-slots (stream) fsout  
      (format stream "$")
      (format stream "~V%" 1)))

(defmethod fs-output-reentrant-value-fn ((fsout tex) reentrant-pointer)
   ;; \attval{label}{\ind{number}}\\
  (with-slots (stream indentation unoutput-label) fsout
    (format stream "\\\\~%~VT\\attval{~A}{\\ind{~A}}" 
            indentation (convert-values-for-tex unoutput-label) 
            reentrant-pointer)
         (setf unoutput-label nil)
         (setf indentation
         (cond ((> indentation 53) (terpri stream) 3)
            (t (+ 12 indentation))))))

(defmethod fs-output-reentrant-fn ((fsout tex) reentrant-pointer)
  (with-slots (stream indentation unoutput-label) fsout
    (format stream "\\\\~%~VT\\attval{~A}{\\ind{~A}}" 
      indentation unoutput-label reentrant-pointer)
    (setf unoutput-label nil)))

(defmethod fs-output-atomic-fn ((fsout tex) atomic-value)
   ;; \attvaltyp{label}{value}\\
  (with-slots (stream indentation unoutput-label) fsout
    (if unoutput-label             
         (format stream "\\\\~%~VT\\attvaltyp{~A}{~(~A~)}" 
            indentation unoutput-label 
            (convert-values-for-tex atomic-value))
         (format stream "\\ \\ \\myvaluebold{~(~A~)}" 
            (convert-values-for-tex atomic-value)))
         (setf unoutput-label nil)))

(defmethod fs-output-shrunk-fn ((fsout tex) type) 
   (with-slots (stream indentation unoutput-label) fsout
     (if unoutput-label             
        (format stream "\\\\~%~VT\\attvalshrunktyp{~A}{~(~A~)}" 
          indentation unoutput-label type)
        (format stream "\\ \\ \\boxvaluebold{~(~A~)}" 
          type))
     (setf unoutput-label nil)))

(defmethod fs-output-shrinks ((fsout tex))
   t)

(defmethod fs-output-start-fs ((fsout tex) type depth labels)
  (with-slots (stream indentation indentation-vector bracket-stack
                unoutput-label) fsout
    (cond 
          ((and unoutput-label labels)
           (format stream 
             "\\\\~%~VT\\attval{~A}{\\avmplus{\\att{~(~A~)}" 
             indentation unoutput-label type)
           ;; \attval{label}{\avmplus{\att{type}\\
           (push 2 bracket-stack)
           (setf unoutput-label nil))
          (labels
           (format stream "~VT\\avmplus{\\att{~(~A~)}" 
             indentation type)
           ;; \avmplus{\att{label}\\
           (push 1 bracket-stack))
          (unoutput-label 
           (format stream 
             "\\\\~%~VT\\attvaltyp{~A}{~(~A~)}" 
             indentation unoutput-label type)
           (setf unoutput-label nil)
           (push 0 bracket-stack))
          (t (format stream "~VT\\ \\ \\myvaluebold{~(~A~)}" 
               indentation type)
            (push 0 bracket-stack))) 
    (setf (aref indentation-vector depth) (+ indentation 2))))

(defmethod fs-output-label-fn ((fsout tex) label depth stored-x stored-y
                               rpath) 
   (declare (ignore stored-x stored-y rpath))
   (with-slots (stream indentation indentation-vector unoutput-label) fsout
     (setf indentation (aref indentation-vector depth))
     (setf indentation (+ indentation 7))
     (setf unoutput-label label)))

(defmethod fs-output-end-fs ((fsout tex) terminal)
  (declare (ignore terminal))
  (with-slots (stream bracket-stack) fsout
    (dotimes (x (pop bracket-stack))
       (format stream "}"))))


;;; TeX support functions 

(defun convert-values-for-tex (atomic-value)
   (if (listp atomic-value)
      (let ((escaped-list
               (mapcar #'convert-underscores-for-tex 
                  atomic-value)))
         (if (cdr escaped-list) escaped-list
            (car escaped-list)))
      (convert-underscores-for-tex atomic-value)))

(defun convert-underscores-for-tex (at-val)
   (let ((value (format nil "~A" at-val))
         (char-bag nil))
         (for char in (coerce value 'list)
            do
            (when (char= char #\_)
               (push #\\ char-bag))
            (push char char-bag))
         (coerce (nreverse char-bag) 'string)))

;;; ********** Paths *************

;;; Output paths in notation defined for types etc
;;; this is the original LKB version, with the type feature
;;; notation in the paths

(defclass pathout (fs-output-type) 
    ((reentrant-vector :accessor pathout-reentrant-vector                      
      :initform (make-array '(3000)))
     (type-label-list :accessor pathout-type-label-list
       :initform nil)))


(defmethod fs-output-start-fn ((fsout pathout))
    (with-slots (stream) fsout  
      (format stream "~V%" 1)))
   
(defmethod fs-output-end-fn ((fsout pathout))
    (with-slots (stream) fsout  
      (format stream ".~V%" 1)))

(defmethod fs-output-reentrant-value-fn ((fsout pathout) reentrant-pointer)
  (with-slots (reentrant-vector type-label-list) fsout
    (setf (aref reentrant-vector reentrant-pointer)
          type-label-list)))
    
(defmethod fs-output-reentrant-fn ((fsout pathout) reentrant-pointer)
  (with-slots (stream type-label-list reentrant-vector) fsout
    (output-typed-list stream type-label-list)
    (format stream " = ")
    (output-typed-list stream 
     (aref reentrant-vector reentrant-pointer))
    (pop type-label-list)
    (format stream "~%")))

(defmethod fs-output-atomic-fn ((fsout pathout) atomic-value)
  (with-slots (stream type-label-list) fsout
    (output-typed-list stream type-label-list)             
    (format stream " = ~A~%" atomic-value)
    (pop type-label-list)))

(defmethod fs-output-start-fs ((fsout pathout) type depth labels)
  (declare (ignore labels depth))
  (with-slots (type-label-list) fsout
    (push type type-label-list)))

(defmethod fs-output-label-fn ((fsout pathout) label depth stored-x stored-y
                               rpath) 
   (declare (ignore depth stored-x stored-y rpath))
   (with-slots (type-label-list) fsout
     (push label type-label-list)))

(defmethod fs-output-end-fs ((fsout pathout) terminal)
  (with-slots (stream type-label-list) fsout
    (when terminal
       (output-typed-list stream (cdr type-label-list))
       (format stream " = ~A~%" (car type-label-list)))
    (pop type-label-list)
    (pop type-label-list)))

;;; support function

(defun output-typed-list (stream type-label-list)
   (let ((ordered-list (reverse type-label-list)))
      (format stream "< ~A ~A " (car ordered-list)
         (cadr ordered-list))
      (when (cddr ordered-list)
         (format stream "~{: ~A ~A ~}" 
            (cddr ordered-list)))
      (format stream ">")))


;;; **** another version of paths ******

;;; this is a simpler version, with no types on
;;; paths


(defclass pathout2 (fs-output-type) 
    ((reentrant-vector :accessor pathout2-reentrant-vector                      
      :initform (make-array '(3000)))
     (type-label-list :accessor pathout2-type-label-list
       :initform nil)))


(defmethod fs-output-start-fn ((fsout pathout2))
    nil)
   
(defmethod fs-output-end-fn ((fsout pathout2))
    (with-slots (stream) fsout  
      (format stream ".~V%" 1)))

(defmethod fs-output-reentrant-value-fn ((fsout pathout2) reentrant-pointer)
  (with-slots (reentrant-vector type-label-list) fsout
    (setf (aref reentrant-vector reentrant-pointer)
            type-label-list)))
    
(defmethod fs-output-reentrant-fn ((fsout pathout2) reentrant-pointer)
  (with-slots (stream type-label-list reentrant-vector) fsout
    (format stream "~%")
    (output-typed-list2 stream type-label-list)
    (format stream " = ")
    (output-typed-list2 stream 
     (aref reentrant-vector reentrant-pointer))
    (pop type-label-list)))

(defmethod fs-output-atomic-fn ((fsout pathout2) atomic-value)
  (with-slots (stream type-label-list) fsout
    (format stream "~%")
    (output-typed-list2 stream type-label-list)             
    (format stream " = ~(~A~)" (if (and (listp atomic-value) 
                                        (null (cdr atomic-value)))
                                  (car atomic-value) atomic-value))
    (pop type-label-list)))
   
   
(defmethod fs-output-start-fs ((fsout pathout2) type depth labels)
  (declare (ignore labels depth))
  (with-slots (type-label-list) fsout
    (push type type-label-list)))

(defmethod fs-output-label-fn ((fsout pathout2) label depth stored-x stored-y
                               rpath) 
   (declare (ignore depth stored-x stored-y rpath))
   (with-slots (type-label-list) fsout
     (push label type-label-list)))

(defmethod fs-output-end-fs ((fsout pathout2) terminal)
  (with-slots (stream type-label-list) fsout
    (when terminal
       (format stream "~%")
       (output-typed-list2 stream (cdr type-label-list))
       (format stream " = ~(~A~)" (car type-label-list)))
    (pop type-label-list)
    (pop type-label-list)))

;;; support fn 

(defun output-typed-list2 (stream type-label-list)
   (let ((ordered-list (reverse type-label-list)))
      (format stream "< ~A " 
         (cadr ordered-list))
      (when (cddr ordered-list)
        (do* ((feat (cadddr ordered-list) (cadr rest))
              (rest (cddddr ordered-list) (cddr rest)))
             ((null feat) nil)
         (format stream ": ~A " feat)))
      (format stream ">")))

;;; ******** Output shrunkenness *********

;;; eg the way a feature structure is displayed is defined by
;;; specifying a series of paths

(defclass shrunk (fs-output-type) 
    ((label-list :accessor shrunk-label-list
                      :initform nil)
     (shrunk-list :accessor shrunk-shrunk-list
                      :initform nil)))

(defmethod fs-output-start-fn ((fsout shrunk))
   nil)

(defmethod fs-output-end-fn ((fsout shrunk))
   nil)

(defmethod fs-output-reentrant-value-fn ((fsout shrunk) reentrant-pointer)
   (declare (ignore reentrant-pointer))
   nil)

(defmethod fs-output-reentrant-fn ((fsout shrunk) reentrant-pointer)
   (declare (ignore reentrant-pointer))
   (with-slots (label-list) fsout
     (pop label-list)))

(defmethod fs-output-atomic-fn ((fsout shrunk) atomic-value)
   (declare (ignore atomic-value))
   (with-slots (label-list) fsout
     (pop label-list)))

(defmethod fs-output-shrinks ((fsout shrunk))
   t)

(defmethod fs-output-shrunk-fn ((fsout shrunk) type)
   (declare (ignore type))
   (with-slots (label-list shrunk-list) fsout
     (let ((ordered-list (reverse label-list)))
            (push ordered-list shrunk-list)
            (pop label-list))))

(defmethod fs-output-start-fs ((fsout shrunk) type depth labels)
   (declare (ignore type depth labels))
         nil)


(defmethod fs-output-label-fn ((fsout shrunk) 
                               label depth stored-x stored-y
                               rpath)
   (declare (ignore depth stored-x stored-y rpath)) 
   (with-slots (label-list) fsout
     (push label label-list)))

(defmethod fs-output-max-width-fn ((fsout shrunk))
   ;;; need to return a value (nothing to do with max-width ...)
   (with-slots (shrunk-list) fsout
         shrunk-list))
     
(defmethod fs-output-end-fs ((fsout shrunk) terminal)
  (declare (ignore terminal))
      (with-slots (label-list) fsout
        (pop label-list)))

;;;
;;; ************* Shrinking paths in types **********
;;; 


(def-lkb-parameter *shrunk-types* nil)
(defvar *shrunk-local-dags* nil)
(defvar *not-shrunk-local-dags* nil)

#|
(defun set-up-display-settings (file-name)
   (with-open-file 
      (istream file-name :direction :input)
      (for setting in-stream istream
         do
         (unless (get-type-entry (car setting))
            (error "Type ~A not found" (car setting)))
         (for path in (cdr setting)
            do
            (unless (listp path)
               (error "~A is not a valid path" path))
            (pushnew (nconc (reverse path) (list (car setting))) *shrunk-types*
		     :test #'equal)))))
|#

(defun set-up-display-settings (filename)
  (when (and filename
	     (probe-file filename))
    (with-open-file (stream filename 
		     :direction :input)
      (setf *shrunk-types* (read stream nil nil)))))

(defun set-dag-display-value (fs f-list action type-fs-display)
   (let* ((sub-dag
             (if (tdfs-p fs) (existing-dag-at-end-of (tdfs-indef fs) f-list) nil))
          (type
             (type-of-fs (if (tdfs-p fs) (tdfs-indef fs) fs)))
          (spec
             (nconc (reverse f-list) (list type))))
      (if (eql action :shrink)
         (progn
            ;; shrink a currently expanded node - remove any block on this particular
            ;; node being shrunk, add it to the local list of shrunk nodes, and
            ;; if this is a type definition record the path to the root of the fs
            ;; as a shrunk path 
            (when sub-dag
               (setf (tdfs-not-shrunk fs) ; override any globally-set shrunk path
                  (remove sub-dag (tdfs-not-shrunk fs) :test #'eq))
               (pushnew sub-dag (tdfs-shrunk fs) ; in addition to any globally-set shrunk paths
                  :test #'eq))
            (when type-fs-display (pushnew spec *shrunk-types* :test #'equal)))
         (progn
            ;; expand a node which is currently shrunk - remove it from the local list
            ;; of shrunk nodes, block any path saying that this node should be shrunk,
            ;; and if this is a type definition remove any record of the path to root
            ;; being a shrunk path
            (when sub-dag
               (setf (tdfs-shrunk fs)
                  (remove sub-dag (tdfs-shrunk fs) :test #'eq))
               ;; we maybe don't quite get the expected behaviour if a user has unshrunk
               ;; a local node, shrunk a matching path in a type definition, and comes
               ;; back and redisplays this dag: the local node won't be shrunk because
               ;; it's on the local not-shrunk list
               (pushnew sub-dag (tdfs-not-shrunk fs) :test #'eq))
            (when type-fs-display
               (setq *shrunk-types* (remove spec *shrunk-types* :test #'equal)))))))


;;; 
;;; *********** Display functions  ************
;;;

(defun display-dag (dag-instance device &optional file-name)
   (flet ((display (dag-instance device stream)
            (cond
               ((dag-p dag-instance)
                  (display-dag1 dag-instance device stream))
               ((tdfs-p dag-instance)
                  (display-dag2 dag-instance device stream))
               (t (error "Not a feature structure")))))
      (if file-name
         (with-open-file (stream file-name :direction :output :if-exists :supersede
                                 :if-does-not-exist :create)
            (display dag-instance device stream))
         (display dag-instance device t))))

(defparameter *no-type* nil
  "if this is set via optional argument to display-dag1 instead
of using the real first type in a feature structure, the code in print-dag-aux is
called on *toptype*.  This is for conversion of type constraints
for PAGE and LiLFeS")

(defun display-dag1 (dag-instance device stream &optional x-pos 
                                  no-first-type box)
  (def-print-operations device (or x-pos 0) stream box)
  (let ((*no-type* no-first-type))
    (cond ((dag-p dag-instance)
           (invalidate-visit-marks)
           (mark-dag-for-output dag-instance)
           (setf *reentrancy-pointer* 0)
           (fs-output-start-fn *display-structure*)
           (print-dag dag-instance 0 nil)
           (fs-output-end-fn *display-structure*)              
           (fs-output-max-width-fn *display-structure*))
          (t (fs-output-error-fn *display-structure* 
               dag-instance)))))

(defun mark-dag-for-output (dag-instance)
   (let ((real-dag (follow-pointers dag-instance)))
      (cond
         ((dag-visit real-dag)
            (setf (dag-visit real-dag) 'double))
         (t
            (setf (dag-visit real-dag) 'single)
            (unless (is-atomic real-dag)
               (dolist (arc (dag-arcs real-dag))
                  (let ((label (dag-arc-attribute arc)))
                     (mark-dag-for-output (get-dag-value real-dag label)))))))))


(defun print-dag (dag-instance depth rpath)
  (let* ((real-dag (deref-dag dag-instance)))
    (multiple-value-bind (dont-display-p stored-flag stored-pointer
			  stored-label-pos)
	(fs-output-no-need-to-display *display-structure* rpath)
      (unless dont-display-p
	(let ((new-rpath (cons (type-of-fs real-dag) rpath))
	      (flag-value (or stored-flag (dag-visit real-dag))))
	  (declare (dynamic-extent new-rpath))
	  (fs-output-record-start *display-structure* rpath
				  flag-value *reentrancy-pointer*)
	  (cond 
	   ((eq flag-value 'double)
	    (setf (dag-visit real-dag)
	      (or stored-pointer *reentrancy-pointer*))
	    (incf *reentrancy-pointer*)
	    (fs-output-reentrant-value-fn *display-structure*
					  (dag-visit real-dag))  
	    (print-dag-aux real-dag depth new-rpath stored-label-pos)
	    (fs-output-reentrant-value-endfn *display-structure*))
	   ((eq flag-value 'single)
	    (print-dag-aux real-dag depth new-rpath stored-label-pos))
	   (t (fs-output-reentrant-fn *display-structure* 
				      flag-value)))
	  (fs-output-record-end *display-structure* rpath))))))
  
(defun print-dag-aux (real-dag depth rpath stored-label-pos)
  (cond 
   ((is-atomic real-dag) 
    (fs-output-atomic-fn *display-structure* (type-of-fs real-dag)))
   ((and (fs-output-shrinks *display-structure*) 
	 ;; shrink it if it is locally specified as shrunk, or it's
	 ;; globally specified and not overriden locally (really
	 ;; should test for shrunk-fn method being defined first)
	 (or (member real-dag *shrunk-local-dags* :test #'eq)
	     (and (find rpath *shrunk-types* :test #'print-dag-shrunk-match-p)
		  (not (member real-dag *not-shrunk-local-dags* :test #'eq)))))
    (fs-output-shrunk-fn *display-structure* (dag-type real-dag)))
   (t 
    (let* ((type (if *no-type* 
		     *toptype*
		   (type-of-fs real-dag)))
	   (labels (top-level-features-of real-dag))
	   (start-x nil)
	   (start-y nil))
      (setf *no-type* nil)
      (if labels
	  (progn
	    (fs-output-start-fs *display-structure* type depth labels)
	    (loop for label in (canonical-order type labels)
		do
		  (when stored-label-pos
		    (setf start-x (caar stored-label-pos))
		    (setf start-y (cdar stored-label-pos))
		    (setf stored-label-pos 
		      (cdr stored-label-pos)))
		  (fs-output-label-fn *display-structure*
				      label depth start-x start-y (cdr rpath))
		  (let ((new-rpath (cons label rpath)))
		    (declare (dynamic-extent new-rpath))
		    (print-dag (get-dag-value real-dag label) 
			       (+ 1 depth) new-rpath)))
	    (fs-output-end-fs *display-structure*
			      (null labels)))
	(fs-output-atomic-fn *display-structure*
			     (list type))))))           
  (setf *no-type* nil))

(defun print-dag-shrunk-match-p (x y)
   ;; x is an alternating list of types and features representing the current place
   ;; in the fs, y is a shrunk path spec consisting of a list of features followed by
   ;; a type. Both are in 'reverse' order (i.e. deeper features first). Return true
   ;; if y an initial segment of x, modulo type subsumption at the end of x
   ;; e.g. true for x = (VERB HEAD CAT CAT LOCAL LOCAL PHR_SYNSEM SYNSEM ROOT_CLAUSE),
   ;; y = (HEAD CAT LOCAL CANONICAL_SYNSEM) if PHR_SYNSEM < CANONICAL_SYNSEM
   ;; and for x = (ROOT_CLAUSE), y = (CANONICAL_SYNSEM), and for
   ;; x = (*DIFF-LIST* H-STORE BASICMRS C-CONT HCOMP_RULE), y = (*DIFF-LIST*)
   (cond
      ((null (cdr y))
         (or (eq (car x) (car y)) (subtype-p (car x) (car y))))
      ((eq (cadr x) (car y))
         (print-dag-shrunk-match-p (cddr x) (cdr y)))))
                                      
                                      
;;; We want a standard order on the features - otherwise the output becomes 
;;; very difficult to read. 


(defun canonical-order (type dag-attributes)
   (let ((ordered-attributes 
            (type-appfeats (get-any-type-entry type))))
       (sort (copy-list dag-attributes)
             #'(lambda (x y)
                (member y (member x ordered-attributes))))))

