;;; Copyright Ann Copestake 1992-1997. All Rights Reserved.
;;; No use or redistribution without permission.
;;; 

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

(defstruct (fs-output)
   name error-fn start-fn end-fn indentation 
   reentrant-value-fn reentrant-fn
   atomic-fn start-fs label-fn end-fs 
   shrunk-fn
   (max-width-fn #'(lambda nil nil)))
 
(defun def-print-operations (device indentation stream)
   (case device 
      (linear (def-linear-print-operations indentation stream))
      (tdl (def-tdl-print-operations indentation stream))
      (simple (def-simple-print-operations indentation stream))
      (edit (def-edit-print-operations indentation stream))
      (tex (def-tex-print-operations indentation stream))
      (shrunk (def-shrunk-print-operations stream))
      (path (def-path-print-operations stream))
      (path2 (def-path2-print-operations stream))
      (tail (def-tail-print-operations indentation stream))))


(defun def-linear-print-operations (indentation stream)
  (declare (ignore indentation))
  (setf *display-structure*
   (make-fs-output 
      :name 'linear
      :error-fn #'(lambda (dag-instance)
         (format stream
            "~%::: ~A is not a dag...~%"
            dag-instance))
      :start-fn #'false      
      :end-fn #'false
      :reentrant-value-fn                 
      #'(lambda (reentrant-pointer)
         (format stream
            "<~A>= "
            reentrant-pointer))
      :reentrant-fn
      #'(lambda (reentrant-pointer)
         (format stream "<~A>" reentrant-pointer))
      :atomic-fn
      #'(lambda (atomic-value)              
         (format stream "~(~A~)" (if (and (listp atomic-value) 
                                             (null (cdr atomic-value)))
                                      (car atomic-value) atomic-value)))
      :start-fs
      #'(lambda (type depth labels)
          (declare (ignore depth labels))
         (format stream "#D(~(~A~)" type))
      :label-fn
      #'(lambda (label depth)
          (declare (ignore depth))
         (format stream " ~A: " label))
      :max-width-fn
      #'(lambda nil 0)
      :end-fs 
      #'(lambda (terminal)
          (declare (ignore terminal))
         (format stream ")")))))

(defmethod print-object ((object dag) (stream t))
  ;; default dag structure output during lisp code tracing etc
  ;; effectively ignores *print-level* etc since control never passes out
  ;; to lisp printer again
  (if *print-readably*
      ;; print so object can be read back into lisp
      (call-next-method)
    ;; usual case
    (display-dag1 object 'linear stream)))


(defun def-tdl-print-operations (indentation stream)
   (let ((indentation-vector (make-array '(3000)))
         (new-fs-p t))
  (setf *display-structure*
   (make-fs-output 
      :name 'tdl
      :error-fn #'(lambda (dag-instance)
         (format stream
            "~%::: ~A is not a dag...~%"
            dag-instance))
      :start-fn #'(lambda nil (format stream "~V%" 1))      
      :end-fn #'(lambda nil (format stream "~V%" 1))
      :reentrant-value-fn                 
      #'(lambda (reentrant-pointer)
         (format stream
            "~VT#~A & "
            indentation
            reentrant-pointer)
         (setf indentation
         (cond ((> indentation 53) (terpri stream) 3)
            (t (+ 12 indentation)))))
      :reentrant-fn
      #'(lambda (reentrant-pointer)
         (format stream "~VT#~A" indentation reentrant-pointer))
      :atomic-fn
      #'(lambda (atomic-value)
         (if (or (stringp atomic-value) 
                 (and (listp atomic-value) (stringp (car atomic-value))))
           (format stream "~VT~S" indentation 
                   (if (listp atomic-value) (car atomic-value)
                                        atomic-value))
           (format stream "~VT~A" indentation 
                   (string-downcase (if (listp atomic-value) (car atomic-value)
                                        atomic-value)))))
      :start-fs
      #'(lambda (type depth labels)
          (declare (ignore labels))
          (if (eql type *toptype*)
            (format stream "~VT[ " indentation)
            (if (stringp type)
              (format stream "~VT ~S & [ " indentation type)
              (format stream "~VT ~A & [ " indentation (string-downcase type))))
          (setf new-fs-p t)
         (setf (aref indentation-vector depth) (+ indentation 2)))
      :label-fn
      #'(lambda (label depth)
         (setf indentation (aref indentation-vector depth))
         (unless new-fs-p 
           (format stream ",~%~VT" indentation))
         (setf new-fs-p nil)
         (format stream "~A " label)
         (setf indentation (+ indentation 7))
         (when (> indentation 65) (terpri stream) (setf indentation 1)))
      :end-fs 
      #'(lambda (terminal)
          (declare (ignore terminal))
         (format stream " ]")
         (incf indentation))))))


         
(defun def-simple-print-operations (indentation stream)
   (let ((indentation-vector (make-array '(3000))))
  (setf *display-structure*
   (make-fs-output 
      :name 'simple
      :error-fn #'(lambda (dag-instance)
         (format stream
            "~%::: ~A is not a dag...~%"
            dag-instance))
      :start-fn #'(lambda nil (format stream "~V%" 1))      
      :end-fn #'(lambda nil (format stream "~V%" 1))
      :reentrant-value-fn                 
      #'(lambda (reentrant-pointer)
         (format stream
            "~VT<~A> = "
            indentation
            reentrant-pointer)
         (setf indentation
         (cond ((> indentation 53) (terpri stream) 3)
            (t (+ 12 indentation)))))
      :reentrant-fn
      #'(lambda (reentrant-pointer)
         (format stream "~VT<~A>" indentation reentrant-pointer))
      :atomic-fn
      #'(lambda (atomic-value)              
         (format stream "~VT~A" indentation atomic-value))
      :start-fs
      #'(lambda (type depth labels)
          (declare (ignore labels))
         (format stream "~VT[~A" indentation type)
         (setf (aref indentation-vector depth) (+ indentation 2)))
      :label-fn
      #'(lambda (label depth)
         (setf indentation (aref indentation-vector depth))
         (format stream "~%~VT~A: " indentation label)
         (setf indentation (+ indentation 7))
         (when (> indentation 65) (terpri stream) (setf indentation 1)))
      :end-fs 
      #'(lambda (terminal)
          (declare (ignore terminal))
         (format stream "]")
         (incf indentation))))))

(defun make-output-label (real-name)
   (let* ((real-string (write-to-string real-name))
	  (real-length (length real-string)))
      (or (cdr (assoc-if 
               #'(lambda (test-string)
                  (let ((test-l (length test-string)))
                     (and (> real-length test-l)
                        (string-equal real-string test-string
                           :start1 (- real-length test-l)))))
               *feature-abbreviations*))
         real-string)))


(defun def-edit-print-operations (indentation stream)
   (let ((type-label-list nil)
         (indentation-vector (make-array '(3000)))
         (max-width 0))  ; was 100 - changed to 0 for tdfs
  (setf *display-structure*
   (make-fs-output 
      :name 'edit
      :error-fn #'(lambda (dag-instance)
         (format stream
            "~%No feature structure~%"
            dag-instance)
         nil)
      :start-fn #'(lambda nil (format stream "~V%" 1))      
      :end-fn #'(lambda nil (format stream "~V%" 1))
      :reentrant-value-fn                 
      #'(lambda (reentrant-pointer)
          (move-to-x-y stream indentation (current-position-y stream))
         (with-bold-output stream 
                           (format stream
                                   "<~A> = "
                                   reentrant-pointer))
         (setf indentation (current-position-x stream))
         (setf max-width (max indentation max-width)))
      :reentrant-fn
      #'(lambda (reentrant-pointer)
         (move-to-x-y stream indentation (current-position-y stream))
         (with-bold-output stream
                           (format stream
                                   "<~A>"
                                   reentrant-pointer))
         (setf max-width (max (current-position-x stream) max-width))
         (pop type-label-list))
      :atomic-fn
      #'(lambda (atomic-value) 
          (let ((val
                 (if (cdr atomic-value) atomic-value 
                     (car atomic-value)))
                (y-pos (current-position-y stream)))
            (move-to-x-y stream indentation y-pos) 
            ; make start-pos the actual place where the type label starts!!
            (let ((start-pos (current-position stream)))
              (add-type-and-active-fs-region stream start-pos type-label-list val nil t)
              (setf max-width (max (current-position-x stream) max-width))
              (pop type-label-list))))
      :shrunk-fn
      #'(lambda (type) 
         (let ((y-pos (current-position-y stream))
               (start-pos (current-position stream)))
            (move-to-x-y stream indentation y-pos)             
            (add-type-and-active-fs-region stream start-pos 
               type-label-list type t nil)
            (frame-text-box stream start-pos (current-position stream))
            (setf max-width (max (current-position-x stream) max-width))
            (pop type-label-list)))
      :start-fs
      #'(lambda (type depth labels)
          (declare (ignore labels))
          (let ((y-pos (current-position-y stream)))
            (move-to-x-y stream indentation y-pos)
            (write-char #\[ stream)
            (let ((start-pos (current-position stream)))   
              (add-type-and-active-fs-region stream start-pos type-label-list 
                                    type nil nil)
              (setf max-width (max (current-position-x stream) max-width))
              (setf (aref indentation-vector depth) 
                    (+ indentation (stream-string-width stream "["))))))
      :label-fn
      #'(lambda (label depth)
         (push label type-label-list)
         (setf indentation (aref indentation-vector depth))         
         (write-char #\newline stream)
         (move-to-x-y stream indentation (current-position-y stream))
         (let ((output-label (make-output-label label)))
	   (write-string output-label stream)
	   (write-string ": " stream))
         (setf max-width (max (current-position-x stream) max-width))
         (setf indentation (current-position-x stream)))
      :max-width-fn
      #'(lambda nil max-width)
      :end-fs 
      #'(lambda (terminal)
          (declare (ignore terminal))
         (setq type-label-list (cdr type-label-list))
         (write-char #\] stream)
         (setf indentation
            (+ indentation (stream-string-width stream "]")))
         (setf max-width (max (current-position-x stream) max-width)))))))


;;; outputting Antonio's TeX macros
;;; \ has to be inserted into the format as a character ~C
                     
(defun def-tex-print-operations (indentation stream)
   (let ((indentation-vector (make-array '(3000)))
         (bracket-stack nil)
         (unoutput-label nil))
      ;; need to keep the last label around to use
      ;; attvaltyp etc which take a label plus a value
      ;; as arguments
  (setf *display-structure*
   (make-fs-output 
      :name 'tex
      :error-fn #'(lambda (dag-instance)
         (format stream
            "~%::: ~A is not a dag...~%"
            dag-instance))
      :start-fn 
      #'(lambda nil 
         (format stream "$"))  
         ;; $   
      :end-fn #'(lambda nil 
         (format stream "$")
         (format stream "~V%" 1))
         ;; $
         ;;
      :reentrant-value-fn                 
      #'(lambda (reentrant-pointer)
         (format stream "\\\\~%~VT\\attval{~A}{\\ind{~A}}" 
            indentation (convert-values-for-tex unoutput-label) 
            reentrant-pointer)
         (setf unoutput-label nil)
         (setf indentation
         (cond ((> indentation 53) (terpri stream) 3)
            (t (+ 12 indentation)))))
         ;; \attval{label}{\ind{number}}\\
      :reentrant-fn
      #'(lambda (reentrant-pointer)
         (format stream "\\\\~%~VT\\attval{~A}{\\ind{~A}}" 
           indentation unoutput-label reentrant-pointer)
        (setf unoutput-label nil))
      :atomic-fn
      #'(lambda (atomic-value) 
         (if unoutput-label             
         (format stream "\\\\~%~VT\\attvaltyp{~A}{~(~A~)}" 
            indentation unoutput-label 
            (convert-values-for-tex atomic-value))
         (format stream "\\ \\ \\myvaluebold{~(~A~)}" 
            (convert-values-for-tex atomic-value)))
         (setf unoutput-label nil))
         ;; \attvaltyp{label}{value}\\
      :shrunk-fn
      #'(lambda (type) 
         (if unoutput-label             
         (format stream "\\\\~%~VT\\attvalshrunktyp{~A}{~(~A~)}" 
           indentation unoutput-label type)
        (format stream "\\ \\ \\boxvaluebold{~(~A~)}" 
           type))
        (setf unoutput-label nil))
         ;; \attvalshrunktyp{label}{value}\\     
      :start-fs
      #'(lambda (type depth labels)
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
               (push 1 bracket-stack)
               )
            (unoutput-label 
               (format stream 
                  "\\\\~%~VT\\attvaltyp{~A}{~(~A~)}" 
                  indentation unoutput-label type)
               (setf unoutput-label nil)
               (push 0 bracket-stack))
            (t (format stream "~VT\\ \\ \\myvaluebold{~(~A~)}" 
                  indentation type)
               (push 0 bracket-stack))) 
      (setf (aref indentation-vector depth) (+ indentation 2)))
      :label-fn
      #'(lambda (label depth)
         (setf indentation (aref indentation-vector depth))
         (setf indentation (+ indentation 7))
         (setf unoutput-label label))
      :end-fs 
      #'(lambda (terminal)
          (declare (ignore terminal))
         (dotimes (x (pop bracket-stack))
         (format stream "}")))
         ))))


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

;;; Output paths in notation defined for types etc

(defun def-path-print-operations (stream)
   (let ((type-label-list nil)
         (reentrant-vector (make-array '(3000))))
    ;;; keep the current path
    ;;; also keep the path to each reentrant node
  (setf *display-structure*
   (make-fs-output 
      :name 'path
      :error-fn #'(lambda (dag-instance)
         (format stream
            "~%::: ~A is not a dag...~%"
            dag-instance))
      :start-fn #'(lambda nil (format stream "~V%" 1))   
      :end-fn #'(lambda nil (format stream ".~V%" 1))
      :reentrant-value-fn                 
      #'(lambda (reentrant-pointer)
         (setf (aref reentrant-vector reentrant-pointer)
            type-label-list))
      :reentrant-fn
      #'(lambda (reentrant-pointer)
          (output-typed-list stream type-label-list)
          (format stream " = ")
          (output-typed-list stream 
             (aref reentrant-vector reentrant-pointer))
          (pop type-label-list)
          (format stream "~%"))
      :atomic-fn
      #'(lambda (atomic-value) 
         (output-typed-list stream type-label-list)             
         (format stream " = ~A~%" atomic-value)
         (pop type-label-list))
      :start-fs
      #'(lambda (type depth labels)
          (declare (ignore depth labels))
         (push type type-label-list))
      :label-fn
      #'(lambda (label depth)
         (declare (ignore depth))
         (push label type-label-list))
      :end-fs 
      #'(lambda (terminal)
         (when terminal
            (output-typed-list stream (cdr type-label-list))
            (format stream " = ~A~%" (car type-label-list)))
         (pop type-label-list)
         (pop type-label-list))))))

(defun output-typed-list (stream type-label-list)
   (let ((ordered-list (reverse type-label-list)))
      (format stream "< ~A ~A " (car ordered-list)
         (cadr ordered-list))
      (when (cddr ordered-list)
         (format stream "~{: ~A ~A ~}" 
            (cddr ordered-list)))
      (format stream ">")))

(defun def-path2-print-operations (stream)
   (let ((type-label-list nil)
         (reentrant-vector (make-array '(3000))))
    ;;; keep the current path
    ;;; also keep the path to each reentrant node
  (setf *display-structure*
   (make-fs-output 
      :name 'path2
      :error-fn #'(lambda (dag-instance)
         (format stream
            "~%::: ~A is not a dag...~%"
            dag-instance))
      :start-fn #'(lambda nil nil)   
      :end-fn #'(lambda nil (format stream ".~V%" 1))
      :reentrant-value-fn                 
      #'(lambda (reentrant-pointer)
         (setf (aref reentrant-vector reentrant-pointer)
            type-label-list))
      :reentrant-fn
      #'(lambda (reentrant-pointer)          
          (format stream "~%")
          (output-typed-list2 stream type-label-list)
          (format stream " = ")
          (output-typed-list2 stream 
             (aref reentrant-vector reentrant-pointer))
          (pop type-label-list))
      :atomic-fn
      #'(lambda (atomic-value) 
         (format stream "~%")
         (output-typed-list2 stream type-label-list)             
         (format stream " = ~(~A~)" (if (and (listp atomic-value) 
                                             (null (cdr atomic-value)))
                                      (car atomic-value) atomic-value))
         (pop type-label-list))
      :start-fs
      #'(lambda (type depth labels)
          (declare (ignore depth labels))
         (push type type-label-list))
      :label-fn
      #'(lambda (label depth)
         (declare (ignore depth))
         (push label type-label-list))
      :end-fs 
      #'(lambda (terminal)
         (when terminal
            (format stream "~%")
            (output-typed-list2 stream (cdr type-label-list))
            (format stream " = ~(~A~)" (car type-label-list)))
         (pop type-label-list)
         (pop type-label-list))))))

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


;;; Output shrunkenness 
;;; eg the way a feature structure is displayed is defined by
;;; specifying a series of paths - with the interpretation that the
;;; value of the display slot in the dag structure is to be set to
;;; :shrunk

(defun def-shrunk-print-operations (stream)
   (let ((label-list nil)
         (shrunk-list nil))
    ;;; keep the current path
  (setf *display-structure*
   (make-fs-output 
      :name 'shrunk
      :error-fn #'(lambda (dag-instance)
         (format stream
            "~%::: ~A is not a dag...~%"
            dag-instance))
      :start-fn #'(lambda nil nil)   
      :end-fn #'(lambda nil nil)
      :reentrant-value-fn                 
      #'(lambda (reentrant-pointer)
          (declare (ignore reentrant-pointer))
         nil)
      :reentrant-fn
      #'(lambda (reentrant-pointer)
          (declare (ignore reentrant-pointer))
          (pop label-list))
      :atomic-fn
      #'(lambda (atomic-value) 
          (declare (ignore atomic-value))
         (pop label-list))
      :shrunk-fn
      #'(lambda (type)
          (declare (ignore type))
         (let ((ordered-list (reverse label-list)))
            (push ordered-list shrunk-list)
            (pop label-list)))
      :start-fs
      #'(lambda (type depth labels)
          (declare (ignore type depth labels))
         nil)
      :label-fn
      #'(lambda (label depth)
          (declare (ignore depth))          
         (push label label-list))
      :max-width-fn 
      ;;; What a hack - just to get a value returned!
      #'(lambda nil
         shrunk-list)
      :end-fs 
      #'(lambda (terminal)
          (declare (ignore terminal))
         (pop label-list))))))


;;; Output tails in TDFSs as paths with no top

(defun def-tail-print-operations (indentation stream)
   (let ((type-label-list nil)
         (reentrant-vector (make-array '(3000)))
         (max-width 0))
    ;;; keep the current path
    ;;; also keep the path to each reentrant node
  (setf *display-structure*
   (make-fs-output 
      :name 'tail
      :error-fn #'(lambda (dag-instance)
         (format stream
            "~%::: ~A is not a dag...~%"
            dag-instance))
      :start-fn #'(lambda nil nil)   
      :end-fn #'(lambda nil nil)
      :reentrant-value-fn                 
      #'(lambda (reentrant-pointer)
         (setf (aref reentrant-vector reentrant-pointer)
            type-label-list))
      :reentrant-fn
      #'(lambda (reentrant-pointer)
          (move-to-x-y stream indentation (current-position-y stream))
          (output-tail-features stream type-label-list)
          (format stream " = ")
          (output-tail-features stream 
             (aref reentrant-vector reentrant-pointer))
          (pop type-label-list))
;          (format stream "~%"))
      :atomic-fn
      #'(lambda (atomic-value) 
         (let ((start-pos (current-position stream))
               (val
                  (if (cdr atomic-value) atomic-value 
                     (car atomic-value))))
            (move-to-x-y stream indentation (current-position-y stream))
            (output-tail-features stream type-label-list)
            (add-type-and-active-fs-region stream start-pos 
                                              type-label-list val nil t)      
            (setf max-width (max (current-position-x stream) max-width))
            (pop type-label-list)))
      :start-fs
      #'(lambda (type depth labels)
          (declare (ignore depth labels))
         (push type type-label-list))
      :label-fn
      #'(lambda (label depth)
          (declare (ignore depth))
         (push label type-label-list))
      :end-fs 
      #'(lambda (terminal)
         (when terminal
            (let ((start-pos (current-position stream)))
               (move-to-x-y stream indentation (current-position-y stream))
               (output-tail-features stream (cdr type-label-list))
               (add-type-and-active-fs-region stream start-pos type-label-list
                                                 (car type-label-list) nil t)))
         (setf max-width (max (current-position-x stream) max-width))
         (pop type-label-list)
         (pop type-label-list))))))


(defun output-tail-features (stream type-label-list)
   (if type-label-list
      (let ((ordered-list (reverse type-label-list)))
         (if (eql (car ordered-list) *toptype*)
            (format stream "< ~A " 
               (cadr ordered-list))
            (format stream "< ~A ~A " (car ordered-list)
               (cadr ordered-list)))
         (when (cddr ordered-list)
            (let ((remainder (cddr ordered-list)))
               (loop 
                  (unless remainder (return))
                  (if (eql (car remainder) *toptype*)
                     (format stream ": ~A " (cadr remainder))
                     (format stream ": ~A ~A " (car remainder)
                        (cadr remainder)))
                  (setf remainder (cddr remainder)))))
         (format stream ">"))
      (format stream "<>")))

;;;
;;; ************* Shrinking paths in types **********
;;; 

;;; minor mods to the following two functions for TDFSs

(def-lkb-parameter *shrunk-types* nil)
(defvar *shrunk-local-dags* nil)
(defvar *not-shrunk-local-dags* nil)

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


(defun display-dag1 (dag-instance device stream &optional x-pos)   
   (def-print-operations device (or x-pos 0) stream)
       (cond ((dag-p dag-instance)
              (invalidate-visit-marks)
              (mark-dag-for-output dag-instance)
              (setf *reentrancy-pointer* 0)
              (funcall (fs-output-start-fn *display-structure*))
              (print-dag dag-instance 0 nil)
              (funcall (fs-output-end-fn *display-structure*))              
              (funcall (fs-output-max-width-fn *display-structure*)))
             (t (funcall (fs-output-error-fn *display-structure*) 
                   dag-instance))))

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
   (let* ((real-dag (follow-pointers dag-instance))
          (flag-value (dag-visit real-dag))
          (new-rpath (cons (type-of-fs dag-instance) rpath)))
      (declare (dynamic-extent new-rpath))
      (cond 
         ((equal flag-value 'double)
            (setf (dag-visit real-dag)
               *reentrancy-pointer*)
            (incf *reentrancy-pointer*)
            (funcall (fs-output-reentrant-value-fn *display-structure*) 
               (dag-visit real-dag))                  
            (print-dag-aux real-dag depth new-rpath))
         ((equal flag-value 'single)
            (print-dag-aux real-dag depth new-rpath))
         (t (funcall (fs-output-reentrant-fn *display-structure*) 
               flag-value))))) 
  
(defun print-dag-aux (real-dag depth rpath)
   (cond 
      ((is-atomic real-dag) 
         (funcall (fs-output-atomic-fn *display-structure*)
            (type-of-fs real-dag)))
      ((and
          ;; shrink it if it is locally specified as shrunk, or it's globally
          ;; specified and not overriden locally
          (or (member real-dag *shrunk-local-dags* :test #'eq)
              (and (find rpath *shrunk-types* :test #'print-dag-shrunk-match-p)
                 (not (member real-dag *not-shrunk-local-dags* :test #'eq))))
          (fs-output-shrunk-fn *display-structure*))
         (funcall (fs-output-shrunk-fn *display-structure*)
            (dag-type real-dag)))
      (t 
         (let* 
            ((type (type-of-fs real-dag))
             (labels (top-level-features-of real-dag))
             (label-list (if labels (canonical-order type labels))))
            (if labels
               (progn
                  (funcall (fs-output-start-fs *display-structure*) 
                     type depth labels)
                  (for label in label-list
                     do
                     (funcall (fs-output-label-fn *display-structure*) 
                        label depth)
                     (let ((new-rpath (cons label rpath)))
                        (declare (dynamic-extent new-rpath))
                        (print-dag (get-dag-value real-dag label) (+ 1 depth) new-rpath)))
                  (funcall (fs-output-end-fs *display-structure*)
                     (null labels)))
               (funcall (fs-output-atomic-fn *display-structure*)
                  (list type)))))))

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

;;; not sure whether the following is being used any more

(defun convert-dag-to-paths (dag-instance)   
   (cond 
      ((dag-p dag-instance)
         (invalidate-visit-marks)
         (mark-dag-for-output dag-instance)
         (convert-dag-to-paths1 dag-instance nil))
      (t (error "~A is not a dag" dag-instance))))


;;; Output a dag as a list of unifications etc

(defun convert-dag-to-paths1 (dag-instance path-so-far)
   (let* ((real-dag (follow-pointers dag-instance))
         (flag-value (dag-visit real-dag)))
      (cond 
         ((equal flag-value 'double)
            (setf (dag-visit real-dag)
               path-so-far)                             
            (convert-dag-to-paths2 real-dag path-so-far))
         ((equal flag-value 'single)
            (convert-dag-to-paths2 real-dag path-so-far))
         (t (list (make-unification :lhs (construct-path path-so-far)
               :rhs (construct-path flag-value)))))))

(defun convert-dag-to-paths2 (real-dag path-so-far)
   (cond 
      ((is-atomic real-dag) 
         (list (make-unification :lhs (construct-path path-so-far)
               :rhs (make-u-value :types
                  (type-of-fs real-dag)))))
      (t
         (let 
            ((labels (top-level-features-of real-dag)))
            (cond (labels
                  (push
                     (dag-type real-dag) path-so-far)
                  (for label in labels
                     append
                     (convert-dag-to-paths1 (get-dag-value real-dag label)
                        (cons label path-so-far))))
               (t (list (make-unification :lhs 
                        (construct-path path-so-far)
                        :rhs (make-u-value :types
                           (list 
                           (dag-type real-dag)))))))))))

(defun construct-path (path)
   ;;; takes a reversed list of alternating features and types 
   ;;; and converts it into a valid path structure
   ;;; eg (f2 t2 f1 t1) into the structure
   ;;; equivalent to <t1 f1 : t2 f2>
   (let ((typed-feature-list nil))
      (loop (when (null path) (return))
         (let* ((feature (pop path))
               (type (pop path)))
            (push (make-type-feature-pair :type type :feature feature)
               typed-feature-list)))
      (make-typed-path :typed-feature-list typed-feature-list)))



