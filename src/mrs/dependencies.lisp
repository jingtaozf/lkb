(in-package :mrs)

(defparameter %mrs-psoa% nil)

(defparameter %mrs-variable-counter% 0)

(defparameter %mrs-symbol-table% nil)

(defparameter %mrs-relevant-features% 
  '("ARG" "ARG1" "ARG2" "ARG3" "ARG4" "BV" "SOA" "NAMED" "CONST_VALUE"))

(defun mrs-output-psoa (psoa &key (stream t))
  (let* ((index (psoa-index psoa))
         (name (when (var-p index) (var-name index))))
    (format stream "{~@[~a~]:~%" name))
  (loop
      with %mrs-psoa% = psoa
      with %mrs-symbol-table% = (make-hash-table)
      with %mrs-variable-counter% = 0
      for relation in (psoa-liszt psoa)
      do
        (mrs-output-relation relation :stream stream))
  (format stream "}~%"))

(defun mrs-output-relation (relation &key (stream t))
  (when (rel-p relation)
    (loop
        with id = (mrs-find-identifier relation)
        with name = (symbol-name (rel-reltype relation))
        with flist = (rel-flist relation)
        with output = nil
        for feature in %mrs-relevant-features%
        for key = (vsym feature)
        for value = (loop
                        for fvpair in flist
                        thereis 
                          (when (eq (fvpair-feature fvpair) key)
                            (fvpair-value fvpair)))
        for representative = (and value (mrs-find-representative value))
        when representative do (push (cons feature representative) output)
        finally
          (when output
            (format stream "  ~a:~(~a~)[" id name)
            (loop
                with output = (nreverse output)
                for foo in output
                for feature = (first foo)
                for representative = (rest foo)
                for id = (first representative)
                for value = (rest representative)
                do
                  (format 
                   stream 
                   "~:[, ~;~]~:@(~a~) ~a:~(~a~)"
                   (eq foo (first output)) feature id value))
            (format stream "]~%")))))

(defun mrs-find-identifier (relation)
  (let* ((flist (and (rel-p relation) (rel-flist relation)))
         instance event)
    (loop
        for fvpair in flist
        when (eq (fvpair-feature fvpair) (vsym "EVENT"))
        do (setf event (fvpair-value fvpair))
        when (eq (fvpair-feature fvpair) (vsym "INST"))
        do (setf instance (fvpair-value fvpair)))
    (or
     (and instance (var-p instance) (var-name instance))
     (and event (var-p event) (var-name event))
     (setf (gethash relation %mrs-symbol-table%) 
       (format nil "v~d" (incf %mrs-variable-counter%))))))

(defun mrs-find-representative (value)
  (cond
   ((handle-var-p value)
    (loop
        with name = (handle-var-name value)
        with qeq = (mrs-hcons-qeq name)
        for relation in (psoa-liszt %mrs-psoa%)
        for handle = (handle-var-name (rel-handel relation))
        thereis
          (when (or (equal name handle) (equal qeq handle)) 
            (cons handle (rel-reltype relation)))))
   ((var-p value)
    (loop
        with name = (var-name value)
        for relation in (psoa-liszt %mrs-psoa%)
        for id = (mrs-find-identifier relation)
        thereis
          (when (equal name id) 
            (cons id (rel-reltype relation)))))
   ((stringp value)
    (cons "" value))))

(defun mrs-hcons-qeq (handle)
  (loop
      with name = (if (stringp handle) handle (when (handle-var-p handle)
                                                (handle-var-name handle)))
      for hcons in (psoa-h-cons %mrs-psoa%)
      for scarg = (when (eq (hcons-relation hcons) (vsym "QEQ"))
                    (handle-var-name (hcons-scarg hcons)))
      thereis 
        (when (equal name scarg) 
          (handle-var-name (hcons-outscpd hcons)))))
