(in-package :mrs)

(defparameter %mrs-psoa% nil)

(defparameter %mrs-variable-counter% 0)

(defparameter %mrs-symbol-table% nil)

(defparameter %mrs-representatives-table% nil)

(defparameter %mrs-relevant-features% 
  '("ARG" "ARG0" "ARG1" "ARG2" "ARG3" "ARG4" "BV" "SOA" "NAMED" 
    "CVAL" "CONST_VALUE"
    "L-INDEX" "R-INDEX" "L-HANDEL" "R-HANDEL" "MAIN" "SUBORD" "ROLE"
    "HINST" "NHINST"))

(defun mrs-output-psoa (psoa &key (stream t))
  (if (psoa-p psoa)
    (let* ((index (psoa-index psoa))
           (name (when (var-p index) (var-name index))))
      (format stream "{~@[~a~]:~%" name)
      (loop
          with %mrs-psoa% = psoa
          with %mrs-symbol-table% = (make-hash-table)
          with %mrs-representatives-table% = (make-hash-table :test #'equal)
          with %mrs-variable-counter% = 0
          for relation in (psoa-liszt psoa)
          do
            (mrs-output-relation relation :stream stream))
      (format stream "}~%"))
    (format stream "{}~%")))

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
  (or (gethash relation %mrs-symbol-table%)
    (let* ((flist (and (rel-p relation) (rel-flist relation)))
           instance event)
      (loop
          for fvpair in flist
          for feature = (fvpair-feature fvpair)
          when (eq feature (vsym "EVENT"))
          do (setf event (fvpair-value fvpair))
          when (or (eq feature (vsym "INST")) 
                   (eq feature (vsym "ARG0"))
                   (eq feature (vsym "C-ARG")))
          do (setf instance (fvpair-value fvpair)))
      (let ((name (or
                   (and instance (var-p instance) (var-name instance))
                   (and event (var-p event) (var-name event))
                   (format nil "_~d" (incf %mrs-variable-counter%)))))
        (setf (gethash relation %mrs-symbol-table%) name)))))

(defun mrs-find-representative (value)
  (cond
   ((handle-var-p value)
    (loop
        with name = (handle-var-name value)
        with qeq = (mrs-hcons-qeq name)
        for relation in (psoa-liszt %mrs-psoa%)
        for handle = (handle-var-name (rel-handel relation))
        when (or (equal name handle) (equal qeq handle))
        collect (cons (mrs-find-identifier relation) (rel-reltype relation))
        into candidates
        finally 
          (return
            (if (= (length candidates) 1)
              (first candidates)
              (or (loop
                      for candidate in candidates
                      for identifier = (first candidate)
                      thereis
                        (when (not (eql (char identifier 0) #\_)) candidate))
                  (first candidates))))))
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
