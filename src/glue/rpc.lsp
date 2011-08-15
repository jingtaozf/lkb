(in-package :lkb)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (require :process)
  (require :aserve)
  (require :xml-rpc)
  (set (intern "*DEFAULT-ASERVE-EXTERNAL-FORMAT*" :net.aserve)
    (excl:crlf-base-ef :utf-8)))

(defparameter *rpc-port* 4713)

(defparameter *rpc-debug-p* nil)

(defparameter *rpc-server* nil)

(defparameter %rpc-object-counter% 0)

(defparameter %rpc-attic% (make-array 512))

(defparameter %rpc-free-list% nil)

(defstruct rpcc
  id edges)

(defun rpc-shutdown ()
  (when *rpc-server*
    (net.xml-rpc:disable-xml-rpc-server *rpc-server*)
    (setf *rpc-server* nil)
    (net.aserve:shutdown)))

(defun rpc-initialize (&key (port *rpc-port*))
  (when *rpc-server* (rpc-shutdown))
  (setf %rpc-object-counter% 0)
  (setf %rpc-attic% (make-array 512))
  (setf %rpc-free-list% nil)
  (setf *rpc-server*
    (net.xml-rpc:make-xml-rpc-server
     :name "LKB" :start nil :publish '(:path "/") :enable t :introspect t))
  ;;
  ;; _fix_me_
  ;; add chart.root() and grammar.instantiate-lexical-type(), though maybe
  ;; leave the latter undocumented.                             (27-oct-10; oe)
  ;;
  (net.xml-rpc:export-xml-rpc-method
   *rpc-server*
   '("grammar.instantiate-lexical-entry" rpc-grammar-instantiate-lexical-entry)
   :int :int :string :string)
  (net.xml-rpc:export-xml-rpc-method
   *rpc-server*
   '("grammar.instantiate-lexical-entry" rpc-grammar-instantiate-lexical-entry)
   :int :int :string :string :array)
  (net.xml-rpc:export-xml-rpc-method
   *rpc-server*
   '("grammar.instantiate-rule" rpc-grammar-instantiate-rule)
   :int :int :string)
  (net.xml-rpc:export-xml-rpc-method
   *rpc-server*
   '("grammar.instantiate-type" rpc-grammar-instantiate-type)
   :int :int :string)
  (net.xml-rpc:export-xml-rpc-method
   *rpc-server*
   '("chart.create" rpc-chart-create)
   :int)
  (net.xml-rpc:export-xml-rpc-method
   *rpc-server*
   '("chart.release" rpc-chart-release)
   :int :int)
  (net.xml-rpc:export-xml-rpc-method
   *rpc-server*
   '("chart.combine" rpc-chart-combine)
   :int :int :int :int :int)
  (net.xml-rpc:export-xml-rpc-method
   *rpc-server*
   '("chart.root" rpc-chart-root)
   :int :int :int :string)
  (net.aserve:start :port port :external-format (excl:crlf-base-ef :utf-8))
  (net.xml-rpc:enable-xml-rpc-server *rpc-server*)
  (setf (sys:gsgc-parameter :auto-step) nil)
  (excl:gc :tenure))

(defun rpc-grammar-instantiate-lexical-entry (id form instance
                                              &optional tokens)
  (let* ((*package* *lkb-package*)
         (*edge-registry* nil)
         (chart (rpc-retrieve-object nil id))
         (name (intern (string-upcase instance) *lkb-package*))
         (instance (ignore-errors (get-lex-entry-from-id name)))
         (result (when instance
                   (copy-tdfs-completely (lex-entry-full-fs instance)))))
    (when (or (null chart) (not (rpcc-p chart)))
      (when *rpc-debug-p*
        (format
         *error-output*
         "rpc-instantiate-lexical-entry(): invalid chart handle #~a.~%"
         id))
      (return-from rpc-grammar-instantiate-lexical-entry -1))
    (when (null instance)
      (when *rpc-debug-p*
        (format
         *error-output*
         "rpc-instantiate-lexical-entry(): invalid instance `~(~a~)'.~%"
         name))
      (return-from rpc-grammar-instantiate-lexical-entry -2))
    (compress-dag (tdfs-indef (lex-entry-full-fs instance)))
    (forget-psort *lexicon* (lex-entry-id instance))
    (when (and tokens *lexicon-tokens-path* *lexicon-last-token-path*)
      (with-unification-context (foo)
        (loop
            for token in tokens
            for path = *lexicon-tokens-path*
            then (append path *list-tail*)
            for i from 0
            while result do
              (setf result (yadu! result token (append path *list-head*)))
            finally
              (when result
                (let ((token (first (last tokens))))
                  (setf result
                    (yadu! result token *lexicon-last-token-path*)))
                (when result
                  (setf result (copy-tdfs-elements result))))))
      (when (null result)
        (when *rpc-debug-p*
          (format
           *error-output*
           "rpc-instantiate-lexical-entry(): ~
            tokens unification failure in `~(~a~)'.~%"
           name))
        (return-from rpc-grammar-instantiate-lexical-entry -2)))
    (let* ((ids (list (lex-entry-id instance)))
           (edge (make-edge
                  :dag result :category (indef-type-of-tdfs result)
                  :rule form :leaves (list form) :lex-ids ids))
           (id (rpc-store-object (rpcc-id chart) edge)))
      (setf (edge-id edge) id)
      (push edge (rpcc-edges chart))
      id)))

(defun rpc-grammar-instantiate-rule (id instance)
  (let* ((*package* *lkb-package*)
         (*edge-registry* nil)
         (chart (rpc-retrieve-object nil id))
         (name (intern (string-upcase instance) *lkb-package*))
         (instance 
          (ignore-errors
           (or (get-lex-rule-entry name) (get-grammar-rule-entry name)))))
    (when (or (null chart) (not (rpcc-p chart)))
      (when *rpc-debug-p*
        (format
         *error-output*
         "rpc-instantiate-lexical-entry(): invalid chart handle #~a.~%"
         id))
      (return-from rpc-grammar-instantiate-rule -1))
    (when (null instance)
      (when *rpc-debug-p*
        (format
         *error-output*
         "rpc-instantiate-rule(): invalid instance `~(~a~)'.~%"
         name))
      (return-from rpc-grammar-instantiate-rule -2))
    (let* ((result (copy-tdfs-completely (rule-full-fs instance)))
           (edge (make-edge 
                  :dag result :category (indef-type-of-tdfs result)
                  :rule instance :leaves nil :lex-ids nil :children nil))
           (id (rpc-store-object (rpcc-id chart) edge)))
      (setf (edge-id edge) id)
      (push edge (rpcc-edges chart))
      (compress-dag (tdfs-indef (rule-full-fs instance)))
      id)))

(defun rpc-grammar-instantiate-type (id instance)
  (let* ((*package* *lkb-package*)
         (*edge-registry* nil)
         (chart (rpc-retrieve-object nil id))
         (name (intern (string-upcase instance) *lkb-package*))
         (instance (ignore-errors
                    (eval-possible-leaf-type *leaf-types* name)
                    (get-type-entry name)))
         (result (when instance (copy-tdfs-completely (ltype-tdfs instance)))))
    (when (or (null chart) (not (rpcc-p chart)))
      (when *rpc-debug-p*
        (format
         *error-output*
         "rpc-instantiate-type(): invalid chart handle #~a.~%"
         id))
      (return-from rpc-grammar-instantiate-type -1))
    (when (null instance)
      (when *rpc-debug-p*
        (format
         *error-output*
         "rpc-instantiate-type(): invalid instance `~(~a~)'.~%"
         name))
      (return-from rpc-grammar-instantiate-type -2))
    (let* ((edge (make-edge
                  :dag result :category (indef-type-of-tdfs result)
                  :rule name :leaves (list name)))
           (id (rpc-store-object (rpcc-id chart) edge)))
      (setf (edge-id edge) id)
      (push edge (rpcc-edges chart))
      id)))

(defun rpc-chart-create ()
  (let* ((*package* *lkb-package*)
         (chart (make-rpcc))
         (id (rpc-store-object nil chart)))
    (setf (rpcc-id chart) id)
    id))

(defun rpc-chart-release (id)
  (let* ((*package* *lkb-package*)
         (chart (rpc-retrieve-object nil id)))
    (when (or (null chart) (not (rpcc-p chart)))
      (when *rpc-debug-p*
        (format
         *error-output*
         "rpc-chart-release(): invalid chart handle #~a.~%"
         id))
      (return-from rpc-chart-release -1))
    (loop
        for edge in (rpcc-edges chart)
        do (rpc-forget-object id (edge-id edge)))
    (rpc-forget-object nil id)
    (purge-edge-registry)
    (invalidate-marks)
    (invalidate-visit-marks)
    #+:pooling
    (reset-pools :compressp t)
    (excl:gc))
  0)

(defun rpc-chart-combine (id mother child index)
  (let* ((*package* *lkb-package*)
         (*edge-registry* nil)
         (chart (rpc-retrieve-object nil id)))
    (when (or (null chart) (not (rpcc-p chart)))
      (when *rpc-debug-p*
        (format
         *error-output*
         "rpc-chart-combine(): invalid chart handle #~a.~%"
         id))
      (return-from rpc-chart-combine -1))
    (let* ((id (rpcc-id chart))
           (medge (rpc-retrieve-object id mother))
           (cedge (rpc-retrieve-object id child)))
      (when (null medge)
        (when *rpc-debug-p*
          (format
           *error-output*
           "rpc-instantiate-rule(): invalid mother edge handle #~a.~%"
           mother))
        (return-from rpc-chart-combine -2))
      (when (null cedge)
        (when *rpc-debug-p*
          (format
           *error-output*
           "rpc-instantiate-rule(): invalid child edge handle #~a.~%"
           child))
        (return-from rpc-chart-combine -3))
      ;;
      ;; _fix_me_
      ;; should also check .index. against arity of the rule.  (26-oct-10; oe)
      ;;
      (with-unification-context (ignore)
        (let* ((path (loop for i from 1 to index append *list-tail*))
               (path (append *args-path* path *list-head*))
               (mtdfs (edge-dag medge))
               (ctdfs (edge-dag cedge))
               (result (yadu! mtdfs ctdfs path))
               (*deleted-daughter-features* nil)
               (result (and result (copy-tdfs-elements result)))
               (edge (when result
                       (make-edge
                        :dag result :category (indef-type-of-tdfs result)
                        :rule (edge-rule medge)
                        :children (cons cedge (edge-children medge)))))
               (id (if edge (rpc-store-object(rpcc-id chart) edge) 0)))
          (when edge
            (setf (edge-id edge) id)
            (push edge (rpcc-edges chart)))
          id)))))

(defun rpc-chart-root (id edge instance)
  (let* ((*package* *lkb-package*)
         (chart (rpc-retrieve-object nil id)))
    (when (or (null chart) (not (rpcc-p chart)))
      (when *rpc-debug-p*
        (format
         *error-output*
         "rpc-chart-root(): invalid chart handle #~a.~%"
         id))
      (return-from rpc-chart-root -1))
    (let* ((id (rpcc-id chart))
           (name (intern (string-upcase instance) *lkb-package*))
           (root (get-tdfs-given-id name))
           (foo (rpc-retrieve-object id edge)))
      (when (null foo)
        (when *rpc-debug-p*
          (format
           *error-output*
           "rpc-chart-root(): invalid edge handle #~a.~%"
           edge))
        (return-from rpc-chart-root -2))
      (when (null root)
        (when *rpc-debug-p*
          (format
           *error-output*
           "rpc-chart-root(): invalid instance `~(~a~)'.~%"
           name))
        (return-from rpc-chart-root -3))
      (setf edge foo)
      (let ((tdfs (edge-dag edge)))
        (if (and (yaduablep root tdfs)
                 (or (null *additional-root-condition*)
                     (funcall *additional-root-condition* tdfs)))
          1
          0)))))

(let ((lock (mp:make-process-lock)))
  (defun rpc-store-object (id object &key globalp)
    (mp:with-process-lock (lock)
      (let ((n (or (pop %rpc-free-list%) (incf %rpc-object-counter%))))
        (when (>= %rpc-object-counter% (array-total-size %rpc-attic%))
          (setf %rpc-attic%
            (adjust-array %rpc-attic% (* %rpc-object-counter% 2))))
        (setf (aref %rpc-attic% n) (cons (if globalp -1 id) object))
        n)))

  (defun rpc-retrieve-object (id n)
    (mp:with-process-lock (lock)
      (when (and (numberp n) (>= n 0) (< n (array-total-size %rpc-attic%)))
        (let ((bucket (aref %rpc-attic% n)))
          (when (or (equal (first bucket) -1) (equal (first bucket) id))
            (rest bucket))))))

  (defun rpc-forget-object (id n)
    (mp:with-process-lock (lock)
      (when (and (numberp n) (>= n 0) (< n (array-total-size %rpc-attic%)))
        (let ((bucket (aref %rpc-attic% n)))
          (when (or (equal (first bucket) -1) (equal (first bucket) id))
            (setf (aref %rpc-attic% n) nil)
            (push n %rpc-free-list%)))))))

