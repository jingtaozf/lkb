;;; -*- Mode: COMMON-LISP; Syntax: Common-Lisp; Package: TSDB -*-

(in-package :tsdb)

(defparameter *tagger-left-context* 2)

(defparameter *tagger-right-context* 2)

(defparameter *tagger-prefix-size* 4)

(defparameter *tagger-suffix-size* 4)

(defparameter *tagger-options*
  '(*tagger-left-context*
    *tagger-right-context*
    *tagger-prefix-size*
    *tagger-suffix-size*))

(defparameter *tagger-lexicon* nil)

(defparameter *tagger-debug-p* t)

(defstruct lexicon
  (forms (make-hash-table :test #'equal))
  (suffixes (make-hash-table :test #'equal))
  (prefixes (make-hash-table :test #'equal)))

(defun record-form (form tag lexicon)
  (let ((match (gethash form (lexicon-forms lexicon))))
    (if match
      (loop
          for foo in match
          when (equal (first foo) tag) 
          do (incf (rest foo)) (return)
          finally (nconc match (list (cons tag 1))))
      (setf (gethash form (lexicon-forms lexicon))
        (list (cons tag 1)))))
  (loop
      for i from 1 to (min *tagger-prefix-size* (length form))
      for prefix = (subseq form 0 i)
      do
        (let ((match (gethash prefix (lexicon-prefixes lexicon))))
          (if match
            (loop
                for foo in match
                when (equal (first foo) tag) 
                do (incf (rest foo)) (return)
                finally (nconc match (list (cons tag 1))))
            (setf (gethash prefix (lexicon-prefixes lexicon))
              (list (cons tag 1))))))
  (loop
      with n = (length form)
      for i from (max 0 (- n *tagger-suffix-size*)) to (- n 1)
      for suffix = (subseq form i)
      do
        (let ((match (gethash suffix (lexicon-suffixes lexicon))))
          (if match
            (loop
                for foo in match
                when (equal (first foo) tag) 
                do (incf (rest foo)) (return)
                finally (nconc match (list (cons tag 1))))
            (setf (gethash suffix (lexicon-suffixes lexicon))
              (list (cons tag 1)))))))

(defun print-lexicon (lexicon &key (stream t))
  (format stream "forms:~%")
  (loop
      for form being each hash-key in (lexicon-forms lexicon)
      do
        (format stream "  ~a:" form)
        (loop
            for (tag . n) in (gethash form (lexicon-forms lexicon))
            do (format stream " ~a [~a]" tag n))
        (format stream "~%"))
  (format stream "suffixes:~%")
  (loop
      for form being each hash-key in (lexicon-suffixes lexicon)
      do
        (format stream "  ~a:" form)
        (loop
            for (tag . n) in (gethash form (lexicon-suffixes lexicon))
            do (format stream " ~a [~a]" tag n))
        (format stream "~%")))

(defun estimate-tagger (items &key (stream *tsdb-io*) model (estimatep t))

  (loop
      with model = (or model 
                       (let ((model (make-mem)))
                         (initialize-mem model)
                         model))
      for item in items
      for iid = (get-field :i-id item)
      for i from 1
      for readings = (get-field :readings item)
      when (and (integerp readings) (> readings 0)) do
        (format 
         stream
         "~&[~a] estimate-tagger(): item # ~a (~a reading~p);~%"
         (current-time :long :short) iid readings readings)
        (loop
            with *reconstruct-cache* = (make-hash-table :test #'eql)
            with *reconstruct-cfg-separator* = nil
            for result in (get-field :results item)
            for rid = (get-field :result-id result)
            for derivation = (get-field :derivation result)
            for edge = (or (get-field :edge result)
                           (reconstruct derivation nil))
            when (null edge) do
              (format
               stream
               "[~a] estimate-tagger(): ignoring item # ~d (no edge for ~d)~%"
               (current-time :long :short) iid rid)
              (return)
            else do (tag-edge-to-contexts edge model))
      finally 
        (when estimatep
          (let* ((out (format nil "/tmp/.mem.~a.mew" (current-user)))
                 (command (format 
                           nil 
                           "estimate -events_in ~a -params_out ~a"
                           (mem-file model) out))
                 (output (if *tagger-debug-p* nil "/dev/null")))
            (force-output (mem-stream model))
            (close (mem-stream model))
            (setf (mem-stream model) nil)
            (when (probe-file out) (ignore-errors (delete-file out)))
            (when (and (zerop (run-process 
                               command :wait t 
                               :output output :if-output-exists :append))
                       (probe-file out))
              (read-weights model out))))
        (return model)))

(defun tag-edge-to-contexts (edge model)
  
  (loop
      with histories = nil
      with forms = (lkb::edge-leaves edge)
      with tags = (loop
                      for edge in (lkb::edge-children edge)
                      collect (lkb::edge-category edge))
      with n = (length forms)
      initially
        (if (and (eq (lkb::edge-foo edge) model)
                 (consp (lkb::edge-baz edge))
                 nil)
          (return (lkb::edge-baz edge))
          (when (not (= (length forms) (length tags)))
            (format
             t
             "tag-edge-to-histories(): mysterious forms vs. tags mismatch.~%")
            (return)))
      for i from 0 to (- n 1)
      for j = (max (- i *tagger-left-context*) 0)
      for k = (min (+ i 1 *tagger-right-context*) n)
      do (push (list (nth i forms) (nth i tags)
                     (subseq tags j (max i 0))
                     (subseq forms j (max i 0))
                     (subseq forms (min (+ i 1) n) k))
               histories)
      finally
        (loop
            with table = (mem-table model)
            for i from 0
            for (form tag tags left right) in (reverse histories)
            for context = (make-context)
            for event = (make-event :id i :frequency 1)
            do
              (setf left left)
              (setf right right)
              ;;
              ;; in the following, the `feature type' (leading integer) values
              ;; correspond to lines numbers of Table 1 in (Ratnaparkhi, 1996).
              ;;
              (let* ((code (symbol-to-code (list 0 tag form) table))
                     (feature (make-feature :code code)))
                (record-feature feature event model))
              (let* ((code 
                      (symbol-to-code (cons 6 (cons tag (last tags))) table))
                     (feature (make-feature :code code)))
                (record-feature feature event model))
              #+:null
              (let* ((code (symbol-to-code (cons 7 (cons tag tags)) table))
                     (feature (make-feature :code code)))
                (record-feature feature event model))
        (record-event event context)
        (record-context context model))
        (return (nreverse histories))))
                
(defun tag-score-edge (edge model &key recursivep)
  (if (and (not recursivep) 
           (eq (lkb::edge-foo edge) model) (numberp (lkb::edge-score edge)))
    (lkb::edge-score edge)
    0.0))

#+:null
(defun parse-item (string 
                   &key id exhaustive nanalyses trace
                        edges derivations semantix-hook trees-hook
                        burst (nresults 0))

  (declare (ignore id exhaustive nanalyses trace
                   edges derivations semantix-hook trees-hook
                   burst nresults))
  
  (multiple-value-bind (return condition)
    (ignore-errors
     (let (tgc tcpu treal conses symbols others derivation)
       (tsdb::time-a-funcall
        #'(lambda () 
            (let* ((tree (read-ptb-from-string string))
                   (leaves (extract-ptb-leaves tree)))
              (setf derivation
                (cons 
                 "?"
                 (loop
                     for leaf in leaves
                     unless (string-equal (first leaf) "-none-")
                     collect leaf)))))
        #'(lambda (tgcu tgcs tu ts tr scons ssym sother &rest ignore)
            (declare (ignore ignore))
            (setq tgc (+ tgcu tgcs) tcpu (+ tu ts) treal tr
                  conses (* scons 8) symbols (* ssym 24) others sother)))
       (let* ((*print-pretty* nil) (*print-level* nil) (*print-length* nil)
              (readings 1))
         `((:others . ,others) (:symbols . ,symbols) 
           (:conses . ,conses)
           (:treal . ,treal) (:tcpu . ,tcpu)
           (:tgc . ,tgc)
           (:readings . ,readings)
           (:results .
            ,(list
              (let ((string (with-standard-io-syntax
                              (write-to-string derivation))))
                (pairlis '(:result-id :derivation) (list 1 string)))))))))
  (append
   (when condition
       (let ((error (tsdb::normalize-string (format nil "~a" condition))))
         (pairlis '(:readings :condition :error)
                  (list -1 condition error))))
   return)))
