(in-package :common-lisp-user)

;;;
;;; attempt to add a template mechanism to LKB.  the idea goes as follows:
;;; input undergoes tokenization as a preprocessing stage; input tokens such
;;; as numbers, ideosyncratic date and time expressions (`10-feb-99 15:04 h'),
;;; room numbers, or mathematical formulae are preprocessed into a template
;;; call like (for ``if $a > b$ and $c > d$ then $a + c > b + d$.'')
;;;
;;; "if"
;;; (math :referents (*list* :first "a" :rest (*list* :first "b" :rest *null*))
;;;       :expression (term_formula :op > 
;;;                                 :lopnd (term_variable :opnd "a") 
;;;                                 :ropnd (term_variable :opnd "b")))
;;; "and"
;;; (math :referents (*list* :first "c" :rest (*list* :first "d" :rest *null*))
;;;       :expression (term_formula :op > 
;;;                                 :lopnd (term_variable :opnd "c") 
;;;                                 :ropnd (term_variable :opnd "d")))
;;; "then"
;;; (math :referents 
;;;       (*list* :first "a" 
;;;               :rest (*list* :first "b" 
;;;                             :rest (*list* :first "c" 
;;;                                           :rest (*list* :first "d" 
;;;                                                         :rest *null*))))
;;;       :expression
;;;       (term_formula :op > 
;;;                     :lopnd 
;;;                     (complex_term :op + 
;;;                                   :lopnd (term_variable :opnd "a") 
;;;                                   :ropnd (term_variable :opnd "c"))
;;;                     :ropnd 
;;;                     (complex_term :op + 
;;;                                   :lopnd (term_variable :opnd "b") 
;;;                                   :ropnd (term_variable :opnd "d")))))
;;; .
;;;
;;; where the first element (e.g. `math' is a type name that identifies the
;;; template and keywords like `:referents' are features appropriate for that
;;; type.  template instantiation recursively walks through the structure and
;;; returns a typed features structure corresponding to the input.
;;;
;;; to simplify the interface to morphological and lexical processing, 
;;; template calls in the input are replaced with placeholders (of the form
;;; `_math_1_') and only treated specially during lexical lookup.  then, the
;;; placeholder is used to retrieve the template instantiation which, in turn,
;;; is used to initialize the chart (i.e. in the place of a lexical entry).
;;;
;;; i can think of at least three issues that deserve improved treatment:
;;;
;;;   - validation of template call; error messages generated are often not
;;;     very informative;
;;;   - integration of preprocessing with regular input channel (some people
;;;     may want an external tokenizer --- like the lex(1)-and-yacc(1)-based
;;;     scanner used in the math domain --- others maybe a lisp function);
;;;   - provision to allow preprocessing to return ambiguous segmentations; the
;;;     current approach assumes that tokens are identified unambiguously and
;;;     never overlap.
;;;
;;;                                           (10-nov-98 -- oe@coli.uni-sb.de)
;;;

(defvar *template-bunker* (make-hash-table :test #'equal))

(defvar *template-id* 0)

(defun clear-template-bunker ()
  (clrhash *template-bunker*)
  (setf *template-id* 0))

(defun make-template-locum (template)
  (let ((locum 
         (format nil "~(_~a_~d_~)" (first template) (incf *template-id*))))
    (setf (gethash locum *template-bunker*) template)
    locum))

(defun template-p (token)
  (and (stringp token)
       (char= (schar token 0) #\_)
       (char= (schar token (- (length token) 1)) #\_)))

(defun retrieve-template (locum)
  (gethash locum *template-bunker*))

(defun get-template-surface (template)
  (let ((surface (getf (rest template) :surface)))
    (when surface
      (format nil "[~a]" surface))))
      
(defun instantiate-template (template)
  (let* ((unifications (instantiate-nested-template nil nil template))
         (unifications (when unifications (process-unifications unifications)))
         (wffs (when unifications (create-wffs unifications))))
    (when wffs
      (make-tdfs :indef wffs))))

(defun instantiate-nested-template (prefix feature template)
  (cond
   ((consp template)
    (let* ((type (first template))
           (path (when feature (append prefix (list feature))))
           (unification (make-unification 
                         :lhs (create-path-from-feature-list path)
                         :rhs (make-u-value :types (list type)))))
      (if (get-type-entry type)
        (cons unification
              (loop 
                  for avps = (rest template) then (rest (rest avps))
                  for feature = (intern (first avps))
                  for value = (second avps)
                  while (and feature value)
                  append 
                    (instantiate-nested-template path feature value)))
        (format
         t
         "instantiate-nested-template(): invalid type `~a'.~%" type))))
   (t
    (list (make-unification 
           :lhs (create-path-from-feature-list (append prefix (list feature)))
           :rhs (make-u-value :types (list template)))))))

(defparameter *tokenizer* "/bin/cat")

(defparameter *verbose-preprocesser-p* t)

(defparameter *result* nil)

(defun parse-with-preprocessor (&optional file)
  (let* ((file
          (or file
              (ask-user-for-existing-pathname "Input file?"))))
    (when file
      (close-existing-chart-windows)
      (if (probe-file file)
        (multiple-value-bind (stream foo pid)
            (run-process
             (format nil "exec ~a" *tokenizer*)
             :input file
             :output :stream
             :wait nil)
          (declare (ignore foo))
          (setf *result* nil)
          (let* ((result
                  (loop 
                      for form = (read stream nil nil)
                      while form
                      when (and (listp form) (not (keywordp (first form))))
                      do 
                        (parse (preprocess-form form))
                      and collect (extract-semantix)
                      and do 
                        (if *verbose-preprocesser-p*
                          (let ((readings (length *parse-record*)))
                            (format t "~&~s~%" form)
                            (format t "[found ~d reading~p.]~%"
                                  readings readings))
                          (when (zerop (length *parse-record*))
                            (format t "no analysis for:~%  ~s~%"
                                    form)))
                      else 
                      collect form)))
            (close stream)
            #+:allegro (sys:os-wait nil pid)
            (setf *result* result)))
        (format
         t
         "parse-with-preprocessor(): failed to open `~a'.~%" file)))))

(defun preprocess-form (form)
  (clear-template-bunker)
  (loop for a in form
      if (stringp a) collect (string-downcase a)
      else collect (make-template-locum a)))
