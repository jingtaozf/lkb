;;; Copyright (c) 1998-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions

(in-package :lkb)


;;;
;;; transfer-based MT prototype from the Gothenburg -- Oslo train.  many things
;;; remain to be improved, including:
;;;
;;;   - an actual solution to IPC among LKB processes or a way of loading more
;;;     than one grammar into the same image;
;;;   - the MRS reader requires each relation to have a handel, even where it
;;;     is completely unspecific;
;;;   - without an input condition, a munging rule will never fire; currently,
;;;     always need to provide INPUT.RELS <! !> instead;
;;;   - the type -- sort dichotomy in the MRS code is confusing; i should take
;;;     out the variant that prints boths, and we should all agree that at the
;;;     MRS level there can only be one predicate.
;;;

(defun transfer (&optional (edge (first *parse-record*))
                 &key (file "/tmp/.transfer"))
  (let* ((*package* (find-package :lkb))
         (rules (reverse lkb:*ordered-mrs-rule-list*))
         (input (mrs::extract-mrs edge))
         (output (and input (mrs::munge-mrs-struct input rules))))
    (when output
      (with-open-file (stream file :direction :output
                       :if-exists :supersede)
        (mrs::output-mrs1 output 'mrs::simple stream))
      (mrs::browse-mrs output "Transfer Result"))))

(defun translate (&key serverp (file "/tmp/.transfer"))
  (declare (special %mrs%))
  
  (when serverp
    (loop
        until (probe-file file) do (sleep 1)))
  (when (probe-file file)
    (with-open-file (stream file :direction :input)
      (let* ((mrs (mrs::read-mrs-from-file file))
             (*bypass-equality-check* t))
        ;(delete-file file)
        (setf %mrs% mrs)
        (generate-from-mrs mrs)
        (show-gen-result))))
  (when serverp 
    (delete-file file)
    (translate :serverp serverp :file file)))


;;; functions that allow for translation (using interlingua)
;;; and `translation'

;;; to add - specification of constraints on input and output 
;;; - e.g. formality, dialect etc

;;; eventually, the control of transfer stuff should also be here
;;; although actual transfer rules will be specific to e.g. MRS

#+:interlingua
(defun translate (sentence)
  (when (and sentence *source-language* *target-language*)
      (close-existing-chart-windows)
      (let ((existing-language *current-language*))
        (set-current-language *source-language*)
        (unwind-protect
            (progn
              (parse (split-into-words 
                      (preprocess-sentence-string 
                       (string-trim '(#\space #\tab #\newline) sentence))))
              (loop for parse-res in *parse-record*
                   do
                   (set-current-language *source-language*)
                   ;;; mrs extraction might be language specific
                   (let ((mrs (mrs::extract-mrs parse-res)))
                     (set-current-language *target-language*)
                     (multiple-value-bind
                         (strings unifs-tried unifs-failed active inactive)
                         (generate-from-mrs mrs)
                       (loop for string in strings
                            collect
                            (fix-spelling string))))))
          ;;; reset the current-language etc
          (set-current-language existing-language)))))


(defun set-current-language (language)
    (setf *current-language* language))



          
            