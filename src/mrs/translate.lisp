(in-package :cl-user)

;;; functions that allow for translation (using interlingua)
;;; and `translation'

;;; to add - specification of constraints on input and output 
;;; - e.g. formality, dialect etc

;;; eventually, the control of transfer stuff should also be here
;;; although actual transfer rules will be specific to e.g. MRS

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
              (for parse-res in *parse-record*
                   do
                   (set-current-language *source-language*)
                   ;;; mrs extraction might be language specific
                   (let ((mrs (mrs::extract-mrs parse-res t)))
                     (set-current-language *target-language*)
                     (multiple-value-bind
                         (strings unifs-tried unifs-failed active inactive)
                         (generate-from-mrs mrs)
                       (for string in strings
                            collect
                            (fix-spelling string))))))
          ;;; reset the current-language etc
          (set-current-language existing-language)))))


(defstruct (language-info)
  language
  abbreviation
  lexicon)

(defun set-current-language (language &optional abbreviation)
  (declare (ignore abbreviation))
  ;;; this is here because we may need to generate particular names
  (let ((existing-language (find language *language-record*
                                 :key #'language-info-language)))
    (unless existing-language
      (setf existing-language
        (let ((new-lexicon (make-new-lexicon)))
          (make-language-info :language language
                              :lexicon new-lexicon)))
      (push existing-language *language-record*))
    (setf *lexicon* (language-info-lexicon existing-language))
    (setf *current-language* language)))



          
            