(in-package :cl-user)

;;; test functions for generation

;;; evaluate the following for this stuff to be run
;;; as part of LKB batch testing

#|
(defparameter *do-something-with-parse* 'check-generate)

(defparameter *first-only-p* t)
|#


#|
output results

0. input sentence
1. strings
2. error caused
3. total edges
4. gc time
5. cpu time
6. real time

|#

(defun read-gen-test-file (file-name)
  ;;; takes a file of output results from generation, and produces
  ;;; averages etc
  (let ((sentence-count 0)
        (total-sentence-length 0)
        (total-strings 0)
        (total-edges 0)
        (total-time 0))
  (with-open-file (istream file-name :direction :input)
    (loop (let ((res (read istream nil nil)))
            (unless res (return))
 ;           (format t "~%~S" res)
            (let* ((input-sentence (elt res 0))
                   (input-sentence-text
                    (apply #'concatenate 'string 
                           (add-spaces input-sentence)))
                   (strings (elt res 1))
                   (strings-text 
                    (mapcar 
                     #'(lambda (str) 
                         (preprocess-sentence-string
                                (apply #'concatenate 'string 
                                       (add-spaces str))))
                            strings))
                  (errorp (elt res 2))
                  (edges (elt res 3))
                  (time (elt res 6)))
              (if errorp
                (format t 
                        "~%Error when generating from ~A - results ignored" 
                        input-sentence-text)
                (progn
                  (if strings-text
                      (unless
                          (member input-sentence-text strings-text 
                                  :test #'string-equal)
                        (format t 
                                "~%String not matched when generating from ~A" 
                                input-sentence-text))
                    (format t 
                         "~%No strings generated from ~A" 
                                input-sentence-text))
                  (setf sentence-count (+ sentence-count 1))
                  (setf total-sentence-length 
                    (+ total-sentence-length (length input-sentence)))
                  (setf total-strings (+ total-strings (length strings)))
                  (setf total-edges (+ total-edges edges))
                  (setf total-time (+ total-time time)))))))
    (format t 
         "~%Mean length ~,2F Mean strings ~,2F Mean edges ~,1F Mean time ~,1F secs"
            (/ total-sentence-length sentence-count)
            (/ total-strings sentence-count)
            (/ total-edges sentence-count)
            (/ (/ total-time sentence-count) 1000)))))

(defun add-spaces (str-list)
  (cons (car str-list)
     (for str in (cdr str-list)
       append
       (list " " str))))


(defun check-generate nil
  (let ((sentence (or *sentence* (split-into-words (car *last-parses*))))
        (ostream (or *ostream* t)))
    (unless *parse-record*
      (format ostream "~%#| Parse failure: ~A |#" sentence))
    (for parse-res in *parse-record*
         do
         (let ((mrs (car (mrs::extract-mrs (list parse-res) t)))
               (tgc nil)
               (tcpu nil)
               (treal nil)
               (conses nil)
               (errorp nil))
           (multiple-value-bind
               (strings unifs-tried unifs-failed active inactive)
               (excl::time-a-funcall
                #'(lambda () 
                    (handler-case 
                        (generate-from-mrs mrs)
                      (error (condition)
                        (format t  
                                "~%Error in generation: ~A caused by ~A~%" condition sentence)
                        (setf errorp t))))
                #'(lambda (tgcu tgcs tu ts tr scons ssym sother &rest ignore)
                    (declare (ignore ignore sother ssym))
                    (setq tgc (+ tgcu tgcs))
                    (setq tcpu (+ tu ts))
                    (setq treal tr)
                    (setq conses scons)))
             (declare (ignore unifs-tried unifs-failed))
             (unless (and (integerp active)
                          (integerp inactive))
               (setf errorp t)
               (format t "~%Problem in generation caused by missing relation?"))
             (format ostream "~%(")
             (format ostream "~S" sentence)
             (format ostream "~%~S" (if (listp strings) strings nil))
             (format ostream " ~A " errorp)
             (format ostream "~A " (if errorp -1 (+ active inactive)))
             (format ostream "~A " (if errorp -1 tgc))
             (format ostream "~A " (if errorp -1 tcpu))
             (format ostream "~A)~%" (if errorp -1 treal))
             (finish-output ostream))))))


