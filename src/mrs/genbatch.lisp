(in-package :lkb)

;;; test functions for generation

;;; evaluate the following for this stuff to be run
;;; as part of LKB batch testing

;;; currently only works in Allegro CL because of timing



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
                  (time (elt res 5)))
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

(defun read-gen-test-file1 (file-name)
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
            (let* ((input-sentence
                    (split-into-words 
                     (preprocess-sentence-string (elt res 0))))
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
                  (time (elt res 5)))
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

(defun pick-from-gen-test-file (file-name)
  ;;; takes a file of output results from generation, and produces
  ;;; averages etc
  (with-open-file (istream file-name :direction :input)
    (loop (let ((res (read istream nil nil)))
            (unless res (return))
 ;           (format t "~%~S" res)
            (let* ((input-sentence (elt res 0))
                   (input-sentence-text
                    (apply #'concatenate 'string 
                           (add-spaces input-sentence)))
;                   (strings (elt res 1))
                   #|
                   (strings-text 
                    (mapcar 
                     #'(lambda (str) 
                         (preprocess-sentence-string
                                (apply #'concatenate 'string 
                                       (add-spaces str))))
                                       strings))
                                       |#
                  (errorp (elt res 2))
                  (edges (elt res 3))
                  (time (elt res 5)))
              (if (and (not errorp) (> time 10000))
                (format t 
                        "~%~A time ~A edges ~A" 
                        input-sentence-text time edges)))))))

(defun add-spaces (str-list)
  (cons (car str-list)
     (for str in (cdr str-list)
       append
       (list " " str))))

(defun compare-gen-test-files (file-name1 file-name2)
  ;;; compares two files of output results from generation
    (with-open-file (istream1 file-name1 :direction :input)
      (with-open-file (istream2 file-name2 :direction :input)  
        (loop (let ((res1 (read istream1 nil nil))
                    (res2 (read istream2 nil nil)))
                (unless (and res1 res2) (return))
                (let* ((input-sentence1 (elt res1 0))
                       (input-sentence2 (elt res2 0)))
                  (unless (equalp input-sentence1 input-sentence2)
                    (error "~%Unequal sentences"))
                  (let
                   ((input-sentence-text 
                    (apply #'concatenate 'string 
                           (add-spaces input-sentence1)))
                    (strings1 (elt res1 1))
                    (strings2 (elt res2 1))
                    (errorp1 (elt res1 2))
                    (errorp2 (elt res2 2))
                    (edges1 (elt res1 3))
                    (edges2 (elt res2 3)))
                   (unless (equal errorp1 errorp2)
                     (format t "~%Differences in error in ~A" input-sentence-text)
                     (format t "~%1 ~A" res1)
                     (format t "~%2 ~A" res2))                     
                   (unless (string-set-equal strings1 strings2)
                     (format t "~%Differences in strings in ~A" input-sentence-text)
                     (format t "~%1 ~A" res1)
                     (format t "~%2 ~A" res2))
                   (unless (eql edges1 edges2)
                     (format t "~%Differences in edges in ~A" input-sentence-text)                     
                     (format t "~%Edges ~A ~A" edges1 edges2))
                   )))))))

(defun compare-gen-test-files1 (file-name1 file-name2)
  ;;; compares two files of output results from generation
    (with-open-file (istream1 file-name1 :direction :input)
      (with-open-file (istream2 file-name2 :direction :input) 
        (let ((total1 0) (total2 0) (count 0))
        (loop (let ((res1 (read istream1 nil nil))
                    (res2 (read istream2 nil nil)))
                (unless (and res1 res2) (return))
                (let* ((input-sentence1 (split-into-words 
                     (preprocess-sentence-string  (elt res1 0))))
                       (input-sentence2 (split-into-words 
                     (preprocess-sentence-string (elt res2 0)))))
                  (unless (equalp input-sentence1 input-sentence2)
                    (error "~%Unequal sentences"))
                  (let
                   ((input-sentence-text 
                    (apply #'concatenate 'string 
                           (add-spaces input-sentence1)))
                    (strings1 (elt res1 1))
                    (strings2 (elt res2 1))
                    (errorp1 (elt res1 2))
                    (errorp2 (elt res2 2))
                    (time1 (elt res1 5))
                    (time2 (elt res2 5)))
                   (unless (equal errorp1 errorp2)
                     (format t "~%Differences in error in ~A" input-sentence-text)
                     (format t "~%1 ~A" res1)
                     (format t "~%2 ~A" res2))                     
                   (unless (string-set-equal strings1 strings2)
                     (format t "~%Differences in strings in ~A" input-sentence-text)
                     (format t "~%1 ~A" res1)
                     (format t "~%2 ~A" res2))
                   (when (equal errorp1 errorp2)
                     (setf total1 (+ total1 time1))
                     (setf total2 (+ total2 time2))
                     (incf count))))))
        (format t "~%mean time run 1: ~,1F seconds ~% mean time run 2: ~,1F seconds"  
                (/ (/ total1 count) 1000)
                (/ (/ total2 count) 1000))))))
                              
                     
(defun string-set-equal (set1 set2)
  (and (eql (length set1) (length set2))
       (for x in set1
            all-satisfy
            (member x set2 :test #'equal))))


;;; **************************************************************

(defun check-generate nil
  (let ((sentence *sentence*)
        (ostream (if (and *ostream* (streamp *ostream*) (output-stream-p *ostream*)) *ostream*  t)))
    (unless *parse-record*
      (format ostream "~%#| Parse failure: ~A |#" sentence))
    (for parse-res in *parse-record*
         do
         (let ((mrs (mrs::extract-mrs parse-res t))
               (tgc nil)
               (tcpu nil)
               (treal nil)
               (errorp nil))
           ;;
           ;; when generating from a parse, dag recycling caused a serious
           ;; problem.  apparently, something in the generator makes reference
           ;; to a dag node allocated from the pool in the parser.  when the
           ;; pool is reset prior to generation, this backward reference causes
           ;; desaster (though curiously not for all sentences; ``Abrams will
           ;; interview Browne as soon as Browne sleeps.'' seems a good test).
           ;; supposedly, the MRS structure extracted from the parser is fully
           ;; self-contained; so, if nothing else referenced parser results,
           ;; resetting the pool on entry to the generator would be the right
           ;; thing.  room for debugging here ...           (8-sep-99  -  oe)
           ;;
           ;; following should fix the problem - aac
           (when mrs::*null-semantics-hack-p*
             (format ostream 
                     "~%#| Setting mrs::*null-semantics-hack-p* to nil |#")
             (setf mrs::*null-semantics-hack-p* nil))
           #+(and :pooling :gdebug)
           (reset-pools #+:gdebug :forcep #+:gdebug t)
           (if (not (mrs::make-scoped-mrs mrs))
               (format ostream "~%#| Scope failure: ~A |#" sentence)  
             ;;; check for scoping, because incorrect MRS often
             ;;; causes serious problems for generator
               (multiple-value-bind
                   (strings unifs-tried unifs-failed active inactive)
                   (excl::time-a-funcall
                    #'(lambda () 
                        (#-:gdebug 
                         handler-case 
                         #+:gdebug 
                         progn
                            (generate-from-mrs mrs)
                          #-:gdebug  
                          (storage-condition (condition)
                            (format t "~%Memory allocation problem in generation: ~A caused by ~A~%" condition sentence)
                            (setf errorp t))
                          #-:gdebug
                          (error (condition)
                            (format t  
                                    "~%Error in generation: ~A caused by ~A~%" condition sentence)
                            (setf errorp t))))
                    #'(lambda (tgcu tgcs tu ts tr scons ssym sother &rest ignore)
                        (declare (ignore ignore sother ssym scons))
                        (setq tgc (+ tgcu tgcs))
                        (setq tcpu (+ tu ts))
                        (setq treal tr)
                        ))
                 (declare (ignore unifs-tried unifs-failed))
                 (clear-gen-chart) ; prevent any recycled dags from hanging around
                 (unless (and (integerp active)
                              (integerp inactive))
                   (setf errorp t)
                   (format t "~%Problem in generation caused by missing relation?"))
                 (format ostream "~%(")
                 (format ostream "~A" sentence)
                 (format ostream "~%~S" (if (listp strings) strings nil))
                 (format ostream " ~A " errorp)
                 (format ostream "~A " (if errorp -1 (+ active inactive)))
                 (format ostream "~A " (if errorp -1 tgc))
                 (format ostream "~A " (if errorp -1 tcpu))
                 (format ostream "~A)~%" (if errorp -1 treal))
                 (finish-output ostream)))))))


;;; somewhat different fn for generating from a file of MRSs

;;; (generate-from-mrs-file "foo" t)

(defun generate-from-mrs-file (file ostream)
  (when mrs::*null-semantics-hack-p*
    (format ostream 
            "~%#| Setting mrs::*null-semantics-hack-p* to nil |#")
    (setf mrs::*null-semantics-hack-p* nil))
  (let ((mrss (mrs::read-mrs-files-aux (list file))))
    (for mrs in mrss
         do
         (let ((tgc nil)
               (tcpu nil)
               (treal nil)
               (errorp nil))
         (when (and mrs (mrs::psoa-p mrs))
           (reset-pools #+:gdebug :forcep #+:gdebug t)
           (if (not (mrs::make-scoped-mrs mrs))
               (format ostream "~%#| Scope failure: ~A |#")  
             ;;; check for scoping, because incorrect MRS often
             ;;; causes serious problems for generator
             (multiple-value-bind
                 (strings unifs-tried unifs-failed active inactive)
                 (excl::time-a-funcall
                  #'(lambda () 
                      (#-:gdebug 
                       handler-case 
                       #+:gdebug 
                       progn
                       (generate-from-mrs mrs)
                       #-:gdebug  
                       (storage-condition (condition)
                                          (format t "~%Memory allocation problem in generation: ~A" condition)
                                          (setf errorp t))
                       #-:gdebug
                       (error (condition)
                              (format t  
                                      "~%Error in generation: ~A%" condition)
                              (setf errorp t))))
                  #'(lambda (tgcu tgcs tu ts tr scons ssym sother &rest ignore)
                      (declare (ignore ignore sother ssym scons))
                      (setq tgc (+ tgcu tgcs))
                      (setq tcpu (+ tu ts))
                      (setq treal tr)
                      ))
               (declare (ignore unifs-tried unifs-failed))
               (clear-gen-chart)        ; prevent any recycled dags from hanging around
               (unless (and (integerp active)
                            (integerp inactive))
                 (setf errorp t)
                 (format t "~%Problem in generation caused by missing relation?"))
               (format ostream "~%(")
               (format ostream "~A" "Unknown sentence")
               (format ostream "~%~S" (if (listp strings) strings nil))
               (format ostream " ~A " errorp)
               (format ostream "~A " (if errorp -1 (+ active inactive)))
               (format ostream "~A " (if errorp -1 tgc))
               (format ostream "~A " (if errorp -1 tcpu))
               (format ostream "~A)~%" (if errorp -1 treal))
               (finish-output ostream))))))))


