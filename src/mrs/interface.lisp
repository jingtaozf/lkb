;;; Copyright (c) 1998-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions

(in-package "MRS")

;;; LKB specific

#| 
(mrs::output-mrs-after-parse *parse-record*)
|#

(defun output-mrs-after-parse (&optional edges stream)
  ;;; for ACL this is most likely to be useful in an emacs window
  ;;; the need to use *lkb-background-stream* is because 
  ;;; of the complexity with background streams in ACL
  ;;; it's set in topmenu.lsp
  (when (or *mrs-scoping*
            *mrs-output-p*)
    (unless stream
      (setf stream lkb::*lkb-background-stream*))
    (unless edges (setf edges *parse-record*))
    (let ((*print-circle* nil))
      (loop for edge in edges 
           do
           (let ((mrs (extract-mrs edge)))
             (format stream "~%Edge number ~A" 
                     (lkb::edge-id edge))
             (format stream "~%~A~%" 
                     (lkb::parse-tree-structure edge))
             (treat-mrs mrs t stream))))))

(defvar *mrs-debug* nil)

(defvar *mrs-discourse* nil)

(defun treat-mrs (mrs-struct simplep stream)
  (format stream "~%~A " lkb::*sentence*)
  (setf *mrs-debug* mrs-struct)
  (cond (*mrs-scoping*
         (process-mrs-struct mrs-struct nil 10 simplep stream))
        (*mrs-discourse*
	 (output-mrs1 mrs-struct 'simple stream)
	 (output-mrs1 mrs-struct 'prolog stream)
	 (with-open-file (pro-out "~/tmp/prologformat"
			  :direction :output :if-does-not-exist :create
			  :if-exists :append)
	   (output-mrs1 mrs-struct 'prolog pro-out)))      
        (t (output-mrs1 mrs-struct 'simple stream))))

(defun process-mrs-struct (mrs-psoa sentence maximum simplep stream)
  (when mrs-psoa
    (when sentence
      (format stream "~%~A~%" sentence))
    (when simplep
      (output-mrs1 mrs-psoa 'simple stream))
    (when (and (boundp '*ordered-mrs-rule-list*)
               *ordered-mrs-rule-list*)
      (format stream "~%Premunged form")
      (output-mrs1 mrs-psoa 'indexed stream))
    (let ((mrsstruct
            (if (and (boundp '*ordered-mrs-rule-list*)
                     *ordered-mrs-rule-list*)
                (munge-mrs-struct mrs-psoa *ordered-mrs-rule-list*)
              mrs-psoa)))
      (format stream "~%Unscoped form")
      (output-mrs1 mrsstruct 'indexed stream)
      (setf *canonical-bindings* nil)
      (let ((binding-sets (make-scoped-mrs mrsstruct)))
        (show-some-scoped-structures mrsstruct binding-sets
                                     stream maximum)))))


;;;

#|
(defparameter lkb::*do-something-with-parse* 'mrs::batch-output-mrs)
|#

(defun batch-output-mrs nil
  ;;; to be called from LKB batch processing
  (let ((sentence lkb::*sentence*)
        (ostream (if (and lkb::*ostream* 
                          (streamp lkb::*ostream*) 
                          (output-stream-p lkb::*ostream*)) 
                     lkb::*ostream*  t)))
    (if *parse-record*
        (progn
          (format ostream "~%;;; MRS for: ~A " sentence)
          (loop for parse in *parse-record*
               do
               (let* ((mrs-struct (extract-mrs parse)))
                 (output-mrs1 mrs-struct 'simple ostream))))
      (format ostream "~%;;; Parse failure: ~A " sentence))
    (finish-output ostream)))


;;; The following are primarily for the TSDB machinery
;;; - they all take an edge and return a string related
;;; to the MRS in some way
;;; Functions are from mrsfns.lisp

(defun get-mrs-string (parse)
  (return-mrs-info-string parse :simple))
  
(defun get-mrs-indexed-string (parse) 
  (return-mrs-info-string parse :indexed))

(defun get-mrs-resolved-string (parse)
  (return-mrs-info-string parse :first-scoped))
  
(defun count-scopes (parse)
  (return-mrs-info-string parse :count-scopes))
    
(defun return-mrs-info-string (parse info-type)
  (let* ((mrs-struct (extract-mrs parse)))
    (with-output-to-string (stream)
      (ecase info-type
        (:simple (output-mrs1 mrs-struct 'simple stream))
        (:indexed (output-mrs1 mrs-struct 'indexed stream))
        (:first-scoped (let ((binding-sets (make-scoped-mrs mrs-struct)))
                   (when binding-sets
                     (with-output-to-string (stream) 
                       (setf *canonical-bindings* (canonical-bindings 
                                                   (first binding-sets)))
                       (output-scoped-mrs mrs-struct :stream stream)))))
        (:count-scopes (format stream "~A" 
                               (length (make-scoped-mrs mrs-struct))))))))

(defun read-mrs-from-string (string)
  (let ((*package* (find-package :mrs)))
    (ignore-errors 
     (with-input-from-string (stream string)
       (read-mrs stream)))))

(defun read-indexed-mrs-from-string (string)
  (let ((*package* (find-package :mrs)))
     (with-input-from-string (stream string)
       (read-indexed-mrs stream))))

(defun safe-mrs-unequalp (mrs1 mrs2 &rest options)
  (not 
   (if (and mrs1 mrs2)
     (apply #'mrs-equalp mrs1 mrs2 '(t nil))
     (equal mrs1 mrs2))))

(defun browse-mrs (mrs &optional title)
  (ignore-errors
   (let ((browser (fboundp (find-symbol "SHOW-MRS-WINDOW" :lkb))))
     (if (functionp browser)
         (apply browser (list nil mrs title))
       (output-mrs mrs 'simple)))))

#|

(defun time-scope nil
  (setf *scoping-call-limit* 1000000)
  (loop for sentence in 
       #|'("Kim sleeps in Berlin in Berlin in Berlin in Berlin in Berlin in Berlin")
       |#       
       '("every daughter sees most daughters"
                     "every daughter sees most daughters of a daughter"
                     "every daughter sees most daughters of a daughter of a daughter"
                     "every daughter sees most daughters of a daughter of a daughter of a daughter"
                     "every daughter of a daughter sees most daughters of a daughter"                     
                     "every daughter of a daughter sees most daughters of a daughter of a daughter"
                     "every daughter of a daughter sees most daughters of a daughter of a daughter of a daughter"
                     "every daughter of a daughter of a daughter sees most daughters of a daughter"                     
                     "every daughter of a daughter of a daughter sees most daughters of a daughter of a daughter"
                     "every daughter of a daughter of a daughter sees most daughters of a daughter of a daughter of a daughter"
                     "every daughter of a daughter of a daughter of a daughter sees most daughters of a daughter"                     
                     "every daughter of a daughter of a daughter of a daughter sees most daughters of a daughter of a daughter"
                     "every daughter of a daughter of a daughter of a daughter sees most daughters of a daughter of a daughter of a daughter")
                     
       do
       (let  ((user-input (lkb::split-into-words sentence)))
         (lkb::parse user-input nil)
  (when *parse-record*
  (let* ((edges *parse-record*)
         (mrs (extract-mrs (car edges))))
    (setf *canonical-bindings* nil)
    
    (let* ((start-time (get-internal-run-time))
           (binding-sets (make-scoped-mrs mrs)))
      (format t "~%~A ~A ~A ~A" sentence
              (length binding-sets) mrs::*scoping-calls* (- (get-internal-run-time) start-time))))))))
                                                            
|#


