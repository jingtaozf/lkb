;;; Copyright (c) 1998--2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package "MRS")

;;; LKB specific

#| 
(mrs::output-mrs-after-parse *parse-record*)
|#

(defvar *mrs-scoping-output-p* nil)
;; interface control - causes scoping code to be run when set

(defvar *mrs-base-output-p* nil)
;; interface control - set via mrstoplevel

(defvar *rmrs-xml-output-p* nil)
;; interface control - set via mrstoplevel

(defvar *rmrs-compact-output-p* nil)
;; interface control - set via mrstoplevel

(defvar *mrs-discourse* nil)
;; interface control

(defvar *mrs-fol-output-p* nil)

(defvar *mrs-debug* nil)
;;; for debugging - set to latest mrs structure

#+:lkb
(defun output-mrs-after-parse (&optional edges stream)
  ;;; for ACL this is most likely to be useful in an emacs window
  ;;; the need to use *lkb-background-stream* is because 
  ;;; of the complexity with background streams in ACL
  ;;; it's set in topmenu.lsp
  (when (or *mrs-scoping-output-p*
            *mrs-base-output-p*
            *rmrs-xml-output-p*
            *rmrs-compact-output-p*
	    *mrs-discourse*)
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

#+:lkb
(defun treat-mrs (mrs-struct simplep stream)
  (format stream "~%~A " lkb::*sentence*)
  (setf *mrs-debug* mrs-struct)
  (when *mrs-base-output-p*
    (output-mrs1 mrs-struct 'simple stream))
  (when *mrs-scoping-output-p*
    (process-mrs-struct mrs-struct nil 10 simplep stream))
  (when *mrs-fol-output-p*
    (output-fol-approximation mrs-struct stream))
  (when *rmrs-xml-output-p*
         (output-rmrs1 (mrs-to-rmrs mrs-struct) 'xml stream))
  (when *rmrs-compact-output-p*
    (output-rmrs1 (mrs-to-rmrs mrs-struct) 'compact stream t))
  (when *mrs-discourse*
	 (output-mrs1 mrs-struct 'simple stream)
	 (output-mrs1 mrs-struct 'prolog stream)
	 (with-open-file (pro-out "~/tmp/prologformat"
			  :direction :output :if-does-not-exist :create
			  :if-exists :append)
	   (output-mrs1 mrs-struct 'prolog pro-out))))


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
      (let ((disj-structs (disj-test-mrs mrsstruct)))
	(loop for disj-struct in disj-structs
	    do
	      (let ((binding-sets (make-scoped-mrs disj-struct)))
		(show-some-scoped-structures disj-struct binding-sets
					     stream maximum)))))))

(defun output-fol-approximation (mrs stream)
  (let ((fol-exp (make-fol-approximation mrs)))
    (if fol-exp
	(dolist (exp fol-exp)
	  (format stream "~%~A" exp))
      (format stream "~%Expression cannot be converted"))))


(defun make-fol-approximation (mrs)  
  (if mrs
      (let ((disj-structs (disj-test-mrs mrs)))
	(loop for disj-struct in disj-structs
	    collect
	      (let ((binding-sets (make-scoped-mrs disj-struct)))
		(loop for binding in binding-sets
		    collect
		      (progn 
			(setf *canonical-bindings* 
			  (canonical-bindings binding))
			(let ((gq-exp 
			       (output-gq-mrs mrs)))
			  (convert-gq-to-fol-top gq-exp)))))))))
		      
		



;;;

#|
(defparameter lkb::*do-something-with-parse* 'mrs::batch-output-mrs)
|#

#+:lkb
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


#|
(defparameter lkb::*do-something-with-parse* 'mrs::batch-output-scoped-mrs)
|#

#+:lkb
(defun batch-output-scoped-mrs nil
  ;;; to be called from LKB batch processing
  (let ((sentence lkb::*sentence*)
        (ostream (if (and lkb::*ostream* 
                          (streamp lkb::*ostream*) 
                          (output-stream-p lkb::*ostream*)) 
                     lkb::*ostream*  t)))
    (format ostream "~%;;; Sentence: ~A " sentence)
    (if *parse-record*
	(loop for parse in *parse-record*
	    do
	      (let* ((mrs-struct (extract-mrs parse)))
		(if mrs-struct
		    (progn
		      (setf *canonical-bindings* nil)
		      (let ((disj-structs (disj-test-mrs mrs-struct)))
			(loop for disj-struct in disj-structs
			    do
			      (let ((binding-sets 
				     (make-scoped-mrs disj-struct)))
				(if binding-sets
				    (show-some-scoped-structures 
				     disj-struct binding-sets
				     ostream (length binding-sets))
				  (format ostream 
					  "~%;;; Failure to scope"))))))	  
		  (format ostream "~%;;; Failure to extract mrs"))))  
      (format ostream "~%;;; Parse failure"))
    (finish-output ostream)))

#|
(defparameter lkb::*do-something-with-parse* 'mrs::batch-test-mrs-io)
|#

#+:lkb
(defun batch-test-mrs-io nil
  ;;; to be called from LKB batch processing
  (let ((sentence lkb::*sentence*)
	(analysis-no 0)
        (ostream (if (and lkb::*ostream* 
                          (streamp lkb::*ostream*) 
                          (output-stream-p lkb::*ostream*)) 
                     lkb::*ostream*  t)))
    (loop for parse in *parse-record*
	do
	  (let* ((mrs-struct (extract-mrs parse)))
	    (incf analysis-no)
	    (if mrs-struct
		(progn 
		  (with-open-file (ostream1 "/tmp/foo5mrstest"
				   :direction :output
				   :if-exists :supersede)
		    (output-mrs1 mrs-struct 'mrs-xml ostream1)
		    (finish-output ostream1))
		  (let ((read-mrs 
			 (read-single-mrs-xml-file "/tmp/foo5mrstest")))
		    (unless (mrs-equalp read-mrs mrs-struct t)
		      (format ostream 
			      "~%MRS difference for ~A structure ~A"
			      sentence analysis-no))))
	      (format ostream "~%;;; Failure to extract mrs"))  
	    (finish-output ostream)))))

#|
For attempting to learn null semantics

(defparameter lkb::*do-something-with-parse* 'mrs::batch-null-semantics)
|#

#|
<!ELEMENT null-sem (sentence, parse-record*)>
<!ELEMENT parse-record (mrs, id*)>
<!ELEMENT sentence (#PCDATA)>
<!ELEMENT id (#PCDATA)>
|#

#+:lkb
(defun batch-null-semantics nil
  ;;; to be called from LKB batch processing
  (let ((sentence lkb::*sentence*)
        (ostream (if (and lkb::*ostream* 
                          (streamp lkb::*ostream*) 
                          (output-stream-p lkb::*ostream*)) 
                     lkb::*ostream*  t)))
    (format ostream
	    "~%<null-sem>")
    (format ostream
	    "~%<sentence>~A</sentence>" sentence)
    (loop for parse in *parse-record*
	do
	  (let* ((mrs-struct (extract-mrs parse))
		 (lex-ids (lkb::edge-lex-ids parse))
		 (null-sem-lex-ids 
		  (loop for id in lex-ids
			when (null-semantics-entry-p id)
			collect id)))
	    (when mrs-struct
		(progn
		  (format ostream
			  "~%<parse-record>")
		  (output-mrs1 mrs-struct 'mrs-xml ostream)
		  (dolist (id null-sem-lex-ids)
		    (format ostream "~%<id>~A</id>" id))
		  (format ostream
			  "</parse-record>")
		  (finish-output ostream)))))
    (format ostream
	    "~%</null-sem>")))


#|
(defparameter lkb::*do-something-with-parse* 'mrs::batch-trigger-check)
|#

#+:lkb
(defun batch-trigger-check nil
  ;;; to be called from LKB batch processing
  ;;; test-gen-predict-on-parse is in genpredict.lisp
  (let ((sentence lkb::*sentence*)
        (ostream (if (and lkb::*ostream* 
                          (streamp lkb::*ostream*) 
                          (output-stream-p lkb::*ostream*)) 
                     lkb::*ostream*  t)))
    (test-gen-predict-on-parse *parse-record* sentence ostream)))


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
  (let* ((*package* (find-package :lkb))
         (mrs-struct (extract-mrs parse)))
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
  (if (psoa-p string)
    string
    (let ((*package* (find-package :lkb))
          (mrs (ignore-errors 
                (with-input-from-string (stream string)
                  (read-mrs stream)))))
      (when (psoa-p mrs) mrs))))

(defun read-and-scope-mrs-from-string (string)
  (let ((*package* (find-package :lkb))
        (mrs (#+:debug progn #-:debug ignore-errors 
              (with-input-from-string (stream string)
                (read-mrs stream)))))
    (when (and (psoa-p mrs) (ignore-errors (make-scoped-mrs mrs)))
      mrs)))

(defun read-mrs-from-file (file)
  ;;; called by oe
  (when (probe-file file)
    (#+:debug progn #-:debug ignore-errors 
     (with-open-file (istream file :direction :input)
       (let ((*package* (find-package :lkb)))
         (read-mrs istream))))))

(defun read-mrss-from-file (file)
  (when (probe-file file)
    (#+:debug progn #-:debug ignore-errors 
     (with-open-file (istream file :direction :input)
       (let ((*package* (find-package :lkb)))
         (read-mrs-stream istream))))))

(defun read-indexed-mrss-from-file (file)
  (when (probe-file file)
     (with-open-file (istream file :direction :input)
       (let ((*package* (find-package :lkb)))
         (read-mrs-stream istream :indexed)))))


(defun read-indexed-mrs-from-string (string)
  (let ((*package* (find-package :mrs)))
     (with-input-from-string (stream string)
       (read-indexed-mrs stream))))

(defun safe-mrs-unequalp (mrs1 mrs2 &rest options)
  (declare (ignore options))
  (not 
   (if (and mrs1 mrs2)
     (apply #'mrs-equalp mrs1 mrs2 '(t nil))
     (equal mrs1 mrs2))))

(defun display-mrs (edge &optional mrs title (format :simple))
  (if #+:lui (lkb::lui-status-p :mrs format) #-:lui nil
    (let ((mrs (or mrs (lkb::edge-mrs edge) (extract-mrs edge))))
      (when (psoa-p mrs) 
        (lkb::lui-display-mrs mrs title format)))
    (case format
      (:simple
       (lkb::show-mrs-window edge mrs title))
      (:indexed 
       (lkb::show-mrs-indexed-window edge mrs title))
      (:prolog 
       (lkb::show-mrs-prolog-window edge mrs title))
      (:scoped 
       (lkb::show-mrs-scoped-window edge mrs title))
      (:robust 
       (lkb::show-mrs-rmrs-window edge :mrs mrs :title title))
      (t
       (lkb::show-mrs-dependencies-window edge mrs title)))))


(defparameter *mrs-default-display* :simple)

(defun browse-mrs (mrs &optional title)
  (ignore-errors
   (let ((browser 
          (fboundp 
           (case *mrs-default-display*
             (:simple (find-symbol "SHOW-MRS-WINDOW" :lkb))
             (:scoped (find-symbol "SHOW-MRS-SCOPED-WINDOW" :lkb))
             (:eds (find-symbol "SHOW-MRS-DEPENDENCIES-WINDOW" :lkb))))))
         
     (if (functionp browser)
       (apply browser (list nil mrs title))
       (output-mrs mrs 'simple)))))


;;;
;;; (LOGON) fragment detection; see comment in `mrsglobals.lisp'.
;;;
(defun fragmentp (mrs)
  (when (psoa-p mrs)
    (loop
        for ep in (psoa-liszt mrs)
        when (member (rel-pred ep) *semi-fragment-relations* :test #'equal)
        count 1 into n
        finally (return (unless (zerop n) n)))))

;;;
;;; initially mostly for HTML output, though maybe of general utility?  LOGON
;;; with its liberal use of the MRS framework has various `types' of EP, e.g.
;;; :fragment relations used to sew together connected pieces of semantics in
;;; robust mode, :token EPs (the low end of robustness: a piece of unanalyzed
;;; source langguage), and punctuation EPs (which maybe could be :token EPs).
;;; the following is intended as a user function, i.e. a way of adding colour
;;; to the HTML output.                                        (3-nov-04; oe)
;;;
(defun determine-ep-class (ep)
  (if (member (rel-pred ep) *semi-fragment-relations* :test #'equal)
    :fragment
    nil))

;;;
;;; for similar reasons, do roughly the same thing for the MRS as a whole.
;;;
(defun determine-mrs-class (mrs)
  (when (fragmentp mrs) :fragment))

;;;
;;; a couple of RMRS interface functions (mostly) for [incr tsdb()]; we started
;;; this collection in `rmrs/interface.lisp', but the ECL -- PET linking would
;;; be unhappy about duplicate file names, for a silly reason.
;;;
;;; FIX - so change the file name?  
;;; this should be in rmrs directory (AAC 12 Oct 2003)

(defun rasp-semantix-hook (derivation)
  (let* ((*package* (find-package :mrs))
         (derivation (read-from-string derivation nil nil)))
    (ignore-errors
     (with-output-to-string (stream)
       (construct-sem-for-tree derivation :rasp stream)))))

#|
#+:xml
(defun read-rmrs-from-string (string)
  (let ((*package* (find-package :mrs)))
    (ignore-errors 
     (read-rmrs (first (xml:parse-xml string))))))
;;; FIX - now need a second argument to read-rmrs, indicating origin
|#

(defun browse-rmrs (rmrs &optional title)
  (ignore-errors
   (let ((browser (fboundp (find-symbol "SHOW-MRS-RMRS-WINDOW" :lkb))))
     (if (functionp browser)
       (apply browser (list nil :rmrs rmrs :title title))
       (output-rmrs rmrs 'compact)))))

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


