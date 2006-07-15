;;; Copyright (c) 1998-2005
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions


(in-package "MRS")

;;; Variant of munging rules for generation heuristics for
;;; filtering items with no semantics

;;; (read-mrs-rule-file-aux "~aac/grammar/data/genrules.mrs" t)


(defun predict-for-gen nil
  ;;; for debugging
  (format t "~%~A " lkb::*sentence*)
  (dolist (edge lkb::*parse-record*)
    (let ((mrs-struct (extract-mrs edge)))
      (when mrs-struct
        (unless lkb::*gen-rule-list*
          (error "~%No heuristic rules defined"))
        (format t "~%~S" 
                (genpredict-mrs-struct mrs-struct 
                                       lkb::*gen-rule-list*))))))


(defun test-gen-predict-on-parse (&optional parses sentence (ostream t))
   #+:mt
  (declare (special mt::*transfer-triggers*))
  (dolist (parse (or parses *parse-record*))
    (let* ((mrs-struct (extract-mrs parse))
	   (lex-ids (lkb::edge-lex-ids parse))
	   (null-sem-lex-ids 
	    (loop for id in lex-ids
		when (null-semantics-entry-p id)
		collect id)))
      (when mrs-struct
	(let ((predicted-ids 
	       (cond
		#+:mt
		((and (hash-table-p mt::*transfer-triggers*)
                          (> (hash-table-count mt::*transfer-triggers*) 0))
		 (oe-genpredict-mrs-struct mrs-struct))
		(lkb::*gen-rule-list*
		 (genpredict-mrs-struct mrs-struct lkb::*gen-rule-list*))
		(t (format ostream "~%Warning: no trigger rules defined")
		   nil))))
	  (unless (subsetp null-sem-lex-ids predicted-ids)
	    (format ostream "~%Missing predictions:")
	    (when sentence
	      (format ostream "~%~A" sentence))
	    (dolist (null-id null-sem-lex-ids)
	      (when (not (member null-id predicted-ids))
		(format ostream " ~A" null-id)))))))))
#|
test-gen-predict-on-mrs-file

reads in:

<!ELEMENT null-sem (sentence, parse-record*)>
<!ELEMENT parse-record (mrs, id*)>
<!ELEMENT sentence (#PCDATA)>
<!ELEMENT id (#PCDATA)>

outputs:

<!ELEMENT null-sem (sentence, parse-record*)>
<!ELEMENT parse-record (mrs, predicted*, id*)>
<!ELEMENT sentence (#PCDATA)>
<!ELEMENT id (#PCDATA)>
<!ELEMENT predicted (#PCDATA)>
|#


#|
(defun test-gen-predict-on-nullsem-file (ifile ofile)
  (declare (special mt::*transfer-triggers*))
  (let ((*package* (find-package :mrs)))
    (with-open-file (istream ifile :direction :input)
      (with-open-file (ostream ofile :direction :output :if-exists :supersede)
	(let ((ns (parse-xml-removing-junk istream)))
	  (unless (equal (car ns) '|ns-list|)
	    (error "Not a valid ns file"))
	  (write-line "<ns-list>" ostream)
	  (loop for null-sem-item in (cdr ns)
	      unless (xml-whitespace-string-p null-sem-item)
	      do
		(test-gen-predict-on-ns null-sem-item ostream))
  	  (write-line "</ns-list>" ostream))))))

(defun test-gen-predict-on-ns (content ostream)
  (unless (equal (car content) '|null-sem|)
    (error "Tough"))
  (setf content (cdr content))
  (loop (let ((next-el (car content)))
	  (if (xml-whitespace-string-p next-el)
	      (pop content)
	    (return))))
  (when content
    (let ((sentence-rec (car content))
	  (sentence nil))
      (unless (equal (car sentence-rec) '|sentence|)
	(error "Not sentence"))
      (setf sentence (cadr sentence-rec))
      (write-line "<null-sem>" ostream)
      (format ostream "<sentence>~A</sentence>~%" sentence)
      (loop for pr in (cdr content)
	  unless (xml-whitespace-string-p pr)
	  do 
	    (test-gen-predict-on-ns-pr pr ostream))
      (write-line "</null-sem>" ostream))))

(defun test-gen-predict-on-ns-pr (content ostream)    
  (loop (let ((next-el (car content)))
	  (if (xml-whitespace-string-p next-el)
	      (pop content)
	    (return))))
  (when content
    (unless (eql (car content) '|parse-record|)
      (error "Not parse record"))
    (write-line "<parse-record>" ostream)
    (setf content (cdr content))
    (loop (let ((next-el (car content)))
	  (if (xml-whitespace-string-p next-el)
	      (pop content)
	    (return))))
    (let ((mrs-struct (read-mrs-xml (car content))))
      (unless mrs-struct
	(error "No mrs"))
      (let ((predicted-ids 
	     (cond
	      #+:mt
	      ((and (hash-table-p mt::*transfer-triggers*)
		    (> (hash-table-count mt::*transfer-triggers*) 0))
	       (oe-genpredict-mrs-struct mrs-struct))
	      (lkb::*gen-rule-list*
	       (genpredict-mrs-struct mrs-struct lkb::*gen-rule-list*))
	      (t (format t "~%Warning: no trigger rules defined")
		 nil))))
	(output-mrs1 mrs-struct 'mrs-xml ostream)
	(dolist (id predicted-ids)
	  (format ostream "<predicted>~A</predicted>~%" id)))
      (loop for id in (cdr content)
	  unless (xml-whitespace-string-p id)
	  do 
	    (unless (eql (car id) '|id|)
	      (error "id"))
	    (format ostream "<id>~A</id>~%" (cadr id)))
      (write-line "</parse-record>" ostream))))
	
		   
		     
|#

#+:mt
(defun oe-genpredict-mrs-struct (input-sem)
  ;;; copied from lexlookup.lisp
  (let ((triggers (mt::transfer-mrs
		   input-sem :filter nil :task :trigger)))
    (remove-duplicates
     (loop
	 for edge in triggers
	 for mtr = (mt::edge-rule edge)
	 for id = (mt::mtr-trigger mtr)
	 when (and id
		   (not (lkb::smember
			 id lkb::*duplicate-lex-ids*)))
	 collect id))))


(defun genpredict-mrs-struct (mrsstruct rules)
  ;;; takes an mrs structure and a set of generation rules
  ;;; using the output of the rules to predict null semantic
  ;;; items for generation
  ;;; Rules are applied in order and are not applied recursively
  (if rules
      (progn
        (setf *original-variables* nil)
        (let ((null-ids nil))
          (dolist (rule rules)
            (let ((new-results 
                   (match-mrs-rule 
                    mrsstruct  
                    (mrs-munge-rule-input-condition rule))))
              (when new-results
                (pushnew (mrs-munge-rule-output-spec 
                          rule) null-ids))))
          null-ids))
    ;;; NB - we do only want one instance of an id 
    ;;; despite the fact that more than one may be required in 
    ;;; a sentence.  
    (progn
      (format t "~%Warning: no generator prediction rules have been set")
      *empty-semantics-lexical-entries*)))

  


;;; *************** Rule input ****************

#|

(read-mrs-rule-file-aux "~aac/grammar/data/genrules.mrs" t)

|#



(defun construct-gen-rule-from-fs (id fs funny-unifs)
  ;;; input and output are constructed using construct-mrs
  ;;; with a given variable-generator
  (declare (ignore id))
  (when funny-unifs
    (error "Funny unifs not expected in generator rules"))
  (let ((output-fs (path-value fs *mrs-rule-output-path*))
        (condition-fs (path-value fs *mrs-rule-condition-path*)))
      (if (and condition-fs output-fs)
          (let* ((variable-generator (create-variable-generator 1000))
                 (output-spec (construct-output-id output-fs))
                 (condition-spec 
                      (construct-mrs condition-fs variable-generator)))
            (if (and condition-spec output-spec)
                (progn 
                  (pushnew output-spec *gen-rule-ids* :test #'eq)
                  (make-mrs-munge-rule 
                   :output-spec output-spec
                   :input-condition condition-spec)))))))

(defun construct-output-id (fs)
  (let ((res (fs-type fs)))
    (if (stringp res)
        (intern (string-upcase res) :lkb))))

        


