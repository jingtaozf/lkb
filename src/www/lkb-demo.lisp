;;; Copyright (c) 1999-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions


;;;**************************************************************************
;;; Provide a World-Wide-Web interface for the LKB.
;;;
;;;  Rob Malouf, 5-March-1999
;;;

(in-package :http-user)

(defparameter *demo-version* "April 30, 1999")
(defparameter *parser-log-file* "lkb-sentences.log")

(www-utils:add-periodic-task "Flush lexicon" :daily '(user::clear-non-parents))

;;***************************************************************************
;; Feature structure cache

(defvar *cached-parses* (make-hash-table :test #'equal)
  "Feature structure cache, full of old parses")

(defvar *cache-lock* (mp:make-process-lock))

(defparameter *cache-interval* 240
  "Life span of a cache entry, in seconds.")
 
(defun cache-reaper (key)
  "Wait a while, then remove an old fs from the cache."
  (sleep *cache-interval*)
  (mp:with-process-lock (*cache-lock*)
    (remhash key *cached-parses*)))

(defmacro cached-fs (key)
  "Pull a fs out of the cache."
  `(mp:with-process-lock (*cache-lock*)
     (gethash ,key *cached-parses*)))

(defsetf cached-fs (key) (fs)
  "Put a fs into the cache for later use."
  `(progn
     (mp:with-process-lock (*cache-lock*)
     (setf (gethash ,key *cached-parses*) ,fs)
     (mp:process-run-function "Reaper" #'cache-reaper ,key))
     ,key))

;;***************************************************************************
;; Export parser demo page

(defun write-parser-query (url stream defaults)
  (declare (ignore url))
  (write-links :parser stream)
  (with-section-heading ("Linguistic Grammars Online" :stream stream)
    (with-paragraph (:stream stream)
      (with-table (:stream stream)
	(with-table-row (:stream stream)
	  (with-table-cell (:stream stream)
	    (with-rendition (:bold :stream stream)
	      (write-string "Sentence to parse: " stream)))
	  (with-table-cell (:stream stream)
	    (accept-input 'string "EXPR" :size 40 :stream stream)
	    (write-string " " stream)
	    (accept-input 'submit-button "Submit" :stream stream)))
	(with-table-row (:stream stream)
	  (with-table-cell (:stream stream))
	  (with-table-cell (:stream stream)
	    (write-string "Find: " stream)
	    (accept-input 'radio-button "All" :stream stream
			  :choices '((" first parse only " . "no") 
				     (" all parses " . "yes"))
			  :default (first defaults)
			  :linebreaks nil)))
	(with-table-row (:stream stream)
	  (with-table-cell (:stream stream))
	  (with-table-cell (:stream stream)
	    (write-string "Print: " stream)
	    (accept-input 'checkbox "Output" :stream stream
			  :choices '((" MRS " . "mrs")
				     (" FOL " . "fol")
				     (" VIT " . "vit"))
			  :default (second defaults)
			  :linebreaks nil)))))))

;; Wraps the header and footer for parser demo around something

(defmacro with-header ((url stream title) &body body)
  `(http:with-successful-response (stream
				   :html
				   :expires 
				   (url:expiration-universal-time ,url)
				   :last-modification (get-universal-time))
     (html:with-html-document (:stream stream)
       (html:with-document-preamble (:stream stream)
	 (html:declare-title ,title :stream stream))
       (html:with-standard-document-body (:stream stream)
	 ,@body
	 (html:horizontal-line :stream stream)
	 (html:with-emphasis (:address :stream stream)
	   (html:note-anchor "Comments?" :stream stream
			     :reference "mailto:malouf@csli.stanford.edu")
	   (format stream " Let us know!<BR>")
	   (format stream "LKB Demo -- ~A (~A)" *demo-version*
		   user::*grammar-version*))))))


(defun write-links (where stream)
  (labels ((write-link (place name url)
	     (if (eq place where)
		 (html:with-rendition (:bold :stream stream)
		   (write-string name stream))
	       (html:note-anchor name :stream stream :reference url))))
    (html:with-paragraph (:stream stream :alignment :right)
      (write-string "[ " stream)
      (write-link :lingo "lingo" "http://hpsg.stanford.edu/hpsg/lingo.html")
      (write-string " &middot; " stream)
      (write-link :news "info" "http://hpsg.stanford.edu/hpsg/demo.html")
      (write-string " &middot; " stream)
      (write-link :grammar "grammar" "http://hpsg.stanford.edu/grammar/")
      (write-string " &middot; " stream)
      (write-link :parser "parser" "/lingo/parser.html")
      (write-string " ]" stream))))

(defun compute-lingo-parser-form (url stream)
  (with-header (url stream "LinGO Parser Demo")
    (with-fillout-form (:post url :stream stream)
      (write-parser-query url stream '("no" ("mrs"))))))

;; Output results of last parse/generation and query for next operation

(defun respond-to-lingo-parser (url stream query-alist)
  (let ((*output-stream* stream))
    (declare (special *output-stream*))
    (with-header (url stream "LinGO Parser Demo")
      (with-fillout-form (:post url :stream stream)
	(http:bind-query-values 
	 (all output) (url query-alist)
	 (write-parser-query url stream (list all (if (consp output)
						      output
						    (list output)))))
	(html:horizontal-line :stream stream)
	(if (member "Generate" query-alist :test #'equal :key #'second)
	    (write-generator-results url stream query-alist)
	  (write-parser-results url stream query-alist))))))

(export-url #u"/lingo/parser.html"
	    :html-computed-form
	    :form-function #'compute-lingo-parser-form
	    :response-function #'respond-to-lingo-parser
	    :expiration `(:interval ,(* 15. 60.))
	    :public t
	    :language :en)


;;***************************************************************************
;; Parse a sentence and build HTML output for the parser results.  The parser
;; isn't re-entrant, so we want to make sure that we're only parsing one
;; sentence at a time.  We use this process lock to control access to the
;; parser.

(defvar *parser-lock* (mp:make-process-lock))

(defun write-parser-results (url stream query-alist)
  (http:bind-query-values 
   (expr all output) (url query-alist)
   (mp:with-process-lock (*parser-lock*)
     (let ((*standard-output* stream))
       (user::parse (user::split-into-words 
		     (user::preprocess-sentence-string 
		      (string-trim '(#\space #\tab #\newline) expr)))
		    nil			; Show results
		    (equal all "no")	; First parse only?
		    )
       (break-line)
       (write-parses expr all output user::*parse-record* stream)))))

;;***************************************************************************
;; Output a list of parse results as a table with trees in the first column,
;; MRS's in the second column, and links to fegramed files in the third
;; column.  We don't actually generate the tree or the fegramed file just now.
;; We just cache the information needed to create them and put in links here.
;; When the client GETs them, they'll be generated on the fly.

(defun write-parses (expr all output parses stream)
  (when (or (null parses) (equal all "yes"))
    (format stream "There ~[are~;is~:;are~] ~:*~D parse~:P for \"~A\"" 
	    (length parses) expr)
    (write-string "<P>" stream))
  (dolist (parse parses)
    (let ((filename (symbol-name (gentemp "parse"))))
      (setf (cached-fs filename) parse)
      ;; Display tree
      (format stream "<IMG SRC=\"tree?~A\">"  filename)
      ;; Display semantics
      (let* ((output (if (consp output) 
			 output
		       (list output)))
	     (*print-circle* nil)
	     (*standard-output* stream)
	     (fs (mrs::get-parse-fs parse))
	     (sem-fs (mrs::path-value fs mrs::*initial-semantics-path*))
	     (mrs-struct (when (mrs::is-valid-fs sem-fs)
			   (mrs::construct-mrs sem-fs))))
	(when mrs-struct
	  (with-table (:stream stream)
	    (with-table-row (:vertical-alignment :top :stream stream)
	      (with-table-cell (:stream stream)      
		(with-verbatim-text (:stream stream)
		  (finish-output stream)		
		  (when (member "mrs" output :test #'equalp)
		    (mrs::output-mrs mrs-struct 'mrs::simple))))
	      (with-table-cell (:stream stream)      
		(accept-input 'submit-button filename
			      :display-string "Generate" :stream stream))))
	  (with-verbatim-text (:stream stream)
	    (finish-output stream)	
	    (if (member "vit" output :test #'equalp)
		(mrs::mrs-to-vit-convert mrs-struct)	      
	      (when (member "fol" output :test #'equalp)
		(mrs::scope-mrs-struct mrs-struct)))))))
    (finish-output stream))
  parses)

;;***************************************************************************
;; Export parse tree directory

(defmethod respond-to-tree ((url http-search) stream)
  (let ((tree 
	 (user::make-new-parse-tree (cached-fs (car (search-keys url))) 1)))
    (with-successful-response (stream :gif
				      :expires (expiration-universal-time url)
				      :last-modification (get-universal-time))
      (mp:with-process-lock (*parser-lock*)
	(draw-parse-www tree stream)))))

(export-url #u"/lingo/tree?"
	    :search
	    :response-function #'respond-to-tree
	    :expiration `(:interval ,*cache-interval*)
	    :public t
	    :language :en)

;;***************************************************************************
;; Interface to generator

(defun write-generator-results (url stream query-alist)
  (let* ((parse 
	  (string-downcase
	   (symbol-name 
	    (car 
	     (find "Generate" query-alist :test #'equal :key #'second)))))
	 (content (mrs::extract-mrs (cached-fs parse))))
    (when (mrs::psoa-liszt content)
      (user::generate-from-mrs content))
    (let ((sentences
	   (sort
	    (mapcar
	     #'(lambda (edge)
		 (format nil "~{~A~^ ~}" 
			 (user::fix-spelling (user::g-edge-leaves edge))))
	     user::*gen-record*)
	    #'string-lessp))) 
      (if sentences
	  (dolist (s sentences)
	    (format stream "<P>~A" s))
	(format stream "<p>No strings generated")))))

