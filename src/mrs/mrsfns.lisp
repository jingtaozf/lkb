(in-package "MRS")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Add access function used by TSDB machinery

;;
;; files copied from `patches/mrsfns.lisp' from the grammar (24-aug-98  -  dpf)
;;

(defun get-mrs-strings (parse-list)
  (loop for parse in parse-list
        collecting
        (let* ((fs (get-parse-fs parse))
               (sem-fs (path-value fs *initial-semantics-path*)))
          (if (is-valid-fs sem-fs)
              (let ((mrs-struct (sort-mrs-struct (construct-mrs sem-fs))))
                (with-output-to-string (stream) 
		  (format stream "~%~S" mrs-struct)
                  ;(output-mrs1 mrs-struct 'simple stream)
		  ))))))

(defun get-mrs-resolved-strings (parse-list)
  (loop for parse in parse-list
	collecting
        (let* ((fs (get-parse-fs parse))
               (sem-fs (path-value fs *initial-semantics-path*)))
          (when (is-valid-fs sem-fs)
              (let* ((mrs-struct (sort-mrs-struct (construct-mrs sem-fs)))
		     (binding-sets (make-scoped-mrs mrs-struct)))
		(when binding-sets
		  (with-output-to-string (stream) 
		    (setf *canonical-bindings* (canonical-bindings 
						(first binding-sets)))
		    (output-scoped-mrs mrs-struct :stream stream))))))))

#|
(defun expand-tsdb-results (result-file dest-file &optional (vitp nil))
  (let ((sent-list
	 (tsdb::select "(parse-id i-input)" :string "(item result)" nil 
		       result-file))
	(tree-list
	 (tsdb::select "(parse-id tree)" :string "(item result)" nil 
		       result-file))
	(mrs-list
	 (tsdb::select "(parse-id mrs)" :string "(item result)" nil result-file))
	(*raw-mrs-output-p* nil))
    (with-open-file 
	(ostream dest-file :direction :output :if-exists :supersede)
    (loop for rawsent in sent-list
	  as rawtree in tree-list
          as rawmrs in mrs-list
	do (let* ((sentstr (cdar rawsent))
		  (sent (subseq sentstr (1+ (position #\@ sentstr))))
		  (treestr (cdar rawtree))
		  (tree (read-from-string 
			 (subseq treestr (1+ (position #\@ treestr)))))
		  (mrsstr (cdar rawmrs))
		  (mrs (subseq mrsstr (1+ (position #\@ mrsstr)))))
	     (format t "~%~A" sent)
	     (format ostream "~%String: ~A~%" sent)
	     (output-parse-tree tree ostream)
	     (if vitp
	    #|
	    (progn
	      (multiple-value-bind 
		  (vit binding-sets)
		  (mrs-to-vit mrs))
	      (write-vit-pretty t (horrible-hack-2 vit))
	      (format ostream "~%")
	      (check-vit vit))
	      |#
	    (progn
	      (format ostream "~A~%~%" mrs)
	      (finish-output ostream)
	      (check-vit mrs t ostream)
	      (format ostream "~%"))
	  (format ostream "~%~A~%" mrs)))))))
|#

#+page
(defun get-vit-strings (parse)
  (setf *mrs-wg-liszt* (loop for form in (main::output-stream main::*scanner*)
			   collect
			     (list (string-left-trim "'"
					     (main::typed-item-form form)))))
  (when parse 
    (let* ((fs (if (eq (type-of parse) 'pg::combo-item)
		   (get-parse-fs-alt parse)
		 (get-parse-fs parse)))
	   (sem-fs (path-value fs *initial-semantics-path*)))
      (if (is-valid-fs sem-fs)
	  (let* ((mrs-struct1 (sort-mrs-struct (construct-mrs sem-fs)))
		 (mrs-struct (if (boundp '*ordered-mrs-rule-list*)
				 (munge-mrs-struct mrs-struct1
						   *ordered-mrs-rule-list*)
			       mrs-struct1)))
	    (multiple-value-bind (vit binding-sets)
		(mrs-to-vit mrs-struct)
	      (with-output-to-string (stream) 
		(format nil "~S" (write-vit stream 
					    (horrible-hack-2 vit))))))))))

#+page
(defun get-vit-strings-from-phrases (parse)
  (setf *mrs-wg-liszt* (loop for form in (main::output-stream main::*scanner*)
                             collect
                               (list (string-left-trim "'"
					 (main::typed-item-form form)))))
  (let* ((fs (if (eq (type-of parse) 'pg::combo-item)
		 (get-parse-fs-alt parse)
	       (get-parse-fs parse)))
	 (sem-fs (path-value fs *initial-semantics-path*)))
          (if (is-valid-fs sem-fs)
              (let* ((mrs-struct1 (sort-mrs-struct (construct-mrs sem-fs)))
		     (mrs-struct (if (boundp '*ordered-mrs-rule-list*)
				     (munge-mrs-struct mrs-struct1
						       *ordered-mrs-rule-list*)
				   mrs-struct1)))
		 (multiple-value-bind (vit binding-sets)
		     (mrs-to-vit mrs-struct)
		   (with-output-to-string (stream) 
		     (format nil "~S" (write-vit stream 
						 (horrible-hack-2 vit)))))))))

#+page
(defun extract-and-output (parse-list)
  (let ((*print-circle* nil))
    (setf *mrs-wg-liszt* (loop for form in (main::output-stream main::*scanner*)
                             collect
                               (list (string-left-trim "'"
					 (main::typed-item-form form)))))
    (loop for parse in parse-list
        do
          (let* ((fs (get-parse-fs parse))
                 (sem-fs (path-value fs *initial-semantics-path*)))
            (if (is-valid-fs sem-fs)
                (let 
                    ((mrs-struct (construct-mrs sem-fs)))
                  (unless *mrs-to-vit*
                    (output-mrs mrs-struct 'simple))
                  (if *mrs-to-vit*
                      (mrs-to-vit-convert mrs-struct)
                    (if *mrs-scoping-p*
                        (scope-mrs-struct mrs-struct)))
                  (when *mrs-results-check*
                    (let ((sorted-mrs-struct (sort-mrs-struct mrs-struct))
			  (previous-result
                           (gethash (remove-trailing-periods
                                     (get-last-sentence))
                                    *mrs-results-table*)))
                      (if previous-result
                          (unless (mrs-equalp sorted-mrs-struct previous-result)
                            (when 
                                (y-or-n-p "Differs from previous result.
                                       Replace?")
                              (setf 
                                  (gethash
                                   (remove-trailing-periods
                                    (get-last-sentence))
                                   *mrs-results-table*)
                                sorted-mrs-struct)))
                        (when (y-or-n-p "No previous result.
                                       Add?")
                          (setf 
                              (gethash
                               (remove-trailing-periods
                                (get-last-sentence)) *mrs-results-table*)
                            sorted-mrs-struct)))))))))))
