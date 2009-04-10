(in-package mrs)
(setf *rasp-rmrs-gram-file* "/local/scratch/cr351/lkb/src/rmrs/rasp3/gram15.rmrs")
(setf *rasp-rmrs-tag-file* "/local/scratch/cr351/lkb/src/rmrs/rasp3/lex15.rmrs")
(setf *rasp-xml-word-p* t)
(setf *rasp-xml-type* :none)

(clear-rule-record)
(read-rmrs-grammar *rasp-rmrs-gram-file*)
(read-rmrs-tag-templates *rasp-rmrs-tag-file*)

(in-package mrs)

(defun r-server-rasp-input (s input &key (mode :string))
  (case mode
    (:string
     (format t "~&;;; processing input: ~w" (length input))
;	     (length input))
;     (format t "~&;;; debugging: ~a" input)
;     (format s "~&~a" input)
     (with-input-from-string (istream input)
       (setf (stream-external-format s) :utf-8)
       (let* ((tagged (read istream nil nil))
	      (number (read istream nil nil))
	      (tree (read istream nil nil)))
	 (declare (ignore number))
	 (unless tree
	   (return))
	 (when tree
	   (mrs::construct-sem-for-tree 
	    tree
	    :rasp s tagged))))
     )
    (t
     (error "unknown test server mode '~a'" mode))
    ))

(defun run-rasp-server (&key (port 8891) (mode :string))
  (saf::r-server (lambda (x y)
	      (r-server-rasp-input x y :mode mode))
	    "RASP"
	    :port port))

; (with-output-to-string (out) (with-open-file  (istream "/tmp/cr351_13692/b204433c.xml.pollard":direction :input)(loop
; 	(let* ((tagged (read istream nil nil))
; 	       (number (read istream nil nil))
; 	       (tree (read istream nil nil)))
; 	  (declare (ignore number))
; 	  (unless tree
; 	    (return))
; 	  (when tree
; 	    (mrs::construct-sem-for-tree 
; 	     tree
; 	     :rasp out tagged))))))

