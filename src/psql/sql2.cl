(in-package :pg)

;; 21/01/04 Ben Waldron: code to handle input from stdin

(defun wrap-putline (con pgline)
  (unless (= 0 (putline con pgline))
    (warn "BAD LINE TO ~a: '~a'" (db con) pgline)
    (throw 'sql-error pgline)))

(defun wrap-endcopy (con)
  (unless (= 0 (endcopy con))
    (warn "COPY ERROR (~a)" 
	  (db con) )
    (throw 'sql-error 'endcopy)))

(defun stdin-command-istream (con sql-str istream)
  (let ((result (exec con sql-str)))
    (unwind-protect
	(let ((exec-status (decode-exec-status (result-status result))))
	  (unless (eq exec-status :PGRES-COPY-IN)
	    (warn "BAD COMMAND TO ~s: ~a" 
		  (db con) 
		  (result-error-message result))
	    (throw 'sql-error exec-status))
	  (do* ((line (read-line istream nil) (read-line istream nil))
		(pgline (format nil "~a~%" line) (format nil "~a~%" line)))
	      ((null line))
	    (wrap-putline con pgline))
	  (wrap-putline con (format nil "\\.~%"))
	  )
      (progn (clear result)
	     (wrap-endcopy con)))))

(defun stdin-command-file (con sql-str filename)
  (with-open-file (istream 
		   filename
		   :direction :input)
    (stdin-command-istream con sql-str istream)))

(defun connect-db-with-handler (&rest rest)
  (handler-case
      (apply #'connect-db rest)
    (simple-error () (error "PostgreSQL functionality not loaded"))))
