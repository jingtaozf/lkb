;; Copyright (c) 2006
;;;   Ben Waldron;
;;; see `licence.txt' for conditions.

(in-package :common-lisp-user)

(defpackage :saf
  (:use :common-lisp)
  (:export ))

(in-package :saf)

;;;
;;; socket SERVER code
;;;

(defvar *r-server-debug* nil)

;; characters not allowed in XML
(defvar *xml-bad-chars*
    '(
      #\^a 
      #\^b 
      #\^c 
      #\^d 
      #\^e 
      #\^f 
      #\bel 
      #\Backspace 
      ;#\Tab 
      ;#\Newline 
      #\vt 
      #\Page 
      ;#\Return 
      #\^n 
      #\^o 
      #\^p 
      #\^q 
      #\^r 
      #\^s 
      #\^t 
      #\^u 
      #\^v 
      #\^w 
      #\^x 
      #\^y 
      #\^z 
      #\esc 
      #\fs 
      #\^] 
      #\^^ 
      #\^_ 
      ))

;; read input chunk delimited by term-char followed by newline
;; ignore non-xml chars while constructing chunk
(defun read-input (stream &key (term-char (code-char 17)))
  (loop
      with strm = (make-string-output-stream)
      for c = (read-char stream nil nil)
      when (svr-debug :raw-char-in)
      do (print c)
      while (and c (not (char= c term-char)))
      unless (or
	      (char= c #\Return)
	      (member c *xml-bad-chars* :test 'char=))
      do 
	;(print c)
	(write-char c strm)
      finally 
	(if (not c) (return-from read-input :eof))
	(read-line stream nil nil)
	(return
	  (get-output-stream-string strm))))

;; remove 
 from end of line
;; (for use with telnet client)
(defun clean-line (line)
  (when (stringp line)
    (let ((len (length line)))
      (when (string= (subseq line (- len 1)) "
")
	(subseq line 0 (- len 1))))))
;;

;; PROTOCOL:
;; - Lisp's r-server opens socket on given port
;;   and accepts connection from client
;; - client sends (XML, UTF-8) input, terminated by CONTROL-Q
;; - server sends (XML, UTF-8) result, terminated by CONTROL-Q
;; - by sending input consisting solely of whitespace, client signals intention to exit
;; - client closes socket, server closes socket

;; generic socket server
;; caller supplys processor function (and name)
;; debug takes a list of keywords from the set:
;; :raw-char-in :input-chunk
(defun r-server (processor name &key (port 9876) debug)
  (setf *r-server-debug* debug)
  (format t "~&;;; [entering ~a server mode]" name)
  (format t "~&;;; starting server on port ~a" port)
  (let ((socket (socket::make-socket :connect :passive :local-port port)))
    (unwind-protect
	(handler-case
	    (r-server-accept-connections socket processor)
	  (error (condition)
	    (print condition)
	    #-:sbcl (format t  "~&Error: ~A" condition)
	    #+:sbcl (format t  "~&Error: ~S" condition)
	    ))
      (close socket)
      (format t "~&;;; [exiting server mode]"))))

;; process connections forever
(defun r-server-accept-connections (socket processor)
  (loop
    (r-server-process-connection socket processor)))

;; process single connection
(defun r-server-process-connection (socket processor)
  (format t "~&;;; waiting for connection")
  (let ((s (socket::accept-connection socket)))
    (unwind-protect
	(progn
	  ;; use UTF-8 when communicating with socket
	  ;; FIXME: works by default under sbcl, but interface
	  ;;        is incomplete 
	  #-:sbcl (setf (stream-external-format s) :utf-8)
	  (format t "~&;;; connection established")
	  (r-server-process-input s processor))
      (close s)
      (format t "~&;;; connection closed"))))

;; process single input chunk
(defun r-server-process-input (s processor)
  (loop
      for input = (handler-case
		      (read-input s)
		    (error (condition)
		      (format t  "~&Error: ~A" condition)
		      :eof))
;;      for empty-input = (lxml::xml-whitespace-p input)
      for empty-input = (and (stringp input)
 			     (net.xml.parser::all-xml-whitespace-p input))
      when (svr-debug :input-chunk)
      do (format t "~&I: ~a" input)
      while (not (eq :eof input))
      if empty-input
      do (format t "~&;;; empty input!")
      when (not empty-input)
      do 
	(handler-case
	    (funcall processor s input)
	  (error (condition)
	    (format t  "~&Error: ~A" condition)))
      ;;do
	 (format s "~a" #\^q)
	 (terpri s)
	 (force-output s)
	 ))


(defun svr-debug (type)
  (member type *r-server-debug*))

;;
;; RASP SERVER
;; (contributed by CJ Rupp)
;;

(in-package mrs)

;#+:rasp-server
(defvar *rasp-rmrs-gram-file*)
;#+:rasp-server
(defvar *rasp-rmrs-tag-file*)
;#+:rasp-server
(defvar *rasp-xml-word-p*)
;#+:rasp-server
(defvar *rasp-xml-type*)

;#+:rasp-server
(defun init-rasp-server (&optional (rmrs-gram-file "/home/bmw20/lkb/src/rmrs/rasp3/gram15.rmrs")
				   (rmrs-tag-file "/home/bmw20/lkb/src/rmrs/rasp3/lex15.rmrs"))
    
  (setf *rasp-rmrs-gram-file* rmrs-gram-file )
  (setf *rasp-rmrs-tag-file* rmrs-tag-file)
  (setf *rasp-xml-word-p* t)
  (setf *rasp-xml-type* :none)
  
  (clear-rule-record)
  (read-rmrs-grammar rmrs-gram-file)
  (read-rmrs-tag-templates rmrs-tag-file))
  
;#+:rasp-server
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
	 ;(unless tree
	 ;  (return))
	 (when tree
	   (construct-sem-for-tree 
	    tree
	    :rasp s tagged))))
     )
    (t
     (error "unknown test server mode '~a'" mode))
    ))

;#+:rasp-server
(defun run-rasp-server (&key (port 8891) (mode :string) debug)
  (saf::r-server (lambda (x y)
	      (r-server-rasp-input x y :mode mode))
	    "RASP"
	    :port port
	    :debug debug))

