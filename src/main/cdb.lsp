;;; cdb.lsp
;;;  R. Malouf (23-Apr-1999)
;;;
;; This file implements a simple constant database.  It's pretty closely based
;; on cdb-0.55 by D. J. Bernstein (djb@pobox.com), re-implemented in Common
;; Lisp for the sake of portability.
;;
;; A cdb contains 256 pointers to linearly probed open hash tables. The hash
;; tables contain pointers to (key,data) pairs. A cdb is stored in a single
;; file on disk:
;;
;;    +--------+---------+-------+-------+-----+---------+----------------+
;;    | header | records | hash0 | hash1 | ... | hash255 | p0 p1 ... p255 |
;;    +--------+---------+-------+-------+-----+---------+----------------+
;;
;; The header consists of the string "CDB", a zero byte (which someday might
;; by the number of keys), and the position of the pointer table.
;;
;; Each of the 256 initial pointers states a position and a length. The
;; position is the starting byte position of the hash table. The length is the
;; number of slots in the hash table.
;;
;; Records are stored sequentially, without special alignment. A record states
;; a key length, a data length, the key, and the data.
;;
;; Each hash table slot states a hash value and a byte position. If the byte
;; position is 0, the slot is empty. Otherwise, the slot points to a record
;; whose key has that hash value.
;;
;; Positions, lengths, and hash values are 32-bit quantities, stored in
;; little-endian form in 4 bytes. Thus a cdb must fit into 4 gigabytes.
;;
;; A record is located as follows. Compute the hash value of the key in the
;; record. The hash value modulo 256 is the number of a hash table.  The hash
;; value divided by 256, modulo the length of that table, is a slot
;; number. Probe that slot, the next higher slot, and so on, until you find
;; the record or run into an empty slot.
;;
;; The cdb hash function is ``h = ((h << 5) + h) ^ c'', with a starting hash
;; of 5381.
;;
;; This all could be much more efficient and robust, but it seems to work well
;; enough for now.

(defpackage "CDB")

(in-package :cdb)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(open-write close-write open-read close-read write-record 
	    read-record)))

(defstruct (cdb)
  (stream nil)
  (num 0)
  (mode nil)
  (tables (make-array '(256) :initial-element nil))
  (count (make-array '(256) :initial-element 0)))

(defmethod common-lisp:print-object ((instance cdb) stream)
    (format stream "#<cdb with ~A entries>" (cdb-num instance)))

;; Write a four-byte integer as four consecutive characters

(defun write-fixnum (num stream)
  (dotimes (x 4)
    (write-char (code-char (ldb (byte 8 0) num)) stream)
    (setq num (ash num -8)))
  (unless (zerop num)
    (error "Overflow in CDB!")))

;; Read four consecutive characters as a four-byte integer

(defun read-fixnum (stream)
  (let ((num 0)
	(c nil))
    (loop for i from 0 to 24 by 8
	do
	  (setq c (read-char stream))
	  (setq num (+ num (ash (char-code c) i))))
    num))

;; The hash function, the same as the one used by D. J. Bernstein.  It might
;; be worth it to investigate perfect hashing someday, but for now this works
;; pretty well.

(defun hash (key)
  (let ((h 5381))
    (loop for c across key
	do
	  (setq h (ldb (byte 32 0) (+ h (ash h 5))))
	  (setq h (logxor h (char-code c))))
    h))

;; Open a database file for writing

(defun open-write (filename)
  (let ((cdb (make-cdb)))
    (with-slots (stream) cdb
      ;; Open database file
      (handler-case
	  (setf stream (open filename
			     :direction :output
			     :if-exists :supersede
			     :if-does-not-exist :create))
	(error (condition)
	  (format t "~%~A" condition)
	  (error "~%Can't open database file ~A" filename)))
      ;; Write blank header
      (unless (file-position stream 8)
	(close stream)
	(error "~%Error writing database file ~A" filename)))
    (setf (cdb-mode cdb) :output)
    cdb))

;; Store a record in the database

(defun write-record (cdb key data)
  (unless (eq (cdb-mode cdb) :output)
    (error "Database not open for output."))
  (with-slots (stream) cdb
    ;; Store in hash table as ( hash . pos )
    (let* ((h (hash key))
	   (tbl (logand 255 h)))
      (push (cons h (file-position stream))
	    (aref (cdb-tables cdb) tbl))
      (incf (aref (cdb-count cdb) tbl))
      (incf (cdb-num cdb)))
    ;; Write record
    (write-fixnum (length key) stream)
    (write-fixnum (length data) stream)
    (write-string key stream)
    (write-string data stream)))

;; Write out hash tables and close a database

(defun close-write (cdb)
  (unless (eq (cdb-mode cdb) :output)
    (error "Database not open for output."))
  (let ((final (make-array '(256)))
	(hash (make-array (list (loop for c across (cdb-count cdb)
				    maximize c))))
	where)
    (with-slots (stream) cdb
      ;; We have all the keys, now we have to construct hash tables
      (loop for tbl from 0 to 255
	  do
	    (let ((len (aref (cdb-count cdb) tbl)))
	      ;; Initialize hash table
	      (loop for h from 0 to (1- (length hash))
		  do (setf (aref hash h) (cons 0 0)))
	      ;; Store location of this hash table
	      (setf (aref final tbl) (cons (file-position stream) len))
	      (unless (zerop len)
		;; Construct table
		(loop for p in (aref (cdb-tables cdb) tbl)
		    do 
		      (setq where (mod (ash (car p) -8) len))
		      (loop until (zerop (cdr (aref hash where)))
			  do 
			    (when (eql (incf where) len)
			      (setq where 0)))
		      (setf (aref hash where) p))
		;; Write hash table
		(loop for (h . p) across hash
		    do
		      (write-fixnum h stream)
		      (write-fixnum p stream)))))
      ;; Write out pointers to tables
      (let ((pos (file-position stream)))
	(loop for (p . l) across final
	    do
	      (write-fixnum p stream)
	      (write-fixnum l stream))
	;; Write header and close.  The file header is the string "CDB", a
	;; null byte, and a four-byte pointer to the table of tables.
	(file-position stream 0)
	(write-string "CDB" stream)
	(write-char (code-char 0) stream)
	(write-fixnum pos stream)
	(setf (cdb-mode cdb) nil)
	(setf stream (close stream))))))

;; Open a CDB file for reading

(defun open-read (filename)
  (let ((cdb (make-cdb)))
    (with-slots (stream) cdb
      ;; Open database file
      (handler-case
	  (setf stream (open filename
			     :direction :input
			     :if-does-not-exist :error))
	(error (condition)
	  (error "~%Error ~A in opening database file ~A" 
		 condition filename)))
      ;; Read header
      (let ((magic (make-string 3)))
	(read-sequence magic stream)
	(unless (and (equal magic "CDB")
		     (zerop (char-code (read-char stream))))
	  (setf stream (close stream))
	  (error "~%Invalid CDB database ~A" filename))
	;; Read index
	(let ((count 0))
	  (file-position stream (read-fixnum stream))
	  (loop for i from 0 to 255 
	      do
		(setf (aref (cdb-tables cdb) i)
		  (cons (read-fixnum stream)
			(read-fixnum stream)))
		(incf count (cdr (aref (cdb-tables cdb) i))))
	  (setf (cdb-num cdb) count))))
    (setf (cdb-mode cdb) :input)
    cdb))

;; If the file is open for reading, close it.  If not, don't do anything.

(defun close-read (cdb)
  (cond ((eq (cdb-mode cdb) :input)
	 (setf (cdb-mode cdb) nil)
	 (setf (cdb-stream cdb) (close (cdb-stream cdb))))
	(t cdb)))

;; Read in a record from stream at pos

(defun grab-record (pos stream)
  (let ((old (file-position stream)))
    (file-position stream pos)
    (let ((key (make-string (read-fixnum stream)))
	  (data (make-string (read-fixnum stream))))
      (read-sequence key stream)
      (read-sequence data stream)
      (file-position stream old)
      (cons key data))))

;; Search hash table for a matching entry

(defun scan-forward (stream key hash origin start end not-first)
  (let (h)
    (loop 
	do 
	  (when (eql end (file-position stream))
	    (file-position stream origin))
	  (when (and not-first (eql start (file-position stream)))
	    (return-from scan-forward nil))
	  (setf not-first t)
	  (setf h (read-fixnum stream))
	  (cond ((zerop h) (return-from scan-forward nil))
		((eql hash h) (return))
		(t (read-fixnum stream)))))
  ;; If we get here, we must have found the record
  (let ((result (grab-record (read-fixnum stream) stream)))
    (when (equal (car result) key)
      (cdr result))))

;; Look up a key in a CDB

(defun read-record (cdb key)
  (unless (eq (cdb-mode cdb) :input)
    (error "Database not open for input."))
  (with-slots (stream) cdb
    (let* ((h (hash key))
	   (tbl-pos (car (aref (cdb-tables cdb) (logand 255 h))))
	   (tbl-len (cdr (aref (cdb-tables cdb) (logand 255 h)))))
      (unless (zerop tbl-len) 
	;; Go to expected slot and scan forward until we find a match
	(let ((start (+ tbl-pos (* 8 (mod (ash h -8) tbl-len))))
	      (end (+ tbl-pos (* 8 tbl-len)))
	      result)
	  (file-position stream start)
	  (let ((r (scan-forward stream key h tbl-pos start end nil)))
	    (when r
	      (push r result)
	      (loop 
		  do 
		    (let ((r (scan-forward stream key h tbl-pos start end t)))
		      (if r
			  (push r result)
			(return))))))
	  result)))))
