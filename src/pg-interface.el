;;; Copyright (c) 2003 Ben Waldron
;;; see licence.txt for conditions

;;; Add a PG menu to the emacs menu bar

(load "pg.elc")

(defconst pg-menu
    '("PG"
      ("Lexical Entry"
       ["Edit" load-record-buffer t]
       ["Commit Buffer"  commit-record-buffer t]
       ["Normalise Buffer"  normalise-buffer t]  
       )))
      
(fi::install-menubar pg-menu)

;;;
;;;
;;;

(defun default-load-record-id ()
  'a-det)

(defun load-record-buffer (id)
  (interactive (list (read-from-minibuffer "Lex Id: ")))
  (let* (
	(buffer (format "%s" id))
	(full-record (retrieve-record id))
	(record (record-to-edit full-record))
	)
    (set-record-buffer record buffer)))

(defun commit-record-buffer (buffer)
  (interactive (list (current-buffer)))
  (let* (
	(record (read-record-buffer buffer))
	)
    (store-record record)))

(defun normalise-buffer (buffer)
  (interactive (list (current-buffer)))
  (set-record-buffer (read-record-buffer buffer) buffer))

;;;
;;;
;;;

(defun set-record-buffer (record buffer)
  (switch-to-buffer buffer)
  (erase-buffer)
  (mapcar #'(lambda (x) (insert (upcase (car x)) (make-string (max 0 (- 15 (length (car x)))) ? ) ":\t" (x-to-str (cdr x)) "\n")) 
	  record)
  buffer)

(defun read-record-buffer (buffer)
  (save-current-buffer
  (set-buffer buffer)
  (let* ((lines (split-string (buffer-string) "\n"))
	 (fv-pairs (remove nil (mapcar #'get-field-val lines))))
    fv-pairs)))
  
(defun get-field-val(line)
  (let ((i 0) (l (length line)) (field "") (val ""))
    (while
	(and
	 (< i l)
	 (not (equal (substring line i (1+ i)) ":")))
      (setf i (1+ i)))
    (unless (= i l)
      (setf val (cut-white-space (substring line (1+ i) l)))
      (setf field (cut-white-space (substring line 0 i)))
      )	   
    (if (equal val "") (setf val nil))
    (unless (equal field "")
      (cons (upcase field) val))))

(defun cut-white-space (str)
  (mapconcat #'(lambda (x) x) (remove "" (split-string str)) " "))

;;;
;;;
;;;

(defun retrieve-record (id)
  (with-pg-connection 
      con ("lingo" "guest" "guest")
      (let* (
	    (res (pg:exec con (print (format "SELECT * FROM erg_max_version WHERE name='%s';" id))))
	    (tuple (pg:result res :tuple 0))
	    (attributes (pg:result res :attributes))
	    )
	(if tuple (cons-list (mapcar #'first attributes) tuple)))))

(defun cons-list (x y)
  (mapcar #'(lambda (a) (cons a (pop y))) x))


(defun attribs-to-edit nil
  '("name" "type" "orthography" "orthkey" "pronunciation" "keyrel" "altkey" "alt2key" "keytag" "compkey" "ocompkey" "complete" "semclasses" "preferences" "classifier" "selectrest" "jlink" "comments" "exemplars" "usages" "lang" "country" "dialect" "domains" "genres" "register" "confidence" "flags"))

(defun x-to-str (x)
  (cond
   ((stringp x) x)
   ((numberp x) (format "%s" x))
   (t "")))

;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun store-record (record-in)
  (with-pg-connection 
      con ("lingo" "guest" "guest")
      (let* (
	     (record (mapcan '(lambda (x) (if (cdr x) (list x))) record-in))
	     (record (set-val "version" (next-version (val "name" record)) record))
	     (record (set-val "id" (next-id) record)) 
	     (record (set-val "source" "Emacs" record))
	     (record (set-val "moddate" 'CURRENT_DATE record))
	     (record (set-val "userid" (user-login-name) record))
	     (fields (mapconcat #'(lambda (x) (car x)) record ", "))
	     (vals (mapconcat #'(lambda (x) (make-sql-val (cdr x))) record ", "))
	     (sql-str (format "INSERT INTO erg (%s) VALUES (%s)" fields vals))
	     )
	(pg:exec con sql-str)
	sql-str)))

(defun set-val (f v record)
  (let ((fv-pair (cons f v)))
    (delete (assoc f record) record)
    (push fv-pair record)))

(defun record-to-edit (full-record)
  (mapcar #'(lambda (x) (assoc x full-record)) (attribs-to-edit)))

(defun make-sql-val (x)
  (cond
   ((null x) "")
   ((stringp x)(format "'%s'" x))
   ((symbolp x)(format "%s" x))
   ((numberp x)(format "%s" x))
   (t
    "")))

(defun retrieve-val (sql-str &optional default)
  (unless default (setf default 0))
  (with-pg-connection 
      con ("lingo" "guest" "guest")
      (let* (
	    (res (pg:exec con sql-str))
	    (val (car (pg:result res :tuple 0)))
	    )
	(if val 
	    val
	  default))))

(defun next-version (id)
  (retrieve-val (format "SELECT next_version('%s')" id) 0))

(defun next-id nil
  (retrieve-val (format "SELECT next_id()") 0))

(defun val (field record)
  (cdr (assoc (upcase field) record)))
