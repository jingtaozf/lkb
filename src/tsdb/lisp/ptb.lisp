(in-package :tsdb)

(defvar *ptb-use-pos-tags-p* t)

(defun read-token (stream breaks)
  (loop
      with length = 2048
      with result = (make-array length
                                :element-type 'character
                                :adjustable nil :fill-pointer 0)
      with whitespace = '(#\space #\tab #\newline #\page)
      with prefixp = t
      for c = (read-char stream nil nil)
      when (member c whitespace :test #'eql) do
        (unless prefixp
          (setf c (peek-char t stream nil nil))
          (when (member c breaks :test #'eql) (read-char stream nil nil))
          (return (cons result c)))
      else when (or (member c breaks :test #'eql) (null c)) do
        (return (cons result c))
      else do
        (setf prefixp nil)
        (vector-push-extend c result length)))

(defun read-ptb-directory (path)
  (let* ((path (if (stringp path) path (namestring path)))
         (pattern (make-pathname :directory path :name :wild))
         (files (directory pattern))
         (buffer (make-array 4096
                             :element-type 'character
                             :adjustable t :fill-pointer 0))
         result)
      (loop
          for file in files
          for base = (pathname-name (pathname file))
          for name = (intern (string-upcase base))
          when (probe-file file) do
            (format t "~&now opening `~a':~%" file)
            (with-open-file (stream file :direction :input)
              (loop
                  with foo = nil
                  with open = 0
                  with last = #\(
                  for (token . break) = (read-token stream '(#\( #\)))
                  when (and (or (null break) (zerop open))
                            (> (fill-pointer buffer) 0)) 
                  do
                    (push (cons name (copy-seq buffer)) foo)
                    (setf (fill-pointer buffer) 0)
                    (setf last #\()
                  when (eql break #\() do
                    (incf open)
                  when (eql break #\)) do
                    (decf open)
                  while break do
                    (loop 
                        for c across token
                        do
                          (vector-push-extend c buffer 4096))
                    (cond
                     ((eql break #\()
                      (unless (and (zerop (length token))
                                   (eql last break))
                        (vector-push-extend #\space buffer 4096))
                      (vector-push-extend break buffer 4096))
                     ((eql break #\))
                      (vector-push-extend break buffer 4096))
                     (t
                      (vector-push-extend #\space buffer 4096)))
                    (setf last break)
                  finally (push (nreverse foo) result))))
      (nreverse result)))

(defun read-ptb-from-string (string)
  (with-input-from-string (stream string)
    (read-ptb-from-stream stream)))

(defun read-ptb-from-stream (stream &optional recursionp)
  (when (or recursionp
            (let ((foo (read-token stream '(#\( #\)))))
              (and (equal (first foo) "") (eql (rest foo) #\())))
    (loop
        for (token . break) = (read-token stream '(#\( #\)))
        when (> (length token) 0) collect token into result
        when (eql break #\() 
        collect (read-ptb-from-stream stream t) into result
        when (or (null break) (eql break #\))) do (return result))))

(defun extract-ptb-leaves (tree)
  (cond
   ((and tree (listp tree) (every #'stringp tree)) (list tree))
   ((listp tree)
    (loop for node in tree append (extract-ptb-leaves node)))
   (t nil)))

(defun rewrite-ptb-token (token pos)
  (cond
   ((string-equal pos "nnp") "NameErsatz")
   ((member pos '("``" "''" "," "\"") :test #'string=) "")
   #+:null
   ((string-equal pos "cd") "TwoDigitErsatz")
   ((string-equal pos "-lrb-") "(")
   ((string-equal pos "-rrb-") ")")
   (t (if lkb::*preprocessor*
        (lkb::preprocess token :globalp nil :format :lkb :verbose nil)
        token))))

(defun ptb-preprocess (string 
                       &key rawp (plainp t) (posp *ptb-use-pos-tags-p*))
  (let ((length 0)
        (result nil))
    (loop
        with tree = (read-ptb-from-string string)
        with leaves = (extract-ptb-leaves tree)
        with i = 0
        with id = 41
        for leaf in leaves
        for pos = (first leaf)
        for raw = (second leaf)
        for form = (rewrite-ptb-token raw pos)
        unless (or (string-equal pos "-none-")
                   (and rawp (string= raw ""))
                   (and (not rawp) (string= form ""))) do
          (cond
           (rawp (push raw result))
           (plainp (push form result))
           (t
            (push (format 
                   nil 
                   "(~d, ~d, ~d, 1, \"~a\" \"~a\", 0, \"null\"~
                    ~:[~*~;, \"~a\" 1.00~])" 
                   (incf id) i (incf i) form raw posp pos)
                  result)))
          (incf length))
    (values (and result (format nil "~{~a~^ ~}" (nreverse result))) length)))

(defun ptb-preprocess-for-pet (string)
  (ptb-preprocess string :rawp nil :plainp nil :posp t))

#+:null         
(eval-when #+:ansi-eval-when (:load-toplevel :execute)
	   #-:ansi-eval-when (load eval)
  (setf (gethash :i-input *statistics-readers*)
    #'(lambda (string)
        (let ((*package* (find-package :tsdb)))
          (preprocess-ptb-string string :plainp t)))))

