(in-package :tsdb)

(defvar *ptb-use-pos-tags-p* nil)

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
          when (probe-file file) do
            (format t "~&now opening `~a':~%" file)
            (with-open-file (stream file :direction :input)
              (loop
                  with open = 0
                  with last = #\(
                  for (token . break) = (read-token stream '(#\( #\)))
                  when (and (or (null break) (zerop open))
                            (> (fill-pointer buffer) 0)) 
                  do
                    (push (copy-seq buffer) result)
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
                    (setf last break))))
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
   ((and (listp tree) (every #'stringp tree)) (list tree))
   ((listp tree)
    (loop for node in tree append (extract-ptb-leaves node)))
   (t nil)))

(defun rewrite-ptb-token (token pos)
  (cond
   ((string-equal pos "cd") "TwoDigitErsatz")
   ((string-equal token "-lrb-") "(")
   ((string-equal token "-rrb-") ")")
   (t token)))

(defun preprocess-ptb-string (string &key (posp *ptb-use-pos-tags-p*))
  (let (result)
    (loop
        with tree = (read-ptb-from-string string)
        with leaves = (extract-ptb-leaves tree)
        with i = 0
        with id = 41
        for leaf in leaves
        for pos = (first leaf)
        for raw = (second leaf)
        for form = (rewrite-ptb-token raw pos)
        unless (string-equal pos "-none-") do
          (push (format 
                 nil 
                 "(~d, ~d, ~d, 1, \"~a\", 0, null~:[~*~;, ~a 1.00~])" 
                 (incf id) i (incf i) form posp pos)
                result))
    (format nil "~{~a~^ ~}" (nreverse result))))



