(in-package :tsdb)

(defparameter *html-directory*
  (pathname-directory 
   (dir-append (get-sources-dir "tsdb") '(:relative "tsdb" "html"))))

(defun html-output (file &key (stream *tsdb-io*) values)
  (let ((file (make-pathname :directory *html-directory* :name file)))
    (when (probe-file file)
      (if values
        (loop
            with size = (file-size file)
            with buffer = (make-array (* size 2)
                                      :element-type 'character
                                      :adjustable nil :fill-pointer 0)
            with in = (open file :direction :input)
            for c = (read-char in nil nil)
            while c do (vector-push c buffer)
            finally
              (close in)
              (apply #'format stream buffer values))
        (with-open-file (in file :direction :input)
          (loop
              for c = (read-char in nil nil)
              while c do (write-char c stream)))))))

    