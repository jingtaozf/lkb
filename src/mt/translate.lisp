(in-package :mt)

(defun translate (edge)
  (when (lkb::edge-p edge)
    (let* ((mrs (ignore-errors (mrs::extract-mrs edge)))
           (output (transfer-mrs mrs :filterp nil))
           (file (format nil "/tmp/.transfer.~a" (lkb::current-user))))
      (browse-mrss output "Transfer Output")
      (with-open-file (stream file
                       :direction :output :if-exists :supersede)
        (loop
            with *package* = (find-package :lkb)
            for edge in output
            for mrs = (edge-mrs edge)
            do
              (mrs::output-mrs1 mrs 'mrs::simple stream))))))
