(in-package :mt)

(defun translate (edge)
  (when (lkb::edge-p edge)
    (let* ((mrs (ignore-errors (mrs::extract-mrs edge)))
           (output (transfer-mrs mrs :filterp nil)))
      (browse-mrss output "Transfer Output")
      (loop
          with *package* = (find-package :lkb)
          with file = (format nil "/tmp/.transfer.~a" (lkb::current-user))
          for edge in output
          for mrs = (edge-mrs edge)
          do
            ;;
            ;; _fix_me_
            ;; we need a synchronized way of talking to the generator server,
            ;; preferably a socket connection.                 (10-feb-04; oe)
            ;;
            (with-open-file (stream file
                             :direction :output :if-exists :supersede)
              (mrs::output-mrs1 mrs 'mrs::simple stream))
            (sleep 2)))))