(in-package :mt)

(defparameter *transfer-show-output-p* t)

(defun rephrase (edge)
  (when (lkb::edge-p edge)
    (let* ((mrs (ignore-errors (mrs::extract-mrs edge)))
           (output (transfer-mrs mrs :filter nil :task :paraphrase)))
      (when *transfer-show-output-p*
        (browse-mrss output "Transfer Output"))
      (loop
          for target in (rest lkb::*translate-grid*)
          for file = (merge-pathnames
                      (lkb::lkb-tmp-dir)
                      (format
                       nil
                       ".transfer.~a.~(~a~)"
                       (lkb::current-user) target))
          do
            (with-open-file (stream file
                             :direction :output :if-exists :supersede)
              (loop
                  with *package* = (find-package :lkb)
                  for edge in output
                  for mrs = (edge-mrs edge)
                  do
                    (mrs::output-mrs1 mrs 'mrs::simple stream)))))))

