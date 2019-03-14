(in-package :mt)

;;;
;;; Copyright (c) 2004 -- 2018 Stephan Oepen (oe@csli.stanford.edu)
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as published by
;;; the Free Software Foundation; either version 2.1 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
;;; License for more details.
;;; 

(defparameter *transfer-show-output-p* t)

(let ((lock (mp:make-process-lock)))
  ;; JAC 13-Jan-2017: added lock since temp file names are not guaranteed to be unique
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
          (mp:with-process-lock (lock)
            (with-open-file (stream file
                             :direction :output :if-exists :supersede
                             #+:ccl :sharing #+:ccl :lock)
              (loop
                with *package* = (find-package :lkb)
                for edge in output
                for mrs = (edge-mrs edge)
                do
                (mrs::output-mrs1 mrs 'mrs::simple stream)))))))))

