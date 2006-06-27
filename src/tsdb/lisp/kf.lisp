(in-package :tsdb)

;;;
;;; Copyright (c) 2004 -- 2006 Stephan Oepen (oe@csli.stanford.edu)
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

;;;
;;; _to_do_
;;;
;;; - more types: v+n combinatorics, n+poss+n, et al.
;;; - maybe a notion of inclusion among outputs: do we actually want both the
;;;   noun and nominalization for `opening'?
;;;
#|
(tsdb :cpu :norgram :task :parse :file t)
(tsdb :cpu :erg :task :parse :file t :reset nil)
(rsa "trag")
(process-kf-file
 "~/src/logon/uio/noen/kf/kf_a.dat"
 :simplex "~/src/logon/uio/noen/kf.a.mtr"
 :log "~/src/logon/uio/noen/kf.a.log"
 :active (list (mrs:vsym "adjective_no")))
(process-kf-file
 "~/src/logon/uio/noen/kf/kf_n.dat"
 :simplex "~/src/logon/uio/noen/kf.n.mtr"
 :log "~/src/logon/uio/noen/kf.n.log"
 :active (list (mrs:vsym "noun_no")))
(process-kf-file
 "~/src/logon/uio/nw/compounds/compounds.logon"
 :simplex "~/src/logon/uio/noen/kf.nn.mtr"
 :log "~/src/logon/uio/noen/kf.nn.log")
(process-kf-file
 "~/src/logon/uio/noen/kf/compounds.logon.autotranslated.dat"
 :simplex "~/src/logon/uio/noen/nn.mtr"
 :log "~/src/logon/uio/noen/nn.log")
(process-kf-file
 "~/src/logon/uio/noen/kf/kf.dat"
 :simplex "~/src/logon/uio/noen/kf.mtr"
 :mwe "~/src/logon/uio/noen/kf.mwe.mtr"
 :log "~/src/logon/uio/noen/kf.log")
|#

(defparameter *trag-combinations*
  (list
   (list
    (mrs:vsym "noun_no")
    (mrs:vsym "noun_en") "noun"
    (mrs:vsym "nominalization_en") "n_nominalization"
    (mrs:vsym "n+n_en") "n_n+n"
    (mrs:vsym "n+nominalization_en") "n_n+nominalization"
    (mrs:vsym "nominalization+n_en") "n_nominalization+n"
    (mrs:vsym "adj+n_en") "n_adj+n"
    (mrs:vsym "n+p+q+n_sg_en") "n_n+p+q+n_sg"
    (mrs:vsym "n+p+q+n_pl_en") "n_n+p+q+n_pl")
   (list
    (mrs:vsym "adjective_no")
    (mrs:vsym "adjective_en") "intersective_attribute"
    (mrs:vsym "prp_participle_en") "adjective_prp_participle"
    (mrs:vsym "psp_participle_en") "adjective_psp_participle")
   (list
    (mrs:vsym "n+n_no")
    (mrs:vsym "noun_en") "n+n_n"
    (mrs:vsym "nominalization_en") "n+n_nominalization"
    (mrs:vsym "n+n_en") "n+n_n+n"
    (mrs:vsym "n+nominalization_en") "n+n_n+nominalization"
    (mrs:vsym "nominalization+n_en") "n+n_nominalization+n"
    (mrs:vsym "adj+n_en") "n+n_adj+n"
    (mrs:vsym "n+p+q+n_sg_en") "n+n_n+p+q+n_sg"
    (mrs:vsym "n+p+q+n_pl_en") "n+n_n+p+q+n_pl")
   (list
    (mrs:vsym "v+n_no")
    (mrs:vsym "noun_en") "v+n_n")))

(defun mtr-equivalent-p (mtr1 mtr2)
  (and (equal (third mtr1) (third mtr2))
       (equal (fourth mtr1) (fourth mtr2))
       (equal (fifth mtr1) (fifth mtr2))))

(defun process-kf-file (kf &key simplex mwe log
                                (source :norgram) (target :erg)
                                active)
  (let* ((sstream (create-output-stream simplex))
         (mstream (if mwe (create-output-stream mwe) sstream))
         (lstream (create-output-stream log)))
    (loop 
        for entry in (read-kf-entries kf)
        for in = (first entry)
        do
          (multiple-value-bind (smrss striggers)
              (process-kf-entry in source)

            (format
             lstream
             "~&<|~a| {~a:~a}" in (length smrss) (length striggers))
            (loop
                for trigger in striggers
                do (format lstream " ~(~a~)" (first trigger)))
            (format lstream "~%")
            (when striggers
              (loop
                  with mtrs = (make-array (length striggers))
                  for out in (rest entry)
                  do
                    (multiple-value-bind (tmrss ttriggers)
                        (process-kf-entry out target)
                      
                      (format
                       lstream
                       ">  |~a| {~a:~a}"
                       out (length tmrss) (length ttriggers))
                      (loop
                          with types
                          for (tid . tpreds) in ttriggers
                          do
                            (format lstream " ~(~a~)" tid)
                            (loop
                                for (sid . spreds) in striggers
                                for i from 0      
                                when (or (null active)
                                         (smember sid active))
                                do
                                  (loop
                                      for combinations
                                      = (rest (assoc sid *trag-combinations*))
                                      then (rest (rest combinations))
                                      while combinations
                                      when (eq (first combinations) tid)
                                      do
                                        (pushnew
                                         (list
                                          in out (second combinations)
                                          spreds tpreds)
                                         (aref mtrs i)
                                         :test #'mtr-equivalent-p)
                                        (pushnew (second combinations) types)))
                          finally
                            ;;
                            ;; _fix_me_
                            ;; the count is potentially mis-leading, in that we
                            ;; just report the number of unique output types, 
                            ;; even where the same type may have more than one
                            ;; instantiation (e.g. based on distinct source
                            ;; triggers).                       (30-may-06; oe)
                            ;;
                            (when types
                              (format
                               lstream
                               " ==> {~a} ~{~(~a~)~^ ~}"
                               (length types) types))
                            (format lstream "~%")))
                  finally
                    (loop
                        for bucket across mtrs
                        for n = (length bucket)
                        do
                          (loop
                              for (in out type spreds tpreds)
                              in (nreverse bucket)
                              for i from 0
                              do
                                (output-mtr
                                 in out i n type spreds tpreds
                                 :simplex sstream :mwe mstream))))))
          (format lstream "~%")
          (force-output sstream) (force-output mstream) (force-output lstream))
    (when (stringp simplex) (close sstream))
    (when (stringp mwe) (close mstream))
    (when (stringp log) (close lstream))))

(defun read-kf-entries (file)
  (unless (probe-file file)
    (error "read-kf(): invalid input `~a'" (namestring file)))
  (with-open-file (stream file :direction :input)
    (loop
        with result
        for n from 1
        for line = (read-line stream nil nil)
        while line
        unless (or (ppcre::scan "^\\s*$" line)
                   (ppcre::scan "^[ \\t]*;+" line))
        do
          (multiple-value-bind (foo bar starts ends) 
              (ppcre::scan
               "^[0-9 \\t]*([^\\t]+)\\t+([^\\t]+)(?:\\t+([^\\t]+))?$"
               line)
            (declare (ignore foo bar))
            (if (or (null starts) (null ends)
                    (null (aref starts 0)) (null (aref starts 1))
                    (null (aref ends 0)) (null (aref ends 1)))
              (format
               t
               "read-kf(): ignoring invalid line # ~d:~%  |~a|~%" n line)
              (let* ((index (if (aref starts 2) 2 1))
                     (source (subseq line (aref starts 0) (aref ends 0)))
                     (target
                      (subseq line (aref starts index) (aref ends index)))
                     targets)
                #+:debug
                (format
                 t "{~a} --> {~a}~%" source targets)
                (ppcre:do-scans
                    (foo bar starts ends "([^|]+)(?: \\| |$)" target)
                  (declare (ignore foo bar))
                  (let ((target (subseq target (aref starts 0) (aref ends 0))))
                    ;;
                    ;; _fix_me_
                    ;; KF also uses |sequence/number of years|, apparently for
                    ;; a disjunction local to one token.        (27-may-06; oe)
                    ;;
                    (multiple-value-bind (start end) 
                        (ppcre::scan "\\([^)]+\\)" target)
                      (when (and start end)
                        (let* ((prefix (subseq target 0 start))
                               (prefix (string-trim '(#\space) prefix))
                               (suffix (subseq target end))
                               (suffix (string-trim '(#\space) suffix))
                               (target
                                (format
                                 nil "~@[~a ~]~a"
                                 (and prefix (not (string= prefix "")) prefix)
                                 suffix)))
                          (pushnew
                           (string-trim '(#\space) target)
                           targets :test #'string-equal))))
                    (pushnew
                     (string-trim '(#\space) target)
                     targets :test #'string=)))
                (push (cons source (nreverse targets)) result))))
        finally (return (nreverse result)))))

(defun process-kf-entry (string &optional (processor :norgram))
  (let* ((result (pvm-process
                  string :parse :class processor
                  :semantix-hook (when (member processor '(:erg :jacy))
                                   "mrs::get-mrs-string")))
         (mrss (loop
                   for result in (get-field :results result)
                   for foo = (get-field :mrs result)
                   for mrs = (ignore-errors (mrs::read-mrs-from-string foo))
                   when mrs collect mrs))
         triggers)
    (unless mrss (return-from process-kf-entry))
    (loop
        for mrs in mrss
        for matches
        = (loop
              for edge in (mt::transfer-mrs
                           mrs :task processor)
              when (mt::edge-daughter edge) collect edge)
        do
          (loop
              for match in matches
              for id = (mt::mtr-id (mt::edge-rule match))
              for preds = (loop
                              for ep in (mrs:psoa-liszt (mt::edge-mrs match))
                              collect (mrs:rel-pred ep))
              when (and id preds) do
                (pushnew (cons id preds) triggers :test #'equal)))
    (values mrss triggers)))

(defun output-mtr (in out i n type spreds tpreds
                   &key (simplex t) (mwe t))
  (labels ((normalize (string)
             (loop
                 with length = (length string)
                 with result = (make-array length
                                           :element-type 'character
                                           :adjustable nil :fill-pointer 0)
                 for c across string
                 when (char= c #\space) do (vector-push #\+ result)
                 else unless (member c '(#\( #\))) do (vector-push c result)
                 finally (return result))))
    (let ((*package* (find-package mrs::*mrs-package*)))
      (format
       (if (rest spreds) mwe simplex)
       "~(~a~)_~(~a~)_~a := ~(~a~)_~:[omtr~;mtr~] &~%~
        [ INPUT.RELS <~{ [ PRED ~(~s~) ]~^,~}, ... >,~%  ~
          OUTPUT.RELS <~{ [ PRED ~(~s~) ]~^,~}, ... >,~%  ~
          FLAGS [ RANK \"~a\", COUNT \"~a\", ~
                  AUTHOR \"~a\", DATE \"~a\" ] ].~%~%"
       (normalize in) (normalize out)
       i type (= i (- n 1)) spreds tpreds
       i n (current-user) (current-time)))))


