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
(load (format nil "~a/dot.tsdbrc" (getenv "LOGONROOT")))
(tsdb :cpu :norgram :task :parse :file t)
(tsdb :cpu :erg :task :parse :file t :reset nil)
(lkb:read-script-file-aux "~/src/logon/uio/trag/lkb/script")
(initialize-kf)
(process-kf
 (read-andre "~/src/logon/uio/noen/andre.n.dat")
 :simplex "/tmp/andre.n.mtr"
 :mwe "/tmp/andre.nn.mtr"
 :log "/tmp/andre.n.log"
 :active (list (mrs:vsym "noun_no") (mrs:vsym "v*n_no") (mrs:vsym "n*n_no")))
(process-kf
 (read-unknown "~/src/logon/uio/noen/unknown.n.dat")
 :simplex "/tmp/unknown.n.mtr"
 :log "/tmp/unknown.n.log"
 :source (mrs:vsym "noun_no")
 :active (list (mrs:vsym "noun_no")))
(process-kf
 (read-unknown "~/src/logon/uio/noen/unknown.a.dat")
 :simplex "/tmp/unknown.a.mtr"
 :log "/tmp/unknown.a.log"
 :source (mrs:vsym "adjective_no")
 :active (list (mrs:vsym "adjective_no")))
(process-kf
 (kf-list :category :noun)
 :simplex "/tmp/kf.n.mtr"
 :mwe "/tmp/kf.nn.mtr"
 :log "/tmp/kf.n.log"
 :active (list (mrs:vsym "noun_no") (mrs:vsym "v*n_no") (mrs:vsym "n*n_no")))
(process-kf
 (kf-list :category :adjective)
 :simplex "/tmp/kf.a.mtr"
 :log "/tmp/kf.a.log"
 :active (list (mrs:vsym "adjective_no")))
(process-kf
 "~/src/logon/uio/nw/compounds/compounds.logon"
 :simplex "/tmp/nn.mtr"
 :log "/tmp/nn.log")
|#


(defparameter *kf* nil)

(defparameter *kf-files*
  (list
   "PE1_a.xml" "PE1_aa.xml" "PE1_ae.xml"
   "PE1_b.xml" "PE1_c.xml" "PE1_d.xml"
   "PE1_e.xml" "PE1_f.xml" "PE1_g.xml" "PE1_h.xml"
   "PE1_i.xml" "PE1_j.xml" "PE1_k.xml"
   "PE1_l.xml" "PE1_m.xml" "PE1_n.xml"
   "PE1_o.xml" "PE1_oe.xml" "PE1_p.xml" "PE1_q.xml"
   "PE1_r.xml" "PE1_s.xml" "PE1_t.xml"
   "PE1_u.xml" "PE1_v.xml" "PE1_w.xml"
   "PE1_x.xml" "PE1_y.xml" "PE1_z.xml"))

(defparameter *trag-combinations*
  (list
   (list
    (mrs:vsym "noun_no")
    (mrs:vsym "noun_en") "noun"
    (mrs:vsym "nominalization_en") "n_nominalization"
    (mrs:vsym "n*n_en") "n_n+n"
    (mrs:vsym "n*nominalization_en") "n_n+nominalization"
    (mrs:vsym "nominalization*n_en") "n_nominalization+n"
    (mrs:vsym "adj+n_en") "n_adj+n"
    (mrs:vsym "n+p+q+n_sg_en") "n_n+p+q+n_sg"
    (mrs:vsym "n+p+q+n_pl_en") "n_n+p+q+n_pl")
   (list
    (mrs:vsym "adjective_no")
    (mrs:vsym "adjective_en") "intersective_attribute"
    (mrs:vsym "prp_participle_en") "adjective_prp_participle"
    (mrs:vsym "psp_participle_en") "adjective_psp_participle")
   (list
    (mrs:vsym "n*n_no")
    (mrs:vsym "noun_en") "n+n_n"
    (mrs:vsym "nominalization_en") "n+n_nominalization"
    (mrs:vsym "n*n_en") "n+n_n+n"
    (mrs:vsym "n*nominalization_en") "n+n_n+nominalization"
    (mrs:vsym "nominalization*n_en") "n+n_nominalization+n"
    (mrs:vsym "adj+n_en") "n+n_adj+n"
    (mrs:vsym "n+p+q+n_sg_en") "n+n_n+p+q+n_sg"
    (mrs:vsym "n+p+q+n_pl_en") "n+n_n+p+q+n_pl")
   (list
    (mrs:vsym "adj*n_no")
    (mrs:vsym "adj+n_en") "adj*n_adj+n"
    (mrs:vsym "noun_en") "adj*n_n")
   (list
    (mrs:vsym "v*n_no")
    (mrs:vsym "noun_en") "v+n_n")))

(defun mtr-equivalent-p (mtr1 mtr2)
  (and (equal (third mtr1) (third mtr2))
       (equal (fourth mtr1) (fourth mtr2))
       (equal (fifth mtr1) (fifth mtr2))))

(defstruct kfe
  surface ;; `oppslagsord'
  id ;; `homografnr'
  gender ;; `kjønn'
  category ;; `ordklasse' or `ordkategori'
  senses
  xml)

(defstruct kfs
  surfaces)

(defun process-kf (kf
                   &key simplex mwe log
                        (source :norgram) (target :erg)
                        active)
  (let* ((sstream (create-output-stream simplex))
         (mstream (if mwe (create-output-stream mwe) sstream))
         (lstream (create-output-stream log))
         (nounp (eq source (mrs:vsym "noun_no")))
         (adjectivep (eq source (mrs:vsym "adjective_no"))))
    (loop 
        for entry in (if (stringp kf) (read-kf-entries kf) kf)
        for in = (first entry)
        unless (upper-case-p (char in 0)) do
          (multiple-value-bind (smrss striggers)
              (cond
               (nounp (values nil (list (list source in))))
               (adjectivep (values nil (list (list source in))))
               (t (process-kf-entry in source)))

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
                                when (or (null active) (smember sid active))
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
                            ;; triggers).                      (30-may-06 ; oe)
                            ;;
                            (when types
                              (format
                               lstream
                               " ==> {~a} ~{~(~a~)~^ ~}"
                               (length types) types))
                            (format lstream "~%")))
                  finally
                    (loop
                        with finalization
                        for bucket across mtrs
                        for n = (length bucket)
                        do
                          (loop
                              for (in out type spreds tpreds)
                              in (nreverse bucket)
                              for j from 0
                              unless finalization do
                                (setf finalization
                                  (list
                                   in out j n type spreds tpreds
                                   :simplex sstream :mwe mstream))
                              else do
                                (output-mtr
                                 in out j n type spreds tpreds
                                 :simplex sstream :mwe mstream))
                        finally
                          (when finalization
                            (apply #'output-mtr finalization))))))
          (format lstream "~%")
          (force-output sstream) (force-output mstream) (force-output lstream))
    (when (stringp simplex) (close sstream))
    (when (stringp mwe) (close mstream))
    (when (stringp log) (close lstream))))

(defun read-kf-file (file)
  (let* ((root (system:getenv "LOGONROOT"))
         (kf (and root (dir-append root '(:relative "kf" "noen"))))
         (path (and kf (merge-pathnames
                        kf (make-pathname :name file :type "gz")))))
    (when (and path (probe-file path))
      (multiple-value-bind (input foo pid)
          (run-process
           (format nil "gzip -d -c '~a'" (namestring path))
           :wait nil
           :output :stream :input "/dev/null"
           :error-output "/dev/null" :if-error-output-exists :append)
        (declare (ignore foo))
        #+:allegro
        (setf (stream-external-format input)
          (excl:find-external-format :utf-8))
        (let ((xml (net.xml.parser:parse-xml input)))
          #+:allegro
          (sys:os-wait nil pid)
          xml)))))

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
                     (comma (position #\; target))
                     (target (if comma (subseq target 0 comma) target))
                     targets)
                #+:debug
                (format
                 t "{~a} --> {~a}~%" source targets)
                (ppcre:do-scans
                    (foo bar starts ends "([^|]+)(?: \\| |$)" target)
                  (declare (ignore foo bar))
                  (loop
                      for target
                      in (explode-kf-surface
                          (subseq target (aref starts 0) (aref ends 0)))
                      do (pushnew target targets :test #'string-equal)))
                (push (cons source (nreverse targets)) result))))
        finally (return (nreverse result)))))

(defun process-names (names &key file)
  (let* ((stream (create-output-stream file)))
    (loop 
        for entry in names
        for in = (first entry)
        for out = (second entry)
        when (and (stringp in) (stringp out)) do
          (output-mtr
           in out 0 1 "proper_np" (list in) (list out)
           :simplex stream :suffix "np" :cargp t)
          (output-mtr
           in out 0 1 "proper_noun" (list in) (list out)
           :simplex stream :suffix "n" :cargp t)
          (force-output stream))
    (when (stringp file) (close stream))))

(defun read-names (file)
  (with-open-file (stream file :direction :input)
    (loop
        for entry = (read stream nil nil)
        for in = (first entry)
        for out
        = (loop
              for string in (rest entry)
              for n = (count #\Space string)
              when (zerop n) collect string into simplex
              else collect (cons n string) into mwes
              finally
                (return
                  (or (first simplex)
                      (let ((mwes (sort mwes #'< :key #'first)))
                        (rest (first mwes))))))
        while entry
        when (and (stringp in) (stringp out) (not (string= in out)))
        collect (list in out))))

(defun read-andre (file &key semi)
  (with-open-file (stream file :direction :input)
    (loop
        with result
        with comments
        for c = (peek-char t stream nil nil)
        while c
        when (char= c #\;) do
          (let ((line (read-line stream nil nil)))
             (when line (push line comments)))
        else when (char= c #\() do
          (let ((form (ignore-errors (read stream nil nil))))
            (when (and form (rest (rest form))
                       (or (null semi)
                           (mt::lookup-predicate (first form) semi)))
              (push (cons (nreverse comments)
                          (cons (first form) (rest (rest form))))
                    result)))
          (setf comments nil)
        else do 
          (format
           t
           "read-andre(): skipping |~a|.~%"
           (read-line stream nil nil))
        finally
          ;;
          ;; _fix_me_
          ;; after going through all the trouble, we should actually make use
          ;; of those comments.                                 (1-dec-06; oe)
          ;;
          (return (loop
                      for foo in (nreverse result)
                      collect (rest foo))))))

(defun read-unknown (file &key semi)
  (with-open-file (stream file :direction :input)
    (loop
        for entry = (read stream nil nil)
        while entry
        when (and (rest entry)
                  (or (null semi)
                      (mt::lookup-predicate (first entry) semi)))
        collect entry)))

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
                   &key (simplex t) (mwe t) suffix cargp)
  (labels ((normalize (string)
             (multiple-value-bind (start end) (ppcre:scan "_rel$" string)
               (declare (ignore end))
               (loop
                   with length = (length string)
                   with result = (make-array length
                                             :element-type 'character
                                             :adjustable nil :fill-pointer 0)
                   for c across (subseq string 0 start)
                   when (char= c #\space) do (vector-push #\+ result)
                   else unless (member c '(#\( #\) #\.))
                   do (vector-push c result)
                   finally (return result)))))
    (let ((*package* (find-package mrs::*mrs-package*)))
      (format
       (if (rest spreds) mwe simplex)
       "~(~a~)_~(~a~)~@[_~a~]~@[_~a~] := ~(~a~)_~:[omtr~;mtr~] &~%~
        [ INPUT.RELS <~{ [ ~:[PRED~;CARG~] ~s ]~^,~}, ... >,~%  ~
          OUTPUT.RELS <~{ [ ~:[PRED~;CARG~] ~s ]~^,~}, ... >,~%  ~
          FLAGS [ RANK \"~a\", COUNT \"~a\", ~
                  AUTHOR \"~a\", DATE \"~a\" ] ].~%~%"
       (normalize in) (normalize out) suffix (unless suffix i) 
       type (zerop i) 
       (loop for spred in spreds collect cargp collect spred)
       (loop for tpred in tpreds collect cargp collect tpred)
       i n (current-user) (current-time)))))

(defun initialize-kf (&key count)
  (setf *kf* (make-hash-table :test #'equal))
  (loop
      for i from 0
      for file in *kf-files*
      while (or (null count) (< i count))
      do (parse-kf-file file))
  *kf*)

(defun parse-kf-file (file)
  "Parses .file. into a list of `kfe' structures (from `artikkel' entries)"
  (let* ((*package* (find-package :tsdb))
         (tree (read-kf-file file)))
    (loop
        for art in (get-xml-elements
                    '(|parse-it-export| |expartcontent|
                      |exptext| |exptextcontent| |artikkel|)
                    tree)
        for kfe = (parse-kf-artikkel art)
        for surface = (kfe-surface kfe)
	do 
          (setf (gethash surface *kf*)
            ;; Add homonyms in order for simplicity
            (append (gethash surface *kf*) (list kfe))))))

(defun parse-kf-artikkel (art)
  "parses an artikkel entry into a kfe structure"
  (let* ((ordklasse (or (get-first-xml-content '(|hode| |ordklasse|) art)
                        (get-first-xml-content '(|hode| |ordkategori|) art)))
         (category (cond
                    ((not (string ordklasse)) nil)
                    ((search "subst" ordklasse) :noun)
                    ((search "adj" ordklasse) :adjective)
                    ((search "adv" ordklasse) :adverb)
                    ((search "verb" ordklasse) :verb))))
    (make-kfe
     :surface (get-first-xml-content '(|hode| |oppslagsord|) art)
     :id (get-first-xml-content '(|hode| |homografnr|) art)
     :gender (get-first-xml-content '(|hode| |kjoenn|) art)
     :category category :senses (parse-kf-betydninger art) :xml art)))

(defun parse-kf-betydninger (art)
  (loop
      for betydning
      in (get-xml-elements '(|betydningsseksjon| |betydning|) art)
      for surfaces
      = (loop
            for ekv in (get-xml-elements '(|oversettelse| |ekv|) betydning)
            for surfaces = (get-xml-content ekv)
            append (loop
                       for surface in surfaces
                       append (explode-kf-surface surface)))
      when surfaces collect (make-kfs :surfaces surfaces)))

(defun kf-list (&key (heuristic #'first-of-each-sense)
                     (category :noun))
  (loop
      with keys
      = (sort (loop for key being each hash-key in *kf* collect key) #'string<)
      for source in keys
      for bucket = (gethash source *kf*)
      for kfes
      = (loop
            for kfe in bucket
            when (or (null category)
                     (and (functionp category) (funcall category kfe))
                     (equal (kfe-category kfe) category))
            collect kfe)
      for primary = (funcall heuristic kfes)
      for secondary
      = (loop
            for kfe in kfes
            nconc (loop
                      for kfs in (kfe-senses kfe)
                      for surfaces = (kfs-surfaces kfs)
                      append
                        (loop
                            for surface in surfaces
                            unless (member surface primary :test #'string=)
                            collect surface)))
      for targets = (append primary secondary)
      when targets collect (cons source targets)))

(defun first-of-each-sense (kfes)
  (loop
      for kfe in kfes
      append (loop
                 for kfs in (kfe-senses kfe)
                 for surface = (first (kfs-surfaces kfs))
                 when surface collect surface)))

(defun sorted-by-frequency (kfes)
  (loop
      with counts
      for kfe in kfes
      do
        (loop
            for kfs in (kfe-senses kfe)
            do
              (loop
                  for surface in (kfs-surfaces kfs)
                  for match = (assoc surface counts :test #'string=)
                  when match do (incf (rest match))
                  else do (push (cons surface 1) counts)))
      finally
        (return 
          (loop
              for match in (sort counts #'> :key #'rest)
              collect (first match)))))

(defun explode-kf-surface (string &optional (key :parentheses keyp))
  (let (strings)
    (setf strings
      (case key
        (:parentheses
         (multiple-value-bind (start end) (ppcre:scan "\\([^)]+\\)" string)
           (if (and start end)
             (let* ((match (subseq string (+ start 1) (- end 1)))
                    (prefix (string-trim '(#\space) (subseq string 0 start)))
                    (prefix (and prefix (not (string= prefix "")) prefix))
                    (suffix (string-trim '(#\space) (subseq string end)))
                    (suffix (and suffix (not (string= suffix "")) suffix))
                    (full (format
                           nil
                           "~@[~a ~]~a~@[ ~a~]"
                           prefix match suffix))
                    (reduced (format
                              nil
                              "~@[~a~]~:[~; ~]~@[~a~]"
                              prefix (and prefix suffix) suffix)))
               (list full reduced))
             (list string))))
        (:slash
         ;;
         ;; assume that `/'-disjunctions are local at the token level
         ;;
         (multiple-value-bind (start end) (ppcre:scan "[^ /]+/[^ /]+" string)
           (if (and start end)
             (let* ((match (subseq string start end))
                    (prefix (string-trim '(#\space) (subseq string 0 start)))
                    (prefix (and prefix (not (string= prefix "")) prefix))
                    (suffix (string-trim '(#\space) (subseq string end)))
                    (suffix (and suffix (not (string= suffix "")) suffix))
                    (slash (position #\/ match))
                    (one (format
                          nil
                          "~@[~a ~]~a~@[ ~a~]"
                          prefix (subseq match 0 slash) suffix))
                    (two (format
                          nil
                          "~@[~a ~]~a~@[ ~a~]"
                          prefix (subseq match (+ slash 1)) suffix)))
               (list one two))
             (list string))))))
    ;;
    ;; in case we have expanded something at this level, continue recursively
    ;;
    (when (rest strings)
      (setf strings
        (loop
            for string in strings
            append (explode-kf-surface string key))))

    (when (null keyp)
      (setf strings
        (loop
            for string in strings
            append (explode-kf-surface string :slash))))
    strings))

(defun get-xml-elements (path tree)
  ;; parses in order so just take the first off the list if
  ;; you only want the first element encountered
  "Get elements at the end of path in XML tree"
  (if (null path)
      (list tree)
    (if (listp tree)
	(loop for elt in tree
	    append (if (equal-xml-element (first path) elt)
		       (get-xml-elements (rest path) elt)
		     ;; Contentless elements are be bundled
		     ;; into a list
		     (get-xml-elements path elt))))))

(defun get-first-xml-content (path tree)
  (if (null path)
      (first (get-xml-content tree))
    (first (get-xml-content (first (get-xml-elements path tree))))))

(defun get-xml-content (elt &key )
  (let ((elt (rest elt))) ;; remove tag/reduce on recursion
    (cond ((null elt) nil)
	  ((equal (first elt) ", ") (get-xml-content elt)) ;; remove comma
	  ((stringp (first elt)) ;; text content
	   (list (first elt)))
	  ((equal-xml-element '|ekv| (first elt))
	   (append  (get-xml-content (first elt))
		    (get-xml-content elt)))
	  ;; other structured content tags may be handled here:
	  ;; pseudoekv parafrase henvisning
	  (t (get-xml-content elt)))))

(defun equal-xml-element (tag elt)
  "Checks if the top element of a tree corresponds to tag"
  (if (listp elt)
      (let* ((elt-tag (first elt))
	     (elt-tag (if (listp elt-tag) ;; strip attributes
			  (first elt-tag)
			elt-tag)))
	(equal tag elt-tag))))