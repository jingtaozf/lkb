(in-package :mt)

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

(defparameter *semis* nil)

(defparameter *semi-test* '(:predicates :roles :arity :properties))

(defparameter *semi-ignore-roles*
  (list (mrs::vsym "CARG") (mrs::vsym "LNK")))

(defparameter *semi-generalize-ignore-properties* nil)

(defstruct semi
  name
  signature
  (roles (make-hash-table))
  (predicates (make-hash-table :test #'equal))
  (aliases (make-hash-table :test #'equal))
  (properties (make-hash-table))
  (ges (make-hash-table)))

(defmethod print-object ((object semi) stream)
  (declare (special %transfer-raw-output-p%))
  (if %transfer-raw-output-p%
    (call-next-method)
    (let ((roles (hash-table-count (semi-roles object)))
          (predicates (hash-table-count (semi-predicates object)))
          (properties (hash-table-count (semi-properties object)))
          (ges (hash-table-count (semi-ges object))))
      (format
       stream
       "#[SEM-I {~a ge~p}: ~a role~p; ~a predicate~p; ~a propertie~p]"
       ges ges roles roles predicates predicates properties properties))))

(defstruct sps
  i synopses forms spes active type context parents children)

(defstruct spe
  id stem forms type ep index mrs)

(defstruct ges
  spes mrs)

(defmacro ignored-role-p (role)
  `(member ,role *semi-ignore-roles* :test #'eq))

(defmacro lookup-predicate (predicate semi)
  `(gethash ,predicate (semi-predicates ,semi)))

(defmacro lookup-alias (predicate semi)
  `(gethash (string-downcase ,predicate) (semi-aliases ,semi)))

(defun semi-lookup (semi &key predicate alias)
  (or
   (and predicate (lookup-predicate predicate semi))
   (and alias (lookup-alias alias semi))))

(defun read-synopsis (string &optional (offset 0))
  (let ((stream (make-string-input-stream string offset)))
    (labels ((read-role ()
               (let ((c (peek-char t stream nil nil)))
                 (when (char= c #\.) (return-from read-role))
                 (when (char= c #\,) (read-char stream nil nil)))
               (let* ((*package* (find-package mrs:*mrs-package*))
                      (c (peek-char t stream nil nil))
                      (optionality (and (char= c #\[) (read-char stream)))
                      (name (ignore-errors (read stream nil nil)))
                      (type (ignore-errors (read stream nil nil)))
                      properties)
                 (when (char= (peek-char t stream nil nil) #\{)
                   (read-char stream nil nil)
                   (loop
                       for c = (peek-char t stream nil nil)
                       while (and c (not (char= c #\})))
                       when (char= c #\,) do (read-char stream nil nil)
                       else do
                         (let ((name (read stream nil nil))
                               (value (read stream nil nil)))
                           (when (and name value)
                             (push
                              (make-property :name name :value value)
                              properties)))
                       finally
                         (loop
                             for c = (read-char stream nil nil)
                             while (and c (not (char= c #\}))))))
                 (unless (or (null optionality)
                             (let ((c (read-char stream nil nil)))
                               (and c (char= c #\]))))
                   (return-from read-synopsis))
                 (when (and name type)
                   (let ((variable (make-variable
                                    :type (string-downcase (string type))
                                    :properties properties
                                    :optionality optionality)))
                     (make-role
                      :name name :value variable :optionality optionality))))))
      (make-ep :roles (loop for role = (read-role) while role collect role)))))

(defun read-semi (file &key semi (encoding :utf-8) close)
  (let* ((file (pathname file))
         (name (format
                nil
                "~a~@[.~a~]"
                (pathname-name file) (pathname-type file)))
         (id (subseq name 0 (search ".smi" name)))
         (id (intern (string-upcase id) :keyword))
         (includep semi)
         (semi (or semi (make-semi :name id))))
    (with-open-file (stream file :direction :input)
      #+:allegro
      (setf (stream-external-format stream)
        (excl:find-external-format encoding))
      (format t "read-semi(): reading file `~a'.~%" name)
      (loop
          with *readtable* = (lkb::make-tdl-break-table)
          with *package* = (find-package :lkb)
          with context = :top
          for c = (peek-char t stream nil nil)
          while c do
            (cond
             ((char= c #\;) (read-line stream))
             ;;
             ;; _fix_me_
             ;; this should do full detection of `#|' comments instead.
             ;;                                         (8-oct-03; oe)
             ((char= c #\#) (lkb::read-tdl-comment stream))
             (t
              (let* ((line (read-line stream nil nil))
                     (line (string-trim '(#\space #\tab #\return) line)))
                (cond
                 ((search "predicates:" line :start2 0)
                  (setf context :predicates))
                 ((search "include:" line :start2 0)
                  (let* ((name (ignore-errors
                                (subseq line (+ (search "include" line) 8))))
                         (name (when name
                                 (string-trim '(#\space #\tab) name)))
                         (path (when name
                                 (merge-pathnames
                                  (make-pathname :name name) 
                                  (make-pathname
                                   :directory (pathname-directory file))))))
                    (if (and path (probe-file path))
                      (read-semi path :semi semi :encoding encoding)
                      (format t "read-semi(): invalid `~a'.~%" line))))
                 (t
                  (case context
                    (:predicates
                     (let* ((pred (ignore-errors
                                   (read-from-string line nil nil)))
                            (pred
                             (if (stringp pred) (string-downcase pred) pred))
                            (alias (predicate-alias pred))
                            (colon (and pred (position #\: line)))
                            (synopsis 
                             (and colon (read-synopsis line (+ colon 1))))
                            (bucket (and pred (lookup-predicate pred semi))))
                       (cond
                        (synopsis
                         (setf (ep-pred synopsis) pred)
                         (if bucket
                           (push synopsis (sps-synopses bucket))
                           (let ((sps (make-sps :synopses (list synopsis))))
                             (setf (lookup-predicate pred semi) sps)
                             (when alias (setf (lookup-alias alias semi) sps)))))
                        (t
                         (format
                          t
                          "read-semi(): ignoring |~a|." line)))))))))))))
    (when close (close-semi semi))
    (unless includep (push semi *semis*))
    semi))

(defun close-semi (semi)
  (declare (ignore semi))
  #+:null
  (loop
      for sps being each hash-value
      using (hash-value pred) in (semi-predicates semi)
      for descendants
      = (and (lkb::is-valid-type pred) (lkb::retrieve-descendants pred))
      for ancestors
      = (and (lkb::is-valid-type pred) (lkb::retrieve-ancestors pred))
      do
        (loop
            for type in descendants
            for descendant = (lkb::ltype-name type)
            for bucket = (lookup-predicate descendant semi))))

(defun test-semi-compliance (mrs
                             &optional semi
                             &key tags)

  (if (null semi)
    (setf semi (first *semis*))
    (unless (semi-p semi)
      (setf semi
        (loop for foo in *semis* when (eq (semi-name foo) semi) return foo))))
  (unless (semi-p semi) (return-from test-semi-compliance))
  
  (labels ((test-ep (ep)
             (let* ((pred (mrs:rel-pred ep))
                    (pred (if (stringp pred) (string-downcase pred) pred)))
               (or
                (member pred *semi-fragment-relations* :test #'eq)
                (member
                 (mrs:rel-pred ep)
                 *semi-punctuation-relations* :test #'eq)
                (member pred *semi-token-relations* :test #'eq)
                (loop
                    with sps = (lookup-predicate pred semi)
                    for test = (loop
                                   for test in *semi-test*
                                   when (or (eq test :roles)
                                            (eq test :properties))
                                   collect test)
                    for synopsis in (when sps (sps-synopses sps))
                    thereis (or (null test)
                                (test-synopsis ep synopsis))))))
           (test-synopsis (ep synopsis)
             #+:debug (pprint (list ep synopsis))
             (loop
                 with matched
                 with rolep = (member :roles *semi-test* :test #'eq)
                 for role in (mrs:rel-flist ep)
                 for name = (mrs:fvpair-feature role)
                 for ignorep = (ignored-role-p name)
                 for match = (unless ignorep
                               (loop
                                   for role in (ep-roles synopsis)
                                   when (eq name (role-name role))
                                   return role))
                 always (or ignorep (null rolep) match)
                 unless (or ignorep (null rolep)
                            (unify-types
                             (mrs::var-type (mrs:fvpair-value role))
                             (variable-type (role-value match))
                             :internp t))
                 do (return-from test-synopsis)
                 else do (push match matched) #+:debug (pprint role)
                 finally
                   (return
                     (or (not (member :arity *semi-test* :test #'eq))
                         (loop
                             for role in (and rolep (ep-roles synopsis))
                             always (or (member role matched :test #'eq)
                                        (ignored-role-p (role-name role))
                                        (role-optionality role))))))))
    (loop
        with tags = (loop for tag in tags collect (format nil "_~a_" tag))
        for ep in (mrs:psoa-liszt mrs)
        unless (or (null *semi-test*)
                   (when tags
                     (loop
                         with pred = (string (mrs::rel-pred ep))
                         for tag in (when (char= (char pred 0) #\_) tags)
                         never (search tag pred :test #'string-equal)))
                   (test-ep ep))
        collect ep)))

(defun construct-semi (&key ids semi (rules t) 
                            (warn '(:collision)) (stream t))
  (let ((semi (or semi (make-semi)))
        (ids (or ids (lkb::collect-psort-ids lkb::*lexicon*))))
    (loop
        for id in ids
        for le = (lkb::get-lex-entry-from-id id :cache nil)
        when le do (record-le semi id le))
    (when rules
      (loop
          with ids
          = (if (listp rules)
              rules
              (loop
                  for id being each hash-key in lkb::*rules*
                  collect id))
          for id in ids
          for rule = (gethash id lkb::*rules*)
          when rule do (record-rule semi id rule))
      (loop
          with ids
          = (if (listp rules)
              rules
              (loop
                  for id being each hash-key in lkb::*lexical-rules*
                  collect id))
          for id in ids
          for rule = (gethash id lkb::*lexical-rules*)
          when rule do (record-rule semi id rule)))
    ;;
    ;; when requested, provide some sanity tests on the predicate inventory
    ;;
    (when (member :collision warn :test #'eq)
      (let* ((predicates
              (loop
                  for predicate being each hash-key in (semi-predicates semi)
                  collect (string-downcase predicate)))
             (predicates (remove-duplicates predicates :test #'string=))
             (predicates (sort predicates #'string<)))
        (loop
            for predicate in predicates
            for variant = (mrs:vsym predicate)
            when (and (lookup-predicate predicate semi)
                      (lookup-predicate variant semi))
            do (format
                stream "construct-semi(): predicate collision for ‘~(~a~)’.~%"
                predicate))))
    ;;
    ;; finally, construct `generalized synopses' (i.e. folding multiple frames
    ;; into one, where possible using optionality and type underspecification).
    ;;
    (loop
        for sps being each hash-value in (semi-predicates semi)
        do (generalize-sps sps))
    semi))

(defun predicate-alias (predicate)
  (let* ((string (string-downcase predicate))
         (n (search mrs::*sem-relation-suffix* string :from-end t))
         (alias (subseq string 0 n)))
    (unless (string= predicate alias) alias)))

(defun record-le (semi id le)
  (let* ((tdfs (lkb::lex-entry-full-fs le))
         (dag (lkb::tdfs-indef tdfs))
         (type (lkb::type-of-fs dag))
         (stem (format nil "~{~a~^ ~}" (lkb::lex-entry-orth le)))
         (forms (list (list :base stem)))
         (mrs
          (when tdfs
            ;;
            ;; the following is slightly round-about: use the LKB construction
            ;; code to read off a PSOA from the lexical entry FS, but then use
            ;; some of its internal state to construct an enriched MRS.  when
            ;; building the full ERG SEM-I (in jan-06), one fourth of the time
            ;; goes in mrs::construct-mrs(): investigate further one day ...
            ;;
            (let* ((mrs::*restart-variable-generator* nil)
                   (mrs::*named-nodes* nil)
                   (mrs::*ref-ind-type* mrs::*non_expl-ind-type*)
                   (generator (let ((n 0)) #'(lambda () (decf n))))
                   (cont
                    (mrs::path-value dag mrs::*initial-semantics-path*))
                   (psoa
                    (when (mrs::is-valid-fs cont)
                      (mrs::construct-mrs cont generator)))
                   (psoa (and psoa (map-mrs psoa :abstract))))
              (import-mrs psoa :externals mrs::*named-nodes*)))))
    (when (mrs-p mrs)
      (loop
          with externals
          = (loop
                for variable in (mrs-variables mrs)
                collect (variable-external variable))
          for optionality
          in (lkb::determine-argument-optionality dag externals)
          for variable in (mrs-variables mrs)
          do (setf (variable-optionality variable) optionality))
      ;;
      ;; at this point, dis-associate the MRS from the FS universe and add its
      ;; information into the SEM-I.
      ;;
      (loop
          for variable in (mrs-variables mrs)
          do (setf (variable-external variable) nil))
      (loop
          with ges = (make-ges :mrs mrs)
          for i from 0
          for ep in (mrs-eps mrs)
          for pred = (ep-pred ep)
          for alias = (and pred (predicate-alias pred))
          for sps = (or (lookup-predicate pred semi)
                        (setf (lookup-predicate pred semi) (make-sps)))
          for spe = (make-spe
                     :id id :stem stem :forms forms
                     :type type :ep ep :index i :mrs mrs)
          do
            ;;
            ;; _fix_me_
            ;; for the current ERG, subjects to verbs are [OPT bool].  fix 
            ;; this up here, so we get a more conventional looking SEM-I, but
            ;; really this should be adjusted in the grammar proper. 
            ;;                                                  (27-jan-06; oe)
            #+:logon
            (let ((arg1 (mrs::vsym "ARG1"))
                  (pred (string pred)))
              (when (search "_v_" pred)
                (loop
                    for role in (ep-roles ep)
                    for value = (role-value role)
                    when (and (eq (role-name role) arg1)
                              (variable-p value))
                    do
                      #+:null
                      (setf (variable-type value) *semi-p-type*)
                      (setf (variable-optionality value) nil))))
            (push spe (sps-spes sps))
            (push spe (ges-spes ges))
            (when alias (setf (lookup-alias alias semi) sps))
          finally
            (setf (ges-spes ges) (nreverse (ges-spes ges)))
            (setf (gethash id (semi-ges semi)) ges)))))

(defun record-rule (semi id &optional rule)
  (let* ((rule (or rule
                   (gethash id lkb::*rules*)
                   (gethash id lkb::*lexical-rules*)))
         (tdfs (lkb::rule-full-fs rule))
         (dag (lkb::tdfs-indef tdfs))
         (type (lkb::type-of-fs dag))
         (mrs
          (when tdfs
            ;;
            ;; the following is slightly round-about: use the LKB construction
            ;; code to read off a PSOA from the lexical entry FS, but then use
            ;; some of its internal state to construct an enriched MRS.  when
            ;; building the full ERG SEM-I (in jan-06), one fourth of the time
            ;; goes in mrs::construct-mrs(): investigate further one day ...
            ;;
            (let* ((mrs::*restart-variable-generator* nil)
                   (mrs::*named-nodes* nil)
                   (mrs::*ref-ind-type* mrs::*non_expl-ind-type*)
                   (generator (let ((n 0)) #'(lambda () (decf n))))
                   (cont
                    (mrs::path-value dag mrs::*construction-semantics-path*))
                   (rels
                    (when (mrs::is-valid-fs cont)
                      (mrs::extract-relations-from-liszt
                       cont id mrs::*construction-semantics-path* dag
                       :indexingp nil :generator generator)))
                   (psoa (and rels (mrs::make-psoa :liszt rels)))
                   (psoa (and psoa (map-mrs psoa :semi)))
                   (psoa (and psoa (map-mrs psoa :abstract))))
              (when psoa (import-mrs psoa))))))

    (when (mrs-p mrs)
      ;;
      ;; at this point, dis-associate the MRS from the FS universe and add its
      ;; information into the SEM-I.
      ;;
      (loop
          for variable in (mrs-variables mrs)
          do (setf (variable-external variable) nil))
      (loop
          with ges = (make-ges :mrs mrs)
          for i from 0
          for ep in (mrs-eps mrs)
          for pred = (ep-pred ep)
          for alias = (predicate-alias pred)
          for sps = (or (lookup-predicate pred semi)
                        (setf (lookup-predicate pred semi) (make-sps)))
          for spe = (make-spe
                     :id id :type type :ep ep :index i :mrs mrs)
          do
            (push spe (sps-spes sps))
            (push spe (ges-spes ges))
            (when alias (setf (lookup-alias alias semi) sps))
          finally
            (setf (ges-spes ges) (nreverse (ges-spes ges)))
            (setf (gethash id (semi-ges semi)) ges)))))

(defun record-mrs (semi mrs)
  (declare (ignore semi mrs)))

(defun print-semi (&optional (semi (first *semis*))
                   &key (format :concise) (stream t))
  (labels ((print-roles (ep stream)
             (loop
                 with last = (first (last (ep-roles ep)))
                 for role in (ep-roles ep)
                 for value = (role-value role)
                 when (variable-p value) do
                   (let ((properties (variable-properties value))
                         (optionality (variable-optionality value)))
                     (format
                      stream
                      "~@[~*[ ~]~:@(~a~) ~(~a~)"
                      optionality
                      (role-name role) (variable-type value)
                      optionality (eq role last))
                     (when properties (format stream " { "))
                     (loop
                         with last = (first (last properties))
                         for property in properties
                         do
                           (format
                            stream
                            "~@:(~a~) ~(~a~)~:[, ~; }~]"
                            (property-name property)
                            (property-value property)
                            (eq property last)))
                     (format
                      stream
                      "~@[~* ]~]~:[, ~;.~]"
                      optionality (eq role last)))
                 else do
                   (format
                    stream
                    "~:@(~a~) |~a|~:[, ~;.~]"
                    (role-name role) (constant-value value) (eq role last))))
           (print-forms (forms stream &optional bracketp)
             (when forms
               (when bracketp (format stream " ["))
               (loop
                   for (tag . strings) in forms
                   do 
                     (format
                      stream
                      "~:[, ~;~]~(~a~): ~{|~a|~^ ~}"
                      (eq tag (first (first forms))) tag strings))
               (when bracketp (format stream "]")))))
    (let* ((predicates
            (loop
                for pred being each hash-key in (semi-predicates semi)
                collect pred))
           (predicates (sort predicates #'string<)))
      (format stream "predicates:~%~%")
      (loop
          for pred in predicates
          for sps = (lookup-predicate pred semi)
          when (eq format :concise) do
            (loop
                with *package* = (find-package :lkb)
                for spe in (sps-spes sps)
                for id = (spe-id spe)
                for type = (spe-type spe)
                for index = (spe-index spe)
                for stem = (spe-stem spe)
                do
                  (format stream "  ~(~s~) : " pred)
                  (print-roles (spe-ep spe) stream)
                  (format
                   stream
                   " { #~d in ~(~a~)~@[ [~(~a~)]~] |~@[~a~]| }"
                   index id type stem)
                  #+:null
                  (print-forms (spe-forms spe) stream t)
                  (format stream "~%"))
          when (eq format :compact) do
            (loop
                with *package* = (find-package :lkb)
                for synopsis in (sps-synopses sps) do
                  (format stream "  ~(~s~) : " pred)
                  (print-roles synopsis stream)
                  (format stream "~%"))
          when (eq format :forms) do
            (let ((*package* (find-package :lkb)))
              (format stream "  ~(~s~) : " pred)
              (print-forms (sps-forms sps) stream)
              (format stream "~%"))))))

(defun generalize-types (type1 type2)
  (or
   (and (string-equal type1 type2) (values type1 nil))
   (and (string-equal type1 *semi-u-type*) (values *semi-u-type* :forward))
   (and (string-equal type1 *semi-i-type*)
        (or (string-equal type2 *semi-e-type*)
            (string-equal type2 *semi-x-type*))
        (values *semi-i-type* :forward))
   (and (string-equal type1 *semi-p-type*)
        (or (string-equal type2 *semi-h-type*)
            (string-equal type2 *semi-x-type*))
        (values *semi-p-type* :forward))
   (and (string-equal type2 *semi-u-type*) (values *semi-u-type* :backward))
   (and (string-equal type2 *semi-i-type*)
        (or (string-equal type1 *semi-e-type*)
            (string-equal type1 *semi-x-type*))
        (values *semi-i-type* :backward))
   (and (string-equal type2 *semi-p-type*)
        (or (string-equal type1 *semi-h-type*)
            (string-equal type1 *semi-x-type*))
        (values *semi-p-type* :backward))))

(defun generalize-properties (properties1 properties2)
  ;;
  ;; _fix_me_
  ;; provide an actual implementation one day; for now, we fail to generalize
  ;; anything, i.e. end up with more distinct outputs than absolutely needed.
  ;;                                                            (26-sep-06; oe)
  (unless (= (length properties1) (length properties2))
    (return-from generalize-properties :fail))
  (loop
      for property1 in properties1
      for name1 = (property-name property1)
      for value1 = (property-value property1)
      for match = (find name1 properties2 :key #'property-name)
      when (and match (equal value1 (property-value match)))
      collect (make-property :name name1 :value value1)
      else return :fail))

(defun generalize-ep (ep)
  (let ((roles
         (loop
             for role in (ep-roles ep)
             for name = (role-name role)
             for value = (role-value role)
             unless (member
                     name *semi-ignore-roles* :test #'eq)
             collect
               (make-role
                :name name
                :value (if (variable-p value)
                         (make-variable
                          :type (variable-type value)
                          :optionality (variable-optionality value)
                          :properties (variable-properties value))
                         (make-constant :value (constant-value value)))))))
    (make-ep :roles roles)))

(defun generalize-eps (ep1 ep2)
  ;;
  ;; _fix_me_
  ;; for the time being, we are only looking at arity, argument types, and
  ;; optionality; we should be doing something about surprising properties
  ;; (e.g. pluralia tante) at some point.                     (26-jan-06; oe)
  ;;
  ;;   (e x x x) & (e x x) --> (e x x [x])
  ;;   (e x h) & (e x x) --> (e x p)
  ;;   (e x) & (e h) --> (e p)
  ;;   (e [x] x) & (e x [x]) --> fail
  ;;
  (loop
      with ep = (make-ep) with used with direction
      with roles1
      = (loop
            for role in (ep-roles ep1) for name = (role-name role)
            unless (member name *semi-ignore-roles* :test #'eq)
            collect role)
      with roles2
      = (loop
            for role in (ep-roles ep2) for name = (role-name role)
            unless (member name *semi-ignore-roles* :test #'eq)
            collect role)
      for role1 in roles1
      for name1 = (role-name role1)
      for value1 = (role-value role1)
      for role2 = (loop
                      for role in roles2
                      when (eq (role-name role) name1) return role)
      for value2 = (and role2 (role-value role2))
      for role = (make-role :name name1)
      when (null role2) do
        (when (eq direction :forward) (return-from generalize-eps))
        (setf direction :backward)
        (setf (role-value role)
          (if (variable-p value1)
            (make-variable :type (variable-type value1) :optionality t)
            (copy-constant value1)))
        (push role (ep-roles ep))
      else do
        (cond
         ((and (variable-p value1) (variable-p value2))
          (let ((optionality1 (variable-optionality value1))
                (optionality2 (variable-optionality value2))
                (properties (generalize-properties
                             (variable-properties value1)
                             (variable-properties value2))))
            (unless (or (eq optionality1 optionality2)
                        (and optionality1 (not (eq direction :backward)))
                        (and optionality2 (not (eq direction :forward))))
              (return-from generalize-eps))
            (when (eq properties :fail) (return-from generalize-eps))
            (when (and optionality1 (not optionality2))
              (setf direction :forward))
            (when (and (not optionality1) optionality2)
              (setf direction :backward))
            (multiple-value-bind (type foo)
                (generalize-types
                 (variable-type value1) (variable-type value2))
              (when (or (null type)
                        (and foo direction (not (eq direction foo))))
                (return-from generalize-eps))
              (when foo (setf direction foo))
              (setf (role-value role)
                (make-variable
                 :type type :optionality (or optionality1 optionality2)
                 :properties properties)))))
         ((and (constant-p value1) (constant-p value2))
          (unless (constant= value1 value2) (return-from generalize-eps))
          (setf (role-value role)
            (make-constant :value (constant-value value1))))
         (t (return-from generalize-eps)))
        (push role (ep-roles ep))
        (push role2 used)
      finally
        (when (or (= (length roles2) (length used))
                  (not (eq direction :backward)))
          ;;
          ;; make sure to preserve roles that only occur on the second EP, and
          ;; then force them to be optional.
          ;;
          (loop
              for role2 in roles2
              unless (member role2 used :test #'eq)
              do
                (unless (variable-p (role-value role2))
                  (return-from generalize-eps))
                (push (make-role
                       :name (role-name role2)
                       :value  (make-variable
                                :type (variable-type (role-value role2))
                                :optionality t))
                      (ep-roles ep)))
          (setf (ep-roles ep) (nreverse (ep-roles ep)))
          (return ep))))

(defun generalize-sps (sps)
  (setf (sps-type sps) :object)
  (loop
      for spe in (sps-spes sps)
      for mrs = (spe-mrs spe)
      for ep1 = (spe-ep spe)
      for synopses
      = (list (generalize-ep ep1))
      then (loop
               for rest on synopses
               for ep2 = (first rest)
               for generalization = (generalize-eps ep1 ep2)
               when generalization
               return (nconc result (list generalization) (rest rest))
               else collect ep2 into result
               finally
                 (return (nconc result (list (generalize-ep ep1)))))
      when (and (mrs-p mrs) (rest (mrs-eps mrs)))
      do (setf (sps-type sps) :meta)
      finally (setf (sps-synopses sps) synopses))
  (loop
      for spe in (sps-spes sps)
      for forms = (spe-forms spe)
      when forms do
        (loop
            for (tag . strings) in forms
            for match = (assoc tag (sps-forms sps))
            when match do
              (loop
                  for string in strings
                  do (pushnew string (rest match) :test #'string=))
            else do
              (push (cons tag strings) (sps-forms sps))
            finally (setf (sps-forms sps) (nreverse (sps-forms sps)))))
  sps)


;;; added by AAC - called from rmrs-convert.lisp - added here because
;;; I don't want to add yet another file to lkb.system or mess around 
;;; with the file ordering there.
;;; takes a string and tries to look it up in the SEMI
;;; could no doubt be improved by better understanding of the code

(defun find-semi-entries (pred)
;;; code adapted from test-semi-compliance
  (unless *semis*     (error "Semis not initialised"))
  (let* ((semi (first *semis*))
	 (pred-symbol (mrs::vsym (string-upcase pred))))
    (if
	(or
	 (member pred-symbol *semi-fragment-relations* :test #'eq)
	 (member
	  pred-symbol
	  *semi-punctuation-relations* :test #'eq)
	 (member pred-symbol *semi-token-relations* :test #'eq)
	 (lookup-predicate pred-symbol semi))
	pred-symbol
      (if (lookup-predicate pred semi)
	  pred
	nil))))
