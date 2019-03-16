(in-package :mt)

;;;
;;; Copyright (c) 2004 -- 2016 Stephan Oepen (oe@ifi.uio.no)
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

(defparameter %semi-patches% nil)

(defstruct semi
  name
  signature
  (roles (make-hash-table))
  (predicates (make-hash-table :test #'equal))
  (aliases (make-hash-table :test #'equal))
  (properties (make-hash-table))
  (ges (make-hash-table))
  (types (make-hash-table :test #'eq)))

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
  predicate synopses forms spes active type flags context 
  parents children ancestors descendants compatible)

(defmethod print-object ((object sps) stream)
  (declare (special %transfer-raw-output-p%))
  (if %transfer-raw-output-p%
    (call-next-method)
    (let ((*print-circle* nil)
          (*print-readably* nil))
      (format
       stream 
       "#[SPS ~a~@[ < ~{~a~^ ~}~]~@[ > ~{~a~^ ~}~]~
        ~@[ << ~{~a~^ ~}~]~@[ >> ~{~a~^ ~}~]]"
       (sps-predicate object)
       (loop 
           for parent in (sps-parents object)
           collect (if (sps-p parent) (sps-predicate parent) parent))
       (loop for child in (sps-children object) collect (sps-predicate child))
       (loop 
           for ancestor in (sps-ancestors object)
           unless (member ancestor (sps-parents object) :test #'eq)
           collect (sps-predicate ancestor))
       (loop 
           for descendant in (sps-descendants object)
           unless (member descendant (sps-children object) :test #'eq)
           collect (sps-predicate descendant))))))

(defstruct spe
  id stem forms type ep index mrs)

(defstruct ges
  spes mrs)

(defmacro ignored-role-p (role)
  `(member ,role *semi-ignore-roles* :test #'eq))

(defmacro glbp (predicate)
  `(ppcre:scan "^glbtype[0-9]+$" (string-downcase ,predicate)))

(defmacro lookup-predicate (predicate semi)
  `(gethash ,predicate (semi-predicates ,semi)))

(defmacro lookup-alias (predicate semi)
  `(gethash (string-downcase ,predicate) (semi-aliases ,semi)))

(defun semi-lookup (&key (semi (first *semis*)) predicate alias)
  (or
   (and predicate (lookup-predicate predicate semi))
   (and alias (lookup-alias alias semi))))

(defmacro patches-blocked-p (predicate)
  `(when (stringp ,predicate)
     (loop
         for (key . rest) in %semi-patches%
         thereis (and (eq key :block) (string= ,predicate (first rest))))))

(defmacro patches-alias (predicate)
  `(when (stringp ,predicate)
     (loop
         for (key . rest) in %semi-patches%
         when (and (eq key :alias) (string= ,predicate (first rest)))
         return (second rest))))

(defmacro patches-parents ()
  `(loop
       for (key . rest) in %semi-patches%
       when (eq key :parent) collect rest))

(defmacro patches-links (name)
  `(loop
       for (key . rest) in %semi-patches%
       when (and (eq key :link)
                 (eq (first rest) ,name))
       collect (second rest)))
  

(defun read-predicate (line &optional n)
  (let* ((pred (if n (subseq line 0 n) line))
         (pred (and pred (string-trim '(#\space #\tab) pred))))
    (if mrs::*normalize-predicates-p*
      (mrs::normalize-predicate pred)
      (let ((i (length pred)))
        (if (and (> i 2)
                 (char= (schar pred 0) #\")
                 (char= (schar pred (- i 1)) #\"))
          (string-downcase (read-from-string pred nil nil))
          pred)))))

(defun read-synopsis (line &optional (offset 0))
  (let ((stream (make-string-input-stream line offset)))
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

(defun read-semi (file &key semi (encoding :utf-8) 
                            resetp (recordp t) (includep t) (finalizep t))
  (when resetp (setf *semis* nil))
  (let* ((file (pathname file))
         (name (format
                nil
                "~a~@[.~a~]"
                (pathname-name file) (pathname-type file)))
         (id (subseq name 0 (search ".smi" name)))
         (id (intern (string-upcase id) :keyword))
         (inclusionp semi)
         (semi (or semi (make-semi :name id))))
    (with-open-file (stream file :direction :input)
      #+:allegro
      (setf (stream-external-format stream)
        (excl:find-external-format encoding))
      (format t "read-semi(): reading file `~a'.~%" name)
      (loop
          with *readtable* = (copy-readtable)
          with *package* = (find-package :lkb)
          with context = :top
          initially
            (set-syntax-from-char #\: #\space)
            (set-syntax-from-char #\, #\space)
            (set-syntax-from-char #\. #\space)
            (set-syntax-from-char #\[ #\space)
            (set-syntax-from-char #\] #\space)
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
                      (when includep
                        (read-semi path :semi semi :encoding encoding))
                      (format t "read-semi(): invalid `~a'.~%" line))))
                 (t
                  (case context
                    (:predicates
                     (let* ((n (length line))
                            (i (search " <" line))
                            (j (search " :" line :start2 (if i (+ i 1) 0)))
                            (parents
                             (when i
                               (let ((strings 
                                      (ppcre:split 
                                       " ?& ?" line
                                       :start (+ i 2) :end (or j n))))
                                 (loop
                                     for string in strings
                                     for foo = (read-from-string string nil nil)
                                     for pred
                                     = (if mrs::*normalize-predicates-p*
                                         (mrs::normalize-predicate foo)
                                         (if (stringp foo) 
                                           (string-downcase foo)
                                           foo))
                                     collect pred))))
                            (pred (read-predicate line (or i j)))
                            (alias (and pred (predicate-alias pred)))
                            (synopsis 
                             (and pred j (read-synopsis line (+ j 2))))
                            (bucket 
                             (and pred (lookup-predicate pred semi))))
                       (when synopsis (setf (ep-pred synopsis) pred))
                       (if (or parents synopsis)
                         (cond
                          (bucket
                           (when synopsis
                             (push synopsis (sps-synopses bucket)))
                           (loop
                               for parent in parents
                               do
                                 (pushnew
                                  parent (sps-parents bucket)
                                  :test #'string=)))
                          (t
                           (let ((sps (make-sps
                                       :predicate pred
                                       :synopses (and synopsis (list synopsis))
                                       :parents parents)))
                             (setf (lookup-predicate pred semi) sps)
                             (when alias
                               (setf (lookup-alias alias semi) sps)))))
                         (format
                          t
                          "read-semi(): ignoring |~a|.~%" line))))))))))))
    (unless inclusionp
      (anchor-semi semi)
      (when finalizep (finalize-semi semi))
      (when recordp (push semi *semis*)))
    semi))

(defun anchor-semi (semi)
  (loop 
      for sps being each hash-value in (semi-predicates semi)
      do (anchor-sps semi sps)))
 
(defun anchor-sps (semi sps)
  #+:lkb
  (unless (sps-type sps)
    (let* ((predicate (sps-predicate sps))
           (alias (patches-alias predicate))
           (string
            (concatenate 'string (string predicate) mrs::*sem-relation-suffix*))
           (type (lkb::get-type-entry (or alias (mrs:vsym string)))))
      (setf (sps-type sps) type)
      (setf (gethash type (semi-types semi)) sps))))

(defun embed-semi (semi &key (stream t))
  (loop
      for link in (patches-parents)
      for child = (lookup-predicate (first link) semi)
      for parent = (second link)
      when (and child (lookup-predicate parent semi))
      do (pushnew parent (sps-parents child))
      else do
        (format
         stream "embed-semi(): invalid parent patch: ~a < ~a.~%"
         (first link) parent))
  ;;
  ;; for just now, a naive pairwise comparison: we do not expect these sets
  ;; to be large, as the SEM-I is selective about which nodes to expose in
  ;; its external hierarchy.
  ;; _fix_me_
  ;; the current code is actually more naive than i originally realized; there
  ;; is no need to drive the embedding of ‘pairwise comparison’; instead, one
  ;; single traversal of all SEM-I entries, expanding parent links on visited
  ;; entries, should suffice.                                  (19-apr-16; oe)
  ;;
  (let ((cache (make-hash-table :test #'eq)))
    (labels ((link (types)
               (loop
                   for type in types
                   for match = (gethash type (semi-types semi))
                   when match collect match))
             (minimize (ancestors)
               (loop
                   for candidate in ancestors
                   for type = (sps-type candidate)
                   unless (loop
                              for ancestor in ancestors
                              thereis
                                (and (not (eq ancestor candidate))
                                     (sps-type ancestor)
                                     (member type (lkb::ltype-ancestors
                                                   (sps-type ancestor)))))
                   collect candidate))
             (ancestors (sps)
               (let* ((parents (sps-parents sps))
                      (ancestors (loop
                                     for parent in parents
                                     for sps = (lookup-predicate parent semi)
                                     append (and sps (ancestors sps)))))
                 (append parents ancestors)))
             (patch (type)
               (loop
                   for name in (patches-links (lkb::ltype-name type))
                   for link = (lkb::get-type-entry name)
                   unless link
                   do 
                     (format 
                      t "embed-semi(): ignoring invalid link `~(~a~)'."
                      name)
                   else append (cons link (lkb::ltype-ancestors type))))
             (embed (sps1 sps2)
               (let* ((type1 (sps-type sps1))
                      (type2 (sps-type sps2))
                      (ancestors2 (lkb::ltype-ancestors type2))
                      (ancestors2
                       (append
                        ancestors2
                        (loop
                            for type in ancestors2
                            append (patch type))))
                      (parents
                       (when (member type1 ancestors2 :test #'eq)
                         (or (gethash type2 cache)
                             (setf (gethash type2 cache)
                               (minimize (link ancestors2)))))))
                 (loop
                     with ancestors
                     = (remove-duplicates (ancestors sps2) :test #'string=)
                     for parent in parents
                     for predicate = (sps-predicate parent)
                     unless (member predicate ancestors :test #'string=)
                     do
                       (format 
                        t "embed-semi(): ~a < ~a.~%"
                        (sps-predicate sps2) predicate)
                       (push predicate (sps-parents sps2))))))
      (loop
          for sps1 being each hash-value in (semi-predicates semi)
          when (and (sps-type sps1)
                    #+:null
                    (member :entity (sps-flags sps1) :test #'eq)) do
            (loop
                for sps2 being each hash-value in (semi-predicates semi)
                when (and (sps-type sps2)
                          (not (eq sps1 sps2))
                          (member :entity (sps-flags sps2) :test #'eq))
                do (embed sps1 sps2)))
      ;;
      ;; the above can result in ‘redundant’ parent links, e.g. ‘_at_p_temp’
      ;; ending up with both ‘_at_p’ and ‘unspec_loc’, where the latter is the
      ;; parent of ‘_at_p’ already.  given we are not imposing any ordering
      ;; constraints on the embedding (currently) such redundancy needs to be
      ;; eliminated after the fact ...
      ;;
      (loop
          for sps being each hash-value in (semi-predicates semi)
          for parents = (loop 
                            for parent in (sps-parents sps) 
                            collect (lookup-predicate parent semi))
          do
            (setf (sps-parents sps)
              (loop
                  for sps in (minimize parents) 
                  collect (sps-predicate sps)))))))

(defun finalize-semi (semi)
  (loop
      for sps being each hash-value
      using (hash-key predicate) in (semi-predicates semi)
      for parents = (sps-parents sps)
      when (and parents (not (sps-p (first parents)))) do
        (setf (sps-parents sps)
          (loop
              for name in (remove-duplicates parents :test #'string=)
              for parent = (lookup-predicate name semi)
              when parent 
              do (pushnew sps (sps-children parent) :test #'eq)
              and collect parent
              else 
              do 
              (format
               t "finalize-semi(): ignoring invalid parent ‘~a’ for ‘~a’.~%"
               name predicate))))
  (labels ((walk (node ancestors)
             (loop
                 for ancestor in ancestors
                 do (pushnew ancestor (sps-ancestors node) :test #'eq))
             (or (sps-descendants node)
                 (setf (sps-descendants node)
                   (append (sps-children node)
                           (loop
                               with ancestors = (cons node ancestors)
                               for child in (sps-children node)
                               append (walk child ancestors)))))))
    (loop
        for sps being each hash-value in (semi-predicates semi)
        when (and (sps-children sps) (null (sps-parents sps)))
        do (setf (sps-descendants sps) (walk sps nil))))
    (loop
        for sps being each hash-value in (semi-predicates semi)
        do
          (setf (sps-descendants sps)
            (sort
             (sps-descendants sps)
             #'< :key #'(lambda (sps) (length (sps-descendants sps)))))))

(defun test-semi-compliance (mrs
                             &optional semi
                             &key tags)

  (if (null semi)
    (setf semi (first *semis*))
    (unless (semi-p semi)
      (setf semi
        (loop for foo in *semis* when (eq (semi-name foo) semi) return foo))))
  (unless (and (semi-p semi) (mrs::psoa-p mrs))
    (return-from test-semi-compliance))
  
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

(defun construct-semi (&key (ids t) semi (rules t) patches
                            embedp descendp finalizep
                            (warn '(:collision)) (stream t))
  (let* ((semi (or semi (make-semi)))
         (ids (if (eq ids t) (lkb::collect-psort-ids lkb::*lexicon*) ids))
         (%semi-patches% (if patches
                           (with-open-file (stream patches)
                             (let ((*package* (find-package :lkb)))
                               (read stream)))
                           %semi-patches%)))
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
    ;; now, construct `generalized synopses' (i.e. folding multiple frames
    ;; into one, where possible using optionality and type underspecification).
    ;;
    (loop
        for sps being each hash-value in (semi-predicates semi)
        do (generalize-sps sps))
    ;;
    ;; connect the SEM-I Predicate Structures to the grammar-internal type
    ;; hierarchy; when requested, descend into sub-types; add parent links
    ;; among SEM-I entries according to the type hierarchy; and expand parent
    ;; links into inverse children and transitive ancestor and descendant
    ;; links.
    ;;
    (anchor-semi semi)
    (when descendp
      (loop
          for sps being each hash-value in (semi-predicates semi)
          when (and (find :entity (sps-flags sps))
                    (not (patches-blocked-p (sps-predicate sps))))
          do (sps-descend semi sps)))
    (when embedp (embed-semi semi))
    (when finalizep (finalize-semi semi))
    semi))

(defun predicate-alias (predicate)
  (let* ((string (string-downcase predicate))
         (n (search mrs::*sem-relation-suffix* string :from-end t))
         (alias (subseq string 0 n)))
    (unless (string= predicate alias) alias)))

(defun record-le (semi id le &key (flags '(:entity)))
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
                        (setf (lookup-predicate pred semi) 
                          (make-sps :predicate pred)))
          for spe = (make-spe
                     :id id :stem stem :forms forms
                     :type type :ep ep :index i :mrs mrs)
          do
            (setf (sps-flags sps) (union (sps-flags sps) flags :test #'equal))
            ;;
            ;; _fix_me_
            ;; for the current ERG, subjects to verbs are [OPT bool].  fix 
            ;; this up here, so we get a more conventional looking SEM-I, but
            ;; really this should be adjusted in the grammar proper. 
            ;;                                                  (27-jan-06; oe)
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

(defun record-rule (semi id &optional rule &key (flags '(:entity)))
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
                        (setf (lookup-predicate pred semi) 
                          (make-sps :predicate pred)))
          for spe = (make-spe
                     :id id :type type :ep ep :index i :mrs mrs)
          do
            (setf (sps-flags sps) (union (sps-flags sps) flags :test #'equal))
            (push spe (sps-spes sps))
            (push spe (ges-spes ges))
            (when alias (setf (lookup-alias alias semi) sps))
          finally
            (setf (ges-spes ges) (nreverse (ges-spes ges)))
            (setf (gethash id (semi-ges semi)) ges)))))

(defun sps-descend (semi sps &optional (top sps) (type (sps-type sps)))
  #+:lkb
  (loop
      for descendant in (and type (lkb::ltype-descendants type))
      for predicate = (if mrs:*normalize-predicates-p*
                        (mrs:normalize-predicate (lkb::ltype-name descendant))
                        (lkb::ltype-name descendant))
      for old = (lookup-predicate predicate semi)
      for new = (unless (or (glbp predicate)
                            (patches-blocked-p predicate)
                            (when old
                              (member
                               (sps-predicate sps) (sps-parents old)
                               :test #'string=)))
                  (or old
                      (setf (lookup-predicate predicate semi)
                        (make-sps
                         :predicate predicate :synopses (sps-synopses sps)
                         :flags (acons :descend (list top) nil)))))
      when new do
        #-:debug
        (format
         t "sps-descend(): ~a > ~a.~%"
         (sps-predicate sps) predicate)
        (anchor-sps semi new)
        (push (sps-predicate sps) (sps-parents new))
        (when old
          (loop
              for flag in (sps-flags old)
              when (and (consp flag) (eq (first flag) :descend))
              do (push top (rest flag))))
      do (sps-descend semi (or new sps) top (unless new descendant))))

(defun record-mrs (semi mrs)
  (declare (ignore semi mrs)))

(defun print-semi (&optional (semi (first *semis*))
                   &key (format :concise) (predicates nil predicatesp)
                        filter (stream t))

  (if (stringp stream)
    (with-open-file (stream stream :direction :output :if-exists :supersede)
      (print-semi semi :format format :filter filter :stream stream))
    (labels ((print-predicate (predicate stream)
               (if mrs::*normalize-predicates-p*
                 (format stream "  ~(~a~) " predicate)
                 (format stream "  ~(~s~) " predicate)))
             (print-roles (ep stream)
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
              (or predicates
                  (loop
                      for pred being each hash-key in (semi-predicates semi)
                      collect pred)))
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
                    (print-predicate pred stream)
                    (format stream ": ")
                    (print-roles (spe-ep spe) stream)
                    (format
                     stream
                     " { #~d in ~(~a~)~@[ [~(~a~)]~] |~@[~a~]| }"
                     index id type stem)
                    #+:null
                    (print-forms (spe-forms spe) stream t)
                    (format stream "~%"))
            when (and (eq format :compact)
                      (or (null filter) (ppcre:scan filter pred)))
            do
              (loop
                  with *package* = (find-package :lkb)
                  with parents = (sps-parents sps)
                  for synopsis in (sps-synopses sps) 
                  do
                    (print-predicate pred stream)
                    #+:null
                    (when parents
                      (loop
                          initially (format stream "< ")
                            with last = (first (last parents))
                          for parent in parents
                          do
                            (format
                             stream "~(~a~)~:[ & ~;~]"
                             (sps-predicate parent) (eq parent last)))
                      (format stream " "))
                    (format stream ": ")
                    (print-roles synopsis stream)
                    (format stream "~%"))
            when (eq format :forms) do
              (let ((*package* (find-package :lkb)))
                (print-predicate pred stream)
                (format stream ": ")
                (print-forms (sps-forms sps) stream)
                (format stream "~%"))
            when (eq format :hierarchy) do
              (let ((parents (sps-parents sps)))
                (when (or parents predicatesp)
                  (print-predicate pred stream)
                  (when parents
                    (loop
                        initially (format stream "< ")
                        with last = (first (last parents))
                        for parent in parents
                        do
                          (format
                           stream "~(~a~)~:[ & ~;~]"
                           (sps-predicate parent) (eq parent last))))
                  (format stream ".~%"))))))))

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
  (loop
      for spe in (sps-spes sps)
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

(defun semi-compare-predicates (predicate1 predicate2
                                &key (type :subsumption)
                                     (semi (first *semis*)))
  (or
   (when (eq predicate1 predicate2) predicate1)
   (when (and (stringp predicate1) (stringp predicate2)
              (string= predicate1 predicate2))
     predicate1)
   (when (or (eq type :subsumption) (eq type :unification))
     (let ((sps1 (semi-lookup
                  :semi semi :predicate predicate1
                  :alias (unless mrs::*normalize-predicates-p* predicate1)))
           (sps2 (semi-lookup
                  :semi semi :predicate predicate2
                  :alias (unless mrs::*normalize-predicates-p* predicate2))))
       (when (and sps1 sps2)
         (or 
          (first (member sps1 (sps-descendants sps2) :test #'eq))
          (when (eq type :unification)
            (or
             (first (member sps2 (sps-descendants sps1) :test #'eq))
             (first (intersection
                     (sps-descendants sps1) (sps-descendants sps2)
                     :test #'eq))))))))))

(defun semi-compatible-predicates (predicate &key (semi (first *semis*)))
  ;;
  ;; determine ‘compatible’ predicates for use in generation: all predicates
  ;; that can unify with .predicate. (i.e. subsume it, are subsumbed by it, or
  ;; have at least one descendant in common with it) need to be considered for
  ;; initialization of the generator chart.
  ;;
  (let ((sps (lookup-predicate predicate semi)))
    (when sps
      (or (sps-compatible sps)
          (setf (sps-compatible sps)
            (let ((result (list sps)))
              (loop
                  for descendant in (sps-descendants sps)
                  do
                    (pushnew descendant result :test #'eq)
                    (loop
                        for ancestor in (sps-ancestors descendant)
                        do (pushnew ancestor result :test #'eq)))
              (loop
                  for ancestor in (sps-ancestors sps)
                  do (pushnew ancestor result :test #'eq))
              (loop
                  for sps in result collect (sps-predicate sps))))))))
