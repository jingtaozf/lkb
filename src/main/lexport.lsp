;;; Copyright (c) 2001 -- 2002 
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

;;;
;;; first shot at exporting TDL lexicon for Access or Postgres import.
;;;

(in-package :lkb)

(defvar *export-lexicon-p* nil)
(defvar *export-output-file* "/tmp/lexicon.txt")
(defvar *export-skip-file* "/tmp/lexicon.skip")
(defvar *export-separator* #\,)
(defvar *export-counter* 0)

(defun export-lexicon (&key (output *export-output-file*) 
                            (skip *export-skip-file*)
                            (separator *export-separator*))
  (setf *export-counter* 0)
  (setf *export-separator* separator)
  (setf *export-output-file* output)
  (setf *export-skip-file* skip)
  (with-open-file (stream *export-output-file* :direction :output 
                   :if-exists :supersede :if-does-not-exist :create))
  (with-open-file (stream *export-skip-file* :direction :output 
                   :if-exists :supersede :if-does-not-exist :create))
  (let ((*export-lexicon-p* t))
    (reload-lex-files :allp nil)))

(defun export-lexical-entry (name constraint)
  (with-open-file (stream *export-output-file*
                   :direction :output 
                   :if-does-not-exist :create :if-exists :append)
    (let* ((separator *export-separator*)
           (type (extract-type-from-unifications constraint))
           (temp (extract-stem-from-unifications constraint))
           (stem (cdr temp))
           (count (car temp))
           (keyrel (extract-key-from-unifications constraint))      
           (keytag (extract-tag-from-unifications constraint))
           (altkey (extract-altkey-from-unifications constraint))
           (alt2key (extract-alt2key-from-unifications constraint))
           (compkey (extract-comp-from-unifications constraint))
           (ocompkey (extract-ocomp-from-unifications constraint))
           (total (+ count 1 
                     (if keyrel 1 0) (if keytag 1 0) (if altkey 1 0)
                     (if alt2key 1 0) (if compkey 1 0) (if ocompkey 1 0))))
      (cond 
       ((= total (length constraint))
        ;;print the lexID and type fields to a file,
        ;;along with the first word of the stem
        (format 
         stream
         "~d~a~(~a~)~a~(~a~)~a~a"
         (incf *export-counter*) 
         separator name separator type separator (first stem))
        ;;print the rest of the stem to the file
        (loop
            for word in (rest stem)
            do (format stream " ~a" word))
        ;;print the other fields
        (format
         stream
         "~a~a~a~a~(~a~)~(~a~)~a~(~a~)~a~(~a~)~a~(~a~)~a~(~a~)~
          ~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a"
         separator (first (last stem))
         separator  ;;pronunciation
         separator (or keyrel "")
         separator (or altkey "")
         separator (or alt2key "")
         separator (or keytag "")
         separator (or compkey "")
         separator (or ocompkey "")
         separator ;;complete
         separator ;;semclasses
         separator ;;preferences
         separator ;;classifier
         separator ;;selectrest
         separator ;;jlink
         separator ;;comments
         separator ;;exemplars
         separator ;;usages
         separator "EN" ;;lang
         separator "US" ;;country
         separator ;;dialect
         separator ;;domains
         separator ;;genres
         separator ;;register
         separator ;;confidence
         separator "danf" ;;user
         separator "03/27/02" ;;moddate
         separator 0 ;;version
         separator "LinGO" ;;source
         separator 1 ;;flags: 1 = not deleted
         )
        (format stream "~%")
        t)
       (t
        (format t"~%skipping super-rich entry: `~a'~%"  name)
        nil)))))


(defun skip-lexical-entry (istream position)
  (with-open-file (ostream *export-skip-file* 
                   :direction :output
                   :if-exists :append :if-does-not-exist :create)
    (let ((end (file-position istream)))
      (file-position istream (- position 1))
      (loop
          for c = (read-char istream)
          while (<= (file-position istream) end)
          do 
            (format ostream "~a" c))
      (format ostream "~%~%"))))


(defun extract-key-from-unification (unification)
  (when (unification-p unification)
    (let ((lhs (unification-lhs unification)))
      (when (path-p lhs)
        (path-typed-feature-list lhs)))))

(defun extract-value-from-unification (unification)
  (when (unification-p unification)
    (let ((rhs (unification-rhs unification)))
      (when (u-value-p rhs)
        (u-value-type rhs)))))


;;type
(defun extract-type-from-unifications (constraint)
  (extract-value-by-path-from-unifications constraint nil))


;;keyrel
(defun extract-key-from-unifications (constraint)
  (extract-value-by-path-from-unifications constraint 
                                           '(SYNSEM LOCAL KEYS KEY)))

;;keytag
(defun extract-tag-from-unifications (constraint)
  (extract-value-by-path-from-unifications 
   constraint '(SYNSEM LOCAL KEYS KEY CONST_VALUE)))

;;altkey
(defun extract-altkey-from-unifications (constraint)
  (extract-value-by-path-from-unifications constraint 
                                           '(SYNSEM LOCAL KEYS ALTKEY)))

;;altkey
(defun extract-alt2key-from-unifications (constraint)
  (extract-value-by-path-from-unifications constraint 
                                           '(SYNSEM LOCAL KEYS ALT2KEY)))

;;compkey
(defun extract-comp-from-unifications (constraint)
  (extract-value-by-path-from-unifications constraint 
                                           '(SYNSEM LOCAL KEYS --COMPKEY)))


;;ocompkey
(defun extract-ocomp-from-unifications (constraint)
  (extract-value-by-path-from-unifications constraint 
                                           '(SYNSEM LOCAL KEYS --OCOMPKEY)))


;;orthography
(defun extract-stem-from-unifications (constraint)
  (let ((stem nil)
        (count 1))
    (loop
        for path = nil then (cons 'rest path)
        for value = (extract-value-by-path-from-unifications
                     constraint (cons 'stem (append path '(first))))
        while value do 
          (incf count)
          (push value stem))
    (cons count (reverse stem))))


(defun extract-value-by-path-from-unifications (constraint path)
  (let* ((unification (find path constraint
                            :key #'extract-key-from-unification 
                            :test #'equal)))
    (when (unification-p unification)
      (extract-value-from-unification unification))))

