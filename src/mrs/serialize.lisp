(in-package :mrs)

(defparameter lkb::*predicates-temp-file* nil)

(defparameter lkb::*semantics-temp-file* nil)

(defun serialize-semantics-indices
    (&optional (forward lkb::*predicates-temp-file*)
               (backward lkb::*semantics-temp-file*))
  (handler-case 
      (let ((tenurep
             #+:allegro (sys:gsgc-parameter :auto-step) #-:allegro nil))
        #+:allegro
        (when tenurep (setf (sys:gsgc-parameter :auto-step) nil))
        (loop
            with *package* = (find-package :lkb)
            with cdb = (cdb::open-write forward)
            for pred being each hash-key
            using (hash-value entry) in *relation-index*
            for key = (with-standard-io-syntax (write-to-string pred))
            for value
            = (with-standard-io-syntax
                (write-to-string
                 (if (hash-table-p entry) 
                   (cons
                    0
                    (loop
                        for id being each hash-key in entry
                        collect id))
                   (cons
                    1
                    (loop
                        for (role . values) in entry
                        collect
                          (cons
                           role
                           (loop
                               for string being each hash-key
                               using (hash-value ids) in values
                               collect (cons string ids))))))))
            do (cdb:write-record cdb key value)
            finally (cdb::close-cdb cdb))
        (loop
            with *package* = (find-package :lkb)
            with *mrs-raw-output-p* = t
            with cdb = (cdb::open-write backward)
            for id being each hash-key
            using (hash-value record) in *semantic-table*
            for key = (with-standard-io-syntax (write-to-string id))
            for value = (with-standard-io-syntax
                          (write-to-string record))
            do (cdb:write-record cdb key value)
            finally (cdb::close-cdb cdb))
        (when tenurep
          #+:allegro
          (setf (sys:gsgc-parameter :auto-step) tenurep))
        (cons
         (hash-table-count *relation-index*)
         (hash-table-count *semantic-table*)))
    (error (condition)
      (format
       t
       "serialize-semantics-indices(): error: `~a'~%"
       condition)
      (when (probe-file forward) (delete-file forward))
      (when (probe-file backward) (delete-file backward))
      nil)))

(defun unserialize-semantics-indices
    (&optional (forward lkb::*predicates-temp-file*)
               (backward lkb::*semantics-temp-file*))
  (loop
      with *package* = (find-package :lkb)
      with cdb = (cdb::open-read forward)
      for (key . datum) in (cdb::all-records cdb)
      for pred = (read-from-string key)
      for value = (read-from-string datum)
      for type = (first value)
      when (zerop type)
      do
        (let ((table (make-hash-table)))
           (loop
               for id in (rest value)
               do
                 (setf (gethash id table) t))
           (setf (gethash pred *relation-index*) table))
       when (eql type 1)
       do
         (loop
             for (role . values) in (rest value)
             for table = (make-hash-table :test #'equal)
             do
               (loop
                   for (string . ids) in values
                   do (setf (gethash string table) ids))
               (push (cons role table) (gethash pred *relation-index*))))
  (clrhash *semantic-table*)
  (loop
      with *package* = (find-package :lkb)
      with cdb = (cdb::open-read backward)
      for (key . datum) in (cdb::all-records cdb)
      for id = (read-from-string key)
      for record = (read-from-string datum)
      do
        (setf (gethash id *semantic-table*) record))
  (cons
   (hash-table-count *relation-index*)
   (hash-table-count *semantic-table*)))

(defun restore-semantic-indices ()
  (when (typep lkb::*lexicon* 'lkb::cdb-lex-database)
    (with-slots (lkb::source-files) lkb::*lexicon*
      (when (lkb::up-to-date-p
             lkb::source-files
             (list lkb::*predicates-temp-file* lkb::*semantics-temp-file*))
        (handler-case
            (unserialize-semantics-indices
             lkb::*predicates-temp-file*
             lkb::*semantics-temp-file*)
          (error (condition)
            (format
             t
             "unserialize-semantics-indices(): error: `~a'~%"
             condition)
            (when (probe-file lkb::*predicates-temp-file*)
              (delete-file lkb::*predicates-temp-file*))
            (when (probe-file lkb::*predicates-temp-file*)
              (delete-file (delete-file lkb::*predicates-temp-file*)))
            nil))))))
