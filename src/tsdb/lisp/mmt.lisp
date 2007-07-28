(in-package :tsdb)

(defparameter *mmt-root*
  (let ((root (system:getenv "LOGONROOT")))
    (and root (namestring (parse-namestring root)))))

(defparameter *mmt-languages* nil)

(defparameter *mmt-transfer-grammars* nil)

(defun mmt-determine-transfer-grammar (source target
                                       &key (grammars *mmt-transfer-grammars*))
  (loop
      for (in out transfer) in grammars
      when (and (or (eq source in) (eq in :any))
                (or (eq target out) (eq out :any)))
      return transfer))

(defun create-mmt-client (id &key (root *mmt-root*)
                                  (task '(:parse :generate)))
  (let* ((binary (format nil "~a/bin/logon" root))
         (base (format nil "~a/franz/~a/base.dxl" root mk::%system-binaries%))
         (client (format nil "~a/lingo/client.lisp" root))
         (options (list #-:runtime-standard "--source" 
                        #+:runtime-standard "--binary"
                        "--tty" "-I" base "-qq" "-locale" "no_NO.UTF-8"
                        #-:runtime-standard "-L" #-:runtime-standard client))
         (start (format
                 nil
                 "(tsdb::mmt-initialize-client ~(~s~) ~@[:transfer~])"
                 id (eq task :transfer)))
         (cpu (make-cpu
               :host (short-site-name) :spawn binary
               :options (append options (list "-e" start))
               :class id :name "lkb" :grammar (format nil "~(~a~)" id)
               :task task :wait 120 :quantum 60)))
    (unless (loop
                for cpu in *pvm-cpus*
                for class = (cpu-class cpu)
                thereis (or (eq class id)
                            (and (consp class) (smember id class))))
      (push cpu *pvm-cpus*))))

(defun create-mmt-system (source transfer target
                          &key (root *mmt-root*))
  (let* ((binary (format nil "~a/bin/logon" root))
         (base (format nil "~a/franz/~a/base.dxl" root mk::%system-binaries%))
         (client (format nil "~a/lingo/client.lisp" root))
         (options (list #-:runtime-standard "--source"
                        #+:runtime-standard "--binary"
                        "--tty" "-I" base "-qq" "-locale" "no_NO.UTF-8"
                        #-:runtime-standard "-L" #-:runtime-standard client))
         (start (format
                 nil
                 "(tsdb::mmt-initialize-system ~(~s ~s ~s~))"
                 source transfer target))
         (name (format nil "~(~a2~a~)" source target))
         (id (intern (string-upcase name) :keyword))
         (cpu (make-cpu
               :host (short-site-name) :spawn binary
               :options (append options (list "-e" start))
               :class id :name name :task '(:translate)
               :template "%s/%t/%d" :wait 360 :quantum 300)))
    (unless (loop
                for cpu in *pvm-cpus*
                for class = (cpu-class cpu)
                thereis (or (eq class id)
                            (and (consp class) (smember id class))))
      (push cpu *pvm-cpus*))))

(defun mmt-create-clients (&key (languages *mmt-languages*)
                                (reset t) )
  (when reset (setf *pvm-cpus* nil))

  (let ((setup (merge-pathnames
                 (dir-append *mmt-root* '(:relative "uw" "mmt"))
                 (make-pathname :name "setup" :type "lisp"))))
    (when (and (null *mmt-languages*)
               (probe-file setup) (load setup))))
  
  (loop
      with count = 0
      for languages on (or languages *mmt-languages*)
      for source = (first languages)
      do
        (loop
            for target in (rest languages)
            for forward = (mmt-determine-transfer-grammar source target)
            for backward = (mmt-determine-transfer-grammar target source)
            do
              (when (create-mmt-client source) (incf count))
              (when (create-mmt-client forward :task :transfer) (incf count))
              (when (create-mmt-client backward :task :transfer) (incf count))
              (when (create-mmt-client target) (incf count))
              (when (create-mmt-system source forward target) (incf count))
              (when (create-mmt-system target backward source) (incf count)))
      finally (return count)))

(defun mmt-initialize-client (language &optional task)
  (let* ((logon (system:getenv "LOGONROOT")))
    ;;
    ;; load MK defsystem() and LinGO load-up library first
    ;;
    (load (format nil "~a/lingo/lkb/src/general/loadup" logon))

    (funcall (symbol-function (find-symbol "INITIALIZE-TSDB" :tsdb))
             nil :rc (format nil "~a/dot.tsdbrc" logon))

    (excl:tenuring 
     (funcall 
      (intern "READ-SCRIPT-FILE-AUX" :lkb)
      (format
       nil
       "~a/uw/mmt/~(~a~)/~:[lkb/~;~]script"
       logon language (eq task :transfer)))
     (unless (eq task :transfer)
       (funcall (intern "INDEX-FOR-GENERATOR" :lkb))))

    ;;
    ;; allow the generator to relax post-generation MRS comparison, if need be
    ;;
    (set (intern "*BYPASS-EQUALITY-CHECK*" :lkb) :filter)

    ;;
    ;; where possible (i.e. generation), limit the actual search to some 
    ;; maximum number of hypotheses; in generation, this activates selective
    ;; unpacking.
    ;;
    (set (intern "*TSDB-MAXIMAL-NUMBER-OF-ANALYSES*" :tsdb) 50)
    (set (intern "*TSDB-EXHAUSTIVE-P*" :tsdb) nil)
    
    ;;
    ;; make sure that parsing and generation clients return MRSs too
    ;;
    (unless (eq task :transfer)
      (set (intern "*TSDB-SEMANTIX-HOOK*" :tsdb) "mrs::get-mrs-string"))

    (excl:gc :tenure) (excl:gc) (excl:gc t) (excl:gc)
    (setf (sys:gsgc-parameter :auto-step) nil)
    (funcall (symbol-function (find-symbol "SLAVE" :tsdb)))))

(defun mmt-initialize-system (source transfer target)
  (let* ((logon (system:getenv "LOGONROOT")))
    ;;
    ;; load MK defsystem() and LinGO load-up library first
    ;;
    (load (format nil "~a/lingo/lkb/src/general/loadup" logon))

    (funcall (symbol-function (find-symbol "INITIALIZE-TSDB" :tsdb))
             nil :rc (format nil "~a/dot.tsdbrc" logon))

    ;;
    ;; activate duplicate result elimination for analysis and generation
    ;;
    (set (intern "*PROCESS-SUPPRESS-DUPLICATES*" :tsdb) '(:mrs :surface))

    ;;
    ;; where possible (i.e. generation), limit the actual search to some 
    ;; maximum number of hypotheses; in generation, this activates selective
    ;; unpacking.
    ;;
    (set (intern "*TSDB-MAXIMAL-NUMBER-OF-ANALYSES*" :tsdb) 100)
    (set (intern "*TSDB-EXHAUSTIVE-P*" :tsdb) nil)
    
    ;;
    ;; get ourselves three sub-contractors, one for each task we need to do. 
    ;; we must not reset the PVM at this point, as likely we were launched 
    ;; ourselves via  another PVM client (aka our parent).
    ;;
    (mmt-create-clients :reset t)
    (funcall (symbol-function (find-symbol "TSDB" :tsdb)) 
             :cpu source :task :parse :reset nil :file t
             :wait 120 :error :exit)
    (funcall (symbol-function (find-symbol "TSDB" :tsdb)) 
             :cpu transfer :task :transfer :reset nil :file t
             :wait 120 :error :exit)
    (funcall (symbol-function (find-symbol "TSDB" :tsdb)) 
             :cpu target :task :generate :reset nil :file t
             :wait 120 :error :exit)

    (excl:gc :tenure) (excl:gc) (excl:gc t) (excl:gc)
    (setf (sys:gsgc-parameter :auto-step) nil)
    (funcall (symbol-function (find-symbol "SLAVE" :tsdb)))))