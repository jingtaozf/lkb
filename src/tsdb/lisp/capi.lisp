(in-package "TSDB")

#+(version>= 5 0)
(def-foreign-call 
    (_create_run "create_run")
    ((tid :int integer)
     (data (* :char) string)
     (run-id :int integer)
     (comment (* :char) string)
     (interactive :int integer)
     (protocol :int integer)
     (custom (* :char) string))
  :returning :int
  #+(version>= 6 0) :strings-convert #+(version>= 6 0) t)

#-(version>= 5 0)
(defforeign
    '_create_run :entry-point "create_run"
    :arguments '(integer string integer string integer integer string)
    :return-type :integer)

(defun create_run (tid data run-id comment interactive protocol custom)
  (let* ((comment (if (stringp comment) comment ""))
         (interactive (if interactive 1 0))
         (protocol (if (and (numberp protocol) (>= protocol 1) (<= protocol 2))
                     protocol
                     1))
         (custom (if (stringp custom) custom ""))
         (status (_create_run 
                  tid data run-id comment interactive protocol custom)))
    (cond
     ((zerop status) :ok)
     (t :error))))

#+(version>= 5 0)
(def-foreign-call 
    (_process_item "process_item")
    ((tid :int integer)
     (i_id :int integer)
     (i_input (* :char) string)
     (parse_id :int integer)
     (edges :int integer)
     (exhaustive :int integer)
     (nderivations :int integer)
     (interactive :int integer))
  :returning :int
  #+(version>= 6 0) :strings-convert #+(version>= 6 0) t)

#-(version>= 5 0)
(defforeign
    '_process_item :entry-point "process_item"
    :arguments '(integer integer string
                 integer integer integer integer integer)
    :return-type :integer)

(defun process_item (tid item exhaustive nderivations interactive)
  (let* ((i-id (get-field :i-id item))
         (i-input (or (get-field :p-input item) (get-field :i-input item)))
         (parse-id (get-field :parse-id item))
         (edges (or (get-field :edges item) -1))
         (exhaustive (if exhaustive 1 0))
         (interactive (if interactive 1 0))
         (status 
          (_process_item tid i-id i-input parse-id 
                         edges exhaustive nderivations interactive)))
    (cond
     ((zerop status) :ok)
     (t :error))))

#+(version>= 5 0)
(def-foreign-call 
    (_complete_run "complete_run")
    ((tid :int integer)
     (run_id :int integer)
     (custom (* :char) string))
  :returning :int
  #+(version>= 6 0) :strings-convert #+(version>= 6 0) t)

#-(version>= 5 0)
(defforeign
    '_complete_run :entry-point "complete_run"
    :arguments '(integer integer string)
    :return-type :integer)

(defun complete_run (tid run-id custom block interrupt)
  (let ((custom (if (stringp custom) custom "")))
    (if (< (_complete_run tid run-id custom) 0)
      :error
      (if block
        (loop
            for message = (pvm_poll tid %pvm_lisp_message% block)
            when (eq message :error)
            return :error
            when (and (message-p message)
                      (eql (message-tag message) %pvm_lisp_message%)
                      (eq (first (message-content message)) :return)
                      (eql (second (message-content message)) 
                           :complete-run))
            return (third (message-content message))
            else when (interrupt-p interrupt)
            do
              (return-from complete_run :interrupt))
        :ok))))
