(in-package "TSDB")

(def-foreign-call 
    (_create_run "create_run")
    ((tid :int integer)
     (data (* :char) string)
     (run-id :int integer)
     (comment (* :char) string)
     (interactive :int integer))
  :returning :int)

(defun create_run (tid data run-id comment interactive)
  (let* ((comment (if (stringp comment) comment ""))
         (interactive (if interactive 1 0))
         (status (_create_run tid data run-id comment interactive)))
    (cond
     ((zerop status) :ok)
     (t :error))))

(def-foreign-call 
    (_process_item "process_item")
    ((tid :int integer)
     (i_id :int integer)
     (i_input (* :char) string)
     (parse_id :int integer)
     (edges :int integer)
     (exhaustive :int integer)
     (interactive :int integer))
  :returning :int)

(defun process_item (tid item exhaustive interactive)
  (let* ((i-id (get-field :i-id item))
         (i-input (get-field :i-input item))
         (parse-id (get-field :parse-id item))
         (edges (get-field :edges item))
         (exhaustive (if exhaustive 1 0))
         (interactive (if interactive 1 0))
         (status 
          (_process_item tid i-id i-input parse-id 
                         edges exhaustive interactive)))
    (cond
     ((zerop status) :ok)
     (t :error))))

(def-foreign-call 
    (_complete_test_run "complete_test_run")
    ((tid :int integer)
     (run_id :int integer))
  :returning :int)

(defun complete_test_run (tid run-id block)
  (if (< (_complete_test_run tid run-id) 0)
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
                         :complete-test-run))
          return (third (message-content message))
          else
          do (when message (push message *pvm-pending-events*)))
      :ok)))

