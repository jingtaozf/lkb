(in-package :tsdb)

(defvar *rasp-use-pos-tags-p* t)

(defun rasp-read-sentences (file)
  (loop
      with file = (if (stringp file) file (namestring file))
      with stream = (ignore-errors (open file :direction :input))
      with result = nil
      with sentence = nil
      for token = (when stream (read-line stream nil nil))
      for line from 1
      when (or (null token) (char= (char token 0) #\^)) do
        (when (and sentence 
                   (or (rest sentence)
                       (not (member (first sentence)
                                    '("._." "!_!" "?_?" "(_(" ")_)""..._...")
                                    :test #'string=))))
          (push (nreverse sentence) result))
        (setf sentence nil)
      else do (push token sentence)
      when (null token) do
        (close stream)
        (return (nreverse result))))

(defun rewrite-rasp-token (token pos)
  (cond
   ((string-equal pos "nnp") "NameErsatz")
   ((member pos '("``" "''" "," "\"") :test #'string=) "")
   ((member token '("--" "...") :test #'string=) "")
   ((string-equal pos "-lrb-") "(")
   ((string-equal pos "-rrb-") ")")
   (t (if lkb::*preprocessor*
        (lkb::preprocess token :globalp nil :format :lkb :verbose nil)
        token))))

(defun extract-rasp-tokens (string)
  (with-input-from-string (stream string)
    (loop
        for token = (first (read-token stream))
        for break = (and token (position #\_ token :from-end t :test #'char=))
        for form = (and break (subseq token 0 break))
        for tag = (and break (subseq token (+ break 1)))
        while (and (stringp token) (not (string= token "")))
        unless (and form tag) do
          (format
           t
           "extract-rasp-tokens(): ignoring invalid token `~a'.~%" token)
        else collect (list form tag))))

(defun rasp-preprocess (string 
                        &key rawp (plainp t) (posp *ptb-use-pos-tags-p*))
  (let ((length 0)
        (result nil))
    (loop
        with tokens = (extract-rasp-tokens string)
        with i = 0
        with id = 41
        for token in tokens
        for raw = (first token)
        for pos = (second token)
        for form = (rewrite-rasp-token raw pos)
        unless (or (string-equal pos "-none-")
                   (and rawp (string= raw ""))
                   (and (not rawp) (string= form ""))) do
          (cond
           (rawp (push raw result))
           (plainp (push form result))
           (t
            (push (format 
                   nil 
                   "(~d, ~d, ~d, 1, \"~a\" \"~a\", 0, \"null\"~
                    ~:[~*~;, \"~a\" 1.00~])" 
                   (incf id) i (incf i) form raw posp pos)
                  result)))
          (incf length))
    (values (and result (format nil "~{~a~^ ~}" (nreverse result))) length)))

(defun rasp-preprocess-for-pet (string)
  (rasp-preprocess string :rawp nil :plainp nil :posp t))

