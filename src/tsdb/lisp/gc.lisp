(in-package "TSDB")

#+(version>= 5 0)
(def-foreign-call enable_gc_cursor ((type :int integer)) :returning :int)

#-(version>= 5 0)
(defforeign 'enable_gc_cursor :arguments '(integer) :return-type :integer)

#+(version>= 5 0)
(def-foreign-call gc_start ((type :int integer)) :returning :int)

#-(version>= 5 0)
(defforeign 'gc_start :arguments '(integer) :return-type :integer)

#+(version>= 5 0)
(def-foreign-call gc_end ((type :int integer)) :returning :int)

#-(version>= 5 0)
(defforeign 'gc_end :arguments '(integer) :return-type :integer)

(defun enable-gc-cursor (pid)
  (let ((gc-start (get-entry-point "gc_start"))
        (gc-end (get-entry-point "gc_end")))
    (when (and gc-start gc-end (integerp pid))
      (enable_gc_cursor pid)
      #+(version>= 5 0)
      (push (make-array 1 :element-type '(unsigned-byte 32) 
                        :initial-element gc-start)
            (excl:gc-before-c-hooks))
      #+(version>= 5 0)
      (push (make-array 1 :element-type '(unsigned-byte 32) 
                        :initial-element gc-end)
            (excl:gc-after-c-hooks)))))

            