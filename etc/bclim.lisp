(in-package :common-lisp-user)

(defmacro mbyte (n) 
  `(round (* 1024 1024 ,n)))

(excl:build-lisp-image "bclim.dxl" 
  :include-clim t :include-composer t
  :newspace (mbyte 256)
  :oldspace (mbyte 96)
  #-:64bit :lisp-heap-start #-:64bit (mbyte (+ 1024 64))
  :lisp-heap-size (mbyte (or #+:64bit 4096 1850))
  #+:sparc :c-heap-start #+:sparc #xe0000000
  #+(and :linux86 (not :64bit)) :c-heap-start 
  #+(and :linux86 (not :64bit)) "2944M"
  :pll-file (file-namestring (pll-file))
  :bundle-file (file-namestring (namestring (bundle-pathname)))
  :opt-speed 3 :opt-space 1 :opt-safety 2 :opt-debug 2
  :read-init-files '(".clinit.cl" "clinit.cl"))

(excl:exit)
