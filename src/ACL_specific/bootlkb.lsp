;;; Compiling the lkb
;;; main functionality plus graphics


(in-package :cl-user)

(eval-when (compile load eval)
   (shadow '(defmacro defstruct)))

(eval-when (compile load eval)
   (lisp:defmacro defmacro (&rest args)
      `(eval-when (compile load eval)
          (lisp:defmacro ,@args)))
    (defmacro defstruct (&rest args)
      `(eval-when (compile load eval)
          (lisp:defstruct ,@args))))

(defparameter *lkb-system-version* :page)

;;; change *lkb-source-dir* and *lkb-fasl-dir* for a different file system

;;(defparameter *lkb-source-dir* 
;;              '(:absolute "user" "malouf" "lkb"))

;;(defparameter *lkb-fasl-dir* 
;;              '(:absolute "user" "malouf" "lkb" "fasl"))

(defparameter *lkb-source-dir* 
  (butlast (pathname-directory *load-truename*)))

(defparameter *lkb-fasl-dir* 
  (append *lkb-source-dir* '("fasl")))

(defparameter *psorts-temp-file* 
  (make-pathname :name "templex" 
                 :directory *lkb-source-dir*)
   "a temporary file for the lexicon")


(eval-when (eval)
   (load
      (make-pathname :name "for" :type "lsp"
                     :directory (append *lkb-source-dir* (list "main")))
      :verbose nil :print nil))

(eval-when (load eval)
(excl:without-package-locks
(with-compilation-unit ()
(progn 
      (dolist  (dir-and-file 
      '( ("main" "for")         ; duplicate useful Procyon CL for loops
                           ; generic CL
         ("main" "globals")
         ("io-general" "utils")
         ("ACL_specific" "topmenu") ; dialect specific - toplevel menu
         ("io-general" "menus")
         ("ACL_specific" "graphics") ; dialect specific basic graphics
         ("ACL_specific" "misc") ; dialect specific minor fns for compatability
         ("main" "types")       ; generic CL
         ("main" "dag")         ; generic CL
         ("main" "yadu")        ; generic CL
         ("main" "gen")         ; generic CL
         ("main" "structs")     ; generic CL
         ("main" "user-fns")     ; generic CL
         ("main" "checktypes")  ; generic CL
         ("main" "marks")       ; generic CL
         ("io-paths" "typeinput")   ; generic CL
         ("io-general" "outputfs")    ; generic CL - calls some graphics
         ("io-general" "outputtdfs")    ; generic CL - calls some graphics
         ("ACL_specific" "activefs")    ; some dialect specific 
         ("io-paths" "lexinput")    ; generic CL
         ("main" "lex")         ; generic CL
         ("io-general" "toplevel")    ; generic CL
         ("main" "rules")       ; generic CL
         ("io-paths" "ruleinput")       ; generic CL
         ("main" "parse")       ; generic CL
         ("main" "generate")       ; generic CL
	 ("ACL_specific" "graph")	; dialect specific - tree drawing fns
	 ("ACL_specific" "parseout")	; some dialect specific - parse tree fns
         ("ACL_specific" "chartout") ; some dialect specific
         ("main" "morph")       ; generic CL
         ("ACL_specific" "dialog")
         ("ACL_specific" "tree") ; some dialect specific - type hierarchy fns
         ("main" "check-unif")  ; generic CL
         ("main" "lkb-tsdb")    ; generic CL
         ("io-general" "tree-nodes")
         ("io-tdl" "tdltypeinput")
         ("io-tdl" "tdloutput")
         ("io-tdl" "tdlruleinput")
         ("io-tdl" "tdllexinput")
         ("main" "batch-check")
         ))
      (let ((dir (car dir-and-file))
            (file (cadr dir-and-file)))
      (compile-file
            (make-pathname :name file :type "lsp"
               :directory (append *lkb-source-dir* (list dir)))
            :output-file 
            (merge-pathnames
               (make-pathname :name file :type "fasl"
               :directory (append *lkb-fasl-dir* (list dir)))))))))))








