;;; Loading the lkb
;;; tty interface - main functionality only

(defparameter *lkb-system-version* :page)

;;(defparameter *lkb-source-dir* '(:absolute "Macintosh HD" "lkb99"))

;;(defparameter *lkb-fasl-dir* 
;;'(:absolute "Macintosh HD" "lkb99" "fasl"))

(defparameter *lkb-source-dir* 
  (butlast (pathname-directory *load-truename*)))

(defparameter *lkb-fasl-dir* 
  (append *lkb-source-dir* '("fasl")))

(defparameter *psorts-temp-file* 
  (make-pathname :name "templex" 
                 :directory *lkb-source-dir*)
   "a temporary file for the lexicon")


(import '(enable-type-interactions disable-type-interactions))

(defpackage "MRS")
(defpackage "MAIN")

(eval-when (load eval)
(#+allegro excl:without-package-locks
 #-allegro progn
    (dolist  (dir-and-file
      '( ("main" "for")         ; duplicate useful Procyon CL for loops
                           ; generic CL
         ("main" "types")       ; generic CL
         ("main" "dag")         ; generic CL
         ("main" "yadu")        ; generic CL
         ("main" "gen")         ; generic CL
         ("main" "structs")     ; generic CL
         ("main" "globals")     ; generic CL
         ("main" "checktypes")  ; generic CL
         ("main" "marks")       ; generic CL
         ("io-paths" "typeinput")   ; generic CL
         ("io-general" "outputfs")   ; generic CL - calls some graphics
         ("io-general" "outputtdfs")   ; generic CL
         ("io-paths" "lexinput")    ; generic CL
         ("io-tdl" "tdltypeinput")
         ("io-tdl" "tdloutput")
         ("io-tdl" "tdlruleinput")
         ("io-tdl" "tdllexinput")
         ("main" "lex")         ; generic CL
         ("main" "rules")       ; generic CL
         ("io-paths" "ruleinput")       ; generic CL
         ("main" "parse")       ; generic CL
         ("main" "morph")       ; generic CL
         ("main" "check-unif")  ; generic CL
         ("main" "batch-check")
         ("main" "lkb-tsdb")    ; generic CL
         ("io-general" "tree-nodes")
         ("io-general" "utils")
         ("tty" "tty")
         ))
      (let* ((dir (car dir-and-file))
             (file (cadr dir-and-file)))
         #+mcl
	 (load (make-pathname :name file :type "lsp"
                :directory (append *lkb-source-dir* (list dir))))
         #-mcl
         (load (make-pathname :name file :type "fasl"
                :directory (append *lkb-fasl-dir* (list dir))))))))













