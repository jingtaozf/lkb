;;; Loading the lkb

(defparameter *lkb-system-version* :main)

;;(defparameter *lkb-source-dir* '(:absolute "Macintosh HD" "lkb99-expt"))

;;(defparameter *lkb-fasl-dir* '(:absolute "Macintosh HD" "lkb99-expt" "fasl"))

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

(progn 
      (dolist  (dir-and-file 
      '( ("main" "for")         ; duplicate useful Procyon CL for loops
                           ; generic CL
         ("main" "globals")
         ("MCL_specific" "picwin")      ; MacCL specific PICT windows
         ("MCL_specific" "picfield")    ; MacCL specific pop-up-fields
         ("MCL_specific" "topmenu") ; dialect specific - toplevel menu
         ("io-general" "menus")
         ("MCL_specific" "graphics") ; dialect specific basic graphics
         ("MCL_specific" "misc") ; dialect specific minor fns for compatability
         ("main" "types")       ; generic CL
         ("main" "dag")         ; generic CL
         ("main" "yadu")        ; generic CL
         ("main" "gen")         ; generic CL
         ("main" "structs")     ; generic CL
         ("main" "user-fns")     ; generic CL
         ("main" "marks")       ; generic CL
         ("main" "checktypes")           ; generic CL
         ("main" "leaf")
         ("io-paths" "typeinput")   ; generic CL
         ("io-general" "outputfs")    ; generic CL - calls some graphics
         ("io-general" "outputtdfs")    ; generic CL - calls some graphics
         ("MCL_specific" "activefs")    ; some dialect specific 
         ("io-paths" "lexinput")    ; generic CL
         ("main" "lex")         ; generic CL
         ("io-general" "toplevel")    ; generic CL
         ("main" "rules")       ; generic CL
         ("io-paths" "ruleinput")       ; generic CL
         ("main" "parse")       ; generic CL
         ("main" "generate")       ; generic CL
         ("MCL_specific" "parseout")    ; some dialect specific - parse tree fns
         ("MCL_specific" "chartout")    ; some dialect specific - chart output fns
         ("main" "morph")       ; generic CL
         ("MCL_specific" "dialog")
         ("MCL_specific" "tree") ; some dialect specific - type hierarchy fns
         ("io-general" "graph")       ; graph drawing by JAC - generic CL         
         ("main" "check-unif")  ; generic CL
         ("main" "lkb-tsdb")    ; generic CL
         ("io-general" "tree-nodes")
         ("io-tdl" "tdltypeinput")
         ("io-tdl" "tdloutput")
         ("io-tdl" "tdlruleinput")
         ("io-tdl" "tdllexinput")
         ("main" "batch-check")
         ("io-general" "utils")
         ))
      (let* ((dir (car dir-and-file))
             (file (cadr dir-and-file))
             (uncompiled-file (make-pathname :name file :type "lsp"
               :directory (append *lkb-source-dir* (list dir)))))
          (load uncompiled-file))))
;;;
;;; set-up-lkb-interaction constructs the top level frame and
;;; calls the menu set up in topmenu.lsp
;;;
;;; (set-up-lkb-interaction :full)
;;;
;;; if the argument is :core - the "mini" menu is set up
(set-up-lkb-interaction :big) 






