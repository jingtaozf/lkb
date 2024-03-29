;;;
;;; this file should be `Index.lisp' and reside in the directory containing the
;;; tsdb(1) test suite skeleton databases (typically a subdirectory `skeletons'
;;; in the tsdb(1) database root directory `*tsdb-home*').
;;;
;;; the file should contain a single un-quote()d Common-Lisp list enumerating
;;; the available skeletons, e.g.
;;;
;;;   (((:path . "english") (:content . "English TSNLP test suite"))
;;;    ((:path . "csli") (:content . "CSLI (ERGO) test suite"))
;;;    ((:path . "vm") (:content . "English VerbMobil data")))
;;;
;;; where the individual entries are assoc() lists with at least two elements:
;;;
;;;   - :path --- the (relative) directory name containing the skeleton;
;;;   - :content --- a descriptive comment.
;;;
;;; the order of entries is irrelevant as the `tsdb :skeletons' command sorts
;;; the list lexicographically before output.
;;;

(((:path . "conll") (:content . "CoNLL 2009 Shared Task Data"))
 ((:path . "kihon") (:content . "Basic Japanese Constructions"))
 ((:path . "ginkougyou") (:content . "Hand-Constructed Banking Data"))
 ((:path . "hizuke") (:content . "Date Expressions"))
 ((:path . "suuji") (:content . "Number Expressions"))
 ((:path . "faqs") (:content . "Online Banking FAQ Sample"))
 ((:path . "mrs") (:content . "Japanese MRS Test Suite"))
 ((:path . "shiken") (:content . "Embryonic UiO MT Test Suite")))

 
