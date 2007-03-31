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

(((:path . "tub") (:content . "Esprit Corpus Data (TU Berlin)"))
 ((:path . "german") (:content . "TSNLP Test Suite (DFKI)"))
 ((:path . "vm") (:content . "VerbMobil Reference Dialogue"))
 ((:path . "diet") (:content . "DiET version of TSNLP Test Suite"))
 ((:path . "vmd") (:content . "VerbMobil Development Data"))
 ((:path . "vmt") (:content . "VerbMobil Eiche Test Segment"))
 ((:path . "babel") (:content . "Babel Test Suite"))
 ((:path . "mrs") (:content . "DELPH-IN MRS Test Suite")))

 


