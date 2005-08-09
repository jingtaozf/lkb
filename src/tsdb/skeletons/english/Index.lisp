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

(((:path . "english") (:content . "TSNLP Test Suite"))
 ((:path . "csli") (:content . "CSLI (LinGO) Test Suite"))
 ((:path . "mrs") (:content . "LinGO MRS Test Suite"))
 ((:path . "vm97") (:content . "VerbMobil 97"))
 ((:path . "vm97p") (:content . "VerbMobil 97 (Partials)"))
 ((:path . "vm98") (:content . "VerbMobil 98"))
 ((:path . "vm6") (:content . "VerbMobil CD # 06"))
 ((:path . "vm13") (:content . "VerbMobil CD # 13"))
 ((:path . "vm31") (:content . "VerbMobil CD # 31"))
 ((:path . "vm32") (:content . "VerbMobil CD # 32"))
 ((:path . "demo") (:content . "VerbMobil Sample Dialogue"))
 ((:path . "fuse") (:content . "Balanced Fuse of Corpora Extracts"))
 ((:path . "blend") (:content . "Balanced Blend of Corpora Extracts"))
 ((:path . "aged") (:content . "Aged VerbMobil Data"))
 ((:path . "parc") (:content . "Wall Street Journal (PTB; PARC DB)"))
 ((:path . "wsj00") (:content . "Wall Street Journal (PTB; Section 0)"))
 ((:path . "cell") (:content . "DT Cell Phone Groups (Development Section)"))
 ((:path . "ecpa") (:content . "Ecommerce Email (Product Availability)"))
 ((:path . "ecos") (:content . "Ecommerce Email (Order Status)"))
 ((:path . "ecoc") (:content . "Ecommerce Email (Order Cancellation)"))
 ((:path . "ecpr") (:content . "Ecommerce Email (Product Return)"))
 ((:path . "trec9") (:content . "TREC QA Questions (Ninth Conference)"))
 ((:path . "hike") (:content . "LOGON First Development Corpus"))
 ((:path . "rondane") (:content . "LOGON Hiking Treebank (Rondane)"))
 ((:path . "jh0") (:content . "LOGON Jotunheimen Corpus (Section 0)"))
 ((:path . "jh1") (:content . "LOGON Jotunheimen Corpus (Section 1)"))
 ((:path . "jh2") (:content . "LOGON Jotunheimen Corpus (Section 2)"))
 ((:path . "jh3") (:content . "LOGON Jotunheimen Corpus (Section 3)"))
 ((:path . "jh4") (:content . "LOGON Jotunheimen Corpus (Section 4)"))
 ((:path . "jh5") (:content . "LOGON Jotunheimen Corpus (Section 5)"))
 ((:path . "shiken") (:content . "Embryonic UiO MT Test Suite")))
