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

(((:path . "enkel") (:content . "Basic Development Test Suite"))
 ((:path . "base") (:content . "LOGON Closed-Class Test Suite"))
 ((:path . "mrs") (:content . "Norwegian MRS Test Suite"))
 ((:path . "harmoni") (:content . "LOGON Harmonization Test Suite"))
 ((:path . "usts") (:content . "LOGON Unknown Syntactic Test Suite"))
 ((:path . "bokmaal") (:content . "NorGram Development Test Suite"))
 ((:path . "tur") (:content . "First LOGON Development Corpus"))
 ((:path . "vei") (:content . "LOGON Jotunheimen Corpus (1 to 10 Words)"))
 ((:path . "sti") (:content . "LOGON Jotunheimen Corpus (11 to 15 Words)"))
 ((:path . "topp") (:content . "LOGON Jotunheimen Corpus (16 to 20 Words)"))
 ((:path . "raak") (:content . "LOGON PSTG Corpus (1 to 10 Words)"))
 ((:path . "traakk") (:content . "LOGON PSTG Corpus (11 to 15 Words)"))
 ((:path . "nut") (:content . "LOGON PSTG Corpus (16 to 20 Words)"))
 ((:path . "jh") (:content . "LOGON Jotunheimen Corpus (All Sections)"))
 ((:path . "jh0") (:content . "LOGON Jotunheimen Corpus (Section 0)"))
 ((:path . "jh1") (:content . "LOGON Jotunheimen Corpus (Section 1)"))
 ((:path . "jh2") (:content . "LOGON Jotunheimen Corpus (Section 2)"))
 ((:path . "jh3") (:content . "LOGON Jotunheimen Corpus (Section 3)"))
 ((:path . "jh4") (:content . "LOGON Jotunheimen Corpus (Section 4)"))
 ((:path . "jh5") (:content . "LOGON Jotunheimen Corpus (Section 5)"))
 ((:path . "jhe") (:content . "LOGON Jotunheimen Corpus (Evaluation Sub-Set)"))
 ((:path . "jhk") (:content . "LOGON Jotunheimen Test Corpus (Known-Vocabulary)"))
 ((:path . "jhu") (:content . "LOGON Jotunheimen Test Corpus (Unknown-Vocabulary)"))
 ((:path . "ps") (:content . "LOGON Preikestolen Corpus"))
 ((:path . "pse") (:content . "LOGON Preikestolen Corpus (Evaluation Sub-Set)"))
 ((:path . "psk") (:content . "LOGON Preikestolen Test Corpus (Known-Vocabulary)"))
 ((:path . "psu") (:content . "LOGON Preikestolen Test Corpus (Unknown-Vocabulary)"))
 ((:path . "tg") (:content . "LOGON Turglede Corpus"))
 ((:path . "tge") (:content . "LOGON Turglede Corpus (Evaluation Sub-Set)"))
 ((:path . "tgk") (:content . "LOGON Turglede Test Corpus (Known-Vocabulary)"))
 ((:path . "tgu") (:content . "LOGON Turglede Test Corpus (Unknown-Vocabulary)")))

