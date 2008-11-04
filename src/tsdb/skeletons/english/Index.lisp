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

;;;
;;; to make MaxEnt experimentation across data sets (and generally reference) a
;;; little easier, item identifiers need to be globally unique.  to accomodate
;;; existing data sets in this scheme, dan and i came up with a break-down of
;;; identifiers as follows
;;;
;;; [         1 --      1,999]  
;;; [     2,000 --      9,999] --- The Cathedral and the Bazaar
;;; [ 1,000,000 --  1,999,999] --- VerbMobil
;;; [ 2,000,000 --  2,999,999] --- ecommerce
;;; [ 3,000,000 --  3,999,999] --- LOGON
;;; [ 4,000,000 --  4,999,999] --- SciBorg
;;; [10,000,000 -- 19,999,999] --- WeScience
;;; [20,000,000 -- 29,999,999] --- PTB
;;; 

(((:path . "english") (:content . "TSNLP Test Suite"))
 ((:path . "csli") (:content . "CSLI (LinGO) Test Suite"))
 ((:path . "mrs") (:content . "DELPH-IN MRS Test Suite"))
 ((:path . "cb") (:content . "The Cathedral and the Bazaar"))
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
 ((:path . "wsj01") (:content . "Wall Street Journal (PTB; Section 1)"))
 ((:path . "wsj02") (:content . "Wall Street Journal (PTB; Section 2)"))
 ((:path . "wsj03") (:content . "Wall Street Journal (PTB; Section 3)"))
 ((:path . "wsj04") (:content . "Wall Street Journal (PTB; Section 4)"))
 ((:path . "wsj05") (:content . "Wall Street Journal (PTB; Section 5)"))
 ((:path . "wsj06") (:content . "Wall Street Journal (PTB; Section 6)"))
 ((:path . "wsj07") (:content . "Wall Street Journal (PTB; Section 7)"))
 ((:path . "wsj08") (:content . "Wall Street Journal (PTB; Section 8)"))
 ((:path . "wsj09") (:content . "Wall Street Journal (PTB; Section 9)"))
 ((:path . "wsj10") (:content . "Wall Street Journal (PTB; Section 10)"))
 ((:path . "wsj11") (:content . "Wall Street Journal (PTB; Section 11)"))
 ((:path . "wsj12") (:content . "Wall Street Journal (PTB; Section 12)"))
 ((:path . "wsj13") (:content . "Wall Street Journal (PTB; Section 13)"))
 ((:path . "wsj14") (:content . "Wall Street Journal (PTB; Section 14)"))
 ((:path . "wsj15") (:content . "Wall Street Journal (PTB; Section 15)"))
 ((:path . "wsj16") (:content . "Wall Street Journal (PTB; Section 16)"))
 ((:path . "wsj17") (:content . "Wall Street Journal (PTB; Section 17)"))
 ((:path . "wsj18") (:content . "Wall Street Journal (PTB; Section 18)"))
 ((:path . "wsj19") (:content . "Wall Street Journal (PTB; Section 19)"))
 ((:path . "wsj20") (:content . "Wall Street Journal (PTB; Section 20)"))
 ((:path . "wsj21") (:content . "Wall Street Journal (PTB; Section 21)"))
 ((:path . "wsj22") (:content . "Wall Street Journal (PTB; Section 22)"))
 ((:path . "wsj23") (:content . "Wall Street Journal (PTB; Section 23)"))
 ((:path . "wsj24") (:content . "Wall Street Journal (PTB; Section 24)"))
 ((:path . "cell") (:content . "DT Cell Phone Groups (Development Section)"))
 ((:path . "ecpa") (:content . "Ecommerce Email (Product Availability)"))
 ((:path . "ecos") (:content . "Ecommerce Email (Order Status)"))
 ((:path . "ecoc") (:content . "Ecommerce Email (Order Cancellation)"))
 ((:path . "ecpr") (:content . "Ecommerce Email (Product Return)"))
 ((:path . "trec9") (:content . "TREC QA Questions (Ninth Conference)"))
 ((:path . "harmony") (:content . "LOGON Harmonization Test Suite"))
 ((:path . "hike") (:content . "LOGON First Development Corpus"))
 ((:path . "rondane") (:content . "LOGON Hiking Treebank (Rondane)"))
 ((:path . "jhpstg") (:content . "LOGON Development Corpus (All Segments)"))
 ((:path . "jh") (:content . "LOGON Jotunheimen Corpus (All Sections)"))
 ((:path . "jh0") (:content . "LOGON Jotunheimen Corpus (Section 0)"))
 ((:path . "jh1") (:content . "LOGON Jotunheimen Corpus (Section 1)"))
 ((:path . "jh2") (:content . "LOGON Jotunheimen Corpus (Section 2)"))
 ((:path . "jh3") (:content . "LOGON Jotunheimen Corpus (Section 3)"))
 ((:path . "jh4") (:content . "LOGON Jotunheimen Corpus (Section 4)"))
 ((:path . "jh5") (:content . "LOGON Jotunheimen Corpus (Section 5)"))
 ((:path . "jhk") (:content . "LOGON Jotunheimen Test Corpus (Known-Vocabulary)"))
 ((:path . "jhu") (:content . "LOGON Jotunheimen Test Corpus (Unknown-Vocabulary)"))
 ((:path . "ps") (:content . "LOGON Preikestolen Corpus"))
 ((:path . "psk") (:content . "LOGON Preikestolen Test Corpus (Known-Vocabulary)"))
 ((:path . "psu") (:content . "LOGON Preikestolen Test Corpus (Unknown-Vocabulary)"))
 ((:path . "tg") (:content . "LOGON Turglede Corpus"))
 ((:path . "tgk") (:content . "LOGON Turglede Test Corpus (Known-Vocabulary)"))
 ((:path . "tgu") (:content . "LOGON Turglede Test Corpus (Unknown-Vocabulary)"))
 ((:path . "dnt.activity") (:content . "HandOn Corpus from DNT Activities"))
 ((:path . "dnt.article") (:content . "HandOn Corpus from DNT Articles"))
 ((:path . "dnt.cabin") (:content . "HandOn Corpus from DNT Cabins"))
 ((:path . "dnt.index") (:content . "HandOn Corpus from DNT Overview"))
 ((:path . "dnt.location") (:content . "HandOn Corpus from DNT Areas"))
 ((:path . "dnt.trip") (:content . "HandOn Corpus from DNT Trips"))
 ((:path . "guide.general") (:content . "HandOn Corpus from Willassen Overview"))
 ((:path . "guide.location") (:content . "HandOn Corpus from Willassen Areas"))
 ((:path . "bike") (:content . "HandOn Corpus from http://www.bike-norway.com"))
 ((:path . "gol") (:content . "HandOn Corpus from http://www.golinfo.no"))
 ((:path . "romsdal") (:content . "HandOn Corpus from http://www.romsdalsalpene.no"))
 ((:path . "tiltopps") (:content . "HandOn Corpus from http://www.tilltopps.com"))
 ((:path . "turistveg") (:content . "HandOn Corpus from http://www.turistveg.no"))
 
 ((:path . "shiken") (:content . "Embryonic JaEn MT Test Suite"))
 ((:path . "ws01") (:content . "WeScience Articles 1 -- 9"))
 ((:path . "ws02") (:content . "WeScience Articles 10 -- 16"))
 ((:path . "ws03") (:content . "WeScience Articles 17 -- 23"))
 ((:path . "ws04") (:content . "WeScience Articles 24 -- 28"))
 ((:path . "ws05") (:content . "WeScience Articles 29 -- 33"))
 ((:path . "ws06") (:content . "WeScience Articles 34 -- 42"))
 ((:path . "ws07") (:content . "WeScience Articles 43 -- 54"))
 ((:path . "ws08") (:content . "WeScience Articles 55 -- 66"))
 ((:path . "ws09") (:content . "WeScience Articles 67 -- 76"))
 ((:path . "ws10") (:content . "WeScience Articles 77 -- 84"))
 ((:path . "ws11") (:content . "WeScience Articles 85 -- 94"))
 ((:path . "ws12") (:content . "WeScience Articles 95 -- 104"))
 ((:path . "ws13") (:content . "WeScience Articles 105 -- 111"))
 ((:path . "ws14") (:content . "WeScience Articles 112 -- 115")))
 
