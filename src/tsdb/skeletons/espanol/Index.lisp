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

(((:path . "banca") (:content . "YY Banking Transactions Examples"))
 ((:path . "mrs") (:content . "Spanish MRS Test Suite"))
 ((:path . "phenomena") (:content . "Linguistic Phenomena Test Suite"))
 ((:path . "mess") (:content . "Messy Details Test Suite"))
 ((:path . "a00003") (:content . "IULA `a00003' Corpus Sample"))
 ((:path . "a00039") (:content . "IULA `a00039' Corpus Sample"))
 ((:path . "a00060") (:content . "IULA `a00060' Corpus Sample"))
 ((:path . "a00245") (:content . "IULA `a00245' Corpus Sample"))
 ((:path . "clefqa") (:content . "CLEF QA Sample"))
 ((:path . "d00009") (:content . "IULA `d00009' Corpus Sample"))
 ((:path . "d00065") (:content . "IULA `d00065' Corpus Sample"))
 ((:path . "d00110") (:content . "IULA `d00110' Corpus Sample"))
 ((:path . "d00137") (:content . "IULA `d00137' Corpus Sample"))
 ((:path . "e00012") (:content . "IULA `e00012' Corpus Sample"))
 ((:path . "e00013") (:content . "IULA `e00013' Corpus Sample"))
 ((:path . "e00014") (:content . "IULA `e00014' Corpus Sample"))
 ((:path . "e00024") (:content . "IULA `e00024' Corpus Sample"))
 ((:path . "e00025") (:content . "IULA `e00025' Corpus Sample"))
 ((:path . "e00030") (:content . "IULA `e00030' Corpus Sample"))
 ((:path . "e00135") (:content . "IULA `e00135' Corpus Sample"))
 ((:path . "g20374") (:content . "IULA `g20374' Corpus Sample"))
 ((:path . "g20374a") (:content . "IULA `g20374a' Corpus Sample"))
 ((:path . "g20805") (:content . "IULA `g20805' Corpus Sample"))
 ((:path . "g20992") (:content . "IULA `g20992' Corpus Sample"))
 ((:path . "i00005") (:content . "IULA `i00005' Corpus Sample"))
 ((:path . "i00008") (:content . "IULA `i00008' Corpus Sample"))
 ((:path . "i00009") (:content . "IULA `i00009' Corpus Sample"))
 ((:path . "i00016") (:content . "IULA `i00016' Corpus Sample"))
 ((:path . "i00027") (:content . "IULA `i00027' Corpus Sample"))
 ((:path . "i00112") (:content . "IULA `i00112' Corpus Sample"))
 ((:path . "i00115") (:content . "IULA `i00115' Corpus Sample"))
 ((:path . "m00105a") (:content . "IULA `m00105a' Corpus Sample"))
 ((:path . "m00153") (:content . "IULA `m00153' Corpus Sample"))
 ((:path . "m00446") (:content . "IULA `m00446' Corpus Sample"))
 ((:path . "m00573") (:content . "IULA `m00573' Corpus Sample"))
 ((:path . "m00588") (:content . "IULA `m00588' Corpus Sample"))
 ((:path . "m00780") (:content . "IULA `m00780' Corpus Sample"))
 ((:path . "m00788") (:content . "IULA `m00788' Corpus Sample"))
 ((:path . "m00792") (:content . "IULA `m00792' Corpus Sample"))
 ((:path . "m00858") (:content . "IULA `m00858' Corpus Sample"))
 ((:path . "m00876") (:content . "IULA `m00876' Corpus Sample"))
 ((:path . "m00893") (:content . "IULA `m00893' Corpus Sample")))
