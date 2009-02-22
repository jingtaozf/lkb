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
;;; _fix_me_
;;; most of the old skeletons (and existing treebanks, where applicable) still
;;; need to be converted.  this will require a re-numbering script and close
;;; synchronization with dan.                                   (8-nov-08; oe)
;;;
(((:path . "verbmobil") (:content . "Scheduling Dialogues (VerbMobil)"))
 ((:path . "yy") (:content . "ECommerce Email (YY)"))
 ((:path . "logon") (:content . "Tourism Brochures (LOGON)"))
 ((:path . "handon") (:content . "Tourism Web Sites (HandOn)"))
 ((:path . "wescience") (:content . "Scholarly Literature (WeScience)"))
 ((:path . "ptb") (:content . "Penn Treebank (WSJ)"))
 ((:path . "english") (:content . "TSNLP English Test Suite"))
 ((:path . "csli") (:content . "CSLI (LinGO) Test Suite"))
 ((:path . "mrs") (:content . "DELPH-IN MRS Test Suite"))
 ((:path . "cb") (:content . "The Cathedral and the Bazaar"))
 ((:path . "trec9") (:content . "TREC QA Questions (Ninth Conference)"))
 ((:path . "fracas") (:content . "FraCaS Semantics Test Suite")))
 
