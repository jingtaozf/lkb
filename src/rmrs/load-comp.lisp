(in-package :cl-user)

#| code for construction of `robust' MRSs, from trees
produced by robust parsers of various types.  
|#

;;; the idea is that the code can either be used in an
;;; LKB image, or standalone.  The load-comp.lisp file
;;; itself is intended for the standalone case.  The
;;; standalone.lisp file replicates some LKB stuff, including
;;; the MRS package definition
;;; But the standalone code won't convert MRSs to RMRSs
;;; (this is only available with the LKB)

(compile-file "standalone.lisp")
(load "standalone.fasl")

(in-package :mrs)

;;; main code for semantic composition
;;; and the RMRS specific structures
(compile-file "comp.lisp")
(load "comp.fasl")

;;; The ANNLT specific code - navigates round the input trees
(compile-file "annlt.lisp")
(load "annlt.fasl")

;;; outputting RMRS structures, and also LKB
;;; generated structures in a compatible format.
;;; In principle, supports multiple outputs - right now
;;; outputs stuff in XML
(compile-file "output.lisp")
(load "output.fasl")

;;; file with some utilities for reading XML style tags
;;; (very flakily right now)
(compile-file "xml-utils.lisp")
(load "xml-utils.fasl")

;;; loads the data file that associates the rule names with
;;; semantic operations
(compile-file "readgram.lisp")
(load "readgram.fasl")

;;; loads the data file that associates the tags with
;;; base semantic structures - uses a lot of code from 
;;; readgram
(compile-file "readtag.lisp")
(load "readtag.fasl")

;;; Examples of how to use the code

#|
;;; Simple test

 :pa :mrs
 (read-rmrs-grammar "rmrs/test1/gram.rmrs")
 (read-rmrs-tag-templates "rmrs/test1/lex.rmrs")
 (load "rmrs/test1/fns.lisp") ;;; overrides the fns in annlt.lisp
 (rmrs-from-file "rmrs/test1/test.data")
 
|#


#|
;;; ANNLT tree output

 :pa :mrs
 (read-rmrs-grammar "../annlt-data/gram.rmrs")
 (read-rmrs-tag-templates "../annlt-data/lex.rmrs")
  ;;; functions for ANNLT version are in annlt.lisp
  (rmrs-from-file "/usr/groups/corpora/trec8qa/parses/14.parses-st" 
  "../results/14.rmrsout")
 
|#

