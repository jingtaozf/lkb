;;; Copyright (c) 2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.


(in-package :cl-user)

#| code for construction of `robust' MRSs, from trees
produced by robust parsers of various types.  
|#

;;; the idea is that the RMRS code can either be used in an
;;; LKB image, or standalone.  The load-comp.lisp file
;;; itself is intended for the standalone case.  The
;;; standalone.lisp file replicates some LKB stuff, including
;;; the MRS package definition
;;; But the standalone code won't convert MRSs to RMRSs
;;; (this is only available with the LKB)

;;; first load the xml stuff 

(compile-file "../xml/pxml0.cl")
(load "../xml/pxml0.fasl")
(compile-file "../xml/pxml1.cl")
(load "../xml/pxml1.fasl")
(compile-file "../xml/pxml3.cl")
(load "../xml/pxml3.fasl")
(compile-file "../xml/pxml2.cl")
(load "../xml/pxml2.fasl")


;;; then the standalone file

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

;;; file with some utilities for XML parser
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

;;; loads the underlying rmrs input code
(compile-file "input.lisp")
(load "input.fasl")
;;; Examples of how to use the code

#|
;;; ANNLT tree output

 :pa :mrs
 (read-rmrs-grammar "annlt-test/gram14.1.rmrs")
 (read-rmrs-grammar "rmrs/annlt-test/gram14.1.rmrs")
 (read-rmrs-tag-templates "annlt-test/lex14.1.rmrs")
 (read-rmrs-tag-templates "rmrs/annlt-test/lex14.1.rmrs")
  ;;; functions for ANNLT version are in annlt.lisp

  :pa :mrs
  (rmrs-from-file "annlt-test/test-select.rasp"
  "annlt-test/test-select.rmrs")

  (rmrs-from-file "rmrs/annlt-test/test-select.rasp"
  "rmrs/annlt-test/test-select.rmrs")

  (read-rmrs-file "annlt-test/test-select.rmrs")
  (read-rmrs-file "rmrs/annlt-test/test-select.rmrs")

  (construct-sem-for-tree
  '(|T/txt-sc1/----|
     (|S/np_vp| (|NP/n1_n1-name/-| (|N1/n| |Abrams_NP1|))
      (|V1/v| |bark+ed_VVD|))))

|#

