;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ginzburgh and Sag (2002) Implementation
;;; Copyright (c) 2001-2002, John Beavers, Chris Callison Burch, and Ivan Sag
;;; see license.txt (distributed with LKB) for details
;;;
;;; Filename: readme.txt
;;; Purpose: To be read.
;;; Last modified: 05/11/02 by John Beavers (JTB)
;;; Notes:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

This is an implementation of a fragment of the grammar from Ginzburg
and Sag (2000).  Supplemental grammatical information was taken from
Sag and Wasow (1999) and Pollard and Sag (1994).  The basis of this
grammar was an earlier implementation of the grammar from Sag and
Wasow (1999) developed by Chris Callison-Burch and Scott Guffey.
Initial development was done by Chris Callison-Burch, documented in
Callison-Burch (2000), and then further development continued by
myself, Chris, and Ivan Sag.  This grammar primarily covers three
fragments from these collective sources:

1.  Interrogative sentences, focusing on the basic syntactic and
    semantic type hierarchies as well as pied-piping.
2.  Auxiliaries, drawing heavily from Sag (2001) and Warner (2000).
3.  Complement clauses.

See the file "test-sentences.txt" for a list of the sort of coverage
we're getting, although it's by no means inclusive.  I'll only
apologize for a few hacks: the type hierarchy for auxiliaries looks
pretty awful, but I tried to follow Warner's types only approach as
much as possible.  There are several new features that don't appear in
any of the sources that were done for implementation purposes
(e.g. Q-STORE and P-STORE are quantifier and parameter stores
respectively, cf. STORE in Ginzburgh and Sag (2000), and REPRISABLE is
a total hack to prevent reprises from reprising and leading to deadly
infinite recursion).  Further, more serious, divergences and hacks
necessitated in the theory to implementation switch over are covered
in Callison-Burch (2000), and more general theory to implementation
problems are covered in Copestake (2002).  Finally, the grammar files
are pretty shoddily documented, so enter at your own risk.  For
further questions and comments feel free to email me.

John Beavers 
May, 2002

jbeavers@csli.stanford.edu
Dept. of Linguistics and CSLI
Stanford University
Stanford, CA, 94305-2150

Bibliography

Callison-Burch, Chris. (2000).  "A Computer Model for a grammar of
      English Questions".  B.S. Honors Thesis, Stanford University

Copestake, Ann.  (2002).  Implementing Typed Feature Structure
      Grammars.  CSLI Publications, Stanford, CA.

Ginzburg, Jonathan and Ivan A. Sag. (2002). Interrogative
      Investigations: The Form, Meaning, and Use of English Interrogatives.  
      CSLI Publications, Stanford, CA.

Sag, Ivan A.  (2000).  "Rules and Exceptions in the English Auxiliary
      System".  Manuscript: Stanford University.

Sag, Ivan A. and Thomas Wasow.  (1999).  Syntactic Theory: A Formal
      Introduction.  CSLI Publications, Stanford, CA.

Warner, Anthony. (1993).  "English Auxiliaries without Lexical Rules".
      In R. Borsley, ed., The Nature and Function of Syntactic Categories,
      vol. 32 of Syntax and Semantics, pages 167-220.  Academic Press, San
      Diego.


