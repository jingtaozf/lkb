(in-package "CSLI")

(defun expand-engl-infl (affix)
  (cond ((equal affix "-S") '("3rd-sing-verb" "plur-noun"))
	((equal affix "-ED") '("past-verb" "past-part-verb"))
	((equal affix "-ING") '("pres-part-verb"))
	((equal affix "") '("inf-verb" "non-3rd-sing-verb" "sing-noun" 
			    "no-affix"))
	(t (list affix))))

(in-package "MRS")

(defun extract-and-output (arg)
  t)

(in-package "PARSING")

(defmethod get-instances ((lexicon tdl-lex) le-string) nil)

(in-package "CSLI")

(defun Analyze-English-Word-Morphology (spelling)
  (let ((result (append (Irregular-Inflections spelling)
                  (cond 
                   ;;; disabled -n't stripping because it is not handled
                   ;;; in the lexicon anyway (3-jun-96 -- dan@cl.dfki)
		   ;;;  ((Ends-In spelling "N'T")
		   ;;;   (Strip-Nt spelling))
		     ((find #\' spelling :test #'char=)
		      (Handle-Apostrophe spelling))
		     ((Ends-In spelling "S")
		      (Strip-S spelling))
		     ((Ends-In spelling "ED")
		      (Strip-ED spelling))
		     ((Ends-In spelling "ING")
		      (Strip-ING spelling))
		     (t (list (list spelling)))
		     ))))
    result))  

(in-package "TREES")
(defparameter *recursive-path* '(DISCO::SYN DISCO::GAP DISCO::LIST DISCO::FIRST))

(in-package "MAIN")

