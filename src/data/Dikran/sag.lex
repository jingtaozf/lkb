;;; English Grammar: Version 4.0 for Linguistics 120
;;; or some approximation thereof
;;; AAC - December 2 1995
;;; MHK - July 15 1996
;;;       July 16 1996 : moved < SYN : ARG-ST > to just < ARG-ST >
;;;       July 16 1996 : subtypes nom-head and non-nom-head of head type.
;;;       July 18 1996 : new typing for part-of-speech and semantics.
;;;       July 19 1996 : CASE no longer coerces noun part of speech type.
;;;       July 23 1996 : new type structure
;;;       July 26 1996 : binding theory - argument structures typed to try to improve
;;;                      parsing speed - yikes!
;;;       July 29 1996 : can SPR case nom.
;;;                      added pronoun "I"
;;;       July 30 1996 : raising verbs (expect) decoupled gap - only head,val,sem equated.
;;;       Aug  2  1996 : idiomatic complements of take and keep must be words to block
;;;                      complement extraction

;;; Aug 8 1996 : Removed binding theory constraints and conjunction
;;;              to improve performance for Dikran's demo.

;;; lexicon

;;; recall that typing a lexical entry as a lexeme implies the following:
;;;   - orthography is a singleton difference list
;;;   - gap (slash) value is an empty difference list
;;; typing a lexical entry as a word implies only:
;;;   - orthography is a singleton difference list

;;; a note on type coercion
;;; -----------------------
;;; not all lexical entries have a constraint like:
;;;     < SYN : HEAD > = noun     <- or verb, prep, etc.
;;; this is because often making reference to one of the HEAD sub-features
;;; removes any doubt (ambiguity) about what part-of-speech type the lexeme
;;; or word belongs to.  for instance, constraining AUX to true means
;;; the lexeme or word can only be a verb, since only verbs have an AUX feature.

;;; *********
;;; * nouns *
;;; *********

I 1
<> = word
< ORTH : LST : HD > = "I"
< SYN : HEAD > = noun
< SYN : HEAD : CASE > = nom
< SYN : HEAD : REF : ANA > = false
< SYN : HEAD : REF : PRO > = true
< SYN : HEAD : AGR > = non-3sing
< SYN : HEAD : AGR : PER > = 1st
< SYN : HEAD : AGR : NUM > = sing
< SYN : HEAD : FORM > = normal
< SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS > = e-list
< SYN : GAP : LST > = < SYN : GAP : LAST >
< SEM : INDEX > = < SYN : HEAD : AGR >.

she 1
<> = word
< ORTH : LST : HD > = "she"
< SYN : HEAD > = noun
< SYN : HEAD : CASE > = nom
< SYN : HEAD : REF : ANA > = false
< SYN : HEAD : REF : PRO > = true
< SYN : HEAD : AGR : GEND > = fem
< SYN : HEAD : FORM > = normal
< SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS > = e-list
< SYN : GAP : LST > = < SYN : GAP : LAST >
< SEM : INDEX > = < SYN : HEAD : AGR >.

him 1
<> = word
< ORTH : LST : HD > = "him"
< SYN : HEAD > = noun
< SYN : HEAD : CASE > = acc
< SYN : HEAD : REF : ANA > = false
< SYN : HEAD : REF : PRO > = true
< SYN : HEAD : AGR : GEND > = masc
< SYN : HEAD : FORM > = normal
< SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS > = e-list
< SYN : GAP : LST > = < SYN : GAP : LAST >
< SEM : INDEX > = < SYN : HEAD : AGR >.

themselves 1
<> = word
< ORTH : LST : HD > = "themselves"
< SYN : HEAD > = noun
< SYN : HEAD : CASE > = acc
< SYN : HEAD : REF : ANA > = true
< SYN : HEAD : REF : PRO > = true
< SYN : HEAD : AGR > = non-3sing
< SYN : HEAD : AGR : PER > = 3rd
< SYN : HEAD : AGR : NUM > = plur
< SYN : HEAD : FORM > = normal
< SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS > = e-list
< SYN : GAP : LST > = < SYN : GAP : LAST >
< SEM : INDEX > = < SYN : HEAD : AGR >.

Kim 1
<> = word
< ORTH : LST : HD > = "Kim"
< SYN : HEAD > = noun
< SYN : HEAD : REF : ANA > = false
< SYN : HEAD : REF : PRO > = false
< SYN : HEAD : AGR > = 3sing
< SYN : HEAD : FORM > = normal
< SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS > = e-list
< SYN : GAP : LST > = < SYN : GAP : LAST >
< SEM : INDEX > = < SYN : HEAD : AGR >.

book 1
<> = lexeme
< ORTH : LST : HD > = "book"
< SYN : HEAD > = noun
< SYN : HEAD : REF : ANA > = false
< SYN : HEAD : REF : PRO > = false
< SYN : HEAD : AGR : PER > = 3rd
< SYN : HEAD : AGR > = < SYN : VAL : SPR : HD : SYN : HEAD : AGR > 
< SYN : HEAD : FORM > = normal
< SYN : VAL : SPR : HD : SYN : HEAD > = det
< SYN : VAL : SPR : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : SPR : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS > = e-list
< SEM : INDEX > = < SYN : HEAD : AGR >.

tabs 1
<> = word
< ORTH : LST : HD > = "tabs"
< SYN : HEAD > = noun
< SYN : HEAD : REF : ANA > = false
< SYN : HEAD : REF : PRO > = false
< SYN : HEAD : AGR > = non-3sing
< SYN : HEAD : AGR : PER > = 3rd
< SYN : HEAD : AGR : NUM > = plur
< SYN : HEAD : FORM > = f_tabs
< SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS > = e-list
< SYN : GAP : LST > = < SYN : GAP : LAST >
< SEM > = null-sem.

advantage 1
<> = word
< ORTH : LST : HD > = "advantage"
< SYN : HEAD > = noun
< SYN : HEAD : REF : ANA > = false
< SYN : HEAD : REF : PRO > = false
< SYN : HEAD : AGR > = 3sing
< SYN : HEAD : FORM > = f_advantage
< SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS > = e-list
< SYN : GAP : LST > = < SYN : GAP : LAST >
< SEM > = null-sem.

it 1
<> = word
< ORTH : LST : HD > = "it"
< SYN : HEAD > = noun
< SYN : HEAD : REF : ANA > = false
< SYN : HEAD : REF : PRO > = true
< SYN : HEAD : AGR > = 3sing
< SYN : HEAD : FORM > = f_it
< SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS > = e-list
< SYN : GAP : LST > = < SYN : GAP : LAST >
< SEM > = null-sem.

there 1
<> = word
< ORTH : LST : HD > = "there"
< SYN : HEAD > = noun
< SYN : HEAD : REF : ANA > = false
< SYN : HEAD : REF : PRO > = true
< SYN : HEAD : AGR : PER > = 3rd
< SYN : HEAD : FORM > = f_there
< SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS > = e-list
< SYN : GAP : LST > = < SYN : GAP : LAST >
< SEM > = null-sem.

;;; *********
;;; * verbs *
;;; *********

;;; note: the correct way to get the identity between ARG-STR and
;;; the append of SPR and COMPS is to use difference lists, but
;;; with the valence features represented as simple lists instead
;;; of diff-lists this cannot be done.

die 1
<> = lexeme
< ORTH : LST : HD > = "die"
< SYN : HEAD : AUX > = false
< SYN : HEAD : INV > = false
< SYN : HEAD : PRED > = false
< SYN : HEAD : NEG > = false
< SYN : VAL : SPR : HD : SYN : HEAD > = noun
< SYN : VAL : SPR : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : SPR : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS > = e-list
< SEM : CORPSE > = < ARG-STR : HD : SEM : INDEX >
< ARG-STR : HD > = < SYN : VAL : SPR : HD >
< ARG-STR : TL > = e-list.

;;; note that by making use of typing, constraining the CORPSE feature
;;; means that the semantics must be a die_reln - see the note above
;;; on type coercion.

love 1
<> = lexeme
< ORTH : LST : HD > = "love"
< SYN : HEAD : AUX > = false
< SYN : HEAD : INV > = false
< SYN : HEAD : PRED > = false
< SYN : HEAD : NEG > = false
< SYN : VAL : SPR : HD : SYN : HEAD > = noun
< SYN : VAL : SPR : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : SPR : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : HEAD > = noun
< SYN : VAL : COMPS : HD : SYN : HEAD : CASE > = acc
< SYN : VAL : COMPS : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL > = e-list
< SEM : LOVER > = < ARG-STR : HD : SEM : INDEX > 
< SEM : LOVED > = < ARG-STR : TL : HD : SEM : INDEX >
< ARG-STR > = nom-pred-arg-stem
< ARG-STR : HD > = < SYN : VAL : SPR : HD >
< ARG-STR : TL : HD > = < SYN : VAL : COMPS : HD >
< ARG-STR : TL : TL > = e-list.

give 1
<> = lexeme
< ORTH : LST : HD > = "give"
< SYN : HEAD : AUX > = false
< SYN : HEAD : INV > = false
< SYN : HEAD : PRED > = false
< SYN : HEAD : NEG > = false
< SYN : VAL : SPR : HD : SYN : HEAD > = noun
< SYN : VAL : SPR : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : SPR : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : HEAD > = noun
< SYN : VAL : COMPS : HD : SYN : HEAD : CASE > = acc
< SYN : VAL : COMPS : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL : HD : SYN : HEAD > = noun
< SYN : VAL : COMPS : TL : HD : SYN : HEAD : CASE > = acc
< SYN : VAL : COMPS : TL : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS : TL : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL : TL > = e-list
< SEM : GIVER > = < ARG-STR : HD : SEM : INDEX > 
< SEM : GIVEN > = < ARG-STR : TL : HD : SEM : INDEX >
< SEM : GIFT > = < ARG-STR : TL : TL : HD : SEM : INDEX >
< ARG-STR > = nom-nom-pred-arg-stem
< ARG-STR : HD > = < SYN : VAL : SPR : HD >
< ARG-STR : TL : HD > = < SYN : VAL : COMPS : HD >
< ARG-STR : TL : TL : HD > = < SYN : VAL : COMPS : TL : HD >
< ARG-STR : TL : TL : TL > = e-list.

give 2
<> = lexeme
< ORTH : LST : HD > = "give"
< SYN : HEAD : AUX > = false
< SYN : HEAD : INV > = false
< SYN : HEAD : PRED > = false
< SYN : HEAD : NEG > = false
< SYN : VAL : SPR : HD : SYN : HEAD > = noun
< SYN : VAL : SPR : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : SPR : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : HEAD > = noun
< SYN : VAL : COMPS : HD : SYN : HEAD : CASE > = acc
< SYN : VAL : COMPS : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL : HD : SYN : HEAD > = prep
< SYN : VAL : COMPS : TL : HD : SYN : HEAD : FORM > = f_to
< SYN : VAL : COMPS : TL : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS : TL : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL : TL > = e-list
< SEM : GIVER > = < ARG-STR : HD : SEM : INDEX > 
< SEM : GIFT > = < ARG-STR : TL : HD : SEM : INDEX >
< SEM : GIVEN > = < ARG-STR : TL : TL : HD : SEM : INDEX >
< ARG-STR > = nom-nom-pred-arg-stem
< ARG-STR : HD > = < SYN : VAL : SPR : HD >
< ARG-STR : TL : HD > = < SYN : VAL : COMPS : HD >
< ARG-STR : TL : TL : HD > = < SYN : VAL : COMPS : TL : HD >
< ARG-STR : TL : TL : TL > = e-list.

keep 1
<> = lexeme
< ORTH : LST : HD > = "keep"
< SYN : HEAD : AUX > = false
< SYN : HEAD : INV > = false
< SYN : HEAD : PRED > = false
< SYN : HEAD : NEG > = false
< SYN : VAL : SPR : HD : SYN : HEAD > = noun
< SYN : VAL : SPR : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : SPR : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD > = word
< SYN : VAL : COMPS : HD : SYN : HEAD > = noun
< SYN : VAL : COMPS : HD : SYN : HEAD : FORM > = f_tabs
< SYN : VAL : COMPS : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL : HD : SYN : HEAD > = prep
< SYN : VAL : COMPS : TL : HD : SYN : HEAD : FORM > = f_on
< SYN : VAL : COMPS : TL : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS : TL : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL : TL > = e-list
< SEM : OBSERVER > = < ARG-STR : HD : SEM : INDEX > 
< SEM : OBSERVED > = < ARG-STR : TL : TL : HD : SEM : INDEX >
< ARG-STR > = nom-nom-pred-arg-stem
< ARG-STR : HD > = < SYN : VAL : SPR : HD >
< ARG-STR : TL : HD > = < SYN : VAL : COMPS : HD >
< ARG-STR : TL : TL : HD > = < SYN : VAL : COMPS : TL : HD >
< ARG-STR : TL : TL : TL > = e-list.

take 1
<> = lexeme
< ORTH : LST : HD > = "take"
< SYN : HEAD : AUX > = false
< SYN : HEAD : INV > = false
< SYN : HEAD : PRED > = false
< SYN : HEAD : NEG > = false
< SYN : VAL : SPR : HD : SYN : HEAD > = noun
< SYN : VAL : SPR : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : SPR : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD > = word
< SYN : VAL : COMPS : HD : SYN : HEAD > = noun
< SYN : VAL : COMPS : HD : SYN : HEAD : FORM > = f_advantage
< SYN : VAL : COMPS : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL : HD : SYN : HEAD > = prep
< SYN : VAL : COMPS : TL : HD : SYN : HEAD : FORM > = f_of
< SYN : VAL : COMPS : TL : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS : TL : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL : TL > = e-list
< SEM : EXPLOITER > = < ARG-STR : HD : SEM : INDEX > 
< SEM : EXPLOITED > = < ARG-STR : TL : TL : HD : SEM : INDEX >
< ARG-STR > = nom-nom-pred-arg-stem
< ARG-STR : HD > = < SYN : VAL : SPR : HD >
< ARG-STR : TL : HD > = < SYN : VAL : COMPS : HD >
< ARG-STR : TL : TL : HD > = < SYN : VAL : COMPS : TL : HD >
< ARG-STR : TL : TL : TL > = e-list.

kick_the_bucket 1
<> = lexeme
< ORTH : LST : HD > = "kick"
< ORTH : LST : TL : HD > = "the"
< ORTH : LST : TL : TL : HD > = "bucket"
< SYN : HEAD : AUX > = false
< SYN : HEAD : INV > = false
< SYN : HEAD : PRED > = false
< SYN : HEAD : NEG > = false
< SYN : VAL : SPR : HD : SYN : HEAD > = noun
< SYN : VAL : SPR : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : SPR : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS > = e-list
< SEM : CORPSE > = < ARG-STR : HD : SEM : INDEX >
< ARG-STR : HD > = < SYN : VAL : SPR : HD >
< ARG-STR : TL > = e-list.

;;; be 1
;;; ====
;;; this is the "kim is loved by him" and "kim is dying" sense of the verb be.

;;; should the complement be itself complement empty?

be 1
<> = word
< ORTH : LST : HD > = "be"
< SYN : HEAD : FORM > = inf
< SYN : HEAD : AUX > = true
< SYN : HEAD : INV > = false
< SYN : HEAD : PRED > = false
< SYN : HEAD : NEG > = false
< SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : HEAD : PRED > = true
< SYN : VAL : COMPS : HD : SYN : VAL : SPR : HD > = 
   < SYN : VAL : SPR : HD >
< SYN : VAL : COMPS : HD : SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL > = e-list
< SYN : GAP : LST > = < SYN : GAP : LAST >
< SEM > = < ARG-STR : TL : HD : SEM >
< ARG-STR > = v-pred-arg-list
< ARG-STR : HD > = < SYN : VAL : SPR : HD >
< ARG-STR : TL : HD > = < SYN : VAL : COMPS : HD >
< ARG-STR : TL : TL > = e-list.

been 1
<> = word
< ORTH : LST : HD > = "been"
< SYN : HEAD : FORM > = past-p
< SYN : HEAD : AUX > = true
< SYN : HEAD : INV > = false
< SYN : HEAD : PRED > = false
< SYN : HEAD : NEG > = false
< SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : HEAD : PRED > = true
< SYN : VAL : COMPS : HD : SYN : VAL : SPR : HD > = 
   < SYN : VAL : SPR : HD >
< SYN : VAL : COMPS : HD : SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL > = e-list
< SYN : GAP : LST > = < SYN : GAP : LAST >
< SEM > = < ARG-STR : TL : HD : SEM >
< ARG-STR > = v-pred-arg-list
< ARG-STR : HD > = < SYN : VAL : SPR : HD >
< ARG-STR : TL : HD > = < SYN : VAL : COMPS : HD >
< ARG-STR : TL : TL > = e-list.

being 1
<> = word
< ORTH : LST : HD > = "being"
< SYN : HEAD : FORM > = pres-p
< SYN : HEAD : AUX > = true
< SYN : HEAD : INV > = false
< SYN : HEAD : PRED > = true
< SYN : HEAD : NEG > = false
< SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : HEAD : PRED > = true
< SYN : VAL : COMPS : HD : SYN : VAL : SPR : HD > = 
   < SYN : VAL : SPR : HD >
< SYN : VAL : COMPS : HD : SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL > = e-list
< SYN : GAP : LST > = < SYN : GAP : LAST >
< SEM > = < ARG-STR : TL : HD : SEM >
< ARG-STR > = v-pred-arg-list
< ARG-STR : HD > = < SYN : VAL : SPR : HD >
< ARG-STR : TL : HD > = < SYN : VAL : COMPS : HD >
< ARG-STR : TL : TL > = e-list.

am 1
<> = word
< ORTH : LST : HD > = "am"
< SYN : HEAD : FORM > = fin
< SYN : HEAD : AUX > = true
< SYN : HEAD : INV > = false
< SYN : HEAD : PRED > = false
< SYN : HEAD : NEG > = false
< SYN : VAL : SPR : HD : SYN : HEAD : CASE > = nom
< SYN : VAL : SPR : HD : SYN : HEAD : AGR > = non-3sing
< SYN : VAL : SPR : HD : SYN : HEAD : AGR : PER > = 1st
< SYN : VAL : SPR : HD : SYN : HEAD : AGR : NUM > = sing
< SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : HEAD : PRED > = true
< SYN : VAL : COMPS : HD : SYN : VAL : SPR : HD > = 
   < SYN : VAL : SPR : HD >
< SYN : VAL : COMPS : HD : SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL > = e-list
< SYN : GAP : LST > = < SYN : GAP : LAST >
< SEM > = < ARG-STR : TL : HD : SEM >
< ARG-STR > = v-pred-arg-list
< ARG-STR : HD > = < SYN : VAL : SPR : HD >
< ARG-STR : TL : HD > = < SYN : VAL : COMPS : HD >
< ARG-STR : TL : TL > = e-list.

was 1
<> = word
< ORTH : LST : HD > = "was"
< SYN : HEAD : FORM > = fin
< SYN : HEAD : AUX > = true
< SYN : HEAD : INV > = false
< SYN : HEAD : PRED > = false
< SYN : HEAD : NEG > = false
< SYN : VAL : SPR : HD : SYN : HEAD : CASE > = nom
< SYN : VAL : SPR : HD : SYN : HEAD : AGR > = non-3sing
< SYN : VAL : SPR : HD : SYN : HEAD : AGR : PER > = 1st
< SYN : VAL : SPR : HD : SYN : HEAD : AGR : NUM > = sing
< SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : HEAD : PRED > = true
< SYN : VAL : COMPS : HD : SYN : VAL : SPR : HD > = 
   < SYN : VAL : SPR : HD >
< SYN : VAL : COMPS : HD : SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL > = e-list
< SYN : GAP : LST > = < SYN : GAP : LAST >
< SEM > = < ARG-STR : TL : HD : SEM >
< ARG-STR > = v-pred-arg-list
< ARG-STR : HD > = < SYN : VAL : SPR : HD >
< ARG-STR : TL : HD > = < SYN : VAL : COMPS : HD >
< ARG-STR : TL : TL > = e-list.

are 1
<> = word
< ORTH : LST : HD > = "are"
< SYN : HEAD : FORM > = fin
< SYN : HEAD : AUX > = true
< SYN : HEAD : INV > = false
< SYN : HEAD : PRED > = false
< SYN : HEAD : NEG > = false
< SYN : VAL : SPR : HD : SYN : HEAD : CASE > = nom
< SYN : VAL : SPR : HD : SYN : HEAD : AGR > = non-3sing
< SYN : VAL : SPR : HD : SYN : HEAD : AGR : PER > = (2nd 3rd)
< SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : HEAD : PRED > = true
< SYN : VAL : COMPS : HD : SYN : VAL : SPR : HD > = 
   < SYN : VAL : SPR : HD >
< SYN : VAL : COMPS : HD : SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL > = e-list
< SYN : GAP : LST > = < SYN : GAP : LAST >
< SEM > = < ARG-STR : TL : HD : SEM >
< ARG-STR > = v-pred-arg-list
< ARG-STR : HD > = < SYN : VAL : SPR : HD >
< ARG-STR : TL : HD > = < SYN : VAL : COMPS : HD >
< ARG-STR : TL : TL > = e-list.

are 3
<> = word
< ORTH : LST : HD > = "are"
< SYN : HEAD : FORM > = fin
< SYN : HEAD : AUX > = true
< SYN : HEAD : INV > = false
< SYN : HEAD : PRED > = false
< SYN : HEAD : NEG > = false
< SYN : VAL : SPR : HD : SYN : HEAD : CASE > = nom
< SYN : VAL : SPR : HD : SYN : HEAD : AGR > = non-3sing
< SYN : VAL : SPR : HD : SYN : HEAD : AGR : PER > = 1st
< SYN : VAL : SPR : HD : SYN : HEAD : AGR : NUM > = plur
< SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : HEAD : PRED > = true
< SYN : VAL : COMPS : HD : SYN : VAL : SPR : HD > = 
   < SYN : VAL : SPR : HD >
< SYN : VAL : COMPS : HD : SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL > = e-list
< SYN : GAP : LST > = < SYN : GAP : LAST >
< SEM > = < ARG-STR : TL : HD : SEM >
< ARG-STR > = v-pred-arg-list
< ARG-STR : HD > = < SYN : VAL : SPR : HD >
< ARG-STR : TL : HD > = < SYN : VAL : COMPS : HD >
< ARG-STR : TL : TL > = e-list.

were 1
<> = word
< ORTH : LST : HD > = "were"
< SYN : HEAD : FORM > = fin
< SYN : HEAD : AUX > = true
< SYN : HEAD : INV > = false
< SYN : HEAD : PRED > = false
< SYN : HEAD : NEG > = false
< SYN : VAL : SPR : HD : SYN : HEAD : CASE > = nom
< SYN : VAL : SPR : HD : SYN : HEAD : AGR > = non-3sing
< SYN : VAL : SPR : HD : SYN : HEAD : AGR : PER > = (2nd 3rd)
< SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : HEAD : PRED > = true
< SYN : VAL : COMPS : HD : SYN : VAL : SPR : HD > = 
   < SYN : VAL : SPR : HD >
< SYN : VAL : COMPS : HD : SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL > = e-list
< SYN : GAP : LST > = < SYN : GAP : LAST >
< SEM > = < ARG-STR : TL : HD : SEM >
< ARG-STR > = v-pred-arg-list
< ARG-STR : HD > = < SYN : VAL : SPR : HD >
< ARG-STR : TL : HD > = < SYN : VAL : COMPS : HD >
< ARG-STR : TL : TL > = e-list.

were 3
<> = word
< ORTH : LST : HD > = "were"
< SYN : HEAD : FORM > = fin
< SYN : HEAD : AUX > = true
< SYN : HEAD : INV > = false
< SYN : HEAD : PRED > = false
< SYN : HEAD : NEG > = false
< SYN : VAL : SPR : HD : SYN : HEAD : CASE > = nom
< SYN : VAL : SPR : HD : SYN : HEAD : AGR > = non-3sing
< SYN : VAL : SPR : HD : SYN : HEAD : AGR : PER > = 1st
< SYN : VAL : SPR : HD : SYN : HEAD : AGR : NUM > = plur
< SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : HEAD : PRED > = true
< SYN : VAL : COMPS : HD : SYN : VAL : SPR : HD > = 
   < SYN : VAL : SPR : HD >
< SYN : VAL : COMPS : HD : SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL > = e-list
< SYN : GAP : LST > = < SYN : GAP : LAST >
< SEM > = < ARG-STR : TL : HD : SEM >
< ARG-STR > = v-pred-arg-list
< ARG-STR : HD > = < SYN : VAL : SPR : HD >
< ARG-STR : TL : HD > = < SYN : VAL : COMPS : HD >
< ARG-STR : TL : TL > = e-list.

is 1
<> = word
< ORTH : LST : HD > = "is"
< SYN : HEAD : FORM > = fin
< SYN : HEAD : AUX > = true
< SYN : HEAD : INV > = false
< SYN : HEAD : PRED > = false
< SYN : HEAD : NEG > = false
< SYN : VAL : SPR : HD : SYN : HEAD : CASE > = nom
< SYN : VAL : SPR : HD : SYN : HEAD : AGR > = 3sing
< SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : HEAD : PRED > = true
< SYN : VAL : COMPS : HD : SYN : VAL : SPR : HD > = 
   < SYN : VAL : SPR : HD >
< SYN : VAL : COMPS : HD : SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL > = e-list
< SYN : GAP : LST > = < SYN : GAP : LAST >
< SEM > = < ARG-STR : TL : HD : SEM >
< ARG-STR > = v-pred-arg-list
< ARG-STR : HD > = < SYN : VAL : SPR : HD >
< ARG-STR : TL : HD > = < SYN : VAL : COMPS : HD >
< ARG-STR : TL : TL > = e-list.

was 3
<> = word
< ORTH : LST : HD > = "was"
< SYN : HEAD : FORM > = fin
< SYN : HEAD : AUX > = true
< SYN : HEAD : INV > = false
< SYN : HEAD : PRED > = false
< SYN : HEAD : NEG > = false
< SYN : VAL : SPR : HD : SYN : HEAD : CASE > = nom
< SYN : VAL : SPR : HD : SYN : HEAD : AGR > = 3sing
< SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : HEAD : PRED > = true
< SYN : VAL : COMPS : HD : SYN : VAL : SPR : HD > = 
   < SYN : VAL : SPR : HD >
< SYN : VAL : COMPS : HD : SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL > = e-list
< SYN : GAP : LST > = < SYN : GAP : LAST >
< SEM > = < ARG-STR : TL : HD : SEM >
< ARG-STR > = v-pred-arg-list
< ARG-STR : HD > = < SYN : VAL : SPR : HD >
< ARG-STR : TL : HD > = < SYN : VAL : COMPS : HD >
< ARG-STR : TL : TL > = e-list.

;;; be 2
;;; ====
;;; this is the "there is kim loving the books" sense of the verb be.
;;; 
;;; Note the absence of: am_2 and being_2
;;; am: since 'there' is always 3rd person, no 1st person subjects for this sense of be.
;;; being: the 'there is' form of be does not have a present participle:
;;;        e.g. * there is there being kim loving the books.

be 2
<> = word
< ORTH : LST : HD > = "be"
< SYN : HEAD : FORM > = inf
< SYN : HEAD : AUX > = true
< SYN : HEAD : INV > = false
< SYN : HEAD : PRED > = false
< SYN : HEAD : NEG > = false
< SYN : VAL : SPR : HD : SYN : HEAD > = noun
< SYN : VAL : SPR : HD : SYN : HEAD : FORM > = f_there
< SYN : VAL : SPR : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : SPR : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : HEAD > = noun
< SYN : VAL : COMPS : HD : SYN : HEAD : REF : PRO > = false
< SYN : VAL : COMPS : HD : SYN : HEAD : AGR : NUM > =
   < SYN : VAL : SPR : HD : SYN : HEAD : AGR : NUM >
< SYN : VAL : COMPS : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL : HD : SYN : HEAD : PRED > = true
< SYN : VAL : COMPS : TL : HD : SYN : VAL : SPR : HD > =
   < SYN : VAL : COMPS : HD >
< SYN : VAL : COMPS : TL : HD : SYN : VAL : SPR : TL > =  e-list
< SYN : VAL : COMPS : TL : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL : TL > = e-list
< SYN : GAP : LST > = < SYN : GAP : LAST >
< SEM > = < ARG-STR : TL : TL : HD : SEM >
< ARG-STR > = nom-v-pred-arg-stem
< ARG-STR : HD > = < SYN : VAL : SPR : HD >
< ARG-STR : TL : HD > = < SYN : VAL : COMPS : HD >
< ARG-STR : TL : TL : HD > = < SYN : VAL : COMPS : TL : HD >
< ARG-STR : TL : TL : TL > = e-list.

been 2
<> = word
< ORTH : LST : HD > = "been"
< SYN : HEAD : FORM > = past-p
< SYN : HEAD : AUX > = true
< SYN : HEAD : INV > = false
< SYN : HEAD : PRED > = false
< SYN : HEAD : NEG > = false
< SYN : VAL : SPR : HD : SYN : HEAD > = noun
< SYN : VAL : SPR : HD : SYN : HEAD : FORM > = f_there
< SYN : VAL : SPR : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : SPR : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : HEAD > = noun
< SYN : VAL : COMPS : HD : SYN : HEAD : REF : PRO > = false
< SYN : VAL : COMPS : HD : SYN : HEAD : AGR : NUM > =
   < SYN : VAL : SPR : HD : SYN : HEAD : AGR : NUM >
< SYN : VAL : COMPS : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL : HD : SYN : HEAD : PRED > = true
< SYN : VAL : COMPS : TL : HD : SYN : VAL : SPR : HD > =
   < SYN : VAL : COMPS : HD >
< SYN : VAL : COMPS : TL : HD : SYN : VAL : SPR : TL > =  e-list
< SYN : VAL : COMPS : TL : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL : TL > = e-list
< SYN : GAP : LST > = < SYN : GAP : LAST >
< SEM > = < ARG-STR : TL : TL : HD : SEM >
< ARG-STR > = nom-v-pred-arg-stem
< ARG-STR : HD > = < SYN : VAL : SPR : HD >
< ARG-STR : TL : HD > = < SYN : VAL : COMPS : HD >
< ARG-STR : TL : TL : HD > = < SYN : VAL : COMPS : TL : HD >
< ARG-STR : TL : TL : TL > = e-list.

is 2
<> = word
< ORTH : LST : HD > = "is"
< SYN : HEAD : FORM > = fin
< SYN : HEAD : AUX > = true
< SYN : HEAD : INV > = false
< SYN : HEAD : PRED > = false
< SYN : HEAD : NEG > = false
< SYN : VAL : SPR : HD : SYN : HEAD > = noun
< SYN : VAL : SPR : HD : SYN : HEAD : FORM > = f_there
< SYN : VAL : SPR : HD : SYN : HEAD : AGR > = 3sing
< SYN : VAL : SPR : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : SPR : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : HEAD > = noun
< SYN : VAL : COMPS : HD : SYN : HEAD : REF : PRO > = false
< SYN : VAL : COMPS : HD : SYN : HEAD : AGR : NUM > =
   < SYN : VAL : SPR : HD : SYN : HEAD : AGR : NUM >
< SYN : VAL : COMPS : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL : HD : SYN : HEAD : PRED > = true
< SYN : VAL : COMPS : TL : HD : SYN : VAL : SPR : HD > =
   < SYN : VAL : COMPS : HD >
< SYN : VAL : COMPS : TL : HD : SYN : VAL : SPR : TL > =  e-list
< SYN : VAL : COMPS : TL : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL : TL > = e-list
< SYN : GAP : LST > = < SYN : GAP : LAST >
< SEM > = < ARG-STR : TL : TL : HD : SEM >
< ARG-STR > = nom-v-pred-arg-stem
< ARG-STR : HD > = < SYN : VAL : SPR : HD >
< ARG-STR : TL : HD > = < SYN : VAL : COMPS : HD >
< ARG-STR : TL : TL : HD > = < SYN : VAL : COMPS : TL : HD >
< ARG-STR : TL : TL : TL > = e-list.

are 2
<> = lexeme
< ORTH : LST : HD > = "are"
< SYN : HEAD : FORM > = fin
< SYN : HEAD : AUX > = true
< SYN : HEAD : INV > = false
< SYN : HEAD : PRED > = false
< SYN : HEAD : NEG > = false
< SYN : VAL : SPR : HD : SYN : HEAD > = noun
< SYN : VAL : SPR : HD : SYN : HEAD : FORM > = f_there
< SYN : VAL : SPR : HD : SYN : HEAD : AGR : NUM > = plur
< SYN : VAL : SPR : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : SPR : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : HEAD > = noun
< SYN : VAL : COMPS : HD : SYN : HEAD : REF : PRO > = false
< SYN : VAL : COMPS : HD : SYN : HEAD : AGR : NUM > =
   < SYN : VAL : SPR : HD : SYN : HEAD : AGR : NUM >
< SYN : VAL : COMPS : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL : HD : SYN : HEAD : PRED > = true
< SYN : VAL : COMPS : TL : HD : SYN : VAL : SPR : HD > =
   < SYN : VAL : COMPS : HD >
< SYN : VAL : COMPS : TL : HD : SYN : VAL : SPR : TL > =  e-list
< SYN : VAL : COMPS : TL : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL : TL > = e-list
< SYN : GAP : LST > = < SYN : GAP : LAST >
< SEM > = < ARG-STR : TL : TL : HD : SEM >
< ARG-STR > = nom-v-pred-arg-stem
< ARG-STR : HD > = < SYN : VAL : SPR : HD >
< ARG-STR : TL : HD > = < SYN : VAL : COMPS : HD >
< ARG-STR : TL : TL : HD > = < SYN : VAL : COMPS : TL : HD >
< ARG-STR : TL : TL : TL > = e-list.

was 2
<> = word
< ORTH : LST : HD > = "was"
< SYN : HEAD : FORM > = fin
< SYN : HEAD : AUX > = true
< SYN : HEAD : INV > = false
< SYN : HEAD : PRED > = false
< SYN : HEAD : NEG > = false
< SYN : VAL : SPR : HD : SYN : HEAD > = noun
< SYN : VAL : SPR : HD : SYN : HEAD : FORM > = f_there
< SYN : VAL : SPR : HD : SYN : HEAD : AGR > = 3sing
< SYN : VAL : SPR : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : SPR : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : HEAD > = noun
< SYN : VAL : COMPS : HD : SYN : HEAD : REF : PRO > = false
< SYN : VAL : COMPS : HD : SYN : HEAD : AGR : NUM > =
   < SYN : VAL : SPR : HD : SYN : HEAD : AGR : NUM >
< SYN : VAL : COMPS : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL : HD : SYN : HEAD : PRED > = true
< SYN : VAL : COMPS : TL : HD : SYN : VAL : SPR : HD > =
   < SYN : VAL : COMPS : HD >
< SYN : VAL : COMPS : TL : HD : SYN : VAL : SPR : TL > =  e-list
< SYN : VAL : COMPS : TL : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL : TL > = e-list
< SYN : GAP : LST > = < SYN : GAP : LAST >
< SEM > = < ARG-STR : TL : TL : HD : SEM >
< ARG-STR > = nom-v-pred-arg-stem
< ARG-STR : HD > = < SYN : VAL : SPR : HD >
< ARG-STR : TL : HD > = < SYN : VAL : COMPS : HD >
< ARG-STR : TL : TL : HD > = < SYN : VAL : COMPS : TL : HD >
< ARG-STR : TL : TL : TL > = e-list.

were 2
<> = lexeme
< ORTH : LST : HD > = "were"
< SYN : HEAD : FORM > = fin
< SYN : HEAD : AUX > = true
< SYN : HEAD : INV > = false
< SYN : HEAD : PRED > = false
< SYN : HEAD : NEG > = false
< SYN : VAL : SPR : HD : SYN : HEAD > = noun
< SYN : VAL : SPR : HD : SYN : HEAD : FORM > = f_there
< SYN : VAL : SPR : HD : SYN : HEAD : AGR : NUM > = plur
< SYN : VAL : SPR : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : SPR : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : HEAD > = noun
< SYN : VAL : COMPS : HD : SYN : HEAD : REF : PRO > = false
< SYN : VAL : COMPS : HD : SYN : HEAD : AGR : NUM > =
   < SYN : VAL : SPR : HD : SYN : HEAD : AGR : NUM >
< SYN : VAL : COMPS : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL : HD : SYN : HEAD : PRED > = true
< SYN : VAL : COMPS : TL : HD : SYN : VAL : SPR : HD > =
   < SYN : VAL : COMPS : HD >
< SYN : VAL : COMPS : TL : HD : SYN : VAL : SPR : TL > =  e-list
< SYN : VAL : COMPS : TL : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL : TL > = e-list
< SYN : GAP : LST > = < SYN : GAP : LAST >
< SEM > = < ARG-STR : TL : TL : HD : SEM >
< ARG-STR > = nom-v-pred-arg-stem
< ARG-STR : HD > = < SYN : VAL : SPR : HD >
< ARG-STR : TL : HD > = < SYN : VAL : COMPS : HD >
< ARG-STR : TL : TL : HD > = < SYN : VAL : COMPS : TL : HD >
< ARG-STR : TL : TL : TL > = e-list.

annoy 1
<> = lexeme
< ORTH : LST : HD > = "annoy"
< SYN : HEAD : AUX > = false
< SYN : HEAD : INV > = false
< SYN : HEAD : PRED > = false
< SYN : HEAD : NEG > = false
< SYN : VAL : SPR : HD : SYN : HEAD > = comp
< SYN : VAL : SPR : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : HEAD > = noun
< SYN : VAL : COMPS : HD : SYN : HEAD : CASE > = acc
< SYN : VAL : COMPS : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL > = e-list
< SEM : ANNOYANCE > = < ARG-STR : HD : SEM > 
< SEM : ANNOYED > = < ARG-STR : TL : HD : SEM : INDEX >
< ARG-STR > = nom-pred-arg-stem
< ARG-STR : HD > = < SYN : VAL : SPR : HD >
< ARG-STR : TL : HD > = < SYN : VAL : COMPS : HD >
< ARG-STR : TL : TL > = e-list.

continue 1
<> = lexeme
< ORTH : LST : HD > = "continue"
< SYN : HEAD : AUX > = false
< SYN : HEAD : INV > = false
< SYN : HEAD : PRED > = false
< SYN : HEAD : NEG > = false
< SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : HEAD > = comp
< SYN : VAL : COMPS : HD : SYN : HEAD : FORM > = inf
< SYN : VAL : COMPS : HD : SYN : VAL : SPR > = < SYN : VAL : SPR >
< SYN : VAL : COMPS : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL > = e-list
< SEM : CONTINUANT > = < ARG-STR : TL : HD : SEM >
< ARG-STR > = nom-pred-arg-stem
< ARG-STR : HD > = < SYN : VAL : SPR : HD >
< ARG-STR : TL : HD > = < SYN : VAL : COMPS : HD >
< ARG-STR : TL : TL > = e-list.

try 1
<> = lexeme
< ORTH : LST : HD > = "try"
< SYN : HEAD : AUX > = false
< SYN : HEAD : INV > = false
< SYN : HEAD : PRED > = false
< SYN : HEAD : NEG > = false
< SYN : VAL : SPR : HD : SYN : HEAD > = noun
< SYN : VAL : SPR : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : SPR : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : HEAD > = comp
< SYN : VAL : COMPS : HD : SYN : HEAD : FORM > = inf
< SYN : VAL : COMPS : HD : SYN : VAL : SPR : HD : SYN : HEAD > = noun
< SYN : VAL : COMPS : HD : SYN : VAL : SPR : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS : HD : SYN : VAL : SPR : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : HD : SYN : VAL : SPR : HD : SEM : INDEX > =
   < SYN : VAL : SPR : HD : SEM : INDEX >
< SYN : VAL : COMPS : HD : SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL > = e-list
< SEM : TRYER > = < ARG-STR : HD : SEM : INDEX >
< SEM : TRIED > = < ARG-STR : TL : HD : SEM >
< ARG-STR > = nom-pred-arg-stem
< ARG-STR : HD > = < SYN : VAL : SPR : HD >
< ARG-STR : TL : HD > = < SYN : VAL : COMPS : HD >
< ARG-STR : TL : TL > = e-list.

expect 1
<> = lexeme
< ORTH : LST : HD > = "expect"
< SYN : HEAD : AUX > = false
< SYN : HEAD : INV > = false
< SYN : HEAD : PRED > = false
< SYN : HEAD : NEG > = false
< SYN : VAL : SPR : HD : SYN : HEAD > = noun
< SYN : VAL : SPR : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : SPR : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : TL : HD : SYN : HEAD > = comp
< SYN : VAL : COMPS : TL : HD : SYN : HEAD : FORM > = inf
< SYN : VAL : COMPS : TL : HD : SYN : VAL : SPR : HD : SYN : HEAD > = 
   < SYN : VAL : COMPS : HD : SYN : HEAD >
< SYN : VAL : COMPS : TL : HD : SYN : VAL : SPR : HD : SYN : VAL > = 
   < SYN : VAL : COMPS : HD : SYN : VAL >
< SYN : VAL : COMPS : TL : HD : SYN : VAL : SPR : HD : SYN : GAP : LST > = 
   < SYN : VAL : COMPS : TL : HD : SYN : VAL : SPR : HD : SYN : GAP : LAST >
< SYN : VAL : COMPS : TL : HD : SYN : VAL : SPR : HD : SEM > = 
   < SYN : VAL : COMPS : HD : SEM >
< SYN : VAL : COMPS : TL : HD : SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : TL : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL : TL > = e-list
< SEM : EXPECTOR > = < ARG-STR : HD : SEM : INDEX >
< SEM : EXPECTED > = < ARG-STR : TL : TL : HD : SEM >
< ARG-STR > = nom-nom-pred-arg-stem
< ARG-STR : HD > = < SYN : VAL : SPR : HD >
< ARG-STR : TL : HD > = < SYN : VAL : COMPS : HD >
< ARG-STR : TL : TL : HD > = < SYN : VAL : COMPS : TL : HD >
< ARG-STR : TL : TL : TL > = e-list.

persuade 1
<> = lexeme
< ORTH : LST : HD > = "persuade"
< SYN : HEAD : AUX > = false
< SYN : HEAD : INV > = false
< SYN : HEAD : PRED > = false
< SYN : HEAD : NEG > = false
< SYN : VAL : SPR : HD : SYN : HEAD > = noun
< SYN : VAL : SPR : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : SPR : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : HEAD > = noun
< SYN : VAL : COMPS : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL : HD : SYN : HEAD > = comp
< SYN : VAL : COMPS : TL : HD : SYN : HEAD : FORM > = inf
< SYN : VAL : COMPS : TL : HD : SYN : VAL : SPR : HD : SYN : HEAD > = noun
< SYN : VAL : COMPS : TL : HD : SYN : VAL : SPR : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS : TL : HD : SYN : VAL : SPR : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL : HD : SYN : VAL : SPR : HD : SEM : INDEX > =
   < SYN : VAL : COMPS : HD : SEM : INDEX >
< SYN : VAL : COMPS : TL : HD : SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : TL : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL : TL > = e-list
< SEM : PERSUADER > = < ARG-STR : HD : SEM : INDEX >
< SEM : PERSUADED > = < ARG-STR : TL : HD : SEM : INDEX >
< SEM : PERSUADED-OF > = < ARG-STR : TL : TL : HD : SEM >
< ARG-STR > = nom-nom-pred-arg-stem
< ARG-STR : HD > = < SYN : VAL : SPR : HD >
< ARG-STR : TL : HD > = < SYN : VAL : COMPS : HD >
< ARG-STR : TL : TL : HD > = < SYN : VAL : COMPS : TL : HD >
< ARG-STR : TL : TL : TL > = e-list.

have 1
<> = lexeme
< ORTH : LST : HD > = "have"
< SYN : HEAD : AUX > = true
< SYN : HEAD : INV > = false
< SYN : HEAD : PRED > = false
< SYN : HEAD : NEG > = false
< SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : HEAD > = verb
< SYN : VAL : COMPS : HD : SYN : HEAD : FORM > = past-p
< SYN : VAL : COMPS : HD : SYN : VAL : SPR : HD > = 
   < SYN : VAL : SPR : HD >
< SYN : VAL : COMPS : HD : SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : TL > = e-list
< SEM : HAVE-ARG > = < ARG-STR : TL : HD : SEM >
< ARG-STR > = v-pred-arg-list
< ARG-STR : HD > = < SYN : VAL : SPR : HD >
< ARG-STR : TL : HD > = < SYN : VAL : COMPS : HD >
< ARG-STR : TL : TL > = e-list.

can 1
<> = word
< ORTH : LST : HD > = "can"
< SYN : HEAD : FORM > = fin
< SYN : HEAD : AUX > = true
< SYN : HEAD : INV > = false
< SYN : HEAD : PRED > = false
< SYN : HEAD : NEG > = false
< SYN : VAL : SPR : HD : SYN : HEAD : CASE > = nom
< SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : HEAD > = verb
< SYN : VAL : COMPS : HD : SYN : HEAD : FORM > = inf
< SYN : VAL : COMPS : HD : SYN : VAL : SPR : HD > = 
   < SYN : VAL : SPR : HD >
< SYN : VAL : COMPS : HD : SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : TL > = e-list
< SEM : CAN-ARG > = < ARG-STR : TL : HD : SEM >
< SYN : GAP : LST > = < SYN : GAP : LAST >
< ARG-STR > = v-pred-arg-list
< ARG-STR : HD > = < SYN : VAL : SPR : HD >
< ARG-STR : TL : HD > = < SYN : VAL : COMPS : HD >
< ARG-STR : TL : TL > = e-list.

do 1
<> = lexeme
< ORTH : LST : HD > = "do"
< SYN : HEAD : AUX > = true
< SYN : HEAD : INV > = false
< SYN : HEAD : PRED > = false
< SYN : HEAD : NEG > = false
< SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : HEAD : FORM > = inf
< SYN : VAL : COMPS : HD : SYN : HEAD : AUX > = false
< SYN : VAL : COMPS : HD : SYN : VAL : SPR : HD > = 
   < SYN : VAL : SPR : HD >
< SYN : VAL : COMPS : HD : SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : TL > = e-list
< SEM > = < ARG-STR : TL : HD : SEM >
< ARG-STR > = v-pred-arg-list
< ARG-STR : HD > = < SYN : VAL : SPR : HD >
< ARG-STR : TL : HD > = < SYN : VAL : COMPS : HD >
< ARG-STR : TL : TL > = e-list.

;;; *****************
;;; * odds and ends *
;;; *****************

the 1
<> = word
< ORTH : LST : HD > = "the"
< SYN : HEAD > = det
< SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS > = e-list 
< SYN : GAP : LST > = < SYN : GAP : LAST >.

few 1
<> = word
< ORTH : LST : HD > = "few"
< SYN : HEAD > = det
< SYN : HEAD : AGR > = non-3sing
< SYN : HEAD : AGR : NUM > = plur
< SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS > = e-list 
< SYN : GAP : LST > = < SYN : GAP : LAST >.

s 1
<> = word
< ORTH : LST : HD > = "s"
< SYN : HEAD > = det
< SYN : VAL : SPR : HD : SYN : HEAD > = noun
< SYN : VAL : SPR : HD : SYN : HEAD : REF : PRO > = false
< SYN : VAL : SPR : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : SPR : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : SPR : HD : SEM > = ind-obj
< SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS > = e-list 
< SYN : GAP : LST > = < SYN : GAP : LAST >.

to 1
<> = word
< ORTH : LST : HD > = "to"
< SYN : HEAD > = prep
< SYN : HEAD : FORM > = f_to
< SYN : HEAD : REF > = < SYN : VAL : COMPS : HD : SYN : HEAD : REF >
< SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS : HD : SYN : HEAD > = noun
< SYN : VAL : COMPS : HD : SYN : HEAD : CASE > = acc
< SYN : VAL : COMPS : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL > = e-list
< SYN : GAP : LST > = < SYN : GAP : LAST >
< SEM > = < SYN : VAL : COMPS : HD : SEM >
< ARG-STR > = e-list.

on 1
<> = word
< ORTH : LST : HD > = "on"
< SYN : HEAD > = prep
< SYN : HEAD : FORM > = f_on
< SYN : HEAD : REF > = < SYN : VAL : COMPS : HD : SYN : HEAD : REF >
< SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS : HD : SYN : HEAD > = noun
< SYN : VAL : COMPS : HD : SYN : HEAD : CASE > = acc
< SYN : VAL : COMPS : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL > = e-list
< SYN : GAP : LST > = < SYN : GAP : LAST >
< SEM > = < SYN : VAL : COMPS : HD : SEM >
< ARG-STR > = e-list.

of 1
<> = word
< ORTH : LST : HD > = "of"
< SYN : HEAD > = prep
< SYN : HEAD : FORM > = f_of
< SYN : HEAD : REF > = < SYN : VAL : COMPS : HD : SYN : HEAD : REF >
< SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS : HD : SYN : HEAD > = noun
< SYN : VAL : COMPS : HD : SYN : HEAD : CASE > = acc
< SYN : VAL : COMPS : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL > = e-list
< SYN : GAP : LST > = < SYN : GAP : LAST >
< SEM > = < SYN : VAL : COMPS : HD : SEM >
< ARG-STR > = e-list.

by 1
<> = word
< ORTH : LST : HD > = "by"
< SYN : HEAD > = prep
< SYN : HEAD : FORM > = f_by
< SYN : HEAD : REF > = < SYN : VAL : COMPS : HD : SYN : HEAD : REF >
< SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS : HD : SYN : HEAD > = noun
< SYN : VAL : COMPS : HD : SYN : HEAD : CASE > = acc
< SYN : VAL : COMPS : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL > = e-list
< SYN : GAP : LST > = < SYN : GAP : LAST >
< SEM > = < SYN : VAL : COMPS : HD : SEM >
< ARG-STR > = e-list.

that 1
<> = word
< ORTH : LST : HD > = "that"
< SYN : HEAD > =  comp
< SYN : HEAD : FORM > = fin
< SYN : HEAD : AGR > = 3sing
< SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS : HD : SYN : HEAD > = verb
< SYN : VAL : COMPS : HD : SYN : HEAD : FORM > = fin
< SYN : VAL : COMPS : HD : SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL > = e-list
< SYN : GAP : LST > = < SYN : GAP : LAST >
< SEM > = < SYN : VAL : COMPS : HD : SEM >
< ARG-STR > = e-list.

to 2
<> = word
< ORTH : LST : HD > = "to"
< SYN : HEAD > = comp
< SYN : HEAD : FORM > = inf
< SYN : HEAD : AGR > = 3sing
< SYN : VAL : SPR : TL > = e-list
< SYN : VAL : COMPS : HD : SYN : HEAD > = verb
< SYN : VAL : COMPS : HD : SYN : HEAD : FORM > = inf
< SYN : VAL : COMPS : HD : SYN : VAL : SPR > = < SYN : VAL : SPR >
< SYN : VAL : COMPS : HD : SYN : VAL : COMPS > = e-list
< SYN : VAL : COMPS : TL > = e-list
< SYN : GAP : LST > = < SYN : GAP : LAST >
< SEM > = < SYN : VAL : COMPS : HD : SEM >
< ARG-STR > = e-list.

not 1
<> = word
< ORTH : LST : HD > = "not"
< SYN : HEAD > = adv
< SYN : HEAD : FORM > = f_not
< SYN : VAL : SPR > = e-list
< SYN : VAL : COMPS > = e-list
< SYN : GAP : LST > = < SYN : GAP : LAST >.
