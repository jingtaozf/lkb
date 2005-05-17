--
-- function template to introduce additional sense distinctions into lexicon
-- by redefining rev_all to merge in additional senses from table lex_sense
-- field names must match the fields of the lexdb in use
--

CREATE OR REPLACE FUNCTION public.sense_distinction_setup() RETURNS boolean AS '
BEGIN
	CREATE OR REPLACE VIEW rev_all_senses_collapsed
		AS SELECT * FROM public.rev 
			UNION 
 			SELECT * FROM rev;
	CREATE OR REPLACE VIEW rev_all AS
		SELECT 	name || coalesce(sense,\'\') as name,
			userid, 
			modstamp, 
			dead, 
			type, 
			orth, 
			coalesce(sense,keyrel) as keyrel, 
			infl, 
			cargs, 
			preds, 
			keyrelpred, 
			nklid, 
			pronunciation, 
			complete, 
			semclasses, 
			preferences, 
			classifier, 
			selectrest, 
			jlink, 
			comments, 
			exemplars, 
			usages, 
			lang, 
			country, 
			dialect, 
			domains, 
			genres, 
			register, 
			confidence, 
			source
		FROM rev_all_senses_collapsed LEFT OUTER JOIN lex_sense ON name=lex_id;

RETURN true;
END;
' LANGUAGE plpgsql;