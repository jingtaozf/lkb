--- Copyright (c) 2003-2004 
--- Fabre Lambeau, Stephan Oepen, Benjamin Waldron;
--- see `licence.txt' for conditions.

-- bmw20 26.11.04
-- convert sql fns w/ args to plpgsql with use of EXECUTE
--  (avoid major performance inadequacy) 

create table revision_all as select * from revision where null;
CREATE OR REPLACE FUNCTION public.revision_new() RETURNS SETOF revision AS '
	SELECT * FROM revision_new
' LANGUAGE sql;
drop table revision_all;

CREATE OR REPLACE FUNCTION public.retrieve_head_entry(text) RETURNS SETOF revision AS '
DECLARE
	x RECORD;
BEGIN
	FOR x IN
		EXECUTE \'SELECT * FROM filtered WHERE name LIKE \' || quote_literal($1) || \' AND modstamp=(SELECT max(modstamp) FROM filtered WHERE name LIKE \' || quote_literal($1) || \') LIMIT 1\'
		LOOP
		RETURN NEXT x;
	END LOOP;
	RETURN;
END;

' LANGUAGE plpgsql;

create table current_grammar as select * from revision where null;
CREATE OR REPLACE FUNCTION public.retrieve_all_entries() RETURNS SETOF revision AS '
	SELECT * FROM current_grammar
' LANGUAGE sql;
drop table current_grammar;

CREATE OR REPLACE FUNCTION public.retrieve_entries_by_orthkey(text) RETURNS SETOF revision AS '
DECLARE
	x RECORD;
BEGIN
	FOR x IN
		EXECUTE \'SELECT * FROM current_grammar WHERE orthkey LIKE \' || quote_literal($1)
		LOOP
		RETURN NEXT x;
	END LOOP;
	RETURN;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.retrieve_entry(text) RETURNS SETOF revision AS '
DECLARE
	x RECORD;
BEGIN
	FOR x IN
		EXECUTE \'SELECT * FROM current_grammar WHERE name LIKE \' || quote_literal($1)
		LOOP
		RETURN NEXT x;
	END LOOP;
	RETURN;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.semi_out_of_date() RETURNS SETOF revision AS '
DECLARE
	x RECORD;
BEGIN
	FOR x IN
		SELECT * FROM current_grammar as g NATURAL LEFT JOIN semi_mod as s WHERE g.modstamp > COALESCE(s.modstamp,\'-infinity\')
		LOOP
		RETURN NEXT x;
	END LOOP;
	RETURN;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.retrieve_private_revisions() RETURNS SETOF revision AS '
DECLARE
	x RECORD;
BEGIN
	FOR x IN
		SELECT * FROM revision
		LOOP
		RETURN NEXT x;
	END LOOP;
	RETURN;
END;
' LANGUAGE plpgsql;

