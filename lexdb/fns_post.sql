--- Copyright (c) 2003 - 2005 
--- Benjamin Waldron, Fabre Lambeau, Stephan Oepen;
--- see `licence.txt' for conditions.

-- bmw20 26.11.04
-- convert sql fns w/ args to plpgsql with use of EXECUTE
--  (avoid major performance problem) 

CREATE OR REPLACE FUNCTION public.rev_new() RETURNS SETOF rev AS '
	SELECT * FROM rev_new
' LANGUAGE sql;

CREATE OR REPLACE FUNCTION public.retrieve_head_entry(text) RETURNS SETOF rev AS '
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

create table lex as select * from rev where null;
CREATE OR REPLACE FUNCTION public.retrieve_all_entries() RETURNS SETOF rev AS '
	SELECT * FROM lex
' LANGUAGE sql;
drop table lex;

-- abandon 'lower'ed orthkey: 'lower' incompatible with Lisp's downcase
-- so we must:
--  * ensure all db entries are in appropriate case
--  * convert orthkeys to appropriate case before entering db universe
CREATE OR REPLACE FUNCTION public.retrieve_entries_by_orthkey(text) RETURNS SETOF rev AS '
DECLARE
	x RECORD;
BEGIN
	FOR x IN
		--EXECUTE \'SELECT * FROM lex WHERE orthkey LIKE lower(\' || quote_literal($1) || \')\'
		EXECUTE \'SELECT * FROM lex WHERE orthkey LIKE \' || quote_literal($1)
		LOOP
		RETURN NEXT x;
	END LOOP;
	RETURN;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.retrieve_entry(text) RETURNS SETOF rev AS '
DECLARE
	x RECORD;
BEGIN
	FOR x IN
		EXECUTE \'SELECT * FROM lex WHERE name LIKE \' || quote_literal($1)
		LOOP
		RETURN NEXT x;
	END LOOP;
	RETURN;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.semi_out_of_date() RETURNS SETOF rev AS '
DECLARE
	x RECORD;
BEGIN
	FOR x IN
		SELECT g.* FROM lex as g NATURAL LEFT JOIN semi_mod as s WHERE g.modstamp > COALESCE(s.modstamp0,\'-infinity\')
		LOOP
		RETURN NEXT x;
	END LOOP;
	RETURN;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.retrieve_private_revs() RETURNS SETOF rev AS '
DECLARE
	x RECORD;
BEGIN
	FOR x IN
		SELECT * FROM rev
		LOOP
		RETURN NEXT x;
	END LOOP;
	RETURN;
END;
' LANGUAGE plpgsql;
