--- Copyright (c) 2003 - 2005 
--- Benjamin Waldron, Fabre Lambeau, Stephan Oepen;
--- see `licence.txt' for conditions.

-- bmw20 26.11.04
-- convert sql fns w/ args to plpgsql with use of EXECUTE
--  (avoid major performance problem) 

create table lex as select * from rev where null;
CREATE OR REPLACE FUNCTION public.retrieve_all_entries() RETURNS SETOF rev AS '
	SELECT * FROM lex
' LANGUAGE sql;
drop table lex;

--  * ensure all db entries are in appropriate case
--  * convert orthkeys to appropriate case before entering db universe
CREATE OR REPLACE FUNCTION public.retrieve_entries_by_orthkey(text) RETURNS SETOF rev AS '
DECLARE
	x RECORD;
BEGIN
	FOR x IN
		EXECUTE \'SELECT lex.* FROM lex JOIN lex_key USING (name,userid,modstamp) WHERE lex_key.key LIKE \' || quote_literal($1)
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

CREATE OR REPLACE FUNCTION public.rev() RETURNS SETOF rev AS '
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

--

--CREATE OR REPLACE FUNCTION public.lookup_general3(text,text) RETURNS SETOF rev AS '
--DECLARE
--	x RECORD;
--	sql_str text;
--BEGIN
--	sql_str := \'SELECT * FROM lex WHERE \' || quote_ident($1) || \' ILIKE \' || quote_literal($2);
--	FOR x IN 
--		EXECUTE sql_str LOOP 
--     	  RETURN NEXT x;
-- 	END LOOP;	
--	RETURN;
--END;
--' LANGUAGE plpgsql;

--CREATE OR REPLACE FUNCTION public.lookup_general3_null(text) RETURNS SETOF rev AS '
--DECLARE
--	x RECORD;
--	sql_str text;
--BEGIN
--	sql_str := \'SELECT * FROM lex WHERE \' || quote_ident($1) || \' IS NULL \';
--	FOR x IN EXECUTE sql_str LOOP 
--     	  RETURN NEXT x;
-- 	END LOOP;	
--	RETURN;
--END;
--' LANGUAGE plpgsql;

---

---



CREATE OR REPLACE FUNCTION public.lookup_general3_rev_all(text,text) RETURNS SETOF rev AS '
DECLARE
	x RECORD;
	sql_str text;
BEGIN
	sql_str := \'SELECT * FROM rev_all WHERE \' || quote_ident($1) || \' ILIKE \' || quote_literal($2);
	FOR x IN EXECUTE sql_str LOOP 
     	  RETURN NEXT x;
 	END LOOP;	
	RETURN;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.lookup_general3_null_rev_all(text) RETURNS SETOF rev AS '
DECLARE
	x RECORD;
	sql_str text;
BEGIN
	sql_str := \'SELECT * FROM rev_all WHERE \' || quote_ident($1) || \' IS NULL \';
	FOR x IN EXECUTE sql_str LOOP 
     	  RETURN NEXT x;
 	END LOOP;	
	RETURN;
END;
' LANGUAGE plpgsql;



---

CREATE OR REPLACE FUNCTION public.retrieve_entry_ium(text,text,text) RETURNS SETOF rev AS '
DECLARE
	x RECORD;
BEGIN
	FOR x IN
		EXECUTE \'SELECT * FROM rev_all WHERE (name,userid,modstamp) = (\' || quote_literal($1) || \',\' || quote_literal($2) || \',\' || quote_literal($3) || \')\'
		LOOP
		RETURN NEXT x;
	END LOOP;
	RETURN;
END;
' LANGUAGE plpgsql;

