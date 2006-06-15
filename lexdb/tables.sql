--- Copyright (c) 2003 - 2006
--- Benjamin Waldron, Fabre Lambeau, Stephan Oepen;
--- see `licence.txt' for conditions.

--
-- table creation functions
--

CREATE OR REPLACE FUNCTION public.create_public_rev_table() RETURNS boolean AS '
DECLARE
	sql_qry text;
BEGIN
	sql_qry := \'CREATE TABLE public.rev (
		name TEXT NOT NULL,
		userid TEXT DEFAULT user NOT NULL,
		modstamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
		dead BOOLEAN DEFAULT \\\'0\\\' NOT NULL
		\' || soft_rev_field_definitions() || \' ) \';
	EXECUTE sql_qry;
	--PERFORM public.index_public_rev();
	RETURN TRUE;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION soft_rev_field_definitions() RETURNS text AS '
DECLARE
	x RECORD;
	t text;
BEGIN
	IF (reln_exists(\'public\',\'fld\') 
			AND 
			(SELECT count(*) FROM public.fld)>0) THEN
		RAISE DEBUG \'Using field defns found in public.fld\';
		FOR x IN SELECT dfn FROM fld LOOP
			t := COALESCE(t,\'\');
			t:= t || \',\n \' || x.dfn;
		END LOOP;
	ELSE
		RAISE EXCEPTION \'\n*\n*\n* no field definitions provided! (TABLE public.fld IS EMPTY)\n*\n*\';
	END IF;
	RETURN t;
END;
' LANGUAGE plpgsql;

--
-- private rev indexes:
--   rev_name_userid_modstamp
--

-- move these fns into Lisp code ???

CREATE OR REPLACE FUNCTION public.index_rev() RETURNS boolean AS '
BEGIN
	RAISE DEBUG \'indexing rev\';
	EXECUTE \'CREATE UNIQUE INDEX rev_name ON rev (name)\';
	RETURN true;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.deindex_rev() RETURNS boolean AS '
BEGIN
	RAISE DEBUG \'deindexing rev\';
	DROP INDEX rev_name;
	RETURN true;
END;
' LANGUAGE plpgsql;
