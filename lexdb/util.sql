--- Copyright (c) 2003 - 2007
--- Benjamin Waldron, Fabre Lambeau, Stephan Oepen;
--- see `licence.txt' for conditions.

--x
CREATE OR REPLACE FUNCTION fn_exists(text,text) RETURNS boolean AS '
BEGIN
RETURN
	(SELECT (SELECT count(*)
		FROM pg_catalog.pg_proc p
     		LEFT JOIN pg_catalog.pg_namespace n ON n.oid = p.pronamespace
		WHERE p.prorettype <> ''pg_catalog.cstring''::pg_catalog.regtype
      		AND p.proargtypes[0] <> ''pg_catalog.cstring''::pg_catalog.regtype
      		AND NOT p.proisagg
      		AND n.nspname = $1
      		AND p.proname = $2) > 0);
END;
' LANGUAGE plpgsql;

--x
CREATE OR REPLACE FUNCTION reln_exists(text,text) RETURNS boolean AS '
BEGIN
RETURN
	(SELECT (SELECT count(*)
		FROM pg_catalog.pg_class c
     		LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
		WHERE n.nspname = $1
      		AND c.relname = $2)
		> 0);
END;
' LANGUAGE plpgsql;

--
--
--

--x
CREATE OR REPLACE FUNCTION public.define_type_text_2() RETURNS boolean AS '
BEGIN
	IF reln_exists(''public'',''text_2'') THEN
		DROP TYPE public.text_2 CASCADE;
	END IF;
	CREATE TYPE text_2 AS (t1 text, t2 text);
	RETURN true;
END;
' LANGUAGE plpgsql;

--x
CREATE OR REPLACE FUNCTION public.define_type_text_3() RETURNS boolean AS '
BEGIN
	IF reln_exists(''public'',''text_3'') THEN
		DROP TYPE public.text_3 CASCADE;
	END IF;
	CREATE TYPE text_3 AS (t1 text, t2 text, t3 text);
	RETURN true;
END;
' LANGUAGE plpgsql;

--x
CREATE OR REPLACE FUNCTION public.define_type_attname_typename_atttypmod() RETURNS boolean AS '
BEGIN
	IF reln_exists(''public'',''attname_typename_atttypmod'') THEN
		DROP TYPE public.attname_typename_atttypmod CASCADE;
	END IF;
	CREATE TYPE attname_typename_atttypmod AS (attname name, typename name, atttypmod int);
	RETURN true;
END;
' LANGUAGE plpgsql;

--
--
--

SELECT public.define_type_text_2();
SELECT public.define_type_text_3();
SELECT public.define_type_attname_typename_atttypmod();

--
--
--

--x
CREATE OR REPLACE FUNCTION public.return_oid(text,text) RETURNS integer AS '
BEGIN
	RETURN
		(
		SELECT c.oid
		FROM pg_catalog.pg_class c
     			LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
		WHERE n.nspname = $1
      		AND c.relname = $2
		);
END;
' LANGUAGE plpgsql;

--x
CREATE OR REPLACE FUNCTION public.return_type(text,text,text) RETURNS text AS '
DECLARE
	table_oid integer;
BEGIN
	table_oid := return_oid($1,$2);
	RETURN
		(
		SELECT pg_catalog.format_type(a.atttypid, a.atttypmod)
		FROM pg_catalog.pg_attribute a
		WHERE a.attrelid = table_oid AND attname=$3 AND a.attnum > 0 AND NOT a.attisdropped
		);
END;
' LANGUAGE plpgsql;

--mu
CREATE OR REPLACE FUNCTION psql_server_version(text) RETURNS boolean AS '
DECLARE
	x text;
BEGIN
	SELECT INTO x * FROM version();
	x := split_part(x,'' '',2);
	RETURN x>=$1;
END;
' LANGUAGE plpgsql;

--mu
-- if server version then ...
CREATE OR REPLACE FUNCTION if_psql_server_version(text,text,text) RETURNS text AS '
BEGIN
	IF psql_server_version($1) THEN
		EXECUTE $2;
		RETURN true;
	ELSE
		EXECUTE $3;
		RETURN false;
 	END IF;
END;
' LANGUAGE plpgsql;

--mu
CREATE OR REPLACE FUNCTION public.assert_db_owner() RETURNS boolean AS 
'
BEGIN
 	IF NOT(public.user_is_db_owner_p()) THEN
   		RAISE EXCEPTION ''You are not the DB owner.'';
 	END IF;
 	RETURN true;
END;
' LANGUAGE plpgsql;

--mu
CREATE OR REPLACE FUNCTION public.user_is_db_owner_p() RETURNS boolean AS 
'
DECLARE
	uid int;
	db_uid int;
BEGIN
	uid := (select usesysid from pg_catalog.pg_user where usename=user);
	db_uid := (select datdba from pg_catalog.pg_database where datname=current_database());
 	RETURN (uid = db_uid);
END;
' LANGUAGE plpgsql;
