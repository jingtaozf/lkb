--- Copyright (c) 2003-2004 
--- Fabre Lambeau, Stephan Oepen, Benjamin Waldron;
--- see `licence.txt' for conditions.

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

CREATE OR REPLACE FUNCTION public.define_type_text_text() RETURNS boolean AS '
BEGIN
	IF reln_exists(\'public\',\'text_text\') THEN
		DROP TYPE public.text_text CASCADE;
	END IF;
	CREATE TYPE text_text AS (t1 text, t2 text);
	RETURN true;
END;
' LANGUAGE plpgsql;

--
--
--

SELECT public.define_type_text_text();

--
--
--

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

CREATE OR REPLACE FUNCTION public.return_field_info(text,text) RETURNS SETOF text_text AS '
SELECT a.attname::text as field, pg_catalog.format_type(a.atttypid, a.atttypmod) as type
	FROM pg_catalog.pg_attribute a
	WHERE a.attrelid = return_oid($1,$2) AND a.attnum > 0 AND NOT a.attisdropped
	ORDER BY a.attnum
' LANGUAGE sql;


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

CREATE OR REPLACE FUNCTION server_version(text) RETURNS boolean AS '
DECLARE
	x text;
BEGIN
	SELECT INTO x * FROM version();
	x := split_part(x,'' '',2);
	RETURN x>=$1;
END;
' LANGUAGE plpgsql;

-- if server version then ...
CREATE OR REPLACE FUNCTION if_server_version(text,text,text) RETURNS text AS '
BEGIN
	IF server_version($1) THEN
		EXECUTE $2;
		RETURN true;
	ELSE
		EXECUTE $3;
		RETURN false;
 	END IF;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.db_owner() RETURNS text AS 
'
DECLARE
	uid int;
	uname text;
BEGIN
	uid := (select datdba from pg_catalog.pg_database where datname=current_database());
	uname := (select usename from pg_catalog.pg_user where usesysid=uid);
 RETURN uname;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.assert_db_owner() RETURNS boolean AS 
'
DECLARE
	uid int;
	db_uid int;
BEGIN
	uid := (select usesysid from pg_catalog.pg_user where usename=user);
	db_uid := (select datdba from pg_catalog.pg_database where datname=current_database());
 	IF uid != db_uid THEN
   		RAISE EXCEPTION \'You are not the DB owner.\';
 	END IF;
 	RETURN true;
END;
' LANGUAGE plpgsql;
