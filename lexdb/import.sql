--- Copyright (c) 2003-2004 
--- Fabre Lambeau, Stephan Oepen, Benjamin Waldron;
--- see `licence.txt' for conditions.

CREATE TABLE backup (b text);

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

CREATE OR REPLACE FUNCTION table_exists_p(text,text) RETURNS boolean AS '
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

-- check server version
CREATE OR REPLACE FUNCTION check_version(text) RETURNS boolean AS '
DECLARE
	x text;
BEGIN
 SELECT INTO x * FROM version();
 x := split_part(x,'' '',2);
 RETURN x>=$1;
END;
' LANGUAGE plpgsql;

-- if server version then ...
CREATE OR REPLACE FUNCTION if_version(text,text,text) RETURNS text AS '
DECLARE 
	x boolean;
BEGIN
	x:= false;
 IF check_version($1) THEN
	EXECUTE $2;
	x:= true;
 ELSE
	EXECUTE $3;
 END IF;
 RETURN x;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.db_version() RETURNS text AS '
BEGIN
	RETURN (SELECT val FROM public.meta WHERE var=\'db-version\');
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.create_meta_table() RETURNS boolean AS '
BEGIN
	IF (table_exists_p(\'public\',\'meta\')) THEN
		PERFORM public.hide_schemas();
		DROP TABLE public.meta CASCADE;
	END IF;
	CREATE TABLE public.meta (
		var text,
		val text
	);
	RETURN true;
END;
' LANGUAGE plpgsql;

\i fns_plpgsql.sql

SELECT create_meta_table();

CREATE OR REPLACE FUNCTION public.create_defn_table() RETURNS boolean AS '
DECLARE
	backup_file_base text;
BEGIN
	IF (table_exists_p(\'public\',\'defn\')) THEN
		DROP TABLE public.defn CASCADE;
	END IF;	
	CREATE TABLE public.defn (
			mode TEXT,
			slot TEXT,
			field TEXT,
			path TEXT,
			type TEXT,
		PRIMARY KEY (mode,slot, field)
		);

	IF (table_exists_p(\'public\',\'backup\')) THEN
		backup_file_base := (SELECT b FROM public.backup LIMIT 1);
		IF backup_file_base IS NOT NULL THEN
			PERFORM restore_public_defn_su(backup_file_base);
		END IF;
	END IF;
	RETURN true;
END;
' LANGUAGE plpgsql;


SELECT create_defn_table();

\i permissions.sql

--\i mwe.sql

---
-- meta table
---

\copy public.meta from 'public.meta.tsv'

DROP TABLE backup;