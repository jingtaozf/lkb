--- Copyright (c) 2003-2004 
--- Fabre Lambeau, Stephan Oepen, Benjamin Waldron;
--- see `licence.txt' for conditions.

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

\i embedded-code.sql

SELECT create_meta_table();

---
-- main table
---
CREATE OR REPLACE FUNCTION public.recreate_public_revision() RETURNS boolean AS '
BEGIN

	IF (table_exists_p(\'public\',\'revision\')) THEN
		PERFORM dump_db_su(\'BACKUP-BEFORE-LEXDB-UPDATE\');
		INSERT INTO public.meta VALUES (\'_backup\',\'BACKUP-BEFORE-LEXDB-UPDATE\');
		DROP TABLE public.revision CASCADE;
	END IF;
	---
	--- main table
	---
	CREATE TABLE public.revision (
		name TEXT NOT NULL,
		userid TEXT DEFAULT user,
		version INTEGER DEFAULT 0,
		modstamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
		f1 TEXT,
		f2 TEXT,
		orthkey TEXT NOT NULL,
		pronunciation TEXT,
		f3 TEXT,
		f4 TEXT,
		f5 TEXT,
		f6 TEXT,
		f7 TEXT,
		f8 TEXT,
		f9 TEXT,
		complete TEXT,
		semclasses TEXT,
		preferences TEXT,
		classifier TEXT,
		selectrest TEXT,
		jlink TEXT,
		comments TEXT,
		exemplars TEXT,
		usages TEXT,
		lang TEXT DEFAULT \'EN\',
		country TEXT,
		dialect TEXT,
		domains TEXT,
		genres TEXT,
		register TEXT,
		confidence real NOT NULL,
		source TEXT,
		flags INTEGER DEFAULT 0 NOT NULL
	);

	IF (SELECT var FROM public.meta WHERE var=\'_backup\') IS NOT NULL THEN
		PERFORM restore_public_revision_su(\'BACKUP-BEFORE-LEXDB-UPDATE\');
	END IF;

	PERFORM public.index_public_revision();

	RETURN TRUE;
END;
' LANGUAGE plpgsql;

SELECT recreate_public_revision();

--\i embedded-code.sql

CREATE OR REPLACE FUNCTION public.create_defn_table() RETURNS boolean AS '
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
	IF (SELECT var FROM public.meta WHERE var=\'_backup\') IS NOT NULL THEN
		PERFORM restore_public_defn_su(\'BACKUP-BEFORE-LEXDB-UPDATE\');
	END IF;
	RETURN true;
END;
' LANGUAGE plpgsql;


SELECT create_defn_table();

---DELETE FROM defn WHERE mode = 'EXAMPLE_erg';
---DELETE FROM defn WHERE mode = '_EXAMPLE_erg';
---\copy defn FROM 'defn.tsv'

CREATE OR REPLACE FUNCTION public.create_bc_temp_tables() RETURNS boolean AS '
BEGIN
	IF (table_exists_p(\'public\',\'temp\')) THEN
		DROP TABLE public.temp CASCADE;
	END IF;
	IF (table_exists_p(\'public\',\'revision_new\')) THEN
		DROP TABLE public.revision_new CASCADE;
	END IF;
	CREATE TABLE public.temp AS SELECT * FROM public.revision WHERE NULL;
	CREATE TABLE public.revision_new AS SELECT * FROM public.revision WHERE NULL;
	RETURN true;
END;
' LANGUAGE plpgsql;

SELECT public.create_bc_temp_tables();

\i permissions.sql

--\i mwe.sql

---
-- meta table
---

\copy public.meta from 'public.meta.tsv'

---
-- retain for backwards compatibility...
---

CREATE OR REPLACE FUNCTION public.orthography_set() RETURNS SETOF text AS '
BEGIN
    RETURN (SELECT DISTINCT f2 FROM current_grammar);
END' LANGUAGE plpgsql;

