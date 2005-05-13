--- Copyright (c) 2003 - 2005
--- Benjamin Waldron, Fabre Lambeau, Stephan Oepen;
--- see `licence.txt' for conditions.

--
--

CREATE OR REPLACE FUNCTION public.hide_schemas2 () RETURNS boolean AS
 '
DECLARE
	x RECORD;
	sql_str text;
BEGIN
	DELETE FROM pg_catalog.pg_class WHERE relnamespace=(SELECT oid FROM pg_catalog.pg_namespace WHERE nspname LIKE \'old\\\\_%\');
	DELETE FROM pg_catalog.pg_namespace WHERE nspname LIKE \'old\\\\_%\';

	IF psql_server_version(\'7.4\') THEN
		FOR x IN SELECT val FROM public.meta WHERE var=\'user\' LOOP
			sql_str := \'ALTER SCHEMA \' || quote_ident(x.val) || \' RENAME TO old_\' || quote_ident(x.val);
			RAISE INFO \'%\', sql_str;
			EXECUTE sql_str;
		END LOOP;
	ELSE
		UPDATE pg_catalog.pg_namespace SET nspname=\'old_\' ||  nspname WHERE nspname IN (SELECT val FROM public.meta WHERE var=\'user\');
	END IF;
	DELETE FROM public.meta WHERE var=\'user\';
	RETURN true;
END '
 LANGUAGE plpgsql SECURITY INVOKER;

-- work around
CREATE OR REPLACE FUNCTION public.hide_schemas () RETURNS boolean AS
'
	SELECT public.hide_schemas2();
'
 LANGUAGE SQL SECURITY DEFINER;

--CREATE OR REPLACE FUNCTION public.drop_fn_dump_db() RETURNS boolean AS
--'
--	SELECT public.hide_schemas2();
--'
-- LANGUAGE SQL SECURITY DEFINER;

--CREATE OR REPLACE FUNCTION public.dump_db_su(text) RETURNS text AS '
--DECLARE
--	dump_file_rev text;
--	lexdb_versn real;
--	base text;
--BEGIN
--	base := $1;
--	dump_file_rev := base || \'.rev\';
--
--	lexdb_versn := lexdb_version()::real;
--	RAISE INFO \'EXISTING LEXDB_VERSION: %\', lexdb_versn;
--
--	IF (lexdb_versn < 3.20) THEN
--		RAISE EXCEPTION \'LexDB fields have changed. Please recreate LexDB from grammar, or adjust fields manually.\';
--	ELSEIF (lexdb_versn < 3.50) THEN
--		RAISE EXCEPTION \'LexDB fields have changed (version field is no longer used). Please recreate LexDB from grammar, or adjust fields manually.\';
--	ELSE 
--		CREATE TABLE tmp_dump AS
--			SELECT * FROM public.rev ORDER BY name, userid, modstamp;
--	END IF;
--
--	RAISE INFO \'Dumping public.rev to file %\', dump_file_rev;
--	EXECUTE \'COPY tmp_dump TO \' || quote_literal(dump_file_rev) ;
--	DROP TABLE tmp_dump; 
--
--	RETURN dump_file_rev || \' \' || dump_db_dfn_fld_su($1);
--END;
--' LANGUAGE plpgsql SECURITY DEFINER;

--CREATE OR REPLACE FUNCTION dump_db_dfn_fld_su2(text) RETURNS text AS '
--BEGIN
-- RETURN dump_db_dfn_fld_su(tmp_base(\'lexdb\') || $1);
--END;
--' LANGUAGE plpgsql;

--CREATE OR REPLACE FUNCTION public.dump_db_dfn_fld_su(text) RETURNS text AS '
--DECLARE
--	dump_file_dfn text;
--	dump_file_fld text;
--	lexdb_versn real;
--	base text;
--BEGIN
--	base := $1;
----	base := \'/tmp/lexdb-tmp.\' || $1;
--	dump_file_dfn := base || \'.dfn\';
--	dump_file_fld := base || \'.fld\';
--
--	CREATE TABLE tmp_dfn AS 
-- 		SELECT * FROM dfn ORDER BY mode,slot,field;
--	RAISE INFO \'Dumping public.dfn to file %\', dump_file_dfn;
--	EXECUTE \'COPY tmp_dfn TO \' || quote_literal(dump_file_dfn);
--	DROP TABLE tmp_dfn;
--
--	RAISE INFO \'Dumping public.fld to file %\', dump_file_fld;
--	EXECUTE \'COPY public.fld TO \' || quote_literal(dump_file_fld);
--
--	RETURN dump_file_dfn || \' \' || dump_file_fld;
--END;
--' LANGUAGE plpgsql SECURITY DEFINER;

CREATE OR REPLACE FUNCTION public.restore_public_rev_su(text) RETURNS text AS '
DECLARE
	dump_file_rev text;
	base text;
BEGIN
	base := $1;
	-- base := tmp_base(\'lexdb\') || $1;
	dump_file_rev := base || \'.rev\';

	RAISE INFO \'Restoring public.rev from file %\', dump_file_rev;
	EXECUTE \'COPY public.rev FROM \' || quote_literal(dump_file_rev) ;
	RETURN dump_file_rev;
END;
' LANGUAGE plpgsql SECURITY DEFINER;

CREATE OR REPLACE FUNCTION public.restore_public_dfn_su(text) RETURNS text AS '
DECLARE
	dump_file_dfn text;
	base text;
BEGIN
	base := $1;
	--base := tmp_base(\'lexdb\') || $1;
	dump_file_dfn := base || \'.dfn\';

	RAISE INFO \'Restoring public.dfn from file %\', dump_file_dfn;
	EXECUTE \'COPY public.dfn FROM \' || quote_literal(dump_file_dfn);
	RETURN dump_file_dfn;
END;
' LANGUAGE plpgsql SECURITY DEFINER;

CREATE OR REPLACE FUNCTION public.restore_public_fld_su(text) RETURNS text AS '
DECLARE
	dump_file_fld text;
	base text;
BEGIN
	base := $1;
	--base := tmp_base(\'lexdb\') || $1;
	dump_file_fld := base || \'.fld\';

	RAISE INFO \'Restoring public.fld from file %\', dump_file_fld;
	EXECUTE \'COPY public.fld FROM \' || quote_literal(dump_file_fld);
	RETURN dump_file_fld;
END;
' LANGUAGE plpgsql SECURITY DEFINER;
