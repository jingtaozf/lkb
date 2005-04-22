--- Copyright (c) 2003-2005
--- Fabre Lambeau, Stephan Oepen, Benjamin Waldron;
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

	IF server_version(\'7.4\') THEN
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

-- fix works only for erg/default fields
CREATE OR REPLACE FUNCTION dump_db_su2(text) RETURNS text AS '
BEGIN
 RETURN dump_db_su(tmp_base(\'lexdb\') || $1);
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.dump_db_su(text) RETURNS text AS '
DECLARE
	dump_file_rev text;
	lexdb_versn real;
	base text;
BEGIN
	base := $1;
	dump_file_rev := base || \'.rev\';

 	lexdb_versn := lexdb_version()::real;
	RAISE INFO \'EXISTING LEXDB_VERSION: %\', lexdb_versn;

	IF (lexdb_versn < 3.20) THEN
		RAISE EXCEPTION \'LexDB fields have changed. Please recreate LexDB from grammar, or adjust fields manually.\';
	ELSEIF (lexdb_versn < 3.50) THEN
		RAISE EXCEPTION \'LexDB fields have changed (version field is no longer used). Please recreate LexDB from grammar, or adjust fields manually.\';
	ELSE 
		CREATE TABLE temp_dump AS
			SELECT * FROM public.revision ORDER BY name, userid, modstamp;
	END IF;

	RAISE INFO \'Dumping public.revision to file %\', dump_file_rev;
	EXECUTE \'COPY temp_dump TO \' || quote_literal(dump_file_rev) ;
	DROP TABLE temp_dump; 

	RETURN dump_file_rev || \' \' || dump_db_dfn_fld_su($1);
END;
' LANGUAGE plpgsql SECURITY DEFINER;

CREATE OR REPLACE FUNCTION dump_db_dfn_fld_su2(text) RETURNS text AS '
BEGIN
 RETURN dump_db_dfn_fld_su(tmp_base(\'lexdb\') || $1);
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.dump_db_dfn_fld_su(text) RETURNS text AS '
DECLARE
	dump_file_dfn text;
	dump_file_fld text;
	lexdb_versn real;
	base text;
BEGIN
	base := $1;
--	base := \'/tmp/lexdb-temp.\' || $1;
	dump_file_dfn := base || \'.dfn\';
	dump_file_fld := base || \'.fld\';

	CREATE TABLE temp_defn AS 
  		SELECT * FROM defn ORDER BY mode,slot,field;
	RAISE INFO \'Dumping public.defn to file %\', dump_file_dfn;
	EXECUTE \'COPY temp_defn TO \' || quote_literal(dump_file_dfn);
	DROP TABLE temp_defn;

	RAISE INFO \'Dumping public.fields to file %\', dump_file_fld;
	EXECUTE \'COPY public.fields TO \' || quote_literal(dump_file_fld);

	RETURN dump_file_dfn || \' \' || dump_file_fld;
END;
' LANGUAGE plpgsql SECURITY DEFINER;

CREATE OR REPLACE FUNCTION public.restore_public_revision_su(text) RETURNS text AS '
DECLARE
	dump_file_rev text;
	base text;
BEGIN
	base := $1;
	-- base := tmp_base(\'lexdb\') || $1;
	dump_file_rev := base || \'.rev\';

	RAISE INFO \'Restoring public.revision from file %\', dump_file_rev;
	EXECUTE \'COPY public.revision FROM \' || quote_literal(dump_file_rev) ;
	RETURN dump_file_rev;
END;
' LANGUAGE plpgsql SECURITY DEFINER;

CREATE OR REPLACE FUNCTION public.restore_public_defn_su(text) RETURNS text AS '
DECLARE
	dump_file_dfn text;
	base text;
BEGIN
	base := $1;
	--base := tmp_base(\'lexdb\') || $1;
	dump_file_dfn := base || \'.dfn\';

	RAISE INFO \'Restoring public.defn from file %\', dump_file_dfn;
	EXECUTE \'COPY public.defn FROM \' || quote_literal(dump_file_dfn);
	RETURN dump_file_dfn;
END;
' LANGUAGE plpgsql SECURITY DEFINER;

CREATE OR REPLACE FUNCTION public.restore_public_fields_su(text) RETURNS text AS '
DECLARE
	dump_file_fld text;
	base text;
BEGIN
	base := $1;
	--base := tmp_base(\'lexdb\') || $1;
	dump_file_fld := base || \'.fld\';

	RAISE INFO \'Restoring public.fields from file %\', dump_file_fld;
	EXECUTE \'COPY public.fields FROM \' || quote_literal(dump_file_fld);
	RETURN dump_file_fld;
END;
' LANGUAGE plpgsql SECURITY DEFINER;

----fix me
--CREATE OR REPLACE FUNCTION public.dump_multi_db(text) RETURNS boolean AS '
--BEGIN
--DELETE FROM temp_multi;
--INSERT INTO temp_multi
-- (SELECT * FROM multi ORDER BY name);
--EXECUTE \'COPY temp_multi TO \' || $1 ;
-- RETURN true;
--END;
--' LANGUAGE plpgsql SECURITY DEFINER;

----fix me
--CREATE OR REPLACE FUNCTION public.merge_multi_into_db(text) RETURNS boolean AS '
--BEGIN
-- DELETE FROM temp_multi;
-- EXECUTE \' COPY temp_multi FROM \' || $1 ;
-- DELETE FROM public.multi WHERE name IN (SELECT name FROM temp_multi);
-- INSERT INTO public.multi
--  (SELECT * FROM temp_multi);
--
-- DELETE FROM public.meta WHERE var=\'mod_time\';
-- INSERT INTO public.meta VALUES (\'mod_time\',current_timestamp);
--
-- RETURN true;
--END;
--' LANGUAGE plpgsql SECURITY DEFINER;
