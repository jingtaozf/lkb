CREATE FUNCTION "plpgsql_call_handler" () RETURNS LANGUAGE_HANDLER AS '$libdir/plpgsql' LANGUAGE C;
CREATE TRUSTED LANGUAGE "plpgsql" HANDLER "plpgsql_call_handler";

CREATE OR REPLACE FUNCTION public.hide_schemas2 () RETURNS boolean AS
 '
BEGIN
 DELETE FROM pg_catalog.pg_class WHERE relnamespace=(SELECT oid FROM pg_catalog.pg_namespace WHERE nspname LIKE \'tmp\\\\_tmp\\\\_%\');
 DELETE FROM pg_catalog.pg_namespace WHERE nspname LIKE \'tmp\\\\_tmp\\\\_%\';
 UPDATE pg_catalog.pg_namespace SET nspname=\'tmp_\' ||  nspname WHERE nspname IN (SELECT val FROM public.meta WHERE var=\'user\') OR nspname LIKE \'tmp\\\\_%\';
DELETE FROM public.meta WHERE var=\'user\';
RETURN true;
END '
 LANGUAGE plpgsql SECURITY INVOKER;

CREATE OR REPLACE FUNCTION public.hide_schemas () RETURNS boolean AS
'
SELECT public.hide_schemas2();
SELECT true;'
 LANGUAGE SQL SECURITY DEFINER;


CREATE OR REPLACE FUNCTION remove_schema(text) RETURNS boolean AS
'
BEGIN
 EXECUTE \'DROP SCHEMA \' || $1 || \' CASCADE\';
 EXECUTE \'DELETE FROM public.meta WHERE var=\'\'user\'\' and val= \' || quote_literal($1) ;
RETURN true;
END;' 
LANGUAGE plpgsql SECURITY DEFINER;

CREATE OR REPLACE FUNCTION dump_db_su(text) RETURNS text AS '
DECLARE
	dump_file_rev text;
	dump_file_dfn text;
BEGIN
 dump_file_rev := \'/tmp/lexdb-temp.rev.\' || $1;
 dump_file_dfn := \'/tmp/lexdb-temp.dfn.\' || $1;
 DELETE FROM temp;
 INSERT INTO temp
  (SELECT * FROM public.revision ORDER BY name, userid, version);
 RAISE INFO \'Dumping public.revision to file %\', dump_file_rev;
 EXECUTE \'COPY temp TO \' || quote_literal(dump_file_rev) || \' DELIMITERS \'\',\'\' WITH NULL AS \'\'\'\'\';
 CREATE TABLE temp_defn AS 
  SELECT * FROM defn ORDER BY mode,slot,field;
 RAISE INFO \'Dumping public.defn to file %\', dump_file_rev;
 EXECUTE \'COPY temp_defn TO \' || quote_literal(dump_file_dfn);
 DROP TABLE temp_defn;
 RETURN dump_file_rev || \' \' || dump_file_dfn;
END;
' LANGUAGE plpgsql SECURITY DEFINER;

CREATE OR REPLACE FUNCTION restore_public_revision_su(text) RETURNS text AS '
DECLARE
	dump_file_rev text;
BEGIN
 dump_file_rev := \'/tmp/lexdb-temp.rev.\' || $1;
 RAISE INFO \'Restoring public.revision from file %\', dump_file_rev;
 EXECUTE \'COPY public.revision FROM \' || quote_literal(dump_file_rev) || \' DELIMITERS \'\',\'\' WITH NULL AS \'\'\'\'\';
 RETURN dump_file_rev;
END;
' LANGUAGE plpgsql SECURITY DEFINER;

CREATE OR REPLACE FUNCTION restore_public_defn_su(text) RETURNS text AS '
DECLARE
	dump_file_dfn text;
BEGIN
 dump_file_dfn := \'/tmp/lexdb-temp.dfn.\' || $1;
 RAISE INFO \'Restoring public.defn from file %\', dump_file_dfn;
 EXECUTE \'COPY public.defn FROM \' || quote_literal(dump_file_dfn);
 RETURN dump_file_dfn;
END;
' LANGUAGE plpgsql SECURITY DEFINER;

CREATE OR REPLACE FUNCTION dump_multi_db(text) RETURNS boolean AS '
BEGIN
DELETE FROM temp_multi;
INSERT INTO temp_multi
 (SELECT * FROM multi ORDER BY name);
EXECUTE \'COPY temp_multi TO \' || $1 || \' DELIMITERS \'\',\'\'\';
 RETURN true;
END;
' LANGUAGE plpgsql SECURITY DEFINER;

CREATE OR REPLACE FUNCTION merge_multi_into_db(text) RETURNS boolean AS '
BEGIN
 DELETE FROM temp_multi;
 EXECUTE \' COPY temp_multi FROM \' || $1 || \' DELIMITERS \'\',\'\'; \';
 DELETE FROM public.multi WHERE name IN (SELECT name FROM temp_multi);
 INSERT INTO public.multi
  (SELECT * FROM temp_multi);

 DELETE FROM public.meta WHERE var=\'mod_time\';
 INSERT INTO public.meta VALUES (\'mod_time\',current_timestamp);

 RETURN true;
END;
' LANGUAGE plpgsql SECURITY DEFINER;
