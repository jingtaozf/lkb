CREATE FUNCTION "plpgsql_call_handler" () RETURNS LANGUAGE_HANDLER AS '$libdir/plpgsql' LANGUAGE C;
CREATE TRUSTED LANGUAGE "plpgsql" HANDLER "plpgsql_call_handler";

CREATE OR REPLACE FUNCTION public.hide_schemas2 () RETURNS boolean AS
 '
BEGIN
 DELETE FROM pg_catalog.pg_class WHERE relnamespace=(SELECT oid FROM pg_catalog.pg_namespace WHERE nspname LIKE ''tmp\\\\_tmp\\\\_%'');
 DELETE FROM pg_catalog.pg_namespace WHERE nspname LIKE ''tmp\\\\_tmp\\\\_%'';
 UPDATE pg_catalog.pg_namespace SET nspname=''tmp_'' ||  nspname WHERE nspname IN (SELECT val FROM public.meta WHERE var=''user'') OR nspname LIKE ''tmp\\\\_%'';
DELETE FROM public.meta WHERE var=''user'';
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
 EXECUTE ''DROP SCHEMA '' || $1 || '' CASCADE'';
 EXECUTE ''DELETE FROM public.meta WHERE var=''''user'''' and val= '' || quote_literal($1) ;
RETURN true;
END;' 
LANGUAGE plpgsql SECURITY DEFINER;

CREATE OR REPLACE FUNCTION dump_db(text,text) RETURNS boolean AS '
BEGIN
 DELETE FROM temp;
 INSERT INTO temp
  (SELECT * FROM public.revision ORDER BY name, userid, version);
 EXECUTE ''COPY temp TO '' || $1 || '' DELIMITERS '''','''' WITH NULL AS '''''''''';
 CREATE TABLE temp_defn AS 
  SELECT * FROM defn ORDER BY mode,slot,field;
 EXECUTE ''COPY temp_defn TO '' || $2;
 DROP TABLE temp_defn;
 RETURN true;
END;
' LANGUAGE plpgsql SECURITY DEFINER;

CREATE OR REPLACE FUNCTION dump_multi_db(text) RETURNS boolean AS '
BEGIN
DELETE FROM temp_multi;
INSERT INTO temp_multi
 (SELECT * FROM multi ORDER BY name);
EXECUTE ''COPY temp_multi TO '' || $1 || '' DELIMITERS '''','''''';
 RETURN true;
END;
' LANGUAGE plpgsql SECURITY DEFINER;

CREATE OR REPLACE FUNCTION merge_multi_into_db(text) RETURNS boolean AS '
BEGIN
 DELETE FROM temp_multi;
 EXECUTE '' COPY temp_multi FROM '' || $1 || '' DELIMITERS '''',''''; '';
 DELETE FROM public.multi WHERE name IN (SELECT name FROM temp_multi);
 INSERT INTO public.multi
  (SELECT * FROM temp_multi);

 DELETE FROM public.meta WHERE var=''mod_time'';
 INSERT INTO public.meta VALUES (''mod_time'',current_timestamp);

 RETURN true;
END;
' LANGUAGE plpgsql SECURITY DEFINER;

