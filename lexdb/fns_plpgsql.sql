CREATE OR REPLACE FUNCTION public.db_owner() RETURNS text AS 
'
DECLARE
	uid int;
	uname text;
BEGIN
 uid := (select datdba from pg_catalog.pg_database where datname=current_database());
 uname = (select usename from pg_catalog.pg_user where usesysid=uid);
 RETURN
  uname;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.assert_db_owner() RETURNS boolean AS 
'
DECLARE
	uid int;
	db_uid int;
BEGIN
 uid := (select usesysid from pg_catalog.pg_user where usename=user);
 db_uid= (select datdba from pg_catalog.pg_database where datname=current_database());
 IF uid != db_uid THEN
   RAISE EXCEPTION ''You are not the DB owner.'';
 END IF;
 RETURN true;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.merge_all() RETURNS integer AS 
'
BEGIN
 PERFORM assert_db_owner();	
---- n o t e : must copy file to temp before invoking this code
----           eg. COPY TO stdin from frontend
---- n o t e : must copy file to temp_defn before invoking this code
----           eg. COPY TO stdin from frontend
 PERFORM merge_into_db2();
 PERFORM merge_dfn();
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.merge_into_db2() RETURNS integer AS 
'
DECLARE
	num_merged int;
BEGIN
 PERFORM assert_db_owner();	
---- n o t e : must copy file to temp before invoking this code
----           eg. COPY TO stdin from frontend

 RAISE INFO ''Preparing tables to receive data...'';

 DROP INDEX public_orthkey;
 DROP INDEX name_modstamp;
 DROP INDEX public_revision_name_modstamp;
 DROP INDEX public_revision_name;
 DROP INDEX public_revision_name_pattern;
 ALTER TABLE public.revision DROP CONSTRAINT revision_pkey;

 DELETE FROM revision_new;
 
 CREATE INDEX temp_name_userid_version on temp (name, userid, version);
 
 RAISE INFO ''Copying data...'';

 INSERT INTO revision_new
  SELECT * FROM (SELECT DISTINCT name,userid,version FROM temp EXCEPT SELECT name,userid,version FROM public.revision) AS t1 NATURAL JOIN temp;

 num_merged := (SELECT count(*) FROM revision_new);

 RAISE INFO ''% new entries merged'', num_merged;

 DROP INDEX temp_name_userid_version;
 DELETE FROM temp;
 INSERT INTO public.revision SELECT * FROM revision_new;
 
 RAISE INFO ''Recreating table relations...'';

 ALTER TABLE public.revision ADD PRIMARY KEY (name,version,userid);
 CREATE INDEX public_orthkey ON public.revision (orthkey); 
 CREATE UNIQUE INDEX name_modstamp ON public.revision (name,modstamp); 
 CREATE INDEX public_revision_name_modstamp ON public.revision (name, modstamp);
 CREATE INDEX public_revision_name
  ON public.revision (name varchar_ops); 
 PERFORM 
  if_version(''7.4'',
   ''CREATE INDEX public_revision_name_pattern ON public.revision (name varchar_pattern_ops)'',
   ''CREATE INDEX public_revision_name_pattern ON public.revision (name)'');

 RAISE INFO ''Updating timestamps...'';

 DELETE FROM public.meta WHERE var=''mod_time'';
 INSERT INTO public.meta VALUES (''mod_time'',current_timestamp);

 RETURN num_merged;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.merge_dfn() RETURNS integer AS 
'
DECLARE
	num_new int;
BEGIN
 PERFORM assert_db_owner();	
---- n o t e : must copy file to temp_defn before invoking this code
----           eg. COPY TO stdin from frontend

 num_new := (SELECT count(*) FROM 
             (SELECT mode, slot, field FROM temp_defn EXCEPT
               SELECT mode, slot, field FROM defn) AS t1
             NATURAL JOIN temp_defn);

 RAISE INFO ''% new field mappings'', num_new;
 RAISE INFO ''Copying data...'';

 DELETE FROM defn;
 INSERT INTO defn
  SELECT * FROM temp_defn; 

 RAISE INFO ''Updating timestamps...'';

 DELETE FROM public.meta WHERE var=''mod_time'';
 INSERT INTO public.meta VALUES (''mod_time'',current_timestamp);

 RETURN num_new;
END;
' LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION public.complete(text,text) RETURNS SETOF text AS '
DECLARE
	x RECORD;
	sql_str text;
BEGIN
	sql_str := ''SELECT DISTINCT '' || $1 || '' AS field FROM current_grammar WHERE '' || $1 || '' ILIKE '' || $2 || '' || ''''%'''' '';
	FOR x IN EXECUTE sql_str LOOP 
     	  RETURN NEXT x.field;
 	END LOOP;
	RETURN;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.lookup_general(text,text) RETURNS SETOF text AS '
DECLARE
	x RECORD;
	sql_str text;
BEGIN
	sql_str := ''SELECT name FROM current_grammar WHERE '' || $1 || '' ILIKE '' || $2;
	FOR x IN EXECUTE sql_str LOOP 
     	  RETURN NEXT x.name;
 	END LOOP;	
	RETURN;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.lookup_general_null(text) RETURNS SETOF text AS '
DECLARE
	x RECORD;
	sql_str text;
BEGIN
	sql_str := ''SELECT name FROM current_grammar WHERE '' || $1 || '' IS NULL '';
	FOR x IN EXECUTE sql_str LOOP 
     	  RETURN NEXT x.name;
 	END LOOP;	
	RETURN;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.initialize_current_grammar(text) RETURNS boolean AS '
BEGIN
 EXECUTE ''
  CREATE OR REPLACE VIEW filtered
   AS SELECT * 
    FROM revision_all
    WHERE '' || $1 ;
 EXECUTE ''UPDATE meta SET val= '' || quote_literal($1) || '' WHERE var=''''filter'''''';
 IF mod_time() > build_time() THEN
   EXECUTE ''SELECT build_current_grammar()'';
 END IF;
 RETURN true;
END;
' LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION public.create_schema(text) RETURNS boolean AS '
BEGIN
  EXECUTE ''CREATE SCHEMA '' || $1;
  EXECUTE ''INSERT INTO public.meta VALUES (''''user'''', '' || quote_literal($1) || '')'';
 CREATE TABLE meta AS SELECT * FROM public.meta WHERE var=''filter'';
-- scratch
 CREATE TABLE revision AS SELECT * FROM public.revision WHERE NULL;
 EXECUTE ''CREATE UNIQUE INDEX user_'' || user || ''_name_revision_userid
  ON revision (name,version,userid)''; 
-- current_grammar
 CREATE TABLE current_grammar AS SELECT * FROM public.revision WHERE NULL;
 CREATE UNIQUE INDEX current_grammar_name ON current_grammar (name varchar_ops);
 CREATE INDEX current_grammar_orthkey ON current_grammar (orthkey varchar_ops); 

 IF check_version(''7.4'') THEN
	CREATE UNIQUE INDEX current_grammar_name_pattern ON current_grammar (name varchar_pattern_ops);
	CREATE INDEX current_grammar_orthkey_pattern ON current_grammar (orthkey varchar_pattern_ops);
 ELSE
	CREATE UNIQUE INDEX current_grammar_name_pattern ON current_grammar (name);
	CREATE INDEX current_grammar_orthkey_pattern ON current_grammar (orthkey);
 END IF; 

-- views
 CREATE VIEW filtered AS SELECT * FROM public.revision WHERE NULL;
 CREATE VIEW active
  AS SELECT fil.*
  FROM 
   (filtered AS fil
   NATURAL JOIN 
    (SELECT name, max(modstamp) AS modstamp 
      FROM filtered
      GROUP BY name) AS t1)
  WHERE flags=1;
 CREATE VIEW revision_all
  AS SELECT * FROM public.revision 
   UNION 
   SELECT * FROM revision;

-- mod time
DELETE FROM meta WHERE var=''mod_time'';
INSERT INTO meta VALUES (''mod_time'',current_timestamp);

-- semi

CREATE TABLE semi_pred (
 lex_id text NOT NULL,
 pred_id text NOT NULL,
 frame_id int NOT NULL,
 pred_txt text NOT NULL,
 string_p boolean NOT NULL,
 modstamp TIMESTAMP WITH TIME ZONE
);

CREATE TABLE semi_frame (
 frame_id int NOT NULL,
 slot text NOT NULL,
 str text,
 symb text,
 var_id int,
 type text
);

CREATE TABLE semi_var (
 var_id int NOT NULL,
 extra_id int NOT NULL
);

CREATE TABLE semi_extra (
 extra_id int NOT NULL,
 feat text NOT NULL,
 val text NOT NULL
);

-- semi mod time
DELETE FROM meta WHERE var=''semi_build_time'';

RETURN true;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION dump_db() RETURNS text AS '
BEGIN
 RETURN dump_db_su(user);
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION semi_setup_1() RETURNS boolean AS '
BEGIN
DROP TABLE semi_pred CASCADE;
DROP TABLE semi_frame CASCADE;
DROP TABLE semi_var CASCADE;
DROP TABLE semi_extra CASCADE;

CREATE TABLE semi_pred (
 lex_id text NOT NULL,
 pred_id text NOT NULL,
 frame_id int NOT NULL,
 pred_txt text NOT NULL,
 string_p boolean NOT NULL,
 modstamp TIMESTAMP WITH TIME ZONE
);

CREATE TABLE semi_frame (
 frame_id int NOT NULL,
 slot text NOT NULL,
 str text,
 symb text,
 var_id int,
 type text
);

CREATE TABLE semi_var (
 var_id int NOT NULL,
 extra_id int NOT NULL
);

CREATE TABLE semi_extra (
 extra_id int NOT NULL,
 feat text NOT NULL,
 val text NOT NULL
);
 RETURN true;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION semi_setup_2() RETURNS text AS '
BEGIN
-- build time
DELETE FROM meta WHERE var=''semi_build_time'';
INSERT INTO meta VALUES (''semi_build_time'',current_timestamp);

CREATE INDEX semi_pred_lex_id ON semi_pred (lex_id);
CREATE INDEX semi_pred_pred_id ON semi_pred (pred_id);
CREATE INDEX semi_frame_frame_id ON semi_frame (frame_id);
CREATE INDEX semi_frame_var_id ON semi_frame (var_id);
CREATE INDEX semi_var_var_id ON semi_var (var_id);
CREATE INDEX semi_extra_extra_id ON semi_extra (extra_id);

---
-- merge join is fastest
---
SET ENABLE_HASHJOIN TO false;

CREATE OR REPLACE VIEW semi_obj AS
 SELECT * FROM
  semi_pred NATURAL JOIN
  semi_frame NATURAL LEFT JOIN
  semi_var NATURAL LEFT JOIN
  semi_extra;
 RETURN true;
END;
' LANGUAGE plpgsql;

