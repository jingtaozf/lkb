--- Copyright (c) 2003-2004 
--- Benjamin Waldron;
--- see `licence.txt' for conditions.

CREATE OR REPLACE FUNCTION public.next_version(text) RETURNS integer AS '
BEGIN
	RETURN (SELECT COALESCE(1 + max(version),0) FROM revision_all WHERE name LIKE $1 AND user=user);
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.lex_id_set() RETURNS SETOF text AS '
DECLARE
	x RECORD;
BEGIN
	FOR x IN
		SELECT DISTINCT name FROM current_grammar
		LOOP
		RETURN NEXT x.name;
	END LOOP;
	RETURN;
END;
 ' LANGUAGE plpgsql;

-- obsolete
CREATE OR REPLACE FUNCTION public.lookup_word(text) RETURNS SETOF text AS '
DECLARE
	x RECORD;
BEGIN
	FOR x IN
		SELECT name FROM current_grammar WHERE orthkey LIKE $1
		LOOP
		RETURN NEXT x.name;
	END LOOP;
	RETURN;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.show_scratch() RETURNS SETOF text AS '
DECLARE
	x RECORD;
BEGIN
	FOR x IN
		SELECT distinct name FROM revision
		LOOP
		RETURN NEXT x.name;
	END LOOP;
	RETURN;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.get_filter() RETURNS text AS '
BEGIN
	RETURN (SELECT val FROM meta WHERE var=\'filter\' LIMIT 1);
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.test_user(text) RETURNS SETOF text AS '
DECLARE
	x RECORD;
BEGIN
	FOR x IN
		SELECT * FROM public.meta WHERE var=\'user\' AND val = $1
		LOOP
		RETURN NEXT x.val;
	END LOOP;
	RETURN;
END;
' LANGUAGE plpgsql;

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
   RAISE EXCEPTION \'You are not the DB owner.\';
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
 PERFORM merge_defn();
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.index_public_revision() RETURNS boolean AS '
BEGIN
 	RAISE INFO \'Creating indices...\';
	ALTER TABLE public.revision ADD PRIMARY KEY (name,version,userid);
	CREATE INDEX public_orthkey ON public.revision (orthkey); 
	CREATE UNIQUE INDEX name_modstamp ON public.revision (name,modstamp); 
	CREATE INDEX public_revision_name_modstamp ON public.revision (name, modstamp);
	CREATE INDEX public_revision_name
		ON public.revision (name varchar_ops); 
	PERFORM if_version(\'7.4\', \'CREATE INDEX public_revision_name_pattern ON public.revision (name varchar_pattern_ops)\', \'CREATE INDEX public_revision_name_pattern ON public.revision (name)\');
	RETURN true;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.deindex_public_revision() RETURNS boolean AS '
BEGIN
 	RAISE INFO \'Dropping indices...\';
 	DROP INDEX public_orthkey;
 	DROP INDEX name_modstamp;
	DROP INDEX public_revision_name_modstamp;
	DROP INDEX public_revision_name;
	DROP INDEX public_revision_name_pattern;
	ALTER TABLE public.revision DROP CONSTRAINT revision_pkey;
	RETURN true;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.merge_into_db2() RETURNS integer AS 
'
DECLARE
	count_new int;
BEGIN
	PERFORM assert_db_owner();	
	---- n o t e : must copy file to temp before invoking this code
	----           eg. COPY TO stdin from frontend

	RAISE INFO \'Selecting new entries to merge...\';
 	CREATE INDEX temp_name_userid_version on temp (name, userid, version);
 	DELETE FROM revision_new;
	INSERT INTO revision_new
		SELECT * FROM (SELECT DISTINCT name,userid,version FROM temp EXCEPT SELECT name,userid,version FROM public.revision) AS t1 NATURAL JOIN temp;
	DROP INDEX temp_name_userid_version;
	DELETE FROM temp;
	count_new := (SELECT count(*) FROM revision_new);

	IF count_new > 0 THEN
 		PERFORM public.deindex_public_revision();

		RAISE INFO \'Inserting new % entries...\', count_new;
		INSERT INTO public.revision SELECT * FROM revision_new;
 
 		PERFORM public.index_public_revision();

		PERFORM update_modstamp_pub();
	ELSE
		RAISE INFO \'0 new entries\';
	END IF;
 RETURN count_new;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.merge_defn() RETURNS integer AS 
'
DECLARE
	num_new int;
BEGIN
 	PERFORM assert_db_owner();	
	---- n o t e : must copy file to temp_defn before invoking this code
	----           eg. COPY TO stdin from frontend

 	num_new := (SELECT count(*) FROM 
             		(SELECT * FROM temp_defn EXCEPT
               			SELECT * FROM defn) AS t1
             			NATURAL JOIN temp_defn);

 	RAISE INFO \'% new field mappings\', num_new;

	IF num_new > 0 THEN
 		RAISE INFO \'Updating table...\';
 		DELETE FROM defn WHERE mode IN (SELECT DISTINCT mode FROM temp_defn);
		INSERT INTO defn
  			SELECT * FROM temp_defn; 
 		RAISE INFO \'Updating timestamps...\';
 		DELETE FROM public.meta WHERE var=\'mod_time\';
 		INSERT INTO public.meta VALUES (\'mod_time\',current_timestamp);
	END IF;
 	RETURN num_new;
END;
' LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION public.complete(text,text) RETURNS SETOF text AS '
DECLARE
	x RECORD;
	sql_str text;
BEGIN
	sql_str := \'SELECT DISTINCT \' || quote_ident($1) || \' AS field FROM current_grammar WHERE \' || quote_ident($1) || \' ILIKE \' || quote_literal($2) || \' || \'\'%\'\' \';
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
	sql_str := \'SELECT name FROM current_grammar WHERE \' || quote_ident($1) || \' ILIKE \' || quote_literal($2);
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
	sql_str := \'SELECT name FROM current_grammar WHERE \' || quote_ident($1) || \' IS NULL \';
	FOR x IN EXECUTE sql_str LOOP 
     	  RETURN NEXT x.name;
 	END LOOP;	
	RETURN;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.initialize_current_grammar(text) RETURNS boolean AS '
DECLARE
	current_filter TEXT;
	new_filter TEXT;
	m_time TEXT;
	b_time TEXT;
BEGIN
	current_filter := (SELECT val FROM meta WHERE var=\'filter\');
	new_filter := $1;
	IF new_filter=\'\' THEN
		new_filter := current_filter;
	ENd IF;
	m_time := (SELECT mod_time());	
	b_time := (SELECT build_time());	

	RAISE INFO \'current filter: %\', current_filter;
	RAISE INFO \'build time: %\', b_time;
	RAISE INFO \'mod time: %\', m_time;

	-- set (new) filter
 	EXECUTE \'
  		CREATE OR REPLACE VIEW filtered
 		AS SELECT * 
  		FROM revision_all
    		WHERE \' || $1 ;
	IF new_filter != current_filter THEN
 		EXECUTE \'UPDATE meta SET val= \' || quote_literal($1) || \' WHERE var=\'\'filter\'\'\';
		RAISE INFO \'new filter: %\', new_filter;
		RAISE INFO \'rebuilding db cache\';
   		EXECUTE \'SELECT build_current_grammar()\';
	ELSIF mod_time() > build_time() THEN
		RAISE INFO \'rebuilding db cache\';
   		EXECUTE \'SELECT build_current_grammar()\';
 	END IF;
 	RETURN true;
END;
' LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION public.index_current_grammar() RETURNS boolean AS '
BEGIN
	RAISE INFO \'indexing db cache\';
	CREATE UNIQUE INDEX current_grammar_name ON current_grammar (name varchar_ops);
 	CREATE INDEX current_grammar_orthkey ON current_grammar (orthkey varchar_ops); 

 	IF check_version(\'7.4\') THEN
		CREATE UNIQUE INDEX current_grammar_name_pattern ON current_grammar (name varchar_pattern_ops);
		CREATE INDEX current_grammar_orthkey_pattern ON current_grammar (orthkey varchar_pattern_ops);
 	ELSE
		CREATE UNIQUE INDEX current_grammar_name_pattern ON current_grammar (name);
		CREATE INDEX current_grammar_orthkey_pattern ON current_grammar (orthkey);
 	END IF; 
	RETURN true;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.deindex_current_grammar() RETURNS boolean AS '
BEGIN
	RAISE INFO \'deindexing db cache\';
	DROP INDEX current_grammar_name;
 	DROP INDEX current_grammar_orthkey; 

	DROP INDEX current_grammar_name_pattern;
	DROP INDEX current_grammar_orthkey_pattern;

	RETURN true;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.create_schema(text) RETURNS boolean AS '
BEGIN
	EXECUTE \'CREATE SCHEMA \' || $1;
	EXECUTE \'INSERT INTO public.meta VALUES (\'\'user\'\', \' || quote_literal($1) || \')\';
	CREATE TABLE meta AS SELECT * FROM public.meta WHERE var=\'filter\';
	
	-- scratch
 	CREATE TABLE revision AS SELECT * FROM public.revision WHERE NULL;
 	EXECUTE \'CREATE UNIQUE INDEX user_\' || user || \'_name_revision_userid ON revision (name,version,userid)\'; 

	-- current_grammar
 	CREATE TABLE current_grammar AS SELECT * FROM public.revision WHERE NULL;

 	PERFORM public.index_current_grammar();

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
	PERFORM update_modstamp_priv();

	-- semi

	CREATE TABLE semi_pred (
		lex_id text NOT NULL,
		pred_id text NOT NULL,
		frame_id int NOT NULL,
		pred_txt text NOT NULL,
		string_p boolean NOT NULL,
		modstamp TIMESTAMP WITH TIME ZONE);

	CREATE TABLE semi_frame (
		frame_id int NOT NULL,
		slot text NOT NULL,
		str text,
		symb text,
		var_id int,
		type text);

	CREATE TABLE semi_var (
		var_id int NOT NULL,
		extra_id int NOT NULL);

	CREATE TABLE semi_extra (
		extra_id int NOT NULL,
		feat text NOT NULL,
		val text NOT NULL);

CREATE OR REPLACE FUNCTION public.retrieve_semi_pred() RETURNS SETOF semi_pred AS \'
BEGIN
	RETURN ( SELECT * FROM semi_pred );
END;
\' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.retrieve_semi_frame() RETURNS SETOF semi_frame AS \'
BEGIN
	RETURN ( SELECT * FROM semi_frame );
END;
\' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.retrieve_semi_var() RETURNS SETOF semi_var AS \'
BEGIN
	RETURN ( SELECT * FROM semi_var );
END;
\' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.retrieve_semi_extra() RETURNS SETOF semi_extra AS \'
BEGIN
	RETURN ( SELECT * FROM semi_extra );
END;
\' LANGUAGE plpgsql;

	-- semi mod time
	DELETE FROM meta WHERE var=\'semi_build_time\';

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
DELETE FROM meta WHERE var=\'semi_build_time\';
INSERT INTO meta VALUES (\'semi_build_time\',current_timestamp);

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

CREATE OR REPLACE FUNCTION public.build_current_grammar () RETURNS boolean AS
'
DECLARE
	b_time text;
BEGIN
	-- we need "indices on the view" for reasons of efficicency...
	raise info \'creating filtered_temp\';
	CREATE TABLE filtered_temp AS SELECT * FROM filtered;
	raise info \'creating index filtered_temp_i1\';
	CREATE INDEX filtered_temp_i1 ON filtered_temp (name);
	raise info \'creating index filtered_temp_i2\';
	CREATE INDEX filtered_temp_i2 ON filtered_temp (modstamp);

	-- recreate db cache
	raise info \'emptying db cache\';
	DELETE FROM current_grammar; 
	PERFORM public.deindex_current_grammar();

	raise info \'populating db cache\';
	INSERT INTO current_grammar 
  		SELECT fil.*
  		FROM 
   		(filtered_temp AS fil
   		NATURAL JOIN 
    		(SELECT name, max(modstamp) AS modstamp 
      			FROM filtered_temp
      		GROUP BY name) AS t1)
  	WHERE flags=1;
	PERFORM public.index_current_grammar();

	DROP TABLE filtered_temp;

	-- set build time 
	DELETE FROM meta WHERE var=\'build_time\';
	INSERT INTO meta VALUES (\'build_time\',current_timestamp);
	b_time := (SELECT build_time());
	raise info \'build time: %\', b_time;

	RETURN true;
END;'
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.update_modstamp_priv() RETURNS text AS '
DECLARE
	mod_time text;
BEGIN
	RAISE INFO \'Updating timestamps...\';
	DELETE FROM meta WHERE var=\'mod_time\';
	mod_time := current_timestamp;
	INSERT INTO meta VALUES (\'mod_time\',mod_time);
	RETURN mod_time;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.update_modstamp_pub() RETURNS text AS '
DECLARE
	mod_time text;
BEGIN
	RAISE INFO \'Updating timestamps...\';
	DELETE FROM public.meta WHERE var=\'mod_time\';
	mod_time := current_timestamp;
	INSERT INTO public.meta VALUES (\'mod_time\',mod_time);
	RETURN mod_time;
END;
' LANGUAGE plpgsql SECURITY DEFINER;

CREATE OR REPLACE FUNCTION public.clear_scratch() RETURNS boolean AS '
BEGIN
	DELETE FROM revision;
	PERFORM update_modstamp_priv();
	RETURN TRUE;
END;
' LANGUAGE plpgsql;

--CREATE OR REPLACE FUNCTION public.update_meta_mod_time_public() RETURNS boolean AS '
--DELETE FROM public.meta WHERE var=\'mod_time\';
--INSERT INTO public.meta VALUES (\'mod_time\',current_timestamp);
--SELECT true;
--' LANGUAGE SQL SECURITY DEFINER;

CREATE OR REPLACE FUNCTION public.commit_scratch() RETURNS boolean AS '
BEGIN
	INSERT INTO public.revision (SELECT * FROM revision);
	PERFORM update_modstamp_pub();
	PERFORM clear_scratch();
	RETURN true;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.mod_time_private() RETURNS text AS '
BEGIN
	RETURN COALESCE((SELECT val FROM meta WHERE var=\'mod_time\' LIMIT 1),\'-infin\');
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.mod_time_public() RETURNS text AS '
BEGIN
	RETURN COALESCE((SELECT val FROM public.meta WHERE var=\'mod_time\' LIMIT 1),\'-infin\');
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.mod_time() RETURNS text AS '
BEGIN
	RETURN max(t) FROM (SELECT mod_time_private() AS t UNION SELECT mod_time_public() AS t) AS foo;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.build_time() RETURNS text AS '
BEGIN
	RETURN COALESCE((SELECT val FROM meta WHERE var=\'build_time\' LIMIT 1),\'\');
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.semi_build_time_private() RETURNS text AS '
BEGIN
	RETURN COALESCE((SELECT val FROM meta WHERE var=\'semi_build_time\' LIMIT 1),\'\');
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.define_type_text_text() RETURNS boolean AS '
BEGIN
	IF table_exists_p(\'public\',\'text_text\') THEN
		DROP TYPE public.text_text CASCADE;
	END IF;
	CREATE TYPE text_text AS (t1 text, t2 text);
	RETURN true;
END;
' LANGUAGE plpgsql;
SELECT public.define_type_text_text();

CREATE OR REPLACE FUNCTION public.mneum_f_map(text) RETURNS SETOF text_text AS '
DECLARE
	x text_text;
BEGIN
	FOR x IN
		SELECT slot,field from defn where mode = \'_\' || $1
		LOOP
		RETURN NEXT x;
	END LOOP;
	RETURN;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.semi_out_of_date() RETURNS SETOF text AS '
DECLARE
	x RECORD;
BEGIN
	FOR x IN
		SELECT name FROM current_grammar WHERE modstamp > semi_build_time_private()
		LOOP
		RETURN NEXT x.name;
	END LOOP;
	RETURN;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.semi_up_to_date_p() RETURNS boolean AS '
BEGIN
	RETURN ( SELECT mod_time() < (SELECT min(modstamp) FROM semi_pred ));
END;
' LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION public.value_set(text) RETURNS SETOF text AS '
DECLARE
	x RECORD;
	sql_str text;
BEGIN
	sql_str := \'SELECT DISTINCT \' || quote_ident($1) || \'::text AS foo FROM revision_all WHERE \' || quote_ident($1) || \' IS NOT NULL\';
	FOR x IN
		EXECUTE sql_str
		LOOP
		RETURN NEXT x.foo;
	END LOOP;
	RETURN;
END;
' LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION public.size_current_grammar() RETURNS int AS '
BEGIN
	RETURN ( SELECT count(*) FROM current_grammar );
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.update_entry(text,text,text) RETURNS boolean AS '
DECLARE
	sql_str text;
BEGIN
	sql_str := \'INSERT INTO revision ( name, \' || $2 || \' ) VALUES ( \' || quote_literal($1) || \', \' || $3 || \')\';
	RAISE INFO \'%\', sql_str;
	EXECUTE sql_str;
	DELETE FROM current_grammar
		WHERE name = $1 ;
	INSERT INTO current_grammar
		SELECT * FROM active WHERE name = $1 LIMIT 1;

	PERFORM update_modstamp_priv();
	RETURN TRUE;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.lexdb_version() RETURNS text AS '
BEGIN
	RETURN ( SELECT val FROM public.meta WHERE var=\'db-version\' LIMIT 1 );
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.filter() RETURNS text AS '
BEGIN
	RETURN ( SELECT val FROM meta WHERE var=\'filter\' LIMIT 1 );
END;
' LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION public.retrieve_current_timestamp() RETURNS text AS '
BEGIN
	RETURN current_timestamp;
END;
' LANGUAGE plpgsql;

---
--- define public.revision (needed for fn defns below)
---

CREATE OR REPLACE FUNCTION public.recreate_public_revision() RETURNS boolean AS '
DECLARE
	backup_file_base text;
BEGIN
	IF (table_exists_p(\'public\',\'revision\')) THEN
		backup_file_base := \'BACKUP-BEFORE-LEXDB-UPDATE\';
		PERFORM dump_db_su(backup_file_base);
		DELETE FROM public.backup;
		INSERT INTO public.backup VALUES (backup_file_base);
		DROP TABLE public.revision CASCADE;
	END IF;
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

	IF (table_exists_p(\'public\',\'backup\')) THEN
		backup_file_base := (SELECT b FROM public.backup LIMIT 1);
		IF backup_file_base IS NOT NULL THEN
			PERFORM restore_public_revision_su(backup_file_base);
		END IF;
	END IF;

	PERFORM public.index_public_revision();

	RETURN TRUE;
END;
' LANGUAGE plpgsql;

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

SELECT recreate_public_revision();
SELECT public.create_bc_temp_tables();

---
---
---

create table revision_all as select * from revision where null;
CREATE OR REPLACE FUNCTION public.revision_new() RETURNS SETOF revision AS '
	SELECT * FROM revision_new
' LANGUAGE sql;
drop table revision_all;

create table filtered as select * from revision where null;
CREATE OR REPLACE FUNCTION public.retrieve_head_entry(text) RETURNS revision AS '
	SELECT * FROM filtered WHERE name LIKE $1 AND modstamp=(SELECT max(modstamp) FROM filtered WHERE name LIKE $1) LIMIT 1
' LANGUAGE sql;
drop table filtered;

create table current_grammar as select * from revision where null;
CREATE OR REPLACE FUNCTION public.retrieve_all_entries() RETURNS SETOF revision AS '
	SELECT * FROM current_grammar
' LANGUAGE sql;
drop table current_grammar;

create table current_grammar as select * from revision where null;
CREATE OR REPLACE FUNCTION public.retrieve_entries_by_orthkey(text) RETURNS SETOF revision AS '
	SELECT * FROM current_grammar WHERE orthkey LIKE $1
' LANGUAGE sql;
drop table current_grammar;

create table current_grammar as select * from revision where null;
CREATE OR REPLACE FUNCTION public.retrieve_entry(text) RETURNS SETOF revision AS '
	SELECT * FROM current_grammar WHERE name LIKE $1
' LANGUAGE sql;
drop table current_grammar;

CREATE OR REPLACE FUNCTION public.user_read_only_p(text) RETURNS boolean AS '
BEGIN
	RETURN (SELECT $1 IN (SELECT val FROM public.meta WHERE var=\'user-read-only\'));
END;
' LANGUAGE plpgsql;

