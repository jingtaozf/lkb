--- Copyright (c) 2003-2005
--- Benjamin Waldron;
--- see `licence.txt' for conditions.

---
--- GENERAL LEXDB FUNCTIONS
---

--
-- admin tasks
--

CREATE OR REPLACE FUNCTION public.tmp_dir() RETURNS text AS '
BEGIN
 RETURN (SELECT val FROM public.meta WHERE var=\'tmp-dir\' LIMIT 1);
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.tmp_base(text) RETURNS text AS '
BEGIN
 RETURN tmp_dir() || \'/\' || $1 || \'-temp.\';
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.ext_fns() RETURNS SETOF text AS '
DECLARE
	x RECORD;
BEGIN
	FOR x IN
		SELECT val FROM public.meta WHERE var=\'ext-fn\'
		LOOP
		RETURN NEXT x.val;
	END LOOP;
	RETURN;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.server_version() RETURNS text AS '
	select split_part((select version()),\' \',2)
' LANGUAGE sql;

CREATE OR REPLACE FUNCTION public.supported_server_versions() RETURNS SETOF text AS '
DECLARE
	x RECORD;
BEGIN
	FOR x IN
		SELECT val FROM public.meta WHERE var=\'supported-server\'
		LOOP
		RETURN NEXT x.val;
	END LOOP;
	RETURN;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.check_server_version() RETURNS SETOF text AS '
DECLARE
	supported text;
	actual text;
	release text;
	major text;
	release_major text;
	message text;
	x RECORD;
BEGIN
	SELECT * INTO supported FROM public.supported_server_versions();
	actual := server_version();
	release := split_part(actual,\'.\',1);
	major := split_part(actual,\'.\',2);
	release_major := release || \'.\' || major;

	IF (SELECT count(*) FROM public.supported_server_versions() WHERE supported_server_versions=release_major)=0 THEN
		message := \'Unsupported PSQL Server version (\' || actual || \')\';
		RAISE WARNING \'%\', message;
		RETURN NEXT message;

		message := \'Supported PSQL Server versions: \';
		FOR x IN SELECT * FROM supported_server_versions() LOOP
			message := message || x.supported_server_versions || \'.x \';
		END LOOP;

		RAISE WARNING \'%\', message;
		RETURN NEXT message;
		RETURN;
	ELSE
		RETURN;
	END IF;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.dump_data() RETURNS boolean AS '
DECLARE
	backup_file_base text;
BEGIN
	IF NOT(reln_exists(\'public\',\'fields\')) THEN
		CREATE TABLE public.fields (defn text);
	END IF;
	IF (reln_exists(\'public\',\'revision\')) THEN
		backup_file_base := tmp_base(\'lexdb\') || \'BACKUP-BEFORE-LEXDB-UPDATE\';
		PERFORM dump_db_su(backup_file_base);
		DELETE FROM public.backup;
		INSERT INTO public.backup VALUES (backup_file_base);
		DROP TABLE public.revision CASCADE;
	END IF;
	RETURN true;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.user_schema_initialized() RETURNS boolean AS '
BEGIN
	RETURN (SELECT (user IN (SELECT val FROM public.meta WHERE var=\'user\')));
END;' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.initialize_user_schema() RETURNS boolean AS '
DECLARE
	user_str text;
BEGIN
	user_str := user;
	IF user_schema_initialized() THEN
		RETURN false;
	END IF;
	RAISE INFO \'Initializing user schema %\', user_str;

	EXECUTE \'CREATE SCHEMA \' || user;
	EXECUTE \'INSERT INTO public.meta VALUES (\'\'user\'\', \' || quote_literal(user) || \')\';
	CREATE TABLE meta AS SELECT * FROM public.meta WHERE var=\'filter\';
	
	-- temp
 	CREATE TABLE temp AS SELECT * FROM public.revision WHERE NULL;
 
	-- scratch
 	CREATE TABLE revision AS SELECT * FROM public.revision WHERE NULL;
 	EXECUTE \'CREATE UNIQUE INDEX user_\' || user || \'_name_revision_userid ON revision (name,userid,modstamp)\'; 

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
	PERFORM create_tables_semi();
	
	RETURN true;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.initialize_current_grammar(text) RETURNS boolean AS '
DECLARE
	current_filter TEXT;
	new_filter text;
	m_time TEXT;
	b_time TEXT;
BEGIN
	new_filter := $1;
	current_filter := (SELECT val FROM meta WHERE var=\'filter\' LIMIT 1);
	IF new_filter=\'\' THEN
		new_filter = current_filter;
	END IF;
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
    		WHERE \' || new_filter ;
	IF new_filter != current_filter THEN
 		EXECUTE \'UPDATE meta SET val= \' || quote_literal(new_filter) || \' WHERE var=\'\'filter\'\'\';
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

CREATE OR REPLACE FUNCTION public.build_current_grammar () RETURNS boolean AS
'
DECLARE
	b_time text;
BEGIN
	-- we need "indices on the view" for reasons of efficicency...
	RAISE DEBUG \'creating filtered_temp\';
	CREATE TABLE filtered_temp AS SELECT * FROM filtered;
	RAISE DEBUG \'creating index filtered_temp_i1\';
	CREATE INDEX filtered_temp_i1 ON filtered_temp (name);
	RAISE DEBUG \'creating index filtered_temp_i2\';
	CREATE INDEX filtered_temp_i2 ON filtered_temp (modstamp);

	-- recreate db cache
	RAISE INFO \'emptying db cache\';
	DELETE FROM current_grammar; 
	PERFORM public.deindex_current_grammar();

	RAISE INFO \'populating db cache\';
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
	RAISE DEBUG \'build time: %\', b_time;

	RETURN true;
END;'
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.update_modstamp_priv() RETURNS text AS '
DECLARE
	mod_time text;
BEGIN
	RAISE DEBUG \'Updating timestamps...\';
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
	RAISE DEBUG \'Updating timestamps...\';
	DELETE FROM public.meta WHERE var=\'mod_time\';
	mod_time := current_timestamp;
	INSERT INTO public.meta VALUES (\'mod_time\',mod_time);
	RETURN mod_time;
END;
' LANGUAGE plpgsql SECURITY DEFINER;

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

CREATE OR REPLACE FUNCTION public.merge_into_db2() RETURNS integer AS 
'
DECLARE
	count_new int;
	num_dups int;
BEGIN
	PERFORM assert_db_owner();	
	---- n o t e : must copy file to public.temp before invoking this code
	----           eg. COPY TO stdin from frontend

	RAISE INFO \'Selecting new entries to merge...\';
 	CREATE INDEX temp_name_userid_modstamp on public.temp (name, userid, modstamp);

	-- check for duplicates in public.temp
	num_dups := (SELECT count(*) FROM (SELECT name,userid,modstamp, count(*) FROM public.temp GROUP BY name,userid,modstamp HAVING count(*)>1) AS foo);
	IF (num_dups=1) THEN
		RAISE EXCEPTION \'Entries to merge contain % duplicated instance of <name,userid,modstamp>\', num_dups;
	ELSIF (num_dups>1) THEN
		RAISE EXCEPTION \'Entries to merge contain % duplicated instances of <name,userid,modstamp>\', num_dups;
	END IF;

 	DELETE FROM revision_new;
	INSERT INTO revision_new
		SELECT * FROM (SELECT DISTINCT name,userid,modstamp FROM public.temp EXCEPT SELECT name,userid,modstamp FROM public.revision) AS t1 NATURAL JOIN public.temp;
	DROP INDEX temp_name_userid_modstamp;
	DELETE FROM public.temp;
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
	---- n o t e : must copy file to public.temp_defn before invoking this code
	----           eg. COPY TO stdin from frontend

 	num_new := (SELECT count(*) FROM 
             		(SELECT * FROM public.temp_defn EXCEPT
               			SELECT * FROM defn) AS t1
             			NATURAL JOIN public.temp_defn);

 	RAISE INFO \'% new field mappings\', num_new;

	IF num_new > 0 THEN
 		RAISE INFO \'Updating table...\';
 		DELETE FROM defn WHERE mode IN (SELECT DISTINCT mode FROM public.temp_defn);
		INSERT INTO defn
  			SELECT * FROM public.temp_defn; 
 		RAISE DEBUG \'Updating timestamps...\';
 		DELETE FROM public.meta WHERE var=\'mod_time\';
 		INSERT INTO public.meta VALUES (\'mod_time\',current_timestamp);
		--DELETE FROM meta WHERE var=\'semi_build_time\';
	END IF;
 	RETURN num_new;
END;
' LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION public.dump_rev() RETURNS boolean AS '
DECLARE
	dump_file_rev text;
	lexdb_versn real;
	base text;
BEGIN
	RAISE INFO \'creating ordered copy of public.revision in temp\';
	DELETE FROM temp;
	INSERT INTO temp SELECT * FROM public.revision ORDER BY name, userid, modstamp;
	--RAISE INFO \'dumping temp to database stdout\';
	--COPY temp TO stdout;

	RETURN true;
END;
' LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION dump_db_dfn_fld() RETURNS text AS '
BEGIN
 RETURN dump_db_dfn_fld_su2(user);
END;
' LANGUAGE plpgsql;

-- obsolete
--CREATE OR REPLACE FUNCTION public.next_version(text) RETURNS integer AS '
--DECLARE
--	i integer;
--BEGIN
--	i := (SELECT max(version) FROM revision_all);
--	RETURN i;
--END;
--' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.size_current_grammar() RETURNS int AS '
BEGIN
	RETURN ( SELECT count(*) FROM current_grammar );
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


CREATE OR REPLACE FUNCTION public.user_read_only_p(text) RETURNS boolean AS '
BEGIN
	RETURN (SELECT $1 IN (SELECT val FROM public.meta WHERE var=\'user-read-only\'));
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

--
-- 
--

CREATE OR REPLACE FUNCTION public.clear_scratch() RETURNS boolean AS '
BEGIN
	DELETE FROM revision;
	PERFORM update_modstamp_priv();
	RETURN TRUE;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.commit_scratch() RETURNS boolean AS '
BEGIN
	INSERT INTO public.revision (SELECT * FROM revision);
	PERFORM update_modstamp_pub();
	PERFORM clear_scratch();
	RETURN true;
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

-- orthkey must enter db universe in normalized form (case)
-- Total runtime: 4.259 ms
CREATE OR REPLACE FUNCTION public.update_entry(text,text,text) RETURNS boolean AS '
DECLARE
	sql_str text;
BEGIN
	sql_str := \'INSERT INTO revision ( name, \' || $2 || \' ) VALUES ( \' || quote_literal($1) || \', \' || $3 || \')\';
	RAISE DEBUG \'%\', sql_str;
	EXECUTE sql_str;

	--sql_str := \'UPDATE revision SET orthkey=lower(orthkey) WHERE ( name, \' || $2 || \' ) = ( \' || quote_literal($1) || \', \' || $3 || \')\'; 
	--RAISE DEBUG \'%\', sql_str;
	--EXECUTE sql_str;

	DELETE FROM current_grammar
		WHERE name = $1 ;
	INSERT INTO current_grammar
		SELECT * FROM active WHERE name = $1 LIMIT 1;

	PERFORM update_modstamp_priv();
	RETURN TRUE;
END;
' LANGUAGE plpgsql;

--
-- semi
--

CREATE OR REPLACE FUNCTION semi_setup_pre() RETURNS boolean AS '
BEGIN
	PERFORM semi_drop_indices();

	DELETE FROM semi_pred;
	DELETE FROM semi_frame;
	DELETE FROM semi_var;
	DELETE FROM semi_extra;
	DELETE FROM semi_mod;
RETURN true;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION semi_setup_post() RETURNS boolean AS '
BEGIN
	PERFORM semi_create_indices();

	INSERT INTO semi_mod (SELECT DISTINCT name,userid,current_grammar.modstamp,CURRENT_TIMESTAMP FROM current_grammar JOIN semi_pred ON name=lex_id);

	-- coz merge join is faster
	SET ENABLE_HASHJOIN TO false;

RETURN true;
END;

' LANGUAGE plpgsql;

-- fix me?
CREATE OR REPLACE FUNCTION public.semi_mod_time_private(text,text,int) RETURNS text AS '
BEGIN
	RETURN (SELECT modstamp FROM semi_mod WHERE (name,userid,modstamp)=($1,$2,$3));
END;
' LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION public.semi_up_to_date_p() RETURNS boolean AS '
BEGIN
	RETURN ( SELECT mod_time() < (SELECT min(modstamp) FROM semi_pred ));
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.list_fields() RETURNS SETOF text AS '
	SELECT t1 FROM return_field_info(\'public\',\'revision\');
' LANGUAGE sql;

