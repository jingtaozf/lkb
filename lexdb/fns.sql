--- Copyright (c) 2003 - 2005
--- Benjamin Waldron;
--- see `licence.txt' for conditions.

---
--- GENERAL LEXDB FUNCTIONS
---

--
-- admin tasks
--

-- returns field names in public.rev
CREATE OR REPLACE FUNCTION public.list_fld() RETURNS SETOF text AS '
	SELECT t1 FROM return_field_info2(\'public\',\'rev\');
' LANGUAGE sql;

--
-- psql server version
--

CREATE OR REPLACE FUNCTION public.psql_server_version() RETURNS text AS '
	select split_part((select version()),\' \',2)
' LANGUAGE sql;

CREATE OR REPLACE FUNCTION public.supported_psql_server_versions() RETURNS SETOF text AS '
DECLARE
	x RECORD;
BEGIN
	FOR x IN
		SELECT val FROM public.meta WHERE var=\'supported-psql-server\'
		LOOP
		RETURN NEXT x.val;
	END LOOP;
	RETURN;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.check_psql_server_version() RETURNS SETOF text AS '
DECLARE
	supported text;
	actual text;
	release text;
	major text;
	release_major text;
	message text;
	x RECORD;
BEGIN
	SELECT * INTO supported FROM public.supported_psql_server_versions();
	actual := psql_server_version();
	release := split_part(actual,\'.\',1);
	major := split_part(actual,\'.\',2);
	release_major := release || \'.\' || major;

	IF (SELECT count(*) FROM public.supported_psql_server_versions() WHERE supported_psql_server_versions=release_major)=0 THEN
		message := \'Unsupported PSQL Server version (\' || actual || \')\';
		RAISE WARNING \'%\', message;
		RETURN NEXT message;

		message := \'Supported PSQL Server versions: \';
		FOR x IN SELECT * FROM supported_psql_server_versions() LOOP
			message := message || x.supported_psql_server_versions || \'.x \';
		END LOOP;

		RAISE WARNING \'%\', message;
		RETURN NEXT message;
		RETURN;
	ELSE
		RETURN;
	END IF;
END;
' LANGUAGE plpgsql;

--
-- user schema setup
--

CREATE OR REPLACE FUNCTION public.user_schema_init_p() RETURNS boolean AS '
BEGIN
	RETURN (SELECT (user IN (SELECT val FROM public.meta WHERE var=\'user\')));
END;' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.register_user_schema(text) RETURNS boolean AS '
DECLARE
	user_str text;
BEGIN
	user_str := $1;
	EXECUTE \'INSERT INTO public.meta VALUES (\'\'user\'\', \' || quote_literal(user_str) || \')\';
	RETURN true;
END;
' LANGUAGE plpgsql SECURITY DEFINER;;

CREATE OR REPLACE FUNCTION public.initialize_user_schema() RETURNS boolean AS '
BEGIN
	IF user_schema_init_p() THEN
		RETURN false;
	END IF;

	-- db owner has no private schema
	IF user_is_db_owner_p() THEN
		RETURN false;
	END IF;

	EXECUTE \'CREATE SCHEMA \' || user;	
	EXECUTE \'GRANT USAGE ON SCHEMA \' || user || \' TO lexdb\';	
	PERFORM public.register_user_schema(user);
	
	CREATE TABLE meta AS SELECT * FROM public.meta WHERE var=\'filter\';
	INSERT INTO meta VALUES (\'mod_time\',\'\');
	INSERT INTO meta VALUES (\'build_time\',\'\');
 	
	CREATE TABLE tmp AS SELECT * FROM public.rev WHERE NULL;
 
 	CREATE TABLE rev AS SELECT * FROM public.rev WHERE NULL;
	PERFORM index_rev(); 
	GRANT SELECT ON rev TO lexdb;

 	CREATE TABLE lex AS SELECT * FROM public.rev WHERE NULL;
 	PERFORM index_lex();
	CREATE TABLE lex_key (
		name TEXT NOT NULL,
		userid TEXT DEFAULT user NOT NULL,
		modstamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
		key text NOT NULL
		);
 	PERFORM index_lex_key();

	-- views
	CREATE VIEW rev_all
		AS SELECT * FROM public.rev 
			UNION 
 			SELECT * FROM rev;

 	CREATE VIEW filt AS SELECT * FROM rev_all WHERE NULL;
	CREATE TABLE filt_tmp AS SELECT * FROM filt WHERE NULL;

 	CREATE VIEW head
		AS SELECT fil.*
 			FROM 
 			(filt AS fil
 			NATURAL JOIN 
 				(SELECT name, max(modstamp) AS modstamp 
 					FROM filt
					GROUP BY name) AS t1)
			WHERE dead=\'0\';

	-- register mod time
	PERFORM register_mod_time();

	-- semi setup
	PERFORM create_tables_semi();
	
	RETURN true;
END;
' LANGUAGE plpgsql;

--
--
--

CREATE OR REPLACE FUNCTION public.update_lex(text) RETURNS boolean AS '
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

	-- set (new) filter
 	EXECUTE \'
  		CREATE OR REPLACE VIEW filt
 		AS SELECT * 
  		FROM rev_all
    		WHERE \' || new_filter ;
	IF new_filter != current_filter THEN
 		EXECUTE \'UPDATE meta SET val= \' || quote_literal(new_filter) || \' WHERE var=\'\'filter\'\'\';
   		EXECUTE \'SELECT build_lex()\';
		RETURN true;
	ELSIF mod_time() > build_time() THEN
   		EXECUTE \'SELECT build_lex()\';
		RETURN true;
 	END IF;
 	RETURN false;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.build_lex () RETURNS boolean AS
'
DECLARE
	b_time text;
BEGIN
	-- we need a table in order to build and use indexes ...
	INSERT INTO filt_tmp SELECT * FROM filt;
	CREATE INDEX filt_tmp_name ON filt_tmp (name);
	CREATE INDEX filt_tmp_modstamp ON filt_tmp (modstamp);

	-- recreate lex
	DELETE FROM lex; 
	PERFORM public.deindex_lex();

	INSERT INTO lex 
		-- =head, but faster lookup
  		SELECT fil.*
  		FROM 
   		(filt_tmp AS fil
   		NATURAL JOIN 
    		(SELECT name, max(modstamp) AS modstamp 
      			FROM filt_tmp
      		GROUP BY name) AS t1)
  		WHERE dead=\'0\';
	PERFORM public.index_lex();

	DROP INDEX filt_tmp_name;
	DROP INDEX filt_tmp_modstamp;
	DELETE FROM filt_tmp;

	-- set build time 
	PERFORM register_build_time();

	RETURN true;
END;'
LANGUAGE plpgsql;

--
--
--

CREATE OR REPLACE FUNCTION public.register_build_time() RETURNS bool AS '
DECLARE
	mod_time text;
BEGIN
	UPDATE meta SET val=current_timestamp WHERE var=\'build_time\';
	RETURN true;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.register_mod_time() RETURNS text AS '
DECLARE
	mod_time text;
BEGIN
	UPDATE meta SET val=current_timestamp WHERE var=\'mod_time\';
	RETURN true;
END;
' LANGUAGE plpgsql;

--
--
--

CREATE OR REPLACE FUNCTION public.mod_time_private() RETURNS text AS '
BEGIN
	RETURN (SELECT val FROM meta WHERE var=\'mod_time\' LIMIT 1);
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.mod_time_public() RETURNS text AS '
BEGIN
	RETURN (SELECT val FROM public.meta WHERE var=\'mod_time\' LIMIT 1);
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.mod_time() RETURNS text AS '
BEGIN
	RETURN max(t) FROM (SELECT mod_time_private() AS t UNION SELECT mod_time_public() AS t) AS foo;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.build_time() RETURNS text AS '
BEGIN
	RETURN (SELECT val FROM meta WHERE var=\'build_time\' LIMIT 1);
END;
' LANGUAGE plpgsql;

--
--
--

CREATE OR REPLACE FUNCTION public.merge_public_rev_from_tmp() RETURNS integer AS 
'
DECLARE
	count_new int;
	num_dups int;
BEGIN
	PERFORM assert_db_owner();	

 	CREATE INDEX tmp_name_userid_modstamp on public.tmp (name, userid, modstamp);

	-- check for duplicates in public.tmp
	num_dups := (SELECT count(*) FROM (SELECT name,userid,modstamp, count(*) FROM public.tmp GROUP BY name,userid,modstamp HAVING count(*)>1) AS foo);
	IF (num_dups>=1) THEN
		RAISE EXCEPTION \'Entries to merge contain % duplicated instance(s) of <name,userid,modstamp>\', num_dups;
	END IF;

	DELETE FROM tmp WHERE (name,userid,modstamp) IN (SELECT name,userid,modstamp FROM rev);
	DROP INDEX tmp_name_userid_modstamp;

	count_new := (SELECT count(*) FROM tmp);
	IF count_new > 0 THEN
 		PERFORM public.deindex_public_rev();

		RAISE DEBUG \'Inserting new % entries into public.rev\', count_new;
		INSERT INTO public.rev SELECT * FROM tmp;

 		PERFORM public.index_public_rev();
		PERFORM register_mod_time();
	ELSE
		RAISE DEBUG \'0 new entries\';
	END IF;
 RETURN count_new;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.merge_dfn_from_tmp_dfn() RETURNS integer AS 
'
DECLARE
	num_new int;
BEGIN
 	PERFORM assert_db_owner();	

 	num_new := (SELECT count(*) FROM 
             		(SELECT * FROM public.tmp_dfn EXCEPT
               			SELECT * FROM dfn) AS t1
             			NATURAL JOIN public.tmp_dfn);

 	RAISE DEBUG \'% new dfn entries\', num_new;

	IF num_new > 0 THEN
 		RAISE DEBUG \'Updating table...\';
 		DELETE FROM dfn WHERE mode IN (SELECT DISTINCT mode FROM public.tmp_dfn);
		INSERT INTO dfn
  			SELECT * FROM public.tmp_dfn;
 	END IF;
 	RETURN num_new;
END;
' LANGUAGE plpgsql;

--
--
--

CREATE OR REPLACE FUNCTION public.dump_public_rev_to_tmp() RETURNS boolean AS '
BEGIN
	RAISE DEBUG \'creating ordered copy of public.rev in tmp\';
	DELETE FROM tmp;
	INSERT INTO tmp SELECT * FROM public.rev ORDER BY name, userid, modstamp;

	RETURN true;
END;
' LANGUAGE plpgsql;

--
--
--

CREATE OR REPLACE FUNCTION public.size_lex() RETURNS int AS '
BEGIN
	RETURN ( SELECT count(*) FROM lex );
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.lexdb_version() RETURNS text AS '
BEGIN
	RETURN ( SELECT val FROM public.meta WHERE var=\'lexdb-version\' LIMIT 1 );
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.filter() RETURNS text AS '
BEGIN
	RETURN ( SELECT val FROM meta WHERE var=\'filter\' LIMIT 1 );
END;
' LANGUAGE plpgsql;


--
--
--

-- read-only permissions should be done properly
CREATE OR REPLACE FUNCTION public.user_read_only_p(text) RETURNS boolean AS '
BEGIN
	RETURN (SELECT $1 IN (SELECT val FROM public.meta WHERE var=\'user-read-only\'));
END;
' LANGUAGE plpgsql;

--
-- 
--

-- this could be done more intelligently so that a full build_lex is not triggered
CREATE OR REPLACE FUNCTION public.clear_rev() RETURNS boolean AS '
BEGIN
	DELETE FROM rev;
	PERFORM register_mod_time();
	RETURN TRUE;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.commit_rev(text) RETURNS boolean AS '
BEGIN
	EXECUTE \'INSERT INTO public.rev (SELECT * FROM \' || $1 || \'.rev)\';
	PERFORM register_mod_time();
	RETURN true;
END;
' LANGUAGE plpgsql;

--
--
--

CREATE OR REPLACE FUNCTION public.complete(text,text) RETURNS SETOF text AS '
DECLARE
	x RECORD;
	sql_str text;
BEGIN
	sql_str := \'SELECT DISTINCT \' || quote_ident($1) || \' AS field FROM lex WHERE \' || quote_ident($1) || \' ILIKE \' || quote_literal($2) || \' || \'\'%\'\' \';
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
	sql_str := \'SELECT name FROM lex WHERE \' || quote_ident($1) || \' ILIKE \' || quote_literal($2);
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
	sql_str := \'SELECT name FROM lex WHERE \' || quote_ident($1) || \' IS NULL \';
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
	sql_str := \'SELECT DISTINCT \' || quote_ident($1) || \'::text AS foo FROM rev_all WHERE \' || quote_ident($1) || \' IS NOT NULL\';
	FOR x IN
		EXECUTE sql_str
		LOOP
		RETURN NEXT x.foo;
	END LOOP;
	RETURN;
END;
' LANGUAGE plpgsql;

-- Total runtime was: 4.259 ms
CREATE OR REPLACE FUNCTION public.update_entry(text,text,text) RETURNS boolean AS '
DECLARE
	sql_str text;
BEGIN
	DELETE FROM tmp;
	sql_str := \'INSERT INTO tmp ( name, \' || $2 || \' ) VALUES ( \' || quote_literal($1) || \', \' || $3 || \')\';
	EXECUTE sql_str;
	
	INSERT INTO rev SELECT * FROM tmp LIMIT 1;

	DELETE FROM lex WHERE name = $1 ;
	INSERT INTO lex SELECT * FROM head WHERE name = $1 LIMIT 1;
	DELETE FROM lex_key WHERE name = $1 ;
	-- lex_keys for name=$1 must be added immediately after calling this fn

	RETURN TRUE;
END;
' LANGUAGE plpgsql;

--
-- semi
--

-- -> drop_semi()
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

-- -> create_semi()
CREATE OR REPLACE FUNCTION semi_setup_post() RETURNS boolean AS '
BEGIN
	PERFORM semi_create_indices();

	INSERT INTO semi_mod (SELECT DISTINCT name,userid,lex.modstamp,CURRENT_TIMESTAMP FROM lex JOIN semi_pred ON name=lex_id);

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
	RETURN ( SELECT mod_time() < (SELECT min(modstamp0) FROM semi_mod ));
--	RETURN ( SELECT mod_time() < (SELECT min(modstamp) FROM semi_pred ));
END;
' LANGUAGE plpgsql;