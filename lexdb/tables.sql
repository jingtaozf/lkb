--- Copyright (c) 2003-2004 
--- Fabre Lambeau, Stephan Oepen, Benjamin Waldron;
--- see `licence.txt' for conditions.

--
-- table creation functions
--

CREATE OR REPLACE FUNCTION public.create_public_backup_table() RETURNS boolean AS '
BEGIN
	IF (reln_exists(\'public\',\'backup\')) THEN
		DROP TABLE public.backup CASCADE;
	END IF;
	CREATE TABLE backup (b text);
	RETURN true;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.create_public_meta_table() RETURNS boolean AS '
BEGIN
	IF (reln_exists(\'public\',\'meta\')) THEN
		PERFORM public.hide_schemas();
		DROP TABLE public.meta CASCADE;
	END IF;
	CREATE TABLE public.meta (
		var text,
		val text);
	RETURN true;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.create_public_defn_table() RETURNS boolean AS '
DECLARE
	backup_file_base text;
BEGIN
	IF (reln_exists(\'public\',\'defn\')) THEN
		DROP TABLE public.defn CASCADE;
	END IF;	
	CREATE TABLE public.defn (
			mode TEXT,
			slot TEXT,
			field TEXT,
			path TEXT,
			type TEXT,
		PRIMARY KEY (mode,slot, field));

	--
	-- restore from backup
	--
	IF (reln_exists(\'public\',\'backup\')) THEN
		backup_file_base := (SELECT b FROM public.backup LIMIT 1);
		IF backup_file_base IS NOT NULL THEN
			PERFORM restore_public_defn_su(backup_file_base);
		END IF;
	END IF;
	RETURN true;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.create_public_fields_table() RETURNS boolean AS '
DECLARE
	backup_file_base text;
BEGIN
	IF (reln_exists(\'public\',\'fields\')) THEN
		DROP TABLE public.fields CASCADE;
	END IF;
	CREATE TABLE public.fields (defn text);

	IF (reln_exists(\'public\',\'backup\')) THEN
		backup_file_base := (SELECT b FROM public.backup LIMIT 1);
		IF backup_file_base IS NOT NULL THEN
			PERFORM restore_public_fields_su(backup_file_base);
		END IF;
	END IF;

	IF (reln_exists(\'public\',\'default_fields\')) THEN
		DROP TABLE public.default_fields CASCADE;
	END IF;
	CREATE TABLE public.default_fields (defn text);
	RETURN true;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.create_public_revision_table() RETURNS boolean AS '
DECLARE
	backup_file_base text;
	sql_qry text;
BEGIN
	IF (reln_exists(\'public\',\'revision\')) THEN
		DROP TABLE public.revision CASCADE;
	END IF;
	sql_qry := \'CREATE TABLE public.revision (
		name TEXT NOT NULL,
		userid TEXT DEFAULT user NOT NULL,
		modstamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
		orthkey TEXT NOT NULL,
		flags INTEGER DEFAULT 0 NOT NULL
		\' || field_defn_text() || \' ) \';

	RAISE DEBUG \'%\', sql_qry;
	EXECUTE sql_qry;

	IF (reln_exists(\'public\',\'backup\')) THEN
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
	IF (reln_exists(\'public\',\'temp\')) THEN
		DROP TABLE public.temp CASCADE;
	END IF;
	IF (reln_exists(\'public\',\'revision_new\')) THEN
		DROP TABLE public.revision_new CASCADE;
	END IF;
	CREATE TABLE public.temp AS SELECT * FROM public.revision WHERE NULL;
	CREATE TABLE public.revision_new AS SELECT * FROM public.revision WHERE NULL;
	RETURN true;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.index_public_revision() RETURNS boolean AS '
BEGIN
 	RAISE INFO \'Creating indices...\';
	ALTER TABLE public.revision ADD PRIMARY KEY (name,userid,modstamp);
	CREATE INDEX public_orthkey ON public.revision (orthkey); 
	CREATE UNIQUE INDEX name_modstamp ON public.revision (name,modstamp); 
	CREATE INDEX public_revision_name_modstamp ON public.revision (name, modstamp);
	CREATE INDEX public_revision_name
		ON public.revision (name varchar_ops); 
	PERFORM if_server_version(\'7.4\', \'CREATE INDEX public_revision_name_pattern ON public.revision (name varchar_pattern_ops)\', \'CREATE INDEX public_revision_name_pattern ON public.revision (name)\');
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

CREATE OR REPLACE FUNCTION public.index_current_grammar() RETURNS boolean AS '
BEGIN
	RAISE INFO \'indexing db cache\';
	CREATE UNIQUE INDEX current_grammar_name ON current_grammar (name varchar_ops);
 	CREATE INDEX current_grammar_orthkey ON current_grammar (orthkey varchar_ops); 

 	IF server_version(\'7.4\') THEN
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

----
---- clean up obsolete structures
----
--CREATE OR REPLACE FUNCTION public.clean_up() RETURNS boolean AS '
--BEGIN
--	IF (reln_exists(\'public\',\'qry\')) THEN
--		DROP TABLE public.qry CASCADE;
--	END IF;
--	IF (reln_exists(\'public\',\'qrya\')) THEN
--		DROP TABLE public.qrya CASCADE;
--	END IF;
--	RETURN true;
--END;
--' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION semi_create_indices() RETURNS boolean AS '
BEGIN
 	RAISE DEBUG \'Creating SEMI indices...\';
	CREATE INDEX semi_pred_lex_id ON semi_pred (lex_id);
	CREATE INDEX semi_pred_pred_id ON semi_pred (pred_id);
	CREATE INDEX semi_frame_frame_id ON semi_frame (frame_id);
	CREATE INDEX semi_frame_var_id ON semi_frame (var_id);
	CREATE INDEX semi_var_var_id ON semi_var (var_id);
	CREATE INDEX semi_extra_extra_id ON semi_extra (extra_id);
	CREATE INDEX semi_mod_name_userid_modstamp0 ON semi_mod (name,userid,modstamp0);
RETURN true;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION semi_drop_indices() RETURNS boolean AS '
BEGIN
 	RAISE DEBUG \'Dropping SEMI indices...\';
	DROP INDEX semi_pred_lex_id CASCADE;
	DROP INDEX semi_pred_pred_id CASCADE;
	DROP INDEX semi_frame_frame_id CASCADE;
	DROP INDEX semi_frame_var_id CASCADE;
	DROP INDEX semi_var_var_id CASCADE;
	DROP INDEX semi_extra_extra_id CASCADE;
	DROP INDEX semi_mod_name_userid_modstamp0 CASCADE;
RETURN true;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION create_tables_semi() RETURNS boolean AS '
BEGIN
 	RAISE INFO \'Creating SEMI structures...\';
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

	CREATE TABLE semi_mod (
		name text,
		userid text,
		modstamp0 TIMESTAMP WITH TIME ZONE,
		modstamp TIMESTAMP WITH TIME ZONE
	);

	PERFORM semi_create_indices();

	CREATE OR REPLACE VIEW semi_obj AS
		SELECT * FROM
		semi_pred NATURAL JOIN
		semi_frame NATURAL LEFT JOIN
		semi_var NATURAL LEFT JOIN
		semi_extra;

	CREATE OR REPLACE FUNCTION retrieve_semi_pred() RETURNS SETOF semi_pred AS \'
		SELECT * FROM semi_pred;
	\' LANGUAGE sql;

	CREATE OR REPLACE FUNCTION retrieve_semi_frame() RETURNS SETOF semi_frame AS \'
		SELECT * FROM semi_frame;
	\' LANGUAGE sql;

	CREATE OR REPLACE FUNCTION retrieve_semi_var() RETURNS SETOF semi_var AS \'
		SELECT * FROM semi_var;
	\' LANGUAGE sql;

	CREATE OR REPLACE FUNCTION retrieve_semi_extra() RETURNS SETOF semi_extra AS \'
		SELECT * FROM semi_extra;
	\' LANGUAGE sql;

	RETURN true;
END;
' LANGUAGE plpgsql;

--
--
--

CREATE OR REPLACE FUNCTION field_defn_text() RETURNS text AS '
DECLARE
	x RECORD;
	t text;
BEGIN

	IF (reln_exists(\'public\',\'fields\') 
			AND 
			(SELECT count(*) FROM public.fields)>0) THEN
		RAISE INFO \'Using field defns found in public.fields\';
		FOR x IN SELECT defn FROM fields LOOP
			t := COALESCE(t,\'\');
			t:= t || \',\n \' || x.defn;
		END LOOP;
	ELSE
		-- obsolete
		RAISE INFO \'Using field defns found in public.default_fields\';
		FOR x IN SELECT defn FROM default_fields LOOP
			t := COALESCE(t,\'\');
			t:= t || \',\n \' || x.defn;
		END LOOP;		
	END IF;
	RETURN t;
END;
' LANGUAGE plpgsql;

