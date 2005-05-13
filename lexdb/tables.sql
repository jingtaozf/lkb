--- Copyright (c) 2003 - 2005
--- Benjamin Waldron, Fabre Lambeau, Stephan Oepen;
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

CREATE OR REPLACE FUNCTION public.create_public_dfn_table() RETURNS boolean AS '
DECLARE
	backup_file_base text;
BEGIN
	IF (reln_exists(\'public\',\'dfn\')) THEN
		DROP TABLE public.dfn CASCADE;
	END IF;	
	CREATE TABLE public.dfn (
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
			PERFORM restore_public_dfn_su(backup_file_base);
		END IF;
	END IF;
	RETURN true;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.create_public_fld_table() RETURNS boolean AS '
DECLARE
	backup_file_base text;
BEGIN
	IF (reln_exists(\'public\',\'fld\')) THEN
		DROP TABLE public.fld CASCADE;
	END IF;
	CREATE TABLE public.fld (dfn text);

	IF (reln_exists(\'public\',\'backup\')) THEN
		backup_file_base := (SELECT b FROM public.backup LIMIT 1);
		IF backup_file_base IS NOT NULL THEN
			PERFORM restore_public_fld_su(backup_file_base);
		END IF;
	END IF;

	IF (reln_exists(\'public\',\'default_fld\')) THEN
		DROP TABLE public.default_fld CASCADE;
	END IF;
	CREATE TABLE public.default_fld (dfn text);
	RETURN true;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.create_public_rev_table() RETURNS boolean AS '
DECLARE
	backup_file_base text;
	sql_qry text;
BEGIN
	IF (reln_exists(\'public\',\'rev\')) THEN
		DROP TABLE public.rev CASCADE;
	END IF;
	sql_qry := \'CREATE TABLE public.rev (
		name TEXT NOT NULL,
		userid TEXT DEFAULT user NOT NULL,
		modstamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
		orthkey TEXT NOT NULL,
		flags INTEGER DEFAULT 0 NOT NULL
		\' || field_dfn_text() || \' ) \';

	RAISE DEBUG \'%\', sql_qry;
	EXECUTE sql_qry;

	IF (reln_exists(\'public\',\'backup\')) THEN
		backup_file_base := (SELECT b FROM public.backup LIMIT 1);
		IF backup_file_base IS NOT NULL THEN
			PERFORM restore_public_rev_su(backup_file_base);
		END IF;
	END IF;

	PERFORM public.index_public_rev();

	RETURN TRUE;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.create_bc_tmp_tables() RETURNS boolean AS '
BEGIN
	IF (reln_exists(\'public\',\'tmp\')) THEN
		DROP TABLE public.tmp CASCADE;
	END IF;
	IF (reln_exists(\'public\',\'rev_new\')) THEN
		DROP TABLE public.rev_new CASCADE;
	END IF;
	CREATE TABLE public.tmp AS SELECT * FROM public.rev WHERE NULL;
	CREATE TABLE public.rev_new AS SELECT * FROM public.rev WHERE NULL;
	RETURN true;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.index_public_rev() RETURNS boolean AS '
BEGIN
 	RAISE INFO \'Creating indices...\';
	ALTER TABLE public.rev ADD PRIMARY KEY (name,userid,modstamp);
	CREATE INDEX public_orthkey ON public.rev (orthkey); 
	CREATE UNIQUE INDEX name_modstamp ON public.rev (name,modstamp); 
	CREATE INDEX public_rev_name_modstamp ON public.rev (name, modstamp);
	CREATE INDEX public_rev_name
		ON public.rev (name varchar_ops); 
	PERFORM if_server_version(\'7.4\', \'CREATE INDEX public_rev_name_pattern ON public.rev (name varchar_pattern_ops)\', \'CREATE INDEX public_rev_name_pattern ON public.rev (name)\');
	RETURN true;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.deindex_public_rev() RETURNS boolean AS '
BEGIN
 	RAISE INFO \'Dropping indices...\';
 	DROP INDEX public_orthkey;
 	DROP INDEX name_modstamp;
	DROP INDEX public_rev_name_modstamp;
	DROP INDEX public_rev_name;
	DROP INDEX public_rev_name_pattern;
	ALTER TABLE public.rev DROP CONSTRAINT rev_pkey;
	RETURN true;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.index_lex() RETURNS boolean AS '
BEGIN
	RAISE INFO \'indexing db cache\';
	CREATE UNIQUE INDEX lex_name ON lex (name varchar_ops);
 	CREATE INDEX lex_orthkey ON lex (orthkey varchar_ops); 

 	IF server_version(\'7.4\') THEN
		CREATE UNIQUE INDEX lex_name_pattern ON lex (name varchar_pattern_ops);
		CREATE INDEX lex_orthkey_pattern ON lex (orthkey varchar_pattern_ops);
 	ELSE
		CREATE UNIQUE INDEX lex_name_pattern ON lex (name);
		CREATE INDEX lex_orthkey_pattern ON lex (orthkey);
 	END IF; 
	RETURN true;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.deindex_lex() RETURNS boolean AS '
BEGIN
	RAISE INFO \'deindexing db cache\';
	DROP INDEX lex_name;
 	DROP INDEX lex_orthkey; 

	DROP INDEX lex_name_pattern;
	DROP INDEX lex_orthkey_pattern;

	RETURN true;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION semi_create_indices() RETURNS boolean AS '
BEGIN
 	RAISE DEBUG \'Creating SEMI indices...\';
	CREATE INDEX semi_pred_lex_id ON semi_pred (lex_id);
	CREATE INDEX semi_pred_pred_id ON semi_pred (pred_id);
	CREATE INDEX semi_frame_frame_id ON semi_frame (frame_id);
	CREATE INDEX semi_frame_var_id ON semi_frame (var_id);
	CREATE INDEX semi_var_var_id ON semi_var (var_id);
	CREATE INDEX semi_extra_extra_id ON semi_extra (extra_id);
	CREATE INDEX semi_mod_name_userid_modstamp ON semi_mod (name,userid,modstamp);
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
	DROP INDEX semi_mod_name_userid_modstamp CASCADE;
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
		modstamp TIMESTAMP WITH TIME ZONE,
		modstamp0 TIMESTAMP WITH TIME ZONE
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

CREATE OR REPLACE FUNCTION field_dfn_text() RETURNS text AS '
DECLARE
	x RECORD;
	t text;
BEGIN

	IF (reln_exists(\'public\',\'fld\') 
			AND 
			(SELECT count(*) FROM public.fld)>0) THEN
		RAISE INFO \'Using field defns found in public.fld\';
		FOR x IN SELECT dfn FROM fld LOOP
			t := COALESCE(t,\'\');
			t:= t || \',\n \' || x.dfn;
		END LOOP;
	ELSE
		RAISE EXCEPTION \'\n*\n*\n* no field definitions provided! (TABLE public.fld IS EMPTY)\n*\n*\';
	END IF;
	RETURN t;
END;
' LANGUAGE plpgsql;

