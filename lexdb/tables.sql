--
-- table creation functions
--

CREATE OR REPLACE FUNCTION public.create_backup_table() RETURNS boolean AS '
BEGIN
	IF (table_exists_p(\'public\',\'backup\')) THEN
		DROP TABLE public.backup CASCADE;
	END IF;
	CREATE TABLE backup (b text);
	RETURN true;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.create_meta_table() RETURNS boolean AS '
BEGIN
	IF (table_exists_p(\'public\',\'meta\')) THEN
		PERFORM public.hide_schemas();
		DROP TABLE public.meta CASCADE;
	END IF;
	CREATE TABLE public.meta (
		var text,
		val text
	);
	RETURN true;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.create_defn_table() RETURNS boolean AS '
DECLARE
	backup_file_base text;
BEGIN
	IF (table_exists_p(\'public\',\'defn\')) THEN
		DROP TABLE public.defn CASCADE;
	END IF;	
	CREATE TABLE public.defn (
			mode TEXT,
			slot TEXT,
			field TEXT,
			path TEXT,
			type TEXT,
		PRIMARY KEY (mode,slot, field)
		);

	IF (table_exists_p(\'public\',\'backup\')) THEN
		backup_file_base := (SELECT b FROM public.backup LIMIT 1);
		IF backup_file_base IS NOT NULL THEN
			PERFORM restore_public_defn_su(backup_file_base);
		END IF;
	END IF;
	RETURN true;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION field_defn_text() RETURNS text AS '
DECLARE
	x RECORD;
	t text;
BEGIN

	IF (table_exists_p(\'public\',\'fields\') AND (SELECT count(*) FROM public.fields)>0) THEN
		RAISE INFO \'Using field defns in public.fields\';
		FOR x IN SELECT defn FROM fields LOOP
			t := COALESCE(t,\'\');
			t:= t || \',\n \' || x.defn;
		END LOOP;
	ELSE
		RAISE INFO \'Using field defns in public.default_fields\';
		FOR x IN SELECT defn FROM default_fields LOOP
			t := COALESCE(t,\'\');
			t:= t || \',\n \' || x.defn;
		END LOOP;		
	END IF;
	RETURN t;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.create_fields_table() RETURNS boolean AS '
BEGIN
	IF (table_exists_p(\'public\',\'fields\')) THEN
		DROP TABLE public.fields CASCADE;
	END IF;
	CREATE TABLE public.fields (defn text);

	IF (table_exists_p(\'public\',\'default_fields\')) THEN
		DROP TABLE public.default_fields CASCADE;
	END IF;
	CREATE TABLE public.default_fields (defn text);
	RETURN true;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.recreate_public_revision() RETURNS boolean AS '
DECLARE
	backup_file_base text;
	sql_qry text;
BEGIN
	IF (table_exists_p(\'public\',\'revision\')) THEN
		DROP TABLE public.revision CASCADE;
	END IF;
	sql_qry := \'CREATE TABLE public.revision (
		name TEXT NOT NULL,
		userid TEXT DEFAULT user,
		version INTEGER DEFAULT 0,
		modstamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
		orthkey TEXT NOT NULL,
		flags INTEGER DEFAULT 0 NOT NULL
		\' || field_defn_text() || \' ) \';

	RAISE INFO \'%\', sql_qry;
	EXECUTE sql_qry;

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

CREATE OR REPLACE FUNCTION public.clean_up() RETURNS boolean AS '
BEGIN
	IF (table_exists_p(\'public\',\'qry\')) THEN
		DROP TABLE public.qry CASCADE;
	END IF;
	IF (table_exists_p(\'public\',\'qrya\')) THEN
		DROP TABLE public.qrya CASCADE;
	END IF;
	RETURN true;
END;
' LANGUAGE plpgsql;

