--- Copyright (c) 2003 - 2005
--- Benjamin Waldron, Fabre Lambeau, Stephan Oepen;
--- see `licence.txt' for conditions.

--
-- table creation functions
--

CREATE OR REPLACE FUNCTION public.create_public_rev_table() RETURNS boolean AS '
DECLARE
	sql_qry text;
BEGIN
	sql_qry := \'CREATE TABLE public.rev (
		name TEXT NOT NULL,
		userid TEXT DEFAULT user NOT NULL,
		modstamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
		dead BOOLEAN DEFAULT \\\'0\\\' NOT NULL
		\' || soft_rev_field_definitions() || \' ) \';
	EXECUTE sql_qry;
	--PERFORM public.index_public_rev();
	RETURN TRUE;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION soft_rev_field_definitions() RETURNS text AS '
DECLARE
	x RECORD;
	t text;
BEGIN
	IF (reln_exists(\'public\',\'fld\') 
			AND 
			(SELECT count(*) FROM public.fld)>0) THEN
		RAISE DEBUG \'Using field defns found in public.fld\';
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

--
-- public.rev indexes:
--   name_modstamp
--   rev_name_modstamp
--   rev_name
--   rev_pattern
--

--CREATE OR REPLACE FUNCTION public.index_public_rev() RETURNS boolean AS '
--BEGIN
-- 	RAISE DEBUG \'Creating indices...\';
--	ALTER TABLE public.rev ADD PRIMARY KEY (name,userid,modstamp);
--	CREATE UNIQUE INDEX name_modstamp ON public.rev (name,modstamp); 
--	CREATE INDEX rev_name_modstamp ON public.rev (name, modstamp);
--	CREATE INDEX rev_name
--		ON public.rev (name varchar_ops); 
--	PERFORM if_psql_server_version(\'7.4\', \'CREATE INDEX rev_name_pattern ON public.rev (name varchar_pattern_ops)\', \'CREATE INDEX rev_name_pattern ON public.rev (name)\');
--	RETURN true;
--END;
--' LANGUAGE plpgsql;

--CREATE OR REPLACE FUNCTION public.deindex_public_rev() RETURNS boolean AS '
--BEGIN
-- 	RAISE DEBUG \'Dropping indices...\';
-- 	DROP INDEX name_modstamp;
--	DROP INDEX rev_name_modstamp;
--	DROP INDEX rev_name;
--	DROP INDEX rev_name_pattern;
--	ALTER TABLE public.rev DROP CONSTRAINT rev_pkey;
--	RETURN true;
--END;
--' LANGUAGE plpgsql;

--
-- lex indexes:
--       lex_name
--       lex_name_pattern
--

--CREATE OR REPLACE FUNCTION public.index_lex() RETURNS boolean AS '
--BEGIN
--	RAISE DEBUG \'indexing db cache\';
--	CREATE UNIQUE INDEX lex_name ON lex (name varchar_ops);
--
-- 	IF psql_server_version(\'7.4\') THEN
--		CREATE UNIQUE INDEX lex_name_pattern ON lex (name varchar_pattern_ops);
-- 	ELSE
--		CREATE UNIQUE INDEX lex_name_pattern ON lex (name);
-- 	END IF; 
--	RETURN true;
--END;
--' LANGUAGE plpgsql;

--CREATE OR REPLACE FUNCTION public.deindex_lex() RETURNS boolean AS '
--BEGIN
--	RAISE DEBUG \'deindexing db cache\';
--	DROP INDEX lex_name;
--	DROP INDEX lex_name_pattern;
--	RETURN true;
--END;
--' LANGUAGE plpgsql;

--
-- lex_key indexes:
--   lex_key_key
--

--CREATE OR REPLACE FUNCTION public.index_lex_key() RETURNS boolean AS '
--BEGIN
--	RAISE DEBUG \'indexing lex_key\';
--	CREATE INDEX lex_key_key ON lex_key (key);
--	RETURN true;
--END;
--' LANGUAGE plpgsql;

--CREATE OR REPLACE FUNCTION public.deindex_lex_key() RETURNS boolean AS '
--BEGIN
--	RAISE DEBUG \'deindexing lex_key\';
--	DROP INDEX lex_key_key;
--	RETURN true;
--END;
--' LANGUAGE plpgsql;

--
-- private rev indexes:
--   rev_name_userid_modstamp
--

CREATE OR REPLACE FUNCTION public.index_rev() RETURNS boolean AS '
BEGIN
	RAISE DEBUG \'indexing rev\';
	EXECUTE \'CREATE UNIQUE INDEX rev_name ON rev (name)\';
	RETURN true;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION public.deindex_rev() RETURNS boolean AS '
BEGIN
	RAISE DEBUG \'deindexing rev\';
	DROP INDEX rev_name;
	RETURN true;
END;
' LANGUAGE plpgsql;

--
-- semi indexes:
--  semi_pred_lex_id
--  semi_pred_pred_id
--  semi_frame_frame_id
--  semi_frame_var_id
--  semi_var_var_id
--  semi_extra_extra_id
--  semi_mod_name_userid_modstamp
--

--CREATE OR REPLACE FUNCTION semi_create_indices() RETURNS boolean AS '
--BEGIN
-- 	RAISE DEBUG \'Creating SEMI indices...\';
--	CREATE INDEX semi_pred_lex_id ON semi_pred (lex_id);
--	CREATE INDEX semi_pred_pred_id ON semi_pred (pred_id);
--	CREATE INDEX semi_frame_frame_id ON semi_frame (frame_id);
--	CREATE INDEX semi_frame_var_id ON semi_frame (var_id);
--	CREATE INDEX semi_var_var_id ON semi_var (var_id);
--	CREATE INDEX semi_extra_extra_id ON semi_extra (extra_id);
--	CREATE INDEX semi_mod_name_userid_modstamp ON semi_mod (name,userid,modstamp);
--RETURN true;
--END;
--' LANGUAGE plpgsql;

--CREATE OR REPLACE FUNCTION semi_drop_indices() RETURNS boolean AS '
--BEGIN
-- 	RAISE DEBUG \'Dropping SEMI indices...\';
--	DROP INDEX semi_pred_lex_id CASCADE;
--	DROP INDEX semi_pred_pred_id CASCADE;
--	DROP INDEX semi_frame_frame_id CASCADE;
--	DROP INDEX semi_frame_var_id CASCADE;
--	DROP INDEX semi_var_var_id CASCADE;
--	DROP INDEX semi_extra_extra_id CASCADE;
--	DROP INDEX semi_mod_name_userid_modstamp CASCADE;
--RETURN true;
--END;
--' LANGUAGE plpgsql;

--CREATE OR REPLACE FUNCTION create_table_lex_key() RETURNS boolean AS '
--BEGIN
--	CREATE TABLE lex_key (
--		name TEXT NOT NULL,
--		userid TEXT DEFAULT user NOT NULL,
--		modstamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
--		key text NOT NULL
--		);
--RETURN true;
--END;
--' LANGUAGE plpgsql;

--CREATE OR REPLACE FUNCTION create_view_rev_all() RETURNS boolean AS '
--BEGIN
--	CREATE VIEW rev_all
--		AS SELECT * FROM public.rev 
--			UNION 
-- 			SELECT * FROM rev;
--RETURN true;
--END;
--' LANGUAGE plpgsql;

--CREATE OR REPLACE FUNCTION create_view_head() RETURNS boolean AS '
--BEGIN
-- 	CREATE VIEW head
--		AS SELECT fil.*
-- 			FROM 
-- 			(filt AS fil
-- 			NATURAL JOIN 
-- 				(SELECT name, max(modstamp) AS modstamp 
-- 					FROM filt
--					GROUP BY name) AS t1)
--			WHERE dead=\'0\';
--RETURN true;
--END;
--' LANGUAGE plpgsql;

--CREATE OR REPLACE FUNCTION create_tables_semi() RETURNS boolean AS '
--BEGIN
-- 	RAISE DEBUG \'Creating SEMI structures...\';
--	CREATE TABLE semi_pred (
--		lex_id text NOT NULL,
--		pred_id text NOT NULL,
--		frame_id int NOT NULL,
--		pred_txt text NOT NULL,
--		string_p boolean NOT NULL
--	);
--
--	CREATE TABLE semi_frame (
--		frame_id int NOT NULL,
--		slot text NOT NULL,
--		str text,
--		symb text,
--		var_id int,
--		type text
--	);
--
--	CREATE TABLE semi_var (
--		var_id int NOT NULL,
--		extra_id int NOT NULL
--	);
--
--	CREATE TABLE semi_extra (
--		extra_id int NOT NULL,
--		feat text NOT NULL,
--		val text NOT NULL
--	);
--
--	CREATE TABLE semi_mod (
--		name text,
--		userid text,
--		modstamp TIMESTAMP WITH TIME ZONE,
--		modstamp0 TIMESTAMP WITH TIME ZONE
--	);
--
--	--PERFORM semi_create_indices();
--
--	CREATE OR REPLACE VIEW semi_obj AS
--		SELECT lex_id,pred_id, slot, str, type, feat, val FROM
--		semi_pred NATURAL JOIN
--		semi_frame NATURAL LEFT JOIN
--		semi_var NATURAL LEFT JOIN
--		semi_extra;
--
--	--CREATE OR REPLACE VIEW semi_obj2 AS
--	--	SELECT * FROM
--	--	semi_pred NATURAL JOIN
--	--	semi_frame NATURAL LEFT JOIN
--	--	semi_var NATURAL LEFT JOIN
--	--	semi_extra;
--
--	RETURN true;
--END;
--' LANGUAGE plpgsql;
