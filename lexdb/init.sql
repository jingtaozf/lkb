--
-- create tables
--

SELECT public.clean_up();

CREATE OR REPLACE FUNCTION public.dump_data() RETURNS boolean AS '
DECLARE
	backup_file_base text;
BEGIN
	IF (table_exists_p(\'public\',\'meta\')) THEN
		backup_file_base := \'BACKUP-BEFORE-LEXDB-UPDATE\';
		PERFORM dump_db_su(backup_file_base);
		DELETE FROM public.backup;
		INSERT INTO public.backup VALUES (backup_file_base);
		DROP TABLE public.revision CASCADE;
	END IF;
	RETURN true;
END;
' LANGUAGE plpgsql;

SELECT public.dump_data();
SELECT create_meta_table();
SELECT recreate_public_revision();
SELECT create_defn_table();
SELECT public.create_bc_temp_tables();
\copy public.meta from 'public.meta.tsv'

--
-- remaining function definitions
--

\i fns_plpgsql_2.sql

\i permissions.sql
