--- Copyright (c) 2003-2004 
--- Fabre Lambeau, Stephan Oepen, Benjamin Waldron;
--- see `licence.txt' for conditions.

--
-- function definitions
--

\i util.sql
\i tables.sql
\i fns_plpgsql.sql

SELECT create_public_backup_table();
SELECT public.dump_data();

SELECT create_public_fields_table();
\copy default_fields FROM 'default-fields.tsv'