--- Copyright (c) 2003 - 2005 
--- Benjamin Waldron;
--- see `licence.txt' for conditions.

--
-- function definitions
--

\i util.sql
\i tables.sql
\i fns.sql

--SELECT public.create_public_backup_table();
--SELECT public.dump_data();

--SELECT public.create_public_fld_table();
CREATE TABLE public.fld (dfn text);

-- obsolete
--\copy default_fld FROM 'default-fld.tsv'
