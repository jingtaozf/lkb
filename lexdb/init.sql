--- Copyright (c) 2003-2004 
--- Fabre Lambeau, Stephan Oepen, Benjamin Waldron;
--- see `licence.txt' for conditions.

--
-- create tables
--

SELECT public.clean_up();
SELECT public.create_public_meta_table();
SELECT public.create_public_revision_table();
SELECT public.create_public_defn_table();
SELECT public.create_bc_temp_tables();
\copy public.meta from 'public.meta.tsv'

--
-- remaining function definitions
--

\i fns_plpgsql_2.sql
\i permissions.sql
