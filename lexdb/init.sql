--- Copyright (c) 2003 - 2005 
--- Benjamin Waldron, Fabre Lambeau, Stephan Oepen;
--- see `licence.txt' for conditions.

--
-- create tables
--

SELECT public.create_public_meta_table();
SELECT public.create_public_rev_table();
SELECT public.create_public_dfn_table();
SELECT public.create_bc_tmp_tables();
\copy public.meta from 'public.meta'

--
-- remaining function definitions
--

\i fns_plpgsql_2.sql
\i permissions.sql
