--- Copyright (c) 2003 - 2005 
--- Benjamin Waldron, Fabre Lambeau, Stephan Oepen;
--- see `licence.txt' for conditions.

--
-- create tables
--

--SELECT public.create_public_meta_table();
CREATE TABLE public.meta (
	var text,
	val text);
SELECT public.create_public_rev_table();
--SELECT public.create_public_dfn_table();
CREATE TABLE public.dfn (
		mode TEXT,
		slot TEXT,
		field TEXT,
		path TEXT,
		type TEXT,
	PRIMARY KEY (mode,slot, field));
--SELECT public.create_bc_tmp_tables();
CREATE TABLE public.tmp AS SELECT * FROM public.rev WHERE NULL;
CREATE TABLE public.rev_new AS SELECT * FROM public.rev WHERE NULL;
\copy public.meta from 'meta.tsv'

--
-- remaining function definitions
--

\i fns_plpgsql_2.sql
\i permissions.sql
