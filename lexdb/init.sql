--- Copyright (c) 2003 - 2005 
--- Benjamin Waldron, Fabre Lambeau, Stephan Oepen;
--- see `licence.txt' for conditions.

-- TABLE fld HAS BEEN LOADED

--
-- create tables
--

SET client_min_messages TO warning;

CREATE TABLE public.meta (
	var text,
	val text);
SELECT public.create_public_rev_table();
CREATE TABLE public.dfn (
		mode TEXT,
		slot TEXT,
		field TEXT,
		path TEXT,
		type TEXT,
	PRIMARY KEY (mode,slot,field));
CREATE TABLE public.tmp AS SELECT * FROM public.rev WHERE NULL;
\copy public.meta from 'meta.tsv'

--
-- remaining function definitions
--

\i fns_post.sql
\i permissions.sql