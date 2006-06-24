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
\copy public.meta from 'meta.tsv'

SELECT public.create_public_rev_table();

CREATE TABLE public.dfn (
		mode TEXT,
		slot TEXT,
		field TEXT,
		path TEXT,
		type TEXT,
	PRIMARY KEY (mode,slot,field));

--
-- remaining function definitions
--

GRANT SELECT ON public.fld TO PUBLIC;
GRANT SELECT ON public.meta TO PUBLIC;
GRANT SELECT ON public.dfn TO PUBLIC;
GRANT SELECT ON public.rev TO PUBLIC;