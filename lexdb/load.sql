--- Copyright (c) 2003 - 2005 
--- Benjamin Waldron;
--- see `licence.txt' for conditions.

--
-- function definitions
--

SET client_min_messages TO warning;

\i util.sql
\i tables.sql

CREATE TABLE public.fld (dfn text);

-- NOW LOAD TABLE fld !