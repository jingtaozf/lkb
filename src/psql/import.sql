--- Copyright (c) 2003-2004 
--- Fabre Lambeau, Stephan Oepen, Benjamin Waldron;
--- see `licence.txt' for conditions.

--- postgres optimization is poor...
ALTER DATABASE lingo SET enable_seqscan TO off;

CREATE FUNCTION "plpgsql_call_handler" () RETURNS LANGUAGE_HANDLER AS '$libdir/plpgsql' LANGUAGE C;
CREATE TRUSTED LANGUAGE "plpgsql" HANDLER "plpgsql_call_handler";

CREATE TABLE public.meta (
  var varchar,
  val varchar
);
DELETE FROM public.meta WHERE var='db-version';
DELETE FROM public.meta WHERE var='filter';
INSERT INTO public.meta VALUES ('db-version', '3.04');
INSERT INTO public.meta VALUES ('filter', 'TRUE');

---
--- main table
---
CREATE TABLE public.revision (
  name VARCHAR NOT NULL,
  userid VARCHAR DEFAULT user,
  version INTEGER DEFAULT 0,
  modstamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  type VARCHAR NOT NULL,
  orthography VARCHAR NOT NULL,
  orthkey VARCHAR NOT NULL,
  pronunciation VARCHAR,
  keyrel VARCHAR,
  altkey VARCHAR,
  alt2key VARCHAR,
  keytag VARCHAR,
  altkeytag VARCHAR,
  compkey VARCHAR,
  ocompkey VARCHAR,
  complete VARCHAR,
  semclasses VARCHAR,
  preferences VARCHAR,
  classifier VARCHAR,
  selectrest VARCHAR,
  jlink VARCHAR,
  comments VARCHAR,
  exemplars VARCHAR,
  usages VARCHAR,
  lang VARCHAR DEFAULT 'EN',
  country VARCHAR,
  dialect VARCHAR,
  domains VARCHAR,
  genres VARCHAR,
  register VARCHAR,
  confidence real NOT NULL,
  source VARCHAR,
  flags INTEGER DEFAULT 0 NOT NULL,
 PRIMARY KEY (name,version,userid)
);

CREATE INDEX public_orthkey
ON public.revision (orthkey); 

CREATE UNIQUE INDEX name_modstamp
ON public.revision (name,modstamp); 

CREATE INDEX public_revision_name_modstamp ON public.revision (name, modstamp);

\i embedded-code.sql
\i mwe.sql
\i semi.sql
\i defn.sql

