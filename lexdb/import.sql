--- Copyright (c) 2003-2004 
--- Fabre Lambeau, Stephan Oepen, Benjamin Waldron;
--- see `licence.txt' for conditions.

CREATE OR REPLACE FUNCTION check_version(text) RETURNS boolean AS '
DECLARE
	x text;
BEGIN
 SELECT INTO x * FROM version();
 x := split_part(x,'' '',2);
 RETURN x>=$1;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION if_version(text,text,text) RETURNS text AS '
DECLARE 
	x boolean;
BEGIN
	x:= false;
 IF check_version($1) THEN
	EXECUTE $2;
	x:= true;
 ELSE
	EXECUTE $3;
 END IF;
 RETURN x;
END;
' LANGUAGE plpgsql;

CREATE TABLE public.meta (
  var text,
  val text
);
DELETE FROM public.meta WHERE var='db-version';
DELETE FROM public.meta WHERE var='filter';
INSERT INTO public.meta VALUES ('db-version', '3.08');
INSERT INTO public.meta VALUES ('filter', 'TRUE');

---
--- main table
---
CREATE TABLE public.revision (
  name TEXT NOT NULL,
  userid TEXT DEFAULT user,
  version INTEGER DEFAULT 0,
  modstamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  type TEXT NOT NULL,
  orthography TEXT NOT NULL,
  orthkey TEXT NOT NULL,
  pronunciation TEXT,
  keyrel TEXT,
  altkey TEXT,
  alt2key TEXT,
  keytag TEXT,
  altkeytag TEXT,
  compkey TEXT,
  ocompkey TEXT,
  complete TEXT,
  semclasses TEXT,
  preferences TEXT,
  classifier TEXT,
  selectrest TEXT,
  jlink TEXT,
  comments TEXT,
  exemplars TEXT,
  usages TEXT,
  lang TEXT DEFAULT 'EN',
  country TEXT,
  dialect TEXT,
  domains TEXT,
  genres TEXT,
  register TEXT,
  confidence real NOT NULL,
  source TEXT,
  flags INTEGER DEFAULT 0 NOT NULL,
 PRIMARY KEY (name,version,userid)
);

CREATE UNIQUE INDEX public_revision_name
 ON public.revision (name varchar_ops); 

SELECT if_version('7.4','CREATE UNIQUE INDEX public_revision_name_pattern ON public.revision (name varchar_pattern_ops)','CREATE UNIQUE INDEX public_revision_name_pattern ON public.revision (name)'); 

CREATE INDEX public_orthkey
ON public.revision (orthkey); 

CREATE UNIQUE INDEX name_modstamp
ON public.revision (name,modstamp); 

CREATE INDEX public_revision_name_modstamp ON public.revision (name, modstamp);

-- temp tables defined here for backwards compatibility
CREATE TABLE temp AS SELECT * FROM public.revision WHERE NULL;
CREATE TABLE revision_new AS SELECT * FROM public.revision WHERE NULL;

\i embedded-code.sql

CREATE TABLE defn (
  mode TEXT,
  slot TEXT,
  field TEXT,
  path TEXT,
  type TEXT,
PRIMARY KEY (mode,slot, field)
);

\copy defn FROM 'defn.tsv'
\i permissions.sql

\i mwe.sql
