--- postgres optimization is poor...
ALTER DATABASE lingo SET enable_seqscan TO off;

CREATE TABLE public.meta (
  var varchar(50),
  val varchar(250)
);
INSERT INTO public.meta VALUES ('db-version', '2.9');
INSERT INTO public.meta VALUES ('filter', 'TRUE');

---
--- main table
---
CREATE TABLE public.revision (
  name VARCHAR(95) NOT NULL,
  userid VARCHAR(25) DEFAULT user,
  version INTEGER DEFAULT 0,
  modstamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  type VARCHAR(95) NOT NULL,
  orthography VARCHAR(200) NOT NULL,
  orthkey VARCHAR(200) NOT NULL,
  pronunciation VARCHAR(200),
  keyrel VARCHAR(50),
  altkey VARCHAR(50),
  alt2key VARCHAR(50),
  keytag VARCHAR(50),
  altkeytag VARCHAR(50),
  compkey VARCHAR(50),
  ocompkey VARCHAR(50),
  complete VARCHAR(200),
  semclasses VARCHAR(100),
  preferences VARCHAR(200),
  classifier VARCHAR(25),
  selectrest VARCHAR(50),
  jlink VARCHAR(50),
  comments VARCHAR(255),
  exemplars VARCHAR(50),
  usages VARCHAR(50),
  lang VARCHAR(8) DEFAULT 'EN',
  country VARCHAR(2),
  dialect VARCHAR(25),
  domains VARCHAR(15),
  genres VARCHAR(25),
  register VARCHAR(50),
  confidence real NOT NULL,
  source VARCHAR(50),
  flags INTEGER DEFAULT 0 NOT NULL,
 PRIMARY KEY (name,version,userid)
);

CREATE INDEX public_orthkey
ON public.revision (orthkey); 

CREATE UNIQUE INDEX name_modstamp
ON public.revision (name,modstamp); 

CREATE INDEX public_revision_name_modstamp ON public.revision (name, modstamp);

---
--- multi word entries
---

CREATE TABLE public.multi (
  name VARCHAR(95),
  verb_id VARCHAR(95),
  particle VARCHAR(95),
  type VARCHAR(200),
  keyrel VARCHAR(200),
PRIMARY KEY (name)
);

\i embedded-code.sql
\i defn.sql

