--- Copyright (c) 2003-2004 
--- Fabre Lambeau, Stephan Oepen, Benjamin Waldron;
--- see `licence.txt' for conditions.

SELECT public.hide_schemas();

---
--- sql queries embedded in db
---

CREATE TABLE qry (
  fn text,
  arity int,
  sql_code text,
PRIMARY KEY (fn)
);
CREATE TABLE qrya (
  fn text,
  arg int,
  type text,
PRIMARY KEY (fn,arg)
);

DELETE FROM qry;
DELETE FROM qrya;

\copy qry FROM 'qry.tsv'
\copy qrya FROM 'qrya.tsv'

---
-- SQL functions
---

CREATE VIEW revision_all AS SELECT * FROM revision WHERE NULL;
CREATE VIEW active AS SELECT * FROM revision WHERE NULL;
CREATE TABLE current_grammar AS SELECT * FROM revision WHERE NULL;

\i fns_sql.sql

DROP VIEW revision_all;
DROP VIEW active;
DROP TABLE current_grammar;

---
-- PL/pgSQL
---

\i fns_plpgsql.sql

---
-- fns version
---

\i fns-version.sql