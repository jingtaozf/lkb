--- Copyright (c) 2003-2004 
--- Fabre Lambeau, Stephan Oepen, Benjamin Waldron;
--- see `licence.txt' for conditions.

---
--- sql queries embedded in db
---

CREATE OR REPLACE FUNCTION public.create_qry_tables() RETURNS boolean AS '
BEGIN
	IF (table_exists_p(\'public\',\'qry\')) THEN
		DROP TABLE public.qry CASCADE;
	END IF;
	IF (table_exists_p(\'public\',\'qrya\')) THEN
		DROP TABLE public.qrya CASCADE;
	END IF;
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
	RETURN true;
END;
' LANGUAGE plpgsql;

SELECT create_qry_tables();

\copy qry FROM 'qry.tsv'
\copy qrya FROM 'qrya.tsv'

---
-- PL/pgSQL
---

\i fns_plpgsql.sql