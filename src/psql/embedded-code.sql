---
--- first, get rid of existing user schemas
--- (user schema USER is renamed to tmpUSER,
---  hence data is not _immediately_ lost)
---

CREATE OR REPLACE FUNCTION public.kill_namespace (text) RETURNS text AS
'
delete from public.meta where var=''kill_namespace'';
insert into public.meta values (''kill_namespace'', (select count(*)
FROM pg_catalog.pg_class c
     LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
     where nspname= $1));
delete from pg_catalog.pg_class where relnamespace=(select oid from pg_catalog.pg_namespace where nspname= ''tmp''|| $1);
delete from pg_catalog.pg_namespace where nspname= ''tmp''|| $1;
update pg_catalog.pg_namespace set nspname=''tmp''|| $1 where nspname= $1 ;
select val from public.meta where var=''kill_namespace'' limit 1;
' 
LANGUAGE SQL;

SELECT kill_namespace(val) FROM public.meta WHERE var='user';
DELETE FROM public.meta WHERE var='user';
--DROP FUNCTION public.kill_namespace(text);

---
--- sql queries embedded in db
---

CREATE TABLE public.qry (
  fn VARCHAR(50),
  arity int,
  sql_code VARCHAR(4096),
PRIMARY KEY (fn)
);

-- arities
CREATE TABLE public.qrya (
  fn VARCHAR(50),
  arg int,
  type VARCHAR(50),
PRIMARY KEY (fn,arg)
);

DELETE FROM qry;
DELETE FROM qrya;

INSERT INTO qrya VALUES ( 'test', 0, 'select-list' );
INSERT INTO qry VALUES 
       ( 'test', 1, 
         '$0' );

INSERT INTO qrya VALUES ( 'test-like-text', 0, 'like-text' );
INSERT INTO qry VALUES 
       ( 'test-like-text', 1, 
         'SELECT $0;' );

INSERT INTO qrya VALUES ( 'next-version', 0, 'like-text');
INSERT INTO qry VALUES 
       ( 'next-version', 1, 
         '
SELECT COALESCE(1 + max(version),0) FROM revision_all 
	WHERE name ~* $0 AND user=user; 
');


INSERT INTO qry VALUES 
       ( 'orthography-set', 0, 
         'SELECT DISTINCT orthography FROM current_grammar' );

INSERT INTO qry VALUES 
       ( 'lex-id-set', 0, 
         'SELECT DISTINCT name FROM current_grammar');

INSERT INTO qrya VALUES ( 'lookup-word', 0, 'like-text' );
INSERT INTO qry VALUES 
       ( 'lookup-word', 1, 
         'SELECT name FROM current_grammar WHERE orthkey ILIKE $0' );

INSERT INTO qrya VALUES ( 'lookup-general', 0, 'select-list' );
INSERT INTO qrya VALUES ( 'lookup-general', 1, 'like-text' );
INSERT INTO qry VALUES 
       ( 'lookup-general', 2, 
         'SELECT name FROM current_grammar WHERE $0 ILIKE $1' );

INSERT INTO qrya VALUES ( 'lookup-general-null', 0, 'select-list' );
INSERT INTO qry VALUES 
       ( 'lookup-general-null', 1,
         'SELECT name FROM current_grammar WHERE $0 IS NULL' );

INSERT INTO qrya VALUES ( 'retrieve-entries-by-orthkey', 0, 'select-list' );
INSERT INTO qrya VALUES ( 'retrieve-entries-by-orthkey', 1, 'like-text' );
INSERT INTO qry VALUES 
       ( 'retrieve-entries-by-orthkey', 2, 
         'SELECT $0 FROM current_grammar WHERE orthkey ILIKE $1' );

INSERT INTO qrya VALUES ( 'retrieve-entry', 0, 'select-list' );
INSERT INTO qrya VALUES ( 'retrieve-entry', 1, 'like-text' );
INSERT INTO qry VALUES 
       ( 'retrieve-entry', 2, 
         'SELECT $0 FROM current_grammar WHERE name ILIKE $1' );


INSERT INTO qrya VALUES ( 'retrieve-all-entries', 0, 'select-list' );
INSERT INTO qry VALUES 
       ( 'retrieve-all-entries', 1, 
         'SELECT $0 FROM current_grammar' );


INSERT INTO qrya VALUES ( 'initialize-current-grammar', 0, 'where-subcls' );
INSERT INTO qry VALUES 
       ( 'initialize-current-grammar', 1, 
'

CREATE OR REPLACE VIEW filtered
AS SELECT * 
     FROM revision_all
     WHERE flags = 1
      AND $0;

UPDATE meta SET val=$0:text WHERE var=''filter'';

--
-- build current_grammar
--

SELECT build_current_grammar();

VACUUM ANALYZE current_grammar;

' );

INSERT INTO qry VALUES 
       ( 'build-current-grammar', 0, 
'
SELECT build_current_grammar();
' );

INSERT INTO qry VALUES 
       ( 'cluster-current-grammar', 0, 
'
CLUSTER current_grammar_name ON current_grammar; 
' );


INSERT INTO qrya VALUES ( 'update-entry', 0, 'text' );
INSERT INTO qrya VALUES ( 'update-entry', 1, 'select-list' );
INSERT INTO qrya VALUES ( 'update-entry', 2, 'value-list' );
INSERT INTO qry VALUES 
       ( 'update-entry', 3, 
       '
INSERT INTO revision (name, $1) VALUES ($0 , $2); 
DELETE FROM current_grammar 
 WHERE name ILIKE $0:like-text ; 
INSERT INTO current_grammar 
 SELECT * FROM active
  WHERE name ILIKE $0:like-text 
   LIMIT 1;
' );

INSERT INTO qry VALUES 
       ( 'clear-scratch', 0, 
       '
DELETE FROM revision;
       ' );

INSERT INTO qry VALUES 
       ( 'clear-scratch2', 0, 
       '
SELECT  remove_scratch_entries(name) FROM revision;
       ' );

INSERT INTO qry VALUES 
       ( 'commit-scratch', 0, 
       '
INSERT INTO public.revision (SELECT * FROM revision);
DELETE FROM revision;
       ' );

INSERT INTO qry VALUES 
       ( 'show-scratch', 0, 
       '
SELECT distinct name FROM revision;
       ' );

INSERT INTO qrya VALUES ( 'add-to-db', 0, 'text' );
INSERT INTO qry VALUES 
       ( 'add-to-db', 1, 
       '
COPY public.revision FROM $0 DELIMITERS '','' WITH NULL AS '''';
       ' );

INSERT INTO qrya VALUES ( 'normalize-csv-lexicon', 0, 'text' );
INSERT INTO qrya VALUES ( 'normalize-csv-lexicon', 1, 'text' );
INSERT INTO qry VALUES 
       ( 'normalize-csv-lexicon', 2, 
       '
DELETE FROM temp;
COPY temp FROM $0 DELIMITERS '','' WITH NULL AS '''';
COPY temp TO $1 DELIMITERS '','' WITH NULL AS '''';
DELETE FROM temp;
       ' );

INSERT INTO qrya VALUES ( 'merge-into-db', 0, 'text' );
INSERT INTO qrya VALUES ( 'merge-into-db', 1, 'text' );
INSERT INTO qry VALUES 
       ( 'merge-into-db', 2, 
       '
DROP INDEX public_orthkey;
DROP INDEX name_modstamp;
DROP INDEX public_revision_name_modstamp;
alter table public.revision drop constraint revision_pkey;

DELETE FROM temp;
COPY temp FROM $0 DELIMITERS '','' WITH NULL AS '''';
DELETE FROM revision_new;
INSERT INTO revision_new
	select * from (select distinct name,userid,version from temp except select name,userid,version from public.revision) as t1 natural join temp;
INSERT INTO public.revision SELECT * FROM revision_new;
 
alter table public.revision add primary key (name,version,userid);
CREATE INDEX public_orthkey ON public.revision (orthkey); 
CREATE UNIQUE INDEX name_modstamp ON public.revision (name,modstamp); 
CREATE INDEX public_revision_name_modstamp ON public.revision (name, modstamp);

COPY revision_new TO $1 DELIMITERS '','' WITH NULL AS '''';
SELECT count(*) FROM revision_new;
      ' );

INSERT INTO qrya VALUES ( 'merge-defn', 0, 'text' );
INSERT INTO qry VALUES 
       ( 'merge-defn', 1, 
       '
CREATE TABLE temp_defn AS SELECT * FROM defn WHERE NULL;
COPY temp_defn FROM $0;

CREATE TABLE temp_defn_new AS 
	SELECT * FROM (SELECT mode, slot, field FROM temp_defn EXCEPT
			SELECT mode, slot, field FROM defn) AS t1 
			NATURAL JOIN temp_defn;
INSERT INTO defn
	SELECT * FROM temp_defn_new;
DELETE FROM meta WHERE var=''tmp-merge-defn'';
INSERT INTO meta VALUES (''tmp-merge-defn'',(SELECT count(*) FROM temp_defn_new));
DROP TABLE temp_defn_new;
DROP TABLE temp_defn;
SELECT val FROM meta WHERE var=''tmp-merge-defn'';
      ' );

INSERT INTO qrya VALUES ( 'merge-multi-into-db', 0, 'text' );
INSERT INTO qry VALUES 
       ( 'merge-multi-into-db', 1, 
       '
DELETE FROM temp_multi;
COPY temp_multi FROM $0 DELIMITERS '','';
DELETE FROM public.multi WHERE name IN (SELECT name FROM temp_multi);
INSERT INTO public.multi
 (SELECT * FROM temp_multi);
       ' );

INSERT INTO qrya VALUES ( 'dump-db', 0, 'text' );
INSERT INTO qrya VALUES ( 'dump-db', 1, 'text' );
INSERT INTO qry VALUES 
       ( 'dump-db', 2, 
       '
DELETE FROM temp;
INSERT INTO temp
 (SELECT * FROM public.revision ORDER BY name, userid, version);
COPY temp TO $0 DELIMITERS '','' WITH NULL AS '''';

CREATE TABLE temp_defn AS 
 SELECT * FROM defn ORDER BY mode,slot,field;
COPY temp_defn TO $1;
DROP TABLE temp_defn;
' );

INSERT INTO qrya VALUES ( 'dump-scratch-db', 0, 'text' );
INSERT INTO qry VALUES 
       ( 'dump-scratch-db', 1, 
       '
DELETE FROM temp;
INSERT INTO temp
 (SELECT * FROM revision ORDER BY name, userid, version);
COPY temp TO $0 DELIMITERS '','' WITH NULL AS '''';
' );

INSERT INTO qrya VALUES ( 'dump-multi-db', 0, 'text' );
INSERT INTO qry VALUES 
       ( 'dump-multi-db', 1, 
       '
DELETE FROM temp_multi;
INSERT INTO temp_multi
 (SELECT * FROM multi ORDER BY name);
COPY temp_multi TO $0 DELIMITERS '','';
' );

INSERT INTO qrya VALUES ( 'value-set', 0, 'select-list' );
INSERT INTO qry VALUES 
       ( 'value-set', 1, 
       'SELECT DISTINCT $0 FROM revision_all WHERE $0 IS NOT NULL;' );

INSERT INTO qrya VALUES ( 'create-schema', 0, 'select-list' );
INSERT INTO qry VALUES 
       ( 'create-schema', 1, 
       '
CREATE SCHEMA $0;

INSERT INTO public.meta VALUES (''user'', $0:text );

CREATE TABLE meta AS SELECT * FROM public.meta WHERE var=''filter'';

---
--- scratch tables
---

CREATE TABLE revision AS (SELECT * FROM public.revision);

---
--- temp tables
---

CREATE TABLE temp AS SELECT * FROM public.revision WHERE NULL;
CREATE TABLE revision_new AS SELECT * FROM public.revision WHERE NULL;

---
--- table on which queries executed
---

CREATE TABLE current_grammar AS SELECT * FROM public.revision WHERE NULL;

CREATE UNIQUE INDEX current_grammar_name
ON current_grammar (name); 

CREATE INDEX current_grammar_orthkey
ON current_grammar (orthkey); 

---
--- views
---

--CREATE VIEW active AS SELECT * FROM public.revision WHERE NULL;
CREATE VIEW filtered AS SELECT * FROM public.revision WHERE NULL;

CREATE VIEW active
 AS SELECT fil.*
 FROM 
  (filtered AS fil
  NATURAL JOIN 
   (SELECT name, max(modstamp) AS modstamp 
     FROM filtered
     GROUP BY name) AS t1
); 

CREATE VIEW revision_all
AS SELECT * FROM public.revision 
   UNION 
   SELECT * FROM revision;

---
--- FUNCTIONS
---

CREATE OR REPLACE FUNCTION remove_scratch_entries (text) RETURNS boolean AS
''
DELETE FROM revision WHERE name = \\$1; 
DELETE FROM current_grammar 
 WHERE name = \\$1 ; 
INSERT INTO current_grammar 
 SELECT * FROM active
  WHERE lower(name)=lower(\\$1) 
   LIMIT 1;
CLUSTER current_grammar_name ON current_grammar; 
SELECT true;'' 
LANGUAGE SQL;

CREATE OR REPLACE FUNCTION build_current_grammar () RETURNS boolean AS
''

DELETE FROM current_grammar; 
INSERT INTO current_grammar 
	SELECT * FROM active; 
DELETE FROM meta WHERE var=''''build_time'''';
INSERT INTO meta VALUES (''''build_time'''',current_timestamp);

SELECT true;'' 
LANGUAGE SQL;

--Drop TABLE prd;
CREATE TABLE prd 
	(
	lexid text,
	predkey integer,
	pred text,
	lex text,
	pos text,
	sense text,
	carg text,
PRIMARY KEY (predkey)
	);

CREATE INDEX prd_lexid on prd (lexid);

--DROP TABLE arg;
CREATE TABLE arg
	(
	predkey int,
	argkey int,
	arg int,
	type text,
PRIMARY KEY (argkey)
	);

CREATE INDEX arg_predkey on arg (predkey);

--DROP TABLE ddd;
CREATE TABLE ddd
	(
	argkey int,
	feat text,
	val text
	);

CREATE INDEX ddd_argkey on ddd (argkey);

create or replace view obj_semi as select lexid, pred, lex, pos, sense, carg, arg, type, 
	(select val from ddd where argkey=t1.argkey and feat=''gen'') as gen,
	(select val from ddd where argkey=t1.argkey and feat=''pn'') as pn, 
	(select val from ddd where argkey=t1.argkey and feat=''aspect.perf'') as aspect_perf, 
	(select val from ddd where argkey=t1.argkey and feat=''aspect.progr'') as aspect_progr, 
	(select val from ddd where argkey=t1.argkey and feat=''mood'') as mood, 
	(select val from ddd where argkey=t1.argkey and feat=''tense'') as tense
	from (arg natural join prd) as t1;

' );

INSERT INTO qrya VALUES ( 'remove-schema', 0, 'select-list' );
INSERT INTO qry VALUES 
       ( 'remove-schema', 1, 
       '
DROP SCHEMA $0 CASCADE;

DELETE FROM public.meta WHERE var=''user'' and val=$0:text ;
' );

INSERT INTO qry VALUES 
       ( 'get-filter', 0, 
       'SELECT val FROM meta WHERE var=''filter'' LIMIT 1' );

INSERT INTO qrya VALUES ( 'test-user', 0, 'text' );
INSERT INTO qry VALUES 
       ( 'test-user', 1, 
       'SELECT val FROM public.meta WHERE var=''user'' AND val=$0' );

INSERT INTO qry VALUES 
       ( 'load-semi', 0, 
       '
DELETE FROM prd;
DELETE FROM arg;
DELETE FROM ddd;
copy prd from ''/home/bmw20/tmp/semi.obj.prd'';
copy arg from ''/home/bmw20/tmp/semi.obj.arg'';
copy ddd from ''/home/bmw20/tmp/semi.obj.ddd'';
');
