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

INSERT INTO qrya VALUES ( 'next-version', 0, 'like-text');
INSERT INTO qry VALUES 
       ( 'next-version', 1, 
         '
SELECT next_version($0); 
');


INSERT INTO qry VALUES 
       ( 'orthography-set', 0, 
         'SELECT orthography_set()' );

INSERT INTO qry VALUES 
       ( 'lex-id-set', 0, 
         'SELECT lex_id_set()');

INSERT INTO qrya VALUES ( 'lookup-word', 0, 'like-text' );
INSERT INTO qry VALUES 
       ( 'lookup-word', 1, 
         'SELECT lookup_word($0)' );

INSERT INTO qrya VALUES ( 'lookup-general', 0, 'e-select-list' );
INSERT INTO qrya VALUES ( 'lookup-general', 1, 'e-like-text' );
INSERT INTO qry VALUES 
       ( 'lookup-general', 2, 
         'SELECT * FROM lookup_general($0,$1)' );

INSERT INTO qrya VALUES ( 'lookup-general-null', 0, 'e-select-list' );
INSERT INTO qry VALUES 
       ( 'lookup-general-null', 1,
         'SELECT * FROM lookup_general_null($0)' );

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


INSERT INTO qrya VALUES ( 'initialize-current-grammar', 0, 'e-where-subcls' );
INSERT INTO qry VALUES 
       ( 'initialize-current-grammar', 1, 
'SELECT initialize_current_grammar($0);
 VACUUM ANALYZE current_grammar;' );

INSERT INTO qry VALUES 
       ( 'build-current-grammar', 0, 
'
SELECT build_current_grammar();
' );

INSERT INTO qry VALUES 
       ( 'cluster-current-grammar', 0, 
'
SELECT cluster_current_grammar();
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
SELECT clear_scratch();
       ' );

INSERT INTO qry VALUES 
       ( 'commit-scratch', 0, 
       '
SELECT commit_scratch();
       ' );

INSERT INTO qry VALUES 
       ( 'show-scratch', 0, 
       '
SELECT show_scratch();
       ' );

INSERT INTO qrya VALUES ( 'merge-into-db', 0, 'e-text' );
INSERT INTO qrya VALUES ( 'merge-into-db', 1, 'e-text' );
INSERT INTO qry VALUES 
       ( 'merge-into-db', 2, 
       'SELECT merge_into_db($0,$1)' );

INSERT INTO qrya VALUES ( 'merge-defn', 0, 'e-text' );
INSERT INTO qry VALUES 
       ( 'merge-defn', 1, 
       'SELECT merge_defn($0)' );

INSERT INTO qrya VALUES ( 'merge-multi-into-db', 0, 'e-text' );
INSERT INTO qry VALUES 
       ( 'merge-multi-into-db', 1, 
       'SELECT merge_multi_into_db($0)' );

INSERT INTO qrya VALUES ( 'dump-db', 0, 'e-text' );
INSERT INTO qrya VALUES ( 'dump-db', 1, 'e-text' );
INSERT INTO qry VALUES 
       ( 'dump-db', 2, 
       'SELECT dump_db($0,$1)' );

INSERT INTO qrya VALUES ( 'dump-multi-db', 0, 'e-text' );
INSERT INTO qry VALUES 
       ( 'dump-multi-db', 1, 
       'SELECT dump_multi_db($0)' );

INSERT INTO qrya VALUES ( 'value-set', 0, 'select-list' );
INSERT INTO qry VALUES 
       ( 'value-set', 1, 
       'SELECT DISTINCT $0 FROM revision_all WHERE $0 IS NOT NULL;' );

INSERT INTO qrya VALUES ( 'create-schema', 0, 'e-select-list' );
INSERT INTO qry VALUES 
       ( 'create-schema', 1, 
       'SELECT create_schema($0)' );

INSERT INTO qrya VALUES ( 'remove-schema', 0, 'e-select-list' );
INSERT INTO qry VALUES 
       ( 'remove-schema', 1, 
       'SELECT remove_schema($0)' );

INSERT INTO qry VALUES 
       ( 'get-filter', 0, 
       'SELECT get_filter()' );

INSERT INTO qrya VALUES ( 'test-user', 0, 'text' );
INSERT INTO qry VALUES 
       ( 'test-user', 1, 
       'SELECT test_user($0);' );

INSERT INTO qry VALUES 
       ( 'load-semi', 0, 
       'SELECT load_semi()');



















---
-- SQL functions
---

-- necessary in order to define fns. doesn't mean these will be the actual view queried...
CREATE VIEW revision_all AS SELECT * FROM revision WHERE NULL;
CREATE VIEW active AS SELECT * FROM revision WHERE NULL;
CREATE TABLE current_grammar AS SELECT * FROM revision WHERE NULL;

CREATE OR REPLACE FUNCTION next_version(text) RETURNS integer AS '
    SELECT COALESCE(1 + max(version),0) FROM revision_all 
	       WHERE name ~* $1 AND user=user;
 ' LANGUAGE SQL;

CREATE OR REPLACE FUNCTION orthography_set() RETURNS SETOF text AS '
    SELECT DISTINCT orthography FROM current_grammar;
 ' LANGUAGE SQL;

CREATE OR REPLACE FUNCTION lex_id_set() RETURNS SETOF text AS '
    SELECT DISTINCT name FROM current_grammar;
 ' LANGUAGE SQL;

CREATE OR REPLACE FUNCTION lookup_word(text) RETURNS SETOF text AS '
SELECT name FROM current_grammar WHERE orthkey ILIKE $1;
' LANGUAGE SQL;

CREATE OR REPLACE FUNCTION cluster_current_grammar() RETURNS boolean AS '
CLUSTER current_grammar_name ON current_grammar;
SELECT true;
' LANGUAGE SQL;

CREATE OR REPLACE FUNCTION clear_scratch() RETURNS boolean AS '
DELETE FROM revision;
SELECT true;
' LANGUAGE SQL;

CREATE OR REPLACE FUNCTION commit_scratch() RETURNS boolean AS '
INSERT INTO public.revision (SELECT * FROM revision);
DELETE FROM revision;
SELECT true;
' LANGUAGE SQL;

CREATE OR REPLACE FUNCTION show_scratch() RETURNS SETOF text AS '
SELECT distinct name FROM revision;
' LANGUAGE SQL;

CREATE OR REPLACE FUNCTION get_filter() RETURNS text AS '
SELECT val FROM meta WHERE var=''filter'' LIMIT 1;
' LANGUAGE SQL;

CREATE OR REPLACE FUNCTION test_user(text) RETURNS SETOF text AS '
SELECT val FROM public.meta WHERE var=''user'' AND val=$1;
' LANGUAGE SQL;

CREATE OR REPLACE FUNCTION build_current_grammar () RETURNS boolean AS
'
DELETE FROM current_grammar; 
INSERT INTO current_grammar 
	SELECT * FROM active; 
DELETE FROM meta WHERE var=''build_time'';
INSERT INTO meta VALUES (''build_time'',current_timestamp);

SELECT true;' 
LANGUAGE SQL;

DROP VIEW revision_all;
DROP VIEW active;
DROP TABLE current_grammar;

---
--- PL/pgSQL
---

CREATE OR REPLACE FUNCTION lookup_general(text,text) RETURNS SETOF text AS '
DECLARE
	x RECORD;
	sql_str text;
BEGIN
	sql_str := ''SELECT name FROM current_grammar WHERE '' || $1 || '' ILIKE '' || $2;
	FOR x IN EXECUTE sql_str LOOP 
     	  RETURN NEXT x.name;
 	END LOOP;	
	RETURN;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION lookup_general_null(text) RETURNS SETOF text AS '
DECLARE
	x RECORD;
	sql_str text;
BEGIN
	sql_str := ''SELECT name FROM current_grammar WHERE '' || $1 || '' IS NULL '';
	FOR x IN EXECUTE sql_str LOOP 
     	  RETURN NEXT x.name;
 	END LOOP;	
	RETURN;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION merge_into_db(text,text) RETURNS integer AS '
BEGIN
 DROP INDEX public_orthkey;
 DROP INDEX name_modstamp;
 DROP INDEX public_revision_name_modstamp;
 ALTER TABLE public.revision DROP CONSTRAINT revision_pkey;

 DELETE FROM temp;
 EXECUTE ''COPY temp FROM '' || $1 || '' DELIMITERS '''','''' WITH NULL AS '''''''' '';
 DELETE FROM revision_new;
 INSERT INTO revision_new
  SELECT * FROM (SELECT DISTINCT name,userid,version FROM temp EXCEPT SELECT name,userid,version FROM public.revision) AS t1 NATURAL JOIN temp;
 INSERT INTO public.revision SELECT * FROM revision_new;
 
 ALTER TABLE public.revision ADD PRIMARY KEY (name,version,userid);
 CREATE INDEX public_orthkey ON public.revision (orthkey); 
 CREATE UNIQUE INDEX name_modstamp ON public.revision (name,modstamp); 
 CREATE INDEX public_revision_name_modstamp ON public.revision (name, modstamp);

 EXECUTE '' COPY revision_new TO '' || $2 || '' DELIMITERS '''','''' WITH NULL AS '''''''' '';
 RETURN (SELECT count(*) FROM revision_new);
END;
' LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION merge_defn(text) RETURNS text AS '
BEGIN
 CREATE TABLE temp_defn AS SELECT * FROM defn WHERE NULL;
 EXECUTE ''COPY temp_defn FROM'' || $1;
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
 RETURN (SELECT val FROM meta WHERE var=''tmp-merge-defn'');
END;
' LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION merge_multi_into_db(text) RETURNS boolean AS '
BEGIN
 DELETE FROM temp_multi;
 EXECUTE '' COPY temp_multi FROM '' || $1 || '' DELIMITERS '''',''''; '';
 DELETE FROM public.multi WHERE name IN (SELECT name FROM temp_multi);
 INSERT INTO public.multi
  (SELECT * FROM temp_multi);
 RETURN true;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION dump_db(text,text) RETURNS boolean AS '
BEGIN
 DELETE FROM temp;
 INSERT INTO temp
  (SELECT * FROM public.revision ORDER BY name, userid, version);
 EXECUTE ''COPY temp TO '' || $1 || '' DELIMITERS '''','''' WITH NULL AS '''''''''';
 CREATE TABLE temp_defn AS 
  SELECT * FROM defn ORDER BY mode,slot,field;
 EXECUTE ''COPY temp_defn TO '' || $2;
 DROP TABLE temp_defn;
 RETURN true;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION dump_multi_db(text) RETURNS boolean AS '
BEGIN
DELETE FROM temp_multi;
INSERT INTO temp_multi
 (SELECT * FROM multi ORDER BY name);
EXECUTE ''COPY temp_multi TO '' || $1 || '' DELIMITERS '''','''''';
 RETURN true;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION initialize_current_grammar(text) RETURNS boolean AS '
BEGIN
 EXECUTE ''
  CREATE OR REPLACE VIEW filtered
   AS SELECT * 
    FROM revision_all
    WHERE flags = 1
          AND '' || $1 ;
 EXECUTE ''UPDATE meta SET val= '' || quote_literal($1) || '' WHERE var=''''filter'''''';
 EXECUTE ''SELECT build_current_grammar()'';
 RETURN true;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION create_schema(text) RETURNS boolean AS '
BEGIN
 EXECUTE ''CREATE SCHEMA '' || $1;
 EXECUTE ''INSERT INTO public.meta VALUES (''''user'''', '' || quote_literal($1) || '')'';
 CREATE TABLE meta AS SELECT * FROM public.meta WHERE var=''filter'';
-- scratch
 CREATE TABLE revision AS SELECT * FROM public.revision WHERE NULL;
-- temp
 CREATE TABLE temp AS SELECT * FROM public.revision WHERE NULL;
 CREATE TABLE revision_new AS SELECT * FROM public.revision WHERE NULL;
-- current_grammar
 CREATE TABLE current_grammar AS SELECT * FROM public.revision WHERE NULL;
 CREATE UNIQUE INDEX current_grammar_name
  ON current_grammar (name); 
 CREATE INDEX current_grammar_orthkey
  ON current_grammar (orthkey); 
-- views
 CREATE VIEW filtered AS SELECT * FROM public.revision WHERE NULL;
 CREATE VIEW active
  AS SELECT fil.*
  FROM 
   (filtered AS fil
   NATURAL JOIN 
    (SELECT name, max(modstamp) AS modstamp 
      FROM filtered
      GROUP BY name) AS t1);
 CREATE VIEW revision_all
  AS SELECT * FROM public.revision 
   UNION 
   SELECT * FROM revision;
-- semi
 CREATE TABLE prd 
  (lexid text,
   predkey integer,
   pred text,
   lex text,
   pos text,
   sense text,
   carg text,
   PRIMARY KEY (predkey));
 DELETE FROM prd;
 CREATE INDEX prd_lexid on prd (lexid);
 CREATE TABLE arg
 (predkey int,
  argkey int,
  arg int,
  type text,
  PRIMARY KEY (argkey));
 DELETE FROM arg;
 CREATE INDEX arg_predkey on arg (predkey);
 CREATE TABLE ddd
  (argkey int,
   feat text,
   val text);
 DELETE FROM ddd;
 CREATE INDEX ddd_argkey on ddd (argkey);
 CREATE OR REPLACE VIEW obj_semi AS
  SELECT 
   lexid, pred, lex, pos, sense, carg, arg, type, 
   (SELECT val FROM ddd WHERE argkey=t1.argkey AND feat=''gen'') AS gen,
   (SELECT val FROM ddd WHERE argkey=t1.argkey AND feat=''pn'') AS pn, 
   (SELECT val FROM ddd WHERE argkey=t1.argkey AND feat=''aspect.perf'') AS aspect_perf, 
   (SELECT val FROM ddd WHERE argkey=t1.argkey AND feat=''aspect.progr'') AS aspect_progr, 
   (SELECT val FROM ddd WHERE argkey=t1.argkey AND feat=''mood'') AS mood, 
   (SELECT val FROM ddd WHERE argkey=t1.argkey AND feat=''tense'') AS tense
   FROM (arg NATURAL JOIN prd) AS t1;
RETURN true;
END;
' LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION remove_schema(text) RETURNS boolean AS
'
BEGIN
 EXECUTE ''DROP SCHEMA '' || $1 || '' CASCADE'';
 EXECUTE ''DELETE FROM public.meta WHERE var=''''user'''' and val= '' || quote_literal($1) ;
RETURN true;
END;' 
LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION load_semi() RETURNS boolean AS
'
BEGIN
 DELETE FROM prd;
 DELETE FROM arg;
 DELETE FROM ddd;
 COPY prd FROM ''/home/bmw20/tmp/semi.obj.prd'';
 COPY arg FROM ''/home/bmw20/tmp/semi.obj.arg'';
 COPY ddd FROM ''/home/bmw20/tmp/semi.obj.ddd'';
RETURN true;
END;' 
LANGUAGE plpgsql;

