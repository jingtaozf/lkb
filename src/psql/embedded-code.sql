DELETE FROM public.meta WHERE var='user';

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
-- DROP VIEW active;
-- DROP VIEW filtered;
-- DROP VIEW revision_all;

CREATE OR REPLACE VIEW filtered
AS SELECT * 
     FROM revision_all
     WHERE flags = 1
      AND $0;

--CREATE VIEW active
-- AS SELECT fil.*
-- FROM 
--  (filtered AS fil
--  NATURAL JOIN 
--   (SELECT name, max(modstamp) AS modstamp 
--     FROM filtered
--     GROUP BY name) AS t1
--); 

UPDATE meta SET val=$0:text WHERE var=''filter'';

--
-- build current_grammar
--

DELETE FROM current_grammar; 
INSERT INTO current_grammar 
	SELECT * FROM active
	; 

-- SELECT build_current_grammar();

VACUUM ANALYZE current_grammar;
' );

INSERT INTO qry VALUES 
       ( 'build-current-grammar', 0, 
'
DELETE FROM current_grammar; 
INSERT INTO current_grammar 
	SELECT * FROM active; 

--SELECT build_current_grammar();
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
INSERT INTO qry VALUES 
       ( 'dump-db', 1, 
       '
DELETE FROM temp;
INSERT INTO temp
 (SELECT * FROM public.revision ORDER BY name, userid, version);
COPY temp TO $0 DELIMITERS '','' WITH NULL AS '''';
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

CREATE OR REPLACE VIEW obj_semi_main AS SELECT name, keyrel_lexeme(keyrel) as lexeme, keyrel_pos(keyrel) as pos, keyrel_sense(keyrel) as sense, keytag, comments, exemplars from current_grammar where btrim(keyrel,''"'') like ''\\_%'';

CREATE TABLE obj_semi_main_temp();

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


