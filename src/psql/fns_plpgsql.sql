CREATE OR REPLACE FUNCTION public.lookup_general(text,text) RETURNS SETOF text AS '
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

CREATE OR REPLACE FUNCTION public.lookup_general_null(text) RETURNS SETOF text AS '
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

CREATE OR REPLACE FUNCTION public.initialize_current_grammar(text) RETURNS boolean AS '
BEGIN
 EXECUTE ''
  CREATE OR REPLACE VIEW filtered
   AS SELECT * 
    FROM revision_all
    WHERE '' || $1 ;
 EXECUTE ''UPDATE meta SET val= '' || quote_literal($1) || '' WHERE var=''''filter'''''';
 IF mod_time() > build_time() THEN
   EXECUTE ''SELECT build_current_grammar()'';
 ENd IF;
 RETURN true;
END;
' LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION public.create_schema(text) RETURNS boolean AS '
BEGIN
  EXECUTE ''CREATE SCHEMA '' || $1;
  EXECUTE ''INSERT INTO public.meta VALUES (''''user'''', '' || quote_literal($1) || '')'';
 CREATE TABLE meta AS SELECT * FROM public.meta WHERE var=''filter'';
-- scratch
 CREATE TABLE revision AS SELECT * FROM public.revision WHERE NULL;
 EXECUTE ''CREATE UNIQUE INDEX user_'' || user || ''_name_revision_userid
  ON revision (name,version,userid)''; 
-- current_grammar
 CREATE TABLE current_grammar AS SELECT * FROM public.revision WHERE NULL;
 CREATE UNIQUE INDEX current_grammar_name ON current_grammar (name varchar_ops);
 CREATE INDEX current_grammar_orthkey ON current_grammar (orthkey varchar_ops); 

 IF check_version(''7.4'') THEN
	CREATE UNIQUE INDEX current_grammar_name_pattern ON current_grammar (name varchar_pattern_ops);
	CREATE INDEX current_grammar_orthkey_pattern ON current_grammar (orthkey varchar_pattern_ops);
 ELSE
	CREATE UNIQUE INDEX current_grammar_name_pattern ON current_grammar (name);
	CREATE INDEX current_grammar_orthkey_pattern ON current_grammar (orthkey);
 END IF; 

-- views
 CREATE VIEW filtered AS SELECT * FROM public.revision WHERE NULL;
 CREATE VIEW active
  AS SELECT fil.*
  FROM 
   (filtered AS fil
   NATURAL JOIN 
    (SELECT name, max(modstamp) AS modstamp 
      FROM filtered
      GROUP BY name) AS t1)
  WHERE flags=1;
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

-- mod time
DELETE FROM meta WHERE var=''mod_time'';
INSERT INTO meta VALUES (''mod_time'',current_timestamp);

RETURN true;
END;
' LANGUAGE plpgsql;

