

CREATE OR REPLACE FUNCTION public.next_version(text) RETURNS integer AS '
    SELECT COALESCE(1 + max(version),0) FROM revision_all 
	       WHERE name LIKE $1 AND user=user;
 ' LANGUAGE SQL;

CREATE OR REPLACE FUNCTION public.orthography_set() RETURNS SETOF text AS '
    SELECT DISTINCT orthography FROM current_grammar;
 ' LANGUAGE SQL;

CREATE OR REPLACE FUNCTION public.lex_id_set() RETURNS SETOF text AS '
    SELECT DISTINCT name FROM current_grammar;
 ' LANGUAGE SQL;

CREATE OR REPLACE FUNCTION public.lookup_word(text) RETURNS SETOF text AS '
SELECT name FROM current_grammar WHERE orthkey LIKE $1;
' LANGUAGE SQL;

CREATE OR REPLACE FUNCTION public.clear_scratch() RETURNS boolean AS '
DELETE FROM revision;
SELECT true;
' LANGUAGE SQL;

CREATE OR REPLACE FUNCTION public.update_meta_mod_time_public() RETURNS boolean AS '
DELETE FROM public.meta WHERE var=''mod_time'';
INSERT INTO public.meta VALUES (''mod_time'',current_timestamp);
SELECT true;
' LANGUAGE SQL SECURITY DEFINER;

CREATE OR REPLACE FUNCTION public.commit_scratch() RETURNS boolean AS '
INSERT INTO public.revision (SELECT * FROM revision);

SELECT update_meta_mod_time_public();

DELETE FROM revision;

DELETE FROM meta WHERE var=''mod_time'';
INSERT INTO meta VALUES (''mod_time'',current_timestamp);

SELECT true;
' LANGUAGE SQL;

CREATE OR REPLACE FUNCTION public.show_scratch() RETURNS SETOF text AS '
SELECT distinct name FROM revision;
' LANGUAGE SQL;

CREATE OR REPLACE FUNCTION public.get_filter() RETURNS text AS '
SELECT val FROM meta WHERE var=''filter'' LIMIT 1;
' LANGUAGE SQL;

CREATE OR REPLACE FUNCTION public.test_user(text) RETURNS SETOF text AS '
SELECT val FROM public.meta WHERE var=''user'' AND val = $1;
' LANGUAGE SQL;

CREATE OR REPLACE FUNCTION public.build_current_grammar () RETURNS boolean AS
'
DELETE FROM current_grammar; 
INSERT INTO current_grammar 
	SELECT * FROM active; 
DELETE FROM meta WHERE var=''build_time'';
INSERT INTO meta VALUES (''build_time'',current_timestamp);

SELECT true;' 
LANGUAGE SQL;

CREATE OR REPLACE FUNCTION public.mod_time_private() RETURNS text AS '
SELECT COALESCE((SELECT val FROM meta WHERE var=''mod_time'' LIMIT 1),''infin'')
' LANGUAGE SQL;

CREATE OR REPLACE FUNCTION public.mod_time_public() RETURNS text AS '
SELECT COALESCE((SELECT val FROM public.meta WHERE var=''mod_time'' LIMIT 1),''infin'')
' LANGUAGE SQL;

CREATE OR REPLACE FUNCTION public.mod_time() RETURNS text AS '
SELECT max(t) FROM (SELECT mod_time_private() AS t UNION SELECT mod_time_public() AS t) AS foo;
' LANGUAGE SQL;

CREATE OR REPLACE FUNCTION public.build_time() RETURNS text AS '
SELECT COALESCE((SELECT val FROM meta WHERE var=''build_time'' LIMIT 1),'''')
' LANGUAGE SQL;

CREATE OR REPLACE FUNCTION public.semi_build_time_private() RETURNS text AS '
SELECT COALESCE((SELECT val FROM meta WHERE var=''semi_build_time'' LIMIT 1),'''')
' LANGUAGE SQL;
